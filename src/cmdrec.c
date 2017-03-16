/*
 * Project: cmdrecplay
 * Version: 1.0
 * Copyright: (C) 2014-2016 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/04 13:43:35 JST
 */
/*  Copyright (C) 2012-2014 by László Nagy
    This file is part of Bear.
    This file is part of Cmdrec/Cmdplay.

    Bear is a tool to generate compilation database for clang tooling.

    Bear is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Bear is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "config.h"
#include "debug.h"
#include "protocol.h"
#include "RecPattern.h"
#include "RecDatabase.h"

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/wait.h>
#include <signal.h>


typedef struct cmdrec_commands_t
{
    char const * config_file;
    char const * libcmdrp_file;
    char const * socket_dir;
    char const * socket_file;
    char * const * unprocessed_argv;
    int verbose;
} cmdrec_commands_t;

// variables which are used in signal handler
static volatile pid_t    child_pid;
static volatile int      child_status = EXIT_FAILURE;

// forward declare the used methods
static void mask_all_signals(int command);
static void install_signal_handler(int signum);
static void collect_messages(char const * socket, RecPattern_s * pattern, int sync_fd);
static void set_environment(char const * key, char const * value);
static void prepare_socket_file(cmdrec_commands_t *);
static void teardown_socket_file(cmdrec_commands_t *);
static void release_commands(cmdrec_commands_t *);
static void notify_child(int fd);
static void wait_for_parent(int fd);
static void parse(int argc, char * const argv[], cmdrec_commands_t * commands);

static void print_version();
static void print_usage(char const * const name);

static RecPattern_s* cmdrec_init(const char* config_file);
static void cmdrec_term(RecPattern_s* rec_pattern);
static void bear_message_to_parameter_set(bear_message_t* msg,
                                          RecPattern_s* rec_pattern,
                                          ParameterSet_s* parameter_set);

int main(int argc, char * const argv[])
{
  cmdrec_commands_t commands = {
    .config_file = DEFAULT_CMDREC_CONFIG_FILE,
    .libcmdrp_file = DEFAULT_PRELOAD_FILE,
    .socket_dir = 0,
    .socket_file = 0,
    .unprocessed_argv = 0,
    .verbose = 0
  };
  int sync_fd[2];

  parse(argc, argv, &commands);
  RecPattern_s * const pattern = cmdrec_init(commands.config_file);
  prepare_socket_file(&commands);
  // set up sync pipe
  if (-1 == pipe(sync_fd))
    {
      perror("cmdrec: pipe");
      exit(EXIT_FAILURE);
    }
  // fork
  child_pid = fork();
  if (-1 == child_pid)
    {
      perror("cmdrec: fork");
      exit(EXIT_FAILURE);
    }
  else if (0 == child_pid)
    {
      // child process
      close(sync_fd[1]);
      wait_for_parent(sync_fd[0]);
      set_environment(ENV_PRELOAD, commands.libcmdrp_file);
      set_environment(ENV_OUTPUT, commands.socket_file);
#ifdef ENV_FLAT
      set_environment(ENV_FLAT, "YES");
#endif
      if (-1 == execvp(*commands.unprocessed_argv, commands.unprocessed_argv))
        {
          perror("cmdrec: execvp");
          exit(EXIT_FAILURE);
        }
    }
  else
    {
      // parent process
      install_signal_handler(SIGCHLD);
      install_signal_handler(SIGINT);
      mask_all_signals(SIG_BLOCK);
      close(fileno(stdin));
      close(fileno(stdout));
      close(sync_fd[0]);
      collect_messages(commands.socket_file, pattern, sync_fd[1]);
      teardown_socket_file(&commands);
    }
  if (commands.verbose)
    rec_pattern_print(pattern);
  cmdrec_term(pattern);
  release_commands(&commands);
  return child_status;
}

static void collect_messages(char const * socket_file, RecPattern_s * pattern,
                             int sync_fd)
{
  int s = bear_create_unix_socket(socket_file);
  notify_child(sync_fd);
  // receive messages
  bear_message_t msg;
  mask_all_signals(SIG_UNBLOCK);
  while ((child_pid) && bear_accept_message(s, &msg))
    {
      ParameterSet_s parameter_set;
      parameterSet_init(&parameter_set);
      mask_all_signals(SIG_BLOCK);
      bear_message_to_parameter_set(&msg, pattern, &parameter_set);
      if (rec_pattern_apply(pattern, &parameter_set) == 0)
        database_rec(&parameter_set);
      parameterSet_term(&parameter_set);
      bear_free_message(&msg);
      mask_all_signals(SIG_UNBLOCK);
    }
  mask_all_signals(SIG_BLOCK);
  // release resources
  close(s);
}

static void set_environment(char const * key, char const * value)
{
  if (-1 == setenv(key, value, 1))
    {
      perror("cmdrec: setenv");
      exit(EXIT_FAILURE);
    }
}

static void prepare_socket_file(cmdrec_commands_t * commands)
{
    // create temporary directory for socket
  if (0 == commands->socket_file)
    {
      char template[] = "/tmp/cmdrec-XXXXXX";
      char const * temp_dir = mkdtemp(template);
      if (0 == temp_dir)
        {
          perror("cmdrec: mkdtemp");
          exit(EXIT_FAILURE);
        }
      if (-1 == asprintf((char **)&(commands->socket_file), "%s/socket", temp_dir))
        {
          perror("cmdrec: asprintf");
          exit(EXIT_FAILURE);
        }
      commands->socket_dir = strdup(temp_dir);
      if (0 == commands->socket_dir)
        {
          perror("cmdrec: strdup");
          exit(EXIT_FAILURE);
        }
    }
    // remove old socket file if any
  if ((-1 == unlink(commands->socket_file)) && (ENOENT != errno))
    {
      perror("cmdrec: unlink");
      exit(EXIT_FAILURE);
    }
}

static void teardown_socket_file(cmdrec_commands_t * commands)
{
  unlink(commands->socket_file);
  if (commands->socket_dir)
    {
      rmdir(commands->socket_dir);
    }
}

static void release_commands(cmdrec_commands_t * commands)
{
  if (commands->socket_dir)
    {
      free((void *)commands->socket_dir);
      free((void *)commands->socket_file);
    }
}

static void parse(int argc, char * const argv[], cmdrec_commands_t * commands)
{
  // parse command line arguments.
  int opt;
  while ((opt = getopt(argc, argv, "c:l:s:cexvh?")) != -1)
    {
      switch (opt)
        {
        case 'c':
          commands->config_file = optarg;
          break;
        case 'l':
          commands->libcmdrp_file = optarg;
          break;
        case 's':
          commands->socket_file = optarg;
          break;
        case 'x':
          commands->verbose = 1;
          break;
        case 'v':
          print_version();
          exit(EXIT_SUCCESS);
        case 'h':
          print_usage(argv[0]);
          exit(EXIT_SUCCESS);
        default: /* '?' */
          print_usage(argv[0]);
          exit(EXIT_FAILURE);
        }
    }
    // validate
  if (argc == optind)
    {
      print_usage(argv[0]);
      exit(EXIT_FAILURE);
    }
  commands->unprocessed_argv = &(argv[optind]);
}

static void handler(int signum)
{
  switch (signum)
    {
    case SIGCHLD:
      {
        int status;
        while (0 > waitpid(WAIT_ANY, &status, WNOHANG)) ;
        child_status = WIFEXITED(status) ? WEXITSTATUS(status) : EXIT_FAILURE;
        child_pid = 0;
        break;
      }
    case SIGINT:
      kill(child_pid, signum);
    default:
      break;
    }
}

static void install_signal_handler(int signum)
{
  struct sigaction action;
  action.sa_handler = handler;
  action.sa_flags = SA_NOCLDSTOP;
  if (0 != sigemptyset(&action.sa_mask))
    {
      perror("cmdrec: sigemptyset");
      exit(EXIT_FAILURE);
    }
  if (0 != sigaddset(&action.sa_mask, signum))
    {
      perror("cmdrec: sigaddset");
      exit(EXIT_FAILURE);
    }
  if (0 != sigaction(signum, &action, NULL))
    {
      perror("cmdrec: sigaction");
      exit(EXIT_FAILURE);
    }
}

static void mask_all_signals(int command)
{
  sigset_t signal_mask;
  if (0 != sigfillset(&signal_mask))
    {
      perror("cmdrec: sigfillset");
      exit(EXIT_FAILURE);
    }
  if (0 != sigprocmask(command, &signal_mask, 0))
    {
      perror("cmdrec: sigprocmask");
      exit(EXIT_FAILURE);
    }
}

static void notify_child(int fd)
{
  if (-1 == write(fd, "ready", 5))
    {
      perror("cmdrec: write");
      exit(EXIT_FAILURE);
    }
  close(fd);
}

static void wait_for_parent(int fd)
{
  char buffer[5];
  if (-1 == read(fd, buffer, sizeof(buffer)))
    {
      perror("cmdrec: read");
      exit(EXIT_FAILURE);
    }
  close(fd);
}

static void print_version()
{
  fprintf(stdout,
          "Bear %s\n"
          "Copyright (C) 2012-2014 by László Nagy\n"
          "Cmdrec %s\n"
          "Copyright (C) 2014-2016 by Dr.Sc.KAWAMOTO,Takuji (Ext)\n"
          "This is free software; see the source for copying conditions. "
          "There is NO warranty; not even for MERCHANTABILITY or FITNESS "
          "FOR A PARTICULAR PURPOSE.\n",
          BEAR_VERSION, CMDRECPLAY_VERSION);
}

static void print_usage(char const * const name)
{
  fprintf(stdout,
          "Usage: %s [options] -- <build command>\n"
          "\n"
          "options:\n"
          "  -c config         config file (default: %s)\n"
          "  -l libcmdpreload  library location (default: %s)\n"
          "  -s socket         multiplexing socket (default: randomly generated)\n"
          "  -x                verbose pattern dump at the end (default: disabled)\n"
          "  -v                print %s version and exit\n"
          "  -h                print this message\n"
          "\n"
          "exit status: EXIT_FAILURE on any internal problem,\n"
          "otherwise same as the build command exit status.\n",
          name,
          DEFAULT_CMDREC_CONFIG_FILE,
          DEFAULT_PRELOAD_FILE,
          name);
}

static RecPattern_s* cmdrec_init(const char* config_file)
{
  ConfigFile_s cf;
  RecPattern_s* pp;

  config_file_init(&cf, config_file);
  pp = malloc(sizeof(RecPattern_s));
  rec_pattern_init(pp);
  rec_pattern_read_config(pp, &cf);
  config_file_term(&cf);
  return pp;
}

static void cmdrec_term(RecPattern_s* rec_pattern)
{
  rec_pattern_term(rec_pattern);
}

static void bear_message_to_parameter_set(bear_message_t* msg,
                                          RecPattern_s* rec_pattern,
                                          ParameterSet_s* parameter_set)
{
  char work[100];
  Argv_s argv;
  sprintf(work, "%d", msg->pid);
  parameterSet_set_by_copy_string(parameter_set, PARAMETER_TYPE_PID, work);
  sprintf(work, "%d", msg->ppid);
  parameterSet_set_by_copy_string(parameter_set, PARAMETER_TYPE_PPID, work);
  parameterSet_set_by_copy_string(parameter_set, PARAMETER_TYPE_FUNCTION, msg->fun);
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 1
  fprintf(stderr, "cwd = `%s'\n", msg->cwd);
  for (int offset = 0; msg->cmd[offset]; ++offset)
    fprintf(stderr, "argv[%d] = %s\n", offset, msg->cmd[offset]);
#endif
#endif
  parameterSet_set_by_copy_as_realpath(parameter_set, PARAMETER_TYPE_RECCWD, msg->cwd);
  parameterSet_set_by_copy_string(parameter_set, PARAMETER_TYPE_RECCMD, msg->cmd[0]);
  argv.argv = (char**)(msg->cmd + 1);
  parameterSet_set_argv_value(parameter_set, PARAMETER_TYPE_RECARGS, &argv);
}

/* Local Variables:	*/
/* mode: c		*/
/* End:			*/
