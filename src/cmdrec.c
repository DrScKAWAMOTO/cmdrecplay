/*
 * Project: cmdrecplay
 * Version: 1.1
 * Copyright: (C) 2014-2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/04 13:43:35 JST
 */

#include "config.h"
#include "debug.h"
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
    char const * path_contents;
    char const * cmdskin_path;
    char * const * unprocessed_argv;
    int verbose;
} cmdrec_commands_t;

// variables which are used in signal handler
static volatile pid_t    child_pid;
static volatile int      child_status = EXIT_FAILURE;

// forward declare the used methods
static void mask_all_signals(int command);
static void install_signal_handler(int signum);
static void notify_to_child(int fd);
static void observed_by_parent(int fd);
static void parse(int argc, char * const argv[], cmdrec_commands_t * commands);

static void print_version();
static void print_usage(char const * const name);

static RecPattern_s* cmdrec_init(const char* config_file);
static void cmdrec_term(RecPattern_s* rec_pattern);
static cmdrec_commands_t commands = {
  .config_file = DEFAULT_CMDREC_CONFIG_FILE,
  .path_contents = "",
  .cmdskin_path = "",
  .unprocessed_argv = 0,
  .verbose = 0
};

int main(int argc, char * const argv[])
{
  const char* env = getenv("PATH");
  int sync_fd[2];

  if (!env)
    {
      perror("cmdrec: getenv");
      exit(EXIT_FAILURE);
    }
  commands.path_contents = env;
  parse(argc, argv, &commands);
  RecPattern_s * const pattern = cmdrec_init(commands.config_file);
  rec_pattern_setup_for_cmdskin(pattern, commands.path_contents,
                                commands.cmdskin_path);
  if (commands.verbose)
    rec_pattern_print(pattern);
  // set up sync pipe
  if (pipe(sync_fd) < 0)
    {
      perror("cmdrec: pipe");
      exit(EXIT_FAILURE);
    }
  // fork
  child_pid = fork();
  if (child_pid < 0)
    {
      perror("cmdrec: fork");
      exit(EXIT_FAILURE);
    }
  else if (child_pid == 0)
    {
      // child process
      cmdrec_term(pattern);
      close(sync_fd[1]);
      if (setenv("CMDSKIN_CONFIG_FILE", commands.config_file, 1) < 0)
        {
          perror("cmdrec: setenv");
          exit(EXIT_FAILURE);
        }
      observed_by_parent(sync_fd[0]);
      if (execvp(*commands.unprocessed_argv, commands.unprocessed_argv) < 0)
        {
          perror("cmdrec: execvp");
          exit(EXIT_FAILURE);
        }
      fprintf(stderr, "execvp() returned with non error !!\n");
      exit(EXIT_FAILURE);
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
      notify_to_child(sync_fd[1]);
      while (1)
        {
          int status;
          pid_t wait_pid = wait(&status);
          child_status = status;
          if (wait_pid == child_pid)
            break;
        }
      printf("cmdrec finished !!\n");
      rec_pattern_rm_cmdskin_path();
      cmdrec_term(pattern);
      return child_status;
    }
}

static void parse(int argc, char * const argv[], cmdrec_commands_t * commands)
{
  // parse command line arguments.
  int opt;
  while ((opt = getopt(argc, argv, "c:p:s:xvh?")) != -1)
    {
      switch (opt)
        {
        case 'c':
          commands->config_file = optarg;
          break;
        case 'p':
          commands->path_contents = optarg;
          break;
        case 's':
          commands->cmdskin_path = optarg;
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
  if (sigemptyset(&action.sa_mask) < 0)
    {
      perror("cmdrec: sigemptyset");
      exit(EXIT_FAILURE);
    }
  if (sigaddset(&action.sa_mask, signum) < 0)
    {
      perror("cmdrec: sigaddset");
      exit(EXIT_FAILURE);
    }
  if (sigaction(signum, &action, NULL) < 0)
    {
      perror("cmdrec: sigaction");
      exit(EXIT_FAILURE);
    }
}

static void mask_all_signals(int command)
{
  sigset_t signal_mask;
  if (sigfillset(&signal_mask) < 0)
    {
      perror("cmdrec: sigfillset");
      exit(EXIT_FAILURE);
    }
  if (sigprocmask(command, &signal_mask, 0) < 0)
    {
      perror("cmdrec: sigprocmask");
      exit(EXIT_FAILURE);
    }
}

static void notify_to_child(int fd)
{
  if (write(fd, "ready", 5) < 0)
    {
      perror("cmdrec: write");
      exit(EXIT_FAILURE);
    }
  close(fd);
}

static void observed_by_parent(int fd)
{
  char buffer[5];
  if (read(fd, buffer, sizeof(buffer)) < 0)
    {
      perror("cmdrec: read");
      exit(EXIT_FAILURE);
    }
  close(fd);
}

static void print_version()
{
  fprintf(stdout,
          "Cmdrec %s\n"
          "Copyright (C) 2014-2017 by Dr.Sc.KAWAMOTO,Takuji (Ext)\n"
          "This is free software; see the source for copying conditions.\n"
          "There is NO warranty; not even for MERCHANTABILITY or FITNESS "
          "FOR A PARTICULAR PURPOSE.\n",
          CMDRECPLAY_VERSION);
}

static void print_usage(char const * const name)
{
  fprintf(stdout,
          "Usage: %s [options] -- <build command>\n"
          "\n"
          "options:\n"
          "  -c config         config file (default: %s),\n"
          "  -p path           PATH value for command search (default: %s),\n"
          "  -s cmdskinpath    cmdskin path for splitting command execution (default: none),\n"
          "  -x                verbose pattern dump at the end (default: disabled),\n"
          "  -v                print %s version and exit,\n"
          "  -h                print this message.\n"
          "\n"
          "Exit status: %d on any internal problem,\n"
          "otherwise same as the build command exit status.\n",
          name,
          commands.config_file,
          commands.path_contents,
          name,
          EXIT_FAILURE);
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
  free(rec_pattern);
}

/* Local Variables:     */
/* mode: c              */
/* End:                 */
