/*
 * Project: cmdrecplay
 * Version: 2.0
 * Copyright: (C) 2014-2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/04 13:43:35 JST
 */

#include "config.h"
#include "debug.h"
#include "PlayPatterns.h"
#include "PlayDatabase.h"

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/wait.h>
#include <signal.h>
#include <unistd.h>
#include <sys/param.h>

typedef struct cmdplay_commands_t
{
    char const * config_file;
    char const * optional_file;
    char** unprocessed_argv;
    int verbose;
} cmdplay_commands_t;

// forward declare the used methods
static void put_environment(char const * key_equal_value);
static void release_commands(cmdplay_commands_t *);
static void parse(int argc, char* argv[], cmdplay_commands_t * commands);

static void print_version();
static void print_usage(char const * const name);

static PlayPatterns_s* cmdplay_init(const char* config_file);
static void argv_to_parameter_set(char** argv, const char* optfile,
                                  ParameterSet_s* parameter_set);
static void cmdplay_term(PlayPatterns_s* play_patterns);

int main(int argc, char* argv[])
{
  cmdplay_commands_t commands = {
    .config_file = DEFAULT_CMDPLAY_CONFIG_FILE,
    .optional_file = "",
    .unprocessed_argv = 0,
    .verbose = 0
  };
  Argv_s argvs;
  const char* cwd_ptr;

  parse(argc, argv, &commands);
  PlayPatterns_s * const pattern = cmdplay_init(commands.config_file);
  if (commands.verbose)
    play_patterns_print(pattern);
  ParameterSet_s parameter_set;
  parameterSet_init(&parameter_set);
  argv_to_parameter_set(commands.unprocessed_argv, commands.optional_file,
                        &parameter_set);
  int record_offset = play_patterns_apply(pattern, &parameter_set);
  cwd_ptr = parameterSet_refer_string_value(&parameter_set, PARAMETER_TYPE_EXECCWD);
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 1
  fprintf(stderr, "chdir(`%s')\n", cwd_ptr);
#endif
#endif
  if (chdir(cwd_ptr) < 0)
    {
      fprintf(stderr, "unknown path %s !!\n", cwd_ptr);
      perror("cmdplay: chdir");
      exit(EXIT_FAILURE);
    }
  PlayPattern_s* ptr = pattern->patterns + record_offset;
  for (int i = 0; i < ptr->execenvs.number_of_elements; ++i)
    {
      char* key_equal_value = ptr->execenvs.elements[i].operand;
      put_environment(key_equal_value);
    }
  argvs.argv = NULL;
  parameterSet_get_execargv(&argvs, &parameter_set);
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 1
  fprintf(stderr, "execvp()\n");
  for (int offset = 0; argvs.argv[offset]; offset++)
    fprintf(stderr, "  argv[%d] = `%s'\n", offset, argvs.argv[offset]);
#endif
#endif
  if (execvp(argvs.argv[0], argvs.argv) < 0)
    {
      if (errno == ENOENT)
        fprintf(stderr, "cmdplay: executable `%s' is not found.\n", argvs.argv[0]);
      else
        perror("cmdplay: execvp");
      exit(EXIT_FAILURE);
    }
  return 0;
}

static void put_environment(char const * key_equal_value)
{
  char work[strlen(key_equal_value) + 1];
  strcpy(work, key_equal_value);
  if (putenv(work) < 0)
    {
      fprintf(stderr, "putenv(%s)!!\n", work);
      perror("cmdplay: putenv");
      exit(EXIT_FAILURE);
    }
}

static void release_commands(cmdplay_commands_t * commands)
{
}

static void parse(int argc, char* argv[], cmdplay_commands_t * commands)
{
  // parse command line arguments.
  int opt;
  while ((opt = getopt(argc, argv, "c:f:xvh?")) != -1)
    {
      switch (opt)
        {
        case 'c':
          commands->config_file = optarg;
          break;
        case 'f':
          commands->optional_file = optarg;
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

static void print_version()
{
  fprintf(stdout,
          "Cmdplay %s\n"
          "Copyright (C) 2014-2016 by Dr.Sc.KAWAMOTO,Takuji (Ext)\n"
          "This is free software; see the source for copying conditions.\n"
          "There is NO warranty; not even for MERCHANTABILITY or FITNESS "
          "FOR A PARTICULAR PURPOSE.\n",
          CMDRECPLAY_VERSION);
}

static void print_usage(char const * const name)
{
  fprintf(stderr,
          "Usage: %s [options] -- <flycheck/auto-completion/ifendif command>\n"
          "\n"
          "options:\n"
          "  -c config        config file (default: %s)\n"
          "  -f optionalfile  optional file name (default: none)\n"
          "  -x               verbose pattern dump at the end (default: disabled)\n"
          "  -v               print %s version and exit\n"
          "  -h               print this message\n"
          "\n"
          "exit status: %d on any internal problem,\n"
          "otherwise same as the flycheck/auto-completion/ifendif command exit status.\n",
          name,
          DEFAULT_CMDPLAY_CONFIG_FILE,
          name,
          EXIT_FAILURE);
}

static PlayPatterns_s* cmdplay_init(const char* config_file)
{
  ConfigFile_s cf;
  PlayPatterns_s* pp;

  config_file_init(&cf, config_file);
  pp = malloc(sizeof(PlayPatterns_s));
  play_patterns_init(pp);
  play_patterns_read_config(pp, &cf);
  config_file_term(&cf);
  return pp;
}

static void argv_to_parameter_set(char** argv, const char* optfile,
                                  ParameterSet_s* parameter_set)
{
  Argv_s argvs;
  char buffer[MAXPATHLEN + 1];
  getcwd(buffer, MAXPATHLEN);
  parameterSet_set_by_copy_as_realpath(parameter_set, PARAMETER_TYPE_PLAYCWD, buffer);
  parameterSet_set_by_copy_string(parameter_set, PARAMETER_TYPE_PLAYCMD, argv[0]);
  if (optfile && optfile[0])
    {
      parameterSet_set_by_copy_as_realpath(parameter_set, PARAMETER_TYPE_OPTFILE,
                                           optfile);
    }
  argvs.argv = argv + 1;
  parameterSet_set_argv_value(parameter_set, PARAMETER_TYPE_PLAYARGS, &argvs);
}

static void cmdplay_term(PlayPatterns_s* play_patterns)
{
  play_patterns_term(play_patterns);
}

/* Local Variables:     */
/* mode: c              */
/* End:                 */
