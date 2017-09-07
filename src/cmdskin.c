/*
 * Project: cmdrecplay
 * Version: 1.1
 * Copyright: (C) 2014-2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/04 13:43:35 JST
 */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <limits.h>
#include <sys/wait.h>
#include <signal.h>
#include <assert.h>

#include "config.h"
#include "debug.h"
#include "RecPattern.h"
#include "RecDatabase.h"

static void print_version();
static RecPattern_s* cmdrec_init(const char* config_file);
static void cmdrec_term(RecPattern_s* rec_pattern);

int main(int argc, char * argv[])
{
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 1
  fprintf(stderr, "cmdskin called !!\n");
  char ** argvp = argv;
  int i = 0;
  while (*argvp)
    {
      fprintf(stderr, "argv[%d]=%s\n", i++, *argvp++);
    }
#endif
#endif

  char* file = strrchr(argv[0], '/');
  if (file)
    ++file;
  else
    file = argv[0];
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 1
  fprintf(stderr, "file=%s\n", file);
#endif
#endif
  argv[0] = file;
  if ((argc == 1) || ((argc == 2) && (argv[1][0] == '-')))
    {
      print_version();
      exit(EXIT_FAILURE);
    }
  char* config_file = getenv("CMDSKIN_CONFIG_FILE");
  if (!config_file)
    {
      fprintf(stderr, "cmdskin: getenv(CMDSKIN_CONFIG_FILE)\n");
      exit(EXIT_FAILURE);
    }
  RecPattern_s * const pattern = cmdrec_init(config_file);
  ParameterSet_s parameter_set;
  parameterSet_init(&parameter_set);
  char cwd[LENGTH_OF_PATH_BUFFER];
  parameterSet_set_by_copy_as_realpath(&parameter_set, PARAMETER_TYPE_RECCWD,
                                       getcwd(cwd, LENGTH_OF_PATH_BUFFER));
  parameterSet_set_by_copy_string(&parameter_set, PARAMETER_TYPE_RECCMD, argv[0]);
  Argv_s argvs;
  argv_init(&argvs, argv + 1);
  parameterSet_set_argv_value(&parameter_set, PARAMETER_TYPE_RECARGS, &argvs);
  char command_buffer[PATH_MAX + 20];
  command_buffer[0] = '\0';
  if (rec_pattern_apply(pattern, &parameter_set) == 0)
    {
      database_rec(&parameter_set);
      const char* recfile = parameterSet_refer_string_value(&parameter_set,
                                                            PARAMETER_TYPE_RECFILE);
      sprintf(command_buffer, "srcinc -q %s", recfile);
    }
  parameterSet_term(&parameter_set);
  argv_term(&argvs);
  char* path = getenv("PATH");
  if (!path)
    {
      fprintf(stderr, "cmdskin: getenv(PATH)\n");
      exit(EXIT_FAILURE);
    }
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 1
  fprintf(stderr, "cmdskin: getenv(\"PATH\")=%s !!\n", path);
#endif
#endif
  while ((*path != '\0') && (*path != ':'))
    ++path;
  if (*path == ':')
    {
      if (setenv("PATH", path + 1, 1))
        {
          fprintf(stderr, "cmdskin: setenv(PATH) error");
          exit(EXIT_FAILURE);
        }
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 1
      fprintf(stderr, "cmdskin: to %s !!\n", path + 1);
#endif
#endif
    }
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 1
  fprintf(stderr, "execvp call !!\n");
  fprintf(stderr, "file=%s\n", file);
  argvp = argv;
  i = 0;
  while (*argvp)
    {
      fprintf(stderr, "argv[%d]=%s\n", i++, *argvp++);
    }
#endif
#endif
  if (command_buffer[0])
    {
      int result = system(command_buffer);
      assert(result == 0);
    }
  if (execvp(file, argv) < 0)
    {
      perror("cmdskin: execvp");
      exit(EXIT_FAILURE);
    }
  return 0;
}

static void print_version()
{
  fprintf(stderr,
          "Cmdskin %s\n"
          "Copyright (C) 2014-2017 by Dr.Sc.KAWAMOTO,Takuji (Ext)\n"
          "This is free software; see the source for copying conditions.\n"
          "There is NO warranty; not even for MERCHANTABILITY or FITNESS "
          "FOR A PARTICULAR PURPOSE.\n",
          CMDRECPLAY_VERSION);
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

/* Local Variables:     */
/* mode: c              */
/* End:                 */
