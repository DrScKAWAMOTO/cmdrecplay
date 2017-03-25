/*
 * Project: cmdrecplay
 * Version: 1.1
 * Copyright: (C) 2014-2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/02 11:08:33 JST
 */

#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>
#include <assert.h>
#include <sys/stat.h>

#include "config.h"
#include "RecPattern.h"
#include "Regexp.h"

void rec_pattern_init(RecPattern_s* me)
{
  definition_init(&(me->recfile));
  definition_init(&(me->recmatch));
}

void rec_pattern_read_config(RecPattern_s* me, ConfigFile_s* inpfile)
{
  config_file_read_keyword(inpfile, "rec");
  config_file_read_keyword(inpfile, "pattern");
  config_file_read_keyword(inpfile, "{");
  if (definition_read_config(&(me->recfile), DEFINITION_TYPE_RECFILE, inpfile) < 0)
    {
      fprintf(stderr, "%s:%d: `recfile:' is not specified.\n",
              inpfile->config_file_name, inpfile->line_no);
      exit(EXIT_FAILURE);
    }
  if (definition_read_config(&(me->recmatch), DEFINITION_TYPE_RECMATCH, inpfile) < 0)
    {
      fprintf(stderr, "%s:%d: `recmatch:' is not specified.\n",
              inpfile->config_file_name, inpfile->line_no);
      exit(EXIT_FAILURE);
    }
  config_file_read_keyword(inpfile, "}");
}

int rec_pattern_apply(const RecPattern_s* me, ParameterSet_s* parameter_set)
{
  MatchLines_s mli;
  MatchLocation_s mlo;
  unsigned int start_pos;
  unsigned int end_pos;
  const char* text;
  /* recfile: を適用する */
  assert(me->recfile.definition_type == DEFINITION_TYPE_RECFILE);
  assert(me->recfile.number_of_elements == 1);
  /* recargs =~ /.../ */
  /* recargs !~ /.../ */
  assert((me->recfile.elements[0].operator_type == OPERATOR_TYPE_PARAMETER_MATCHED) ||
         (me->recfile.elements[0].operator_type == OPERATOR_TYPE_PARAMETER_DIDNT_MATCH));
  assert(me->recfile.elements[0].parameter_type == PARAMETER_TYPE_RECARGS);
  /* 適用 */
  regexp_search_all(&mli, me->recfile.elements[0].operand,
                    parameterSet_refer_string_value(parameter_set,
                                                    PARAMETER_TYPE_RECARGS));
  if (me->recfile.elements[0].operator_type == OPERATOR_TYPE_PARAMETER_MATCHED)
    {
      if (mli.matched_count == 1)
        {
          mli.matched_lines[strlen(mli.matched_lines) - 1] = '\0';
          parameterSet_set_by_copy_as_realpath(parameter_set, PARAMETER_TYPE_RECFILE,
                                               mli.matched_lines);
          parameterSet_set_string_value(parameter_set, PARAMETER_TYPE_RECARGS,
                                        mli.unmatched_lines);
          mli.matched_lines = NULL;
          mli.unmatched_lines = NULL;
        }
      else
        {
          match_lines_term(&mli);
          return -1;
        }
    }
  else
    {
      if (mli.unmatched_count == 1)
        {
          mli.unmatched_lines[strlen(mli.unmatched_lines) - 1] = '\0';
          parameterSet_set_by_copy_as_realpath(parameter_set, PARAMETER_TYPE_RECFILE,
                                               mli.unmatched_lines);
          parameterSet_set_string_value(parameter_set, PARAMETER_TYPE_RECARGS,
                                        mli.matched_lines);
          mli.unmatched_lines = NULL;
          match_lines_term(&mli);
        }
      else
        {
          match_lines_term(&mli);
          return -1;
        }
    }

  /* recmatch: を適用する */
  assert(me->recmatch.number_of_elements == 1);
  assert(me->recmatch.definition_type == DEFINITION_TYPE_RECMATCH);
  assert(me->recmatch.number_of_elements == 1);
  /* reccmd =~ /.../ */
  /* reccmd !~ /.../ */
  assert((me->recmatch.elements[0].operator_type == OPERATOR_TYPE_PARAMETER_MATCHED) ||
         (me->recmatch.elements[0].operator_type == OPERATOR_TYPE_PARAMETER_DIDNT_MATCH));
  assert(me->recmatch.elements[0].parameter_type == PARAMETER_TYPE_RECCMD);
  /* 適用 */
  text = parameterSet_refer_string_value(parameter_set, PARAMETER_TYPE_RECCMD);
  start_pos = 0;
  end_pos = strlen(text);
  return regexp_search(&mlo, me->recmatch.elements[0].operand, text, start_pos, end_pos);
}

void rec_pattern_print(RecPattern_s* me)
{
  fprintf(stderr, "rec pattern {\n");
  definition_print(&(me->recfile));
  definition_print(&(me->recmatch));
  fprintf(stderr, "}\n");
}

void rec_pattern_term(RecPattern_s* me)
{
  definition_term(&me->recfile);
  definition_term(&me->recmatch);
}

static void purge_cmdskin_path(const char* home_env)
{
  struct stat sb;
  if (stat(home_env, &sb) != 0)
    {
      fprintf(stderr, "Cannot stat home directory (%s).\n", home_env);
      exit(EXIT_FAILURE);
    }
  if ((sb.st_mode & S_IFDIR) != S_IFDIR)
    {
      fprintf(stderr, "Home (%s) is not a directory.\n", home_env);
      exit(EXIT_FAILURE);
    }
  DIR* dirp = opendir(home_env);
  if (!dirp)
    {
      fprintf(stderr, "Cannot alloc memory.\n");
      exit(EXIT_FAILURE);
    }
  int number_of_pids = 0;
  pid_t* list_of_pids = NULL;
  int* list_of_purges = NULL;
  while (1)
    {
      struct dirent dire;
      struct dirent* dire_p;
      if (readdir_r(dirp, &dire, &dire_p))
        {
          fprintf(stderr, "Cannot readdir_r(1).\n");
          exit(EXIT_FAILURE);
        }
      if (dire_p == NULL)
        break;
      if (((dire.d_type & DT_DIR) == DT_DIR) && (dire.d_namlen >= 10) &&
          (strncmp(dire.d_name, ".cmdskin.", 9) == 0))
        {
          pid_t pid = 0;
          if (sscanf(dire.d_name + 9, "%d", &pid) == 1)
            {
              number_of_pids++;
              if (list_of_pids)
                {
                  list_of_pids =
                    (pid_t*)realloc(list_of_pids, number_of_pids * sizeof(pid_t));
                  list_of_purges =
                    (int*)realloc(list_of_purges, number_of_pids * sizeof(int));
                }
              else
                {
                  list_of_pids = malloc(sizeof(pid_t));
                  list_of_purges = malloc(sizeof(int));
                }
              list_of_pids[number_of_pids - 1] = pid;
              list_of_purges[number_of_pids - 1] = 1;
            }
        }
    }
  closedir(dirp);
  FILE* ps_stream = popen("ps -ax | grep cmdrec | grep -v grep", "r");
  if (!ps_stream)
    {
      fprintf(stderr, "Cannot execute `ps' command.\n");
      exit(EXIT_FAILURE);
    }
  char ps_buffer[LENGTH_OF_BUFFER];
  while (fgets(ps_buffer, LENGTH_OF_BUFFER, ps_stream) == ps_buffer)
    {
      int pid = 0;
      if (sscanf(ps_buffer, "%d", &pid) == 1)
        {
          int i;
          for (i = 0; i < number_of_pids; ++i)
            if (list_of_pids[i] == (pid_t)pid)
              list_of_purges[i] = 0;
        }
    }
  pclose(ps_stream);
  int i;
  for (i = 0; i < number_of_pids; ++i)
    if (list_of_purges[i])
      {
        char command_buffer[LENGTH_OF_BUFFER];
        sprintf(command_buffer, "rm -rf %s/.cmdskin.%d",
                home_env, list_of_pids[i]);
        system(command_buffer);
      }
  if (list_of_pids)
    {
      free(list_of_pids);
      free(list_of_purges);
    }
}

static void make_cmdskin_link(const RecPattern_s* me, const char* path_env,
                              const char* home_env)
{
  pid_t me_pid = getpid();
  char command_buffer[LENGTH_OF_BUFFER];
  sprintf(command_buffer, "mkdir -p %s/.cmdskin.%d", home_env, me_pid);
  system(command_buffer);
  const char* path_ptr = path_env;
  while (*path_ptr != '\0')
    {
      char each_path[LENGTH_OF_PATH_BUFFER];
      char* each_ptr = each_path;
      struct stat sb;
      while ((*path_ptr != '\0') && (*path_ptr != ':'))
        *each_ptr++ = *path_ptr++;
      *each_ptr = '\0';
      if (*path_ptr == ':')
        ++path_ptr;
      if (stat(each_path, &sb) != 0)
        continue;
      if ((sb.st_mode & S_IFDIR) != S_IFDIR)
        continue;
      DIR* dirp = opendir(each_path);
      if (!dirp)
        {
          fprintf(stderr, "Cannot alloc memory.\n");
          exit(EXIT_FAILURE);
        }
      while (1)
        {
          struct dirent dire;
          struct dirent* dire_p;
          if (readdir_r(dirp, &dire, &dire_p))
            {
              fprintf(stderr, "Cannot readdir_r(2).\n");
              exit(EXIT_FAILURE);
            }
          if (dire_p == NULL)
            break;
          if ((dire.d_type & DT_REG) == DT_REG)
            {
              /* recmatch: を適用する */
              const char* text = dire.d_name;
              unsigned int start_pos = 0;
              unsigned int end_pos = strlen(text);
              MatchLocation_s mlo;
              int result = regexp_search(&mlo, me->recmatch.elements[0].operand,
                                         text, start_pos, end_pos);
              if (result == 0)
                {
                  struct stat sb;
                  sprintf(command_buffer, "%s/.cmdskin.%d/%s",
                          home_env, me_pid, dire.d_name);
                  if ((lstat(command_buffer, &sb) == 0) &&
                      ((sb.st_mode & S_IFLNK) == S_IFLNK))
                    continue;
                  sprintf(command_buffer, "ln -s %s/cmdskin %s/.cmdskin.%d/%s",
                          INSTALL_FULL_SBINDIR, home_env, me_pid, dire.d_name);
                  system(command_buffer);
                }
            }
        }
      closedir(dirp);
    }
  sprintf(command_buffer, "%s/.cmdskin.%d", home_env, me_pid);
  int length = strlen(path_env) + 1 + strlen(command_buffer);
  char* new_path_env = malloc(length + 1);
  if (!new_path_env)
    {
      fprintf(stderr, "Cannot alloc memory.\n");
      exit(EXIT_FAILURE);
    }
  sprintf(new_path_env, "%s:%s", command_buffer, path_env);
  if (setenv("PATH", new_path_env, 1) < 0)
    {
      fprintf(stderr, "Cannot setenv.\n");
      exit(EXIT_FAILURE);
    }
  free(new_path_env);
}

void rec_pattern_setup_for_cmdskin(const RecPattern_s* me, const char* path_env)
{
  char* home_env = getenv("HOME");
  if (!home_env)
    {
      fprintf(stderr, "Cannot fetch `HOME' environment.\n");
      exit(EXIT_FAILURE);
    }
  assert(me->recmatch.number_of_elements == 1);
  assert(me->recmatch.definition_type == DEFINITION_TYPE_RECMATCH);
  assert(me->recmatch.number_of_elements == 1);
  /* reccmd =~ /.../ */
  /* reccmd !~ /.../ */
  assert((me->recmatch.elements[0].operator_type == OPERATOR_TYPE_PARAMETER_MATCHED) ||
         (me->recmatch.elements[0].operator_type == OPERATOR_TYPE_PARAMETER_DIDNT_MATCH));
  assert(me->recmatch.elements[0].parameter_type == PARAMETER_TYPE_RECCMD);

  purge_cmdskin_path(home_env);
  make_cmdskin_link(me, path_env, home_env);
  free(home_env);
}

void rec_pattern_rm_cmdskin_path()
{
  char* home_env = getenv("HOME");
  if (!home_env)
    {
      fprintf(stderr, "Cannot fetch `HOME' environment.\n");
      exit(EXIT_FAILURE);
    }
  pid_t me_pid = getpid();
  char command_buffer[LENGTH_OF_BUFFER];
  sprintf(command_buffer, "rm -rf %s/.cmdskin.%d", home_env, me_pid);
  system(command_buffer);
  free(home_env);
}

/* Local Variables:     */
/* mode: c              */
/* End:                 */
