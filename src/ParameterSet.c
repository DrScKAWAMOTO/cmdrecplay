/*
 * Project: cmdrecplay
 * Version: 1.0
 * Copyright: (C) 2014-2016 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/02 11:08:33 JST
 */

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>

#include "ParameterSet.h"

void parameterSet_init(ParameterSet_s* me)
{
  me->error_detected = 0;
  me->reccmd = NULL;
  me->recfile = NULL;
  me->reccwd = NULL;
  me->recargs = NULL;
  me->playcmd = NULL;
  me->playfile = NULL;
  me->optfile = NULL;
  me->playcwd = NULL;
  me->playargs = NULL;
  me->execcmd = NULL;
  me->execfile = NULL;
  me->execcwd = NULL;
  me->execargs = NULL;
  me->function = NULL;
  me->pid = NULL;
  me->ppid = NULL;
}

void parameterSet_term(ParameterSet_s* me)
{
  me->error_detected = 0;
  if (me->reccmd)
    free(me->reccmd);
  if (me->recfile)
    free(me->recfile);
  if (me->reccwd)
    free(me->reccwd);
  if (me->recargs)
    free(me->recargs);
  if (me->playcmd)
    free(me->playcmd);
  if (me->playfile)
    free(me->playfile);
  if (me->optfile)
    free(me->optfile);
  if (me->playcwd)
    free(me->playcwd);
  if (me->playargs)
    free(me->playargs);
  if (me->execcmd)
    free(me->execcmd);
  if (me->execfile)
    free(me->execfile);
  if (me->execcwd)
    free(me->execcwd);
  if (me->execargs)
    free(me->execargs);
  if (me->function)
    free(me->function);
  if (me->pid)
    free(me->pid);
  if (me->ppid)
    free(me->ppid);
  parameterSet_init(me);
}

void parameterSet_set_by_copy_string(ParameterSet_s* me, ParameterType_e parameter,
                                     const char* value_as_string)
{
  char* ptr = (char*)value_as_string;
  if (ptr)
    {
      ptr = malloc(strlen(value_as_string) + 1);
      strcpy(ptr, value_as_string);
    }
  parameterSet_set_string_value(me, parameter, ptr);
}

void parameterSet_set_by_copy_as_realpath(ParameterSet_s* me, ParameterType_e parameter,
                                          const char* value_as_string)
{
  ParameterType_e cwd_parameter;
  const char* cwd_path;
  char* resolved_path;
  char* work_path;
  switch (parameter)
    {
    case PARAMETER_TYPE_RECFILE:
      cwd_parameter = PARAMETER_TYPE_RECCWD;
      goto add_cwd_to_path;
    case PARAMETER_TYPE_PLAYFILE:
    case PARAMETER_TYPE_OPTFILE:
      cwd_parameter = PARAMETER_TYPE_PLAYCWD;
    add_cwd_to_path:
      cwd_path = parameterSet_refer_string_value(me, cwd_parameter);
      if (value_as_string[0] == '/')
        {
          work_path = malloc(strlen(value_as_string) + 1);
          sprintf(work_path, "%s", value_as_string);
        }
      else
        {
          work_path = malloc(strlen(cwd_path) + 1 + strlen(value_as_string) + 1);
          if (cwd_path[strlen(cwd_path) - 1] == '/')
            sprintf(work_path, "%s%s", cwd_path, value_as_string);
          else
            sprintf(work_path, "%s/%s", cwd_path, value_as_string);
        }
      if (work_path[strlen(work_path) - 1] == '\n')
        work_path[strlen(work_path) - 1] = '\0';
      resolved_path = malloc(PATH_MAX + 1);
      realpath(work_path, resolved_path);
      free(work_path);
      parameterSet_set_string_value(me, parameter, resolved_path);
      return;
    case PARAMETER_TYPE_RECCWD:
    case PARAMETER_TYPE_PLAYCWD:
    case PARAMETER_TYPE_EXECCWD:
      assert(value_as_string[0] == '/');
      resolved_path = malloc(PATH_MAX + 1);
      realpath(value_as_string, resolved_path);
      parameterSet_set_string_value(me, parameter, resolved_path);
      return;
    case PARAMETER_TYPE_RECCMD:
    case PARAMETER_TYPE_PLAYCMD:
    case PARAMETER_TYPE_EXECCMD:
    case PARAMETER_TYPE_RECARGS:
    case PARAMETER_TYPE_PLAYARGS:
    case PARAMETER_TYPE_EXECARGS:
    case PARAMETER_TYPE_FUNCTION:
    case PARAMETER_TYPE_PID:
    case PARAMETER_TYPE_PPID:
    default:
      assert(0);
    }
}

void parameterSet_set_string_value(ParameterSet_s* me, ParameterType_e parameter,
                                   char* value_as_string)
{ /* value_as_string は malloc() したものを渡すこと。開放の責務は me に移管される */
  switch (parameter)
    {
    case PARAMETER_TYPE_RECCMD:
      if (me->reccmd)
        free(me->reccmd);
      me->reccmd = value_as_string;
      break;
    case PARAMETER_TYPE_RECFILE:
      if (me->recfile)
        free(me->recfile);
      me->recfile = value_as_string;
      break;
    case PARAMETER_TYPE_RECCWD:
      if (me->reccwd)
        free(me->reccwd);
      me->reccwd = value_as_string;
      break;
    case PARAMETER_TYPE_RECARGS:
      if (me->recargs)
        free(me->recargs);
      me->recargs = value_as_string;
      break;
    case PARAMETER_TYPE_PLAYCMD:
      if (me->playcmd)
        free(me->playcmd);
      me->playcmd = value_as_string;
      break;
    case PARAMETER_TYPE_PLAYFILE:
      if (me->playfile)
        free(me->playfile);
      me->playfile = value_as_string;
      break;
    case PARAMETER_TYPE_OPTFILE:
      if (me->optfile)
        free(me->optfile);
      me->optfile = value_as_string;
      break;
    case PARAMETER_TYPE_PLAYCWD:
      if (me->playcwd)
        free(me->playcwd);
      me->playcwd = value_as_string;
      break;
    case PARAMETER_TYPE_PLAYARGS:
      if (me->playargs)
        free(me->playargs);
      me->playargs = value_as_string;
      break;
    case PARAMETER_TYPE_EXECCMD:
      if (me->execcmd)
        free(me->execcmd);
      me->execcmd = value_as_string;
      break;
    case PARAMETER_TYPE_EXECCWD:
      if (me->execcwd)
        free(me->execcwd);
      me->execcwd = value_as_string;
      break;
    case PARAMETER_TYPE_EXECARGS:
      if (me->execargs)
        free(me->execargs);
      me->execargs = value_as_string;
      break;
    case PARAMETER_TYPE_FUNCTION:
      if (me->function)
        free(me->function);
      me->function = value_as_string;
      break;
    case PARAMETER_TYPE_PID:
      if (me->pid)
        free(me->pid);
      me->pid = value_as_string;
      break;
    case PARAMETER_TYPE_PPID:
      if (me->ppid)
        free(me->ppid);
      me->ppid = value_as_string;
      break;
    default:
      assert(0);
    }
}

void parameterSet_set_argv_value(ParameterSet_s* me, ParameterType_e parameter,
                                 const Argv_s* value_as_argv)
{
  switch (parameter)
    {
    case PARAMETER_TYPE_RECARGS:
      if (me->recargs)
        free(me->recargs);
      me->recargs = argv_encode_to_string(value_as_argv);
      break;
    case PARAMETER_TYPE_PLAYARGS:
      if (me->playargs)
        free(me->playargs);
      me->playargs = argv_encode_to_string(value_as_argv);
      break;
    case PARAMETER_TYPE_EXECARGS:
      if (me->execargs)
        free(me->execargs);
      me->execargs = argv_encode_to_string(value_as_argv);
      break;
    default:
      assert(0);
    }
}

const char* parameterSet_refer_string_value(const ParameterSet_s* me,
                                            ParameterType_e parameter)
{ /* me の中身を参照して返すので、開放などしてはいけない */
  switch (parameter)
    {
    case PARAMETER_TYPE_RECCMD:
      return me->reccmd;
    case PARAMETER_TYPE_RECFILE:
      return me->recfile;
    case PARAMETER_TYPE_RECCWD:
      return me->reccwd;
    case PARAMETER_TYPE_RECARGS:
      return me->recargs;
    case PARAMETER_TYPE_PLAYCMD:
      return me->playcmd;
    case PARAMETER_TYPE_PLAYFILE:
      return me->playfile;
    case PARAMETER_TYPE_OPTFILE:
      return me->optfile;
    case PARAMETER_TYPE_PLAYCWD:
      return me->playcwd;
    case PARAMETER_TYPE_PLAYARGS:
      return me->playargs;
    case PARAMETER_TYPE_EXECCMD:
      return me->execcmd;
    case PARAMETER_TYPE_EXECCWD:
      return me->execcwd;
    case PARAMETER_TYPE_EXECARGS:
      return me->execargs;
    case PARAMETER_TYPE_FUNCTION:
      return me->function;
    case PARAMETER_TYPE_PID:
      return me->pid;
    case PARAMETER_TYPE_PPID:
      return me->ppid;
    default:
      return NULL;
    }
}

void parameterSet_get_argv_value(Argv_s* result, const ParameterSet_s* me,
                                 ParameterType_e parameter)
{
  switch (parameter)
    {
    case PARAMETER_TYPE_RECARGS:
      argv_decode_from_string(result, me->recargs);
      break;
    case PARAMETER_TYPE_PLAYARGS:
      argv_decode_from_string(result, me->playargs);
      break;
    case PARAMETER_TYPE_EXECARGS:
      argv_decode_from_string(result, me->execargs);
      break;
    default:
      assert(0);
    }
}

void parameterSet_get_execargv(Argv_s* result, const ParameterSet_s* me)
{
  const char* arg0 = me->execcmd;
  const char* argv = me->execargs;
  int length = strlen(arg0) + 1 + strlen(argv);
  char* args = malloc(length + 1);
  strcpy(args, arg0);
  strcat(args, "\n");
  strcat(args, argv);
  argv_decode_from_string(result, args);
  free(args);
}

void parameterSet_copy(ParameterSet_s* result, const ParameterSet_s* me)
{
  ParameterType_e types[] =
    { PARAMETER_TYPE_RECCMD, PARAMETER_TYPE_RECFILE, PARAMETER_TYPE_RECCWD,
      PARAMETER_TYPE_RECARGS, PARAMETER_TYPE_PLAYCMD, PARAMETER_TYPE_PLAYFILE,
      PARAMETER_TYPE_OPTFILE, PARAMETER_TYPE_PLAYCWD, PARAMETER_TYPE_PLAYARGS,
      PARAMETER_TYPE_EXECCMD, PARAMETER_TYPE_EXECCWD, PARAMETER_TYPE_EXECARGS,
      PARAMETER_TYPE_FUNCTION, PARAMETER_TYPE_PID, PARAMETER_TYPE_PPID };
  const int size = sizeof(types) / sizeof(ParameterType_e);
  parameterSet_term(result);
  for (int offset = 0; offset < size; ++offset)
    parameterSet_set_by_copy_string(result, types[offset],
                                    parameterSet_refer_string_value(me, types[offset]));
}

void parameterSet_print(ParameterSet_s* me)
{
  if (me->error_detected)
    {
      fprintf(stderr, "error detected\n");
      return;
    }
  if (me->reccmd)
    fprintf(stderr, "reccmd = '%s'\n", me->reccmd);
  if (me->recfile)
    fprintf(stderr, "recfile = '%s'\n", me->recfile);
  if (me->reccwd)
    fprintf(stderr, "reccwd = '%s'\n", me->reccwd);
  if (me->recargs)
    fprintf(stderr, "recargs = '%s'\n", me->recargs);
  if (me->playcmd)
    fprintf(stderr, "playcmd = '%s'\n", me->playcmd);
  if (me->playfile)
    fprintf(stderr, "playfile = '%s'\n", me->playfile);
  if (me->optfile)
    fprintf(stderr, "optfile = '%s'\n", me->optfile);
  if (me->playcwd)
    fprintf(stderr, "playcwd = '%s'\n", me->playcwd);
  if (me->playargs)
    fprintf(stderr, "playargs = '%s'\n", me->playargs);
  if (me->execcmd)
    fprintf(stderr, "execcmd = '%s'\n", me->execcmd);
  if (me->execfile)
    fprintf(stderr, "execfile = '%s'\n", me->execfile);
  if (me->execcwd)
    fprintf(stderr, "execcwd = '%s'\n", me->execcwd);
  if (me->execargs)
    fprintf(stderr, "execargs = '%s'\n", me->execargs);
  if (me->function)
    fprintf(stderr, "function = '%s'\n", me->function);
  if (me->pid)
    fprintf(stderr, "pid = '%s'\n", me->pid);
  if (me->ppid)
    fprintf(stderr, "ppid = '%s'\n", me->ppid);
}

/* Local Variables:	*/
/* mode: c		*/
/* End:			*/
