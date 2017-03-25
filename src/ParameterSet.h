/*
 * Project: cmdrecplay
 * Version: 1.1
 * Copyright: (C) 2014-2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/01 23:55:21 JST
 */

#ifndef __PARAMETERSET_H__
#define __PARAMETERSET_H__

#include "ConfigFile.h"
#include "Pattern.h"
#include "Argv.h"

typedef struct {
  int error_detected;
  char *reccmd;
  char *recfile;
  char *reccwd;
  char *recargs;
  char *playcmd;
  char *playfile;
  char *optfile;
  char *playcwd;
  char *playargs;
  char *execcmd;
  char *execfile;
  char *execcwd;
  char *execargs;
  char *function;
  char *pid;
  char *ppid;
} ParameterSet_s;

extern void parameterSet_init(ParameterSet_s* me);
extern void parameterSet_term(ParameterSet_s* me);
extern void parameterSet_set_by_copy_string(ParameterSet_s* me,
                                            ParameterType_e parameter,
                                            const char* value_as_string);
extern void parameterSet_set_by_copy_as_realpath(ParameterSet_s* me,
                                                 ParameterType_e parameter,
                                                 const char* value_as_string);
/* parameterSet_set_string_value() */
/* value_as_string は malloc() したものを渡すこと。開放の責務は me に移管される */
extern void parameterSet_set_string_value(ParameterSet_s* me, ParameterType_e parameter,
                                          char* value_as_string);
extern void parameterSet_set_argv_value(ParameterSet_s* me, ParameterType_e parameter,
                                        const Argv_s* value_as_argv);
/* parameterSet_refer_string_value() */
/* me の中身を参照して返すので、開放などしてはいけない */
extern const char* parameterSet_refer_string_value(const ParameterSet_s* me,
                                                   ParameterType_e parameter);
extern void parameterSet_get_argv_value(Argv_s* result, const ParameterSet_s* me,
                                        ParameterType_e parameter);
extern void parameterSet_get_execargv(Argv_s* result, const ParameterSet_s* me);
extern void parameterSet_copy(ParameterSet_s* result, const ParameterSet_s* me);
extern void parameterSet_print(ParameterSet_s* me);

#endif /* __PARAMETERSET_H__ */

/* Local Variables:     */
/* mode: c              */
/* End:                 */
