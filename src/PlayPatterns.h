/*
 * Project: cmdrecplay
 * Version: 1.1
 * Copyright: (C) 2014-2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/01 23:55:21 JST
 */

#ifndef __PLAYPATTERNS_H__
#define __PLAYPATTERNS_H__

#include "ConfigFile.h"
#include "ParameterSet.h"
#include "Pattern.h"

typedef struct {
  char* pattern_name;
  int line_no;
  Definition_s playfile;
  Definition_s playmatch;
  Definition_s execcwd;
  Definition_s execcmd;
  Definition_s execargs;
  Definition_s execenvs;
} PlayPattern_s;

typedef struct {
  int number_of_patterns;
  PlayPattern_s patterns[NUMBER_OF_PATTERNS];
} PlayPatterns_s;

extern void play_patterns_init(PlayPatterns_s* me);
extern void play_patterns_term(PlayPatterns_s* me);
extern void play_patterns_read_config(PlayPatterns_s* me, ConfigFile_s* inpfile);
extern int play_patterns_apply(const PlayPatterns_s* me, ParameterSet_s* parameter_set);
extern void play_patterns_print(PlayPatterns_s* me);

#endif /* __PLAYPATTERNS_H__ */

/* Local Variables:     */
/* mode: c              */
/* End:                 */
