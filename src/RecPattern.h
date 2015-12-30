/*
 * Project: cmdrecplay
 * Version: 1.0
 * Copyright: (C) 2014-2016 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/01 23:55:21 JST
 */

#ifndef __RECPATTERN_H__
#define __RECPATTERN_H__

#include "ConfigFile.h"
#include "ParameterSet.h"
#include "Pattern.h"

typedef struct {
  Definition_s recfile;
  Definition_s recmatch;
} RecPattern_s;

extern void rec_pattern_init(RecPattern_s* me);
extern void rec_pattern_term(RecPattern_s* me);
extern void rec_pattern_read_config(RecPattern_s* me, ConfigFile_s* inpfile);
extern int rec_pattern_apply(const RecPattern_s* me, ParameterSet_s* parameter_set);
extern void rec_pattern_print(RecPattern_s* me);

#endif /* __RECPATTERN_H__ */

/* Local Variables:	*/
/* mode: c		*/
/* End:			*/
