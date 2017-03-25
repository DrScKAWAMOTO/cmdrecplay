/*
 * Project: cmdrecplay
 * Version: 1.1
 * Copyright: (C) 2014-2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/01 23:55:21 JST
 */

#ifndef __ARGV_H__
#define __ARGV_H__

#include "ConfigFile.h"

typedef struct {
  char** argv;
} Argv_s;

extern void argv_init(Argv_s* me, char* argv[]);
extern void argv_term(Argv_s* me);
extern void argv_decode_from_string(Argv_s* me, const char* from);
/* argv_encode_to_string() */
/* 返り値は malloc() された領域 */
extern char* argv_encode_to_string(const Argv_s* me);

#endif /* __ARGV_H__ */

/* Local Variables:     */
/* mode: c              */
/* End:                 */
