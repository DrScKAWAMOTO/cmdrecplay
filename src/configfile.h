/*
 * Project: cmdrecplay
 * Version: 1.0
 * Copyright: (C) 2014-2016 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/02 10:15:23 JST
 */

#ifndef __CONFIGFILE_H__
#define __CONFIGFILE_H__

#include <stdio.h>

class ConfigFile {
  FILE* fptr;
  int line_no;
} ConfigFile_s;

extern int configfile_read_line(ConfigFile_s* inpfile, char* buffer, int length);

#endif /* __CONFIGFILE_H__ */

/* Local Variables:	*/
/* mode: c		*/
/* End:			*/
