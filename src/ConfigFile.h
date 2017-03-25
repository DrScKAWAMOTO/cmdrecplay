/*
 * Project: cmdrecplay
 * Version: 1.1
 * Copyright: (C) 2014-2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/02 10:15:23 JST
 */

#ifndef __CONFIGFILE_H__
#define __CONFIGFILE_H__

#include <stdio.h>

#define LENGTH_OF_BUFFER 256
#define LENGTH_OF_PATH_BUFFER 1024

typedef enum {
  TOKEN_UNKNOWN = 0,
  TOKEN_REC,
  TOKEN_PLAY,
  TOKEN_PATTERN,
  TOKEN_OPEN,
  TOKEN_CLOSE,
  TOKEN_LABEL_RECFILE,
  TOKEN_LABEL_RECMATCH,
  TOKEN_LABEL_PLAYFILE,
  TOKEN_LABEL_PLAYMATCH,
  TOKEN_LABEL_EXECCWD,
  TOKEN_LABEL_EXECCMD,
  TOKEN_LABEL_EXECARGS,
  TOKEN_LABEL_EXECENVS,
  TOKEN_PARAMETER_RECCMD,
  TOKEN_PARAMETER_RECFILE,
  TOKEN_PARAMETER_RECCWD,
  TOKEN_PARAMETER_RECARGS,
  TOKEN_PARAMETER_PLAYCMD,
  TOKEN_PARAMETER_PLAYFILE,
  TOKEN_PARAMETER_OPTFILE,
  TOKEN_PARAMETER_PLAYCWD,
  TOKEN_PARAMETER_PLAYARGS,
  TOKEN_PARAMETER_EXECCMD,
  TOKEN_PARAMETER_EXECCWD,
  TOKEN_PARAMETER_EXECARGS,
  TOKEN_MATCH,
  TOKEN_NOT_MATCH,
  TOKEN_EQUALS,
  TOKEN_ASSIGNMENT,
  TOKEN_STRING,
  TOKEN_REGEXP,
} Token_e;

typedef struct {
  char config_file_name[LENGTH_OF_BUFFER + 1];
  FILE* fptr;
  int line_no;
  char line[LENGTH_OF_BUFFER + 1];
  int length;
  int offset;
  int rewinded;
  char word[LENGTH_OF_BUFFER + 1];
  Token_e token;
} ConfigFile_s;

extern void config_file_init(ConfigFile_s* me, const char* config_file_name);
extern void config_file_term(ConfigFile_s* me);
extern int config_file_next_word_or_end_of_file(ConfigFile_s* me);
extern void config_file_next_word(ConfigFile_s* me);
extern void config_file_rewind(ConfigFile_s* me);
extern void config_file_read_keyword(ConfigFile_s* me, const char* keyword);

#endif /* __CONFIGFILE_H__ */

/* Local Variables:     */
/* mode: c              */
/* End:                 */
