/*
 * Project: cmdrecplay
 * Version: 1.0
 * Copyright: (C) 2014-2016 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/02 10:48:37 JST
 */

#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <stdlib.h>

#include "ConfigFile.h"

static int config_file_next_line(ConfigFile_s* me);

void config_file_init(ConfigFile_s* me, const char* config_file_name)
{
  assert(strlen(config_file_name) <= LENGTH_OF_BUFFER);
  strcpy(me->config_file_name, config_file_name);
  me->fptr = fopen(config_file_name, "r");
  if (me->fptr == NULL)
    {
    cannot_read_config_file:
      fprintf(stderr, "Cannot read config file `%s'.\n", config_file_name);
      exit(EXIT_FAILURE);
    }
  me->line_no = 0;
  me->length = 0;
  me->offset = 0;
  me->rewinded = 0;
  if (config_file_next_line(me) < 0)
    goto cannot_read_config_file;
}

void config_file_term(ConfigFile_s* me)
{
  if (me->fptr)
    fclose(me->fptr);
  me->fptr = NULL;
  me->line_no = 0;
  me->length = 0;
  me->offset = 0;
  me->rewinded = 0;
}

int config_file_next_word_or_end_of_file(ConfigFile_s* me)
{
  char* word_ptr;
  if (me->rewinded)
    {
      me->rewinded = 0;
      return 0;
    }
  while ((me->length == me->offset) || (me->line[me->offset] == '#'))
    {
      if (config_file_next_line(me) < 0)
        return -1;
    }
  while (isspace(me->line[me->offset]))
    { /* skip spaces */
      me->offset++;
      while ((me->length == me->offset) || (me->line[me->offset] == '#'))
        {
          if (config_file_next_line(me) < 0)
            return -1;
        }
    }
  word_ptr = me->word;
  me->token = TOKEN_UNKNOWN;
  if ((me->line[me->offset] == '\'') || (me->line[me->offset] == '/'))
    {
      int start_char = me->line[me->offset];
      me->token = (start_char == '/') ? TOKEN_REGEXP : TOKEN_STRING;
      me->offset++;
      while (me->line[me->offset] != start_char)
        {
          if (me->line[me->offset] == '\0')
            {
              *word_ptr = '\0';
              fprintf(stderr, "%s:%d: `%s' illegal %s.\n",
                      me->config_file_name, me->line_no, me->word,
                      (start_char == '/') ? "regular expression" : "string");
              exit(EXIT_FAILURE);
            }
          if (me->line[me->offset] == '\\')
            {
              *word_ptr++ = me->line[me->offset];
              me->offset++;
            }
          *word_ptr++ = me->line[me->offset];
          me->offset++;
        }
      me->offset++;
      *word_ptr = '\0';
    }
  else
    {
      while (!isspace(me->line[me->offset]))
        {
          if (me->line[me->offset] == '\0')
            break;
          *word_ptr++ = me->line[me->offset];
          me->offset++;
        }
      *word_ptr = '\0';
      if (strcmp(me->word, "rec") == 0)
        me->token = TOKEN_REC;
      else if (strcmp(me->word, "play") == 0)
        me->token = TOKEN_PLAY;
      else if (strcmp(me->word, "pattern") == 0)
        me->token = TOKEN_PATTERN;
      else if (strcmp(me->word, "{") == 0)
        me->token = TOKEN_OPEN;
      else if (strcmp(me->word, "}") == 0)
        me->token = TOKEN_CLOSE;
      else if (strcmp(me->word, "recfile:") == 0)
        me->token = TOKEN_LABEL_RECFILE;
      else if (strcmp(me->word, "recmatch:") == 0)
        me->token = TOKEN_LABEL_RECMATCH;
      else if (strcmp(me->word, "playfile:") == 0)
        me->token = TOKEN_LABEL_PLAYFILE;
      else if (strcmp(me->word, "playmatch:") == 0)
        me->token = TOKEN_LABEL_PLAYMATCH;
      else if (strcmp(me->word, "execcmd:") == 0)
        me->token = TOKEN_LABEL_EXECCMD;
      else if (strcmp(me->word, "execcwd:") == 0)
        me->token = TOKEN_LABEL_EXECCWD;
      else if (strcmp(me->word, "execargs:") == 0)
        me->token = TOKEN_LABEL_EXECARGS;
      else if (strcmp(me->word, "execenvs:") == 0)
        me->token = TOKEN_LABEL_EXECENVS;
      else if (strcmp(me->word, "reccmd") == 0)
        me->token = TOKEN_PARAMETER_RECCMD;
      else if (strcmp(me->word, "recfile") == 0)
        me->token = TOKEN_PARAMETER_RECFILE;
      else if (strcmp(me->word, "reccwd") == 0)
        me->token = TOKEN_PARAMETER_RECCWD;
      else if (strcmp(me->word, "recargs") == 0)
        me->token = TOKEN_PARAMETER_RECARGS;
      else if (strcmp(me->word, "playcmd") == 0)
        me->token = TOKEN_PARAMETER_PLAYCMD;
      else if (strcmp(me->word, "playfile") == 0)
        me->token = TOKEN_PARAMETER_PLAYFILE;
      else if (strcmp(me->word, "optfile") == 0)
        me->token = TOKEN_PARAMETER_OPTFILE;
      else if (strcmp(me->word, "playcwd") == 0)
        me->token = TOKEN_PARAMETER_PLAYCWD;
      else if (strcmp(me->word, "playargs") == 0)
        me->token = TOKEN_PARAMETER_PLAYARGS;
      else if (strcmp(me->word, "execcmd") == 0)
        me->token = TOKEN_PARAMETER_EXECCMD;
      else if (strcmp(me->word, "execcwd") == 0)
        me->token = TOKEN_PARAMETER_EXECCWD;
      else if (strcmp(me->word, "execargs") == 0)
        me->token = TOKEN_PARAMETER_EXECARGS;
      else if (strcmp(me->word, "=~") == 0)
        me->token = TOKEN_MATCH;
      else if (strcmp(me->word, "!~") == 0)
        me->token = TOKEN_NOT_MATCH;
      else if (strcmp(me->word, "==") == 0)
        me->token = TOKEN_EQUALS;
      else if (strcmp(me->word, "=") == 0)
        me->token = TOKEN_ASSIGNMENT;
    }
  return 0;
}

void config_file_next_word(ConfigFile_s* me)
{
  if (config_file_next_word_or_end_of_file(me) < -1)
    {
      fprintf(stderr, "%s:%d: end of file, illegal format.\n",
              me->config_file_name, me->line_no);
      exit(EXIT_FAILURE);
    }
}

static int config_file_next_line(ConfigFile_s* me)
{
  if (me->fptr == NULL)
    return -1;
  me->length = 0;
  while (me->length == 0)
    {
      char* ptr;

      ptr = fgets(me->line, LENGTH_OF_BUFFER + 1, me->fptr);
      if (ptr == NULL)
        return -1;
      ++me->line_no;
      me->length = strlen(me->line);
      if ((me->length > 0) && (me->line[me->length - 1] == '\n'))
        me->line[--(me->length)] = '\0';
    }
  me->offset = 0;
  return 0;
}

void config_file_rewind(ConfigFile_s* me)
{
  me->rewinded = 1;
}

void config_file_read_keyword(ConfigFile_s* me, const char* keyword)
{
  config_file_next_word(me);
  if (strcmp(me->word, keyword) != 0)
    {
      fprintf(stderr, "%s:%d: `%s' required but `%s'.\n",
              me->config_file_name, me->line_no, keyword, me->word);
      exit(EXIT_FAILURE);
    }
}

/* Local Variables:	*/
/* mode: c		*/
/* End:			*/
