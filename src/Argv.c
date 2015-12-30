/*
 * Project: cmdrecplay
 * Version: 1.0
 * Copyright: (C) 2014-2016 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/02 11:08:33 JST
 */

#include <string.h>
#include <stdlib.h>

#include "Argv.h"

void argv_init(Argv_s* me, char* argv[])
{
  int argc;
  for (argc = 0; argv[argc]; ++argc)
    ;
  me->argv = malloc(sizeof(char*) * (argc + 1));
  for (int offset = 0; offset < argc; ++offset)
    {
      me->argv[offset] = malloc(strlen(argv[offset]) + 1);
      strcpy(me->argv[offset], argv[offset]);
    }
}

void argv_term(Argv_s* me)
{
  if (me->argv)
    {
      for (int offset = 0; me->argv[offset]; ++offset)
        free(me->argv[offset]);
      free(me->argv);
    }
  me->argv = NULL;
}

void argv_decode_from_string(Argv_s* me, const char* from)
{
  int argc = 0;
  const char* ptr;
  for (ptr = from; *ptr; ++ptr)
    if (*ptr == '\n')
      ++argc;
  argv_term(me);
  me->argv = malloc(sizeof(char*) * (argc + 1));
  ptr = from;
  for (int offset = 0; offset < argc; ++offset)
    {
      const char* top = ptr;
      while (*ptr != '\n')
        ++ptr;
      me->argv[offset] = malloc(ptr - top + 1);
      strncpy(me->argv[offset], top, ptr - top);
      me->argv[offset][ptr - top] = '\0';
      ++ptr;
    }
  me->argv[argc] = NULL;
}

char* argv_encode_to_string(const Argv_s* me)
{ /* 返り値は malloc() された領域 */
  int length = 1;
  char* top;
  char* ptr;

  for (int offset = 0; me->argv[offset]; ++offset)
    length += strlen(me->argv[offset]) + 1;
  ptr = top = malloc(length);
  for (int offset = 0; me->argv[offset]; ++offset)
    {
      strcpy(ptr, me->argv[offset]);
      ptr += strlen(ptr);
      *ptr++ = '\n';
    }
  *ptr = '\0';
  return top;
}

/* Local Variables:	*/
/* mode: c		*/
/* End:			*/
