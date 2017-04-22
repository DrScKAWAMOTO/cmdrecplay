/*
 * Project: ifendif
 * Version: 1.0
 * Copyright: (C) 2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2017/03/03 03:05:10 JST
 */

#include <stdlib.h>

#include "Source.h"

Source::Source(const char* source)
{
  if (!source)
    {
      printf("ifendif: ソースファイルが指定されていません !!\n");
      exit(1);
    }
  p_input = fopen(source, "r");
  if (!p_input)
    {
      printf("ifendif: ソースファイル `%s' がみつかりません2 !!\n", source);
      exit(1);
    }
}

Source::~Source()
{
  fclose(p_input);
}

bool Source::read_line()
{
  if (fgets(p_line_buffer, LINE_BUFFER_LENGTH, p_input) == NULL)
    p_is_eof = true;
  else
    {
      p_is_eof = false;
      ++p_line_no;
    }
  return p_is_eof;
}

/* Local Variables:     */
/* mode: c++            */
/* End:                 */
