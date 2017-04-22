/*
 * Project: ifendif
 * Version: 1.0
 * Copyright: (C) 2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2017/03/02 22:49:00 JST
 */

#include <assert.h>
#include <string.h>
#include <stdio.h>

#include "LineBuffer.h"

LineBuffer::LineBuffer()
  : p_is_eof(false), p_line_no(0)
{
  p_line_buffer[0] = 0;
}

LineBuffer::~LineBuffer()
{
}

/* Local Variables:     */
/* mode: c++            */
/* End:                 */
