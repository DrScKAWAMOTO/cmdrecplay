/*
 * Project: ifendif
 * Version: 1.0
 * Copyright: (C) 2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2017/03/03 05:38:45 JST
 */

#include <string.h>

#include "FillTheSkips.h"

FillTheSkips::FillTheSkips(LineBuffer* parent)
  : p_parent(parent)
{
  p_parent->read_line();
}

FillTheSkips::~FillTheSkips()
{
  delete p_parent;
}

bool FillTheSkips::read_line()
{
  ++p_line_no;
  if (p_parent->is_eof())
    {
      strcpy(p_line_buffer, "\n");
      return false;
    }
  if (p_parent->line_number() > p_line_no)
    {
      strcpy(p_line_buffer, "\n");
      return false;
    }
  strcpy(p_line_buffer, p_parent->line());
  p_parent->read_line();
  return false;
}

/* Local Variables:     */
/* mode: c++            */
/* End:                 */
