/*
 * Project: ifendif
 * Version: 1.0
 * Copyright: (C) 2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2017/03/03 02:59:19 JST
 */

#ifndef __SOURCE_H__
#define __SOURCE_H__

#include <stdio.h>

#include "LineBuffer.h"

class Source : public LineBuffer {
  // friend classes & functions

  // members
protected:
  FILE* p_input;

  // private tools

  // constructors & the destructor
public:
  Source(const char* source);
  Source(const Source& you); // dont use
  virtual ~Source();

  // type converters

  // comparators

  // math operators

  // I/O
public:
  virtual bool read_line();

  // class decision

  // member accessing methods

};

#endif /* __SOURCE_H__ */

/* Local Variables:     */
/* mode: c++            */
/* End:                 */
