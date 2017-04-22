/*
 * Project: ifendif
 * Version: 1.0
 * Copyright: (C) 2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2017/03/02 22:35:02 JST
 */

#ifndef __INCLUDED_H__
#define __INCLUDED_H__

#include <stdio.h>
#include <limits.h>

#include "LineBuffer.h"

class Included : public LineBuffer {
  // friend classes & functions

  // members
protected:
  char p_realpath[PATH_MAX];
  FILE* p_included;

  // private tools

  // constructors & the destructor
public:
  Included(const char* source);
  Included(const Included& you); // dont use
  virtual ~Included();

  // type converters

  // comparators

  // math operators

  // I/O
public:
  virtual bool read_line();
  bool read_line_sub();

  // class decision

  // member accessing methods

};

#endif /* __INCLUDED_H__ */

/* Local Variables:     */
/* mode: c++            */
/* End:                 */
