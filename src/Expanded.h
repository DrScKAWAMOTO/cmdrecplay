/*
 * Project: ifendif
 * Version: 1.0
 * Copyright: (C) 2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2017/03/02 22:35:02 JST
 */

#ifndef __EXPANDED_H__
#define __EXPANDED_H__

#include <stdio.h>
#include <limits.h>

#include "LineBuffer.h"

class Expanded : public LineBuffer {
  // friend classes & functions

  // members
protected:
  char p_realpath[PATH_MAX];
  FILE* p_expanded;

  // private tools

  // constructors & the destructor
public:
  Expanded(const char* detector, const char* source);
  Expanded(const Expanded& you); // dont use
  virtual ~Expanded();

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

#endif /* __EXPANDED_H__ */

/* Local Variables:     */
/* mode: c++            */
/* End:                 */
