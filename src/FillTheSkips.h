/*
 * Project: ifendif
 * Version: 1.0
 * Copyright: (C) 2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2017/03/03 05:33:17 JST
 */

#ifndef __FILLTHESKIPS_H__
#define __FILLTHESKIPS_H__

#include "LineBuffer.h"

class FillTheSkips : public LineBuffer {
  // friend classes & functions

  // members
protected:
  LineBuffer* p_parent;

  // private tools

  // constructors & the destructor
public:
  FillTheSkips(LineBuffer* parent);
  FillTheSkips(const FillTheSkips& you); // dont use
  virtual ~FillTheSkips();

  // type converters

  // comparators

  // math operators

  // I/O
public:
  virtual bool read_line();

  // class decision

  // member accessing methods

};

#endif /* __FILLTHESKIPS_H__ */

/* Local Variables:     */
/* mode: c++            */
/* End:                 */
