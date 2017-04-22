/*
 * Project: ifendif
 * Version: 1.0
 * Copyright: (C) 2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2017/03/02 22:39:47 JST
 */

#ifndef __LINEBUFFER_H__
#define __LINEBUFFER_H__

#define LINE_BUFFER_LENGTH 1000

class LineBuffer {
  // friend classes & functions

  // members
protected:
  bool p_is_eof;
  int p_line_no;
  char p_line_buffer[LINE_BUFFER_LENGTH];

  // private tools

  // constructors & the destructor
public:
  LineBuffer();
  LineBuffer(const LineBuffer& you); // dont use
  virtual ~LineBuffer();

  // type converters

  // comparators

  // math operators

  // I/O
public:
  virtual bool read_line() = 0;

  // class decision

  // member accessing methods
public:
  bool is_eof() const { return p_is_eof; }
  int line_number() const { return p_line_no; }
  const char* line() const { return p_line_buffer; }

};

#endif /* __LINEBUFFER_H__ */

/* Local Variables:     */
/* mode: c++            */
/* End:                 */
