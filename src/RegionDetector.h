/*
 * Project: ifendif
 * Version: 1.0
 * Copyright: (C) 2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2017/03/03 10:57:56 JST
 */

#ifndef __REGIONDETECTOR_H__
#define __REGIONDETECTOR_H__

#include <stdio.h>
#include <vector>

enum IfDirectiveKind {
  IF_DIRECTIVE_KIND_IF,
  IF_DIRECTIVE_KIND_IFDEF,
  IF_DIRECTIVE_KIND_IFNDEF,
  IF_DIRECTIVE_KIND_ELSE,
  IF_DIRECTIVE_KIND_ELIF,
  IF_DIRECTIVE_KIND_ENDIF,
  IF_DIRECTIVE_KIND_NOT,
};


class RegionDetector {
  // friend classes & functions

  // members
protected:
  std::vector<int> p_line_no_of_if_directives;
  std::vector<int> p_kind_of_if_directives;
  std::vector<int> p_line_no_of_region_magic_lines;
  std::vector<bool> p_visibility_of_region_magic_lines;

  // private tools
protected:
  static IfDirectiveKind p_is_if_directive(const char* buffer);
  void p_if_endif_el_pre();
  void p_if_endif_el(bool visibility, int to);
  void p_if_endif_el_show_to_end_of_file();

  // constructors & the destructor
public:
  RegionDetector(const char* source, bool verbose, bool if_endif_el);
  RegionDetector(const RegionDetector& you); // dont use
  ~RegionDetector();

  // type converters

  // comparators

  // math operators

  // I/O

  // class decision

  // member accessing methods

};

#endif /* __REGIONDETECTOR_H__ */

/* Local Variables:     */
/* mode: c++            */
/* End:                 */
