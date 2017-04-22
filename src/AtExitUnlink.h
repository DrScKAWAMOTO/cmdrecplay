/*
 * Project: ifendif
 * Version: 1.0
 * Copyright: (C) 2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2017/03/05 08:40:11 JST
 */

#ifndef __ATEXITUNLINK_H__
#define __ATEXITUNLINK_H__

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <signal.h>
#include <vector>

class AtExitUnlink {
  // friend classes & functions

  // members
protected:
  static void s_at_exit_function();
  static std::vector<AtExitUnlink> s_exits;
  static void s_signal_handler(int sig, siginfo_t *info, void *uap);
  char p_filename[PATH_MAX];

  // private tools

  // constructors & the destructor
public:
  AtExitUnlink();
  AtExitUnlink(const char* filename);
  AtExitUnlink(const AtExitUnlink& you);
  AtExitUnlink& operator=(const AtExitUnlink& you);
  ~AtExitUnlink();

  // type converters

  // comparators

  // math operators

  // I/O
public:
  static void initialize();
  void enter() const;

  // class decision

  // member accessing methods

};

#endif /* __ATEXITUNLINK_H__ */

/* Local Variables:     */
/* mode: c++            */
/* End:                 */
