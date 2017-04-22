/*
 * Project: ifendif
 * Version: 1.0
 * Copyright: (C) 2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2017/03/05 08:46:11 JST
 */

#include <stdlib.h>
#include <unistd.h>

#include "AtExitUnlink.h"

void AtExitUnlink::s_at_exit_function()
{
  int size = s_exits.size();
  for (int i = 0; i < size; ++i)
  {
    const AtExitUnlink& aeu = s_exits[i];
    unlink(aeu.p_filename);
  }
}

std::vector<AtExitUnlink> AtExitUnlink::s_exits;

void AtExitUnlink::s_signal_handler(int sig, siginfo_t *info, void *uap)
{
  exit(1);
}

AtExitUnlink::AtExitUnlink()
{
  strcpy(p_filename, "");
}

AtExitUnlink::AtExitUnlink(const char* filename)
{
  strcpy(p_filename, filename);
}

AtExitUnlink::AtExitUnlink(const AtExitUnlink& you)
{
  strcpy(p_filename, you.p_filename);
}

AtExitUnlink& AtExitUnlink::operator=(const AtExitUnlink& you)
{
  if (&you != this)
  {
    strcpy(p_filename, you.p_filename);
  }
  return *this;
}

AtExitUnlink::~AtExitUnlink()
{
}

void AtExitUnlink::initialize()
{
#if !defined(DEBUG)
  struct sigaction act;

  act.sa_sigaction = (void (*)(int, siginfo_t *, void *))s_signal_handler;
  act.sa_flags = SA_RESTART | SA_SIGINFO;
  sigaction(SIGHUP, &act, 0);
  act.sa_sigaction = (void (*)(int, siginfo_t *, void *))s_signal_handler;
  act.sa_flags = SA_RESTART | SA_SIGINFO;
  sigaction(SIGINT, &act, 0);
  act.sa_sigaction = (void (*)(int, siginfo_t *, void *))s_signal_handler;
  act.sa_flags = SA_RESTART | SA_SIGINFO;
  sigaction(SIGTERM, &act, 0);
  act.sa_sigaction = (void (*)(int, siginfo_t *, void *))s_signal_handler;
  act.sa_flags = SA_RESTART | SA_SIGINFO;
  sigaction(SIGALRM, &act, 0);

  atexit(s_at_exit_function);
#endif
}

void AtExitUnlink::enter() const
{
#if !defined(DEBUG)
  s_exits.push_back(*this);
#endif
}

/* Local Variables:     */
/* mode: c++            */
/* End:                 */
