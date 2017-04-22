/*
 * Project: ifendif
 * Version: 1.0
 * Copyright: (C) 2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2017/03/02 09:44:03 JST
 */

#include <stdlib.h>
#include <stdio.h>

#include "RegionDetector.h"
#include "AtExitUnlink.h"

static char* argv0;

void usage()
{
  printf("usage: %s [options] source\n", argv0);
  printf("cmdplay を呼び出して source をコンパイルした時に無効となる行を調べる\n");
  printf("source:\n");
  printf("\tCソースファイル (拡張子 .c .h)\n");
  printf("\tC++ソースファイル (拡張子 .cc .c++ .cpp .cxx .C .hh .h++ .hpp .hxx)\n");
  printf("options:\n");
  printf("\t-v ....... ifディレクティブの種類も表示する\n");
  printf("\t           I(#if),D(#ifdef),N(#ifndef),L(#else),LI(#elif),E(#endif)\n");
  printf("\t-e ....... cmdplay-ifendif.el 用出力\n");
  printf("\t-d ....... ワークファイルを削除しない\n");
  exit(0);
}

int main(int argc, char* argv[])
{
  bool verbose = false;
  bool if_endif_el = false;
  bool debug = false;
  argv0 = argv[0];
  ++argv;
  --argc;
  while (argc > 1)
    {
      if (strcmp(argv[0], "-v") == 0)
        {
          verbose = true;
          ++argv;
          --argc;
        }
      if (strcmp(argv[0], "-e") == 0)
        {
          if_endif_el = true;
          ++argv;
          --argc;
        }
      if (strcmp(argv[0], "-d") == 0)
        {
          debug = true;
          ++argv;
          --argc;
        }
    }
  if (argc != 1)
    usage();
  if (!debug)
    AtExitUnlink::initialize();
  RegionDetector detector(argv[0], verbose, if_endif_el);
  return 0;
}

/* Local Variables:     */
/* mode: c++            */
/* End:                 */
