/*
 * Project: ifendif
 * Version: 1.0
 * Copyright: (C) 2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2017/03/02 09:44:03 JST
 */

#include <stdlib.h>
#include <stdio.h>
#include <set>
#include <string>

#include "Included.h"
#include "AtExitUnlink.h"
extern "C" {
#include "SrcDatabase.h"
};

static char* argv0;

void usage()
{
  printf("usage: %s [options] source\n", argv0);
  printf("cmdplay を呼び出して source からインクルードされたファイルを一覧する\n");
  printf("source:\n");
  printf("\tCソースファイル (拡張子 .c)\n");
  printf("\tC++ソースファイル (拡張子 .cc .c++ .cpp .cxx .C)\n");
  printf("options:\n");
  printf("\t-d ....... ワークファイルを削除しない\n");
  printf("\t-q ....... 一覧しない(データベース更新のみ)\n");
  exit(0);
}

int main(int argc, char* argv[])
{
  bool debug = false;
  bool quiet = false;
  argv0 = argv[0];
  ++argv;
  --argc;
  while (argc >= 1)
    {
      if (strcmp(argv[0], "-d") == 0)
        {
          debug = true;
          ++argv;
          --argc;
        }
      else if (strcmp(argv[0], "-q") == 0)
        {
          quiet = true;
          ++argv;
          --argc;
        }
      else if (argv[0][0] == '-')
        usage();
      else
        break;
    }
  if (argc != 1)
    usage();
  if (!debug)
    AtExitUnlink::initialize();
  Included inc(argv[0]);
  std::set<std::string> files;
  std::string source;
  bool first_time = true;
  while (1)
    {
      if (inc.read_line())
        break;
      if (inc.line()[0] != '/')
        continue;
      if (strncmp(inc.line(), "/usr/include", 12) == 0)
        continue;
      if (strncmp(inc.line(), "/usr/local/include", 18) == 0)
        continue;
      char real_path[PATH_MAX];
      if (realpath(inc.line(), real_path) == NULL)
        {
          perror("srcinc realpath");
          exit(1);
        }
      if (first_time)
        source = real_path;
      first_time = false;
      const char* offset = strrchr(real_path, '.');
      if (offset && ((offset[1] == 'c') || (offset[1] == 'C')))
        continue;
      if (files.find(real_path) == files.end())
        {
          files.insert(real_path);
          if (!quiet)
            printf("%s\n", real_path);
        }
    }
  std::set<std::string>::const_iterator itr = files.begin();
  while (itr != files.end())
    database_hedsrc((itr++)->c_str(), source.c_str());
  return 0;
}

/* Local Variables:     */
/* mode: c++            */
/* End:                 */
