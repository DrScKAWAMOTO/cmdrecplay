/*
 * Project: ifendif
 * Version: 1.0
 * Copyright: (C) 2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2017/03/02 22:56:16 JST
 */

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

#include "Expanded.h"
#include "AtExitUnlink.h"

#define COMMAND_BUFFER_LENGTH 1000

Expanded::Expanded(const char* detector, const char* source)
{
  if (realpath(detector, p_realpath) == NULL)
    {
      perror("ifendif realpath");
      exit(1);
    }
  char expanded_filename[PATH_MAX];
  strcpy(expanded_filename, detector);
  char* offset = strrchr(expanded_filename, '.');
  if (!offset)
    offset = expanded_filename + strlen(expanded_filename);
  strcpy(offset, ".e");
  char command_buffer[COMMAND_BUFFER_LENGTH];
  int length = strlen(source);
  offset = strrchr((char*)source, '.');
  if ((length >= 2) && (offset - source + 2 == length) && (strcmp(offset, ".c") == 0))
    sprintf(command_buffer, "cmdplay -f %s -- c-prepro -o %s %s",
            source, expanded_filename, detector);
  else
    sprintf(command_buffer, "cmdplay -f %s -- c++-prepro -o %s %s",
            source, expanded_filename, detector);
  AtExitUnlink aeu(expanded_filename);
  aeu.enter();
  int result = system(command_buffer);
  assert(result == 0);
  p_expanded = fopen(expanded_filename, "r");
  if (!p_expanded)
    {
      printf("ifendif: cmdplay 関連のエラー(%s:%d) !!\n",  __FILE__, __LINE__);
      perror("what ?");
      exit(1);
    }
}

Expanded::~Expanded()
{
  fclose(p_expanded);
}

bool Expanded::read_line()
{
  bool normal_line_skip = false;
  while (1)
    {
      if (read_line_sub())
        return true;
      char header = '\0';
      int line_no = 0;
      char file_name[LINE_BUFFER_LENGTH] = "";
      int number = -1;
      number = sscanf(p_line_buffer, "%c %d \"%s", &header, &line_no, file_name);
      assert(number >= 1);
      if ((header != '#') || (number < 3))
        {
          if (normal_line_skip)
            continue;
          ++p_line_no;
          return false;
        }
      char* ptr = strchr(file_name, '"');
      if (ptr)
        *ptr = '\0';
      if (strcmp(file_name, p_realpath) == 0)
        {
          normal_line_skip = false;
          p_line_no = line_no - 1;
        }
      else
        normal_line_skip = true;
    }
}

bool Expanded::read_line_sub()
{
  if (fgets(p_line_buffer, LINE_BUFFER_LENGTH, p_expanded) == NULL)
    p_is_eof = true;
  else
    p_is_eof = false;
  return p_is_eof;
}

/* Local Variables:     */
/* mode: c++            */
/* End:                 */
