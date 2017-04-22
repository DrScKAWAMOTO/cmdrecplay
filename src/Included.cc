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

#include "Included.h"
#include "AtExitUnlink.h"

#define COMMAND_BUFFER_LENGTH 1000

Included::Included(const char* source)
{
  if (realpath(source, p_realpath) == NULL)
    {
      perror("ifendif realpath");
      exit(1);
    }
  char output_filename[PATH_MAX];
  strcpy(output_filename, p_realpath);
  strcat(output_filename, ".e");
  char command_buffer[COMMAND_BUFFER_LENGTH];
  int length = strlen(source);
  const char* offset = strrchr(source, '.');
  if ((length >= 2) && (offset - source + 2 == length) && (strcmp(offset, ".c") == 0))
    sprintf(command_buffer, "cmdplay -f %s -- c-prepro -o %s %s",
            source, output_filename, source);
  else
    sprintf(command_buffer, "cmdplay -f %s -- c++-prepro -o %s %s",
            source, output_filename, source);
  AtExitUnlink aeu(output_filename);
  aeu.enter();
  system(command_buffer);
  p_included = fopen(output_filename, "r");
  if (!p_included)
    {
      printf("ifendif: cmdplay 関連のエラー(%s:%d) !!\n",  __FILE__, __LINE__);
      perror("what ?");
      exit(1);
    }
}

Included::~Included()
{
  fclose(p_included);
}

bool Included::read_line()
{
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
        continue;
      char* ptr = strchr(file_name, '"');
      if (ptr)
        *ptr = '\0';
      strcpy(p_line_buffer, file_name);
      return false;
    }
}

bool Included::read_line_sub()
{
  if (fgets(p_line_buffer, LINE_BUFFER_LENGTH, p_included) == NULL)
    p_is_eof = true;
  else
    p_is_eof = false;
  return p_is_eof;
}

/* Local Variables:     */
/* mode: c++            */
/* End:                 */
