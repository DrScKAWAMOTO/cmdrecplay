/*
 * Project: ifendif
 * Version: 1.0
 * Copyright: (C) 2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2017/03/03 10:59:39 JST
 */

#include <limits.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>
#include <vector>

#include "RegionDetector.h"
#include "Source.h"
#include "Expanded.h"
#include "FillTheSkips.h"
#include "AtExitUnlink.h"

IfDirectiveKind RegionDetector::p_is_if_directive(const char* line)
{
  while (isspace(*line))
    ++line;
  if (*line != '#')
    return IF_DIRECTIVE_KIND_NOT;
  ++line;
  while (isspace(*line))
    ++line;
  if (strncmp(line, "ifndef", 6) == 0)
    {
      if (isspace(line[6]))
        return IF_DIRECTIVE_KIND_IFNDEF;
      else
        return IF_DIRECTIVE_KIND_NOT;
    }
  if (strncmp(line, "ifdef", 5) == 0)
    {
      if (isspace(line[5]))
        return IF_DIRECTIVE_KIND_IFDEF;
      else
        return IF_DIRECTIVE_KIND_NOT;
    }
  if (strncmp(line, "endif", 5) == 0)
    {
      if (isspace(line[5]))
        return IF_DIRECTIVE_KIND_ENDIF;
      else
        return IF_DIRECTIVE_KIND_NOT;
    }
  if (strncmp(line, "else", 4) == 0)
    {
      if (isspace(line[4]))
        return IF_DIRECTIVE_KIND_ELSE;
      else
        return IF_DIRECTIVE_KIND_NOT;
    }
  if (strncmp(line, "elif", 4) == 0)
    {
      if (isspace(line[4]))
        return IF_DIRECTIVE_KIND_ELIF;
      else
        return IF_DIRECTIVE_KIND_NOT;
    }
  if (strncmp(line, "if", 2) == 0)
    {
      if (isspace(line[2]))
        return IF_DIRECTIVE_KIND_IF;
      else
        return IF_DIRECTIVE_KIND_NOT;
    }
  return IF_DIRECTIVE_KIND_NOT;
}

static bool last_visibility;
static int last_from;
static int last_to;

void RegionDetector::p_if_endif_el(bool visibility, int to)
{
  if (last_visibility == visibility)
    {
      if (last_visibility)
        last_to = to;
      else
        {
          printf("(cmdplay-ifendif-gray-out-lines %d %d)\n", last_from + 1, last_to - 1);
          printf("(cmdplay-ifendif-show-lines %d %d)\n", last_to, last_to);
          last_from = last_to;
          last_to = to;
        }
    }
  else
    {
      if (last_visibility)
        printf("(cmdplay-ifendif-show-lines %d %d)\n", last_from, last_to);
      else
        printf("(cmdplay-ifendif-gray-out-lines %d %d)\n", last_from + 1, last_to - 1);
      last_from = last_to;
      last_to = to;
      last_visibility = visibility;
    }
}

void RegionDetector::p_if_endif_el_show_to_end_of_file()
{
  if (!last_visibility)
    {
      printf("(cmdplay-ifendif-gray-out-lines %d %d)\n", last_from + 1, last_to - 1);
      last_from = last_to;
    }
  printf("(cmdplay-ifendif-show-lines-to-end-of-file %d)\n", last_from);
}

RegionDetector::RegionDetector(const char* source, bool verbose, bool if_endif_el)
{
  char detector_file_name[PATH_MAX];
  const char* ptr = strrchr(source, '/');
  int size = 0;
  if (ptr)
    {
      size = ptr - source + 1;
      strncpy(detector_file_name, source, size);
      ptr++;
    }
  else
    ptr = source;
  sprintf(detector_file_name + size, "TK-ifendif-%s", ptr);
  AtExitUnlink aeu(detector_file_name);
  aeu.enter();
  FILE* output = fopen(detector_file_name, "w");
  if (!source)
    { /* TODO */
      printf("ifendif: source が指定されていません !!\n");
      exit(1);
    }
  Source src(source);
  int region_number = 0;
  bool is_after_if_directives = true;
  int line_no = 0;
  while (1)
    {
      ++line_no;
      if (is_after_if_directives)
        {
          p_line_no_of_region_magic_lines.push_back(line_no);
          fprintf(output, "TK-region-%d\n", region_number++);
          is_after_if_directives = false;
          ++line_no;
        }
      if (src.read_line())
        break;
      const char* line = src.line();
      IfDirectiveKind idk = p_is_if_directive(line);
      if (idk != IF_DIRECTIVE_KIND_NOT)
        {
          p_line_no_of_if_directives.push_back(line_no);
          p_kind_of_if_directives.push_back(idk);
          is_after_if_directives = true;
        }
      fprintf(output, "%s", line);
    }
  fclose(output);
  Expanded exp(detector_file_name, source);
  FillTheSkips fts(&exp);
  for (int i = 0; i < p_line_no_of_region_magic_lines.size(); ++i)
    {
      int line_no = p_line_no_of_region_magic_lines[i];
      while (1)
        {
          fts.read_line();
          if (fts.line_number() == line_no)
            break;
        }
      if (strcmp(fts.line(), "\n") == 0)
        p_visibility_of_region_magic_lines.push_back(false);
      else
        p_visibility_of_region_magic_lines.push_back(true);
    }
  size = p_line_no_of_if_directives.size();
  if (if_endif_el)
    {
      last_visibility = true;
      last_from = 1;
      last_to = 1;
      for (int i = 0; i < size; ++i)
        {
          bool visibility = p_visibility_of_region_magic_lines[i];
          int line_no = p_line_no_of_if_directives[i];
          p_if_endif_el(visibility, line_no - i - 1);
        }
      p_if_endif_el_show_to_end_of_file();
    }
  else
    {
      int count = 0;
      for (int i = 0; i < size; ++i)
        {
          bool visibility = p_visibility_of_region_magic_lines[i];
          int line_no = p_line_no_of_if_directives[i];
          if (visibility)
            printf("V,");
          else
            printf("H,");
          printf("%d", line_no - i - 1);
          if (verbose)
            {
              switch (p_kind_of_if_directives[i])
                {
                case IF_DIRECTIVE_KIND_IF:
                  printf("I");
                  break;
                case IF_DIRECTIVE_KIND_IFDEF:
                  printf("D");
                  break;
                case IF_DIRECTIVE_KIND_IFNDEF:
                  printf("N");
                  break;
                case IF_DIRECTIVE_KIND_ELSE:
                  printf("L");
                  break;
                case IF_DIRECTIVE_KIND_ELIF:
                  printf("LI");
                  break;
                case IF_DIRECTIVE_KIND_ENDIF:
                  printf("E");
                  break;
                default:
                  break;
                }
            }
          printf(",");
          count++;
          if (count == 20)
            {
              printf("\n");
              count = 0;
            }
        }
      bool visibility = p_visibility_of_region_magic_lines[size];
      if (visibility)
        printf("V\n");
      else
        printf("H\n");
    }
#if defined(DEBUG)
  {
    int line_count = 0;
    if (!detector_file_name)
      { /* TODO */
        printf("ifendif: detector_file_name が指定されていません !!\n");
        exit(1);
      }
    Source src(detector_file_name);
    FILE* src_out = fopen("source.lst", "w");
    if (!src_out)
      {
        printf("ifendif: ファイル `source.lst' が生成できません !!\n");
        exit(1);
      }
    while (1)
      {
        if (src.read_line())
          break;
        fprintf(src_out, "%3d: %s", src.line_number(), src.line());
        line_count++;
      }
    fclose(src_out);
    Expanded exp(detector_file_name, source);
    FillTheSkips fts(&exp);
    FILE* exp_out = fopen("expanded.lst", "w");
    if (!exp_out)
      {
        printf("ifendif: ファイル `expanded.lst' が生成できません !!\n");
        exit(1);
      }
    for (int i = 0; i < line_count; ++i)
      {
        if (fts.read_line())
          break;
        fprintf(exp_out, "%3d: %s", fts.line_number(), fts.line());
      }
    fclose(exp_out);
  }
#endif
}

RegionDetector::~RegionDetector()
{
}

/* Local Variables:     */
/* mode: c++            */
/* End:                 */
