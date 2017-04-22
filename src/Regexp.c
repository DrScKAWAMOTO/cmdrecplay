/*
 * Project: cmdrecplay
 * Version: 1.1
 * Copyright: (C) 2014-2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/03 22:42:34 JST
 */

#include <oniguruma.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "Regexp.h"

typedef struct {
  regex_t* reg;
} regexp_s;

static void regexp_init(regexp_s* internal, const char* pattern)
{
  int ret;
  const unsigned char* upat = (const unsigned char*)pattern;
  const unsigned char* upat_end = upat + strlen(pattern);
  OnigErrorInfo error_info;
  ret = onig_new(&(internal->reg), upat, upat_end, ONIG_OPTION_NONE,
                 ONIG_ENCODING_ASCII, ONIG_SYNTAX_RUBY, &error_info);
  if (ret != ONIG_NORMAL)
    {
      fprintf(stderr, "onig_new() error (%d)\n", ret);
      exit(EXIT_FAILURE);
    }
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 4
  fprintf(stderr, "onig_new() called %p\n", internal->reg);
#endif
#endif
}

static void regexp_term(regexp_s* internal)
{
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 4
  fprintf(stderr, "onig_free() called %p\n", internal->reg);
#endif
#endif
  onig_free(internal->reg);
}

static int regexp_search_internal(regexp_s* internal, MatchLocation_s* result,
                                  const char* text, unsigned int search_start_pos,
                                  unsigned int search_end_pos)
{
  int ret;
  const unsigned char* str = (const unsigned char*)text;
  const unsigned char* str_end = str + strlen(text);
  const unsigned char* top = str + search_start_pos;
  const unsigned char* end = str + search_end_pos;
  ret = onig_search(internal->reg, str, str_end, top, end, NULL, 0);
  if (ret < 0)
    return -1;
  result->match_start_pos = ret;
  ret = onig_match(internal->reg, str, str_end, str + ret, NULL, 0);
  if (ret < 0)
    {
      regexp_term(internal);
      fprintf(stderr, "onig_search() error (%d)\n", ret);
      exit(EXIT_FAILURE);
    }
  result->match_end_pos = result->match_start_pos + ret;
  return 0;
}

int regexp_search(MatchLocation_s* result, const char* pattern, const char* text,
                  unsigned int search_start_pos, unsigned int search_end_pos)
{
  int ret;
  regexp_s internal;
  unsigned int length = strlen(text);
  if ((search_start_pos > search_end_pos) || (search_end_pos > length))
    return -1;
  regexp_init(&internal, pattern);
  ret = regexp_search_internal(&internal, result, text, search_start_pos,
                               search_end_pos);
  regexp_term(&internal);
  return ret;
}

static void regexp_count(MatchLines_s* result)
{
  int lf_count = 0;
  char* top = result->unmatched_lines;
  char* end = top + strlen(top);
  char* ptr;
  for (ptr = top; ptr < end; ++ptr)
    {
      if (*ptr == '\n')
        lf_count++;
    }
  result->unmatched_count = lf_count;
  lf_count = 0;
  top = result->matched_lines;
  end = top + strlen(top);
  for (ptr = top; ptr < end; ++ptr)
    {
      if (*ptr == '\n')
        lf_count++;
    }
  result->matched_count = lf_count;
}

unsigned int align_to_line_top(unsigned int pos,
                               const char* text, unsigned int start_pos)
{
  while ((pos > start_pos) && (text[pos - 1] != '\n'))
    pos--;
  return pos;
}

unsigned int align_to_line_bottom(unsigned int pos,
                                  const char* text, unsigned int end_pos)
{
  while ((pos < end_pos) && (text[pos - 1] != '\n'))
    pos++;
  return pos;
}

char* copy_text(char* target, unsigned int start_pos, unsigned int end_pos,
                const char* text)
{
  strncpy(target, text + start_pos, end_pos - start_pos);
  target += end_pos - start_pos;
  return target;
}

void regexp_search_all(MatchLines_s* result, const char* pattern, const char* text)
{
  regexp_s internal;
  unsigned int start_pos = 0;
  unsigned int end_pos = 0;
  char* unmatched_ptr;
  char* matched_ptr;

  assert(text);
  end_pos = strlen(text);
  unmatched_ptr = malloc(end_pos + 1);
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 4
  fprintf(stderr, "MatchLines_new() called %p\n", unmatched_ptr);
#endif
#endif
  result->unmatched_lines = unmatched_ptr;
  matched_ptr = malloc(end_pos + 1);
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 4
  fprintf(stderr, "MatchLines_new() called %p\n", matched_ptr);
#endif
#endif
  result->matched_lines = matched_ptr;
  regexp_init(&internal, pattern);
  while (1)
    {
      int ret;
      MatchLocation_s location;
      ret = regexp_search_internal(&internal, &location, text, start_pos, end_pos);
      if ((ret < 0) || (location.match_end_pos == 0))
        {
          strcpy(unmatched_ptr, text + start_pos);
          *matched_ptr = '\0';
          regexp_count(result);
          regexp_term(&internal);
          return;
        }
      location.match_start_pos = align_to_line_top(location.match_start_pos,
                                                   text, start_pos);
      location.match_end_pos = align_to_line_bottom(location.match_end_pos,
                                                    text, end_pos);
      unmatched_ptr =
        copy_text(unmatched_ptr, start_pos, location.match_start_pos, text);
      matched_ptr =
        copy_text(matched_ptr, location.match_start_pos, location.match_end_pos, text);
      start_pos = location.match_end_pos;
      if (start_pos == end_pos)
        {
          *unmatched_ptr = '\0';
          *matched_ptr = '\0';
          regexp_count(result);
          regexp_term(&internal);
          return;
        }
    }
}

void match_lines_term(MatchLines_s* me)
{
  if (me->unmatched_lines)
    {
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 4
      fprintf(stderr, "MatchLines_free() 4 called %p\n", me->unmatched_lines);
#endif
#endif
      free(me->unmatched_lines);
    }
  me->unmatched_lines = NULL;
  if (me->matched_lines)
    {
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 4
      fprintf(stderr, "MatchLines_free() 5 called %p\n", me->matched_lines);
#endif
#endif
      free(me->matched_lines);
    }
  me->matched_lines = NULL;
}

/* Local Variables:     */
/* mode: c              */
/* End:                 */
