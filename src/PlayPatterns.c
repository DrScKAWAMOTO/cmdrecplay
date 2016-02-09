/*
 * Project: cmdrecplay
 * Version: 1.0
 * Copyright: (C) 2014-2016 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/02 11:08:33 JST
 */

#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "debug.h"
#include "PlayPatterns.h"
#include "PlayDatabase.h"
#include "Regexp.h"

static void play_pattern_init(PlayPattern_s* me)
{
  me->pattern_name = NULL;
  definition_init(&(me->playfile));
  definition_init(&(me->playmatch));
  definition_init(&(me->execcwd));
  definition_init(&(me->execargs));
  definition_init(&(me->execenvs));
}

static void play_pattern_read_config(PlayPattern_s* me, PlayPatterns_s* parent,
                                     ConfigFile_s* inpfile)
{
  int offset;
  config_file_read_keyword(inpfile, "play");
  config_file_read_keyword(inpfile, "pattern");
  config_file_next_word(inpfile);
  if (inpfile->token != TOKEN_STRING)
    {
      fprintf(stderr, "%s:%d: play pattern name is not specified.\n",
              inpfile->config_file_name, inpfile->line_no);
      exit(EXIT_FAILURE);
    }
  me->line_no = inpfile->line_no;
  for (offset = 0; offset < parent->number_of_patterns; ++offset)
    {
      if (strcmp(inpfile->word, parent->patterns[offset].pattern_name) == 0)
        {
          fprintf(stderr, "%s:%d: play pattern name %s is not unique,"
                  " previous play pattern is located at %d.\n",
                  inpfile->config_file_name, inpfile->line_no, inpfile->word,
                  parent->patterns[offset].line_no);
          exit(EXIT_FAILURE);
        }
    }
  me->pattern_name = malloc(strlen(inpfile->word) + 1);
  strcpy(me->pattern_name, inpfile->word);
  config_file_read_keyword(inpfile, "{");
  if (definition_read_config(&(me->playfile), DEFINITION_TYPE_PLAYFILE, inpfile) < 0)
    {
      fprintf(stderr, "%s:%d: `playfile:' is not specified.\n",
              inpfile->config_file_name, inpfile->line_no);
      exit(EXIT_FAILURE);
    }
  if (definition_read_config(&(me->playmatch), DEFINITION_TYPE_PLAYMATCH, inpfile) < 0)
    {
      fprintf(stderr, "%s:%d: `playmatch:' is not specified.\n",
              inpfile->config_file_name, inpfile->line_no);
      exit(EXIT_FAILURE);
    }
  if (definition_read_config(&(me->execcwd), DEFINITION_TYPE_EXECCWD, inpfile) < 0)
    {
      fprintf(stderr, "%s:%d: `execcwd:' is not specified.\n",
              inpfile->config_file_name, inpfile->line_no);
      exit(EXIT_FAILURE);
    }
  if (definition_read_config(&(me->execcmd), DEFINITION_TYPE_EXECCMD, inpfile) < 0)
    {
      fprintf(stderr, "%s:%d: `execcmd:' is not specified.\n",
              inpfile->config_file_name, inpfile->line_no);
      exit(EXIT_FAILURE);
    }
  if (definition_read_config(&(me->execargs), DEFINITION_TYPE_EXECARGS, inpfile) < 0)
    {
      fprintf(stderr, "%s:%d: `execargs:' is not specified.\n",
              inpfile->config_file_name, inpfile->line_no);
      exit(EXIT_FAILURE);
    }
  definition_read_config(&(me->execenvs), DEFINITION_TYPE_EXECENVS, inpfile);
  config_file_read_keyword(inpfile, "}");
}

static void play_pattern_print(const PlayPattern_s* me)
{
  fprintf(stderr, "play pattern '%s' {\n", me->pattern_name);
  definition_print(&(me->playfile));
  definition_print(&(me->playmatch));
  definition_print(&(me->execcwd));
  definition_print(&(me->execargs));
  definition_print(&(me->execenvs));
  fprintf(stderr, "}\n");
}

static void play_pattern_term(PlayPattern_s* me)
{
  if (me->pattern_name)
    free(me->pattern_name);
  me->pattern_name = NULL;
  definition_term(&(me->playfile));
  definition_term(&(me->playmatch));
  definition_term(&(me->execcwd));
  definition_term(&(me->execargs));
  definition_term(&(me->execenvs));
}

void play_patterns_init(PlayPatterns_s* me)
{
  for (int offset = 0; offset < NUMBER_OF_PATTERNS; ++offset)
    play_pattern_init(me->patterns + offset);
}

void play_patterns_term(PlayPatterns_s* me)
{
  for (int offset = 0; offset < NUMBER_OF_PATTERNS; ++offset)
    play_pattern_term(me->patterns + offset);
}

void play_patterns_read_config(PlayPatterns_s* me, ConfigFile_s* inpfile)
{
  me->number_of_patterns = 0;
  for (int offset = 0; offset < NUMBER_OF_PATTERNS; ++offset)
    {
      if (config_file_next_word_or_end_of_file(inpfile) < 0)
        break;
      config_file_rewind(inpfile);
      play_pattern_read_config(me->patterns + offset, me, inpfile);
      me->number_of_patterns++;
    }
}

static void play_pattern_apply_pre(const PlayPattern_s* me, ParameterSet_s* parameter_set)
{
  MatchLines_s mli;
  MatchLocation_s mlo;
  unsigned int start_pos;
  unsigned int end_pos;
  /* playfile: を適用する */
  assert(me->playfile.definition_type == DEFINITION_TYPE_PLAYFILE);
  assert(me->playfile.number_of_elements == 1);
  /* playargs =~ /.../ */
  /* playargs !~ /.../ */
  assert((me->playfile.elements[0].operator_type == OPERATOR_TYPE_PARAMETER_MATCHED) ||
         (me->playfile.elements[0].operator_type == OPERATOR_TYPE_PARAMETER_DIDNT_MATCH));
  assert(me->playfile.elements[0].parameter_type == PARAMETER_TYPE_PLAYARGS);
  /* 適用 */
  regexp_search_all(&mli, me->playfile.elements[0].operand,
                    parameterSet_refer_string_value(parameter_set,
                                                    PARAMETER_TYPE_PLAYARGS));
  if (me->playfile.elements[0].operator_type == OPERATOR_TYPE_PARAMETER_MATCHED)
    {
      if (mli.matched_count == 1)
        {
          mli.matched_lines[strlen(mli.matched_lines) - 1] = '\0';
          parameterSet_set_by_copy_as_realpath(parameter_set, PARAMETER_TYPE_PLAYFILE,
                                               mli.matched_lines);
          parameterSet_set_string_value(parameter_set, PARAMETER_TYPE_PLAYARGS,
                                        mli.unmatched_lines);
          mli.unmatched_lines = NULL;
          match_lines_term(&mli);
        }
      else
        {
          match_lines_term(&mli);
          parameter_set->error_detected = 1;
          return;
        }
    }
  else
    {
      if (mli.unmatched_count == 1)
        {
          mli.unmatched_lines[strlen(mli.unmatched_lines) - 1] = '\0';
          parameterSet_set_by_copy_as_realpath(parameter_set, PARAMETER_TYPE_PLAYFILE,
                                               mli.unmatched_lines);
          parameterSet_set_string_value(parameter_set, PARAMETER_TYPE_PLAYARGS,
                                        mli.matched_lines);
          mli.unmatched_lines = NULL;
          match_lines_term(&mli);
        }
      else
        {
          match_lines_term(&mli);
          parameter_set->error_detected = 1;
          return;
        }
    }
  /* optfile が無ければ plyfile と同じものを設定する。 */
  if (parameterSet_refer_string_value(parameter_set, PARAMETER_TYPE_OPTFILE) == NULL)
    {
      parameterSet_set_by_copy_string(parameter_set, PARAMETER_TYPE_OPTFILE,
                    parameterSet_refer_string_value(parameter_set,
                                                    PARAMETER_TYPE_PLAYFILE));
    }

  /* playmatch: を適用する */
  assert(me->playmatch.definition_type == DEFINITION_TYPE_PLAYMATCH);
  for (int offset = 0; offset < me->playmatch.number_of_elements; ++offset)
    {
      const Element_s* element = me->playmatch.elements + offset;
      assert((element->operator_type == OPERATOR_TYPE_PARAMETER_MATCHED) ||
             (element->operator_type == OPERATOR_TYPE_PARAMETER_DIDNT_MATCH) ||
             (element->operator_type == OPERATOR_TYPE_PARAMETER_EQUALS_TO_RECFILE));
      if ((element->operator_type == OPERATOR_TYPE_PARAMETER_MATCHED) ||
          (element->operator_type == OPERATOR_TYPE_PARAMETER_DIDNT_MATCH))
        {
          const char* text;
          int result;
          /* playcmd =~ /.../ */
          assert(element->parameter_type == PARAMETER_TYPE_PLAYCMD);
          /* 適用 */
          text = parameterSet_refer_string_value(parameter_set, PARAMETER_TYPE_PLAYCMD);
          start_pos = 0;
          end_pos = strlen(text);
          result = regexp_search(&mlo, me->playmatch.elements[0].operand,
                                 text, start_pos, end_pos);
          if ((element->operator_type == OPERATOR_TYPE_PARAMETER_MATCHED) &&
              (result < 0))
            {
              parameter_set->error_detected = 1;
              return;
            }
          if ((element->operator_type == OPERATOR_TYPE_PARAMETER_DIDNT_MATCH) &&
              (result >= 0))
            {
              parameter_set->error_detected = 1;
              return;
            }
        }
      else
        {
          const char* text1;
          const char* text2;
          /* playfile == recfile */
          /* optfile == recfile */
          assert((element->parameter_type == PARAMETER_TYPE_PLAYFILE) ||
                 (element->parameter_type == PARAMETER_TYPE_OPTFILE));
          /* 適用 */
          text1 = parameterSet_refer_string_value(parameter_set, element->parameter_type);
          if (text1 == NULL)
            {
              parameter_set->error_detected = 1;
              return;
            }
          text2 = parameterSet_refer_string_value(parameter_set, PARAMETER_TYPE_RECFILE);
          if (text2)
            {
              if (strcmp(text1, text2) != 0)
                {
                  parameter_set->error_detected = 1;
                  return;
                }
            }
          else
            {
              parameterSet_set_by_copy_string(parameter_set, PARAMETER_TYPE_RECFILE,
                                              text1);
            }
        }
    }
}

static void play_pattern_apply_post(const PlayPattern_s* me,
                                    ParameterSet_s* parameter_set)
{
  const Element_s* element;
  char* sum;

  /* execcwd: を適用する */
  assert(me->execcwd.definition_type == DEFINITION_TYPE_EXECCWD);
  assert(me->execcwd.number_of_elements == 1);
  element = me->execcwd.elements;
  /* reccwd */
  /* playcwd */
  /* '...' */
  assert((element->operator_type == OPERATOR_TYPE_PARAMETER) ||
         (element->operator_type == OPERATOR_TYPE_STRING));
  /* 適用 */
  if (element->operator_type == OPERATOR_TYPE_PARAMETER)
    {
      ParameterType_e parameter_type;
      parameter_type = element->parameter_type;
      assert((parameter_type == PARAMETER_TYPE_RECCWD) ||
             (parameter_type == PARAMETER_TYPE_PLAYCWD));
      parameterSet_set_by_copy_string(parameter_set, PARAMETER_TYPE_EXECCWD,
                                      parameterSet_refer_string_value(parameter_set,
                                                                      parameter_type));
    }
  else
    {
      parameterSet_set_by_copy_string(parameter_set, PARAMETER_TYPE_EXECCWD,
                                      element->operand);
    }

  /* execcmd: を適用する */
  assert(me->execcmd.definition_type == DEFINITION_TYPE_EXECCMD);
  assert(me->execcmd.number_of_elements == 1);
  element = me->execcmd.elements;
  /* reccmd */
  /* playcmd */
  /* '...' */
  assert((element->operator_type == OPERATOR_TYPE_PARAMETER) ||
         (element->operator_type == OPERATOR_TYPE_STRING));
  /* 適用 */
  if (element->operator_type == OPERATOR_TYPE_PARAMETER)
    {
      ParameterType_e parameter_type;
      parameter_type = element->parameter_type;
      assert((parameter_type == PARAMETER_TYPE_RECCMD) ||
             (parameter_type == PARAMETER_TYPE_PLAYCMD));
      parameterSet_set_by_copy_string(parameter_set, PARAMETER_TYPE_EXECCMD,
                                      parameterSet_refer_string_value(parameter_set,
                                                                      parameter_type));
    }
  else
    {
      parameterSet_set_by_copy_string(parameter_set, PARAMETER_TYPE_EXECCMD,
                                      element->operand);
    }

  /* execargs: を適用する */
  assert(me->execargs.definition_type == DEFINITION_TYPE_EXECARGS);
  sum = malloc(1);
  sum[0] = '\0';
  for (int offset = 0; offset < me->execargs.number_of_elements; ++offset)
    {
      MatchLines_s mli;
      char* found;
      char* new_sum;
      int new_length;
      int found_length;
      int required_free;
      int required_add_lf = 0;
      element = me->execargs.elements + offset;
      switch (element->operator_type)
        {
        case OPERATOR_TYPE_PARAMETER:
          found = (char*)parameterSet_refer_string_value(parameter_set,
                                                         element->parameter_type);
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 2
          fprintf(stderr, "OPERATOR_TYPE_PARAMETER = '%s'\n", found);
#endif
#endif
          required_free = 0;
          break;
        case OPERATOR_TYPE_PARAMETER_MATCHED:
          regexp_search_all(&mli, element->operand,
                            parameterSet_refer_string_value(parameter_set,
                                                            element->parameter_type));
          found = mli.matched_lines;
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 2
          fprintf(stderr, "OPERATOR_TYPE_PARAMETER_MATCHED = '%s'\n", found);
#endif
#endif
          required_free = 1;
          free(mli.unmatched_lines);
          break;
        case OPERATOR_TYPE_PARAMETER_DIDNT_MATCH:
          regexp_search_all(&mli, element->operand,
                            parameterSet_refer_string_value(parameter_set,
                                                            element->parameter_type));
          found = mli.unmatched_lines;
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 2
          fprintf(stderr, "OPERATOR_TYPE_PARAMETER_DIDNT_MATCH = '%s'\n", found);
#endif
#endif
          required_free = 1;
          free(mli.matched_lines);
          break;
        case OPERATOR_TYPE_STRING:
          found = element->operand;
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 2
          fprintf(stderr, "OPERATOR_TYPE_STRING = '%s'\n", found);
#endif
#endif
          required_free = 0;
          break;
        default:
          assert((element->operator_type == OPERATOR_TYPE_PARAMETER) ||
                 (element->operator_type == OPERATOR_TYPE_PARAMETER_MATCHED) ||
                 (element->operator_type == OPERATOR_TYPE_PARAMETER_DIDNT_MATCH) ||
                 (element->operator_type == OPERATOR_TYPE_STRING));
          break;
        }
      found_length = strlen(found);
      new_length = strlen(sum) + found_length;
      if ((found_length > 0) && (found[found_length - 1] != '\n'))
        {
          required_add_lf = 1;
          new_length++;
        }
      new_sum = malloc(new_length + 1);
      strcpy(new_sum, sum);
      strcat(new_sum, found);
      if (required_add_lf)
        strcat(new_sum, "\n");
      if (required_free)
        free(found);
      free(sum);
      sum = new_sum;
    }
  parameterSet_set_string_value(parameter_set, PARAMETER_TYPE_EXECARGS, sum);
}

int play_patterns_apply(const PlayPatterns_s* me, ParameterSet_s* parameter_set)
{
  int number_of_patterns = me->number_of_patterns;
  ParameterSet_s* parameter_sets;
  int one_record_offset = -1;
  int no_records_offset1 = -1;
  int no_records_offset2 = -1;
  int record_offset;
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 2
  fprintf(stderr, "\n");
  fprintf(stderr, "-- initial parameter set --\n");
  parameterSet_print(parameter_set);
#endif
#endif
  parameter_sets = malloc(sizeof(ParameterSet_s) * number_of_patterns);
  for (int offset = 0; offset < number_of_patterns; ++offset)
    {
      parameterSet_init(parameter_sets + offset);
      parameterSet_copy(parameter_sets + offset, parameter_set);
      play_pattern_apply_pre(me->patterns + offset, parameter_sets + offset);
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 2
      fprintf(stderr, "apply_pre `%s' ---\n", me->patterns[offset].pattern_name);
      parameterSet_print(parameter_sets + offset);
#endif
#endif
      if (parameter_sets[offset].error_detected == 0)
        {
          int result = database_play(parameter_sets + offset);
          if (result == 2)
            {
              fprintf(stderr, "pattern `%s' matched many records.\n",
                      me->patterns[offset].pattern_name);
              exit(EXIT_FAILURE);
            }
          else if (result == 1)
            {
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 1
              fprintf(stderr, "only one record found with pattern `%s'.\n",
                      me->patterns[offset].pattern_name);
#endif
#endif
              if (one_record_offset >= 0)
                {
                  fprintf(stderr, "many patterns `%s' and `%s' suitable,"
                          " and each pattern matched only one record.\n",
                          me->patterns[one_record_offset].pattern_name,
                          me->patterns[offset].pattern_name);
                  exit(EXIT_FAILURE);
                }
              one_record_offset = offset;
            }
          else
            {
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 1
              fprintf(stderr, "no records found with pattern `%s'.\n",
                      me->patterns[offset].pattern_name);
#endif
#endif
              if (no_records_offset1 < 0)
                no_records_offset1 = offset;
              else if (no_records_offset2 < 0)
                no_records_offset2 = offset;
            }
        }
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 2
      fprintf(stderr, "database_play `%s' ---\n", me->patterns[offset].pattern_name);
      parameterSet_print(parameter_sets + offset);
#endif
#endif
    }
  if (one_record_offset == -1)
    {
      if (no_records_offset1 < 0)
        {
          fprintf(stderr, "%s:1:1: warning: cmdplay: no suitable patterns, "
                  "modify `cmdplay.conf'.\n",
                  parameterSet_refer_string_value(parameter_set,
                                                  PARAMETER_TYPE_OPTFILE));
          exit(EXIT_FAILURE);
        }
      parameterSet_copy(parameter_set, parameter_sets + no_records_offset1);
      if (no_records_offset2 >= 0)
        {
          fprintf(stderr, "%s:1:1: warning: cmdplay: too many patterns `%s' and `%s' "
                  "suitable, but matches no records, modify `cmdplay.conf'.\n",
                  parameterSet_refer_string_value(parameter_set, PARAMETER_TYPE_RECFILE),
                  me->patterns[no_records_offset1].pattern_name,
                  me->patterns[no_records_offset2].pattern_name);
          exit(EXIT_FAILURE);
        }
      fprintf(stderr, "%s:1:1: warning: cmdplay: no suitable records, "
              "execute `cmdrec make'.\n",
              parameterSet_refer_string_value(parameter_set,
                                              PARAMETER_TYPE_RECFILE));
      exit(EXIT_FAILURE);
    }
  else
    {
      record_offset = one_record_offset;
      parameterSet_copy(parameter_set, parameter_sets + record_offset);
    }
  for (int offset = 0; offset < number_of_patterns; ++offset)
    parameterSet_term(parameter_sets + offset);
  free(parameter_sets);
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 2
  fprintf(stderr, "only one `%s' ---\n", me->patterns[record_offset].pattern_name);
  parameterSet_print(parameter_set);
#endif
#endif
  play_pattern_apply_post(me->patterns + record_offset, parameter_set);
#if !defined(NDEBUG)
#if DEBUG_LEVEL >= 2
  fprintf(stderr, "apply_post `%s' ---\n", me->patterns[record_offset].pattern_name);
  parameterSet_print(parameter_set);
#endif
#endif
  return record_offset;
}

void play_patterns_print(PlayPatterns_s* me)
{
  for (int offset = 0; offset < me->number_of_patterns; ++offset)
    play_pattern_print(me->patterns + offset);
}

/* Local Variables:	*/
/* mode: c		*/
/* End:			*/
