/*
 * Project: cmdrecplay
 * Version: 1.1
 * Copyright: (C) 2014-2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
 * Create: 2014/05/02 11:08:33 JST
 */

#include <string.h>
#include <stdlib.h>

#include "Pattern.h"

typedef struct {
  ParameterType_e only_parameter;
  ParameterType_e regexp_of_parameter;
  ParameterType_e parameter_equals_to_recfile;
  int only_string;
} RequiredElement_s;

static void element_init(Element_s* me)
{
  me->line_no = 0;
  me->operator_type = OPERATOR_TYPE_UNSPECIFIED;
  me->parameter_type = PARAMETER_TYPE_UNSPECIFIED;
  me->operand = NULL;
}

static ParameterType_e element_is_parameter(ConfigFile_s* inpfile)
{
  if (inpfile->token == TOKEN_PARAMETER_RECCMD)
    return PARAMETER_TYPE_RECCMD;
  if (inpfile->token == TOKEN_PARAMETER_RECFILE)
    return PARAMETER_TYPE_RECFILE;
  if (inpfile->token == TOKEN_PARAMETER_RECCWD)
    return PARAMETER_TYPE_RECCWD;
  if (inpfile->token == TOKEN_PARAMETER_RECARGS)
    return PARAMETER_TYPE_RECARGS;
  if (inpfile->token == TOKEN_PARAMETER_PLAYCMD)
    return PARAMETER_TYPE_PLAYCMD;
  if (inpfile->token == TOKEN_PARAMETER_PLAYFILE)
    return PARAMETER_TYPE_PLAYFILE;
  if (inpfile->token == TOKEN_PARAMETER_OPTFILE)
    return PARAMETER_TYPE_OPTFILE;
  if (inpfile->token == TOKEN_PARAMETER_PLAYCWD)
    return PARAMETER_TYPE_PLAYCWD;
  if (inpfile->token == TOKEN_PARAMETER_PLAYARGS)
    return PARAMETER_TYPE_PLAYARGS;
  if (inpfile->token == TOKEN_PARAMETER_EXECCMD)
    return PARAMETER_TYPE_EXECCMD;
  if (inpfile->token == TOKEN_PARAMETER_EXECCWD)
    return PARAMETER_TYPE_EXECCWD;
  if (inpfile->token == TOKEN_PARAMETER_EXECARGS)
    return PARAMETER_TYPE_EXECARGS;
  return PARAMETER_TYPE_UNSPECIFIED;
}

static DefinitionType_e element_is_label(ConfigFile_s* inpfile)
{
  if (inpfile->token == TOKEN_LABEL_RECFILE)
    return DEFINITION_TYPE_RECFILE;
  if (inpfile->token == TOKEN_LABEL_RECMATCH)
    return DEFINITION_TYPE_RECMATCH;
  if (inpfile->token == TOKEN_LABEL_PLAYFILE)
    return DEFINITION_TYPE_PLAYFILE;
  if (inpfile->token == TOKEN_LABEL_PLAYMATCH)
    return DEFINITION_TYPE_PLAYMATCH;
  if (inpfile->token == TOKEN_LABEL_EXECCWD)
    return DEFINITION_TYPE_EXECCWD;
  if (inpfile->token == TOKEN_LABEL_EXECCMD)
    return DEFINITION_TYPE_EXECCMD;
  if (inpfile->token == TOKEN_LABEL_EXECARGS)
    return DEFINITION_TYPE_EXECARGS;
  if (inpfile->token == TOKEN_LABEL_EXECENVS)
    return DEFINITION_TYPE_EXECENVS;
  return DEFINITION_TYPE_UNSPECIFIED;
}

static int element_read_config(Element_s* me, RequiredElement_s required_element,
                               ConfigFile_s* inpfile)
{
  int length;
  ParameterType_e together;
  ParameterType_e found;
  ParameterType_e found2;
  OperatorType_e operator;
  together = (required_element.only_parameter |
              required_element.regexp_of_parameter |
              required_element.parameter_equals_to_recfile);
  me->operator_type = OPERATOR_TYPE_UNSPECIFIED;
  me->parameter_type = PARAMETER_TYPE_UNSPECIFIED;
  config_file_next_word(inpfile);
  found = element_is_parameter(inpfile);
  if (found == PARAMETER_TYPE_UNSPECIFIED)
    { /* パラメータではない */
      if (required_element.only_string && (inpfile->token == TOKEN_STRING))
        {
          me->line_no = inpfile->line_no;
          me->operand = malloc(strlen(inpfile->word) + 1);
          strcpy(me->operand, inpfile->word);
          me->operator_type = OPERATOR_TYPE_STRING;
          return 0;
        }
      config_file_rewind(inpfile);
      return -1;
    }
  if ((together & found) == 0)
    { /* パラメータだが期待されていなかった */
    not_expected_error:
      fprintf(stderr, "%s:%d: unexpected `%s'.\n",
              inpfile->config_file_name, inpfile->line_no, inpfile->word);
      exit(EXIT_FAILURE);
    }
  me->line_no = inpfile->line_no;
  /* 次の語を読み込み */
  config_file_next_word(inpfile);
  found2 = element_is_parameter(inpfile);
  if (found2 != PARAMETER_TYPE_UNSPECIFIED)
    { /* 次の語はパラメータ */
    only_parameter:
      if (required_element.only_parameter & found)
        {
          me->operator_type = OPERATOR_TYPE_PARAMETER;
          me->parameter_type = found;
          config_file_rewind(inpfile);
          return 0;
        }
      /* 期待されていなかった */
      goto not_expected_error;
    }
  if (inpfile->token == TOKEN_CLOSE)
    { /* 次の語は '}' */
      goto only_parameter;
    }
  if (element_is_label(inpfile))
    { /* 次の語はラベル */
      goto only_parameter;
    }
  if (inpfile->token == TOKEN_MATCH)
    {
      operator = OPERATOR_TYPE_PARAMETER_MATCHED;
    regexp_specified:
      config_file_next_word(inpfile);
      if (inpfile->token != TOKEN_REGEXP)
        goto not_expected_error;
      me->operator_type = operator;
      me->parameter_type = found;
      me->operand = malloc(strlen(inpfile->word) + 1);
      strcpy(me->operand, inpfile->word);
      return 0;
    }
  else if (inpfile->token == TOKEN_NOT_MATCH)
    {
      operator = OPERATOR_TYPE_PARAMETER_DIDNT_MATCH;
      goto regexp_specified;
    }
  else if (inpfile->token == TOKEN_EQUALS)
    {
      operator = OPERATOR_TYPE_PARAMETER_EQUALS_TO_RECFILE;
    equal_specified:
      config_file_next_word(inpfile);
      if (inpfile->token != TOKEN_PARAMETER_RECFILE)
        goto not_expected_error;
      me->operator_type = operator;
      me->parameter_type = found;
      return 0;
    }
  else
    {
      /* 期待されていなかった */
      goto not_expected_error;
    }
}

static void parameter_print(ParameterType_e parameter_type)
{
  switch (parameter_type)
    {
    case PARAMETER_TYPE_RECCMD:
      fprintf(stderr, "reccmd");
      break;
    case PARAMETER_TYPE_RECFILE:
      fprintf(stderr, "recfile");
      break;
    case PARAMETER_TYPE_RECCWD:
      fprintf(stderr, "reccwd");
      break;
    case PARAMETER_TYPE_RECARGS:
      fprintf(stderr, "recargs");
      break;
    case PARAMETER_TYPE_PLAYCMD:
      fprintf(stderr, "playcmd");
      break;
    case PARAMETER_TYPE_PLAYFILE:
      fprintf(stderr, "playfile");
      break;
    case PARAMETER_TYPE_OPTFILE:
      fprintf(stderr, "optfile");
      break;
    case PARAMETER_TYPE_PLAYCWD:
      fprintf(stderr, "playcwd");
      break;
    case PARAMETER_TYPE_PLAYARGS:
      fprintf(stderr, "playargs");
      break;
    case PARAMETER_TYPE_EXECCMD:
      fprintf(stderr, "execcmd");
      break;
    case PARAMETER_TYPE_EXECCWD:
      fprintf(stderr, "execcwd");
      break;
    case PARAMETER_TYPE_EXECARGS:
      fprintf(stderr, "execargs");
      break;
    default:
      fprintf(stderr, "unknown");
      break;
    }
}

static void element_print(const Element_s* me)
{
  fprintf(stderr, "    ");
  switch (me->operator_type)
    {
    case OPERATOR_TYPE_PARAMETER:
      parameter_print(me->parameter_type);
      break;
    case OPERATOR_TYPE_PARAMETER_MATCHED:
      parameter_print(me->parameter_type);
      fprintf(stderr, " =~ /%s/", me->operand);
      break;
    case OPERATOR_TYPE_PARAMETER_DIDNT_MATCH:
      parameter_print(me->parameter_type);
      fprintf(stderr, " !~ /%s/", me->operand);
      break;
    case OPERATOR_TYPE_PARAMETER_EQUALS_TO_RECFILE:
      parameter_print(me->parameter_type);
      fprintf(stderr, " == recfile");
      break;
    case OPERATOR_TYPE_STRING:
      fprintf(stderr, "'%s'", me->operand);
    default:
      fprintf(stderr, "unknown element");
    }
  fprintf(stderr, "\n");
}

static void element_term(Element_s* me)
{
  me->line_no = 0;
  me->operator_type = OPERATOR_TYPE_UNSPECIFIED;
  me->parameter_type = PARAMETER_TYPE_UNSPECIFIED;
  if (me->operand)
    free(me->operand);
  me->operand = NULL;
}

void definition_init(Definition_s* me)
{
  me->definition_type = DEFINITION_TYPE_UNSPECIFIED;
  me->line_no = 0;
  me->number_of_elements = 0;
  for (int offset = 0; offset < NUMBER_OF_ELEMENTS; ++offset)
    element_init(me->elements + offset);
}

int definition_read_config(Definition_s* me,
                           DefinitionType_e required_definition_type,
                           ConfigFile_s* inpfile)
{
  RequiredElement_s required_element = { 0, 0, 0, 0 };
  int offset;
  int number_of_elements = NUMBER_OF_ELEMENTS;
  me->definition_type = DEFINITION_TYPE_UNSPECIFIED;
  config_file_next_word(inpfile);
  if ((required_definition_type & DEFINITION_TYPE_RECFILE) &&
      (inpfile->token == TOKEN_LABEL_RECFILE))
    {
      me->definition_type = DEFINITION_TYPE_RECFILE;
      me->line_no = inpfile->line_no;
      required_element.only_parameter = 0;
      required_element.regexp_of_parameter = PARAMETER_TYPE_RECARGS;
      required_element.parameter_equals_to_recfile = 0;
      required_element.only_string = 0;
      number_of_elements = 1;
    }
  else if ((required_definition_type & DEFINITION_TYPE_RECMATCH) &&
           (inpfile->token == TOKEN_LABEL_RECMATCH))
    {
      me->definition_type = DEFINITION_TYPE_RECMATCH;
      me->line_no = inpfile->line_no;
      required_element.only_parameter = 0;
      required_element.regexp_of_parameter = PARAMETER_TYPE_RECCMD;
      required_element.parameter_equals_to_recfile = 0;
      required_element.only_string = 0;
      number_of_elements = 1;
    }
  else if ((required_definition_type & DEFINITION_TYPE_PLAYFILE) &&
           (inpfile->token == TOKEN_LABEL_PLAYFILE))
    {
      me->definition_type = DEFINITION_TYPE_PLAYFILE;
      me->line_no = inpfile->line_no;
      required_element.only_parameter = 0;
      required_element.regexp_of_parameter = PARAMETER_TYPE_PLAYARGS;
      required_element.parameter_equals_to_recfile = 0;
      required_element.only_string = 0;
      number_of_elements = 1;
    }
  else if ((required_definition_type & DEFINITION_TYPE_PLAYMATCH) &&
           (inpfile->token == TOKEN_LABEL_PLAYMATCH))
    {
      me->definition_type = DEFINITION_TYPE_PLAYMATCH;
      me->line_no = inpfile->line_no;
      required_element.only_parameter = 0;
      required_element.regexp_of_parameter = PARAMETER_TYPE_PLAYCMD;
      required_element.parameter_equals_to_recfile = (PARAMETER_TYPE_PLAYFILE |
                                                      PARAMETER_TYPE_OPTFILE);
      required_element.only_string = 0;
      number_of_elements = 2;
    }
  else if ((required_definition_type & DEFINITION_TYPE_EXECCWD) &&
           (inpfile->token == TOKEN_LABEL_EXECCWD))
    {
      me->definition_type = DEFINITION_TYPE_EXECCWD;
      me->line_no = inpfile->line_no;
      required_element.only_parameter = (PARAMETER_TYPE_RECCWD | PARAMETER_TYPE_PLAYCWD);
      required_element.regexp_of_parameter = 0;
      required_element.parameter_equals_to_recfile = 0;
      required_element.only_string = 1;
      number_of_elements = 1;
    }
  else if ((required_definition_type & DEFINITION_TYPE_EXECCMD) &&
           (inpfile->token == TOKEN_LABEL_EXECCMD))
    {
      me->definition_type = DEFINITION_TYPE_EXECCMD;
      me->line_no = inpfile->line_no;
      required_element.only_parameter = (PARAMETER_TYPE_RECCMD | PARAMETER_TYPE_PLAYCMD);
      required_element.regexp_of_parameter = 0;
      required_element.parameter_equals_to_recfile = 0;
      required_element.only_string = 1;
      number_of_elements = 1;
    }
  else if ((required_definition_type & DEFINITION_TYPE_EXECARGS) &&
           (inpfile->token == TOKEN_LABEL_EXECARGS))
    {
      me->definition_type = DEFINITION_TYPE_EXECARGS;
      me->line_no = inpfile->line_no;
      required_element.only_parameter = (PARAMETER_TYPE_RECCWD |
                                         PARAMETER_TYPE_RECCMD |
                                         PARAMETER_TYPE_RECFILE |
                                         PARAMETER_TYPE_RECARGS |
                                         PARAMETER_TYPE_PLAYCWD |
                                         PARAMETER_TYPE_PLAYCMD |
                                         PARAMETER_TYPE_PLAYFILE |
                                         PARAMETER_TYPE_OPTFILE |
                                         PARAMETER_TYPE_PLAYARGS);
      required_element.regexp_of_parameter = (PARAMETER_TYPE_RECARGS |
                                              PARAMETER_TYPE_PLAYARGS);
      required_element.parameter_equals_to_recfile = 0;
      required_element.only_string = 1;
    }
  else if ((required_definition_type & DEFINITION_TYPE_EXECENVS) &&
           (inpfile->token == TOKEN_LABEL_EXECENVS))
    {
      me->definition_type = DEFINITION_TYPE_EXECENVS;
      me->line_no = inpfile->line_no;
      required_element.only_parameter = 0;
      required_element.regexp_of_parameter = 0;
      required_element.parameter_equals_to_recfile = 0;
      required_element.only_string = 1;
    }
  if (me->definition_type == DEFINITION_TYPE_UNSPECIFIED)
    {
      config_file_rewind(inpfile);
      return -1;
    }

  for (offset = 0; offset < number_of_elements; ++offset)
    {
      int result;
      element_init(me->elements + offset);
      result = element_read_config(me->elements + offset, required_element, inpfile);
      if (result < 0)
        break;
    }
  me->number_of_elements = offset;
  return 0;
}

void definition_print(const Definition_s* me)
{
  switch (me->definition_type)
    {
    case DEFINITION_TYPE_RECFILE:
      fprintf(stderr, "  recfile:\n");
      break;
    case DEFINITION_TYPE_RECMATCH:
      fprintf(stderr, "  recmatch:\n");
      break;
    case DEFINITION_TYPE_PLAYFILE:
      fprintf(stderr, "  playfile:\n");
      break;
    case DEFINITION_TYPE_PLAYMATCH:
      fprintf(stderr, "  playmatch:\n");
      break;
    case DEFINITION_TYPE_EXECCWD:
      fprintf(stderr, "  execcwd:\n");
      break;
    case DEFINITION_TYPE_EXECCMD:
      fprintf(stderr, "  execcmd:\n");
      break;
    case DEFINITION_TYPE_EXECARGS:
      fprintf(stderr, "  execargs:\n");
      break;
    case DEFINITION_TYPE_EXECENVS:
      fprintf(stderr, "  execenvs:\n");
      break;
    default:
      fprintf(stderr, "  unknown definition\n");
      return;
    }
  for (int offset = 0; offset < me->number_of_elements; ++offset)
    element_print(me->elements + offset);
}

void definition_term(Definition_s* me)
{
  me->definition_type = DEFINITION_TYPE_UNSPECIFIED;
  me->line_no = 0;
  for (int offset = 0; offset < NUMBER_OF_ELEMENTS; ++offset)
    element_term(me->elements + offset);
}

/* Local Variables:     */
/* mode: c              */
/* End:                 */
