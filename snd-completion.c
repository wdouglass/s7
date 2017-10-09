#include "snd.h"
#include "sndlib-strings.h"
#include "clm-strings.h"


static char *current_match = NULL;

/* TAB completion requires knowing what is currently defined, which requires scrounging
 *   around in the symbol tables
 */

#if HAVE_SCHEME

typedef struct {
  const char *text;
  int matches, len;
} match_info;

static bool compare_names(const char *symbol_name, void *data)
{
  match_info *m = (match_info *)data;

  if (strncmp(m->text, symbol_name, m->len) == 0)
    {
      m->matches++;
      add_possible_completion(symbol_name);
      if (!current_match)
	current_match = mus_strdup(symbol_name);
      else 
	{
	  int j, curlen;
	  curlen = mus_strlen(current_match);
	  for (j = 0; j < curlen; j++)
	    if (current_match[j] != symbol_name[j])
	      {
		current_match[j] = '\0';
		break;
	      }
	}
    }
  return(false);
}

static int completions(const char *text)
{
  match_info *m;
  int matches;

  m = (match_info *)calloc(1, sizeof(match_info));
  m->text = text;
  m->len = strlen(text);
  m->matches = 0;
  s7_for_each_symbol_name(s7, compare_names, (void *)m);
  matches = m->matches;
  free(m);
  return(matches);
}

#endif


#if HAVE_RUBY
static Xen snd_rb_methods(void)
{
  /* returns all the functions we defined */
  Xen argv[1];
  argv[0] = Xen_true;
  return(rb_class_private_instance_methods(1, argv, rb_mKernel));
  /* rb_ary_new here -- should we free? */
}


static int completions(const char *text)
{
  Xen tab;
  int i, n, len, matches = 0;

  tab = snd_rb_methods();
  n = Xen_vector_length(tab);
  len = strlen(text);

  for (i = 0; i < n; ++i)
    {
      char *sym;
      Xen handle;

      handle = Xen_vector_ref(tab, i);
      sym = Xen_object_to_C_string(handle);

      if (strncmp(text, sym, len) == 0)
	{
	  matches++;
	  add_possible_completion(sym);
	  if (!current_match)
	    current_match = mus_strdup(sym);
	  else 
	    {
	      int j, curlen;
	      curlen = mus_strlen(current_match);
	      for (j = 0; j < curlen; j++)
		if (current_match[j] != sym[j])
		  {
		    current_match[j] = '\0';
		    break;
		  }
	    }
	}
    }
  return(matches);
}
#endif


#if HAVE_FORTH
static int completions(const char *text)
{
  Xen tab = fth_find_in_wordlist(text);
  int i, matches = Xen_vector_length(tab);

  for (i = 0; i < matches; i++)
    {
      char *sym = Xen_string_to_C_string(Xen_vector_ref(tab, i));
      add_possible_completion(sym);
      if (!current_match)
	current_match = mus_strdup(sym);
      else 
	{
	  int j, curlen;
	  curlen = mus_strlen(current_match);
	  for (j = 0; j < curlen; j++)
	    if (current_match[j] != sym[j])
	      {
		current_match[j] = '\0';
		break;
	      }
	}
    }
  return(matches);
}
#endif


#if (!HAVE_EXTENSION_LANGUAGE)
static int completions(const char *text) {return(0);}
#endif


#if HAVE_FORTH
static bool is_separator(char c)
{
  /* only space is separator */
  return(!(isgraph((int)c)));
}
#else
static bool is_separator(char c)
{
  return((!(isalpha((int)c))) &&
	 (!(isdigit((int)c))) &&
#if HAVE_RUBY
	 (c != '?') &&
	 (c != '!') &&
	 (c != '_') &&
#endif
#if HAVE_SCHEME
	 (c != '-') &&
	 (c != '_') &&
	 (c != '>') &&
	 (c != '?') &&
	 (c != '!') &&
	 (c != '=') &&
	 (c != '<') &&
	 (c != '*') &&
	 (c != '+') &&
	 (c != '%') &&
	 (c != ':') &&
	 (c != '/') &&
#endif
	 (c != '$'));
}
#endif


char *direct_completions(const char *str)
{
  int matches = 0;
  current_match = NULL;
  clear_possible_completions();
  set_completion_matches(0);
  set_save_completions(true);
  matches = completions(str);
  set_completion_matches(matches);
  set_save_completions(false);
  return(current_match);
}


char *expression_completer(widget_t w, const char *original_text, void *data)
{
  int len, beg, matches = 0;
  /* first back up to some delimiter to get the current expression */

  current_match = NULL;
  set_completion_matches(0);

  if ((original_text) && (*original_text))
    {
      const char *text;
      int i;

      len = strlen(original_text);
      for (i = len - 1; i >= 0; i--)
	if (is_separator(original_text[i]))
	  break;
      beg = i + 1;

      if (beg == len) 
	{
	  /* returning original = no-op response to <tab> which seems useless;
	   *   so, if it's a function that we recognize and that function has its own completer, call it?
	   *   result null -> return original (current behavior), else append result as selection to current;
	   *   this way, I think it's easy to clear the suggested completion (cursor at end, so backspace deletes all).
	   * or scan back through full text (assuming listener or history available) and find match?
	   */
	  return(mus_strdup(original_text));
	}

      if (beg > 0) 
	text = (const char *)(original_text + beg);
      else text = original_text;
      matches = completions(text);
    }
  else return(mus_strdup(original_text));

  set_completion_matches(matches);
  if ((current_match) && 
      (*current_match))
    {
      if (beg == 0)
	return(current_match);
      else
	{
	  char *text;
	  len = mus_strlen(current_match) + beg + 2;
	  text = (char *)calloc(len, sizeof(char));
	  strncpy(text, original_text, beg);
	  strcat(text, current_match);
	  free(current_match);
	  return(text);
	}
    }
  return(mus_strdup(original_text));
}



/* -------- saved choices -------- */

#define BEST_COMPLETIONS 64
static char *best_completions[BEST_COMPLETIONS];

void preload_best_completions(void)
{
  /* set up the array with some reasonable first choices */
  int n = 0;
  best_completions[n++] = mus_strdup(S_open_sound);
  best_completions[n++] = mus_strdup(S_channels);
  best_completions[n++] = mus_strdup(S_close_sound);
  best_completions[n++] = mus_strdup(S_cursor);
  best_completions[n++] = mus_strdup(S_env_channel);
  best_completions[n++] = mus_strdup(S_file_name);
  best_completions[n++] = mus_strdup(S_framples);
  best_completions[n++] = mus_strdup(S_map_channel);
  best_completions[n++] = mus_strdup(S_maxamp);
  best_completions[n++] = mus_strdup(S_play);
  best_completions[n++] = mus_strdup(S_save_sound);
  best_completions[n++] = mus_strdup(S_scale_channel);
  best_completions[n++] = mus_strdup(S_srate);
  best_completions[n++] = mus_strdup("with-sound");
  best_completions[n++] = mus_strdup(S_outa);
  best_completions[n++] = mus_strdup("*output*");
  best_completions[n++] = mus_strdup("lambda");
  best_completions[n++] = mus_strdup("define");
}


void save_completion_choice(const char *selection)
{
  int i;
  char *cur = NULL, *old = NULL;

  cur = mus_strdup(selection);
  for (i = 0; i < BEST_COMPLETIONS; i++)
    {
      old = best_completions[i];
      best_completions[i] = cur;
      if ((!old) ||
	  (mus_strcmp(old, cur)))
	break;
      cur = old;
    }
  if (old) free(old);
}


int find_best_completion(char **choices, int num_choices)
{
  int i, k;
  for (i = 0; (i < BEST_COMPLETIONS) && (best_completions[i]); i++)
    for (k = 0; k < num_choices; k++)
      if (mus_strcmp(choices[k], best_completions[i]))
	return(k + 1); /* row numbering is 1-based */
  return(1);
}





/* ---------------- EXPRESSION/FILENAME COMPLETIONS ---------------- */

typedef char *(*completer_func)(widget_t w, const char *text, void *data);
static completer_func *completer_funcs = NULL;
typedef void (*multicompleter_func)(widget_t w, void *data);
static multicompleter_func *multicompleter_funcs = NULL;
static void **completer_data = NULL;
static int completer_funcs_size = 0;
static int completer_funcs_end = 0;


int add_completer_func(char *(*func)(widget_t w, const char *text, void *context), void *data)
{
  if (completer_funcs_size == completer_funcs_end)
    {
      completer_funcs_size += 8;
      if (!completer_funcs)
	{
	  completer_funcs = (completer_func *)calloc(completer_funcs_size, sizeof(completer_func));
	  multicompleter_funcs = (multicompleter_func *)calloc(completer_funcs_size, sizeof(multicompleter_func));
	  completer_data = (void **)calloc(completer_funcs_size, sizeof(void *));
	}
      else 
	{
	  int i;
	  completer_funcs = (completer_func *)realloc(completer_funcs, completer_funcs_size * sizeof(completer_func));
	  multicompleter_funcs = (multicompleter_func *)realloc(multicompleter_funcs, completer_funcs_size * sizeof(multicompleter_func));
	  completer_data = (void **)realloc(completer_data, completer_funcs_size * sizeof(void *));
	  for (i = completer_funcs_end; i < completer_funcs_size; i++) completer_data[i] = NULL;
	}
    }
  completer_funcs[completer_funcs_end] = func;
  completer_data[completer_funcs_end] = data;
  completer_funcs_end++;
  return(completer_funcs_end - 1);
}


int add_completer_func_with_multicompleter(char *(*func)(widget_t w, const char *text, void *context), void *data, void (*multi_func)(widget_t w, void *data))
{
  int row;
  row = add_completer_func(func, data);
  multicompleter_funcs[row] = multi_func;
  return(row);
}


static int completion_matches = 0;

int get_completion_matches(void) {return(completion_matches);}
void set_completion_matches(int matches) {completion_matches = matches;}

static bool save_completions = 0;
static char **possible_completions = NULL;
static int possible_completions_size = 0;
static int possible_completions_ctr = 0;

void set_save_completions(bool save) {save_completions = save;}


void add_possible_completion(const char *text)
{
  if (save_completions)
    {
      if (possible_completions_size == possible_completions_ctr)
	{
	  possible_completions_size += 16;
	  if (!possible_completions)
	    possible_completions = (char **)calloc(possible_completions_size, sizeof(char *));
	  else
	    {
	      int i;
	      possible_completions = (char **)realloc(possible_completions, possible_completions_size * sizeof(char *));
	      for (i = possible_completions_ctr; i < possible_completions_size; i++) possible_completions[i] = NULL;
	    }
	}
      if (possible_completions[possible_completions_ctr]) free(possible_completions[possible_completions_ctr]);
      possible_completions[possible_completions_ctr] = mus_strdup(text);
      possible_completions_ctr++;
    }
}


int get_possible_completions_size(void) 
{
  return(possible_completions_ctr);
}


char **get_possible_completions(void) 
{
  return(possible_completions);
}


void handle_completions(widget_t w, int completer)
{
  if ((completer >= 0) &&
      (completer < completer_funcs_end) &&
      (multicompleter_funcs[completer]))
    (*multicompleter_funcs[completer])(w, completer_data[completer]);
}


char *complete_text(widget_t w, const char *text, int func)
{
  /* given text, call proc table entry func, return new text (not text!) */
  completion_matches = -1; /* i.e. no completer */
  possible_completions_ctr = 0;
  if ((func >= 0) && 
      (func < completer_funcs_end))
    return((*completer_funcs[func])(w, text, completer_data[func]));
  else return(mus_strdup(text));
}


void clear_possible_completions(void) 
{
  int i;
  for (i = 0; i < possible_completions_size; i++)
    if (possible_completions[i]) 
      {
	free(possible_completions[i]);
	possible_completions[i] = NULL;
      }
  possible_completions_ctr = 0;
}


static list_completer_info *srate_info = NULL;

static void init_srate_list(void)
{
  if (!srate_info)
    {
      int loc = 0;
      srate_info = (list_completer_info *)calloc(1, sizeof(list_completer_info));
      srate_info->exact_match = true;
      srate_info->values_size = 16;
      srate_info->values = (char **)calloc(srate_info->values_size, sizeof(char *));
      srate_info->values[loc++] = mus_strdup("44100");
      srate_info->values[loc++] = mus_strdup("22050");
      srate_info->values[loc++] = mus_strdup("8000");
      srate_info->values[loc++] = mus_strdup("48000");
      srate_info->num_values = loc;
    }
}


void add_srate_to_completion_list(int srate)
{
  char *str;
  int i;

  init_srate_list();
  str = (char *)malloc(16 * sizeof(char));
  snprintf(str, 16, "%d", srate);

  for (i = 0; i < srate_info->num_values; i++)
    if (mus_strcmp(srate_info->values[i], str))
      {
	free(str);
	return;
      }

  if (srate_info->num_values >= srate_info->values_size)
    {
      srate_info->values_size += 16;
      srate_info->values = (char **)realloc(srate_info->values, srate_info->values_size * sizeof(char *));
      for (i = srate_info->num_values; i < srate_info->values_size; i++) srate_info->values[i] = NULL;
    }

  srate_info->values[srate_info->num_values++] = str;
}


char *srate_completer(widget_t w, const char *text, void * data)
{
  init_srate_list();
  return(list_completer(w, text, (void *)srate_info));
}


#ifndef _MSC_VER
  #include <dirent.h>
#endif

enum {ANY_FILE_TYPE, SOUND_FILE_TYPE};


static char *filename_completer_1(widget_t w, const char *text, int file_type)
{
  /* assume text is a partial filename */
  /* get directory name, opendir, read files checking for match */
  /* return name of same form as original (i.e. don't change user's directory indication) */
  /* if directory, add "/" -- is_directory(name) static in snd-xfile.c */

  char *full_name = NULL, *dir_name = NULL, *file_name = NULL, *current_match = NULL;
  int i, j, k, len, curlen, matches = 0;
  DIR *dpos;

  if (mus_strlen(text) == 0) return(NULL);
  full_name = mus_expand_filename(text);
  len = mus_strlen(full_name);
  for (i = len - 1; i > 0; i--)
    if (full_name[i] == '/')
      break;

  dir_name = (char *)calloc(i + 1, sizeof(char));
  strncpy(dir_name, full_name, i);

  file_name = (char *)calloc(len - i + 2, sizeof(char));
  for (j = 0, k = i + 1; k < len; j++, k++) 
    file_name[j] = full_name[k];

  if (full_name) 
    {
      free(full_name); 
      full_name = NULL;
    }

  len = mus_strlen(file_name);
  if ((dpos = opendir(dir_name)))
    {
      struct dirent *dirp;
      while ((dirp = readdir(dpos)))
	if ((dirp->d_name[0] != '.') && 
	    (strncmp(dirp->d_name, file_name, len) == 0)) /* match dirp->d_name against rest of text */
	  {
	    if ((file_type == ANY_FILE_TYPE) ||
		(is_sound_file(dirp->d_name)))
	      {
		matches++;
		add_possible_completion(dirp->d_name);
		if (!current_match)
		  current_match = mus_strdup(dirp->d_name);
		else 
		  {
		    curlen = strlen(current_match);
		    for (j = 0; j < curlen; j++)
		      if (current_match[j] != dirp->d_name[j])
			{
			  current_match[j] = '\0';
			  break;
			}
		  }
	      }
	  }

      if (closedir(dpos) != 0) 
	snd_error("closedir %s failed (%s)!", dir_name, snd_io_strerror());
    }

  if (dir_name) free(dir_name);
  if (file_name) free(file_name);

  set_completion_matches(matches);

  if ((current_match) && 
      (*current_match))
    {
      /* attach matched portion to user's indication of dir */
      len = mus_strlen(text);
      for (i = len - 1; i >= 0; i--)
	if (text[i] == '/')
	  break;
      if (i < 0) return(current_match);
      curlen = strlen(current_match) + len + 3;
      file_name = (char *)calloc(curlen, sizeof(char));
      strncpy(file_name, text, i + 1);
      strcat(file_name, current_match);
      if (is_directory(file_name)) 
	strcat(file_name, "/");
      free(current_match);
      return(file_name);
    }
  return(mus_strdup(text));
}


char *filename_completer(widget_t w, const char *text, void *data)
{
  return(filename_completer_1(w, text, ANY_FILE_TYPE));
}


char *sound_filename_completer(widget_t w, const char *text, void *data)
{
  return(filename_completer_1(w, text, SOUND_FILE_TYPE));
}


static int find_indentation(char *str, int loc)
{
  int line_beg = 0, open_paren = -1, parens, i;
  parens = 0;
  for (i = loc - 1; i >= 0; i--)
    {
      if (str[i] == ')') parens--;
      if (str[i] == '(') parens++;
      if (parens == 1) 
	{
	  open_paren = i; 
	  break;
	}
    }
  if (open_paren == -1) return(1);
  if (open_paren == 0) return(3);
  for (i = open_paren - 1; i > 0; i--)
    if (str[i] == '\n') 
      {
	line_beg = i; 
	break;
      }
  if (line_beg == 0) return(1);
  return(open_paren - line_beg + 2);
}


char *complete_listener_text(char *old_text, int end, bool *try_completion, char **to_file_text)
{
  int len, i, k, spaces, text_pos = 0, cr_pos = 0;
  char *new_text = NULL, *file_text = NULL;

  len = strlen(old_text);
  for (i = len - 1; i > 0; i--)
    {
      if (old_text[i] == '\n')
	{
	  /* tab as indentation */
	  /* look at previous line to decide */
	  spaces = find_indentation(old_text, i);
	  if (spaces > 0)
	    {
	      file_text = (char *)calloc(spaces + 1, sizeof(char));
	      for (k = 0; k < spaces; k++) 
		file_text[k] = ' ';
	      file_text[spaces] = 0;
	      append_listener_text(end, file_text);
	      free(file_text);
	      file_text = NULL;
	    }
	  (*try_completion) = false;
	  return(NULL);
	}

      if (old_text[i] == ';')
	{
	  /* this isn't quite right, but how much effort should we put in it? */
	  spaces = 20;
	  for (k = i - 1; k > 0; k--) 
	    if (old_text[k] == '\n') 
	      {
		cr_pos = k; 
		break;
	      } 
	    else 
	      if ((!(isspace((int)(old_text[k])))) && 
		  (text_pos == 0)) 
		text_pos = k;
	  if (text_pos > 0)
	    text_pos -= cr_pos;
	  if (cr_pos == 0) spaces--; 
	  if (text_pos < spaces)
	    {
	      file_text = (char *)calloc(spaces + 2, sizeof(char));
	      for (k = text_pos + 1; k < spaces; k++) 
		file_text[k - text_pos - 1] = ' ';
	      file_text[spaces] = ';';
	      file_text[spaces + 1] = 0;
	      append_listener_text(end - 1, file_text);
	      free(file_text);
	      file_text = NULL;
	    }
	  (*try_completion) = false;
	  return(NULL);
	}

      if (old_text[i] == '\"')
	{
	  char *new_file;
	  file_text = mus_strdup((char *)(old_text + i + 1));
	  new_file = filename_completer(NULL_WIDGET, file_text, NULL);
	  len = mus_strlen(new_file);
	  if (len > 0)
	    {
	      len += i + 2;
	      new_text = (char *)calloc(len, sizeof(char));
	      strncpy(new_text, old_text, i + 1);
	      strcat(new_text, new_file);
	      free(new_file);
	    }
	  break;
	}
      if (isspace((int)(old_text[i]))) break;
    }

  if (!new_text) new_text = expression_completer(NULL_WIDGET, old_text, NULL);
  (*try_completion) = true;
  (*to_file_text) = file_text;
  return(new_text);
}


char *list_completer(widget_t w, const char *text, void *data)
{
  list_completer_info *info = (list_completer_info *)data;
  int i, j = 0, len, matches = 0, current_match = -1;
  char *trimmed_text;

  set_completion_matches(0);
  /* check for null text */
  len = mus_strlen(text);
  if (len == 0) return(mus_strdup(text));

  /* strip away leading and trailing white space */
  trimmed_text = (char *)calloc(len + 1, sizeof(char));
  for (i = 0; i < len; i++)
    if (!(isspace((int)text[i])))
      trimmed_text[j++] = text[i];

  if (j == 0)
    {
      free(trimmed_text);
      return(mus_strdup(text));
    }

  /* check for match(es) against values */
  if (info->exact_match)
    {
      for (i = 0; i < info->num_values; i++)
	if ((info->values[i]) &&
	    (strncmp(info->values[i], trimmed_text, len) == 0))
	  {
	    matches++;
	    current_match = i;
	  }
    }
  else
    {
      for (i = 0; i < info->num_values; i++)
	if ((info->values[i]) &&
	    (STRNCMP(info->values[i], trimmed_text, len) == 0))
	  {
	    matches++;
	    current_match = i;
	  }
    }
  free(trimmed_text);
  if (matches != 1)
    return(mus_strdup(text));
  set_completion_matches(1);

  return(mus_strdup(info->values[current_match]));
}
