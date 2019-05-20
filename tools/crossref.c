/* gcc -o crossref crossref.c -O2 -Wall */

#include <ctype.h>
#include <stddef.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>

#if 1
/* this is almost four times faster */
static bool local_strcmp(const char *s1, const char *s2)
{
  unsigned char c1, c2;
  while (true)
    {
      c1 = (unsigned char) *s1++;
      c2 = (unsigned char) *s2++;
      if (c1 != c2) return(false);
      if (c1 == '\0') break;
    }
  return(true);
}
#else
#define local_strcmp(S1, S2) (strcmp(S1, S2) == 0)
#endif

char **names;
char **hnames;
char **nnames;
char **files;
char **headers;
char **defs;
int **counts;
char ***lines;
#define MAX_LINES 16
int *voids;
int *results;
int *procs;

int names_size = 0, names_ctr = 0;
int files_size = 0, files_ctr = 0;
int headers_size = 0, headers_ctr = 0, nname_ctr = 0;
int snd_xen_c = -1;
int snd_noxen_c = -1;
int snd_nogui_c = -1;

char *mus_strdup(char *str)
{
  char *newstr = NULL;
  if (str)
    {
      newstr = (char *)calloc(strlen(str) + 1, sizeof(char));
      strcpy(newstr,str);
    }
  return(newstr);
}

void add_header(char *name)
{
  headers[headers_ctr++] = mus_strdup(name);
  if (headers_ctr == headers_size) fprintf(stderr,"oops headers");
}

int add_name(char *name, char *hdr)
{
  int i;
  
  if (!name) return(-1);
  if ((isdigit(name[0])) || (strlen(name) == 1)) return(-1);
  
  /*
  if (isupper(name[0])) return(-1);
  */
  if (name[0] == '_') return(-1);

  if ((local_strcmp(hdr, "snd-nogui0.h")) ||
      (local_strcmp(hdr, "snd-nogui1.h")))
    nnames[nname_ctr++] = name;
  for (i = 0; i < names_ctr; i++) 
    if (local_strcmp(names[i], name)) return(-1);
  hnames[names_ctr] = hdr;
  names[names_ctr++] = name;
  if (names_ctr == names_size) fprintf(stderr,"oops names");
  return(names_ctr - 1);
}

int in_nogui_h(char *name)
{
  int i;
  for (i = 0; i < nname_ctr; i++)
    if (local_strcmp(name, nnames[i]))
      return(1);
  return(0);
}

void add_file(char *name)
{
  if (local_strcmp(name,"snd-xen.c")) snd_xen_c = files_ctr;
  if (local_strcmp(name,"snd-nogui.c")) snd_nogui_c = files_ctr;
  files[files_ctr++] = mus_strdup(name);
  if (files_ctr == files_size) fprintf(stderr,"oops files");
}

static int add_count(char *name, int curfile)
{
  int i;
  for (i = 0; i < names_ctr; i++)
    if (local_strcmp(names[i], name))
      {
	if (!counts[i]) counts[i] = (int *)calloc(files_size, sizeof(int));
	counts[i][curfile] += 1;
	return(i);
      }
  return(-1);
}

static void add_def(char *name, int curfile)
{
  int i;
  for (i = 0; i < names_ctr; i++)
    if (local_strcmp(names[i], name))
      {
	if (!(defs[i]))
	  defs[i] = strdup(files[curfile]);
	return;
      }
}

/* #define MAX_CHARS 1048576 */
#define MAX_CHARS 8388608
/* xg.c is 3.2 Mb */

static char *get_call(char *input, int input_loc, int curname_len, char *curname, int chars, char *filename)
{
  int start = 0, end = 0, i;
  for (i = input_loc - curname_len; i >= 0; i--)
    if (input[i] == '\n')
      {
	start = i + 1;
	break;
      }
  if (start == 0)
    {
      if (input[0] == '_')
	return(NULL);
      /* fprintf(stderr, "%s: %s at %d found no begin cr\n", filename, curname, input_loc); */
    }
  for (i = input_loc; i < chars; i++)
    {
      if (input[i] == ')')
	{
	  end = i + 1;
	  break;
	}
      if (input[i] == '\n')
	{
	  end = i;
	  break;
	}
    }
  if (end == 0)
    fprintf(stderr, "%s at %d found no end\n", curname, input_loc);
  if ((start != 0) && (end > start))
    {
      char *value;
      int n, k, m;
      m = strlen(filename);
      value = (char *)calloc(end - start + 1 + m + 4, sizeof(char));
      for (n = 0; n < m; n++)
	value[n] = filename[n];
      value[n++] = ' ';
      value[n++] = ':';
      value[n++] = ' ';
      k = n;
      for (n = start; n < end; n++, k++)
	value[k] = input[n];
      return(value);
    }
  return(NULL);
}

static int get_result(char *input, int input_loc, int curname_len)
{
  int i;
  for (i = input_loc - curname_len; i >= 0; i--)
    {
      if ((input[i] == '=') || (input[i] == '(') || (input[i] == ',')) return(1);
      if ((input[i] == '+') || (input[i] == '-') || (input[i] == '*') || (input[i] == '/')) return(1);
      if ((input[i] == '&') || (input[i] == '|') || (input[i] == '^') || (input[i] == '!') || (input[i] == '?')) return(1);
      if ((input[i] == ';') || (input[i] == '{')) return(0);
      if ((input[i] == ')') && (input[i - 1] != '*')) return(0);
    }
  return(1);
}

typedef struct {
  char *name, *hname, *def;
  int i, calls, v, results, proc;
} qdata;

static int greater_compare(const void *a, const void *b)
{
  qdata *d1 = *(qdata **)a;
  qdata *d2 = *(qdata **)b;
  if (d1->calls > d2->calls) 
    return(1); 
  else 
    {
      if (d1->calls == d2->calls) 
	return(0); 
      else return(-1);
    }
}

#define NAME_SIZE 8192
#define ID_SIZE 16384

int main(int argc, char **argv)
{
  int i, j, fd, chars, k, in_comment = 0, in_cpp_comment = 0, calls = 0, in_parens = 0, in_quotes = 0, in_define = 0, in_curly = 0, in_enum = 0;
  int maxc[NAME_SIZE], maxf[NAME_SIZE], maxg[NAME_SIZE], mcalls[NAME_SIZE];
  qdata **qs;
  char input[MAX_CHARS];
  char curname[ID_SIZE];
  FILE *FD = NULL;
  
  names_size = NAME_SIZE;
  names = (char **)calloc(names_size, sizeof(char *));
  hnames = (char **)calloc(names_size, sizeof(char *));
  nnames = (char **)calloc(names_size, sizeof(char *));
  defs = (char **)calloc(names_size, sizeof(char *));
  voids = (int *)calloc(names_size, sizeof(int));
  files_size = 256;
  files = (char **)calloc(files_size, sizeof(char *));
  headers_size = 32;
  headers = (char **)calloc(headers_size, sizeof(char *));
  counts = (int **)calloc(names_size, sizeof(int *));
  lines = (char ***)calloc(names_size, sizeof(char **));
  results = (int *)calloc(names_size, sizeof(int));
  procs = (int *)calloc(names_size, sizeof(int));

  add_header("sndlib.h");
  add_header("clm.h");
  add_header("vct.h");
  add_header("sndlib2xen.h");
  add_header("clm2xen.h");
  add_header("snd.h");
  add_header("glistener.h");
#if 1
  add_header("snd-strings.h");
  add_header("sndlib-strings.h");
  add_header("clm-strings.h");
#endif
  add_header("snd-0.h");
  add_header("snd-1.h");
  add_header("snd-x0.h");
  add_header("snd-x1.h");
  add_header("snd-g0.h");
  add_header("snd-g1.h");
  add_header("snd-nogui0.h");
  add_header("snd-nogui1.h");
  add_header("libclm.def");
  add_header("snd-menu.h");
  add_header("snd-file.h");

  add_header("s7.h");
  add_file("s7.c");

  /* add_file("xen.h"); */
  /* add_file("snd.h"); */

  /* these need to be last */
  add_header("xen.h");
  add_header("mus-config.h.in");

  add_file("glistener.c");
  add_file("headers.c");
  add_file("audio.c");
  add_file("io.c");
  add_file("sound.c");
  add_file("clm.c");
  add_file("vct.c");
  add_file("sndlib2xen.c");
  add_file("clm2xen.c");
  add_file("snd-io.c");
  add_file("snd-utils.c");
  add_file("snd-completion.c");
  add_file("snd-menu.c");
  add_file("snd-draw.c");
  add_file("snd-axis.c");
  add_file("snd-data.c");
  add_file("snd-fft.c");
  add_file("snd-marks.c");
  add_file("snd-file.c");
  add_file("snd-edits.c");
  add_file("snd-chn.c");
  add_file("snd-sig.c");
  add_file("snd-kbd.c");
  add_file("snd-dac.c");
  add_file("snd-region.c");
  add_file("snd-select.c");
  add_file("snd-find.c");
  add_file("snd-snd.c");
  add_file("snd-help.c");
  add_file("snd-main.c");
  add_file("snd-print.c");
  add_file("snd-trans.c");
  add_file("snd-mix.c");
  add_file("snd.c");
  add_file("snd-env.c");
  add_file("snd-xen.c");
  add_file("snd-ladspa.c");
  add_file("snd-motif.c");
  add_file("snd-listener.c");
  add_file("snd-xref.c");
  add_file("snd-gxbitmaps.c");
  add_file("snd-gxcolormaps.c");
  add_file("snd-gutils.c");
  add_file("snd-gfind.c");
  add_file("snd-gmenu.c");
  add_file("snd-gdraw.c");
  add_file("snd-glistener.c");
  add_file("snd-gchn.c");
  add_file("snd-gsnd.c");
  add_file("snd-gregion.c");
  add_file("snd-gmain.c");
  add_file("snd-gmix.c");
  add_file("snd-genv.c");
  add_file("snd-gfft.c");
  add_file("snd-gfile.c");
  add_file("snd-gprefs.c");
  add_file("snd-prefs.c");
  add_file("snd-nogui.c");
  add_file("xen.c");
  add_file("xm.c");
  add_file("gl.c");
  add_file("xg.c");

  add_file("libc_s7.c");
  add_file("libgdbm_s7.c");
  add_file("libgsl_s7.c");
  add_file("libgtk_s7.c");
  add_file("libm_s7.c");
  add_file("utf8proc_s7.c");
  
  add_file("xen.h");

  add_file("cmus.c");
  add_file("ffi.lisp");
  add_file("sndlib2clm.lisp");
  add_file("run.lisp");
  add_file("clm1.lisp");

  add_file("sndins/sndins.c");
  add_file("ffitest.c");
  add_file("tools/gcall.c");

  add_file("/home/bil/dist/snd/s7webserver/s7webserver.cpp");
  add_file("/home/bil/test/rad/newrad/scheme.cpp");

  add_file("/home/bil/test/cm/src/CmSupport.cpp");
  add_file("/home/bil/test/cm/src/Scheme.cpp");
  add_file("/home/bil/test/cm/src/SndLib.cpp");
  add_file("/home/bil/test/cm/src/SndLibBridge.cpp");
  add_file("/home/bil/test/cm/src/s7.cpp");
  add_file("/home/bil/test/cm/src/OscPack.cpp");
  add_file("/home/bil/test/cm/src/SchemeSources.cpp");
  add_file("/home/bil/test/cm/src/OpenSoundControl.cpp");
  add_file("/home/bil/test/cm/src/Liblo.cpp");

  for (i = 0; i < headers_ctr; i++)
    {
      k = 0;
      in_quotes = 0;
      in_parens = 0;
      in_comment = 0;
      in_cpp_comment = 0;
      fd = open(headers[i], O_RDONLY, 0);
      if (fd == -1)
	fprintf(stderr, "can't find %s\n", headers[i]);
      else
	{
	  do 
	    {
	      chars = read(fd, input, MAX_CHARS);
	      for (j = 0; j < chars; j++)
		{
		  if ((in_comment == 0) && (in_cpp_comment == 0) && (in_curly == 0))
		    {
		      if ((isalpha(input[j])) || (isdigit(input[j])) || (input[j] == '_'))
			{
			  if (k < ID_SIZE)
			    curname[k++] = input[j];
			  else fprintf(stderr, "0: curname overflow: %s[%d]: %s%c\n", headers[i], j, curname, input[j]);
			}
		      else
			{
			  if (k < ID_SIZE)
			    curname[k] = 0;
			  else fprintf(stderr, "1: curname overflow: %s[%d]: %s\n", headers[i], j, curname);

			  /* fprintf(stderr, "%s name: %s %d %d %d\n", headers[i], curname, k, in_parens, in_quotes); */

			  if ((k > 0) && (in_parens == 0) && (in_quotes == 0))
			    {
			      int loc;
			      loc = add_name(mus_strdup(curname), headers[i]);
			      if (loc >= 0)
				{
				  int start, n, maybe_proc = 1;
				  for (n = 0; n < k; n++)
				    if (isupper(curname[n]))
				      {
					maybe_proc = 0;
					break;
				      }
				  if ((maybe_proc) && ((input[j] == '(') || ((input[j] == ' ') && (input[j + 1] == '('))))
				    procs[loc] = maybe_proc;
				  else procs[loc] = 0;
				  start = j - strlen(curname) - 6;
				  if (start >= 0)
				    {
				      int m;
				      for (m = 0; m < 3; m++)
					if (strncmp((char *)(input + start + m), "void", 4) == 0)
					  {
					    voids[loc] = 1;
					    break;
					  }
				    }
				}
			    }
			  /* else if (k > 0) fprintf(stderr,"drop %s %d %d\n",curname, in_parens, in_quotes); */
			  k = 0;
			  if ((input[j] == '/') && (input[j + 1] == '*'))
			    in_comment = 1;
			  else 
			    {
			      if ((input[j] == '/') && (input[j + 1] == '/'))
				in_cpp_comment = 1;
			      else 
				{
				  if (input[j] == '#')
				    in_define = 1;
				  else
				    {
				      if ((input[j] == '{') && 
					  ((j < 6) || (strncmp((input + (j - 5)), "enum", 4) != 0)))
					in_curly = 1;
				      else
					{
					  if (input[j] == '(') in_parens++;
					  if (input[j] == ')') in_parens--;
					  if (input[j] == '"')
					    {
					      if (in_quotes == 1)
						in_quotes = 0;
					      else in_quotes = 1;
					    }
					}
				    }
				}
			    }
			}
		    }
		  else /* in comment or curly */
		    {
		      if ((input[j] == '*') && (input[j + 1] == '/'))
			in_comment = 0;
		      else 
			{
			  if (input[j] == '}')
			    in_curly = 2;
			  else
			    {
			      if (input[j] == ';')
				in_curly = 0;
			      else
				{
				  if (input[j] == '\n')
				    in_cpp_comment = 0;
				}
			    }
			}
		    }
		}
	    }
	  while (chars == MAX_CHARS);
	  close(fd);
	}
    }

  FD = fopen("xref.data","w");
  if (!FD)
    fprintf(stderr, "can't write xref.data?");
  else
    {
      fprintf(stderr, "%d names ", names_ctr);

      in_comment = 0;
      in_cpp_comment = 0;
      in_define = 0;
      in_enum = 0;
      for (i = 0; i < files_ctr; i++)
	{
	  char **all_names = NULL;
	  int all_names_size = 0, all_names_top = 0;
	  int *all_names_counts = NULL;
	  
	  k = 0;
	  fd = open(files[i], O_RDONLY, 0);
	  if (fd == -1)
	    fprintf(stderr, "can't find %s\n", files[i]);
	  else
	    {
	      int curly_ctr = 0, paren_ctr = 0, cancel_define = 0;
	      bool got_macro = false;
	      in_define = 0;
	      
	      if ((local_strcmp(files[i], "sndlib2clm.lisp")) ||
		  (local_strcmp(files[i], "ffi.lisp")))
		curly_ctr = 1;
	      
	      do 
		{
		  chars = read(fd, input, MAX_CHARS);
		  /* fprintf(stderr,"%s %d\n", files[i], chars); */
		  
		  for (j = 0; j < chars; j++)
		    {
		      if ((in_comment == 0) && (in_cpp_comment == 0))
			{
			  if ((isalpha(input[j])) || (isdigit(input[j])) || (input[j] == '_'))
			    {
			      if (k < ID_SIZE)
				curname[k++] = input[j];
			      else fprintf(stderr, "2: curname overflow: %s[%d]: %s\n", files[i], j, curname);
			    }
			  else
			    {
			      if ((input[j] == '/') && (input[j + 1] == '*'))
				in_comment = 1;
			      else
				{
				  if ((input[j] == '/') && (input[j + 1] == '/'))
				    in_cpp_comment = 1;
				  else
				    {
				      if ((input[j] == '#') && 
					  (((input[j + 1] == 'd') && (input[j + 2] == 'e')) || 
					   ((input[j + 1] == 'u') && (input[j + 2] == 'n'))))
					{
					  in_define = 1;
					  got_macro = false;
					}
				      else
					{
					  if ((in_define == 1) && (input[j] == '\n') && (j > 0) && (input[j - 1] != '\\'))
					    {
					      cancel_define = 1;
					    }
					}
				      if ((in_define == 0) && 
					  (j < (chars - 1)) && 
					  ((input[j - 1] != '\'') || (input[j + 1] != '\'')))
					{
					  if (input[j] == '{') 
					    {
					      curly_ctr++;
					      if ((j > 4) && 
						  (strncmp((input + (j - 5)), "enum", 4) == 0))
						in_enum = true;
					    }
					  else 
					    {
					      if (input[j] == '}') 
						{
						  curly_ctr--;
						  if (in_enum) in_enum = false;
						}
					    }
					}
				    }
				}

			      if ((input[j + 1] == ' ') &&
				  (input[j + 2] == '{') &&
				  (input[j + 3] == '}'))
				{
				  k = 0;
				}
			      
			      if (k > 0)
				{
				  if (k < ID_SIZE)
				    curname[k] = 0;
				  else fprintf(stderr, "3: curname overflow: %s[%d]: %s\n", files[i], j, curname);
				  
				  if (true)
				    {
				      bool happy = false;
				      int m;
				      if (!all_names)
					{
					  all_names_size = 1024;
					  all_names_top = 0;
					  all_names = (char **)calloc(all_names_size, sizeof(char *));
					  all_names_counts = (int *)calloc(all_names_size, sizeof(int));
					}
				      
				      for (m = 0; m < all_names_top; m++)
					if (local_strcmp(curname, all_names[m]))
					  {
					    happy = true;
					    all_names_counts[m]++;
					  }
				      
				      if (((!got_macro) && 
					   (in_define == 1) && 
					   (!local_strcmp(curname, "define")) &&
					   (!local_strcmp(curname, "undef"))) ||
					  (in_enum))
					{
					  got_macro = true;
					  if (!happy)
					    {
					      all_names[all_names_top++] = strdup(curname);
					      got_macro = true;
					      if (all_names_top == all_names_size)
						{
						  all_names_size *= 2;
						  all_names = (char **)realloc(all_names, all_names_size * sizeof(char *));
						  all_names_counts = (int *)realloc(all_names_counts, all_names_size * sizeof(int));
						  for (m = all_names_top; m < all_names_size; m++)
						    {
						      all_names[m] = NULL;
						      all_names_counts[m] = 0;
						    }
						}
					    }
					}
				    }

				  if ((k < ID_SIZE) && (curly_ctr == 0) && (paren_ctr <= 0))
				    {
				      add_def(curname, i);
				    }
				  
				  if ((k < ID_SIZE) && 
				      ((curly_ctr > 0) || (in_define == 1) || (paren_ctr > 0)))
				    {
				      int loc;
				      loc = add_count(curname, i);
				      if (loc >= 0)
					{
					  if (procs[loc])
					    results[loc] += get_result(input, j, k);
					  if (!lines[loc])
					    {
					      lines[loc] = (char **)calloc(MAX_LINES, sizeof(char *));
					      lines[loc][0] = get_call(input, j, k, curname, chars, files[i]);
					    }
					  else
					    {
					      int m;
					      for (m = 0; m < MAX_LINES; m++)
						if (!lines[loc][m])
						  {
						    lines[loc][m] = get_call(input, j, k, curname, chars, files[i]);
						    break;
						  }
					    }
					}
				    }
				  k = 0;
				}
			      if (cancel_define == 1)
				{
				  cancel_define = 0;
				  in_define = 0;
				}
			    }
			  if (input[j] == '(') paren_ctr++;
			  else if (input[j] == ')') paren_ctr--;
			}
		      else
			{
			  if ((input[j] == '*') && (input[j + 1] == '/'))
			    in_comment = 0;
			  else
			    {
			      if (input[j] == '\n')
				in_cpp_comment = 0;
			    }
			}
		    }
		}
	      while (chars == MAX_CHARS);
	      close(fd);
	      
	      {
		int m;
		bool name_printed = false;
		for (m = 0; m < all_names_top; m++)
		  if ((all_names_counts[m] == 0) &&
		      (all_names[m][0] != '_') &&
		      (((all_names[m][0] != 'Q') && (all_names[m][0] != 'H')) || (all_names[m][1] != '_')))
			
		    {
		      if (!name_printed)
			{
			  fprintf(FD, "\n%s: ", files[i]);
			  name_printed = true;
			}
		      fprintf(FD, "%s ", all_names[m]);
		    }
		for (m = 0; m < all_names_top; m++)
		  if (all_names[m]) free(all_names[m]);
		if (all_names) free(all_names);
		if (all_names_counts) free(all_names_counts);
		all_names = NULL;
		all_names_counts = NULL;
		all_names_size = 0;
		all_names_top = 0;
	      }
	      
	    }
	}
      
      for (i = 0; i < names_ctr; i++)
	{
	  maxc[i] = 0;
	  maxf[i] = 0;
	  maxg[i] = 0;
	  for (j = 0; j < files_ctr; j++)
	    if ((counts[i]) && (counts[i][j] > 0)) 
	      {
		maxc[i] += counts[i][j]; 
		maxf[i]++;
		if ((j == snd_xen_c) || (j == snd_nogui_c)) maxg[i]++;
	      }
	}
      for (i = 0; i < names_ctr; i++)
	{
	  calls = 0;
	  if (counts[i])
	    for (j = 0; j < files_ctr; j++)
	      calls += counts[i][j];
	  mcalls[i] = calls;
	}
      qs = (qdata **)calloc(NAME_SIZE, sizeof(qdata *));
      for (i = 0; i < names_ctr; i++)
	{
	  qdata *q;
	  q = (qdata *)calloc(1, sizeof(qdata));
	  qs[i] = q;
	  q->i = i;
	  q->v = voids[i];
	  q->name = names[i];
	  q->def = defs[i];
	  q->hname = hnames[i];
	  q->calls = mcalls[i];
	  q->results = results[i];
	  q->proc = procs[i];
	}
      qsort((void *)qs, names_ctr, sizeof(qdata *), greater_compare);
      for (i = 0; i < names_ctr; i++)
	{
	  bool menu_case, file_case, nonogui_case, static_case, x_case = true, ffitest_case;
	  int menu_count = 0, file_count = 0, x_count = 0;
	  int nfiles;
	  nfiles = 0;
	  /* try to get rid of a bunch of annoying false positives */
	  if ((local_strcmp(qs[i]->hname, "xen.h")) || 
	      (local_strcmp(qs[i]->hname, "mus-config.h.in")) ||
	      (qs[i]->name[0] == '_') ||
	      ((qs[i]->name[strlen(qs[i]->name) - 2] == '_') &&
	       ((qs[i]->name[strlen(qs[i]->name) - 1] == 't') || 
		(qs[i]->name[strlen(qs[i]->name) - 1] == 'H')))) /* SND_0_H etc */
	    {
	    }
	  else
	    {
	      if (qs[i]->def)
		fprintf(FD, "\n\n%s: %d [%s, %s]", qs[i]->name, qs[i]->calls, qs[i]->hname, qs[i]->def);
	      else fprintf(FD, "\n\n%s: %d [%s, <no function definition found>]", qs[i]->name, qs[i]->calls, qs[i]->hname);
	      if (qs[i]->v) 
		{
		  fprintf(FD, " (void)");
		}
	      else
		{
		  if ((qs[i]->results == 0) && (qs[i]->proc > 0) && (qs[i]->calls > 0) &&
		      (strncmp(qs[i]->name, "set_", 4) != 0) &&
		      (strncmp(qs[i]->name, "in_set_", 7) != 0))
		    fprintf(FD, " (not void but result not used?)");
		}
	      if (qs[i]->calls == 0)
		{
		  char buf1[512], buf2[512];
		  snprintf(buf1, 512, "fgrep -q %s xen.h", qs[i]->name);
		  snprintf(buf2, 512, "fgrep -q %s s7.html", qs[i]->name);
		  if (system((const char *)buf1) == 0) 
		    {
		      if (system((const char *)buf2) == 0) 
			fprintf(FD, " (used in xen.h and s7.html)");
		      else fprintf(FD, " (used in xen.h)");
		    }
		  else
		    {
		      if (system((const char *)buf2) == 0)
			fprintf(FD, " (used in s7.html)");
		    }
		}
		  
	      menu_case = (!local_strcmp(qs[i]->hname, "snd-menu.h"));
	      file_case = (!local_strcmp(qs[i]->hname, "snd-file.h"));
	      static_case = ((qs[i]->def) && (qs[i]->calls > 0));
	      ffitest_case = ((qs[i]->def) && (qs[i]->calls > 0));

	      menu_count  = 0;
	      file_count = 0;
	      
	      nonogui_case = in_nogui_h(qs[i]->name);
	      if ((nonogui_case) && (counts[qs[i]->i]))
		{
		  /* fprintf(stderr, "%s...", qs[i]->name); */
		  for (j = 0; j < files_ctr; j++)
		    if ((counts[qs[i]->i][j] > 0) &&
			((local_strcmp(files[j], "snd-xen.c")) ||
			 ((!local_strcmp(files[j], "snd-nogui.c")) &&
			  (!local_strcmp(files[j], "snd-motif.c")) &&
			  (strncmp(files[j], "snd-g", 5) != 0))))
		      {
			/* fprintf(stderr,"in %s\n", files[j]); */
			nonogui_case = false;
			break;
		      }
		  /* if (nonogui_case) fprintf(stderr, "!\n"); */
		}
	      
	      for (j = 0; j < files_ctr; j++)
		{
		  if ((counts[qs[i]->i]) && (counts[qs[i]->i][j] > 0))
		    {
		      if ((ffitest_case) &&
			  (!local_strcmp(files[j], "ffitest.c")))
			ffitest_case = false;

		      if (menu_case)
			{
			  if ((!local_strcmp(files[j], "snd-menu.c")) &&
			      (!local_strcmp(files[j], "snd-motif.c")) &&
			      (!local_strcmp(files[j], "snd-gmenu.c")))
			    {
			      if (!local_strcmp(files[j], "snd-nogui.c"))
				menu_case = false;
			    }
			  else menu_count++;
			}
		      
		      if (x_case)
			{
			  if (((!local_strcmp(files[j], "snd-motif.c")) &&
			       (strncmp(files[j], "snd-g", 5) != 0)) ||
			      (local_strcmp(files[j], "snd-xen.c")))
			    x_case = false;
			  else x_count++;
			}

		      if (file_case)
			{
			  if ((!local_strcmp(files[j], "snd-file.c")) &&
			      (!local_strcmp(files[j], "snd-motif.c")) &&
			      (!local_strcmp(files[j], "snd-gfile.c")))
			    {
			      if (!local_strcmp(files[j], "snd-nogui.c"))
				file_case = false;
			    }
			  else file_count++;
			}

		      if ((static_case) &&
			  (!local_strcmp(files[j], qs[i]->def)) &&
			  (!local_strcmp(files[j], "snd-nogui.c")))
			{
			  if (((local_strcmp(files[j], "snd-motif.c")) || 
			       (strncmp(files[j], "snd-g", 5) == 0)) &&
			      ((local_strcmp(qs[i]->def, "snd-motif.c")) || 
			       (strncmp(qs[i]->def, "snd-g", 5) == 0)) &&
			      (local_strcmp((const char *)(files[j] + 5), (const char *)(qs[i]->def + 5))))
			    {
			    }
			  else static_case = false;
			}
		      
		      fprintf(FD,"\n    %s: %d", files[j], counts[qs[i]->i][j]);
		      nfiles++;
		    }
		}

	      if (ffitest_case)
		{
		  char buf1[512], buf2[512];
		  snprintf(buf1, 512, "fgrep -q %s xen.h", qs[i]->name);
		  snprintf(buf2, 512, "fgrep -q %s s7.html", qs[i]->name);
		  if ((system((const char *)buf1) != 0) &&
		      (system((const char *)buf2) != 0))
		    fprintf(FD, "\njust ffitest!");
		}

	      if ((menu_case) && 
		  (menu_count > 0) &&
		  ((!(qs[i]->def)) ||
		   (local_strcmp(qs[i]->def, "snd-menu.c")) ||
		   (local_strcmp(qs[i]->def, "snd-motif.c")) ||
		   (local_strcmp(qs[i]->def, "snd-gmenu.c"))))
		fprintf(FD, "\n->SND-MENU.H\n");

	      if ((file_case) && 
		  (file_count > 0) &&
		  ((!(qs[i]->def)) ||
		   (local_strcmp(qs[i]->def, "snd-file.c")) ||
		   (local_strcmp(qs[i]->def, "snd-motif.c")) ||
		   (local_strcmp(qs[i]->def, "snd-gfile.c"))))
		fprintf(FD, "\n->SND-FILE.H\n");

	      if ((x_case) &&
		  (x_count > 0) &&
		  (qs[i]->def) &&
		  (local_strcmp(qs[i]->hname, "snd-1.h")) &&
		  (!local_strcmp(qs[i]->def, "snd-xen.c")) &&
		  ((local_strcmp(qs[i]->def, "snd-motif.c")) || 
		   (strncmp(qs[i]->def, "snd-g", 5) == 0)))
		fprintf(FD, "\n    (all within gui)\n");

	      if (nonogui_case) fprintf(FD, "\nnot needed in snd-nogui?\n");

	      if (static_case) fprintf(FD, "\ncould be static?\n");

	      {
		int m;
		if ((nfiles > 0) && (lines[qs[i]->i]))
		  {
		    fprintf(FD, "\n");
		    for (m = 0; m < MAX_LINES; m++)
		      {
			if (!lines[qs[i]->i][m]) break;
			fprintf(FD, "\n        %s", lines[qs[i]->i][m]);
		      }
		  }
	      }
	      fprintf(FD, "\n----------------");
	    }
	}
      fclose(FD);
    }
  return(0);
}
