/*****************************************************************************/

/* Snd LADSPA Support - Copyright 2000 Richard W.E. Furse. */

/*****************************************************************************/

#include "snd.h"

#if HAVE_LADSPA

#include <dlfcn.h>
#include <ladspa.h>
#include <dirent.h>

/* 
 * 19-Jun-07 added beg dur snd chn args to apply-ladspa.
 * 12-Jun-07 reader can be #f.
 * 1-Sep-05  moved stuff around for better error handling.
 *           added code to handle 0-input plugins ("analog osc" in swh for example).
 * 21-Sep-03 added plugin help menu item.
 * 1-Aug-03  added direct struct readers for LADSPA_Descriptor
 * 21-Nov-02 better checks for C-g interrupt.
 * 2-May-02  use mus_long_t for sample number.
 * 14-Dec-01 various C++ cleanups.
 * 28-Nov-01 input chans need not equal output chans now.
 * 15-Oct-01 added some error returns (rather than snd_error).  multichannel plugin support.
 * 20-Sep-01 changed location of pfInputBuffer to avoid glomming up the stack with a huge array.
 */

/*****************************************************************************/


typedef struct {
  char *m_pcPackedFilename;
  const char *m_pcLabel;
  const LADSPA_Descriptor *m_psDescriptor;
  void *m_pvPluginHandle;
} LADSPAPluginInfo;


/*****************************************************************************/

static char g_bLADSPAInitialised = 0;
static LADSPAPluginInfo ** g_psLADSPARepository;
static long g_lLADSPARepositoryCapacity;
static long g_lLADSPARepositoryCount;

#define LADSPA_REPOSITORY_CAPACITY_STEP 100


/*****************************************************************************/


static int lInputCount, lOutputCount;

static void isLADSPAPluginSupported(const LADSPA_Descriptor *psDescriptor) 
{
  uint32_t lIndex;
  lInputCount = lOutputCount = 0;
  for (lIndex = 0; lIndex < psDescriptor->PortCount; lIndex++) 
    {
      LADSPA_PortDescriptor iPortDescriptor;
      iPortDescriptor = psDescriptor->PortDescriptors[lIndex];
      if (LADSPA_IS_PORT_AUDIO(iPortDescriptor)) 
	{
	  if (LADSPA_IS_PORT_INPUT(iPortDescriptor))
	    lInputCount++;
	  else
	    lOutputCount++;
	}
    }
}


/*****************************************************************************/

/* Assumes repository initialised, returns NULL if not found. */
static const LADSPA_Descriptor *findLADSPADescriptor(const char *pcPackedFilename, const char *pcLabel) 
{
  long lIndex;
  for (lIndex = 0; lIndex < g_lLADSPARepositoryCount; lIndex++) 
    {
      LADSPAPluginInfo *psInfo;
      psInfo = g_psLADSPARepository[lIndex];
      if ((mus_strcmp(pcLabel, psInfo->m_pcLabel)) &&
	  (mus_strcmp(pcPackedFilename, psInfo->m_pcPackedFilename)))
	return psInfo->m_psDescriptor;
    }
  return NULL;
}


/*****************************************************************************/


/* Allocate a new string. The string will contain a library filename,
   stripped of path and .so (if present) */

static char *packLADSPAFilename(const char * pcFilename) 
{

  const char *pcStart, * pcEnd;
  char *pcPackedFilename;

  /* Move start past last /, move pcEnd to end. */
  pcStart = pcFilename;
  for (pcEnd = pcStart; *pcEnd != '\0'; pcEnd++)
    if (*pcEnd == '/')
      pcStart = pcEnd + 1;
  if (pcEnd - pcStart > 3)
    if (strcmp(".so", pcEnd - 3) == 0)
      pcEnd -= 3;

  pcPackedFilename = (char *)malloc(pcEnd - pcStart + 1);
  memcpy(pcPackedFilename, pcStart, pcEnd - pcStart);
  pcPackedFilename[pcEnd - pcStart] = '\0';

  return pcPackedFilename;
}


/*****************************************************************************/


static void unloadLADSPA(void) 
{
  long lIndex;
  LADSPAPluginInfo *pvPluginHandle = NULL;
  if (g_lLADSPARepositoryCount > 0)
    pvPluginHandle = (LADSPAPluginInfo *)(g_psLADSPARepository[0]->m_pvPluginHandle);
  pvPluginHandle++;
  for (lIndex = 0; lIndex < g_lLADSPARepositoryCount; lIndex++) 
    {
      LADSPAPluginInfo *psInfo;
      psInfo = g_psLADSPARepository[lIndex];
      free(psInfo->m_pcPackedFilename);
      /* Don't free Label or Descriptor - this memory is owned by the
	 relevant plugin library. */
      if (pvPluginHandle != psInfo->m_pvPluginHandle) 
	{
	  pvPluginHandle = (LADSPAPluginInfo *)(psInfo->m_pvPluginHandle);
	  dlclose(pvPluginHandle);
	}
      free(psInfo);
    }
  free(g_psLADSPARepository);
  g_bLADSPAInitialised = 0;
}


/*****************************************************************************/


/* Called only from within loadLADSPA->loadLADSPADirectory. */

static void loadLADSPALibrary(void *pvPluginHandle,
			      char *pcFilename,
			      LADSPA_Descriptor_Function fDescriptorFunction) 
{
  LADSPAPluginInfo **psOldRepository;
  long lNewCapacity, lIndex;
  const LADSPA_Descriptor *psDescriptor;

  for (lIndex = 0;
       (psDescriptor = fDescriptorFunction(lIndex));
       lIndex++)
    {
      LADSPAPluginInfo *psInfo;
      if (g_lLADSPARepositoryCount == g_lLADSPARepositoryCapacity) 
	{
	  psOldRepository = g_psLADSPARepository;
	  lNewCapacity = (g_lLADSPARepositoryCapacity
			  + LADSPA_REPOSITORY_CAPACITY_STEP);
	  g_psLADSPARepository = (LADSPAPluginInfo **)malloc(lNewCapacity * sizeof(LADSPAPluginInfo *));
	  memcpy(g_psLADSPARepository,
		 psOldRepository,
		 sizeof(LADSPAPluginInfo *) * g_lLADSPARepositoryCount);
	  g_lLADSPARepositoryCapacity = lNewCapacity;
	  free(psOldRepository);
	}
      psInfo
	= g_psLADSPARepository[g_lLADSPARepositoryCount++]
	= (LADSPAPluginInfo *)malloc(sizeof(LADSPAPluginInfo));
      psInfo->m_pcPackedFilename = packLADSPAFilename(pcFilename);
      psInfo->m_pcLabel = psDescriptor->Label;
      psInfo->m_psDescriptor = psDescriptor;
      psInfo->m_pvPluginHandle = pvPluginHandle;
    }
}


/*****************************************************************************/


/* Search just the one directory. Called only from within
   loadLADSPA. */

static void loadLADSPADirectory(const char *pcDirectory) 
{
  DIR *psDirectory;
  LADSPA_Descriptor_Function fDescriptorFunction;
  long lDirLength;
  long iNeedSlash;
  
  lDirLength = strlen(pcDirectory);
  if (!lDirLength)
    return;
  if (pcDirectory[lDirLength - 1] == '/')
    iNeedSlash = 0;
  else
    iNeedSlash = 1;
  
  psDirectory = opendir(pcDirectory);
  if (!psDirectory)
    return;
  
  while (true) 
    {
      char *pcFilename = NULL;
      struct dirent *psDirectoryEntry;
      void *pvPluginHandle;
      
      psDirectoryEntry = readdir(psDirectory);
      if (!psDirectoryEntry) 
	{
	  closedir(psDirectory);
	  return;
	}
      
      pcFilename = (char *)malloc(lDirLength
				  + strlen(psDirectoryEntry->d_name)
				  + 1 + iNeedSlash);
      strcpy(pcFilename, pcDirectory);
      if (iNeedSlash)
	strcat(pcFilename, "/");
      strcat(pcFilename, psDirectoryEntry->d_name);
      
      pvPluginHandle = dlopen(pcFilename, RTLD_LAZY);
      if (pvPluginHandle) 
	{
	  /* This is a file and the file is a shared library! */
	  
	  dlerror();
	  fDescriptorFunction
	    = (LADSPA_Descriptor_Function)dlsym(pvPluginHandle,
						"ladspa_descriptor");
	  if ((!dlerror()) && (fDescriptorFunction))
	    {
	      loadLADSPALibrary(pvPluginHandle, pcFilename, fDescriptorFunction);
	    }
	  else 
	    {
	      /* It was a library, but not a LADSPA one. Unload it. */
	      /* bil: this is not safe! Could be legit already-loaded library. */
	      /* dlclose(pcFilename); */
	    }
	}
      if (pcFilename) free(pcFilename);
      pcFilename = NULL;
  }
}


/*****************************************************************************/


static void loadLADSPA(void) 
{
  const char *pcEnd;
  const char *pcLADSPAPath;
  const char *pcStart;

  g_bLADSPAInitialised = 1;
  g_psLADSPARepository = (LADSPAPluginInfo **)malloc(sizeof(LADSPAPluginInfo *)
				* LADSPA_REPOSITORY_CAPACITY_STEP);
  g_lLADSPARepositoryCapacity = LADSPA_REPOSITORY_CAPACITY_STEP;
  g_lLADSPARepositoryCount = 0;

  pcLADSPAPath = ladspa_dir(ss);
  if (!pcLADSPAPath)
    {
      pcLADSPAPath = getenv("LADSPA_PATH");
      if (!pcLADSPAPath) 
	{
	  snd_warning("Warning: You have not set " S_ladspa_dir " or the environment variable LADSPA_PATH.\nUsing /usr/lib/ladspa instead."); 
	  pcLADSPAPath = "/usr/lib/ladspa"; 
	}
    }
  
  pcStart = pcLADSPAPath;
  while (*pcStart != '\0') 
    {
      char *pcBuffer;
      pcEnd = pcStart;
      while (*pcEnd != ':' && *pcEnd != '\0')
	pcEnd++;
      
      pcBuffer = (char *)malloc(1 + pcEnd - pcStart);
      if (pcEnd > pcStart)
	memcpy(pcBuffer, pcStart, pcEnd - pcStart);
      pcBuffer[pcEnd - pcStart] = '\0';
      
      loadLADSPADirectory(pcBuffer);
      
      pcStart = pcEnd;
      if (*pcStart == ':')
	pcStart++;
      
      free(pcBuffer);
    }
}


/*****************************************************************************/


#if USE_MOTIF || USE_GTK
static const char *ladspa_xrefs[6] = {
  "LADSPA overview: {LADSPA}",
  "info on specific plugin: {analyze-ladspa}",
  "apply plugin: {apply-ladspa}",
  "plugin directory: {" S_ladspa_dir "}",
  "GUI for plugins: see ladspa.scm",
  NULL};


#if USE_MOTIF
static void ladspa_help_callback(Widget w, XtPointer info, XtPointer context)
#endif
#if USE_GTK
static void ladspa_help_callback(GtkWidget *w, gpointer info)
#endif
{
  /* help dialog with currently loaded plugins (and descriptors), refs to list-ladspa etc */
  int len = 0;
  long lIndex;
  LADSPAPluginInfo *psInfo;
  const LADSPA_Descriptor *psDescriptor;
  char *pcFilename;
  for (lIndex = g_lLADSPARepositoryCount - 1; lIndex >= 0; lIndex--) 
    {
      psInfo = g_psLADSPARepository[lIndex];
      len += mus_strlen(psInfo->m_pcPackedFilename);
      len += 32;
      len += mus_strlen((char *)(psInfo->m_pcLabel));
      pcFilename = packLADSPAFilename(psInfo->m_pcPackedFilename);
      psDescriptor = findLADSPADescriptor(pcFilename, (char *)(psInfo->m_pcLabel));
      free(pcFilename);
      len += mus_strlen(psDescriptor->Name);
    }
  if (len > 0)
    {
      char *desc;
      desc = (char *)calloc(len, sizeof(char));
      for (lIndex = g_lLADSPARepositoryCount - 1; lIndex >= 0; lIndex--) 
	{
	  psInfo = g_psLADSPARepository[lIndex];
	  pcFilename = packLADSPAFilename(psInfo->m_pcPackedFilename);
	  psDescriptor = findLADSPADescriptor(pcFilename, (char *)(psInfo->m_pcLabel));
	  free(pcFilename);
	  strcat(desc, psInfo->m_pcPackedFilename);
	  strcat(desc, ":");
	  strcat(desc, (char *)(psInfo->m_pcLabel));
	  if (psDescriptor->Name)
	    {
	      int name_len;
	      name_len = strlen(psInfo->m_pcPackedFilename) + strlen((char *)(psInfo->m_pcLabel));
	      if (name_len < 20)
		{
		  strcat(desc, ",\t");
		  if (name_len < 9) strcat(desc, "\t");
		}
	      else strcat(desc, " ");
	      strcat(desc, psDescriptor->Name);
	    }
	  strcat(desc, "\n");
	}
      snd_help_with_xrefs("Available plugins", desc, WITHOUT_WORD_WRAP, ladspa_xrefs, NULL);
      free(desc);
    }
}
#endif


#define S_init_ladspa "init-ladspa"

static Xen g_init_ladspa(void) 
{
#define H_init_ladspa "(" S_init_ladspa "): reinitialise LADSPA. This is not \
normally necessary as LADSPA automatically initialises itself, however \
it can be useful when the plugins on the system have changed."

  if (g_bLADSPAInitialised)
    unloadLADSPA();

  loadLADSPA();
#if USE_MOTIF
  {
    Widget m, help_menu;
    Arg args[12];
    int n = 0;
    help_menu = menu_widget(4);
    XtSetArg(args[n], XmNbackground, ss->basic_color); n++;
    m = XtCreateManagedWidget("Plugins", xmPushButtonWidgetClass, help_menu, args, n);
    XtAddCallback(m, XmNactivateCallback, ladspa_help_callback, NULL);
  }
#endif
#if USE_GTK
  {
    GtkWidget *m, *help_menu;
    help_menu = get_help_menu_widget(); /* this is the cascade menu */
    m = gtk_menu_item_new_with_label("Plugins");
    gtk_menu_shell_append(GTK_MENU_SHELL(help_menu), m);
    gtk_widget_show(m);
    SG_SIGNAL_CONNECT(m, "activate", ladspa_help_callback, NULL);
  }
#endif
  return(Xen_false);
}


/*****************************************************************************/


#define S_list_ladspa "list-ladspa"

static Xen g_list_ladspa(void) 
{
#define H_list_ladspa "(" S_list_ladspa "): return a list of lists containing \
information of the LADSPA plugins currently available. For each plugin a \
list containing the plugin-file and plugin-label is included."

  long lIndex;
  Xen xenList;

  if (!g_bLADSPAInitialised)
    loadLADSPA();
  xenList = Xen_empty_list;

  for (lIndex = g_lLADSPARepositoryCount - 1; lIndex >= 0; lIndex--) 
    {
      Xen xenPluginList;
      LADSPAPluginInfo *psInfo;
      psInfo = g_psLADSPARepository[lIndex];
      xenPluginList = Xen_cons(C_string_to_Xen_string(psInfo->m_pcPackedFilename),
			       Xen_cons(C_string_to_Xen_string((char *)psInfo->m_pcLabel),
					Xen_empty_list));
      xenList = Xen_cons(xenPluginList, xenList);
    }
  return xenList;
}


/*****************************************************************************/


#define S_analyse_ladspa "analyse-ladspa"

static Xen g_analyse_ladspa(Xen ladspa_plugin_filename,
			    Xen ladspa_plugin_label) 
{
#define H_analyse_ladspa "(" S_analyse_ladspa " library plugin): return a list of information about \
a LADSPA plugin. The plugin is identified by library and plugin. \
The items are: plugin-name, plugin-maker, \
plugin-copyright, plugin-parameter-list. The plugin-port-list contains a \
list of information for each parameter available. The first item in this \
list is the name of the port. Other hint information may follow this to help \
a user interface edit the parameter in a useful way."

  long lIndex;
  int inchans, outchans;
  const LADSPA_Descriptor *psDescriptor;
  char *pcFilename;
  const char *pcLabel, *pcTmp;
  Xen xenList, xenPortData;
  LADSPA_PortRangeHintDescriptor iHint;

  if (!g_bLADSPAInitialised)
    loadLADSPA();

  Xen_check_type(Xen_is_string(ladspa_plugin_filename),
                  ladspa_plugin_filename,
	          1,
	          S_analyse_ladspa, "a string");
  Xen_check_type(Xen_is_string(ladspa_plugin_label),
	          ladspa_plugin_label,
	          2,
	          S_analyse_ladspa, "a string");

  /* Plugin. */
  pcTmp = Xen_string_to_C_string(ladspa_plugin_filename);
  pcLabel = Xen_string_to_C_string(ladspa_plugin_label);
  pcFilename = packLADSPAFilename(pcTmp);
  psDescriptor = findLADSPADescriptor(pcFilename, pcLabel);
  free(pcFilename);

  if (!psDescriptor) 
    {
      Xen_error(Xen_make_error_type("no-such-plugin"),
		Xen_list_3(C_string_to_Xen_string(S_analyse_ladspa ": no such plugin file: ~A, plugin label: ~A"),
			   ladspa_plugin_filename,
			   ladspa_plugin_label));
      return(Xen_false);
    }

  isLADSPAPluginSupported(psDescriptor);
  inchans = lInputCount;
  outchans = lOutputCount;

  xenList = Xen_empty_list;
  for (lIndex = psDescriptor->PortCount - 1; lIndex >= 0; lIndex--)
    if (LADSPA_IS_PORT_CONTROL(psDescriptor->PortDescriptors[lIndex])
	&& LADSPA_IS_PORT_INPUT(psDescriptor->PortDescriptors[lIndex])) 
      {
	iHint = psDescriptor->PortRangeHints[lIndex].HintDescriptor;
	
	xenPortData = Xen_empty_list;
	if (LADSPA_IS_HINT_TOGGLED(iHint))
	  xenPortData = Xen_cons(C_string_to_Xen_string("toggle"), xenPortData);
	if (LADSPA_IS_HINT_LOGARITHMIC(iHint))
	  xenPortData = Xen_cons(C_string_to_Xen_string("logarithmic"), xenPortData);
	if (LADSPA_IS_HINT_INTEGER(iHint))
	  xenPortData = Xen_cons(C_string_to_Xen_string("integer"), xenPortData);
	if (LADSPA_IS_HINT_SAMPLE_RATE(iHint))
	  xenPortData = Xen_cons(C_string_to_Xen_string("sample_rate"), xenPortData);
	if (LADSPA_IS_HINT_BOUNDED_ABOVE(iHint))
	  xenPortData = Xen_cons(C_string_to_Xen_string("maximum"),
				 Xen_cons(C_double_to_Xen_real(psDescriptor->PortRangeHints[lIndex].UpperBound),
					  xenPortData));
	if (LADSPA_IS_HINT_BOUNDED_BELOW(iHint))
	  xenPortData = Xen_cons(C_string_to_Xen_string("minimum"),
				 Xen_cons(C_double_to_Xen_real(psDescriptor->PortRangeHints[lIndex].LowerBound),
					  xenPortData));
	xenPortData = Xen_cons(C_string_to_Xen_string((char *)psDescriptor->PortNames[lIndex]),
			       xenPortData);
	xenList = Xen_cons(xenPortData, xenList);
      }

  xenList = Xen_cons(C_string_to_Xen_string((char *)psDescriptor->Name),
	     Xen_cons(C_string_to_Xen_string((char *)psDescriptor->Maker),
	      Xen_cons(C_string_to_Xen_string((char *)psDescriptor->Copyright),
	       Xen_cons(Xen_list_4(C_string_to_Xen_string("inputs:"),
				   C_int_to_Xen_integer(inchans),
				   C_string_to_Xen_string("outputs:"),
				   C_int_to_Xen_integer(outchans)),
		Xen_cons(xenList, Xen_empty_list)))));
  return(xenList);
}


/*****************************************************************************/


#define S_apply_ladspa "apply-ladspa"

static Xen g_apply_ladspa(Xen reader,
			  Xen ladspa_plugin_configuration,
			  Xen samples,
			  Xen origin,
			  Xen snd, Xen chn) /* these two args added (much) later */
{
#define H_apply_ladspa "(" S_apply_ladspa " reader (list library plugin pars) dur origin :optional snd chn): apply a LADSPA plugin to process a \
sound. The parameters are soundfile-reader, a ladspa-plugin-configuration, \
the number of samples to process, and an `origin' for edit lists. The \
ladspa-plugin-configuration is a list containing the plugin-file and \
plugin-label for the LADSPA plugin, as provided by " S_list_ladspa ", followed \
by any arguments. The reader argument can also be a list of readers. \
Information about parameters can be acquired using " S_analyse_ladspa "."

  const LADSPA_Descriptor *psDescriptor;
  char *pcFilename;
  const char *pcLabel, *pcTmp;
  LADSPA_Handle *psHandle;
  unsigned long lSampleRate, lPortIndex, lSampleIndex;
  mus_long_t lAt;
  unsigned long lParameterCount;
  Xen xenParameters;
  LADSPA_Data *pfControls = NULL;
  chan_info *cp;
  snd_info *sp;
  char *ofile, *msg;
  int i, ofd, datumb, inchans = 1, readers = 0, outchans = 1;
  mus_long_t num;
  snd_fd **sf = NULL;
  file_info *hdr;
  mus_float_t **data;
  LADSPA_Data **pfInputBuffer = NULL;
  LADSPA_Data **pfOutputBuffer = NULL;
  io_error_t io_err = IO_NO_ERROR;
  snd_fd *tmp_fd;
  bool cp_from_reader = false;

  if (!g_bLADSPAInitialised)
    loadLADSPA();

  /* First parameter should be a file reader or list thereof. */
  Xen_check_type(is_sampler(reader) || Xen_is_list(reader) || Xen_is_false(reader),
		  reader,
		  1,
		  S_apply_ladspa, "a sampler, a list of readers, or " PROC_FALSE);
  if (Xen_is_list(reader)) 
    readers = Xen_list_length(reader);
  else
    {
      if (!(Xen_is_false(reader)))
	readers = 1;
    }

  /* Second parameter should be a list of two strings, then any number
     (inc 0) of numbers. */
  if ((Xen_list_length(ladspa_plugin_configuration) < 2) ||
      (!(Xen_is_string(Xen_car(ladspa_plugin_configuration)))) ||
      (!(Xen_is_string(Xen_cadr(ladspa_plugin_configuration)))))
    Xen_check_type(false, ladspa_plugin_configuration, 2, S_apply_ladspa, "a list of 2 or more strings");

  /* Third parameter is the number of samples to process. */
  Xen_check_type(Xen_is_number(samples),
		  samples,
		  3,
		  S_apply_ladspa, "a number");
  /* Get sample count. */
  num = Xen_llong_to_C_llong(samples);
  if (num <= 0) return(Xen_false);

  /* The fourth parameter is a tag to identify the edit. */
  Xen_check_type(Xen_is_string(origin),
		  origin,
		  4,
		  S_apply_ladspa, "a string");

  Snd_assert_channel(S_apply_ladspa, snd, chn, 5);

  /* Plugin. */
  pcTmp = Xen_string_to_C_string(Xen_car(ladspa_plugin_configuration));
  pcLabel = Xen_string_to_C_string(Xen_cadr(ladspa_plugin_configuration));
  pcFilename = packLADSPAFilename(pcTmp);
  psDescriptor = findLADSPADescriptor(pcFilename, pcLabel);
  free(pcFilename);

  if (!psDescriptor)
    Xen_error(Xen_make_error_type("no-such-plugin"),
	      Xen_list_2(C_string_to_Xen_string(S_apply_ladspa ": no such plugin: ~A"),
			 ladspa_plugin_configuration));

  isLADSPAPluginSupported(psDescriptor);
  inchans = lInputCount;
  outchans = lOutputCount;
  if (outchans == 0)
    Xen_error(Xen_make_error_type("plugin-error"),
	      Xen_list_2(C_string_to_Xen_string(S_apply_ladspa ": plugins must have at least 1 output, ~A"),
			 ladspa_plugin_configuration));

  if (inchans != readers)
    {
      Xen errmsg;

      msg = mus_format("Ladspa %s required inputs (%d) != samplers (%d)", pcLabel, inchans, readers);
      errmsg = C_string_to_Xen_string(msg);
      free(msg);
      Xen_error(Xen_make_error_type("plugin-error"),
		Xen_list_3(C_string_to_Xen_string(S_apply_ladspa ": ~A, ~A"),
			   ladspa_plugin_configuration,
			   errmsg));
    }

  lParameterCount = 0;
  for (lPortIndex = 0; lPortIndex < psDescriptor->PortCount; lPortIndex++)
    if (LADSPA_IS_PORT_CONTROL(psDescriptor->PortDescriptors[lPortIndex])
	&& LADSPA_IS_PORT_INPUT(psDescriptor->PortDescriptors[lPortIndex]))
      lParameterCount++;
  msg = mus_format("a list of 2 strings + %d parameters", (int)lParameterCount);
  Xen_check_type(Xen_list_length(ladspa_plugin_configuration) == (int)(2 + lParameterCount),
		  ladspa_plugin_configuration,
		  2,
		  S_apply_ladspa, 
		  msg);
  free(msg);
  pfControls = (LADSPA_Data *)malloc(psDescriptor->PortCount * sizeof(LADSPA_Data));

  if (Xen_is_bound(snd))
    cp = get_cp(snd, chn, S_apply_ladspa);
  else
    {
      if (inchans > 0)
	{
	  if (Xen_is_list(reader))
	    tmp_fd = xen_to_sampler(Xen_list_ref(reader, 0));
	  else tmp_fd = xen_to_sampler(reader);
	  cp = tmp_fd->cp;
	  cp_from_reader = true;
	}
      else cp = selected_channel();
    }
  sp = cp->sound;

  /* Get parameters. */
  xenParameters = Xen_copy_arg(Xen_cdr(Xen_cdr(ladspa_plugin_configuration)));
  for (lPortIndex = 0; lPortIndex < psDescriptor->PortCount; lPortIndex++) 
    {
      LADSPA_PortDescriptor iPortDescriptor;
      iPortDescriptor = psDescriptor->PortDescriptors[lPortIndex];
      if (LADSPA_IS_PORT_CONTROL(iPortDescriptor)
	  && LADSPA_IS_PORT_INPUT(iPortDescriptor)) 
	{
	  Xen_check_type(Xen_is_number(Xen_car(xenParameters)),
			 ladspa_plugin_configuration,
			 2,
			 S_apply_ladspa, "a number");
	  pfControls[lPortIndex] = (LADSPA_Data)Xen_real_to_C_double(Xen_car(xenParameters));
	  xenParameters = Xen_cdr(xenParameters);
	}
    }
  
  lSampleRate = (unsigned long)(sp->hdr->srate);
  psHandle = (LADSPA_Handle *)psDescriptor->instantiate(psDescriptor, lSampleRate);
  if (!psHandle)
    Xen_error(Xen_make_error_type("plugin-error"),
	      Xen_list_2(C_string_to_Xen_string(S_apply_ladspa ": ~A plugin did not instantiate"),
			 ladspa_plugin_configuration));

  /* Temporary file name. */
  ofile = snd_tempnam();

  /* Create initial header for output file */
  hdr = make_temp_header(ofile,
			 snd_srate(sp),
			 outchans,
			 num,
			 Xen_string_to_C_string(origin));

  /* Open the output file, using the header we've been working on. */
  ofd = open_temp_file(ofile, outchans, hdr, &io_err);
  if (io_err != IO_NO_ERROR)
    {
      if (ofd == -1)
	{
	  free_file_info(hdr);
	  if (pfControls) free(pfControls);
	  psDescriptor->cleanup(psHandle);
	  Xen_error(Xen_make_error_type("cannot-save"),
		    Xen_list_3(C_string_to_Xen_string(S_apply_ladspa ": can't save ~S, ~A"),
			       C_string_to_Xen_string(ofile),
			       C_string_to_Xen_string(snd_io_strerror())));
	  return(Xen_false);
	}
      snd_warning("%s %s: %s", S_apply_ladspa, ofile, io_error_name(io_err));
    }
  /* Tidy up header. */
  datumb = mus_bytes_per_sample(hdr->sample_type);

  if (readers > 0)
    {
      sf = (snd_fd **)calloc(readers, sizeof(snd_fd *));

      /* Local version of sound descriptor. */
      if (Xen_is_list(reader))
	{
	  for (i = 0; i < readers; i++)
	    if (!(Xen_is_false(Xen_list_ref(reader, i))))
	      sf[i] = xen_to_sampler(Xen_list_ref(reader, i));
	}
      else sf[0] = xen_to_sampler(reader);
    }
  /* this code added 20-Sep-01 */
  if (inchans > 0)
    {
      pfInputBuffer = (LADSPA_Data **)calloc(inchans, sizeof(LADSPA_Data *));
      for (i = 0; i < inchans; i++)
	pfInputBuffer[i] = (LADSPA_Data *)calloc(MAX_BUFFER_SIZE, sizeof(LADSPA_Data));
    }
  pfOutputBuffer = (LADSPA_Data **)calloc(outchans, sizeof(LADSPA_Data *));
  for (i = 0; i < outchans; i++)
    pfOutputBuffer[i] = (LADSPA_Data *)calloc(MAX_BUFFER_SIZE, sizeof(LADSPA_Data));

  data = (mus_float_t **)calloc(outchans, sizeof(mus_float_t *));
  for (i = 0; i < outchans; i++)
    data[i] = (mus_float_t *)calloc(MAX_BUFFER_SIZE, sizeof(mus_float_t));

  /* Connect input and output control ports. */
  {
    int inc = 0, outc = 0;
    for (lPortIndex = 0; lPortIndex < psDescriptor->PortCount; lPortIndex++) 
      {
	if (LADSPA_IS_PORT_CONTROL(psDescriptor->PortDescriptors[lPortIndex])) 
	  {
	    psDescriptor->connect_port(psHandle,
				       lPortIndex,
				       pfControls + lPortIndex);
	    /* (Output control data is quietly lost.) */
	  }
	else /* AUDIO */ {
	  if (LADSPA_IS_PORT_INPUT(psDescriptor->PortDescriptors[lPortIndex]))
	    psDescriptor->connect_port(psHandle,
				       lPortIndex,
				       pfInputBuffer[inc++]);
	  else
	    psDescriptor->connect_port(psHandle,
				       lPortIndex,
				       pfOutputBuffer[outc++]);
	}
      }
  }

  if (psDescriptor->activate)
    psDescriptor->activate(psHandle);

  lAt = 0;
  ss->stopped_explicitly = false;
  while (lAt < num) 
    {
      int err;
      unsigned long lBlockSize;
      /* Decide how much audio to process this frame. */
      lBlockSize = num - lAt;
      if (lBlockSize > MAX_BUFFER_SIZE)
	lBlockSize = MAX_BUFFER_SIZE;

      /* Prepare the input data. */
      if (readers > 0)
	for (i = 0; i < readers; i++)
	  {
	    if (sf[i])
	      {
		for (lSampleIndex = 0; lSampleIndex < lBlockSize; lSampleIndex++) 
		  pfInputBuffer[i][lSampleIndex] = read_sample(sf[i]);
	      }
	    else
	      {
		for (lSampleIndex = 0; lSampleIndex < lBlockSize; lSampleIndex++) 
		  pfInputBuffer[i][lSampleIndex] = 0.0;
	      }
	  }

      /* Run the plugin. */
      psDescriptor->run(psHandle, lBlockSize);

      /* Prepare the output data. */
      for (i = 0; i < outchans; i++)
	for (lSampleIndex = 0; lSampleIndex < lBlockSize; lSampleIndex++)
	  data[i][lSampleIndex] = (pfOutputBuffer[i][lSampleIndex]);

      /* Send the output data to the outside world. */
      err = mus_file_write(ofd,
			   0,
			   lBlockSize - 1,
			   outchans,
			   data);
      if (err != MUS_NO_ERROR)
	break;
      if (ss->stopped_explicitly)
	break;

      lAt += lBlockSize;
    }

  if (psDescriptor->deactivate)
    psDescriptor->deactivate(psHandle);

  psDescriptor->cleanup(psHandle);

  close_temp_file(ofile, ofd,
		  hdr->type,
		  num * datumb * outchans);

  /* Discard tmp header. */
  hdr = free_file_info(hdr);
  if (!(ss->stopped_explicitly))
    {
      int j;
      if (outchans > 1)
	remember_temp(ofile, outchans);
      for (i = 0, j = 0; i < outchans; i++)
	{
	  chan_info *ncp;
	  mus_long_t beg;
	  if ((cp_from_reader) && (sf) && (sf[j]))
	    {
	      ncp = sf[j]->cp;
	      beg = sf[j]->initial_samp;
	    }
	  else 
	    {
	      beg = 0;
	      if (i < (int)(sp->nchans))
		ncp = sp->chans[i];
	      else break;
	    }
	  if (file_change_samples(beg,
				  num,
				  ofile,
				  ncp,
				  i,
				  (outchans > 1) ? MULTICHANNEL_DELETION : DELETE_ME,
				  Xen_string_to_C_string(origin),
				  ncp->edit_ctr))
	    update_graph(ncp);
	  j++;
	  if (j >= inchans) j = 0;
	}
    }
  else 
    {
      status_report(sp, S_apply_ladspa " interrupted");
      ss->stopped_explicitly = false;
    }
  if (ofile) free(ofile);
  if (inchans > 0)
    {
      for (i = 0; i < inchans; i++)
	free(pfInputBuffer[i]);
      free(pfInputBuffer);
    }
  /* sf[i] is directly from scheme, so it will presumably handle reader gc */
  for (i = 0; i < outchans; i++)
    {
      free(pfOutputBuffer[i]);
      free(data[i]);
    }
  free(pfOutputBuffer);
  if (sf) free(sf);
  if (pfControls) free(pfControls);
  free(data);
  return(Xen_false);
}


#if HAVE_EXTENSION_LANGUAGE
#if HAVE_SCHEME
  #define PREFIX "."
#endif
#if HAVE_RUBY
  #define PREFIX "R"
#endif
#if HAVE_FORTH
  #define PREFIX "F"
#endif

#define C_to_Xen_Ladspa_Descriptor(Value) \
  ((Value) ? Xen_list_2(C_string_to_Xen_symbol("Ladspa-Descriptor"), Xen_wrap_C_pointer(Value)) : Xen_false)
#define Xen_to_C_Ladspa_Descriptor(Value) ((LADSPA_Descriptor *)(Xen_unwrap_C_pointer(Xen_cadr(Value))))
#define Xen_is_Ladspa_Descriptor(Value) (Xen_is_list(Value) && (Xen_list_length(Value) >= 2) && (Xen_is_symbol(Xen_car(Value))) && \
					 (strcmp("Ladspa-Descriptor", Xen_symbol_to_C_string(Xen_car(Value))) == 0))
  
#define C_to_Xen_Ladspa_Handle(Value) \
  ((Value) ? Xen_list_2(C_string_to_Xen_symbol("Ladspa-Handle"), Xen_wrap_C_pointer(Value)) : Xen_false)
#define Xen_to_C_Ladspa_Handle(Value) ((LADSPA_Handle *)(Xen_unwrap_C_pointer(Xen_cadr(Value))))
#define Xen_is_Ladspa_Handle(Value) (Xen_is_list(Value) && (Xen_list_length(Value) >= 2) && (Xen_is_symbol(Xen_car(Value))) && \
				     (strcmp("Ladspa-Handle", Xen_symbol_to_C_string(Xen_car(Value))) == 0))
  

#define S_ladspa_descriptor "ladspa-descriptor"

static Xen g_ladspa_descriptor(Xen ladspa_plugin_filename, Xen ladspa_plugin_label)
{
  #define H_ladspa_descriptor "(" S_ladspa_descriptor " library plugin): return the descriptor \
associated with the given plugin."
  const LADSPA_Descriptor *psDescriptor;
  char *pcFilename;
  const char *pcLabel, *pcTmp;
  if (!g_bLADSPAInitialised) loadLADSPA();
  Xen_check_type(Xen_is_string(ladspa_plugin_filename), ladspa_plugin_filename, 1, S_ladspa_descriptor, "a string");
  Xen_check_type(Xen_is_string(ladspa_plugin_label), ladspa_plugin_label, 2, S_ladspa_descriptor, "a string");
  pcTmp = Xen_string_to_C_string(ladspa_plugin_filename);
  pcLabel = Xen_string_to_C_string(ladspa_plugin_label);
  pcFilename = packLADSPAFilename(pcTmp);
  psDescriptor = findLADSPADescriptor(pcFilename, pcLabel);
  free(pcFilename);
  if (!psDescriptor) return(Xen_false);
  return(C_to_Xen_Ladspa_Descriptor(psDescriptor));
}


static Xen g_ladspa_Label(Xen ptr)
{
  #define H_ladspa_Label "(" PREFIX "Label descriptor): plugin identifier"
  Xen_check_type(Xen_is_Ladspa_Descriptor(ptr), ptr, 1, PREFIX "Label", "Ladspa descriptor");
  return(C_string_to_Xen_string((Xen_to_C_Ladspa_Descriptor(ptr))->Label));
}


static Xen g_ladspa_Name(Xen ptr)
{
  #define H_ladspa_Name "(" PREFIX "Name descriptor): name of plugin"
  Xen_check_type(Xen_is_Ladspa_Descriptor(ptr), ptr, 1, PREFIX "Name", "Ladspa descriptor");
  return(C_string_to_Xen_string((Xen_to_C_Ladspa_Descriptor(ptr))->Name));
}


static Xen g_ladspa_Copyright(Xen ptr)
{
  #define H_ladspa_Copyright "(" PREFIX "Copyright descriptor): plugin copyright or 'None'"
  Xen_check_type(Xen_is_Ladspa_Descriptor(ptr), ptr, 1, PREFIX "Copyright", "Ladspa descriptor");
  return(C_string_to_Xen_string((Xen_to_C_Ladspa_Descriptor(ptr))->Copyright));
}


static Xen g_ladspa_Maker(Xen ptr)
{
  #define H_ladspa_Maker "(" PREFIX "Maker descriptor): plugin developer"
  Xen_check_type(Xen_is_Ladspa_Descriptor(ptr), ptr, 1, PREFIX "Maker", "Ladspa descriptor");
  return(C_string_to_Xen_string((Xen_to_C_Ladspa_Descriptor(ptr))->Maker));
}


static Xen g_ladspa_Properties(Xen ptr)
{
  #define H_ladspa_Properties "(" PREFIX "Properties descriptor): plugin properties"
  Xen_check_type(Xen_is_Ladspa_Descriptor(ptr), ptr, 1, PREFIX "Properties", "Ladspa descriptor");
  return(C_int_to_Xen_integer((Xen_to_C_Ladspa_Descriptor(ptr))->Properties));
}


static Xen g_ladspa_UniqueID(Xen ptr)
{
  #define H_ladspa_UniqueID "(" PREFIX "UniqueID descriptor): plugin ID number"
  Xen_check_type(Xen_is_Ladspa_Descriptor(ptr), ptr, 1, PREFIX "UniqueID", "Ladspa descriptor");
  return(C_int_to_Xen_integer((Xen_to_C_Ladspa_Descriptor(ptr))->UniqueID));
}


static Xen g_ladspa_PortCount(Xen ptr)
{
  #define H_ladspa_PortCount "(" PREFIX "PortCount descriptor): plugin input and output port count"
  Xen_check_type(Xen_is_Ladspa_Descriptor(ptr), ptr, 1, PREFIX "PortCount", "Ladspa descriptor");
  return(C_int_to_Xen_integer((Xen_to_C_Ladspa_Descriptor(ptr))->PortCount));
}


static Xen g_ladspa_PortDescriptors(Xen ptr)
{
#define H_ladspa_PortDescriptors "(" PREFIX "PortDescriptors descriptor): plugin port descriptors (an array of ints)"
  LADSPA_Descriptor *descriptor;
  int i, len;
  Xen lst = Xen_empty_list;
  Xen_check_type(Xen_is_Ladspa_Descriptor(ptr), ptr, 1, PREFIX "PortDescriptors", "Ladspa descriptor");
  descriptor = Xen_to_C_Ladspa_Descriptor(ptr);
  len = descriptor->PortCount;
  for (i = len - 1; i >= 0; i--)
    lst = Xen_cons(C_int_to_Xen_integer(descriptor->PortDescriptors[i]), lst);
  return(lst);
}


static Xen g_ladspa_PortRangeHints(Xen ptr)
{
  #define H_ladspa_PortRangeHints "(" PREFIX "PortRangeHints descriptor): plugin port hints"
  LADSPA_Descriptor *descriptor;
  int i, len;
  Xen lst = Xen_empty_list;
  Xen_check_type(Xen_is_Ladspa_Descriptor(ptr), ptr, 1, PREFIX "PortRangeHints", "Ladspa descriptor");
  descriptor = Xen_to_C_Ladspa_Descriptor(ptr);
  len = descriptor->PortCount;
  for (i = len - 1; i >= 0; i--)
    lst = Xen_cons(Xen_list_3(C_int_to_Xen_integer(descriptor->PortRangeHints[i].HintDescriptor),
			      C_double_to_Xen_real(descriptor->PortRangeHints[i].LowerBound),
			      C_double_to_Xen_real(descriptor->PortRangeHints[i].UpperBound)),
		   lst);
  return(lst);
}


static Xen g_ladspa_PortNames(Xen ptr)
{
  #define H_ladspa_PortNames "(" PREFIX "PortNames descriptor): plugin descriptive port names"
  LADSPA_Descriptor *descriptor;
  int i, len;
  Xen lst = Xen_empty_list;
  Xen_check_type(Xen_is_Ladspa_Descriptor(ptr), ptr, 1, PREFIX "PortNames", "Ladspa descriptor");
  descriptor = Xen_to_C_Ladspa_Descriptor(ptr);
  len = descriptor->PortCount;
  for (i = len - 1; i >= 0; i--)
    lst = Xen_cons(C_string_to_Xen_string(descriptor->PortNames[i]), lst);
  return(lst);
}



#define S_ladspa_instantiate "ladspa-instantiate"

static Xen g_ladspa_instantiate(Xen ptr, Xen srate)
{
  #define H_ladspa_instantiate "(" S_ladspa_instantiate " descriptor srate): run plugin's instantiate function, return handle"
  const LADSPA_Descriptor *descriptor;
  LADSPA_Handle handle;
  Xen_check_type(Xen_is_Ladspa_Descriptor(ptr), ptr, 1, S_ladspa_instantiate, "Ladspa descriptor");
  Xen_check_type(Xen_is_ulong(srate), srate, 2, S_ladspa_instantiate, "int");
  descriptor = Xen_to_C_Ladspa_Descriptor(ptr);
  handle = descriptor->instantiate(descriptor, Xen_ulong_to_C_ulong(srate));
  return(C_to_Xen_Ladspa_Handle(handle));
}


#define S_ladspa_activate "ladspa-activate"

static Xen g_ladspa_activate(Xen desc, Xen ptr)
{
  #define H_ladspa_activate "(" S_ladspa_activate " descriptor handle): run plugin's activate function"
  const LADSPA_Descriptor *descriptor;
  Xen_check_type(Xen_is_Ladspa_Descriptor(desc), desc, 1, S_ladspa_activate, "Ladspa descriptor");
  Xen_check_type(Xen_is_Ladspa_Handle(ptr), ptr, 2, S_ladspa_activate, "Ladspa handle");
  descriptor = Xen_to_C_Ladspa_Descriptor(desc);
  if (descriptor->activate) descriptor->activate(Xen_to_C_Ladspa_Handle(ptr));
  return(Xen_false);
}


#define S_ladspa_deactivate "ladspa-deactivate"

static Xen g_ladspa_deactivate(Xen desc, Xen ptr)
{
  #define H_ladspa_deactivate "(" S_ladspa_deactivate " descriptor handle): run plugin's deactivate function"
  const LADSPA_Descriptor *descriptor;
  Xen_check_type(Xen_is_Ladspa_Descriptor(desc), desc, 1, S_ladspa_deactivate, "Ladspa descriptor");
  Xen_check_type(Xen_is_Ladspa_Handle(ptr), ptr, 2, S_ladspa_deactivate, "Ladspa handle");
  descriptor = Xen_to_C_Ladspa_Descriptor(desc);
  if (descriptor->deactivate) descriptor->deactivate(Xen_to_C_Ladspa_Handle(ptr));
  return(Xen_false);
}


#define S_ladspa_cleanup "ladspa-cleanup"

static Xen g_ladspa_cleanup(Xen desc, Xen ptr)
{
  #define H_ladspa_cleanup "(" S_ladspa_cleanup " descriptor handle): run plugin's cleanup function"
  const LADSPA_Descriptor *descriptor;
  Xen_check_type(Xen_is_Ladspa_Descriptor(desc), desc, 1, S_ladspa_cleanup, "Ladspa descriptor");
  Xen_check_type(Xen_is_Ladspa_Handle(ptr), ptr, 2, S_ladspa_cleanup, "Ladspa handle");
  descriptor = Xen_to_C_Ladspa_Descriptor(desc);
  if (descriptor->cleanup) descriptor->cleanup(Xen_to_C_Ladspa_Handle(ptr));
  return(Xen_false);
}


#define S_ladspa_run "ladspa-run"

static Xen g_ladspa_run(Xen desc, Xen ptr, Xen count)
{
  #define H_ladspa_run "(" S_ladspa_run " descriptor handle count): run plugin's run function"
  const LADSPA_Descriptor *descriptor;
  Xen_check_type(Xen_is_Ladspa_Descriptor(desc), desc, 1, S_ladspa_run, "Ladspa descriptor");
  Xen_check_type(Xen_is_Ladspa_Handle(ptr), ptr, 2, S_ladspa_run, "Ladspa handle");
  Xen_check_type(Xen_is_ulong(count), count, 3, S_ladspa_run, "unsigned long");
  descriptor = Xen_to_C_Ladspa_Descriptor(desc);
  if (descriptor->run) descriptor->run(Xen_to_C_Ladspa_Handle(ptr), Xen_ulong_to_C_ulong(count));
  return(Xen_false);
}


#define S_ladspa_run_adding "ladspa-run-adding"

static Xen g_ladspa_run_adding(Xen desc, Xen ptr, Xen count)
{
  #define H_ladspa_run_adding "(" S_ladspa_run_adding " descriptor handle count): run plugin's run_adding function"
  const LADSPA_Descriptor *descriptor;
  Xen_check_type(Xen_is_Ladspa_Descriptor(desc), desc, 1, S_ladspa_run_adding, "Ladspa descriptor");
  Xen_check_type(Xen_is_Ladspa_Handle(ptr), ptr, 2, S_ladspa_run_adding, "Ladspa handle");
  Xen_check_type(Xen_is_ulong(count), count, 3, S_ladspa_run_adding, "unsigned long");
  descriptor = Xen_to_C_Ladspa_Descriptor(desc);
  if (descriptor->run_adding) descriptor->run_adding(Xen_to_C_Ladspa_Handle(ptr), Xen_ulong_to_C_ulong(count));
  return(Xen_false);
}


#define S_ladspa_set_run_adding_gain "ladspa-set-run-adding-gain"

static Xen g_ladspa_set_run_adding_gain(Xen desc, Xen ptr, Xen gain)
{
  #define H_ladspa_set_run_adding_gain "(" S_ladspa_set_run_adding_gain " descriptor handle gain): run plugin's set_run_adding_gain function"
  const LADSPA_Descriptor *descriptor;
  Xen_check_type(Xen_is_Ladspa_Descriptor(desc), desc, 1, S_ladspa_set_run_adding_gain, "Ladspa descriptor");
  Xen_check_type(Xen_is_Ladspa_Handle(ptr), ptr, 2, S_ladspa_set_run_adding_gain, "Ladspa handle");
  Xen_check_type(Xen_is_double(gain), gain, 3, S_ladspa_set_run_adding_gain, "float");
  descriptor = Xen_to_C_Ladspa_Descriptor(desc);
  if (descriptor->set_run_adding_gain) descriptor->set_run_adding_gain(Xen_to_C_Ladspa_Handle(ptr), (LADSPA_Data)(Xen_real_to_C_double(gain)));
  return(Xen_false);
}

static float *double_to_float(mus_float_t *data, int data_size)
{
  float *ldata;
  int i;
  ldata = (float *)calloc(data_size, sizeof(float));
  for (i = 0; i < data_size; i++)
    ldata[i] = (float)(data[i]);
  return(ldata);
}


#define S_ladspa_connect_port "ladspa-connect-port"

static Xen g_ladspa_connect_port(Xen desc, Xen ptr, Xen port, Xen data)
{
  #define H_ladspa_connect_port "(" S_ladspa_connect_port " descriptor handle port data): run plugin's connect_port function"
  const LADSPA_Descriptor *descriptor;
  vct *samples;
  Xen_check_type(Xen_is_Ladspa_Descriptor(desc), desc, 1, S_ladspa_connect_port, "Ladspa descriptor");
  Xen_check_type(Xen_is_Ladspa_Handle(ptr), ptr, 2, S_ladspa_connect_port, "Ladspa handle");
  Xen_check_type(Xen_is_ulong(port), port, 3, S_ladspa_connect_port, "unsigned long");
  Xen_check_type(mus_is_vct(data), data, 4, S_ladspa_connect_port, S_vct);
  descriptor = Xen_to_C_Ladspa_Descriptor(desc);
  samples = Xen_to_vct(data);
  if (descriptor->connect_port) 
    descriptor->connect_port(Xen_to_C_Ladspa_Handle(ptr),
			     Xen_ulong_to_C_ulong(port),
			     double_to_float(mus_vct_data(samples), mus_vct_length(samples))
			     );
  return(Xen_false);
}



Xen_wrap_2_args(g_analyse_ladspa_w, g_analyse_ladspa)
Xen_wrap_2_args(g_ladspa_descriptor_w, g_ladspa_descriptor)
Xen_wrap_6_optional_args(g_apply_ladspa_w, g_apply_ladspa)
Xen_wrap_no_args(g_init_ladspa_w, g_init_ladspa)
Xen_wrap_no_args(g_list_ladspa_w, g_list_ladspa)
Xen_wrap_1_arg(g_ladspa_Label_w, g_ladspa_Label)
Xen_wrap_1_arg(g_ladspa_Name_w, g_ladspa_Name)
Xen_wrap_1_arg(g_ladspa_Copyright_w, g_ladspa_Copyright)
Xen_wrap_1_arg(g_ladspa_Maker_w, g_ladspa_Maker)
Xen_wrap_1_arg(g_ladspa_Properties_w, g_ladspa_Properties)
Xen_wrap_1_arg(g_ladspa_UniqueID_w, g_ladspa_UniqueID)
Xen_wrap_1_arg(g_ladspa_PortNames_w, g_ladspa_PortNames)
Xen_wrap_1_arg(g_ladspa_PortDescriptors_w, g_ladspa_PortDescriptors)
Xen_wrap_1_arg(g_ladspa_PortRangeHints_w, g_ladspa_PortRangeHints)
Xen_wrap_1_arg(g_ladspa_PortCount_w, g_ladspa_PortCount)
Xen_wrap_2_args(g_ladspa_instantiate_w, g_ladspa_instantiate)
Xen_wrap_2_args(g_ladspa_activate_w, g_ladspa_activate)
Xen_wrap_2_args(g_ladspa_deactivate_w, g_ladspa_deactivate)
Xen_wrap_2_args(g_ladspa_cleanup_w, g_ladspa_cleanup)
Xen_wrap_3_args(g_ladspa_run_w, g_ladspa_run)
Xen_wrap_3_args(g_ladspa_run_adding_w, g_ladspa_run_adding)
Xen_wrap_3_args(g_ladspa_set_run_adding_gain_w, g_ladspa_set_run_adding_gain)
Xen_wrap_4_args(g_ladspa_connect_port_w, g_ladspa_connect_port)

#if HAVE_SCHEME
  #define define_integer(Name) s7_define_constant(s7, #Name, C_int_to_Xen_integer(Name))
#else
  #define define_integer(Name) Xen_define(#Name, C_int_to_Xen_integer(Name))
#endif

void g_ladspa_to_snd(void)
{
#if HAVE_SCHEME
  s7_pointer i, b, p, t, fv, r, s, l;
  i = s7_make_symbol(s7, "integer?");
  b = s7_make_symbol(s7, "boolean?");
  p = s7_make_symbol(s7, "pair?");
  fv = s7_make_symbol(s7, "float-vector?");
  r = s7_make_symbol(s7, "real?");
  s = s7_make_symbol(s7, "string?");
  l = s7_make_symbol(s7, "list?");
  t = s7_t(s7);
#endif

  define_integer(LADSPA_PROPERTY_REALTIME);
  define_integer(LADSPA_PROPERTY_INPLACE_BROKEN);
  define_integer(LADSPA_PROPERTY_HARD_RT_CAPABLE);

  define_integer(LADSPA_PORT_INPUT);
  define_integer(LADSPA_PORT_OUTPUT);
  define_integer(LADSPA_PORT_CONTROL);
  define_integer(LADSPA_PORT_AUDIO);

  define_integer(LADSPA_HINT_BOUNDED_BELOW);
  define_integer(LADSPA_HINT_BOUNDED_ABOVE);
  define_integer(LADSPA_HINT_TOGGLED);
  define_integer(LADSPA_HINT_SAMPLE_RATE);
  define_integer(LADSPA_HINT_LOGARITHMIC);
  define_integer(LADSPA_HINT_INTEGER);
#ifdef LADSPA_HINT_DEFAULT_MASK
  define_integer(LADSPA_HINT_DEFAULT_MASK);
  define_integer(LADSPA_HINT_DEFAULT_NONE);
  define_integer(LADSPA_HINT_DEFAULT_MINIMUM);
  define_integer(LADSPA_HINT_DEFAULT_LOW);
  define_integer(LADSPA_HINT_DEFAULT_MIDDLE);
  define_integer(LADSPA_HINT_DEFAULT_HIGH);
  define_integer(LADSPA_HINT_DEFAULT_MAXIMUM);
  define_integer(LADSPA_HINT_DEFAULT_0);
  define_integer(LADSPA_HINT_DEFAULT_1);
  define_integer(LADSPA_HINT_DEFAULT_100);
  define_integer(LADSPA_HINT_DEFAULT_440);
#endif

  Xen_define_typed_procedure(S_analyse_ladspa,    g_analyse_ladspa_w,    2, 0, 0, H_analyse_ladspa, s7_make_signature(s7, 3, p, s, s));
  Xen_define_typed_procedure("analyze-ladspa",    g_analyse_ladspa_w,    2, 0, 0, H_analyse_ladspa, s7_make_signature(s7, 3, p, s, s));
  Xen_define_typed_procedure(S_apply_ladspa,      g_apply_ladspa_w,      4, 2, 0, H_apply_ladspa,   s7_make_signature(s7, 7, b, t, p, i, s, t, t));
  Xen_define_typed_procedure(S_init_ladspa,       g_init_ladspa_w,       0, 0, 0, H_init_ladspa,    s7_make_signature(s7, 1, b));
  Xen_define_typed_procedure(S_list_ladspa,       g_list_ladspa_w,       0, 0, 0, H_list_ladspa,    s7_make_signature(s7, 1, l));
  Xen_define_typed_procedure(S_ladspa_descriptor, g_ladspa_descriptor_w, 2, 0, 0, H_ladspa_descriptor, s7_make_circular_signature(s7, 0, 1, t));

  Xen_define_typed_procedure(PREFIX "Label",           g_ladspa_Label_w,              1, 0, 0, H_ladspa_Label,     s7_make_signature(s7, 2, s, t));
  Xen_define_typed_procedure(PREFIX "Name",            g_ladspa_Name_w,               1, 0, 0, H_ladspa_Name,      s7_make_signature(s7, 2, s, t));
  Xen_define_typed_procedure(PREFIX "Copyright",       g_ladspa_Copyright_w,          1, 0, 0, H_ladspa_Copyright, s7_make_signature(s7, 2, s, t));
  Xen_define_typed_procedure(PREFIX "Maker",           g_ladspa_Maker_w,              1, 0, 0, H_ladspa_Maker,     s7_make_signature(s7, 2, s, t));
  Xen_define_typed_procedure(PREFIX "Properties",      g_ladspa_Properties_w,         1, 0, 0, H_ladspa_Properties, s7_make_signature(s7, 2, i, t));
  Xen_define_typed_procedure(PREFIX "UniqueID",        g_ladspa_UniqueID_w,           1, 0, 0, H_ladspa_UniqueID,  s7_make_signature(s7, 2, i, t));
  Xen_define_typed_procedure(PREFIX "PortNames",       g_ladspa_PortNames_w,          1, 0, 0, H_ladspa_PortNames, s7_make_signature(s7, 2, l, t));
  Xen_define_typed_procedure(PREFIX "PortDescriptors", g_ladspa_PortDescriptors_w,    1, 0, 0, H_ladspa_PortDescriptors, s7_make_signature(s7, 2, l, t));
  Xen_define_typed_procedure(PREFIX "PortRangeHints",  g_ladspa_PortRangeHints_w,     1, 0, 0, H_ladspa_PortRangeHints, s7_make_signature(s7, 2, l, t));
  Xen_define_typed_procedure(PREFIX "PortCount",       g_ladspa_PortCount_w,          1, 0, 0, H_ladspa_PortCount,  s7_make_signature(s7, 2, i, t));
 
  Xen_define_typed_procedure(S_ladspa_instantiate,         g_ladspa_instantiate_w,         2, 0, 0, H_ladspa_instantiate, s7_make_signature(s7, 3, t, t, i));
  Xen_define_typed_procedure(S_ladspa_activate,            g_ladspa_activate_w,            2, 0, 0, H_ladspa_activate,    s7_make_signature(s7, 3, b, t, t));
  Xen_define_typed_procedure(S_ladspa_deactivate,          g_ladspa_deactivate_w,          2, 0, 0, H_ladspa_deactivate,  s7_make_signature(s7, 3, b, t, t));
  Xen_define_typed_procedure(S_ladspa_cleanup,             g_ladspa_cleanup_w,             2, 0, 0, H_ladspa_cleanup,     s7_make_signature(s7, 3, b, t, t));
  Xen_define_typed_procedure(S_ladspa_run,                 g_ladspa_run_w,                 3, 0, 0, H_ladspa_run,         s7_make_signature(s7, 4, b, t, t, i));
  Xen_define_typed_procedure(S_ladspa_run_adding,          g_ladspa_run_adding_w,          3, 0, 0, H_ladspa_run_adding,  s7_make_signature(s7, 4, b, t, t, i));
  Xen_define_typed_procedure(S_ladspa_set_run_adding_gain, g_ladspa_set_run_adding_gain_w, 3, 0, 0, H_ladspa_set_run_adding_gain, s7_make_signature(s7, 4, b, t, t, r));
  Xen_define_typed_procedure(S_ladspa_connect_port,        g_ladspa_connect_port_w,        4, 0, 0, H_ladspa_connect_port, s7_make_signature(s7, 5, b, t, t, i, fv));

  Xen_provide_feature("snd-ladspa");
}

#endif
#endif
