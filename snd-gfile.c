#include "snd.h"
#include "snd-file.h"

/* various file-related dialogs:
   File|Edit:Save-as
   File:Open|View
   File|Edit:Mix
   File:Insert
   File:Edit-Header
   File:New
   Info and Raw
   View:Files (replaced)
*/

#define HAVE_G_FILE_MONITOR_DIRECTORY 1 /* (GLIB_CHECK_VERSION(2, 18, 1) but it might be much older */

/* if thumbnail graph display is too slow: save the points? split the idler? g_source_remove if overlap?
 *
 * In gtk2, if we have a file selected, then some other process writes a file in the 
 *   current directory (emacs autosaving), the file chooser gets confused as
 *   to what is selected!  So we can't get the selected filename except immediately upon 
 *   "selection-changed" and even then we're asking for trouble.
 *
 * In gtk3, each time we reopen the chooser, it starts all the way back at the useless "recently used"!
 *   We can't use gtk_..._set|select_filename -- the function is simply ignored!
 *     so currently we never hide the dialog.
 *   And if we resize the dialog, a hand cursor appears, and the next thing we know we're moving the dialog itself!
 *   And valgrind reports a million invalid reads in gtk's code!
 *   And the sketch gets an incomprehensible cairo error
 *     this apparently happens because our "signal-changed" callback takes too long to draw a long sound's graph!
 *     but making it idle does not fully fix the problem -- we have to save the graph points and rescale directly.
 *
 * 3.10 will have gtk_file_chooser_get_current_name
 */

/* we can find the embedded tree view:

  (define* (traveler w (spaces 0))
    (gtk_container_foreach (GTK_CONTAINER w)
      (lambda (w1 d)
        (do ((i 0 (+ i 1)))
	    ((= i spaces))
	  (format #t " "))
        (format #t "~A " (gtk_widget_get_name w1))
        (if (GTK_IS_LABEL w1)
            (format #t "~A~%" (gtk_label_get_text (GTK_LABEL w1)))
	    (if (GTK_IS_BUTTON w1)
	        (format #t "~A~%" (gtk_button_get_label (GTK_BUTTON w1)))
	        (if (GTK_IS_ENTRY w1)
		    (format #t "~A~%" (gtk_entry_get_text (GTK_ENTRY w1)))
		    (begin
		      (newline)
		      (if (GTK_IS_CONTAINER w1)
			  (traveler w1 (+ spaces 2))))))))))

  (traveler (open-file-dialog))

* now how to get at the sidebar and remove "recently used"? or change the row-colors in gtk3?
* no way that I can find...
* in 3.12 they've added gtk_places_sidebar_set_local_only, but it's not clear how to get at this thing
*   and it probably isn't what we want anyway.
*/




/* ---------------------------------------- file monitor ---------------------------------------- */

#if HAVE_G_FILE_MONITOR_DIRECTORY

static void cleanup_new_file_watcher(void);
static void cleanup_edit_header_watcher(void);

void cleanup_file_monitor(void)
{
  cleanup_edit_header_watcher();
  cleanup_new_file_watcher();
  ss->file_monitor_ok = false;
}

void *unmonitor_file(void *watcher) 
{
  if (G_IS_FILE_MONITOR(watcher))
    g_file_monitor_cancel((GFileMonitor *)watcher);
  return(NULL);
}

static void *unmonitor_directory(void *watcher) 
{
  if (G_IS_FILE_MONITOR(watcher))
    g_file_monitor_cancel((GFileMonitor *)watcher);
  return(NULL);
}


static void sp_file_changed(GFileMonitor *mon, GFile *file, GFile *other, GFileMonitorEvent ev, gpointer data)
{
  snd_info *sp = (snd_info *)data;
  if (sp->writing) return;
  
  switch (ev)
    {
    case G_FILE_MONITOR_EVENT_CHANGED:
      /* this includes cp overwriting old etc */
      if (file_write_date(sp->filename) != sp->write_date) /* otherwise chmod? */
	{
	  sp->need_update = true;
	  if (auto_update(ss))
	    snd_update(sp);
	  else start_bomb(sp);
	}
#ifndef _MSC_VER
      else
	{
	  int err;
	  err = access(sp->filename, R_OK);
	  if (err < 0)
	    {
	      char *msg;
	      msg = mus_format("%s is read-protected!", sp->short_filename);
	      status_report(sp, "%s", msg);
	      free(msg);
	      sp->file_unreadable = true;
	      start_bomb(sp);
	    }
	  else
	    {
	      sp->file_unreadable = false;
	      clear_status_area(sp);
	      err = access(sp->filename, W_OK);
	      if (err < 0)   /* if err < 0, then we can't write (W_OK -> error ) */
		sp->file_read_only = FILE_READ_ONLY; 
	      else sp->file_read_only = FILE_READ_WRITE;
	      if ((sp->user_read_only == FILE_READ_ONLY) || 
		  (sp->file_read_only == FILE_READ_ONLY)) 
		show_lock(sp); 
	      else hide_lock(sp);
	    }
	}
#endif
      break;

    case G_FILE_MONITOR_EVENT_DELETED:
      /* snd_update will post a complaint in this case, but I like it explicit */
      if (mus_file_probe(sp->filename) == 0)
	{
	  /* user deleted file while editing it? */
	  status_report(sp, "%s no longer exists!", sp->short_filename);
	  sp->file_unreadable = true;
	  start_bomb(sp);
	  return;
	}

    case G_FILE_MONITOR_EVENT_CREATED:
#if HAVE_GTK_WIDGET_GET_VISIBLE
    case G_FILE_MONITOR_EVENT_MOVED:
#endif
      if (sp->write_date != file_write_date(sp->filename))
	{
	  sp->file_unreadable = false;
	  sp->need_update = true;
	  if (auto_update(ss))
	    snd_update(sp);
	  else start_bomb(sp);
	}
      break;

    default:
      /* ignore the rest */
      break;
    }
}


void monitor_sound(snd_info *sp)
{
  GFile *file;
  GError *err = NULL;

  file = g_file_new_for_path(sp->filename);

  sp->file_watcher = (void *)g_file_monitor_file(file, G_FILE_MONITOR_NONE, NULL, &err);
  if (err)
    snd_warning("%s", err->message);
  else g_signal_connect(G_OBJECT(sp->file_watcher), "changed", G_CALLBACK(sp_file_changed), (gpointer)sp);   

  g_object_unref(file); /* is this safe? */
}

#else

void cleanup_file_monitor(void) {}
void *unmonitor_file(void *watcher) {return(NULL);}
static void *unmonitor_directory(void *watcher) {return(NULL);}
void monitor_sound(snd_info *sp) {}

#endif



/* ---------------------------------------- dialogs ---------------------------------------- */

#define FILE_DIALOG_DEFAULT_WIDTH 500
#define FILE_DIALOG_DEFAULT_HEIGHT 300
#define FILE_DIALOG_DEFAULT_SKETCH_HEIGHT 75

typedef struct file_dialog_info {

  GtkWidget *dialog, *ok_button, *cancel_button, *help_button, *extract_button, *play_button, *chooser;
  read_only_t file_dialog_read_only;
  GtkWidget *frame, *info, *vbox;
  void *unsound_directory_watcher; /* doesn't exist, not a sound file, bogus header, etc */
  void *info_filename_watcher;     /* watch for change in selected file and repost info */
  char *unsound_dirname, *unsound_filename;
  char *info_filename;

  snd_info *player;

  file_data *panel_data;
  char *filename;
  save_dialog_t type;
  void *file_watcher;
  gulong filename_watcher_id;
  mus_header_t header_type;
  mus_sample_t sample_type;
#if (GTK_CHECK_VERSION(3, 89, 0))
  cairo_t *cr;
#endif
  point_t *p0, *p1;
  int pts;
  gc_t *gc;
  axis_info *axis;
  GtkWidget *drawer;
  snd_info *sp;
#if (!GTK_CHECK_VERSION(3, 89, 0))
  bool in_progress;
#endif
  bool two_sided;
  mus_long_t samps;
  int srate;
  bool unreadable;
} file_dialog_info;

static file_dialog_info *odat = NULL; /* open file */
static file_dialog_info *mdat = NULL; /* mix file */
static file_dialog_info *idat = NULL; /* insert file */

void reflect_just_sounds(void) {}


static bool post_sound_info(file_dialog_info *fd, const char *filename, bool with_filename)
{
  if ((!filename) ||
      (is_directory(filename)) ||
      (!is_sound_file(filename)))
    {
      gtk_label_set_text(GTK_LABEL(fd->info), "");
      gtk_widget_hide(fd->drawer);
      return(false);
    }
  else
    {
      char *buf;
      char *mx, *lenstr;
      buf = (char *)calloc(1024, sizeof(char));
      if (mus_sound_maxamp_exists(filename))
	{
	  int i, chns, lim;
	  mus_long_t pos = 0;

	  mx = (char *)calloc(128, sizeof(char));
	  chns = mus_sound_chans(filename);
	  lim = 5;
	  if (chns < lim) lim = chns;
	  snprintf(mx, 128, "\nmaxamp: %.3f", mus_sound_channel_maxamp(filename, 0, &pos));
	  for (i = 1; i < lim; i++)
	    {
	      char fb[16];
	      snprintf(fb, 16, " %.3f", mus_sound_channel_maxamp(filename, i, &pos));
	      strcat(mx, fb);
	    }
	  if (lim < chns)
	    strcat(mx, "...");
	}
      else
	{
	  mx = (char *)calloc(2, sizeof(char));
	  mx[0] = '\n';
	}

      lenstr = (char *)calloc(128, sizeof(char));
      if (mus_sound_samples(filename) < 1000)
	snprintf(lenstr, 128, "%d samples", (int)mus_sound_samples(filename));
      else snprintf(lenstr, 128, "%.3f seconds", mus_sound_duration(filename));

      snprintf(buf, 1024, "%s%s%d chan%s, %d Hz, %s\n%s, %s%s%s",

		   (with_filename) ? filename_without_directory(filename) : "",
		   (with_filename) ? ": " : "", 
		   mus_sound_chans(filename),
		   (mus_sound_chans(filename) > 1) ? "s" : "",
		   mus_sound_srate(filename),
		   lenstr,
		   mus_header_type_name(mus_sound_header_type(filename)),
		   short_sample_type_name(mus_sound_sample_type(filename), filename),
		   snd_strftime(", %d-%b-%Y", mus_sound_write_date(filename)),
		   mx);

      gtk_label_set_text(GTK_LABEL(fd->info), buf);
      free(buf);
      free(mx);
      free(lenstr);
      gtk_widget_show(fd->drawer);
    }
  return(true);
}


static void file_dialog_stop_playing(file_dialog_info *fd)
{
  if ((fd->player) && 
      (fd->player->playing)) 
    {
      stop_playing_sound(fd->player, PLAY_BUTTON_UNSET);
      fd->player = NULL;
    }
}


void clear_deleted_snd_info(void *ufd)
{
  file_dialog_info *fd = (file_dialog_info *)ufd;
  fd->player = NULL;
}


#if WITH_AUDIO
static void file_dialog_play(GtkWidget *w, gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;

  if ((fd->player) && 
      (fd->player->playing)) 
    file_dialog_stop_playing(fd);
  else
    {
      char *filename;
      filename = fd->filename; /* gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(fd->chooser)); */
      if ((filename) &&
	  (!is_directory(filename)))
	{
	  if (mus_file_probe(filename))
	    {
	      fd->player = make_sound_readable(filename, false);
	      fd->player->delete_me = (void *)fd;
	      if (fd->player)
		play_sound(fd->player, 0, NO_END_SPECIFIED);
	    }
	}
    }
}
#endif


static void tiny_string(cairo_t *cr, const char *str, int x0, int y0)
{
  PangoLayout *layout;
  cairo_save(cr);
  layout = pango_cairo_create_layout(cr);
  pango_layout_set_font_description(layout, TINY_FONT(ss));
  pango_layout_set_text(layout, str, -1);
  cairo_move_to(cr, x0, y0);
  pango_cairo_show_layout(cr, layout);
  g_object_unref(G_OBJECT(layout));
  cairo_restore(cr);
}

static void sketch_1(file_dialog_info *fd, bool new_data)
{
  #define X_AXIS 24
  #define Y_AXIS 8

  char *filename, *str;
  int hgt, wid, i, xoff, yoff;
  axis_info *ap;
  cairo_t *old_cr;
  double xscl, yscl;
  point_t *g_p0, *g_p1;

#if (GTK_CHECK_VERSION(3, 89, 0))
  if (!(fd->cr)) return;
#endif

  filename = fd->filename; /* gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(fd->chooser)); */

  if (!filename)
    return;

  if ((is_directory(filename)) ||
      (!is_sound_file(filename)))
    return;
  
  if ((!new_data) &&
      (fd->unreadable))
    return;
  
  wid = widget_width(fd->drawer);
  hgt = widget_height(fd->drawer);
  /* fprintf(stderr, "%d %d\n", wid, hgt); */
  ap = fd->axis;
  
  if (new_data)
    {
      bool two_sided = false;
      chan_info *active_channel;
      axis_info *active_ap = NULL;
      snd_info *sp;
      
      sp = make_sound_readable(filename, false);
      if (!sp)
	{
	  fd->unreadable = true;
	  return;
	}
      fd->unreadable = false;
      active_channel = sp->chans[0];
      active_ap = active_channel->axis;
      ap->graph_active = true;
      
      fd->samps = current_samples(active_channel);
      fd->srate = snd_srate(active_channel->sound);
      
      ap->losamp = 0;
      ap->hisamp = fd->samps - 1;
      ap->y0 = -1.0;
      ap->y1 = 1.0;
      ap->x0 = 0.0;
      ap->x1 = (double)(fd->samps) / (double)(fd->srate);
      
      ap->x_axis_x0 = 0;
      ap->y_axis_y0 = 1000;
      ap->x_axis_x1 = 1000;
      ap->y_axis_y1 = 0;
      
      active_channel->axis = ap;
      init_axis_scales(ap);
      fd->pts = make_background_graph(active_channel, fd->srate, &two_sided);
      fd->two_sided = two_sided;
      memcpy((void *)(fd->p0), (void *)get_grf_points(), fd->pts * sizeof(point_t));
      if (fd->two_sided)
	memcpy((void *)(fd->p1), (void *)get_grf_points1(), fd->pts * sizeof(point_t));
      active_channel->axis = active_ap;
      
      sp->chans[0]->active = CHANNEL_INACTIVE;
      completely_free_snd_info(sp);
      sp = NULL;
    }
  else
    {
      ap->x1 = (double)(fd->samps) / (double)(fd->srate);
    }

  ap->x_axis_x0 = X_AXIS + 2;
  ap->y_axis_y1 = Y_AXIS;
  ap->x_axis_x1 = wid - 4;
  ap->y_axis_y0 = hgt - Y_AXIS * 2;
  
  g_p0 = get_grf_points();
  g_p1 = get_grf_points1();
  if (!new_data)
    {
      memcpy((void *)g_p0, (void *)(fd->p0), fd->pts * sizeof(point_t));
      if (fd->two_sided)
	memcpy((void *)g_p1, (void *)(fd->p1), fd->pts * sizeof(point_t));
    }
  
  xoff = ap->x_axis_x0;
  yoff = ap->y_axis_y1;
  xscl = (ap->x_axis_x1 - xoff) * 0.001;
  yscl = (ap->y_axis_y0 - yoff) * 0.001;
  init_axis_scales(ap);
  
  for (i = 0; i < fd->pts; i++)
    {
      g_p0[i].x = xoff + (int)(g_p0[i].x * xscl);
      g_p0[i].y = yoff + (int)(g_p0[i].y * yscl);
    }
  if (fd->two_sided)
    {
      for (i = 0; i < fd->pts; i++)
	{
	  g_p1[i].x = xoff + (int)(g_p1[i].x * xscl);
	  g_p1[i].y = yoff + (int)(g_p1[i].y * yscl);
	}
    }

  /* here we could check that nothing has changed while getting the data ready, but it doesn't seem to be a problem? */

  old_cr = ss->cr;
#if (GTK_CHECK_VERSION(3, 89, 0))
  ss->cr = fd->cr;
#else
  ss->cr = make_cairo(WIDGET_TO_WINDOW(fd->drawer));
#endif
  
  cairo_push_group(ss->cr);
  cairo_set_source_rgba(ss->cr, fd->gc->bg_color->red, fd->gc->bg_color->green, fd->gc->bg_color->blue, fd->gc->bg_color->alpha);
  cairo_rectangle(ss->cr, 0, 0, wid, hgt);
  cairo_fill(ss->cr);

  cairo_set_source_rgba(ss->cr, fd->gc->fg_color->red, fd->gc->fg_color->green, fd->gc->fg_color->blue, fd->gc->fg_color->alpha);
  /* y axis */
  cairo_rectangle(ss->cr, X_AXIS, 8, 2, hgt - Y_AXIS - 16);
  cairo_fill(ss->cr);
  
  /* x axis */
  cairo_rectangle(ss->cr, X_AXIS, hgt - Y_AXIS - 8, wid - X_AXIS - 4, 2);
  cairo_fill(ss->cr);
  
  tiny_string(ss->cr, "1.0", 4, 6);
  tiny_string(ss->cr, "-1.0", 0, hgt - 24);
  tiny_string(ss->cr, "0.0", 24, hgt - 12);

  str = prettyf(ap->x1, 3);
  if (str)
    {
      tiny_string(ss->cr, str, wid - 4 - 6 * strlen(str), hgt - 12);
      free(str);
    }
      
  if (fd->pts > 0) 
    {
#if (GTK_CHECK_VERSION(3, 89, 0))
      cairo_set_line_width(ss->cr, 1.0);
#endif
      if (fd->two_sided)
	draw_both_grf_points(1, ap->ax, fd->pts, GRAPH_LINES);
      else draw_grf_points(1, ap->ax, fd->pts, ap, 0.0, GRAPH_LINES);
    }

  cairo_pop_group_to_source(ss->cr);
  cairo_paint(ss->cr);
  
#if (!GTK_CHECK_VERSION(3, 89, 0))
  free_cairo(ss->cr);
#endif
  ss->cr = old_cr;
}


static idle_func_t get_sketch(gpointer data)
{
  sketch_1((file_dialog_info *)data, true);
  return(false);
}

#if (!GTK_CHECK_VERSION(3, 89, 0))
static void stop_sketch(gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;
  fd->in_progress = false;
}
#endif

#if GTK_CHECK_VERSION(3, 89, 0)
#define sketch(Fd) get_sketch(Fd)
#else
#define sketch(Fd)   {fd->in_progress = true; g_idle_add_full(G_PRIORITY_DEFAULT_IDLE, get_sketch, (gpointer)Fd, (GDestroyNotify)stop_sketch);}
#define resketch(Fd) if (!fd->in_progress) {fd->in_progress = true; g_idle_add_full(G_PRIORITY_DEFAULT_IDLE, get_sketch, (gpointer)Fd, (GDestroyNotify)stop_sketch);}
#endif

static void selection_changed_callback(GtkFileChooser *w, gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;
  fd->filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(fd->chooser));
  if (post_sound_info(fd, fd->filename, false))
    sketch(fd);
}


static gboolean file_filter_callback(const GtkFileFilterInfo *filter_info, gpointer data)
{
  /* return true => include this file */
  if (filter_info)
    return(Xen_boolean_to_C_bool(Xen_call_with_1_arg((Xen)data, C_string_to_Xen_string(filter_info->filename), "filter func")));
  return(false);
}


#if (GTK_CHECK_VERSION(3, 89, 0))
static void file_sketch_expose(GtkDrawingArea *w, cairo_t *cr, int width, int height, gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;
  fd->cr = cr;
  sketch(fd);
}
#else
static gboolean drawer_expose(GtkWidget *w, GdkEventExpose *ev, gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;
  resketch(fd);
  return(false);
}
#endif


/* if icons do not get displayed, check the system preferences menu+toolbar dialog */

static file_dialog_info *make_fsb(const char *title, const char *file_lab, const char *ok_lab,
				  const gchar *stock, bool with_extract, bool save_as)
{
  file_dialog_info *fd;
  int i;
  GtkFileFilter *just_sounds_filter, *all_files_filter;
  const char **exts;
  char buf[32];
  
  fd = (file_dialog_info *)calloc(1, sizeof(file_dialog_info));

  /* -------- base dialog -------- */
  fd->dialog = snd_gtk_dialog_new();
#if GTK_CHECK_VERSION(3, 14, 0)
  gtk_window_set_transient_for(GTK_WINDOW(fd->dialog), GTK_WINDOW(main_shell(ss)));
#endif
  gtk_window_set_title(GTK_WINDOW(fd->dialog), title);
  sg_make_resizable(fd->dialog);
  sg_container_set_border_width(GTK_CONTAINER(fd->dialog), 10);
  gtk_widget_realize(fd->dialog);


  /* -------- buttons -------- */
  fd->help_button = gtk_dialog_add_button(GTK_DIALOG(fd->dialog), "Help", GTK_RESPONSE_NONE);
#if WITH_AUDIO
  if (!save_as)
    fd->play_button = gtk_dialog_add_button(GTK_DIALOG(fd->dialog), "Play", GTK_RESPONSE_NONE);
#endif
  if (with_extract)
    fd->extract_button = gtk_dialog_add_button(GTK_DIALOG(fd->dialog), "Extract", GTK_RESPONSE_NONE);
  fd->cancel_button = gtk_dialog_add_button(GTK_DIALOG(fd->dialog), "Go away", GTK_RESPONSE_NONE);
  fd->ok_button = gtk_dialog_add_button(GTK_DIALOG(fd->dialog), "Ok", GTK_RESPONSE_NONE);

  gtk_widget_set_name(fd->help_button, "dialog_button");
  gtk_widget_set_name(fd->cancel_button, "dialog_button");
  if (with_extract)
    gtk_widget_set_name(fd->extract_button, "dialog_button");
  gtk_widget_set_name(fd->ok_button, "dialog_button");
#if WITH_AUDIO
  if (!save_as)
    {
      gtk_widget_set_name(fd->play_button, "dialog_button");
#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(fd->play_button);
#endif
    }
#endif

#if GTK_CHECK_VERSION(3, 0, 0)
  add_highlight_button_style(fd->ok_button);
  add_highlight_button_style(fd->cancel_button);
  add_highlight_button_style(fd->help_button);
  if (with_extract) add_highlight_button_style(fd->extract_button);
#endif

  gtk_widget_show(fd->ok_button);
  gtk_widget_show(fd->cancel_button);
  gtk_widget_show(fd->help_button);
#if WITH_AUDIO
  if (!save_as)
    gtk_widget_show(fd->play_button);
#endif
  if (with_extract) gtk_widget_show(fd->extract_button);

  just_sounds_filter = gtk_file_filter_new();
  gtk_file_filter_set_name(just_sounds_filter, "Just sounds");
  exts = get_sound_file_extensions();
  buf[0] = '*';
  buf[1] = '.';
  for (i = 0; i < sound_file_extensions_length(); i++)
    {
      buf[2] = '\0';
      strcat((char *)(buf + 2), exts[i]);
      gtk_file_filter_add_pattern(just_sounds_filter, buf);
    }
  all_files_filter = gtk_file_filter_new();
  gtk_file_filter_set_name(all_files_filter, "All files");
  gtk_file_filter_add_pattern(all_files_filter, "*");

  fd->chooser = gtk_file_chooser_widget_new((save_as) ? GTK_FILE_CHOOSER_ACTION_SAVE : GTK_FILE_CHOOSER_ACTION_OPEN);
  sg_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(fd->dialog)), fd->chooser, true, true, 10);
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(fd->chooser), just_sounds_filter);
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(fd->chooser), all_files_filter);

#if HAVE_EXTENSION_LANGUAGE
  {
    /* now look for added filters added via add-file-filter */
    int i;
    for (i = 0; i < ss->file_filters_size; i++)
      if (!(Xen_is_false(Xen_vector_ref(ss->file_filters, i))))
	{
	  const char *filter_name;
	  GtkFileFilter *nfilt;
	  Xen filter_func;

	  filter_name = Xen_string_to_C_string(Xen_car(Xen_vector_ref(ss->file_filters, i)));
	  filter_func = Xen_cadr(Xen_vector_ref(ss->file_filters, i));

	  nfilt = gtk_file_filter_new();
	  gtk_file_filter_set_name(nfilt, filter_name);
	  gtk_file_filter_add_custom(nfilt, GTK_FILE_FILTER_FILENAME, file_filter_callback, (gpointer)filter_func, NULL);

	  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(fd->chooser), nfilt);
	}
  }
#endif
  
  gtk_widget_show(fd->chooser);
  gtk_widget_set_size_request(fd->chooser, FILE_DIALOG_DEFAULT_WIDTH, FILE_DIALOG_DEFAULT_HEIGHT); 

  gtk_file_chooser_set_filter(GTK_FILE_CHOOSER(fd->chooser), (just_sounds(ss)) ? just_sounds_filter : all_files_filter);
#if HAVE_GTK_WIDGET_GET_VISIBLE
  if (save_as)
    gtk_file_chooser_set_create_folders(GTK_FILE_CHOOSER(fd->chooser), true);
#endif
  fd->filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(fd->chooser));

  return(fd);
}


static file_dialog_info *make_file_dialog(read_only_t read_only, const char *title, const char *file_title, const char *ok_title,
					  snd_dialog_t which_dialog, 
					  GCallback file_ok_proc,
					  GCallback file_mkdir_proc,
					  GCallback file_delete_proc,
					  GCallback file_dismiss_proc,
					  GCallback file_help_proc,
					  const gchar *stock)
{
  file_dialog_info *fd;
  GtkWidget *vbox, *hbox;

  fd = make_fsb(title, file_title, ok_title, stock, false, false);
  fd->file_dialog_read_only = read_only;

  vbox = DIALOG_CONTENT_AREA(fd->dialog);

  hbox = gtk_hbox_new(true, 8);
  sg_box_pack_start(GTK_BOX(vbox), hbox, true, true, 8);
  gtk_widget_show(hbox);

  fd->info = gtk_label_new(NULL);
  sg_box_pack_start(GTK_BOX(hbox), fd->info, false, true, 8);
  gtk_widget_show(fd->info);

  fd->sp = NULL;
  fd->gc = gc_new();
  gc_set_background(fd->gc, ss->white);
  gc_set_foreground(fd->gc, ss->black);
  
  fd->drawer = gtk_drawing_area_new();
  sg_box_pack_end(GTK_BOX(hbox), fd->drawer, true, true, 10);
#if (!GTK_CHECK_VERSION(3, 89, 0))
  sg_widget_set_events(fd->drawer, GDK_EXPOSURE_MASK);
#endif
  gtk_widget_set_size_request(fd->drawer, -1, FILE_DIALOG_DEFAULT_SKETCH_HEIGHT);
  gtk_widget_show(fd->drawer);
  
  fd->axis = (axis_info *)calloc(1, sizeof(axis_info));
  fd->axis->ax = (graphics_context *)calloc(1, sizeof(graphics_context));
  fd->axis->ax->wn = WIDGET_TO_WINDOW(fd->drawer);
  fd->axis->ax->w = fd->drawer;
  fd->axis->ax->gc = fd->gc;
  fd->axis->ax->current_font = AXIS_NUMBERS_FONT(ss);
  fd->p0 = (point_t *)calloc(POINT_BUFFER_SIZE, sizeof(point_t));
  fd->p1 = (point_t *)calloc(POINT_BUFFER_SIZE, sizeof(point_t));
  fd->unreadable = true;

#if GTK_CHECK_VERSION(3, 89, 0)
  gtk_drawing_area_set_content_width(GTK_DRAWING_AREA(fd->drawer), gtk_widget_get_allocated_width(fd->drawer));
  gtk_drawing_area_set_content_height(GTK_DRAWING_AREA(fd->drawer), gtk_widget_get_allocated_height(fd->drawer));
  gtk_drawing_area_set_draw_func(GTK_DRAWING_AREA(fd->drawer), file_sketch_expose, (void *)fd, NULL);
#else
  SG_SIGNAL_CONNECT(fd->drawer, DRAW_SIGNAL, drawer_expose, (gpointer)fd);
#endif

  gtk_widget_show(fd->dialog);

  SG_SIGNAL_CONNECT(fd->help_button, "clicked", file_help_proc, (gpointer)fd);
  SG_SIGNAL_CONNECT(fd->ok_button, "clicked", file_ok_proc, (gpointer)fd);
  SG_SIGNAL_CONNECT(fd->cancel_button, "clicked", file_dismiss_proc, (gpointer)fd);
  if (file_delete_proc) 
    SG_SIGNAL_CONNECT(fd->dialog, "delete_event", file_delete_proc, (gpointer)fd);
#if WITH_AUDIO
  SG_SIGNAL_CONNECT(fd->play_button, "clicked", file_dialog_play, (gpointer)(fd));
#endif

  gtk_label_set_text(GTK_LABEL(fd->info), "");
  set_dialog_widget(which_dialog, fd->dialog);

  SG_SIGNAL_CONNECT(fd->chooser, "selection-changed", selection_changed_callback, (gpointer)fd);
  return(fd);
}


static void file_open_error(const char *error_msg, file_dialog_info *fd)
{
  gtk_label_set_text(GTK_LABEL(fd->info), error_msg);
  gtk_widget_show(fd->info);
}


static void redirect_file_open_error(const char *error_msg, void *ufd)
{
  /* called from snd_error, redirecting error handling to the dialog */
  file_open_error(error_msg, (file_dialog_info *)ufd);
}


static void clear_file_error_label(file_dialog_info *fd)
{
  gtk_label_set_text(GTK_LABEL(fd->info), "");

  if (fd->unsound_directory_watcher)
    {
      fd->unsound_directory_watcher = unmonitor_directory(fd->unsound_directory_watcher);
      if (fd->unsound_dirname) {free(fd->unsound_dirname); fd->unsound_dirname = NULL;}
      if (fd->unsound_filename) {free(fd->unsound_filename); fd->unsound_filename = NULL;}
    }
}


/* key press event here, not key release -- the latter is triggered by the <return> release
 *   that triggered the error, so our error is immediately erased
 */

#if HAVE_G_FILE_MONITOR_DIRECTORY 
static void unpost_unsound_error(GFileMonitor *mon, GFile *file, GFile *other, GFileMonitorEvent ev, gpointer data)
{
  file_dialog_info *fd;
  char *filename;
  switch (ev)
    {
    case G_FILE_MONITOR_EVENT_CHANGED:
    case G_FILE_MONITOR_EVENT_CREATED:
      filename = g_file_get_path(file);
      fd = (file_dialog_info *)data;
      if ((fd) &&
	  (filename) &&
	  (mus_strcmp(filename, fd->unsound_filename)))
	clear_file_error_label(fd);
      break;

    default:
      /* ignore the rest */
      break;
    }
}


static void start_unsound_watcher(file_dialog_info *fd, const char *filename)
{
  GFile *file;
  GError *err = NULL;

  if (fd->unsound_directory_watcher)
    {
      fd->unsound_directory_watcher = unmonitor_directory(fd->unsound_directory_watcher);
      if (fd->unsound_dirname) free(fd->unsound_dirname);
      if (fd->unsound_filename) free(fd->unsound_filename);
    }

  fd->unsound_filename = mus_expand_filename(filename);
  fd->unsound_dirname = just_directory(fd->unsound_filename);

  file = g_file_new_for_path(fd->unsound_dirname);
  fd->unsound_directory_watcher = (void *)g_file_monitor_directory(file, G_FILE_MONITOR_NONE, NULL, &err);
  if (err)
    snd_warning("%s", err->message);
  else g_signal_connect(G_OBJECT(fd->unsound_directory_watcher), "changed", G_CALLBACK(unpost_unsound_error), (gpointer)fd);
  g_object_unref(file);
}
#else
static void start_unsound_watcher(file_dialog_info *fd, const char *filename) {}
#endif


static void file_open_dialog_ok(GtkWidget *w, gpointer data)
{
  file_dialog_info *fd = (file_dialog_info *)data;
  char *filename;

  filename = fd->filename; /* gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(fd->chooser)); */

  /* fprintf(stderr, "open %s, %s\n", fd->filename, gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(fd->chooser))); */

  file_dialog_stop_playing(fd);
  if ((filename) &&
      (!is_directory(filename)))
    {
      snd_info *sp;
      redirect_snd_error_to(redirect_file_open_error, (void *)fd);
      ss->requestor_dialog = fd->dialog;
      ss->open_requestor = FROM_OPEN_DIALOG;
      sp = snd_open_file(filename, fd->file_dialog_read_only);
      redirect_snd_error_to(NULL, NULL);
      if (sp) 
	{
#if (!GTK_CHECK_VERSION(3, 0, 0))
	  gpointer hide_me;
	  hide_me = g_object_get_data(G_OBJECT(fd->dialog), "hide-me"); /* see snd-gtk.scm where this is set */
	  if (hide_me == 0)
	    gtk_widget_hide(fd->dialog);
#endif
	  select_channel(sp, 0); /* add_sound_window (snd-xsnd.c) -> make_file_info (snd-file) will report reason for error, if any */
	}
      else
	{
	  if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
	    {
	      start_unsound_watcher(fd, filename);
	    }
	}
    }
  else
    {
      char *str;
      str = mus_format("%s is a directory", filename);
      file_open_error(str, fd);
      free(str);
    }
}


static void file_open_dialog_dismiss(GtkWidget *w, gpointer context)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  file_dialog_stop_playing(fd);
  gtk_label_set_text(GTK_LABEL(fd->info), "");
  gtk_widget_hide(fd->dialog);
}


static void file_open_dialog_help(GtkWidget *w, gpointer context)
{
  open_file_dialog_help();
}


static gint file_open_dialog_delete(GtkWidget *w, GdkEvent *event, gpointer context)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  file_dialog_stop_playing(fd);
  gtk_label_set_text(GTK_LABEL(fd->info), "");
  gtk_widget_hide(fd->dialog);
  return(true);
}


static void file_activated_callback(GtkFileChooser *w, gpointer data)
{
  const char *filename;
  file_dialog_info *fd = (file_dialog_info *)data;
  fd->filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(fd->chooser));
  filename = fd->filename;
  if ((filename) &&
      (!(is_directory(filename))))
    {
      snd_info *sp;
      
      redirect_snd_error_to(redirect_file_open_error, (void *)fd);
      ss->requestor_dialog = fd->dialog;
      ss->open_requestor = FROM_OPEN_DIALOG;
      sp = snd_open_file(filename, fd->file_dialog_read_only);
      redirect_snd_error_to(NULL, NULL);

      if (sp) 
	{
#if (!GTK_CHECK_VERSION(3, 0, 0))
	  gpointer hide_me;
	  hide_me = g_object_get_data(G_OBJECT(fd->dialog), "hide-me"); /* see snd-gtk.scm where this is set */
	  if (hide_me == 0)
	    gtk_widget_hide(fd->dialog);
#endif
	  select_channel(sp, 0); /* add_sound_window (snd-xsnd.c) -> make_file_info (snd-file) will report reason for error, if any */
	}
      else
	{
	  start_unsound_watcher(fd, filename);
	}
    }
}


widget_t make_open_file_dialog(read_only_t read_only, bool managed)
{
  if (!odat)
    {
      odat = make_file_dialog(read_only, 
			      (char *)((read_only == FILE_READ_ONLY) ? "View" : "Open"), 
			      (char *)((read_only == FILE_READ_ONLY) ? "view:" : "open:"),
			      NULL,
			      FILE_OPEN_DIALOG,
			      (GCallback)file_open_dialog_ok,	
			      NULL, /* no mkdir */
			      (GCallback)file_open_dialog_delete,
			      (GCallback)file_open_dialog_dismiss,
			      (GCallback)file_open_dialog_help,
			      ICON_OPEN);
      SG_SIGNAL_CONNECT(odat->chooser, "file-activated", file_activated_callback, (gpointer)odat);
    }
  else
    {
      if (read_only != odat->file_dialog_read_only)
	{
	  set_stock_button_label(odat->ok_button, (char *)((read_only == FILE_READ_ONLY) ? "View" : "Open"));
	  gtk_window_set_title(GTK_WINDOW(odat->dialog), (char *)((read_only == FILE_READ_ONLY) ? "View" : "Open"));
	  odat->file_dialog_read_only = read_only;
	}
#if GTK_CHECK_VERSION(3, 0, 0)
      /* this doesn't work!! and nothing else does either */
      /* if (odat->filename) gtk_file_chooser_select_filename(GTK_FILE_CHOOSER(odat->chooser), odat->filename); */
#endif
    }

  if (managed) 
    gtk_widget_show(odat->dialog);
  return(odat->dialog);
}


/* -------- mix file dialog -------- */

static void file_mix_cancel_callback(GtkWidget *w, gpointer context)
{
  file_dialog_stop_playing(mdat);
  gtk_label_set_text(GTK_LABEL(mdat->info), "");
  gtk_widget_hide(mdat->dialog);
}


static void file_mix_help_callback(GtkWidget *w, gpointer context)
{
  mix_file_dialog_help();
}


static gint file_mix_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  file_dialog_stop_playing(mdat);
  gtk_label_set_text(GTK_LABEL(mdat->info), "");
  gtk_widget_hide(mdat->dialog);
  return(true);
}


static void file_mix_ok_callback(GtkWidget *w, gpointer context)
{
  char *filename;
  filename = mdat->filename; /* gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(mdat->chooser)); */
  if ((!filename) || (!(*filename)))
    {
      file_open_error("no filename given", mdat);
    }
  else
    {
      file_dialog_stop_playing(mdat);
      if (!(is_directory(filename)))
	{
	  snd_info *sp;
	  int err;
	  sp = any_selected_sound();
	  redirect_snd_error_to(redirect_file_open_error, (void *)mdat);
	  ss->requestor_dialog = mdat->dialog;
	  ss->open_requestor = FROM_MIX_DIALOG;
	  err = mix_complete_file_at_cursor(sp, filename);
	  redirect_snd_error_to(NULL, NULL);
	  if (err < 0) 
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		{
		  if (err == MIX_FILE_NO_FILE)
		    start_unsound_watcher(mdat, filename);
		}
	    }
	  else 
	    {
	      status_report(sp, "%s mixed in at cursor", filename);
	    }
	}
      else 
	{
	  char *str;
	  str = mus_format("%s is a directory", filename);
	  file_open_error(str, mdat);
	  free(str);
	}
    }
}


widget_t make_mix_file_dialog(bool managed)
{
  if (!mdat)
    {
      mdat = make_file_dialog(FILE_READ_ONLY, "Mix", "mix:", "Mix", FILE_MIX_DIALOG,
			      (GCallback)file_mix_ok_callback,
			      NULL, /* no mkdir */
			      (GCallback)file_mix_delete_callback,
			      (GCallback)file_mix_cancel_callback,
			      (GCallback)file_mix_help_callback,
			      ICON_ADD);
    }

  if (managed) gtk_widget_show(mdat->dialog);
  return(mdat->dialog);
}


/* -------- File:Insert dialog -------- */

static void file_insert_cancel_callback(GtkWidget *w, gpointer context)
{
  file_dialog_stop_playing(idat);
  gtk_widget_hide(idat->dialog);
}


static void file_insert_help_callback(GtkWidget *w, gpointer context)
{
  insert_file_dialog_help();
}


static gint file_insert_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  file_dialog_stop_playing(idat);
  gtk_widget_hide(idat->dialog);
  return(true);
}


static void file_insert_ok_callback(GtkWidget *w, gpointer context)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  char *filename;
  filename = fd->filename; /* gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(fd->chooser)); */
  if ((!filename) || (!(*filename)))
    {
      file_open_error("no filename given", fd);
    }
  else
    {
      file_dialog_stop_playing(fd);
      if (!(is_directory(filename)))               /* this can be a directory name if the user clicked 'ok' when he meant 'cancel' */
	{
	  bool ok;
	  snd_info *sp;

	  sp = any_selected_sound();
	  ss->requestor_dialog = w;
	  ss->open_requestor = FROM_INSERT_DIALOG;
	  redirect_snd_error_to(redirect_file_open_error, (void *)fd);
	  ok = insert_complete_file_at_cursor(sp, filename);
	  redirect_snd_error_to(NULL, NULL);
	  if (!ok)
	    {
	      if (ss->open_requestor != FROM_RAW_DATA_DIALOG)
		{
		  char *fullname;
		  fullname = mus_expand_filename(filename);
		  if (!(mus_file_probe(fullname)))
		    start_unsound_watcher(fd, filename);
		  free(fullname);
		}
	    }
	  else 
	    {
	      status_report(sp, "%s inserted at cursor", filename);
	    }
	}
      else 
	{
	  char *str;
	  str = mus_format("%s is a directory", filename);
	  file_open_error(str, fd);
	  free(str);
	}
    }
}

  
widget_t make_insert_file_dialog(bool managed)
{
  if (!idat)
    idat = make_file_dialog(FILE_READ_ONLY, "Insert", "insert:", "Insert", FILE_INSERT_DIALOG,
			    (GCallback)file_insert_ok_callback,
			    NULL, /* no mkdir */
			    (GCallback)file_insert_delete_callback,
			    (GCallback)file_insert_cancel_callback,
			    (GCallback)file_insert_help_callback,
			    ICON_PASTE);

  if (managed) gtk_widget_show(idat->dialog);
  return(idat->dialog);
}


void set_open_file_play_button(bool val) 
{
}



/* ---------------- file data panel ---------------- */

char *get_file_dialog_sound_attributes(file_data *fdat, int *srate, int *chans, mus_header_t *header_type, 
				       mus_sample_t *sample_type, mus_long_t *location, mus_long_t *samples, int min_chan)
{
  char *str;
  int res;
  fdat->error_widget = NOT_A_SCANF_WIDGET;
  fdat->scanf_widget = NOT_A_SCANF_WIDGET;

  if ((srate) && (fdat->srate_text))
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->srate_text)); 
      fdat->scanf_widget = SRATE_WIDGET;
      if ((str) && (*str))
	(*srate) = string_to_int(str, 1, "srate"); 
      else snd_error_without_format("no srate?");
    }

  if ((chans) && (fdat->chans_text))
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->chans_text)); 
      fdat->scanf_widget = CHANS_WIDGET;
      if ((str) && (*str))
	(*chans) = string_to_int(str, min_chan, "chans"); 
       else
 	{
 	  if (min_chan > 0)
 	    snd_error_without_format("no chans?");
 	}
    }

  if ((location) && (fdat->location_text))
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->location_text)); 
      fdat->scanf_widget = DATA_LOCATION_WIDGET;
      if ((str) && (*str))
	(*location) = string_to_mus_long_t(str, 0, "data location"); 
      else snd_error_without_format("no data location?");
    }

  if ((samples) && (fdat->samples_text))
    {
      str = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->samples_text)); 
      fdat->scanf_widget = SAMPLES_WIDGET;
      if ((str) && (*str))
	(*samples) = string_to_mus_long_t(str, 0, "samples"); 
      else snd_error_without_format("no samples?");
    }
  fdat->scanf_widget = SAMPLES_WIDGET;

  if (fdat->header_type_list)
    {
      res = fdat->header_type_list->selected_item;
      if (res == SLIST_NO_ITEM_SELECTED)
	res = fdat->header_type_pos;
      if (res != NO_SELECTION)
	{
	  (*header_type) = position_to_header_type(res);
	  fdat->current_header_type = (*header_type);
	}
    }

  if (fdat->sample_type_list)
    {
      res = fdat->sample_type_list->selected_item;
      if (res == SLIST_NO_ITEM_SELECTED) /* can this happen? */
	res = fdat->sample_type_pos;
      if (res != NO_SELECTION)
	{
	  (*sample_type) = position_to_sample_type(fdat->current_header_type, res);
	  fdat->current_sample_type = (*sample_type);
	}
    }

  if (fdat->comment_text) 
    {
      char *comment = NULL;
      if (GTK_IS_TEXT_VIEW(fdat->comment_text))
	comment = sg_get_text(fdat->comment_text, 0, -1);
      else comment = (char *)gtk_entry_get_text(GTK_ENTRY(fdat->comment_text)); 
      str = mus_strdup(comment);
      return(str);
    }

  return(NULL);
}


#define IGNORE_DATA_LOCATION -1
#define IGNORE_SAMPLES MUS_UNKNOWN_SAMPLE
#define IGNORE_CHANS -1
#define IGNORE_SRATE -1
#define IGNORE_HEADER_TYPE MUS_UNKNOWN_HEADER

static void set_file_dialog_sound_attributes(file_data *fdat, mus_header_t header_type, mus_sample_t sample_type, 
					     int srate, int chans, mus_long_t location, mus_long_t samples, char *comment)
{

  int i;
  const char **fl = NULL;
  if (!(fdat->sample_type_list)) return;

  if (header_type != IGNORE_HEADER_TYPE)
    fdat->current_header_type = header_type;
  else fdat->current_header_type = MUS_RAW;
  fdat->current_sample_type = sample_type;
  fl = header_type_and_sample_type_to_position(fdat, fdat->current_header_type, fdat->current_sample_type);
  if (!fl) return;

  if ((header_type != IGNORE_HEADER_TYPE) &&
      (fdat->header_type_list))
    {
      slist_select(fdat->header_type_list, fdat->header_type_pos);
      slist_moveto(fdat->header_type_list, fdat->header_type_pos);
    }

  slist_clear(fdat->sample_type_list);
  for (i = 0; i < fdat->sample_types; i++) 
    slist_append(fdat->sample_type_list, (const char *)fl[i]);

  slist_select(fdat->sample_type_list, fdat->sample_type_pos);
  slist_moveto(fdat->sample_type_list, fdat->sample_type_pos);

  if ((srate != IGNORE_SRATE) && 
      (fdat->srate_text))
    widget_int_to_text(fdat->srate_text, srate);

  if ((chans != IGNORE_CHANS) && 
      (fdat->chans_text))
    widget_int_to_text(fdat->chans_text, chans);

  if (fdat->comment_text) 
    {
      if (GTK_IS_TEXT_VIEW(fdat->comment_text))
	{
	  gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(fdat->comment_text)), "", 0);
	  sg_text_insert(fdat->comment_text, comment);
	}
      else gtk_entry_set_text(GTK_ENTRY(fdat->comment_text), comment);
    }

  if ((location != IGNORE_DATA_LOCATION) && 
      (fdat->location_text))
    widget_mus_long_t_to_text(fdat->location_text, location);

  if ((samples != IGNORE_SAMPLES) && 
      (fdat->samples_text))
    widget_mus_long_t_to_text(fdat->samples_text, samples);

}


static gboolean data_panel_srate_key_press(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  if (EVENT_KEYVAL(event) == snd_K_Tab)
    {
      gtk_entry_set_text(GTK_ENTRY(w), srate_completer(w, (char *)gtk_entry_get_text(GTK_ENTRY(w)), NULL));
      gtk_editable_set_position(GTK_EDITABLE(w), mus_strlen((char *)gtk_entry_get_text(GTK_ENTRY(w))));
      return(true);
    }
  return(false);
}


#define NUM_REFLECTION_IDS 5
enum {REFLECT_SRATE_ID, REFLECT_CHANS_ID, REFLECT_SAMPLES_ID, REFLECT_LOCATION_ID, REFLECT_COMMENT_ID};

static void reflect_file_data_panel_change(file_data *fd, void *data, void (*change_action)(GtkWidget *w, gpointer context))
{
  if (!(fd->reflection_ids))
    fd->reflection_ids = (gulong *)calloc(NUM_REFLECTION_IDS, sizeof(gulong));
  if (fd->srate_text)
    fd->reflection_ids[REFLECT_SRATE_ID] = SG_SIGNAL_CONNECT(fd->srate_text, "changed", change_action, (gpointer)data);
  if (fd->chans_text)
    fd->reflection_ids[REFLECT_CHANS_ID] = SG_SIGNAL_CONNECT(fd->chans_text, "changed", change_action, (gpointer)data);
  if (fd->samples_text)
    fd->reflection_ids[REFLECT_SAMPLES_ID] = SG_SIGNAL_CONNECT(fd->samples_text, "changed", change_action, (gpointer)data);
  if (fd->location_text)
    fd->reflection_ids[REFLECT_LOCATION_ID] = SG_SIGNAL_CONNECT(fd->location_text, "changed", change_action, (gpointer)data);
  if (fd->comment_text)
    {
      if (GTK_IS_TEXT_VIEW(fd->comment_text))
	fd->reflection_ids[REFLECT_COMMENT_ID] = 
          SG_SIGNAL_CONNECT(gtk_text_view_get_buffer(GTK_TEXT_VIEW(fd->comment_text)), "changed", change_action, (gpointer)data);
      else fd->reflection_ids[REFLECT_COMMENT_ID] = SG_SIGNAL_CONNECT(fd->comment_text, "changed", change_action, (gpointer)data);
    }
}


static void unreflect_file_data_panel_change(file_data *fd, void *data, void (*change_action)(GtkWidget *w, gpointer context))
{
  int i;
  if (!(fd->reflection_ids)) return;
  if ((fd->srate_text) && (fd->reflection_ids[REFLECT_SRATE_ID] > 0))
    g_signal_handler_disconnect(fd->srate_text, fd->reflection_ids[REFLECT_SRATE_ID]);
  if ((fd->chans_text) && (fd->reflection_ids[REFLECT_CHANS_ID] > 0))
    g_signal_handler_disconnect(fd->chans_text, fd->reflection_ids[REFLECT_CHANS_ID]);
  if ((fd->samples_text) && (fd->reflection_ids[REFLECT_SAMPLES_ID] > 0))
    g_signal_handler_disconnect(fd->samples_text, fd->reflection_ids[REFLECT_SAMPLES_ID]);
  if ((fd->location_text) && (fd->reflection_ids[REFLECT_LOCATION_ID] > 0))
    g_signal_handler_disconnect(fd->location_text, fd->reflection_ids[REFLECT_LOCATION_ID]);
  if ((fd->comment_text) && (fd->reflection_ids[REFLECT_COMMENT_ID] > 0))
    {
      if (GTK_IS_TEXT_VIEW(fd->comment_text))
	g_signal_handler_disconnect(gtk_text_view_get_buffer(GTK_TEXT_VIEW(fd->comment_text)), fd->reflection_ids[REFLECT_COMMENT_ID]);
      else g_signal_handler_disconnect(fd->comment_text, fd->reflection_ids[REFLECT_COMMENT_ID]);
    }
  for (i = 0; i < NUM_REFLECTION_IDS; i++) fd->reflection_ids[i] = 0;
}


/* -------- panel error handling -------- */

/* if an error occurs, a callback is added to the offending text widget, and an error is
 *   posted in the error_text label.  When the user modifies the bad entry, the callback
 *   erases the error message, and removes itself from the text widget.
 */

static void clear_dialog_error(file_data *fdat)
{
  gtk_label_set_text(GTK_LABEL(fdat->error_text), "");
}


static void post_file_dialog_error(const char *error_msg, file_data *fdat)
{
  gtk_label_set_text(GTK_LABEL(fdat->error_text), (gchar *)error_msg);
}


static void redirect_post_file_dialog_error(const char *error_msg, void *ufd)
{
  file_data *fdat = (file_data *)ufd;
  gtk_label_set_text(GTK_LABEL(fdat->error_text), error_msg);
}


static gulong chans_key_press_handler_id = 0;

static gboolean chans_key_press_callback(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  file_data *fdat = (file_data *)data;
  gtk_label_set_text(GTK_LABEL(fdat->error_text), "");
  if (chans_key_press_handler_id)
    {
      g_signal_handler_disconnect(fdat->chans_text, chans_key_press_handler_id);
      chans_key_press_handler_id = 0;
    }
  return(false);
}


static void clear_error_if_chans_changes(GtkWidget *dialog, void *data)
{
  file_data *fdat = (file_data *)data;
  if (fdat->chans_text) 
    chans_key_press_handler_id = SG_SIGNAL_CONNECT(fdat->chans_text, "key_press_event", chans_key_press_callback, data);
}


static gulong panel_modify_handler_id = 0;

static gboolean panel_modify_callback(GtkWidget *w, GdkEventKey *event, gpointer data)
{
  file_data *fdat = (file_data *)data;
  gtk_label_set_text(GTK_LABEL(fdat->error_text), "");
  if (panel_modify_handler_id)
    {
      g_signal_handler_disconnect(w, panel_modify_handler_id);
      panel_modify_handler_id = 0;
    }
  return(false);
}


static void clear_error_if_panel_changes(GtkWidget *dialog, file_data *fdat)
{
  GtkWidget *baddy;
  switch (fdat->error_widget)
    {
    case SRATE_WIDGET:         baddy = fdat->srate_text;    break;
    case DATA_LOCATION_WIDGET: baddy = fdat->location_text; break;
    case SAMPLES_WIDGET:       baddy = fdat->samples_text;  break;
    default:                   baddy = fdat->chans_text;    break;
    }
  if (baddy) 
    panel_modify_handler_id = SG_SIGNAL_CONNECT(baddy, "key_press_event", panel_modify_callback, (void *)fdat);
}


static void post_file_panel_error(const char *error_msg, file_data *fdat)
{
  fdat->error_widget = fdat->scanf_widget;
  post_file_dialog_error(error_msg, fdat);
}


static void redirect_post_file_panel_error(const char *error_msg, void *ufd)
{
  post_file_panel_error(error_msg, (file_data *)ufd);
}


/* -------- file data choices -------- */

static void update_header_type_list(const char *name, int row, void *data)
{
  /* needed to reflect type selection in sample_type list */
  file_data *fdat = (file_data *)data;
  if (position_to_header_type(row) != fdat->current_header_type)
    {
      position_to_header_type_and_sample_type(fdat, row);
      set_file_dialog_sound_attributes(fdat,
				       fdat->current_header_type,
				       fdat->current_sample_type,
				       IGNORE_SRATE, IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES, 
				       NULL);
    }
}


static void update_sample_type_list(const char *name, int row, void *data)
{
  file_data *fdat = (file_data *)data;
  fdat->current_sample_type = position_to_sample_type(fdat->current_header_type, row);
}


static void file_data_src_callback(GtkWidget *w, gpointer context)
{
  file_data *fdat = (file_data *)context;
  fdat->src = (bool)(TOGGLE_BUTTON_ACTIVE(w));
}

#define WITH_SRATE_FIELD true
#define WITHOUT_SRATE_FIELD false
#define WITHOUT_AUTO_COMMENT false


static file_data *make_file_data_panel(GtkWidget *parent, const char *name, 
				       dialog_channels_t with_chan, 
				       mus_header_t header_type, 
				       mus_sample_t sample_type,
				       dialog_data_location_t with_loc, 
				       dialog_samples_t with_samples,
				       dialog_header_type_t with_header_type,
				       dialog_comment_t with_comment, 
				       header_choice_t header_choice,
				       bool with_src, 
				       bool with_auto_comment)
{
  GtkWidget *form, *scbox, *frame_box, *frame;
  file_data *fdat;
  int nsample_types = 0, nheaders = 0;
  const char **sample_types = NULL, **headers = NULL;

  switch (header_choice)
    {
    case WITH_READABLE_HEADERS: headers = short_readable_headers(&nheaders); break;
    case WITH_WRITABLE_HEADERS: headers = short_writable_headers(&nheaders); break;
    case WITH_BUILTIN_HEADERS:  headers = short_builtin_headers(&nheaders);  break;
    }

  fdat = (file_data *)calloc(1, sizeof(file_data));
  fdat->src = save_as_dialog_src(ss);
  fdat->auto_comment = save_as_dialog_auto_comment(ss);
  fdat->saved_comment = NULL;
  fdat->current_header_type = header_type;
  fdat->current_sample_type = sample_type;
  sample_types = header_type_and_sample_type_to_position(fdat, header_type, sample_type);
  nsample_types = fdat->sample_types;

  frame = gtk_frame_new(NULL);
  sg_box_pack_start(GTK_BOX(parent), frame, false, true, 8);
  gtk_widget_show(frame);

  frame_box = gtk_vbox_new(false, 0);
  gtk_container_add(GTK_CONTAINER(frame), frame_box);
  gtk_widget_show(frame_box);

  form = gtk_hbox_new(true, 8);
  sg_box_pack_start(GTK_BOX(frame_box), form, false, false, 4);
  gtk_widget_show(form);

  /* header type */
  if (with_header_type == WITH_HEADER_TYPE_FIELD)
    {
      fdat->header_type_list = slist_new_with_title("header type", form, (const char **)headers, nheaders, BOX_PACK); /* BOX_PACK widget_add_t (snd-g0.h) */
      fdat->header_type_list->select_callback = update_header_type_list;
      fdat->header_type_list->select_callback_data = (void *)fdat;
      slist_select(fdat->header_type_list, fdat->header_type_pos);
    }

  /* sample type */ 
#if GTK_CHECK_VERSION(3, 0, 0)
  fdat->sample_type_list = slist_new_with_title("    sample type    ", form, (const char **)sample_types, nsample_types, BOX_PACK);
#else
  fdat->sample_type_list = slist_new_with_title("sample type", form, (const char **)sample_types, nsample_types, BOX_PACK);
#endif
  fdat->sample_type_list->select_callback = update_sample_type_list;
  fdat->sample_type_list->select_callback_data = (void *)fdat;
  slist_select(fdat->sample_type_list, fdat->sample_type_pos);

  scbox = gtk_vbox_new(false, 0);
  sg_box_pack_start(GTK_BOX(form), scbox, false, false, 4);
  gtk_widget_show(scbox);

  /* srate */
  {
    GtkWidget *srate_label;
    srate_label = snd_gtk_highlight_label_new("srate");
    sg_box_pack_start(GTK_BOX(scbox), srate_label, false, false, 0);
    gtk_widget_show(srate_label);

    if (with_src)
      {
	GtkWidget *src_box;
	src_box = gtk_hbox_new(false, 0);
	sg_box_pack_start(GTK_BOX(scbox), src_box, false, false, 0);
	gtk_widget_show(src_box);
	
	fdat->srate_text = snd_entry_new(src_box, NULL, WITH_WHITE_BACKGROUND);
	gtk_entry_set_width_chars(GTK_ENTRY(fdat->srate_text), 8);
	SG_SIGNAL_CONNECT(fdat->srate_text, "key_press_event", data_panel_srate_key_press, NULL); /* srate completer */
	
	fdat->src_button = gtk_check_button_new_with_label("src");
	sg_box_pack_end(GTK_BOX(src_box), fdat->src_button, false, false, 4);
	SG_SIGNAL_CONNECT(fdat->src_button, "toggled", file_data_src_callback, fdat);
	gtk_widget_show(fdat->src_button);
	set_toggle_button(fdat->src_button, fdat->src, false, (void *)fdat);
      }
    else
      {
	fdat->srate_text = snd_entry_new(scbox, NULL, WITH_WHITE_BACKGROUND);
	SG_SIGNAL_CONNECT(fdat->srate_text, "key_press_event", data_panel_srate_key_press, NULL); /* srate completer */
      }
  }

  if (with_samples != WITH_SAMPLES_FIELD)
    {
      GtkWidget *spacer;
      spacer = gtk_vseparator_new();
      sg_box_pack_start(GTK_BOX(scbox), spacer, false, false, 12);
      gtk_widget_show(spacer);
    }

  /* chans */
  if (with_chan != WITHOUT_CHANNELS_FIELD)
    {
      GtkWidget *chans_label;
      chans_label = snd_gtk_highlight_label_new((with_chan == WITH_CHANNELS_FIELD) ? "channels" : "extract channel");
      sg_box_pack_start(GTK_BOX(scbox), chans_label, false, false, 0);
      gtk_widget_show(chans_label);

      fdat->chans_text = snd_entry_new(scbox, NULL, WITH_WHITE_BACKGROUND);
      
      if (with_loc == WITH_DATA_LOCATION_FIELD)
	{
	  GtkWidget *loclab;
	  loclab = snd_gtk_highlight_label_new("location");
	  sg_box_pack_start(GTK_BOX(scbox), loclab, false, false, 0);
	  gtk_widget_show(loclab);

	  fdat->location_text = snd_entry_new(scbox, NULL, WITH_WHITE_BACKGROUND);
	}
    }

  /* samples */
  if (with_samples == WITH_SAMPLES_FIELD)
    {
      GtkWidget *samplab;
      samplab = snd_gtk_highlight_label_new("samples");
      sg_box_pack_start(GTK_BOX(scbox), samplab, false, false, 0);
      gtk_widget_show(samplab);

      fdat->samples_text = snd_entry_new(scbox, NULL, WITH_WHITE_BACKGROUND);
    }
  else
    {
      /* need a spacer to force the lists to have room */
      GtkWidget *spacer;
      spacer = gtk_vseparator_new();
      sg_box_pack_start(GTK_BOX(scbox), spacer, false, false, 12);
      gtk_widget_show(spacer);
    }

  /* comment */
  if (with_comment != WITHOUT_COMMENT_FIELD)
    {
      GtkWidget *frame, *comment_label;
      GtkWidget *w1, *combox;

      w1 = gtk_vseparator_new();
      sg_box_pack_start(GTK_BOX(frame_box), w1, false, false, 4);
      gtk_widget_show(w1);

      combox = gtk_hbox_new(false, 0);
      sg_box_pack_start(GTK_BOX(frame_box), combox, false, true, 4);
      gtk_widget_show(combox);

      if (with_auto_comment)
	{
	  GtkWidget *cbox;
	  cbox = gtk_vbox_new(false, 0);
	  sg_box_pack_start(GTK_BOX(combox), cbox, false, false, 0);
	  gtk_widget_show(cbox);
	  
	  comment_label = snd_gtk_highlight_label_new("comment");
	  sg_box_pack_start(GTK_BOX(cbox), comment_label, false, false, 0);
	  gtk_widget_show(comment_label);
	  
	  fdat->auto_comment_button = gtk_check_button_new_with_label("auto");
	  sg_box_pack_end(GTK_BOX(cbox), fdat->auto_comment_button, false, false, 4);
	  gtk_widget_show(fdat->auto_comment_button);
	  set_toggle_button(fdat->auto_comment_button, fdat->auto_comment, false, (void *)fdat);
	}
      else
	{
	  comment_label = snd_gtk_highlight_label_new("comment");
	  sg_box_pack_start(GTK_BOX(combox), comment_label, false, false, 0);
	  gtk_widget_show(comment_label);
	}

      frame = gtk_frame_new(NULL);
      sg_box_pack_start(GTK_BOX(combox), frame, true, true, 4);  
      gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);
      gtk_widget_show(frame);
      fdat->comment_text = make_scrolled_text(frame, true, CONTAINER_ADD, false); /* this returns a text_view widget */
      connect_mouse_to_text(fdat->comment_text);
    }

  /* error */
  fdat->error_text = gtk_label_new(NULL);
  sg_box_pack_end(GTK_BOX(parent), fdat->error_text, false, false, 0);
  gtk_widget_show(fdat->error_text);

  return(fdat);
}


/* -------- save as dialog (file and edit menus) -------- */

static file_dialog_info *save_sound_as = NULL, *save_selection_as = NULL, *save_region_as = NULL;


void reflect_save_as_sound_selection(const char *sound_name)
{
  if (save_sound_as)
    {
      char *file_string;
      if (sound_name)
	file_string = mus_format("save %s", sound_name);
      else
	{
	  snd_info *sp;
	  sp = any_selected_sound();
	  if (sp)
	    file_string = mus_format("save %s", sp->short_filename);
	  else file_string = mus_strdup("nothing to save!");
	}
      gtk_window_set_title(GTK_WINDOW(save_sound_as->dialog), file_string);
      free(file_string);
    }
}


void reflect_save_as_src(bool val)
{
  if (save_sound_as)
    {
      set_toggle_button(save_sound_as->panel_data->src_button, val, false, (void *)save_sound_as);
      save_sound_as->panel_data->src = val;
    }
  if (save_selection_as)
    {
      set_toggle_button(save_selection_as->panel_data->src_button, val, false, (void *)save_selection_as);
      save_selection_as->panel_data->src = val;
    }
  if (save_region_as)
    {
      set_toggle_button(save_region_as->panel_data->src_button, val, false, (void *)save_region_as);
      save_region_as->panel_data->src = val;
    }
}


void reflect_save_as_auto_comment(bool val)
{
  if (save_sound_as)
    {
      set_toggle_button(save_sound_as->panel_data->auto_comment_button, val, false, (void *)save_sound_as);
      save_sound_as->panel_data->auto_comment = val;
    }
}


static void make_auto_comment(file_dialog_info *fd)
{
  if ((fd == save_sound_as) &&
      (widget_is_active(fd->dialog)))
    {
      file_data *fdat;
      fdat = fd->panel_data;

      if (!(fdat->auto_comment))
	{
	  /* don't erase typed-in comment, if any */
	  sg_text_insert(fdat->comment_text, fdat->saved_comment);
	}
      else
	{
	  snd_info *sp;
	  bool edits = false;
	  int i;
	  char *original_sound_comment, *comment, *orig_comment = NULL;

	  sp = any_selected_sound();

	  original_sound_comment = mus_sound_comment(sp->filename);
	  if (original_sound_comment)
	    {
	      if (*original_sound_comment)
		orig_comment = mus_format("\n%s comment:\n%s\n", sp->short_filename, original_sound_comment);
	      free(original_sound_comment);
	      original_sound_comment = NULL;
	    }

	  fdat->saved_comment = sg_get_text(fdat->comment_text, 0, -1);
	  if ((fdat->saved_comment) &&
	      (!(*(fdat->saved_comment))))
	    fdat->saved_comment = NULL;

	  for (i = 0; i < (int)sp->nchans; i++)
	    if (sp->chans[i]->edit_ctr != 0)
	      {
		edits = true;
		break;
	      }

	  if (!edits)
	    comment = mus_format("%ssaved %s from %s (no edits)\n%s", 
				 (fdat->saved_comment) ? "\n" : "",
				 snd_local_time(),
				 sp->filename,
				 (orig_comment) ? orig_comment : "");
	  else 
	    {
	      int len;
	      char **edit_strs;
	      char *time;
	  
	      time = snd_local_time();
	      len = 2 * mus_strlen(sp->filename) + 
		    mus_strlen(time) + 
		    32 * sp->nchans + 
		    mus_strlen(fdat->saved_comment) + 
		    mus_strlen(original_sound_comment);

	      edit_strs = (char **)malloc(sp->nchans * sizeof(char *));
	      for (i = 0; i < (int)sp->nchans; i++)
		{
		  edit_strs[i] = edit_list_to_function(sp->chans[i], 1, sp->chans[i]->edit_ctr);
		  len += mus_strlen(edit_strs[i]);
		}

	      comment = (char *)calloc(len, sizeof(char));
	      snprintf(comment, len, "%ssaved %s from %s with edits:\n", 
			   (fdat->saved_comment) ? "\n" : "",
			   snd_local_time(),
			   sp->filename);
	      
	      for (i = 0; i < (int)sp->nchans; i++)
		{
		  if (sp->nchans > 1)
		    {
		      char buf[64];
		      snprintf(buf, 64, "\n-------- channel %d --------\n", i);
		      strcat(comment, buf);
		    }
		  strcat(comment, edit_strs[i]);
		}

	      if (orig_comment)
		strcat(comment, orig_comment);
	      free(edit_strs);
	    }

	  sg_text_insert(fdat->comment_text, comment);
	  if (comment) free(comment);
	  if (orig_comment) free(orig_comment);
	}
    }
}


static void file_data_auto_comment_callback(GtkWidget *w, gpointer context)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  fd->panel_data->auto_comment = (bool)(TOGGLE_BUTTON_ACTIVE(w));
  make_auto_comment(fd);
}


void reflect_selection_in_save_as_dialog(bool on)
{
  if ((on) && 
      (save_selection_as) &&
      (save_selection_as->panel_data))
    clear_dialog_error(save_selection_as->panel_data);
}


void reflect_region_in_save_as_dialog(void)
{
  if ((save_region_as) &&
      (save_region_as->dialog) &&
      (widget_is_active(save_region_as->dialog)) &&
      (region_ok(region_dialog_region())))
    clear_dialog_error(save_region_as->panel_data);
}


static void save_as_undoit(file_dialog_info *fd)
{
  set_stock_button_label(fd->ok_button, "Save");
  clear_dialog_error(fd->panel_data);
  fd->file_watcher = unmonitor_file(fd->file_watcher);
}




#if HAVE_G_FILE_MONITOR_DIRECTORY
static void watch_save_as_file(GFileMonitor *mon, GFile *file, GFile *other, GFileMonitorEvent ev, gpointer data)
{
  /* if file is deleted, respond in some debonair manner */
  switch (ev)
    {
    case G_FILE_MONITOR_EVENT_CHANGED:
    case G_FILE_MONITOR_EVENT_DELETED:
    case G_FILE_MONITOR_EVENT_CREATED:
#if HAVE_GTK_WIDGET_GET_VISIBLE
    case G_FILE_MONITOR_EVENT_MOVED:
#endif
      save_as_undoit((file_dialog_info *)data);
      break;

    default:
      /* ignore the rest */
      break;
    }
}
#endif


static bool srates_differ(int srate, file_dialog_info *fd)
{
  switch (fd->type)
    {
    case SOUND_SAVE_AS:
      return(snd_srate(any_selected_sound()) != srate);
      
    case SELECTION_SAVE_AS:
      return(selection_srate() != srate);
      
    case REGION_SAVE_AS:
      return(region_srate(region_dialog_region()) != srate);
    }

  return(false);
}


static double srate_ratio(int srate, file_dialog_info *fd)
{
  switch (fd->type)
    {
    case SOUND_SAVE_AS:
      return((double)(snd_srate(any_selected_sound())) / (double)srate);
      
    case SELECTION_SAVE_AS:
      return((double)selection_srate() / (double)srate);
      
    case REGION_SAVE_AS:
      return((double)region_srate(region_dialog_region()) / (double)srate);
    }

  return(1.0);
}


static void save_or_extract(file_dialog_info *fd, bool saving)
{
  char *str = NULL, *comment = NULL, *msg = NULL, *fullname = NULL, *tmpfile = NULL;
  snd_info *sp = NULL;
  mus_header_t header_type = MUS_NEXT, output_type;
  mus_sample_t sample_type = DEFAULT_OUTPUT_SAMPLE_TYPE;
  int srate = DEFAULT_OUTPUT_SRATE, chans = DEFAULT_OUTPUT_CHANS;
  int chan = 0, extractable_chans = 0;
  bool file_exists = false;
  mus_long_t location = 28, samples = 0;
  io_error_t io_err = IO_NO_ERROR;

  clear_dialog_error(fd->panel_data);

  if ((fd->type == SELECTION_SAVE_AS) &&
      (!(selection_is_active())))
    {
      if (saving)
	msg = (char *)"no selection to save";
      else msg = (char *)"can't extract: no selection";
      post_file_dialog_error((const char *)msg, fd->panel_data);
      return;
    }

  if ((fd->type == REGION_SAVE_AS) &&
      (!(region_ok(region_dialog_region()))))
    {
      post_file_dialog_error("no region to save", fd->panel_data);
      return;
    }

  sp = any_selected_sound();
  if ((!sp) &&
      (fd->type != REGION_SAVE_AS))
    {
      if (saving)
	msg = (char *)"nothing to save";
      else msg = (char *)"nothing to extract";
      post_file_dialog_error((const char *)msg, fd->panel_data);
      return;
    }

  /* get output filename */
  str = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(fd->chooser));
  if ((!str) || (!*str))
    {
      if (saving)
	msg = (char *)"can't save: no file name given";
      else msg = (char *)"can't extract: no file name given";
      post_file_dialog_error((const char *)msg, fd->panel_data);
      return;
    }
  if (is_directory(str))
    {
      post_file_dialog_error("can't overwrite a directory", fd->panel_data);
      return;
    }

  /* get output file attributes */
  redirect_snd_error_to(redirect_post_file_panel_error, (void *)(fd->panel_data));
  if (saving)
    comment = get_file_dialog_sound_attributes(fd->panel_data, &srate, &chans, &header_type, &sample_type, &location, &samples, 0);
  else comment = get_file_dialog_sound_attributes(fd->panel_data, &srate, &chan, &header_type, &sample_type, &location, &samples, 0);
  output_type = header_type;
  redirect_snd_error_to(NULL, NULL);
  if (fd->panel_data->error_widget != NOT_A_SCANF_WIDGET)
    {
      clear_error_if_panel_changes(fd->dialog, fd->panel_data);
      if (comment) free(comment);
      return;
    }

  switch (fd->type)
    {
    case SOUND_SAVE_AS:
      clear_status_area(sp);
      if (!saving)
	extractable_chans = sp->nchans;
      break;

    case SELECTION_SAVE_AS:
      if (!saving)
	extractable_chans = selection_chans();
      break;

    default:
      break;
    }

  if (!saving)
    {
      if ((chan > extractable_chans) ||
	  (((extractable_chans > 1) && (chan == extractable_chans)) ||
	   (chan < 0)))
	{
	  if (chan > extractable_chans)
	    msg = mus_format("can't extract chan %d (%s has %d chan%s)", 
			     chan, 
			     (fd->type == SOUND_SAVE_AS) ? "sound" : "selection",
			     extractable_chans, 
			     (extractable_chans > 1) ? "s" : "");
	  else msg = mus_format("can't extract chan %d (first chan is numbered 0)", chan);
	  post_file_dialog_error((const char *)msg, fd->panel_data);
	  clear_error_if_chans_changes(fd->dialog, (void *)(fd->panel_data));
	  free(msg);
	  if (comment) free(comment);
	  return;
	}
    }

  fullname = mus_expand_filename(str);
  if (run_before_save_as_hook(sp, fullname, fd->type != SOUND_SAVE_AS, srate, sample_type, header_type, comment))
    {
      msg = mus_format("%s cancelled by %s", (saving) ? "save" : "extract", S_before_save_as_hook);
      post_file_dialog_error((const char *)msg, fd->panel_data);
      free(msg);
      free(fullname);
      if (comment) free(comment);
      return;
    }

  file_exists = mus_file_probe(fullname);
  if ((fd->type == SOUND_SAVE_AS) &&
      (mus_strcmp(fullname, sp->filename)))
    {
      /* save-as here is the same as save */
      if ((sp->user_read_only == FILE_READ_ONLY) || 
	  (sp->file_read_only == FILE_READ_ONLY))
	{
	  msg = mus_format("can't overwrite %s (it is write-protected)", sp->short_filename);
	  post_file_dialog_error((const char *)msg, fd->panel_data);
	  free(msg);
	  free(fullname);
	  if (comment) free(comment);
	  return;
	}
    }
  else
    {
      if (!(fd->file_watcher))
	{
	  /* check for overwrites that are questionable -- DoIt click will return here with fd->file_watcher active */
	  snd_info *parlous_sp = NULL;
	  if ((file_exists) &&
	      ((ask_before_overwrite(ss)) ||
	       ((fd->type == SOUND_SAVE_AS) &&
		(parlous_sp = file_is_open_elsewhere_and_has_unsaved_edits(sp, fullname)))))	   
	    {
	      msg = mus_format("%s exists%s. To overwrite it, click '%s'", 
			       str,
			       (parlous_sp) ? ", and has unsaved edits" : "",
			       "DoIt"
			       );
#if HAVE_G_FILE_MONITOR_DIRECTORY
	      {
		GFile *file;
		GError *err = NULL;
		file = g_file_new_for_path(fullname);
		fd->file_watcher = (void *)g_file_monitor_file(file, G_FILE_MONITOR_NONE, NULL, &err);
		if (err)
		  snd_warning("%s", err->message);
		else g_signal_connect(G_OBJECT(fd->file_watcher), "changed", G_CALLBACK(watch_save_as_file), (gpointer)fd);
		g_object_unref(file);
	      }
#endif
	      post_file_dialog_error((const char *)msg, fd->panel_data);
	      set_stock_button_label(fd->ok_button, "DoIt");
	      free(msg);
	      free(fullname);
	      if (comment) free(comment);
	      return;
	    }
	}
    }

  /* try to save... if it exists already, first write as temp, then move */

  if (fd->file_watcher)
    save_as_undoit(fd);
  ss->local_errno = 0;

  if (header_is_encoded(header_type))
    {
      output_type = header_type;
      sample_type = MUS_LSHORT;
      header_type = MUS_RIFF;
      tmpfile = snd_tempnam();
    }
  else
    {
      tmpfile = fullname;
    }

  redirect_snd_error_to(redirect_post_file_dialog_error, (void *)(fd->panel_data));
  switch (fd->type)
    {
    case SOUND_SAVE_AS:
      if (saving)
	io_err = save_edits_without_display(sp, tmpfile, header_type, sample_type, srate, comment, AT_CURRENT_EDIT_POSITION);
      else io_err = save_channel_edits(sp->chans[chan], tmpfile, AT_CURRENT_EDIT_POSITION); /* protects if same name */
      break;

    case SELECTION_SAVE_AS:
      {
	char *ofile;
	if (file_exists) /* file won't exist if we're encoding, so this isn't as wasteful as it looks */
	  ofile = snd_tempnam();
	else ofile = mus_strdup(tmpfile);
	io_err = save_selection(ofile, srate, sample_type, header_type, comment, (saving) ? SAVE_ALL_CHANS : chan);
	if (io_err == IO_NO_ERROR)
	  io_err = move_file(ofile, fullname);
	free(ofile);
	break;
      }

    case REGION_SAVE_AS:
      {
	if (region_ok(region_dialog_region()))
	  {
	    char *ofile;
	    if (file_exists)
	      ofile = snd_tempnam();
	    else ofile = mus_strdup(tmpfile);
	    io_err = save_region(region_dialog_region(), ofile, sample_type, header_type, comment);
	    if (io_err == IO_NO_ERROR)
	      io_err = move_file(ofile, fullname);
	    free(ofile);
	  }
      }
	break;
    }
  redirect_snd_error_to(NULL, NULL);

  /* check for possible srate conversion */
  if ((fd->panel_data->src) &&
      (srates_differ(srate, fd)))
    {
      /* if src, and srates differ, do the sampling rate conversion.
       *    this needs to happen before the snd_encode (->OGG etc) below
       *    if we do it before the save-as above, then undo it later, it messes up the user's edit history list
       *    so do it here to tmpfile (tmpfile is fullname unless we're doing a translation to something like OGG)
       */
      src_file(tmpfile, srate_ratio(srate, fd));
    }

  if (io_err == IO_NO_ERROR)
    {
      if (header_is_encoded(output_type))
	{
	  snd_encode(output_type, tmpfile, fullname);
	  snd_remove(tmpfile, REMOVE_FROM_CACHE);
	  free(tmpfile);
	}

      if (saving)
	{
	  if (fd->type == SOUND_SAVE_AS)
	    status_report(sp, "%s saved as %s", sp->short_filename, str);
	  else status_report(sp, "%s saved as %s", (fd->type == SELECTION_SAVE_AS) ? "selection" : "region", str);
	}
      else
	{
	  if (fd->type == SOUND_SAVE_AS)
	    status_report(sp, "%s chan %d saved as %s", sp->short_filename, chan, str);
	  else status_report(sp, "selection chan %d saved as %s", chan, str);
	}
      run_after_save_as_hook(sp, str, true); /* true => from dialog */
      gtk_widget_hide(fd->dialog);
    }
  else
    {
      msg = mus_format("%s as %s: %s (%s)", (saving) ? "save" : "extract chan", str, io_error_name(io_err), snd_io_strerror());
      post_file_dialog_error((const char *)msg, fd->panel_data);
      free(msg);
    }
  free(fullname);
  if (comment) free(comment);
}


static void save_as_ok_callback(GtkWidget *w, gpointer data)
{ 
  save_or_extract((file_dialog_info *)data, true);
}


static void save_as_extract_callback(GtkWidget *w, gpointer data)
{
  save_or_extract((file_dialog_info *)data, false);
}


static void save_as_cancel_callback(GtkWidget *w, gpointer data)
{ 
  file_dialog_info *fd = (file_dialog_info *)data;
  gtk_widget_hide(fd->dialog);
} 


static void save_as_help_callback(GtkWidget *w, gpointer data)
{
  save_as_dialog_help();
}


static gint save_as_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  file_dialog_info *fd = (file_dialog_info *)context;
  gtk_widget_hide(fd->dialog);
  return(true);
}


static file_dialog_info *make_save_as_dialog(const char *file_string, mus_header_t header_type, mus_sample_t sample_type, int dialog_type)
{
  file_dialog_info *fd;
  GtkWidget *vbox;

  fd = make_fsb(file_string, "save as:", "Save as", ICON_SAVE_AS, (dialog_type != REGION_SAVE_AS), true);
  fd->type = (save_dialog_t)dialog_type;
  fd->header_type = header_type;
  fd->sample_type = sample_type;

  vbox = DIALOG_CONTENT_AREA(fd->dialog);
  fd->panel_data = make_file_data_panel(vbox, "data-form", 
					(fd->type == REGION_SAVE_AS) ? WITHOUT_CHANNELS_FIELD : WITH_EXTRACT_CHANNELS_FIELD, 
					fd->header_type, fd->sample_type, 
					WITHOUT_DATA_LOCATION_FIELD, 
					WITHOUT_SAMPLES_FIELD,
					WITH_HEADER_TYPE_FIELD, 
					WITH_COMMENT_FIELD,
					WITH_WRITABLE_HEADERS,
					WITH_SRATE_FIELD,
					fd->type == SOUND_SAVE_AS);

  gtk_widget_show(fd->dialog);
  
  if (fd->type != REGION_SAVE_AS)
    SG_SIGNAL_CONNECT(fd->extract_button, "clicked", save_as_extract_callback, (void *)fd);
  
  SG_SIGNAL_CONNECT(fd->help_button, "clicked", save_as_help_callback, (gpointer)fd);
  SG_SIGNAL_CONNECT(fd->ok_button, "clicked", save_as_ok_callback, (gpointer)fd);
  SG_SIGNAL_CONNECT(fd->cancel_button, "clicked", save_as_cancel_callback, (gpointer)fd);
  
  fd->panel_data->dialog = fd->dialog;
  switch (fd->type)
    {
    case SOUND_SAVE_AS:
      set_dialog_widget(SOUND_SAVE_AS_DIALOG, fd->dialog);
      break;
      
    case SELECTION_SAVE_AS:
      set_dialog_widget(SELECTION_SAVE_AS_DIALOG, fd->dialog);
      break;
      
    case REGION_SAVE_AS:
      set_dialog_widget(REGION_SAVE_AS_DIALOG, fd->dialog);
      break;
      
    default:
      snd_error("internal screw up");
      break;
    }
  SG_SIGNAL_CONNECT(fd->dialog, "delete_event", save_as_delete_callback, (void *)fd);
  
  if (fd->type != REGION_SAVE_AS)
    {
      if (fd->type == SOUND_SAVE_AS)
	SG_SIGNAL_CONNECT(fd->panel_data->auto_comment_button, "toggled", file_data_auto_comment_callback, fd);
    }
  return(fd);
}


static file_dialog_info *make_sound_save_as_dialog_1(bool managed, int chan)
{
  snd_info *sp;
  char *com = NULL;
  file_info *hdr = NULL;
  file_dialog_info *fd;
  char *file_string;

  sp = any_selected_sound();
  if (sp) hdr = sp->hdr;
  file_string = mus_format("save %s", (char *)((sp) ? sp->short_filename : ""));

  if (!save_sound_as)
    save_sound_as = make_save_as_dialog(file_string, default_output_header_type(ss), default_output_sample_type(ss), SOUND_SAVE_AS);
  else gtk_window_set_title(GTK_WINDOW(save_sound_as->dialog), file_string); 
  free(file_string);

  fd = save_sound_as;
  set_file_dialog_sound_attributes(fd->panel_data,
				   fd->panel_data->current_header_type,
				   fd->panel_data->current_sample_type,
				   (hdr) ? hdr->srate : selection_srate(), 
				   IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES,
				   com = output_comment(hdr));
  if (com) free(com);
  if (chan >= 0)
    {
      char *chan_str;  
      chan_str = (char *)calloc(8, sizeof(char));
      snprintf(chan_str, 8, "%d", chan);
      gtk_entry_set_text(GTK_ENTRY(fd->panel_data->chans_text), chan_str);
      free(chan_str);
    }
  if (managed) gtk_widget_show(fd->dialog);
  make_auto_comment(fd);
  return(fd);
}


widget_t make_sound_save_as_dialog(bool managed)
{
  file_dialog_info *fd;
  fd = make_sound_save_as_dialog_1(managed, -1);
  return(fd->dialog);
}


void make_channel_extract_dialog(int chan)
{
  make_sound_save_as_dialog_1(true, chan);
}


widget_t make_selection_save_as_dialog(bool managed)
{
  file_dialog_info *fd;

  if (!save_selection_as)
    save_selection_as = make_save_as_dialog("save current selection", default_output_header_type(ss), default_output_sample_type(ss), SELECTION_SAVE_AS);
  else gtk_window_set_title(GTK_WINDOW(save_selection_as->dialog), "save current selection");

  fd = save_selection_as;
  set_file_dialog_sound_attributes(fd->panel_data,
				   fd->panel_data->current_header_type,
				   fd->panel_data->current_sample_type,
				   selection_srate(), 
				   IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES, 
				   NULL);

  if (managed) gtk_widget_show(fd->dialog);
  return(fd->dialog);
}


widget_t make_region_save_as_dialog(bool managed)
{
  file_dialog_info *fd;
  char *comment = NULL;

  if (!save_region_as)
    save_region_as = make_save_as_dialog("save current region", default_output_header_type(ss), default_output_sample_type(ss), REGION_SAVE_AS);
  else gtk_window_set_title(GTK_WINDOW(save_region_as->dialog), "save current region");

  fd = save_region_as;
  comment = region_description(region_dialog_region());
  set_file_dialog_sound_attributes(fd->panel_data,
				   fd->panel_data->current_header_type,
				   fd->panel_data->current_sample_type,
				   region_srate(region_dialog_region()), 
				   IGNORE_CHANS, IGNORE_DATA_LOCATION, IGNORE_SAMPLES, 
				   comment);
  if (comment) free(comment);
  if (managed) gtk_widget_show(fd->dialog);
  return(fd->dialog);
}


void save_file_dialog_state(FILE *fd)
{
  if ((odat) && (widget_is_active(odat->dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_open_file_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", to_proc_name(S_open_file_dialog));
#endif
#if HAVE_FORTH
      fprintf(fd, "#t %s drop\n", S_open_file_dialog);
#endif
    }
  if ((mdat) && (widget_is_active(mdat->dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_mix_file_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", to_proc_name(S_mix_file_dialog));
#endif
#if HAVE_FORTH
      fprintf(fd, "#t %s drop\n", S_mix_file_dialog);
#endif
    }
  if ((idat) && (widget_is_active(idat->dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_insert_file_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", to_proc_name(S_insert_file_dialog));
#endif
#if HAVE_FORTH
      fprintf(fd, "#t %s drop\n", S_insert_file_dialog);
#endif
    }
  if ((save_sound_as) && (widget_is_active(save_sound_as->dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_save_sound_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", to_proc_name(S_save_sound_dialog));
#endif
#if HAVE_FORTH
      fprintf(fd, "#t %s drop\n", S_save_sound_dialog);
#endif
    }
  if ((save_selection_as) && (widget_is_active(save_selection_as->dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_save_selection_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", to_proc_name(S_save_selection_dialog));
#endif
#if HAVE_FORTH
      fprintf(fd, "#t %s drop\n", S_save_selection_dialog);
#endif
    }
  if ((save_region_as) && (widget_is_active(save_region_as->dialog)))
    {
#if HAVE_SCHEME
      fprintf(fd, "(%s #t)\n", S_save_region_dialog);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(true)\n", to_proc_name(S_save_region_dialog));
#endif
#if HAVE_FORTH
      fprintf(fd, "#t %s drop\n", S_save_region_dialog);
#endif
    }
}


/* -------------------------------- Raw Data Dialog -------------------------------- */

typedef struct raw_info {
  GtkWidget *dialog;
  mus_long_t location;
  file_data *rdat;
  read_only_t read_only;
  bool selected;
  char *filename;
  char *help;
  open_requestor_t requestor;
  void *requestor_data;
  GtkWidget *requestor_dialog;
} raw_info;

static int raw_info_size = 0;
static raw_info **raw_infos = NULL;

static raw_info *new_raw_dialog(void)
{
  int loc = -1;
  if (raw_info_size == 0)
    {
      loc = 0;
      raw_info_size = 4;
      raw_infos = (raw_info **)calloc(raw_info_size, sizeof(raw_info *));
    }
  else
    {
      int i;
      for (i = 0; i < raw_info_size; i++)
	if ((!raw_infos[i]) ||
	    (!(widget_is_active(raw_infos[i]->dialog))))
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = raw_info_size;
	  raw_info_size += 4;
	  raw_infos = (raw_info **)realloc(raw_infos, raw_info_size * sizeof(raw_info *));
	  for (i = loc; i < raw_info_size; i++) raw_infos[i] = NULL;
	}
    }
  if (!raw_infos[loc])
    {
      raw_infos[loc] = (raw_info *)calloc(1, sizeof(raw_info));
      raw_infos[loc]->dialog = NULL;
      raw_infos[loc]->filename = NULL;
      raw_infos[loc]->help = NULL;
    }
  raw_infos[loc]->requestor = NO_REQUESTOR;
  raw_infos[loc]->requestor_data = NULL;
  raw_infos[loc]->location = 0;
  return(raw_infos[loc]);
}


static void raw_data_ok_callback(GtkWidget *w, gpointer context)
{
  raw_info *rp = (raw_info *)context;
  int raw_srate = 0, raw_chans = 0;
  mus_sample_t raw_sample_type = MUS_UNKNOWN_SAMPLE;
  redirect_snd_error_to(redirect_post_file_panel_error, (void *)(rp->rdat));
  get_file_dialog_sound_attributes(rp->rdat, &raw_srate, &raw_chans, NULL, &raw_sample_type, &(rp->location), NULL, 1);
  redirect_snd_error_to(NULL, NULL);
  if (rp->rdat->error_widget != NOT_A_SCANF_WIDGET)
    {
      clear_error_if_panel_changes(rp->dialog, rp->rdat);
    }
  else
    {
      mus_header_set_raw_defaults(raw_srate, raw_chans, raw_sample_type);
      mus_sound_override_header(rp->filename, raw_srate, raw_chans, 
				raw_sample_type, MUS_RAW, rp->location,
				mus_bytes_to_samples(raw_sample_type, 
						     mus_sound_length(rp->filename) - rp->location));
      /* choose action based on how we got here */
      if ((rp->requestor_dialog) &&
	  ((rp->requestor == FROM_MIX_DIALOG) ||
	   (rp->requestor == FROM_INSERT_DIALOG)))
	{
	  ss->reloading_updated_file = true; /* don't reread lack-of-header! */
	  /* redirection may be still set here, but I'll make it obvious */
	  switch (rp->requestor)
	    {
	    case FROM_MIX_DIALOG:
	      redirect_snd_error_to(redirect_file_open_error, (void *)mdat);
	      mix_complete_file_at_cursor(any_selected_sound(), rp->filename);
	      break;

	    case FROM_INSERT_DIALOG:
	      redirect_snd_error_to(redirect_file_open_error, (void *)idat);
	      insert_complete_file_at_cursor(any_selected_sound(), rp->filename);
	      break;

	    default:
	      snd_error("wrong requestor type in raw data dialog? %d\n", (int)(rp->requestor));
	      break;
	    }
	  redirect_snd_error_to(NULL, NULL);
	  ss->reloading_updated_file = false;
	}
      else
	{
	  file_info *hdr;
	  hdr = (file_info *)calloc(1, sizeof(file_info));
	  hdr->name = mus_strdup(rp->filename);
	  hdr->type = MUS_RAW;
	  hdr->srate = raw_srate;
	  hdr->chans = raw_chans;
	  hdr->sample_type = raw_sample_type;
	  hdr->samples = mus_bytes_to_samples(raw_sample_type, 
					      mus_sound_length(rp->filename) - rp->location);
	  hdr->data_location = rp->location;
	  hdr->comment = NULL;
	  if (rp->requestor == FROM_KEYBOARD)
	    {
	      clear_status_area((snd_info *)(rp->requestor_data));
	      rp->selected = true;
	    }
	  finish_opening_sound(add_sound_window(rp->filename, rp->read_only, hdr), rp->selected);
	}
      gtk_widget_hide(rp->dialog);
    }
}


static void raw_data_cancel_callback(GtkWidget *w, gpointer context)
{
  raw_info *rp = (raw_info *)context;
  gtk_widget_hide(rp->dialog);
  if ((rp->requestor_dialog) && 
      ((rp->requestor == FROM_OPEN_DIALOG) ||
       (rp->requestor == FROM_MIX_DIALOG)))
    gtk_widget_show(rp->requestor_dialog);
}


static gint raw_data_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context) 
{
  raw_info *rp = (raw_info *)context;
  if ((rp->requestor_dialog) && 
      ((rp->requestor == FROM_OPEN_DIALOG) ||
       (rp->requestor == FROM_MIX_DIALOG)))
    gtk_widget_show(rp->requestor_dialog);
  return(true);
}


static void raw_data_reset_callback(GtkWidget *w, gpointer context) 
{
  raw_info *rp = (raw_info *)context;
  int raw_srate, raw_chans;
  mus_sample_t raw_sample_type;
  rp->location = 0;
  mus_header_raw_defaults(&raw_srate, &raw_chans, &raw_sample_type); /* pick up defaults */  
  set_file_dialog_sound_attributes(rp->rdat, 
				   IGNORE_HEADER_TYPE, 
				   raw_sample_type, raw_srate, raw_chans, rp->location, 
				   IGNORE_SAMPLES, NULL);
  clear_dialog_error(rp->rdat);
}


static void raw_data_help_callback(GtkWidget *w, gpointer context) 
{
  raw_info *rp = (raw_info *)context;
  raw_data_dialog_help(rp->help);
}


static void make_raw_data_dialog(raw_info *rp, const char *filename, const char *title)
{
  GtkWidget *reset_button, *help_button, *cancel_button, *ok_button;
  int raw_srate, raw_chans;
  mus_sample_t raw_sample_type;
 
  rp->dialog = snd_gtk_dialog_new();
#if GTK_CHECK_VERSION(3, 14, 0)
  gtk_window_set_transient_for(GTK_WINDOW(rp->dialog), GTK_WINDOW(main_shell(ss)));
#endif
  if (!title)
    gtk_window_set_title(GTK_WINDOW(rp->dialog), "No header on file");
  else gtk_window_set_title(GTK_WINDOW(rp->dialog), title);
  sg_make_resizable(rp->dialog);
  sg_container_set_border_width(GTK_CONTAINER(rp->dialog), 10);
  gtk_window_resize(GTK_WINDOW(rp->dialog), 350, 260);
  gtk_widget_realize(rp->dialog);

  help_button = gtk_dialog_add_button(GTK_DIALOG(rp->dialog), "Help", GTK_RESPONSE_NONE);
  reset_button = gtk_dialog_add_button(GTK_DIALOG(rp->dialog), "Reset", GTK_RESPONSE_NONE);
  cancel_button = gtk_dialog_add_button(GTK_DIALOG(rp->dialog), "Go away", GTK_RESPONSE_NONE);
  ok_button = gtk_dialog_add_button(GTK_DIALOG(rp->dialog), "Ok", GTK_RESPONSE_NONE);

  gtk_widget_set_name(help_button, "dialog_button");
  gtk_widget_set_name(cancel_button, "dialog_button");
  gtk_widget_set_name(reset_button, "dialog_button");
  gtk_widget_set_name(ok_button, "dialog_button");

#if GTK_CHECK_VERSION(3, 0, 0)
  add_highlight_button_style(cancel_button);
  add_highlight_button_style(help_button);
  add_highlight_button_style(reset_button);
  add_highlight_button_style(ok_button);
#endif

  mus_header_raw_defaults(&raw_srate, &raw_chans, &raw_sample_type); /* pick up defaults */

  rp->rdat = make_file_data_panel(DIALOG_CONTENT_AREA(rp->dialog), "data-form", 
				  WITH_CHANNELS_FIELD, 
				  MUS_RAW, raw_sample_type, 
				  WITH_DATA_LOCATION_FIELD, 
				  WITHOUT_SAMPLES_FIELD,
				  WITHOUT_HEADER_TYPE_FIELD, 
				  WITHOUT_COMMENT_FIELD,
				  WITH_READABLE_HEADERS,
				  WITHOUT_SRATE_FIELD, 
				  WITHOUT_AUTO_COMMENT);
  rp->rdat->dialog = rp->dialog;
  set_file_dialog_sound_attributes(rp->rdat, 
				   IGNORE_HEADER_TYPE, 
				   raw_sample_type, raw_srate, raw_chans, rp->location, 
				   IGNORE_SAMPLES, NULL);

  SG_SIGNAL_CONNECT(rp->dialog, "delete_event", raw_data_delete_callback, rp);
  SG_SIGNAL_CONNECT(ok_button, "clicked", raw_data_ok_callback, rp);

  SG_SIGNAL_CONNECT(help_button, "clicked", raw_data_help_callback, rp);
  SG_SIGNAL_CONNECT(reset_button, "clicked", raw_data_reset_callback, rp);
  SG_SIGNAL_CONNECT(cancel_button, "clicked", raw_data_cancel_callback, rp);

  gtk_widget_show(ok_button);
  gtk_widget_show(cancel_button);
  gtk_widget_show(help_button);
  gtk_widget_show(reset_button);

  set_dialog_widget(RAW_DATA_DIALOG, rp->dialog);
}


void raw_data_dialog_to_file_info(const char *filename, char *title, char *info, read_only_t read_only, bool selected)
{
  raw_info *rp;
  rp = new_raw_dialog();
  rp->read_only = read_only;
  rp->selected = selected;
  if (rp->filename) free(rp->filename);
  rp->filename = mus_strdup(filename);
  rp->requestor = ss->open_requestor;
  rp->requestor_dialog = ss->requestor_dialog;
  ss->open_requestor = NO_REQUESTOR;
  ss->requestor_dialog = NULL;
  if ((rp->requestor_dialog) &&
      ((rp->requestor == FROM_OPEN_DIALOG) ||
       (rp->requestor == FROM_MIX_DIALOG)))
    gtk_widget_hide(rp->requestor_dialog);
  if (!title) 
    title = mus_format("no header found on %s\n", filename);
  if (!rp->dialog) 
    make_raw_data_dialog(rp, filename, title);
  else gtk_window_set_title(GTK_WINDOW(rp->dialog), title);
  free(title);
  if (rp->help) free(rp->help);
  if (info)
    {
      rp->help = mus_strdup(info);
      free(info);
    }
  else rp->help = NULL;
  raise_dialog(rp->dialog);
  gtk_widget_show(rp->dialog);
}


/* -------------------------------- New File -------------------------------- */

static GtkWidget *new_file_dialog = NULL, *new_file_text = NULL, *new_file_ok_button = NULL;
static file_data *ndat = NULL;
static mus_long_t initial_samples = 1;
static char *new_file_filename = NULL;
static void *new_file_watcher = NULL;

static void cleanup_new_file_watcher(void)
{
  new_file_watcher = unmonitor_file(new_file_watcher);
}


static gulong new_file_handler_id = 0;
static gboolean new_filename_modify_callback(GtkWidget *w, GdkEventKey *event, gpointer ignored);

static void new_file_undoit(void)
{
  clear_dialog_error(ndat);
  if (new_file_handler_id)
    {
      if (new_file_handler_id)
	{
	  g_signal_handler_disconnect(new_file_text, new_file_handler_id);
	  new_file_handler_id = 0;
	}
    }
  set_stock_button_label(new_file_ok_button, "Ok");
  new_file_watcher = unmonitor_file(new_file_watcher);
}


static gboolean new_filename_modify_callback(GtkWidget *w, GdkEventKey *event, gpointer ignored)
{
  new_file_undoit();
  return(false);
}


static void clear_error_if_new_filename_changes(GtkWidget *dialog)
{
  if (new_file_text)
    new_file_handler_id = SG_SIGNAL_CONNECT(new_file_text, "key_press_event", new_filename_modify_callback, NULL);
}


#if HAVE_G_FILE_MONITOR_DIRECTORY
static void watch_new_file(GFileMonitor *mon, GFile *file, GFile *other, GFileMonitorEvent ev, gpointer data)
{
  /* if file is deleted, respond in some debonair manner */
  switch (ev)
    {
    case G_FILE_MONITOR_EVENT_CHANGED:
    case G_FILE_MONITOR_EVENT_DELETED:
    case G_FILE_MONITOR_EVENT_CREATED:
#if HAVE_GTK_WIDGET_GET_VISIBLE
    case G_FILE_MONITOR_EVENT_MOVED:
#endif
      new_file_undoit();
      break;

    default:
      /* ignore the rest */
      break;
    }
}
#endif


static void new_file_ok_callback(GtkWidget *w, gpointer context) 
{
  mus_long_t loc;
  char *newer_name, *msg;
  mus_header_t header_type;
  mus_sample_t sample_type;
  int srate, chans;
  newer_name = (char *)gtk_entry_get_text(GTK_ENTRY(new_file_text));
  if ((!newer_name) || (!(*newer_name)))
    {
      msg = (char *)"new sound needs a file name ('New file:' field is empty)";
      post_file_dialog_error((const char *)msg, ndat);
      clear_error_if_new_filename_changes(new_file_dialog);
    }
  else
    {
      char *comment;
      redirect_snd_error_to(redirect_post_file_panel_error, (void *)ndat);
      comment = get_file_dialog_sound_attributes(ndat, &srate, &chans, &header_type, &sample_type, &loc, &initial_samples, 1);
      redirect_snd_error_to(NULL, NULL);
      if (ndat->error_widget != NOT_A_SCANF_WIDGET)
	{
	  clear_error_if_panel_changes(new_file_dialog, ndat);
	}
      else
	{
	  /* handle the overwrite hook directly */
	  if (new_file_filename) free(new_file_filename);
	  new_file_filename = mus_expand_filename(newer_name); /* need full filename for fam */
	  if ((!new_file_watcher) &&
	      (ask_before_overwrite(ss)) && 
	      (mus_file_probe(new_file_filename)))
	    {
	      msg = mus_format("%s exists. If you want to overwrite it, click 'DoIt'", newer_name);
#if HAVE_G_FILE_MONITOR_DIRECTORY
	      {
		GFile *file;
		GError *err = NULL;
		file = g_file_new_for_path(new_file_filename);
		new_file_watcher = (void *)g_file_monitor_file(file, G_FILE_MONITOR_NONE, NULL, &err);
		if (err)
		  snd_warning("%s", err->message);
		else g_signal_connect(G_OBJECT(new_file_watcher), "changed", G_CALLBACK(watch_new_file), NULL);
		g_object_unref(file);
	      }
#endif
	      set_stock_button_label(new_file_ok_button, "DoIt");
	      post_file_dialog_error((const char *)msg, ndat);
	      clear_error_if_new_filename_changes(new_file_dialog);
	      free(msg);
	    }
	  else
	    {
	      snd_info *sp;
	      if (new_file_watcher)
		new_file_undoit();

	      ss->local_errno = 0;
	      redirect_snd_error_to(redirect_post_file_dialog_error, (void *)ndat);
	      sp = snd_new_file(newer_name, chans, srate, sample_type, header_type, comment, initial_samples);
	      redirect_snd_error_to(NULL, NULL);
	      if (!sp)
		{
#if HAVE_G_FILE_MONITOR_DIRECTORY
		  if ((ss->local_errno) &&
		      (mus_file_probe(new_file_filename))) /* see comment in snd-xfile.c */
		    {
		      GFile *file;
		      GError *err = NULL;
		      file = g_file_new_for_path(new_file_filename);
		      new_file_watcher = (void *)g_file_monitor_file(file, G_FILE_MONITOR_NONE, NULL, &err);
		      if (err)
			snd_warning("%s", err->message);
		      else g_signal_connect(G_OBJECT(new_file_watcher), "changed", G_CALLBACK(watch_new_file), NULL);
		      g_object_unref(file);
		    }
#endif
		  clear_error_if_new_filename_changes(new_file_dialog);
		}
	      else
		{
		  gtk_widget_hide(new_file_dialog);
		}
	    }
	}
      if (comment) free(comment);
    }
}


static char *new_file_dialog_filename(int header_type)
{
  static int new_file_dialog_file_ctr = 1;
  char *filename;
  const char *extensions[6] = {"aiff", "aiff", "wav", "wav", "caf", "snd"};
  int extension = 0;

  filename = (char *)calloc(64, sizeof(char));
  switch (header_type)
    {
    case MUS_AIFC: extension = 0; break;
    case MUS_AIFF: extension = 1; break;
    case MUS_RF64: extension = 2;  break;
    case MUS_RIFF: extension = 3;  break;
    case MUS_CAFF: extension = 4;  break;
    default:       extension = 5;  break;
    }
  snprintf(filename, 64, "new-%d.%s", new_file_dialog_file_ctr++, extensions[extension]);

  return(filename);
}


static void load_new_file_defaults(char *newname)
{
  char *new_comment;
  mus_header_t header_type;
  mus_sample_t sample_type;
  int chans, srate;

  header_type = default_output_header_type(ss);
  chans =       default_output_chans(ss);
  sample_type = default_output_sample_type(ss);
  srate =       default_output_srate(ss);
  new_comment = output_comment(NULL);

  if ((!newname) || (!(*newname)))
    newname = new_file_dialog_filename(header_type);
  gtk_entry_set_text(GTK_ENTRY(new_file_text), newname);  
  mus_sound_forget(newname);

  set_file_dialog_sound_attributes(ndat, header_type, sample_type, srate, chans, IGNORE_DATA_LOCATION, initial_samples, new_comment);
  if (new_comment) free(new_comment);
}


static void new_file_cancel_callback(GtkWidget *w, gpointer context)
{
  gtk_widget_hide(new_file_dialog);
}


static void new_file_reset_callback(GtkWidget *w, gpointer context)
{
  char *current_name;
  current_name = (char *)gtk_entry_get_text(GTK_ENTRY(new_file_text));
  load_new_file_defaults(current_name);
  if (new_file_watcher)
    new_file_undoit();
}


static gint new_file_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context) 
{
  gtk_widget_hide(new_file_dialog);
  return(true);
}


static void new_file_help_callback(GtkWidget *w, gpointer context) 
{
  new_file_dialog_help();
}


widget_t make_new_file_dialog(bool managed)
{
  if (!new_file_dialog)
    {
      GtkWidget *name_label, *hform, *help_button, *cancel_button, *reset_button;
      new_file_dialog = snd_gtk_dialog_new();
#if GTK_CHECK_VERSION(3, 14, 0)
      gtk_window_set_transient_for(GTK_WINDOW(new_file_dialog), GTK_WINDOW(main_shell(ss)));
#endif
      gtk_window_set_title(GTK_WINDOW(new_file_dialog), "New file");
      sg_make_resizable(new_file_dialog);
      sg_container_set_border_width (GTK_CONTAINER(new_file_dialog), 10);
      gtk_window_resize(GTK_WINDOW(new_file_dialog), 400, 250);
      gtk_widget_realize(new_file_dialog);

      help_button = gtk_dialog_add_button(GTK_DIALOG(new_file_dialog), "Help", GTK_RESPONSE_NONE);
      reset_button = gtk_dialog_add_button(GTK_DIALOG(new_file_dialog), "Reset", GTK_RESPONSE_NONE);
      cancel_button = gtk_dialog_add_button(GTK_DIALOG(new_file_dialog), "Go away", GTK_RESPONSE_NONE);
      new_file_ok_button = gtk_dialog_add_button(GTK_DIALOG(new_file_dialog), "Ok", GTK_RESPONSE_NONE);

      gtk_widget_set_name(help_button, "dialog_button");
      gtk_widget_set_name(cancel_button, "dialog_button");
      gtk_widget_set_name(new_file_ok_button, "dialog_button");
      gtk_widget_set_name(reset_button, "dialog_button");

#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(help_button);
      add_highlight_button_style(cancel_button);
      add_highlight_button_style(reset_button);
      add_highlight_button_style(new_file_ok_button);
#endif

      hform = gtk_hbox_new(false, 0);
      sg_box_pack_start(GTK_BOX(DIALOG_CONTENT_AREA(new_file_dialog)), hform, false, false, 4);
      gtk_widget_show(hform);

      name_label = gtk_label_new("New file:");
      sg_box_pack_start(GTK_BOX(hform), name_label, false, false, 2);
      gtk_widget_show(name_label);

      new_file_text = snd_entry_new(hform, NULL, WITH_WHITE_BACKGROUND);

      ndat = make_file_data_panel(DIALOG_CONTENT_AREA(new_file_dialog), "data-form", 
				  WITH_CHANNELS_FIELD, 
				  default_output_header_type(ss), 
				  default_output_sample_type(ss), 
				  WITHOUT_DATA_LOCATION_FIELD, 
				  WITH_SAMPLES_FIELD,
				  WITH_HEADER_TYPE_FIELD, 
				  WITH_COMMENT_FIELD,
				  WITH_BUILTIN_HEADERS,
				  WITHOUT_SRATE_FIELD, 
				  WITHOUT_AUTO_COMMENT);
      ndat->dialog = new_file_dialog;

      SG_SIGNAL_CONNECT(new_file_dialog, "delete_event", new_file_delete_callback, ndat);
      SG_SIGNAL_CONNECT(cancel_button, "clicked", new_file_cancel_callback, ndat);
      SG_SIGNAL_CONNECT(help_button, "clicked", new_file_help_callback, ndat);
      SG_SIGNAL_CONNECT(new_file_ok_button, "clicked", new_file_ok_callback, ndat);
      SG_SIGNAL_CONNECT(reset_button, "clicked", new_file_reset_callback, ndat);
      SG_SIGNAL_CONNECT(new_file_text, "activate", new_file_ok_callback, ndat);

      gtk_widget_show(cancel_button);
      gtk_widget_show(new_file_ok_button);
      gtk_widget_show(reset_button);
      gtk_widget_show(help_button);

      set_dialog_widget(NEW_FILE_DIALOG, new_file_dialog);
      load_new_file_defaults(NULL);
    }
  else
    {
      char *new_name;
      new_name = (char *)gtk_entry_get_text(GTK_ENTRY(new_file_text));

      if (new_file_watcher)
	{
	  /* if overwrite question pends, but file has been deleted in the meantime, go back to normal state */
	  if ((!new_name) || (!(*new_name)) ||
	      (!(mus_file_probe(new_name))))
	    new_file_undoit();
	}

      if (strncmp(new_name, "new-", 4) == 0)
	{
	  /* if file is open with currently posted new-file dialog name, and it's our name (new-%d), then tick the counter */
	  snd_info *sp;
	  sp = find_sound(new_name, 0);
	  if (sp)
	    {
	      char *filename;
	      filename = new_file_dialog_filename(default_output_header_type(ss));
	      gtk_entry_set_text(GTK_ENTRY(new_file_text), filename);  
	      mus_sound_forget(filename);
	      free(filename);
	    }
	}
    }
  if (managed)
    gtk_widget_show(new_file_dialog);
  return(new_file_dialog);
}



/* ---------------- Edit Header ---------------- */

typedef struct edhead_info {
  GtkWidget *dialog, *save_button;
  file_data *edat;
  snd_info *sp;
  bool panel_changed;
  void *file_ro_watcher;
  int sp_ro_watcher_loc;
} edhead_info;

static int edhead_info_size = 0;
static edhead_info **edhead_infos = NULL;

static edhead_info *new_edhead_dialog(void)
{
  int loc = -1;
  if (edhead_info_size == 0)
    {
      loc = 0;
      edhead_info_size = 4;
      edhead_infos = (edhead_info **)calloc(edhead_info_size, sizeof(edhead_info *));
    }
  else
    {
      int i;
      for (i = 0; i < edhead_info_size; i++)
	if ((!edhead_infos[i]) ||
	    (!(widget_is_active(edhead_infos[i]->dialog))))
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = edhead_info_size;
	  edhead_info_size += 4;
	  edhead_infos = (edhead_info **)realloc(edhead_infos, edhead_info_size * sizeof(edhead_info *));
	  for (i = loc; i < edhead_info_size; i++) edhead_infos[i] = NULL;
	}
    }
  if (!edhead_infos[loc])
    {
      edhead_infos[loc] = (edhead_info *)calloc(1, sizeof(edhead_info));
      edhead_infos[loc]->dialog = NULL;
      edhead_infos[loc]->panel_changed = false;
    }
  edhead_infos[loc]->sp = NULL;
  edhead_infos[loc]->file_ro_watcher = NULL;
  return(edhead_infos[loc]);
}


static void cleanup_edit_header_watcher(void)
{
  int i;
  for (i = 0; i < edhead_info_size; i++)
    if (edhead_infos[i])
      {
	edhead_info *ep;
	ep = edhead_infos[i];
	if (ep->file_ro_watcher)
	  ep->file_ro_watcher = unmonitor_file(ep->file_ro_watcher);
      }
}


static char *make_header_dialog_title(edhead_info *ep, snd_info *sp)
{
  /* dialog may not yet exist */
  char *str;
  str = (char *)calloc(PRINT_BUFFER_SIZE, sizeof(char));
  if ((sp->user_read_only == FILE_READ_ONLY) || 
      (sp->file_read_only == FILE_READ_ONLY))
    {
      if (sp->hdr->type == MUS_RAW)
	snprintf(str, PRINT_BUFFER_SIZE, "Add header to (write-protected) %s", sp->short_filename);
      else snprintf(str, PRINT_BUFFER_SIZE, "Edit header of (write-protected) %s", sp->short_filename);
    }
  else 
    {
      if (sp->hdr->type == MUS_RAW)
	snprintf(str, PRINT_BUFFER_SIZE, "Add header to %s", sp->short_filename);
      else snprintf(str, PRINT_BUFFER_SIZE, "Edit header of %s", sp->short_filename);
    }
  return(str);
}


static void edit_header_help_callback(GtkWidget *w, gpointer context) 
{
  edit_header_dialog_help();
}


static void edit_header_set_ok_sensitive(GtkWidget *w, gpointer context) 
{
  edhead_info *ep = (edhead_info *)context;
  ep->panel_changed = true;
}


static void edit_header_done(edhead_info *ep)
{
  gtk_widget_hide(ep->dialog);
  unreflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
  ep->panel_changed = false;
  ep->file_ro_watcher = unmonitor_file(ep->file_ro_watcher);
}


static void edit_header_cancel_callback(GtkWidget *w, gpointer context) 
{
  edit_header_done((edhead_info *)context);
}


static gint edit_header_delete_callback(GtkWidget *w, GdkEvent *event, gpointer context)
{
  edit_header_done((edhead_info *)context);
  return(true);
}


#if HAVE_G_FILE_MONITOR_DIRECTORY
static void watch_file_read_only(GFileMonitor *mon, GFile *file, GFile *other, GFileMonitorEvent ev, gpointer data)
{
  /* if file is deleted or permissions change, respond in some debonair manner */
  edhead_info *ep = (edhead_info *)data;
  snd_info *sp;
  sp = ep->sp;
  if (sp->writing) return;

  switch (ev)
    {
    case G_FILE_MONITOR_EVENT_CHANGED:
#ifndef _MSC_VER
      {
	if (mus_file_probe(sp->filename))
	  {
	    char *title;
	    int err;
	    err = access(sp->filename, W_OK);
	    sp->file_read_only = ((err < 0) ? FILE_READ_ONLY : FILE_READ_WRITE);
	    if ((sp->file_read_only == FILE_READ_WRITE) && 
		(sp->user_read_only == FILE_READ_WRITE))
	      clear_dialog_error(ep->edat);
	    title = make_header_dialog_title(ep, sp);
	    gtk_window_set_title(GTK_WINDOW(ep->dialog), title);
	    free(title);
	    return;
	  }
      }
#endif

      /* else fall through */
    case G_FILE_MONITOR_EVENT_DELETED:
    case G_FILE_MONITOR_EVENT_CREATED:
#if HAVE_GTK_WIDGET_GET_VISIBLE
    case G_FILE_MONITOR_EVENT_MOVED:
#endif
      /* I don't think it makes sense to continue the dialog at this point */
      clear_dialog_error(ep->edat);
      gtk_widget_hide(ep->dialog);
      if (ep->panel_changed)
	unreflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
      ep->panel_changed = false;
      ep->file_ro_watcher = unmonitor_file(ep->file_ro_watcher);
      break;

    default:
      /* ignore the rest */
      break;
    }
}
#endif


static void edit_header_ok_callback(GtkWidget *w, gpointer context) 
{
  edhead_info *ep = (edhead_info *)context;
  if ((ep->sp) && (ep->sp->active))
    {
      bool ok;
      redirect_snd_error_to(redirect_post_file_dialog_error, (void *)(ep->edat));
      ok = edit_header_callback(ep->sp, ep->edat, redirect_post_file_dialog_error, redirect_post_file_panel_error);
      redirect_snd_error_to(NULL, NULL);
      if (ep->edat->error_widget != NOT_A_SCANF_WIDGET)
	{
	  clear_error_if_panel_changes(ep->dialog, ep->edat);
	  return;
	}
      else
	{
	  if (!ok)
	    return;
	}
      ep->file_ro_watcher = unmonitor_file(ep->file_ro_watcher);
      gtk_widget_hide(ep->dialog);
      unreflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
    }
}


GtkWidget *edit_header(snd_info *sp)
{
  char *str;
  file_info *hdr;
  edhead_info *ep = NULL;

  if (!sp) return(NULL);
  /* look for a dialog already editing this sound, raise if found, else make a new one */
  if (edhead_info_size > 0)
    {
      int i;
      for (i = 0; i < edhead_info_size; i++)
	if ((edhead_infos[i]) &&
	    ((edhead_infos[i]->sp == sp) ||
	     ((edhead_infos[i]->sp) && /* maybe same sound open twice -- only one edit header dialog for it */
	      (edhead_infos[i]->sp->inuse == SOUND_NORMAL) &&
	      (mus_strcmp(sp->filename, edhead_infos[i]->sp->filename)))))
	  {
	    ep = edhead_infos[i];
	    break;
	  }
    }
  if (!ep)
    ep = new_edhead_dialog();

  ep->sp = sp;
  hdr = sp->hdr;
  ep->panel_changed = (hdr->type == MUS_RAW);

  if (!ep->dialog)
    {
      GtkWidget *help_button, *cancel_button;
      ep->dialog = snd_gtk_dialog_new();
#if GTK_CHECK_VERSION(3, 14, 0)
      gtk_window_set_transient_for(GTK_WINDOW(ep->dialog), GTK_WINDOW(main_shell(ss)));
#endif
      /* gtk_window_set_title(GTK_WINDOW(ep->dialog), "Edit Header"); */
      sg_make_resizable(ep->dialog);
      sg_container_set_border_width (GTK_CONTAINER(ep->dialog), 10);
      gtk_window_resize(GTK_WINDOW(ep->dialog), 400, 250);
      gtk_widget_realize(ep->dialog);

      help_button = gtk_dialog_add_button(GTK_DIALOG(ep->dialog), "Help", GTK_RESPONSE_NONE);
      cancel_button = gtk_dialog_add_button(GTK_DIALOG(ep->dialog), "Go away", GTK_RESPONSE_NONE);
      ep->save_button = gtk_dialog_add_button(GTK_DIALOG(ep->dialog), "Save", GTK_RESPONSE_NONE);

      gtk_widget_set_name(help_button, "dialog_button");
      gtk_widget_set_name(cancel_button, "dialog_button");
      gtk_widget_set_name(ep->save_button, "dialog_button");

#if GTK_CHECK_VERSION(3, 0, 0)
      add_highlight_button_style(help_button);
      add_highlight_button_style(cancel_button);
      add_highlight_button_style(ep->save_button);
#endif

      ep->edat = make_file_data_panel(DIALOG_CONTENT_AREA(ep->dialog), "Edit Header", 
				      WITH_CHANNELS_FIELD, 
				      hdr->type, 
				      hdr->sample_type, 
				      WITH_DATA_LOCATION_FIELD, 
				      WITH_SAMPLES_FIELD,
				      WITH_HEADER_TYPE_FIELD, 
				      WITH_COMMENT_FIELD,
				      WITH_BUILTIN_HEADERS,
				      WITHOUT_SRATE_FIELD, 
				      WITHOUT_AUTO_COMMENT);
      ep->edat->dialog = ep->dialog;

      SG_SIGNAL_CONNECT(ep->dialog, "delete_event", edit_header_delete_callback, ep);
      SG_SIGNAL_CONNECT(cancel_button, "clicked", edit_header_cancel_callback, ep);
      SG_SIGNAL_CONNECT(help_button, "clicked", edit_header_help_callback, ep);
      SG_SIGNAL_CONNECT(ep->save_button, "clicked", edit_header_ok_callback, ep);

      gtk_widget_show(cancel_button);
      gtk_widget_show(ep->save_button);
      gtk_widget_show(help_button);

      set_dialog_widget(EDIT_HEADER_DIALOG, ep->dialog);
    }
  else clear_dialog_error(ep->edat);

  str = make_header_dialog_title(ep, sp);
  gtk_window_set_title(GTK_WINDOW(ep->dialog), str);
  free(str);

  if (hdr->type == MUS_RAW)
    set_file_dialog_sound_attributes(ep->edat, 
				     default_output_header_type(ss), 
				     hdr->sample_type, hdr->srate, hdr->chans, 
				     hdr->data_location, hdr->samples, hdr->comment);
  else set_file_dialog_sound_attributes(ep->edat, 
					hdr->type, hdr->sample_type, hdr->srate, hdr->chans, 
					hdr->data_location, hdr->samples, hdr->comment);

  gtk_widget_show(ep->dialog);
  reflect_file_data_panel_change(ep->edat, (void *)ep, edit_header_set_ok_sensitive);
#if HAVE_G_FILE_MONITOR_DIRECTORY
  {
    GFile *file;
    GError *err = NULL;
    file = g_file_new_for_path(ep->sp->filename);
    ep->file_ro_watcher = (void *)g_file_monitor_file(file, G_FILE_MONITOR_NONE, NULL, &err);
    if (err)
      snd_warning("%s", err->message);
    else g_signal_connect(G_OBJECT(ep->file_ro_watcher), "changed", G_CALLBACK(watch_file_read_only), (gpointer)ep);
    g_object_unref(file);
  }
#endif
  return(ep->dialog);
}


void save_edit_header_dialog_state(FILE *fd)
{
  int i;
  for (i = 0; i < edhead_info_size; i++)
    if (edhead_infos[i])
      {
	edhead_info *ep;
	ep = edhead_infos[i];
	if ((ep->dialog) && 
	    (widget_is_active(ep->dialog)) && 
	    (snd_ok(ep->sp)))
	  {
#if HAVE_SCHEME
	    fprintf(fd, "(%s (%s \"%s\"))\n", S_edit_header_dialog, S_find_sound, ep->sp->short_filename);
#endif
#if HAVE_RUBY
	    fprintf(fd, "%s(%s(\"%s\"))\n", to_proc_name(S_edit_header_dialog), to_proc_name(S_find_sound), ep->sp->short_filename);
#endif
#if HAVE_FORTH
	    fprintf(fd, "\"%s\" %s %s drop\n", ep->sp->short_filename, S_find_sound, S_edit_header_dialog);
#endif
	  }
      }
}



/* ---------------- Post-it Monolog ---------------- */

#define POST_IT_ROWS 12
#define POST_IT_COLUMNS 56

static GtkWidget *post_it_text = NULL, *post_it_dialog = NULL;

static void dismiss_post_it(GtkWidget *w, gpointer context) {gtk_widget_hide(post_it_dialog);}

static gint delete_post_it(GtkWidget *w, GdkEvent *event, gpointer context) 
{
  gtk_widget_hide(post_it_dialog);
  return(true);
}


static void create_post_it_monolog(void)
{
  /* create scrollable but not editable text window */
  GtkWidget *ok_button;
  post_it_dialog = gtk_dialog_new();
#if GTK_CHECK_VERSION(3, 14, 0)
  gtk_window_set_transient_for(GTK_WINDOW(post_it_dialog), GTK_WINDOW(main_shell(ss)));
#endif
  SG_SIGNAL_CONNECT(post_it_dialog, "delete_event", delete_post_it, NULL);

  gtk_window_set_title(GTK_WINDOW(post_it_dialog), "Info");
  sg_make_resizable(post_it_dialog);
  sg_container_set_border_width(GTK_CONTAINER(post_it_dialog), 10);
  gtk_window_resize(GTK_WINDOW(post_it_dialog), POST_IT_COLUMNS * 9, POST_IT_ROWS * 20);
  gtk_widget_realize(post_it_dialog);

  ok_button = gtk_dialog_add_button(GTK_DIALOG(post_it_dialog), "Ok", GTK_RESPONSE_NONE);
  gtk_widget_set_name(ok_button, "dialog_button");
  SG_SIGNAL_CONNECT(ok_button, "clicked", dismiss_post_it, NULL);
  gtk_widget_show(ok_button);

#if GTK_CHECK_VERSION(3, 0, 0)
  add_highlight_button_style(ok_button);
#endif

  post_it_text = make_scrolled_text(DIALOG_CONTENT_AREA(post_it_dialog), false, BOX_PACK, true);
  gtk_text_view_set_left_margin(GTK_TEXT_VIEW(post_it_text), 10);
  gtk_widget_show(post_it_dialog);
  set_dialog_widget(POST_IT_DIALOG, post_it_dialog);
}


widget_t post_it(const char *subject, const char *str)
{
  if (!ss) return(NULL);
  if (!(post_it_dialog)) create_post_it_monolog(); else raise_dialog(post_it_dialog);
  gtk_window_set_title(GTK_WINDOW(post_it_dialog), subject);
  gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(post_it_text)), "", 0);
  sg_text_insert(post_it_text, (char *)str);
  return(post_it_dialog);
}


void post_it_append(const char *str)
{
  if (post_it_text)
    sg_text_insert(post_it_text, (char *)str);
}


void save_post_it_dialog_state(FILE *fd)
{
  if ((post_it_dialog) && (widget_is_active(post_it_dialog)))
    {
      char *subject;
      gchar *text;
      subject = dialog_get_title(post_it_dialog);
      text = sg_get_text(post_it_text, 0, -1);
#if HAVE_SCHEME
      fprintf(fd, "(%s \"%s\" \"%s\")\n", S_info_dialog, subject, text);
#endif
#if HAVE_RUBY
      fprintf(fd, "%s(\"%s\", \"%s\")\n", to_proc_name(S_info_dialog), subject, text);
#endif
#if HAVE_FORTH
      fprintf(fd, "\"%s\" \"%s\" %s drop\n", subject, text, S_info_dialog);
#endif
      if (text) g_free(text);
      if (subject) free(subject);
    }
}



/* ---------------- unsaved edits dialog ---------------- */

static int num_unsaved_edits_dialogs = 0;
static GtkWidget **unsaved_edits_dialogs = NULL;
static snd_info **unsaved_edits_sounds = NULL;

static GtkWidget *unsaved_edits_dialog(snd_info *sp)
{
  int i;
  /* are there any such dialogs? */
  if (num_unsaved_edits_dialogs == 0)
    return(NULL);

  /* now see if we've already prompted about this sound */
  for (i = 0; i < num_unsaved_edits_dialogs; i++)
    if (unsaved_edits_sounds[i] == sp)
      return(unsaved_edits_dialogs[i]);

  /* try to find a free unmanaged dialog */
  for (i = 0; i < num_unsaved_edits_dialogs; i++)
    if ((unsaved_edits_dialogs[i]) &&
	(!widget_is_active(unsaved_edits_dialogs[i])))
      return(unsaved_edits_dialogs[i]);

  return(NULL);
}

static void save_unsaved_edits_dialog(GtkWidget *d, snd_info *sp)
{
  if (num_unsaved_edits_dialogs == 0)
    {
      unsaved_edits_dialogs = (GtkWidget **)calloc(1, sizeof(GtkWidget *));
      unsaved_edits_sounds = (snd_info **)calloc(1, sizeof(snd_info *));
    }
  else
    {
      unsaved_edits_dialogs = (GtkWidget **)realloc(unsaved_edits_dialogs, (num_unsaved_edits_dialogs + 1) * sizeof(GtkWidget *));
      unsaved_edits_sounds = (snd_info **)realloc(unsaved_edits_sounds, (num_unsaved_edits_dialogs + 1) * sizeof(snd_info *));
    }

  unsaved_edits_dialogs[num_unsaved_edits_dialogs] = d;
  unsaved_edits_sounds[num_unsaved_edits_dialogs] = sp;
  num_unsaved_edits_dialogs++;
}


void unpost_unsaved_edits_if_any(snd_info *sp)
{
  int i;
  for (i = 0; i < num_unsaved_edits_dialogs; i++)
    if (((unsaved_edits_sounds[i] == sp) ||
	 (!snd_ok(unsaved_edits_sounds[i]))) &&
	(widget_is_active(unsaved_edits_dialogs[i])))
      gtk_widget_hide(unsaved_edits_dialogs[i]);
}


static void zero_edits(chan_info *cp)
{
  cp->edit_ctr = 0;
}


static void unsaved_edits_activate(GtkDialog *w, gint id, gpointer context)
{
  snd_info *sp = (snd_info *)context;
  if (id == GTK_RESPONSE_NO)
    for_each_sound_chan(sp, zero_edits);
  else save_edits(sp);
  snd_close_file(sp);
  gtk_widget_hide(GTK_WIDGET(w));
}


void save_edits_now(snd_info *sp)
{
  char *question;
  GtkWidget *dialog;

  question = mus_format("%s has unsaved edits.  Save them?", sp->short_filename);
  dialog = unsaved_edits_dialog(sp);
  if (!dialog)
    {
      dialog = gtk_message_dialog_new(NULL, GTK_DIALOG_MODAL, GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO, "%s", question);
      SG_SIGNAL_CONNECT(dialog, "response", unsaved_edits_activate, (gpointer)sp);
      save_unsaved_edits_dialog(dialog, sp);
    }
  else
    {
      g_object_set(dialog, "text", question, NULL);
    }

  free(question);
  gtk_widget_show(dialog);
}



/* ---------------- file has changed dialog ---------------- */

static int num_file_has_changed_dialogs = 0;
static GtkWidget **file_has_changed_dialogs = NULL;
static snd_info **file_has_changed_sounds = NULL;

static GtkWidget *file_has_changed_dialog(snd_info *sp)
{
  int i;
  /* are there any such dialogs? */
  if (num_file_has_changed_dialogs == 0)
    return(NULL);

  /* now see if we've already prompted about this sound */
  for (i = 0; i < num_file_has_changed_dialogs; i++)
    if (file_has_changed_sounds[i] == sp)
      return(file_has_changed_dialogs[i]);

  /* try to find a free unmanaged dialog */
  for (i = 0; i < num_file_has_changed_dialogs; i++)
    if ((file_has_changed_dialogs[i]) &&
	(!widget_is_active(file_has_changed_dialogs[i])))
      return(file_has_changed_dialogs[i]);

  return(NULL);
}

static void save_file_has_changed_dialog(GtkWidget *d, snd_info *sp)
{
  if (num_file_has_changed_dialogs == 0)
    {
      file_has_changed_dialogs = (GtkWidget **)calloc(1, sizeof(GtkWidget *));
      file_has_changed_sounds = (snd_info **)calloc(1, sizeof(snd_info *));
    }
  else
    {
      file_has_changed_dialogs = (GtkWidget **)realloc(file_has_changed_dialogs, (num_file_has_changed_dialogs + 1) * sizeof(GtkWidget *));
      file_has_changed_sounds = (snd_info **)realloc(file_has_changed_sounds, (num_file_has_changed_dialogs + 1) * sizeof(snd_info *));
    }

  file_has_changed_dialogs[num_file_has_changed_dialogs] = d;
  file_has_changed_sounds[num_file_has_changed_dialogs] = sp;
  num_file_has_changed_dialogs++;
}


void unpost_file_has_changed_if_any(snd_info *sp)
{
  int i;
  for (i = 0; i < num_file_has_changed_dialogs; i++)
    if (((file_has_changed_sounds[i] == sp) ||
	 (!snd_ok(file_has_changed_sounds[i]))) &&
	(widget_is_active(file_has_changed_dialogs[i])))
      gtk_widget_hide(file_has_changed_dialogs[i]);
}


static void file_has_changed_activate(GtkDialog *w, gint id, gpointer context)
{
  if (id == GTK_RESPONSE_YES)
    {
      snd_info *sp = (snd_info *)context;
      save_edits_without_asking(sp);
      sp->need_update = false;
      stop_bomb(sp);                  /* in case Snd already noticed the problem */
      clear_status_area(sp);
    }
  gtk_widget_hide(GTK_WIDGET(w));
}


void changed_file_dialog(snd_info *sp)
{
  char *question;
  GtkWidget *dialog;

  question = mus_format("%s has changed!  Save edits anyway?", sp->short_filename);
  dialog = file_has_changed_dialog(sp);
  if (!dialog)
    {
      dialog = gtk_message_dialog_new(NULL, GTK_DIALOG_MODAL, GTK_MESSAGE_QUESTION, GTK_BUTTONS_YES_NO, "%s", question);
      SG_SIGNAL_CONNECT(dialog, "response", file_has_changed_activate, (gpointer)sp);
      save_file_has_changed_dialog(dialog, sp);
    }
  else
    {
      g_object_set(dialog, "text", question, NULL);
    }

  free(question);
  gtk_widget_show(dialog);
}


/* drop */

typedef struct {
  void (*drop_watcher)(GtkWidget *w, const char *message, int x, int y, void *data);
  void (*drag_watcher)(GtkWidget *w, const char *message, int x, int y, drag_style_t dtype, void *data);
  GtkWidget *caller;
  void *context;
} drop_watcher_t;

static drop_watcher_t **drop_watchers = NULL;
static int drop_watchers_size = 0;

#define DROP_WATCHER_SIZE_INCREMENT 2


static int add_drop_watcher(GtkWidget *w, 
			    void (*drop_watcher)(GtkWidget *w, const char *message, int x, int y, void *data), 
			    void (*drag_watcher)(GtkWidget *w, const char *message, int x, int y, drag_style_t dtype, void *data), 
			    void *context)
{
  int loc = -1;
  if (!(drop_watchers))
    {
      loc = 0;
      drop_watchers_size = DROP_WATCHER_SIZE_INCREMENT;
      drop_watchers = (drop_watcher_t **)calloc(drop_watchers_size, sizeof(drop_watcher_t *));
    }
  else
    {
      int i;
      for (i = 0; i < drop_watchers_size; i++)
	if (!(drop_watchers[i]))
	  {
	    loc = i;
	    break;
	  }
      if (loc == -1)
	{
	  loc = drop_watchers_size;
	  drop_watchers_size += DROP_WATCHER_SIZE_INCREMENT;
	  drop_watchers = (drop_watcher_t **)realloc(drop_watchers, drop_watchers_size * sizeof(drop_watcher_t *));
	  for (i = loc; i < drop_watchers_size; i++) drop_watchers[i] = NULL;
	}
    }
  drop_watchers[loc] = (drop_watcher_t *)calloc(1, sizeof(drop_watcher_t));
  drop_watchers[loc]->drop_watcher = drop_watcher;
  drop_watchers[loc]->drag_watcher = drag_watcher;
  drop_watchers[loc]->context = context;
  drop_watchers[loc]->caller = w;
  return(loc);
}


static drop_watcher_t *find_drop_watcher(GtkWidget *caller)
{
  if (drop_watchers)
    {
      int i;
      for (i = 0; i < drop_watchers_size; i++)
	{
	  if (drop_watchers[i])
	    {
	      drop_watcher_t *d;
	      d = drop_watchers[i];
	      if (d->caller == caller)
		return(d);
	    }
	}
    }
  return(NULL);
}


enum {TARGET_STRING, TARGET_UTF8, TARGET_URL};

static GtkTargetEntry target_table[] = {
  {(char *)"STRING",        0, TARGET_STRING},
  {(char *)"FILE_NAME",     0, TARGET_STRING},
  {(char *)"text/plain",    0, TARGET_STRING},
  {(char *)"COMPOUND_TEXT", 0, TARGET_STRING}, 
  {(char *)"UTF8_STRING",   0, TARGET_UTF8},    /* untested */
  {(char *)"text/uri-list", 0, TARGET_URL}
};

static Xen drop_hook;


#if HAVE_GTK_ADJUSTMENT_GET_UPPER
  #define SELECTION_DATA(Data)   (gtk_selection_data_get_data(Data))
  #define SELECTION_LENGTH(Data) (gtk_selection_data_get_length(Data))
  #define SELECTION_FORMAT(Data) (gtk_selection_data_get_format(Data))
#else
  #define SELECTION_DATA(Data)   (Data->data)
  #define SELECTION_LENGTH(Data) (Data->length)
  #define SELECTION_FORMAT(Data) (Data->format)
#endif

static void drag_data_received(GtkWidget *caller, GdkDragContext *context, gint mx, gint my, 
			       GtkSelectionData *data, guint info, guint time)
{
  /* data->target */
  if ((SELECTION_LENGTH(data) >= 0) && 
      (SELECTION_FORMAT(data) == 8))
    {
      gsize bread, bwritten;
      GError *error;
      char *str;

      if (info == TARGET_STRING)
	str = (char *)(SELECTION_DATA(data));
      else str = (char *)g_filename_from_utf8((gchar *)(SELECTION_DATA(data)), SELECTION_LENGTH(data), &bread, &bwritten, &error);

      if ((!(Xen_hook_has_list(drop_hook))) || 
	  (!(Xen_is_true(run_or_hook(drop_hook,
				    Xen_list_1(C_string_to_Xen_string(str)),
				    S_drop_hook)))))
	{
	  drop_watcher_t *d;
	  d = find_drop_watcher(caller);
	  if (d)
	    {
	      /* loop through possible list of filenames, calling watcher on each */
	      char *filename;
	      int len = 0, i, j = 0;
	      len = mus_strlen(str);
	      filename = (char *)calloc(len, sizeof(char));
	      for (i = 0; i < len; i++)
		{
		  if (isspace((int)str[i]))
		    {
		      if (j > 0)
			{
			  filename[j] = '\0';
			  if (strncmp(filename, "file://", 7) == 0)
			    {
			      char *tmp;
			      tmp = (char *)(filename + 7);
			      (*(d->drop_watcher))(caller, (const char *)tmp, mx, my, d->context);
			    }
			  else (*(d->drop_watcher))(caller, (const char *)filename, mx, my, d->context);
			  j = 0;
			}
		      /* else ignore extra white space chars */
		    }
		  else
		    {
		      filename[j++] = str[i];
		    }
		}
	      free(filename);
	    }
	}
      gtk_drag_finish (context, true, false, time);
      return;
    }
  gtk_drag_finish(context, false, false, time);
}


static void drag_leave(GtkWidget *w, GdkDragContext *context, guint time)
{
  drop_watcher_t *d;
  d = find_drop_watcher(w);
  if ((d) && (d->drag_watcher))
    (*(d->drag_watcher))(w, NULL, 0, 0, DRAG_LEAVE, d->context);
}
 

static gboolean drag_motion(GtkWidget *w, GdkDragContext *context, gint x, gint y, guint time)
{
  drop_watcher_t *d;
  d = find_drop_watcher(w);
  if ((d) && (d->drag_watcher))
    (*(d->drag_watcher))(w, NULL, x, y, DRAG_MOTION, d->context);
  return(true); /* this is what the examples return in gtk/tests/testdnd.c -- don't know what it means, if anything */
}
 

void add_drag_and_drop(GtkWidget *w, 
		       void (*drop_watcher)(GtkWidget *w, const char *message, int x, int y, void *data), 
		       void (*drag_watcher)(GtkWidget *w, const char *message, int x, int y, drag_style_t dtype, void *data), 
		       void *context)
{
  gtk_drag_dest_set(w, GTK_DEST_DEFAULT_ALL, target_table, 6, (GdkDragAction)(GDK_ACTION_COPY | GDK_ACTION_MOVE | GDK_ACTION_LINK));
  SG_SIGNAL_CONNECT(w, "drag_data_received", drag_data_received, NULL);
  SG_SIGNAL_CONNECT(w, "drag_motion", drag_motion, NULL);
  SG_SIGNAL_CONNECT(w, "drag_leave", drag_leave, NULL);
  add_drop_watcher(w, drop_watcher, drag_watcher, context);
}
 


void g_init_gxfile(void)
{
  #define H_drop_hook S_drop_hook " (name): called whenever Snd receives a drag-and-drop \
event. If it returns " PROC_TRUE ", the file is not opened by Snd."

  drop_hook = Xen_define_hook(S_drop_hook, "(make-hook 'name)", 1, H_drop_hook); 
}
