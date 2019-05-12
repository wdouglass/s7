#include "snd.h"

#if (!GTK_CHECK_VERSION(3, 89, 0))

#if GTK_CHECK_VERSION(3, 22, 0)
  static GdkWindow *last_window = NULL;
  static GdkDrawingContext *last_context = NULL;
#endif

#if GTK_CHECK_VERSION(3, 0, 0)
cairo_t *make_cairo(GdkWindow *win)
#else
cairo_t *make_cairo(GdkDrawable *win)
#endif
{
  ss->line_width = -1.0;
#if GTK_CHECK_VERSION(3, 22, 0)
  last_window = win;
  last_context = gdk_window_begin_draw_frame(win, gdk_window_get_visible_region(win));
  return(gdk_drawing_context_get_cairo_context(last_context));
#else
  return(gdk_cairo_create(win));
#endif
}

void free_cairo(cairo_t *cr)
{
  ss->line_width = -1.0;
#if GTK_CHECK_VERSION(3, 22, 0)
  gdk_window_end_draw_frame(last_window, last_context);
#else
  cairo_destroy(cr);
#endif
}
#endif

bool set_tiny_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  if ((font) && (font[0] == '/')) return(false); /* pango accepts bogus font names, but then cairo segfaults trying to get the font height */
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (tiny_font(ss)) free(tiny_font(ss));
      in_set_tiny_font(mus_strdup(font));
      if (TINY_FONT(ss)) pango_font_description_free(TINY_FONT(ss));
      TINY_FONT(ss) = fs;
      return(true);
    }
  return(false);
}


bool set_listener_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  if ((font) && (font[0] == '/')) return(false);
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (listener_font(ss)) free(listener_font(ss));
      in_set_listener_font(mus_strdup(font));
      if (LISTENER_FONT(ss)) pango_font_description_free(LISTENER_FONT(ss));
      LISTENER_FONT(ss) = fs;
      set_listener_text_font();
      return(true);
    }
  return(false);
}


bool set_peaks_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  if ((font) && (font[0] == '/')) return(false);
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (peaks_font(ss)) free(peaks_font(ss));
      in_set_peaks_font(mus_strdup(font));
      if (PEAKS_FONT(ss)) pango_font_description_free(PEAKS_FONT(ss));
      PEAKS_FONT(ss) = fs;
      return(true);
    }
  return(false);
}


bool set_bold_peaks_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  if ((font) && (font[0] == '/')) return(false);
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (bold_peaks_font(ss)) free(bold_peaks_font(ss));
      in_set_bold_peaks_font(mus_strdup(font));
      if (BOLD_PEAKS_FONT(ss)) pango_font_description_free(BOLD_PEAKS_FONT(ss));
      BOLD_PEAKS_FONT(ss) = fs;
      return(true);
    }
  return(false);
}


bool set_axis_label_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  if ((font) && (font[0] == '/')) return(false);
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (axis_label_font(ss)) free(axis_label_font(ss));
      in_set_axis_label_font(mus_strdup(font));
      if (AXIS_LABEL_FONT(ss)) pango_font_description_free(AXIS_LABEL_FONT(ss));
      AXIS_LABEL_FONT(ss) = fs;
      return(true);
    }
  return(false);
}


bool set_axis_numbers_font(const char *font)
{
  PangoFontDescription *fs = NULL;
  if ((font) && (font[0] == '/')) return(false);
  fs = pango_font_description_from_string(font);
  if (fs)
    {
      if (axis_numbers_font(ss)) free(axis_numbers_font(ss));
      in_set_axis_numbers_font(mus_strdup(font));
      if (AXIS_NUMBERS_FONT(ss)) pango_font_description_free(AXIS_NUMBERS_FONT(ss));
      AXIS_NUMBERS_FONT(ss) = fs;
      return(true);
    }
  return(false);
}


int sg_text_width(const char *txt, PangoFontDescription *font)
{
  int wid = 0;
  if (!txt) return(0);
  if (mus_strlen(txt) == 0) return(0);
  if (!(g_utf8_validate(txt, -1, NULL)))
    return(0);

#if (!GTK_CHECK_VERSION(3, 94, 0))
  {
    PangoLayout *layout = NULL;
    PangoContext *ctx;
    ctx = gdk_pango_context_get();
    layout = pango_layout_new(ctx);
    if (layout)
      {
	pango_layout_set_font_description(layout, font);
	pango_layout_set_text(layout, txt, -1);
	pango_layout_get_pixel_size(layout, &wid, NULL); /* huge (6MBytes!) memleak here */
	g_object_unref(G_OBJECT(layout));
      }
    g_object_unref(ctx);
  }
#endif
  return(wid);
}


int mark_name_width(const char *txt)
{
  if (txt)
    return(sg_text_width(txt, PEAKS_FONT(ss)));
  return(0);
}


int label_width(const char *txt, bool use_tiny_font)
{
  if (txt)
    return(sg_text_width(txt, (use_tiny_font) ? TINY_FONT(ss) : AXIS_LABEL_FONT(ss)));
  else return(0);
}


int number_width(const char *num, bool use_tiny_font)
{
  if (num)
    return(sg_text_width(num, (use_tiny_font) ? TINY_FONT(ss) : AXIS_NUMBERS_FONT(ss)));
  return(0);
}


#if 0
static int sg_font_width(PangoFontDescription *font)
{
  /* returns size in pixels */
  int wid;
  double dpi;

#if HAVE_GTK_LINK_BUTTON_NEW
  dpi = gdk_screen_get_resolution(gdk_display_get_default_screen(gdk_display_get_default())); /* pixels/inch */
#else
  dpi = 96.0; /* see below */
#endif

#if (!GTK_CHECK_VERSION(3, 94, 0))
  {
    PangoContext *ctx;
    PangoFontMetrics *m;
    ctx = gdk_pango_context_get();
    m = pango_context_get_metrics(ctx, font, gtk_get_default_language()); /* returns size in pango-scaled points (1024/72 inch) */
    wid = (int)((dpi / 72.0) * PANGO_PIXELS(pango_font_metrics_get_approximate_char_width(m)));
    pango_font_metrics_unref(m);
    g_object_unref(ctx);
  }
#else
  wid = (int)(dpi / 72.0);
#endif
  return(wid);
}
#endif


static int sg_font_height(PangoFontDescription *font)
{
  /* returns size in pixels */
  double dpi;
  int hgt;

#if HAVE_GTK_LINK_BUTTON_NEW
  /* gtk 2.1: gdk_display_get_default, gdk_display_get_default_screen */
  /* gtk 2.9: gdk_screen_get_resolution */
  dpi = gdk_screen_get_resolution(gdk_display_get_default_screen(gdk_display_get_default()));
#else
  dpi = 96.0; /* a plausible guess */
#endif

#if (!GTK_CHECK_VERSION(3, 94, 0))
  {
    PangoContext *ctx;
    PangoFontMetrics *m;
    ctx = gdk_pango_context_get();
    m = pango_context_get_metrics(ctx, font, gtk_get_default_language()); 
    hgt = (int)((dpi / 72.0) * PANGO_PIXELS(pango_font_metrics_get_ascent(m)));
    pango_font_metrics_unref(m);
    g_object_unref(ctx);
  }
#else
  hgt = (int)(dpi / 72.0);
#endif
      
  return(hgt);
}


static PangoFontDescription *last_tiny_font = NULL, *last_numbers_font = NULL, *last_label_font = NULL, *last_peaks_font = NULL;
static int last_numbers_height = 14, last_tiny_height = 10, last_label_height = 14, last_peaks_height = 10;

int number_height(PangoFontDescription *font)
{
  int hgt = 14;
  if (font == TINY_FONT(ss))
    {
      if (last_tiny_font == TINY_FONT(ss))
	return(last_tiny_height);
      hgt = sg_font_height(TINY_FONT(ss));
      last_tiny_font = TINY_FONT(ss);
      last_tiny_height = hgt;
    }
  else
    {
      if (font == AXIS_NUMBERS_FONT(ss))
	{
	  if (last_numbers_font == AXIS_NUMBERS_FONT(ss))
	    return(last_numbers_height);
	  hgt = sg_font_height(AXIS_NUMBERS_FONT(ss));
	  last_numbers_font = AXIS_NUMBERS_FONT(ss);
	  last_numbers_height = hgt;
	}
      else
	{
	  if (last_peaks_font == PEAKS_FONT(ss))
	    return(last_peaks_height);
	  hgt = sg_font_height(PEAKS_FONT(ss));
	  last_peaks_font = PEAKS_FONT(ss);
	  last_peaks_height = hgt;
	}
    }
  return(hgt);
}


int label_height(bool use_tiny_font)
{
  int hgt = 14;
  if (use_tiny_font)
    {
      if (last_tiny_font == TINY_FONT(ss))
	return(last_tiny_height);
      hgt = sg_font_height(TINY_FONT(ss));
      last_tiny_font = TINY_FONT(ss);
      last_tiny_height = hgt;
    }
  else
    {
      if (last_label_font == AXIS_LABEL_FONT(ss))
	return(last_label_height);
      hgt = sg_font_height(AXIS_LABEL_FONT(ss));
      last_label_font = AXIS_LABEL_FONT(ss);
      last_label_height = hgt;
    }
  return(hgt);
}


void clear_window(graphics_context *ax)
{
  /* if (ax) gdk_window_clear(ax->wn); */
}


void raise_dialog(GtkWidget *w)
{
  /* since we're using non-transient message dialogs, the dialog window can become completely
   * hidden behind other windows, with no easy way to raise it back to the top, so...
   */
  gtk_widget_show(w);
  gtk_window_present(GTK_WINDOW(w));
}


static void set_stock_button_label_1(gpointer w1, gpointer label)
{
  GtkWidget *w = (GtkWidget *)w1;
  if (GTK_IS_LABEL(w))
    {
      gtk_widget_hide(w);
      gtk_label_set_text(GTK_LABEL(w), (char *)label);
      gtk_widget_show(w);
    }
  else
    {
      if (GTK_IS_CONTAINER(w))
	g_list_foreach(gtk_container_get_children(GTK_CONTAINER(w)), set_stock_button_label_1, label);
    }
}


void set_stock_button_label(GtkWidget *w, const char *new_label)
{
  set_stock_button_label_1((gpointer)w, (gpointer)new_label);
}
	
	     
void set_button_label(GtkWidget *label, const char *str)
{
  GtkWidget *w;
  w = BIN_CHILD(label);
#if (GTK_CHECK_VERSION(3, 92, 0))
  if (GTK_IS_LABEL(w))
    gtk_label_set_text(GTK_LABEL(w), str);
  else
    {
      if (GTK_IS_ACCEL_LABEL(w))
	gtk_accel_label_set_label(GTK_ACCEL_LABEL(w), str);
      else fprintf(stderr, "unknown widget type in set_button_label\n");
    }
#else
  gtk_label_set_text(GTK_LABEL(w), str);
#endif
}


#if GTK_CHECK_VERSION(3, 10, 0)
static const char *icon_to_label(const char *label)
{
  /* these are the new-style labels in snd-g0.h */

  /* fprintf(stderr, "label: [%s]\n", label); */
  switch (label[0])
    {
    case 'A': case 'E': case 'O': case 'P': case 'S':
      return(label);
      
    case 'a': return("Exit");

    case 'd':
      if (mus_strcmp(label, "document-new")) return("New"); else
	if (mus_strcmp(label, "document-open")) return("Open"); else
	  if (mus_strcmp(label, "document-print")) return("Print"); else
	    if (mus_strcmp(label, "document-revert")) return("Revert"); else
	      if (mus_strcmp(label, "document-save")) return("Save"); else
		if (mus_strcmp(label, "document-save-as")) return("Save as");
      break;

    case 'e':
      if (mus_strcmp(label, "edit-clear")) return("Clear"); else
	if (mus_strcmp(label, "edit-copy")) return("Copy"); else
	  if (mus_strcmp(label, "edit-cut")) return("Cut"); else
	    if (mus_strcmp(label, "edit-find")) return("Find"); else
	      if (mus_strcmp(label, "edit-paste")) return("Paste"); else
		if (mus_strcmp(label, "edit-redo")) return("Redo"); else
		  if (mus_strcmp(label, "edit-select-all")) return("Select all"); else
		    if (mus_strcmp(label, "edit-undo")) return("Undo"); 
      break;

    case 'g':
      if (mus_strcmp(label, "go-first")) return("Go to start"); else
	if (mus_strcmp(label, "go-last")) return("Go to end"); else
	  if (mus_strcmp(label, "go-next")) return("Next"); else
	    if (mus_strcmp(label, "go-previous")) return("Previous"); 
      break;

    case 'h': return("Help");

    case 'm':
      if (mus_strcmp(label, "media-playback-start")) return("Play"); else
	if (mus_strcmp(label, "media-playback-stop")) return("Stop playing"); else
	  if (mus_strcmp(label, "media-playback-forward")) return("Play from cursor");
      break;

    case 'p': return("Stop");

    case 'v':
      if (mus_strcmp(label, "view-fullscreen")) return("Show all"); else
	if (mus_strcmp(label, "view-refresh")) return("Show again");
      break;

    case 'w': return("Close");

    case 'z':
      if (mus_strcmp(label, "zoom-in")) return("Zoom in"); else
	if (mus_strcmp(label, "zoom-out")) return("Zoom out");
      break;
    }
  return(label);
}

GtkWidget *button_new_with_icon(const gchar *label)
{
  return(gtk_button_new_with_label(icon_to_label(label)));
}
#endif


void set_label(GtkWidget *label, const char *str)
{
  gtk_label_set_text(GTK_LABEL(label), str);
}


void check_for_event(void)
{
  /* this is needed to force label updates and provide interrupts for long computations
   *
   *   Valgrind is confused about something here -- it thinks _XEnq malloc in XTranslateCoordinates in gdk_event_translate is never freed
   *   but this way of letting events run is used (for example) in gtktreeview.c and gtkwidget.c, so if it's wrong here...
   */

  int i = 0;
  if (ss->checking_explicitly) return;
  ss->checking_explicitly = true;
  while ((i < 100) && (gtk_events_pending()))
    {
      gtk_main_iteration();
      i++; /* don't hang! */
    }
  ss->checking_explicitly = false;
}


void set_title(const char *title)
{
  gtk_window_set_title(GTK_WINDOW(main_shell(ss)), title);
}


void goto_window(GtkWidget *text)
{
  gtk_widget_grab_focus(text);
}


/* try to keep track of colors */
void gc_set_foreground(gc_t *gp, color_info *color)
{
  gp->fg_color = color;
}


void gc_set_background(gc_t *gp, color_info *color)
{
  gp->bg_color = color;
}


void gc_set_colors(gc_t *gp, color_info *col1, color_info *col2)
{ 
  gp->fg_color = col1;
  gp->bg_color = col2;
}


gc_t *gc_new(void)
{
  gc_t *gp;
  gp = (gc_t *)calloc(1, sizeof(gc_t));
  return(gp);
}


void color_cursor(color_info *color)
{
  ss->cursor_color = color;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->cursor_color_symbol, Xen_wrap_pixel(color));
#endif
  gc_set_colors(ss->cursor_gc, color, ss->graph_color);
  gc_set_colors(ss->selected_cursor_gc, color, ss->selected_graph_color);
}


void color_marks(color_info *color)
{
  ss->mark_color = color;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->mark_color_symbol, Xen_wrap_pixel(color));
#endif
  gc_set_colors(ss->mark_gc, color, ss->graph_color);
  gc_set_colors(ss->selected_mark_gc, color, ss->selected_graph_color);
}


void color_selection(color_info *color)
{
  ss->selection_color = color;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->selection_color_symbol, Xen_wrap_pixel(color));
#endif
  gc_set_colors(ss->selection_gc, color, ss->graph_color);
  gc_set_colors(ss->selected_selection_gc, color, ss->selected_graph_color);
}


void color_graph(color_info *color)
{
  gc_set_background(ss->basic_gc, color);
  gc_set_foreground(ss->erase_gc, color);
  gc_set_colors(ss->selection_gc, ss->selection_color, color);
  gc_set_colors(ss->cursor_gc, ss->cursor_color, color);
  gc_set_colors(ss->mark_gc, ss->mark_color, color);
}


void color_selected_graph(color_info *color)
{
  gc_set_background(ss->selected_basic_gc, color);
  gc_set_foreground(ss->selected_erase_gc, color);
  gc_set_colors(ss->selected_selection_gc, ss->selection_color, color);
  gc_set_colors(ss->selected_cursor_gc, ss->cursor_color, color);
  gc_set_colors(ss->selected_mark_gc, ss->mark_color, color);
}


void color_data(color_info *color)
{
  gc_set_foreground(ss->basic_gc, color);
  gc_set_background(ss->erase_gc, color);
}


void color_selected_data(color_info *color)
{
  gc_set_foreground(ss->selected_basic_gc, color);
  gc_set_background(ss->selected_erase_gc, color);
}


void set_mix_color(color_info *color)
{
  ss->mix_color = color;
#if HAVE_SCHEME
  s7_symbol_set_value(s7, ss->mix_color_symbol, Xen_wrap_pixel(color));
#endif
  gc_set_foreground(ss->mix_gc, color);
}


color_t rgb_to_color(mus_float_t r, mus_float_t g, mus_float_t b)
{
  color_info *ccolor;
  ccolor = (color_info *)malloc(sizeof(color_info));
  ccolor->red = r;
  ccolor->green = g;
  ccolor->blue = b;
  ccolor->alpha = 1.0;
  return(ccolor);
}


#if (!GTK_CHECK_VERSION(3, 0, 0))
GdkColor *rgb_to_gdk_color(color_t col)
{
  GdkColor gcolor;
  GdkColor *ccolor;
  gcolor.red = (unsigned short)(col->red * 65535);
  gcolor.green = (unsigned short)(col->green * 65535);
  gcolor.blue = (unsigned short)(col->blue * 65535);
  ccolor = gdk_color_copy(&gcolor);
  /* gdk_rgb_find_color(gdk_colormap_get_system(), ccolor); */
  return(ccolor);
}
#endif


#if (!GTK_CHECK_VERSION(3, 92, 1))
void sg_widget_modify_bg(GtkWidget *w, GtkStateType type, color_t color)
{
  /* the color has to stick around??? */
  /* another stop-gap: allocate a color each time... */
#if (!GTK_CHECK_VERSION(3, 0, 0))
  gtk_widget_modify_bg(w, type, rgb_to_gdk_color(color));
#else
#if (!GTK_CHECK_VERSION(3, 16, 0))
  gtk_widget_override_background_color(w, GTK_STATE_FLAG_NORMAL, (GdkRGBA *)color);
#endif
#endif
}


void sg_widget_modify_fg(GtkWidget *w, GtkStateType type, color_t color)
{
#if (!GTK_CHECK_VERSION(3, 0, 0))
  gtk_widget_modify_fg(w, type, rgb_to_gdk_color(color));
#else
#if (!GTK_CHECK_VERSION(3, 16, 0))
  gtk_widget_override_color(w, GTK_STATE_FLAG_NORMAL, (GdkRGBA *)color);
#endif
#endif
}


void sg_widget_modify_base(GtkWidget *w, GtkStateType type, color_t color)
{
#if (!GTK_CHECK_VERSION(3, 0, 0))
  gtk_widget_modify_base(w, type, rgb_to_gdk_color(color));
#else
#if (!GTK_CHECK_VERSION(3, 16, 0))
  gtk_widget_override_background_color(w, GTK_STATE_FLAG_NORMAL, (GdkRGBA *)color);
#endif
#endif
}
#endif

#if (GTK_CHECK_VERSION(3, 92, 1))
guint sg_event_get_keyval(GdkEvent *e)
{
  guint val = 0;
  gdk_event_get_keyval(e, &val);
  return(val);
}

guint sg_event_get_button(const GdkEvent *e)
{
  guint val = 0;
  gdk_event_get_button(e, &val);
  return(val);
}
#endif


void recolor_graph(chan_info *cp, bool selected)
{
#if (!GTK_CHECK_VERSION(3, 92, 1))
  widget_modify_bg(channel_graph(cp), GTK_STATE_NORMAL, (selected) ? ss->selected_graph_color : ss->graph_color);
#endif
}


void set_sensitive(GtkWidget *wid, bool val) 
{
  if (wid) 
    gtk_widget_set_sensitive(wid, val);
}


void set_toggle_button(GtkWidget *wid, bool val, bool passed, void *data) 
{
  if (!passed) g_signal_handlers_block_matched((gpointer)wid, G_SIGNAL_MATCH_DATA, 0, 0, NULL, 0, (gpointer)data);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(wid), val);
  if (!passed) g_signal_handlers_unblock_matched((gpointer)wid, G_SIGNAL_MATCH_DATA, 0, 0, NULL, 0, (gpointer)data);
}


bool cursor_set_blinks(GtkWidget *w, bool blinks)
{
  GtkSettings *settings;
  settings = gtk_widget_get_settings(w);
  g_object_set(settings, "gtk-cursor-blink", (gboolean)blinks, NULL);
  return(blinks);
}


#if GTK_CHECK_VERSION(3, 0, 0)

int widget_height(GtkWidget *w)
{
  return(gtk_widget_get_allocated_height(w));
}


int widget_width(GtkWidget *w)
{
  return(gtk_widget_get_allocated_width(w));
}

#else

guint16 widget_height(GtkWidget *w)
{
  gint x, y;
  gdk_drawable_get_size(WIDGET_TO_WINDOW(w), &x, &y);
  return(y);
}


guint16 widget_width(GtkWidget *w)
{
  gint x, y;
  gdk_drawable_get_size(WIDGET_TO_WINDOW(w), &x, &y);
  return(x);
}
#endif


void set_widget_height(GtkWidget *w, guint16 height)
{
  set_widget_size(w, widget_width(w), height);
}


void set_widget_width(GtkWidget *w, guint16 width)
{
  set_widget_size(w, width, widget_height(w));
}


gint16 widget_x(GtkWidget *w)
{
  gint x, y;
#if GTK_CHECK_VERSION(3, 94, 0)
  gdk_surface_get_position(GDK_SURFACE(w), &x, &y);
#else
  gdk_window_get_position(WIDGET_TO_WINDOW(w), &x, &y);
#endif
  return(x);
}


gint16 widget_y(GtkWidget *w)
{
  gint x, y;
#if GTK_CHECK_VERSION(3, 94, 0)
  gdk_surface_get_position(GDK_SURFACE(w), &x, &y);
#else
  gdk_window_get_position(WIDGET_TO_WINDOW(w), &x, &y);
#endif
  return(y);
}


void set_widget_x(GtkWidget *w, gint16 x)
{
  gtk_window_move(GTK_WINDOW(w), x, widget_y(w));
}


void set_widget_y(GtkWidget *w, gint16 y)
{
  gtk_window_move(GTK_WINDOW(w), widget_x(w), y);
}


void set_widget_size(GtkWidget *w, guint16 width, guint16 height)
{
#if GTK_CHECK_VERSION(3, 94, 0)
  gtk_window_resize(WIDGET_TO_WINDOW(w), width, height);
#else
  gdk_window_resize(WIDGET_TO_WINDOW(w), width, height);
#endif
#if 0
  /* This one doesn't do anything, and prints out errors. */
  gtk_window_resize(GTK_WINDOW(w), width, height);
#endif
}


void set_widget_position(GtkWidget *w, gint16 x, gint16 y)
{
  gtk_window_move(GTK_WINDOW(w), x, y);
}


void set_user_data(GObject *obj, gpointer data)
{
  g_object_set_data(obj, "snd-data", data);
}


gpointer get_user_data(GObject *obj)
{
  return(g_object_get_data(obj, "snd-data"));
}


void set_user_int_data(GObject *obj, int data)
{
  int *gdata;
  gdata = (int *)malloc(sizeof(int));
  gdata[0] = data;
  g_object_set_data(obj, "snd-data", (gpointer)gdata);
}


int get_user_int_data(GObject *obj)
{
  gpointer gdata;
  gdata = g_object_get_data(obj, "snd-data");
  return(((int *)gdata)[0]);
}


void reset_user_int_data(GObject *obj, int data)
{
  gpointer gdata;
  gdata = g_object_get_data(obj, "snd-data");
  ((int *)gdata)[0] = data;
}


char *sg_get_text(GtkWidget *w, int start, int end) /* g_free result */
{
  GtkTextIter s, e;
  GtkTextBuffer *buf;
  buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
  gtk_text_buffer_get_iter_at_offset(buf, &s, start);
  gtk_text_buffer_get_iter_at_offset(buf, &e, end);
  return(gtk_text_buffer_get_text(buf, &s, &e, true));
}


void sg_text_insert(GtkWidget *w, const char *text)
{
  if (text)
    {
      GtkTextIter pos;
      GtkTextBuffer *buf;
      buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
      gtk_text_buffer_get_end_iter(buf, &pos);
      gtk_text_buffer_insert(buf, &pos, text, strlen(text));
    }
  else gtk_text_buffer_set_text(gtk_text_view_get_buffer(GTK_TEXT_VIEW(w)), "", 0);
}


GtkWidget *make_scrolled_text(GtkWidget *parent, bool editable, int add_choice, bool resize)
{
  /* returns new text widget */
  GtkWidget *sw, *new_text;
  GtkTextBuffer *buf;

  sw = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(sw), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  /* gtk_scrolled_window_set_min_content_height(GTK_SCROLLED_WINDOW(sw), 1000);
   *    seems to be a no-op -- I think they are maxing this against the current contents window size (see below)
   */

  new_text = gtk_text_view_new();
  buf = gtk_text_buffer_new(NULL);

  gtk_text_view_set_buffer(GTK_TEXT_VIEW(new_text), buf);
  gtk_text_view_set_editable(GTK_TEXT_VIEW(new_text), editable);
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(new_text), GTK_WRAP_NONE);
  gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(new_text), editable);
  gtk_text_view_set_left_margin(GTK_TEXT_VIEW(new_text), 4);
  gtk_container_add(GTK_CONTAINER(sw), new_text);
#if (!GTK_CHECK_VERSION(3, 92, 1))
  if (editable) sg_widget_set_events(new_text, GDK_ALL_EVENTS_MASK);
#endif
  gtk_widget_show(new_text);

  switch (add_choice)
    {
    case 0: 
      gtk_container_add(GTK_CONTAINER(parent), sw);
      break;
    case 1:
      gtk_paned_pack2(GTK_PANED(parent), sw, resize, true);
      break;
    case 2:
    default:
      sg_box_pack_start(GTK_BOX(parent), sw, true, true, 0);
      break;
    }
  gtk_widget_show(sw);

  return(new_text);
}


void sg_make_resizable(GtkWidget *w)
{
  if (GTK_IS_DIALOG(w))
    {
      gtk_window_set_default_size(GTK_WINDOW(w), -1, -1);
      gtk_window_set_resizable(GTK_WINDOW(w), true);
    }
}


idle_t add_work_proc(GSourceFunc func, gpointer data)
{
  /* during auto-testing I need to force the background procs to run to completion */
  if (with_background_processes(ss))
    return(g_idle_add_full(G_PRIORITY_DEFAULT_IDLE, func, data, NULL));
  else
    {
      while (((*func)(data)) == BACKGROUND_CONTINUE) {};
      return((idle_t)0);
    }
}


GtkWidget *snd_gtk_dialog_new(void)
{
  GtkWidget *w;
  w = gtk_dialog_new();
#if GTK_CHECK_VERSION(3, 14, 0)
  gtk_window_set_transient_for(GTK_WINDOW(w), GTK_WINDOW(main_shell(ss)));
#endif
  add_dialog_style(w);
  g_object_ref(w); 
  return(w);
}


GtkWidget *snd_gtk_highlight_label_new(const char *label)
{
  char *str;
  GtkWidget *w;

  w = gtk_label_new(label);
  str = mus_format("<b>%s</b>", label);
  gtk_label_set_markup(GTK_LABEL(w), str);
  gtk_label_set_use_markup(GTK_LABEL(w), true);
  free(str);

  return(w);
}


void widget_int_to_text(GtkWidget *w, int val)
{
  char *str;
  str = (char *)calloc(8, sizeof(char));
  snprintf(str, 8, "%d", val);
  gtk_entry_set_text(GTK_ENTRY(w), str);
  free(str);
}


void widget_mus_long_t_to_text(GtkWidget *w, mus_long_t val)
{
  char *str;
  str = (char *)calloc(8, sizeof(char));
  snprintf(str, 8, "%" print_mus_long, val);
  gtk_entry_set_text(GTK_ENTRY(w), str);
  free(str);
}

#if HAVE_GTK_ADJUSTMENT_GET_UPPER
  #define ADJUSTMENT_LOWER(Adjust)                gtk_adjustment_get_lower(GTK_ADJUSTMENT(Adjust))
  #define ADJUSTMENT_UPPER(Adjust)                gtk_adjustment_get_upper(GTK_ADJUSTMENT(Adjust))
#else
  #define ADJUSTMENT_LOWER(Adjust)                ((GTK_ADJUSTMENT(Adjust))->lower)
  #define ADJUSTMENT_UPPER(Adjust)                ((GTK_ADJUSTMENT(Adjust))->upper)
#endif

void ensure_scrolled_window_row_visible(widget_t list, int row, int num_rows)
{
  /* view files file list */
  /*   called in snd-file.c on vdat->file_list which is a vbox; its parent is a viewport */
  /* also used in slist_moveto below */
  GtkWidget *parent;
  GtkAdjustment *v;
  gdouble maximum, size, new_value, minimum;

  parent = gtk_widget_get_parent(list);
#if GTK_CHECK_VERSION(3, 0, 0)
  v = gtk_scrollable_get_vadjustment(GTK_SCROLLABLE(parent));
#else
  v = gtk_viewport_get_vadjustment(GTK_VIEWPORT(parent));
#endif
  maximum = ADJUSTMENT_UPPER(v);
  minimum = ADJUSTMENT_LOWER(v);
  size = ADJUSTMENT_PAGE_SIZE(v);
  maximum -= size;
  if (row == 0)
    new_value = 0.0;
  else
    {
      if (row >= (num_rows - 1))
	new_value = maximum;
      else new_value = ((row + 0.5) * ((maximum - minimum) / (double)(num_rows - 1)));
    }
  if (new_value != ADJUSTMENT_VALUE(v))
    ADJUSTMENT_SET_VALUE(v, new_value);
}

  


/* ---------------- scrolled list replacement ---------------- */

static int slist_row(GtkWidget *item);
static void slist_set_row(GtkWidget *item, int row);

static void slist_item_clicked(GtkWidget *w, gpointer gp)
{
  slist *lst = (slist *)gp;
  slist_select(lst, slist_row(w));
  if (lst->select_callback)
    (*(lst->select_callback))((const char *)gtk_button_get_label(GTK_BUTTON(w)), /* do not free this!! */
			      slist_row(w),
			      lst->select_callback_data);
}


static gboolean slist_item_button_pressed(GtkWidget *w, GdkEventButton *ev, gpointer data)
{
  slist *lst = (slist *)data;
  if (lst->button_press_callback)
    return((*(lst->button_press_callback))(ev, lst->button_press_callback_data));
  return(false);
}


#if (GTK_CHECK_VERSION(3, 0, 0))
static GtkCssProvider *wb_provider, *listener_provider, *dialog_provider, *hl_provider, *tb_provider, *mu_provider;
static GtkCssProvider *rsc_provider, *gsc_provider, *bsc_provider, *pd_provider, *cb_provider, *entry_provider;
static GtkCssProvider *cl_provider;

void add_white_button_style(GtkWidget *w)
{
  GtkStyleContext *c;
  c = gtk_widget_get_style_context(w);
  gtk_style_context_add_provider(c, GTK_STYLE_PROVIDER(wb_provider), G_MAXUINT);
  gtk_widget_set_name(w, "white_button");
}

void add_highlight_button_style(GtkWidget *w)
{
  GtkStyleContext *c;
  c = gtk_widget_get_style_context(w);
  gtk_style_context_add_provider(c, GTK_STYLE_PROVIDER(hl_provider), G_MAXUINT);
  gtk_widget_set_name(w, "highlight_button");
}

void add_center_button_style(GtkWidget *w)
{
  GtkStyleContext *c;
  c = gtk_widget_get_style_context(w);
  gtk_style_context_add_provider(c, GTK_STYLE_PROVIDER(cl_provider), G_MAXUINT);
}

void add_listener_style(GtkWidget *w)
{
  GtkStyleContext *c;
  c = gtk_widget_get_style_context(w);
  gtk_style_context_add_provider(c, GTK_STYLE_PROVIDER(listener_provider), G_MAXUINT);
  gtk_widget_set_name(w, "listener_text");
}

void add_dialog_style(GtkWidget *w)
{
  GtkStyleContext *c;
  c = gtk_widget_get_style_context(w);
  gtk_style_context_add_provider(c, GTK_STYLE_PROVIDER(dialog_provider), G_MAXUINT);
}

void add_toolbar_style(GtkWidget *w)
{
  GtkStyleContext *c;
  c = gtk_widget_get_style_context(w);
  gtk_style_context_add_provider(c, GTK_STYLE_PROVIDER(tb_provider), G_MAXUINT);
#if 0
  /* add_toolbar_style is incompatible with GtkCallback */
  if (GTK_IS_CONTAINER(w)) gtk_container_forall(GTK_CONTAINER(w), (GtkCallback)add_toolbar_style, tb_provider);
#endif
}

void add_menu_style(GtkWidget *w)
{
  GtkStyleContext *c;
  c = gtk_widget_get_style_context(w);
  gtk_style_context_add_provider(c, GTK_STYLE_PROVIDER(mu_provider), G_MAXUINT);
}

void add_paned_style(GtkWidget *w)
{
#if 0
  GtkStyleContext *c;
  c = gtk_widget_get_style_context(w);
  gtk_style_context_add_provider(c, GTK_STYLE_PROVIDER(pd_provider), G_MAXUINT);
#endif
}

void add_red_scale_style(GtkWidget *w)
{
  GtkStyleContext *c;
  c = gtk_widget_get_style_context(w);
  gtk_style_context_add_provider(c, GTK_STYLE_PROVIDER(rsc_provider), G_MAXUINT);
}

void add_green_scale_style(GtkWidget *w)
{
  GtkStyleContext *c;
  c = gtk_widget_get_style_context(w);
  gtk_style_context_add_provider(c, GTK_STYLE_PROVIDER(gsc_provider), G_MAXUINT);
}

void add_blue_scale_style(GtkWidget *w)
{
  GtkStyleContext *c;
  c = gtk_widget_get_style_context(w);
  gtk_style_context_add_provider(c, GTK_STYLE_PROVIDER(bsc_provider), G_MAXUINT);
}

void add_check_button_style(GtkWidget *w)
{
  GtkStyleContext *c;
  c = gtk_widget_get_style_context(w);
  gtk_style_context_add_provider(c, GTK_STYLE_PROVIDER(cb_provider), G_MAXUINT);
}

void add_entry_style(GtkWidget *w)
{
  GtkStyleContext *c;
  c = gtk_widget_get_style_context(w);
  gtk_style_context_add_provider(c, GTK_STYLE_PROVIDER(entry_provider), G_MAXUINT);
}

#else
void add_red_scale_style(GtkWidget *w) {}
void add_green_scale_style(GtkWidget *w) {}
void add_blue_scale_style(GtkWidget *w) {}
void add_toolbar_style(GtkWidget *w) {}
void add_menu_style(GtkWidget *w) {}
void add_paned_style(GtkWidget *w) {}
void add_highlight_button_style(GtkWidget *w) {}
void add_center_button_style(GtkWidget *w) {}
void add_white_button_style(GtkWidget *w) {gtk_widget_set_name(w, "white_button");}
void add_listener_style(GtkWidget *w) {gtk_widget_set_name(w, "listener_text");}
void add_dialog_style(GtkWidget *w) {}
void add_check_button_style(GtkWidget *w) {}
void add_entry_style(GtkWidget *w) {}
#endif

static GtkWidget *slist_new_item(slist *lst, const char *label, int row)
{
  GtkWidget *item;

  item = gtk_button_new_with_label(label);
  slist_set_row(item, row);
  gtk_button_set_relief(GTK_BUTTON(item), GTK_RELIEF_NONE);
#if GTK_CHECK_VERSION(3, 14, 0)
  gtk_widget_set_halign(GTK_WIDGET(item), GTK_ALIGN_START);
#else
  gtk_button_set_alignment(GTK_BUTTON(item), 0.05, 1.0);
#endif
  sg_box_pack_start(GTK_BOX(lst->topics), item, false, false, 0);

#if (!GTK_CHECK_VERSION(3, 92, 1))
  widget_modify_bg(item, GTK_STATE_NORMAL, ss->white);
  widget_modify_bg(item, GTK_STATE_PRELIGHT, ss->light_blue);
#endif
  add_white_button_style(item);

  SG_SIGNAL_CONNECT(item, "clicked", slist_item_clicked, (gpointer)lst);
  SG_SIGNAL_CONNECT(item, "button_press_event", slist_item_button_pressed, (gpointer)lst);

  gtk_widget_show(item);
  return(item);
}


slist *slist_new_with_title_and_table_data(const char *title,
					   GtkWidget *parent, 
					   const char **initial_items, 
					   int num_items, 
					   widget_add_t paned,
					   int t1, int t2, int t3, int t4)
{
  slist *lst;
  GtkWidget *topw = NULL;
  lst = (slist *)calloc(1, sizeof(slist));
  lst->selected_item = SLIST_NO_ITEM_SELECTED;

  if (title)
    {
      lst->box = gtk_vbox_new(false, 0);
      widget_set_vexpand(lst->box, true);

      lst->label = snd_gtk_highlight_label_new(title);
      sg_box_pack_start(GTK_BOX(lst->box), lst->label, false, false, 0);
      topw = lst->box;
    }

  lst->topics = gtk_vbox_new(false, 2); /* sets list item vertical spacing */
  widget_set_vexpand(lst->topics, true);
  lst->scroller = gtk_scrolled_window_new(NULL, NULL);

  if (!title) 
    topw = lst->scroller;
  else sg_box_pack_start(GTK_BOX(lst->box), lst->scroller, true, true, 0);

  switch (paned)
    {
    case PANED_ADD1: 
      gtk_paned_add1(GTK_PANED(parent), topw);
      break;

    case BOX_PACK: 
      sg_box_pack_start(GTK_BOX(parent), topw, true, true, 4); 
      break;

    case TABLE_ATTACH: 
      gtk_table_attach(GTK_TABLE(parent), topw, t1, t2, t3, t4,
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND), 
		       (GtkAttachOptions)(GTK_FILL | GTK_EXPAND | GTK_SHRINK), 
		       0, 0);
      break;

    case CONTAINER_ADD: 
      gtk_container_add(GTK_CONTAINER(parent), topw); 
      break;
    }

  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(lst->scroller), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
#if HAVE_GTK_HEADER_BAR_NEW
  gtk_container_add(GTK_CONTAINER(lst->scroller), lst->topics);
#else
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(lst->scroller), lst->topics);
#endif

  if (title)
    {
      gtk_widget_show(lst->label);
      gtk_widget_show(lst->box);
    }
  gtk_widget_show(lst->topics);
  gtk_widget_show(lst->scroller);

  if (num_items > 0)
    {
      int i;
      lst->items = (GtkWidget **)calloc(num_items, sizeof(GtkWidget *));
      lst->items_size = num_items;
      lst->num_items = num_items;
      for (i = 0; i < num_items; i++)
	lst->items[i] = slist_new_item(lst, initial_items[i], i);
    }

  /* gtk_scrolled_window_set_min_content_height(GTK_SCROLLED_WINDOW(lst->scroller), 200); 
   *    this actually works!
   */

  return(lst);
}


slist *slist_new(GtkWidget *parent, const char **initial_items, int num_items, widget_add_t paned)
{
  return(slist_new_with_title_and_table_data(NULL, parent, initial_items, num_items, paned, 0, 0, 0, 0));
}


slist *slist_new_with_title(const char *title, GtkWidget *parent, const char **initial_items, int num_items, widget_add_t paned)
{
  return(slist_new_with_title_and_table_data(title, parent, initial_items, num_items, paned, 0, 0, 0, 0));
}


void slist_clear(slist *lst)
{
  int i;
  for (i = 0; i < lst->items_size; i++)
    if (lst->items[i])
      {
	gtk_widget_hide(lst->items[i]);
	gtk_button_set_label(GTK_BUTTON(lst->items[i]), " ");
      }
  lst->num_items = 0;
#if (!GTK_CHECK_VERSION(3, 92, 1))
  if (lst->selected_item != SLIST_NO_ITEM_SELECTED)
    widget_modify_bg(lst->items[lst->selected_item], GTK_STATE_NORMAL, ss->white);
#endif
  lst->selected_item = SLIST_NO_ITEM_SELECTED;
}


static int slist_row(GtkWidget *item)
{
  gpointer gdata;
  gdata = g_object_get_data(G_OBJECT(item), "slist-row");
  return(((int *)gdata)[0]);
}


static void slist_set_row(GtkWidget *item, int row)
{
  int *gdata;
  gdata = (int *)malloc(sizeof(int));
  gdata[0] = row;
  g_object_set_data(G_OBJECT(item), "slist-row", (gpointer)gdata);
}


#define INITIAL_SLIST_LENGTH 8

void slist_append(slist *lst, const char *name)
{
  int loc = 0;
  if ((!name) || 
      (!g_utf8_validate(name, -1, NULL)))
    return;

  if (lst->items_size == 0)
    {
      lst->items = (GtkWidget **)calloc(INITIAL_SLIST_LENGTH, sizeof(GtkWidget *));
      lst->items_size = INITIAL_SLIST_LENGTH;
      lst->num_items = 0;
    }
  if (lst->num_items == lst->items_size)
    {
      int i;
      lst->items_size += INITIAL_SLIST_LENGTH;
      lst->items = (GtkWidget **)realloc(lst->items, lst->items_size * sizeof(GtkWidget *));
      for (i = lst->num_items; i < lst->items_size; i++) lst->items[i] = NULL;
    }
  loc = lst->num_items++;
  if (!lst->items[loc])
    lst->items[loc] = slist_new_item(lst, name, loc);
  else 
    {
      gtk_button_set_label(GTK_BUTTON(lst->items[loc]), name); /* gtkbutton.c strdups name */
      gtk_widget_show(lst->items[loc]);
    }
}


void slist_moveto(slist *lst, int row)
{
  ensure_scrolled_window_row_visible(lst->topics, row, lst->num_items);
}


void slist_select(slist *lst, int row)
{
#if (!GTK_CHECK_VERSION(3, 92, 1))
  if (lst->selected_item != SLIST_NO_ITEM_SELECTED)
    widget_modify_bg(lst->items[lst->selected_item], GTK_STATE_NORMAL, ss->white);
  if (row != SLIST_NO_ITEM_SELECTED)
    widget_modify_bg(lst->items[row], GTK_STATE_NORMAL, ss->light_blue);
#endif
  lst->selected_item = row;
}


#if (!GTK_CHECK_VERSION(3, 0, 0))
void init_gtk(void)
{
  gtk_rc_parse_string("\n\
\n					 \
style \"default\"\n			 \
{\n					 \
  fg[NORMAL]      = { 0.0,  0.00, 0.0 }\n	\
  text[NORMAL]    = { 0.0,  0.0,  0.0 }\n	\
  bg[NORMAL]      = { 0.96, 0.96, 0.90 }\n	\
  bg[ACTIVE]      = { 0.80, 0.80, 0.75 }\n	\
  bg[INSENSITIVE] = { 0.96, 0.96, 0.90 }\n	\
  base[NORMAL]    = { 1.00, 1.00, 1.00 }\n	\
  bg[PRELIGHT]    = { 0.70, 0.70, 0.64 }\n	\
  fg[PRELIGHT]    = { 1.0,  0.0,  0.0}\n	\
  GtkPaned::handle_size = 6\n			\
  xthickness = 1\n				\
  ythickness = 1\n				\
}\n						\
style \"default_button\" = \"default\"\n	\
{\n						\
  GtkButton::default_border = { 1, 0, 1, 0 }\n		\
  GtkButton::default_outside_border = { 1, 0, 1, 0 }\n	\
  GtkButton::inner_border = { 1, 0, 1, 0 }\n		\
  GtkButton::focus_line_width = 0\n			\
  GtkButton::focus_padding = 0\n			\
}\n							\
style \"default_pane\" = \"default\"\n			\
{\n							\
  bg[NORMAL] = { 0.56, 0.93, 0.56 }\n			\
  bg[PRELIGHT] = { 0.26, 0.8, 0.26}\n			\
}\n							\
style \"default_entry\" = \"default\"\n			\
{\n							\
  base[ACTIVE]      = { 0.96, 0.96, 0.90 }\n		\
  base[SELECTED]    = { 0.80, 0.80, 0.75 }\n		\
  base[PRELIGHT]    = { 1.0, 1.0, 1.0}\n		\
  base[NORMAL]      = { 0.96, 0.96, 0.90 }\n		\
  base[INSENSITIVE] = { 0.96, 0.96, 0.90 }\n		\
  bg[ACTIVE]        = { 1.0, 1.0, 1.0 }\n		\
  bg[SELECTED]      = { 1.0, 1.0, 1.0 }\n		\
  bg[PRELIGHT]      = { 1.0, 1.0, 1.0 }\n		\
  text[ACTIVE]      = { 0.0, 0.0, 0.0 }\n		\
  text[SELECTED]    = { 0.0, 0.0, 0.0 }\n		\
  text[PRELIGHT]    = { 0.0, 0.0, 0.0 }\n		\
}\n							\
style \"default_text\" = \"default_entry\"\n		\
{\n							\
  base[NORMAL] = { 1.0, 1.0, 1.0 }\n			\
}\n							\
style \"default_slider\" = \"default\"\n		\
{\n							\
  bg[NORMAL] = { 0.90, 0.90, 0.85 }\n			\
  bg[ACTIVE] = { 0.70, 0.70, 0.64 }\n			\
  bg[PRELIGHT] = { 0.90, 0.90, 0.85 }\n			\
  GtkRange::slider_width = 13\n				\
  GtkRange::stepper_size = 10\n				\
}\n							\
style \"prefs_scale\" = \"default_slider\"\n            \
{\n                                                     \
  GtkScale::slider-length = 24\n                        \
}\n                                                     \
widget \"*.prefs_color_scale\" style \"prefs_scale\"\n  \
style \"default_frame\" = \"default\"\n			\
{\n							\
  xthickness = 4\n					\
  ythickness = 4\n					\
}\n							\
class \"GtkWidget\" style \"default\"\n			\
class \"GtkButton\" style \"default_button\"\n		\
class \"GtkEntry\" style \"default_entry\"\n		\
class \"GtkTextView\" style \"default_text\"\n		\
class \"GtkPaned\" style \"default_pane\"\n		\
class \"GtkRange\" style \"default_slider\"\n		\
class \"GtkFrame\" style \"default_frame\"\n		\
\n							\
style \"zoom_slider\" = \"default_slider\"\n		\
{\n							\
  bg[NORMAL] = { 0.70, 0.70, 0.64 }\n			\
  bg[ACTIVE] = { 0.54, 0.54, 0.51 }\n			\
  bg[PRELIGHT] = { 0.70, 0.70, 0.64 }\n			\
\n							\
  GtkRange::slider_width = 12\n				\
  GtkRange::stepper_size = 12\n				\
}\n							\
widget \"*.zx_slider\" style \"zoom_slider\"\n		\
widget \"*.zy_slider\" style \"zoom_slider\"\n		\
widget \"*.gzy_slider\" style \"zoom_slider\"\n		\
style \"default_tree_view\" = \"default\"\n		\
{\n							\
 GtkTreeView::odd-row-color = { 0.94, 0.97, 1.0 }\n	\
 GtkTreeView::even-row-color = { 1.0, 1.0, 1.0 }\n	\
}\n							\
class \"GtkTreeView\" style \"default_tree_view\"\n	\
style \"dialog_button\" = \"default_button\"\n          \
{\n                                                     \
  bg[NORMAL] = { 1.0, 1.0, 0.94 }\n                     \
  bg[PRELIGHT] = { 1.0, 1.0, 0.94 }\n                   \
}\n                                                     \
widget \"*.dialog_button\" style \"dialog_button\"\n    \
style \"white_button\" = \"default_button\"\n		\
{\n							\
  bg[NORMAL] = { 1.0, 1.0, 1.0 }\n			\
  bg[PRELIGHT] = { 0.94, 0.97, 1.0 }\n			\
  fg[PRELIGHT] = { 0.0,  0.0,  0.0}\n			\
  GtkButton::default_border = { 0, 0, 0, 0 }\n		\
  GtkButton::default_outside_border = { 0, 0, 0, 0 }\n	\
  GtkButton::inner_border = { 0, 0, 0, 0 }\n		\
  GtkButton::focus_line_width = 0\n			\
  GtkButton::focus_padding = 0\n			\
  xthickness = 0\n					\
  ythickness = 0\n					\
}\n							\
widget \"*.white_button\" style \"white_button\"\n");
}

#else
/* -------------------------------------------------------------------------------- */
#if (GTK_CHECK_VERSION(3, 89, 0))
/* border-width and -gtk-gradient have been removed(??); as far as I can tell, this stuff does not work 
 *   see gtkcssstylepropertyimpl.c gtk_css_style_property_register for properties ("background-color" etc)
 */
void init_gtk(void)
{
#if (GTK_CHECK_VERSION(3, 93, 0))
  GtkCssProvider *dp;
  dp = gtk_css_provider_new();
  gtk_style_context_add_provider_for_display(gdk_display_get_default(), GTK_STYLE_PROVIDER(dp), GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  /* copied from gtk-3.96.0/tests/testwidgetfocus.c */
#endif

  wb_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(wb_provider),
    "GtkButton#white_button { \n"
    "  padding: 0px;\n"
    "  background-color: #ffffff;\n"
    "}\n"
    "GtkButton#white_button:hover { \n"
    "}\n",
    -1);

  hl_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(hl_provider),
    "GtkEventBox, GtkButton#highlight_button { \n"
    "  padding: 0px;\n"
    "  background-color: #fffff0;\n"
    "}\n"
    "GtkButton#highlight_button:hover { \n"
    "}\n",
    -1);

  cl_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(cl_provider),
    "GtkEventBox, GtkButton { \n"
    "  padding: 0px;\n"
    "  background-color: #fffff0;\n"
    "}\n"
    "GtkButton:hover { \n"
    "}\n",
    -1);

  listener_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(listener_provider),
    "#listener_text { \n"
     "  background-color: #ffffff;\n"
     "  color: #000000;\n"
    "}\n"
    "#listener_text:selected { \n"
     "  background-color: darker(rgb(240, 248, 255));\n"
     "  color: #000000;\n"
    "}\n",
    -1);

  entry_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(entry_provider),
    "GtkEntry:selected { \n"
    "  background-color: darker(rgb(240, 248, 255));\n"
    "  color: #000000;\n"
    "}\n",
    -1);

  dialog_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(dialog_provider),
    "GtkDialog { \n"
    "}\n",
    -1);

  tb_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(tb_provider),
    "GtkToolbar, GtkToolButton, GtkToolItem { \n"
    "  border-color: #fffff0;\n"				  
    "  padding: 8px;\n"
    "}\n",
    -1);
  /* the 8px here refers to the whole bar, not each entry */

  mu_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(mu_provider),
    "GtkMenuBar, GtkMenu, GtkMenuItem { \n"
    "  border-color: #fffff0;\n"				  
    "  background-color: #fffff0;\n"
    "  padding: 4px;\n"
    "}\n",
    -1);

  rsc_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(rsc_provider),
    "GtkScale { \n"
    "}\n",
    -1);

  gsc_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(gsc_provider),
    "GtkScale { \n"
    "}\n",
    -1);

  bsc_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(bsc_provider),
    "GtkScale { \n"
    "}\n",
    -1);

  /* I wanted to make the handle larger and set its color to #90ee90 -- no way! */
  pd_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(pd_provider),
    "GtkPaned { \n"
    "  background-color: #fffff0;\n"
    "}\n",
    -1);

  cb_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(cb_provider),
    "GtkRadioButton:hover, GtkCheckButton:hover { \n"
    "  background-color: rgb(200, 200, 190);\n"
    "}\n",
    -1);
}
#else
/* -------------------------------------------------------------------------------- */
void init_gtk(void)
{
#if 0
  /* this makes no difference */
  GtkCssProvider *provider;
  GdkDisplay *display;
  GdkScreen *screen;
  provider = gtk_css_provider_new();
  display = gdk_display_get_default();
  screen = gdk_display_get_default_screen(display);
  gtk_style_context_add_provider_for_screen(screen, GTK_STYLE_PROVIDER(provider), GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
#endif

  wb_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(wb_provider),
    "GtkButton#white_button { \n"
    "  padding-top: 0px;\n"
    "  padding-bottom: 0px;\n"
    "  border-width: 0px;\n"
    "  background-color: #ffffff;\n"
    "}\n"
#if (!GTK_CHECK_VERSION(3, 18, 8))
    "GtkButton#white_button:prelight { \n"
#else
    "GtkButton#white_button:hover { \n"
#endif
    "  background-image: -gtk-gradient (linear, left top, right bottom, from(#ffffff), to(rgb(200, 225, 255)));\n"
    "}\n",
    -1, NULL);

  hl_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(hl_provider),
    "GtkEventBox, GtkButton#highlight_button { \n"
    "  padding-top: 0px;\n"
    "  padding-bottom: 0px;\n"
    "  border-width: 0px;\n"
    "  background-color: #fffff0;\n"
    "}\n"
#if (!GTK_CHECK_VERSION(3, 18, 8))
    "GtkButton#highlight_button:prelight { \n"
#else
    "GtkButton#highlight_button:hover { \n"
#endif
    "  background-image: -gtk-gradient (linear, left top, right bottom, from(#fffff0), to(rgb(200, 225, 255)));\n"
    "}\n",
    -1, NULL);

  cl_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(cl_provider),
    "GtkEventBox, GtkButton { \n"
    "  padding-top: 0px;\n"
    "  padding-bottom: 0px;\n"
    "  padding-left: 8px;\n"
    "  padding-right: 8px;\n"
    "  border-width: 1px;\n"
    "  border-color: gray;\n"
    "  background-color: #fffff0;\n"
    "}\n"
#if (!GTK_CHECK_VERSION(3, 18, 8))
    "GtkButton:prelight { \n"
#else
    "GtkButton:hover { \n"
#endif
    "  background-image: -gtk-gradient (linear, left top, right bottom, from(#fffff0), to(rgb(200, 225, 255)));\n"
    "}\n",
    -1, NULL);

  listener_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(listener_provider),
    "#listener_text { \n"
     /* "  background-color: rgb(240, 248, 255);\n" */
     "  background-color: #ffffff;\n"
     "  color: #000000;\n"
    "}\n"
    "#listener_text:selected { \n"
     "  background-color: darker(rgb(240, 248, 255));\n"
     "  color: #000000;\n"
    "}\n",
    -1, NULL);

  entry_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(entry_provider),
    "GtkEntry:selected { \n"
    "  background-color: darker(rgb(240, 248, 255));\n"
     "  color: #000000;\n"
    "}\n",
    -1, NULL);

  dialog_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(dialog_provider),
    "GtkDialog { \n"
    "  background-image: -gtk-gradient (linear, left top, right bottom, from(rgb(250, 250, 230)), to(rgb(235, 235, 210)));\n"
    "}\n",
    -1, NULL);

  tb_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(tb_provider),
    "GtkToolbar, GtkToolButton, GtkToolItem { \n"
    "  border-color: #fffff0;\n"				  
    /* "  background-color: #fffff0;\n" */
    "  padding-left: 8px;\n"
    "  padding-bottom: 4px;\n"
    "  background-image: -gtk-gradient (linear, left top, right bottom, from(rgb(255, 255, 240)), to(rgb(255, 255, 255)));\n"
    "}\n",
    -1, NULL);
  /* the 8px here refers to the whole bar, not each entry */

  mu_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(mu_provider),
    "GtkMenuBar, GtkMenu, GtkMenuItem { \n"
    "  border-width: 4px;\n"
    "  border-color: #fffff0;\n"				  
    "  background-color: #fffff0;\n"
    "  padding-bottom: 4px;\n"
    "  padding-left: 8px;\n"				  
    "}\n",
    -1, NULL);

  rsc_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(rsc_provider),
    "GtkScale { \n"
    "  background-image: -gtk-gradient (linear, left top, right bottom, from(rgb(250, 250, 230)), to(rgb(160, 0, 0)));\n"
    "}\n",
    -1, NULL);

  gsc_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(gsc_provider),
    "GtkScale { \n"
    "  background-image: -gtk-gradient (linear, left top, right bottom, from(rgb(250, 250, 230)), to(rgb(0, 160, 0)));\n"
    "}\n",
    -1, NULL);

  bsc_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(bsc_provider),
    "GtkScale { \n"
    "  background-image: -gtk-gradient (linear, left top, right bottom, from(rgb(250, 250, 230)), to(rgb(0, 0, 160)));\n"
    "}\n",
    -1, NULL);

  /* I wanted to make the handle larger and set its color to #90ee90 -- no way!
   */
  pd_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(pd_provider),
    "GtkPaned { \n"
    "  background-color: #fffff0;\n"
    "}\n",
    -1, NULL);

  cb_provider = gtk_css_provider_new();
  gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(cb_provider),
    "GtkRadioButton:hover, GtkCheckButton:hover { \n"
    "  background-color: rgb(200, 200, 190);\n"
    "}\n",
    -1, NULL);

/* gtk3 tree view is inaccessible in filechooser and row colors can't be set! */
}
#endif
#endif

