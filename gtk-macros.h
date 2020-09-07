#ifndef GTK_MACROS_H
#define GTK_MACROS_H

#if (GTK_CHECK_VERSION(3, 99, 0))

  #define window_add(Window, Widget) gtk_window_set_child(GTK_WINDOW(Window), Widget)
  #define scrolled_window_add(Window, Widget) gtk_scrolled_window_set_child(GTK_SCROLLED_WINDOW(Window), Widget)
  #define frame_add(Frame, Widget) gtk_frame_set_child(GTK_FRAME(Frame), Widget)
  #define box_add(Box, Widget) gtk_box_append(GTK_BOX(Box), Widget)
  #define box_add2(Box, Widget) gtk_box_append(GTK_BOX(Box), Widget)
  #define scrolled_window_new(Ignore1, Ignore2) gtk_scrolled_window_new()

  #define button_child(Button) gtk_button_get_child(GTK_BUTTON(Button))
  #define frame_child(Frame)   gtk_frame_get_child(GTK_FRAME(Frame))
  #define combo_box_child(Combo) gtk_combo_box_get_child(GTK_COMBO_BOX(Combo))

  #define paned_set_first(Pane, Child, X, Y) gtk_paned_set_start_child(GTK_PANED(Pane), Child)
  #define paned_set_second(Pane, Child, X, Y) gtk_paned_set_end_child(GTK_PANED(Pane), Child)

  #define button_set_relief(Button, Relief)

  #define event_get_keyval(E) gdk_key_event_get_keyval(E)
  #define event_get_button(E) gdk_button_event_get_button(E)

  typedef struct GdkEvent GdkEventAny;
  typedef struct GdkEvent GdkEventExpose;
  typedef struct GdkEvent GdkEventMotion;
  typedef struct GdkEvent GdkEventButton;
  typedef struct GdkEvent GdkEventKey;
  typedef struct GdkEvent GdkEventFocus;
  typedef struct GdkEvent GdkEventCrossing;
  typedef struct GdkEvent GdkEventConfigure;
  typedef struct GdkEvent GdkEventSelection;

  #define MetaMask GDK_META_MASK

#else

  #define window_add(Window, Widget) gtk_container_add(GTK_CONTAINER(Window), Widget)
  #define scrolled_window_add(Window, Widget) gtk_container_add(GTK_CONTAINER(Window), Widget)
  #define frame_add(Frame, Widget)  gtk_container_add(GTK_CONTAINER(Frame), Widget)
  #define box_add(Box, Widget)      gtk_container_add(GTK_CONTAINER(Box), Widget)
  #define box_add2(Box, Widget)     gtk_container_add(GTK_CONTAINER(Box), Widget)
  #define scrolled_window_new(X, Y) gtk_scrolled_window_new(X, Y)
  #define container_add(X, Y)       gtk_container_add(GTK_CONTAINER(X), Y)

  #define button_child(Button)       gtk_bin_get_child(GTK_BIN(Button))
  #define frame_child(Frame)         gtk_bin_get_child(GTK_BIN(Frame))
  #define combo_box_child(Combo)     gtk_bin_get_child(GTK_BIN(Combo))

  #define paned_set_first(Pane, Child, X, Y) gtk_paned_pack1(GTK_PANED(Pane), Child, X, Y)
  #define paned_set_second(Pane, Child, X, Y) gtk_paned_pack2(GTK_PANED(Pane), Child, X, Y)

#if GTK_CHECK_VERSION(3, 94, 0)
  static guint event_get_keyval(GdkKeyEvent *E) {guint val = 0; gdk_event_get_keyval(E, &val); return(val);}
  static guint event_get_button(GdkButtonEvent *E) {guint val = 0; gdk_event_get_button(E, &val); return(val);}
#else
  #define event_get_keyval(E) (E)->keyval
  #define event_get_button(E) (E)->button
#endif

  #define button_set_relief(Button, Relief) gtk_button_set_relief(GTK_BUTTON(Button), Relief)

  #if (!HAVE_SUN)
    #define MetaMask GDK_MOD1_MASK
  #else 
    #define MetaMask (GDK_MOD1_MASK | GDK_MOD4_MASK)
  #endif

#endif

#if (!GTK_CHECK_VERSION(3, 92, 0))
  #define container_set_border_width(Container, Width) gtk_container_set_border_width(GTK_CONTAINER(Container), Width)
#else
  #define container_set_border_width(Container, Width)
#endif


#define ControlMask GDK_CONTROL_MASK
#define ShiftMask   GDK_SHIFT_MASK

#if (!GTK_CHECK_VERSION(3, 92, 0))
  #define box_pack_start(Parent, Child, Expand, Fill, Pad) gtk_box_pack_start(GTK_BOX(Parent), Child, Expand, Fill, Pad)
  #define box_pack_end(Parent, Child, Expand, Fill, Pad) gtk_box_pack_end(GTK_BOX(Parent), Child, Expand, Fill, Pad)
#else
  #define box_pack_start(Parent, Child, Expand, Fill, Pad) \
    do {gtk_box_append(GTK_BOX(Parent), Child); gtk_widget_set_hexpand(GTK_WIDGET(Child), Expand); gtk_widget_set_vexpand(GTK_WIDGET(Child), Expand);} while (0)
  #define box_pack_end(Parent, Child, Expand, Fill, Pad) \
    do {gtk_box_append(GTK_BOX(Parent), Child); gtk_widget_set_hexpand(GTK_WIDGET(Child), Expand); gtk_widget_set_vexpand(GTK_WIDGET(Child), Expand);} while (0)
#endif

/* tested in gtk 2.20.1, 3.24.18, 3.99.1 */
#endif
