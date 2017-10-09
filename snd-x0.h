#ifndef SND_X0_H
#define SND_X0_H

#include <Xm/XmAll.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>

#if HAVE_GL
#include <GL/gl.h>
#include <GL/glx.h>
#endif

#define XOR(a, b) ((a) ^ (b))
/* can't get used to this operator -- in the good old days, ^ meant exponentiation */

#define xm_font_t XmRenderTable
#define XM_FONT_RESOURCE XmNrenderTable
#define XM_FONT_FREE XmRenderTableFree

#define LOTSA_PIXELS 10000

#define Xen_wrap_widget(Value)    ((Value) ? Xen_list_2(C_string_to_Xen_symbol("Widget"), Xen_wrap_C_pointer(Value)) : Xen_false)
#define Xen_wrap_window(Value)    ((Value) ? Xen_list_2(C_string_to_Xen_symbol("Window"), C_ulong_to_Xen_ulong(Value)) : Xen_false)
#define Xen_unwrap_widget(Value)  (Xen_is_list(Value) ? Xen_unwrap_C_pointer(Xen_cadr(Value)) : 0)
#define Xen_is_widget(Value)      (Xen_is_list(Value) && (Xen_list_length(Value) >= 2) && (Xen_is_symbol(Xen_car(Value))) && \
                                     (strcmp("Widget", Xen_symbol_to_C_string(Xen_car(Value))) == 0))
#define Xen_wrap_pixel(Value)     Xen_list_2(C_string_to_Xen_symbol("Pixel"), C_int_to_Xen_integer((int)Value))
                                       /* not ulong here! -- messes up the equal? check */
#define Xen_unwrap_pixel(Value)   (unsigned long)Xen_integer_to_C_int(Xen_cadr(Value))
#define Xen_is_pixel(Value)       (Xen_is_list(Value) && (Xen_list_length(Value) >= 2) && (Xen_is_symbol(Xen_car(Value))) && \
                                      (strcmp("Pixel", Xen_symbol_to_C_string(Xen_car(Value))) == 0))

#define NULL_WIDGET NULL

typedef enum {NOT_ACTIVATABLE, ACTIVATABLE, ACTIVATABLE_BUT_NOT_FOCUSED} text_cr_t;

#define LINE_MARGIN 4
#define CONTROLS_MARGIN 1

#define SCROLLBAR_MAX 10000

#define BACKGROUND_QUIT 1
#define BACKGROUND_CONTINUE 0
#define BACKGROUND_REMOVE(func) XtRemoveWorkProc(func)
/* #define BACKGROUND_ADD(func, data) XtAppAddWorkProc(main_app(ss), func, (XtPointer)data) */
#define BACKGROUND_ADD(func, data) add_work_proc(func, (XtPointer)data)

#define CALL_TIMEOUT(Func, Wait, Data) XtAppAddTimeOut(main_app(ss), Wait, Func, (XtPointer)Data)
#define TIMEOUT_ARGS                   XtPointer context, XtIntervalId *id
#define TIMEOUT_TYPE                   void
#define TIMEOUT_RESULT
#define timeout_result_t               XtIntervalId
#define TIMEOUT_REMOVE(Id)             XtRemoveTimeOut(Id)

#define widget_t Widget
#define widget_is_active(Wid) XtIsManaged(Wid)
#define activate_widget(Wid) XtManageChild(Wid)
#define deactivate_widget(Wid) XtUnmanageChild(Wid)

#define idle_t XtWorkProcId
#define idle_func_t Boolean
#define any_pointer_t XtPointer
/* can't use "pointer_t" -- Mac already defines that type (CoreServices.h) */
#define oclock_t Time
#define color_t Pixel
#define point_t XPoint
#define gc_t GC

#define rgb_t unsigned short
#define RGB_MAX 65535
#define float_to_rgb(Val) (rgb_t)(RGB_MAX * (Val))
#define rgb_to_float(Val) ((double)(Val) / (double)RGB_MAX)


typedef struct {
  GC gc;
  Display *dp;
  Drawable wn;
  Font current_font;
} graphics_context;

typedef enum {NOT_A_SCANF_WIDGET, SRATE_WIDGET, CHANS_WIDGET, DATA_LOCATION_WIDGET, SAMPLES_WIDGET} scanf_widget_t;

typedef struct {
  Widget header_type_list, sample_type_list, srate_text, chans_text, comment_text, location_text, samples_text, error_text;
  Widget dialog, src_button, auto_comment_button;
  mus_header_t current_header_type;
  mus_sample_t current_sample_type;
  int sample_types, header_type_pos, sample_type_pos;
  scanf_widget_t scanf_widget, error_widget;
  bool src, auto_comment;
  char *saved_comment;
} file_data;

typedef enum {WITHOUT_CHANNELS_FIELD, WITH_CHANNELS_FIELD, WITH_EXTRACT_CHANNELS_FIELD} dialog_channels_t;
typedef enum {WITHOUT_SAMPLES_FIELD, WITH_SAMPLES_FIELD} dialog_samples_t;
typedef enum {WITHOUT_DATA_LOCATION_FIELD, WITH_DATA_LOCATION_FIELD} dialog_data_location_t;
typedef enum {WITHOUT_HEADER_TYPE_FIELD, WITH_HEADER_TYPE_FIELD} dialog_header_type_t;
typedef enum {WITHOUT_COMMENT_FIELD, WITH_COMMENT_FIELD} dialog_comment_t;

#define snd_ShiftMask ShiftMask
#define snd_ControlMask ControlMask
#if (!HAVE_SUN)
  #define snd_MetaMask Mod1Mask
#else
  #define snd_MetaMask (Mod1Mask | Mod4Mask)
#endif

#define main_shell(a) (a)->mainshell
#define main_pane(a) (a)->mainpane
#define sound_pane(a) (a)->soundpane
#define sound_pane_box(a) (a)->soundpanebox
#define main_app(a) (a)->mainapp
#define main_display(a) (a)->mdpy
#define PEAKS_FONT(a) (a)->peaks_fontstruct
#define BOLD_PEAKS_FONT(a) (a)->bold_peaks_fontstruct
#define AXIS_NUMBERS_FONT(a) (a)->axis_numbers_fontstruct
#define AXIS_LABEL_FONT(a) (a)->axis_label_fontstruct
#define TINY_FONT(a) (a)->tiny_fontstruct
#define LISTENER_FONT(a) (a)->listener_fontstruct

#define DEFAULT_GRAPH_CURSOR XC_crosshair

#define DEFAULT_TINY_FONT "6x12"
#define DEFAULT_PEAKS_FONT "-*-times-medium-r-*-*-14-*-*-*-*-*-*-*"
#define DEFAULT_BOLD_PEAKS_FONT "-*-times-bold-r-*-*-14-*-*-*-*-*-*-*"
#define DEFAULT_AXIS_NUMBERS_FONT "9x15"
#define DEFAULT_AXIS_LABEL_FONT "-*-times-medium-r-*-*-18-*-*-*-*-*-*-*"

#define KEY_TO_NAME(key) XKeysymToString(key)
/* on the Sun, if key is 0, XKeysymToString segfaults! */

/* #define GUI_CURRENT_TIME(ss) XtLastTimestampProcessed(main_display(ss)) */


/* now pull in the key names (/usr/include/X11/keysymdef.h) */
#define snd_K_Shift_L XK_Shift_L	 
#define snd_K_space XK_space 
#define snd_K_openparen XK_parenleft 
#define snd_K_closeparen XK_parenright 
#define snd_K_plus XK_plus 
#define snd_K_minus XK_minus 
#define snd_K_period XK_period 
#define snd_K_slash XK_slash 
#define snd_K_0 XK_0 
#define snd_K_1 XK_1 
#define snd_K_2 XK_2 
#define snd_K_3 XK_3 
#define snd_K_4 XK_4 
#define snd_K_5 XK_5 
#define snd_K_6 XK_6 
#define snd_K_7 XK_7 
#define snd_K_8 XK_8 
#define snd_K_9 XK_9 
#define snd_K_less XK_less 
#define snd_K_greater XK_greater 
#define snd_K_A XK_A 
#define snd_K_B XK_B 
#define snd_K_C XK_C 
#define snd_K_D XK_D 
#define snd_K_E XK_E 
#define snd_K_F XK_F 
#define snd_K_G XK_G 
#define snd_K_H XK_H 
#define snd_K_I XK_I 
#define snd_K_J XK_J 
#define snd_K_K XK_K 
#define snd_K_L XK_L 
#define snd_K_M XK_M 
#define snd_K_N XK_N 
#define snd_K_O XK_O 
#define snd_K_P XK_P 
#define snd_K_Q XK_Q 
#define snd_K_R XK_R 
#define snd_K_S XK_S 
#define snd_K_T XK_T 
#define snd_K_U XK_U 
#define snd_K_V XK_V 
#define snd_K_W XK_W 
#define snd_K_X XK_X 
#define snd_K_Y XK_Y 
#define snd_K_Z XK_Z 
#define snd_K_underscore XK_underscore 
#define snd_K_a XK_a 
#define snd_K_b XK_b 
#define snd_K_c XK_c 
#define snd_K_d XK_d 
#define snd_K_e XK_e 
#define snd_K_f XK_f 
#define snd_K_g XK_g 
#define snd_K_h XK_h 
#define snd_K_i XK_i 
#define snd_K_j XK_j 
#define snd_K_k XK_k 
#define snd_K_l XK_l 
#define snd_K_m XK_m 
#define snd_K_n XK_n 
#define snd_K_o XK_o 
#define snd_K_p XK_p 
#define snd_K_q XK_q 
#define snd_K_r XK_r 
#define snd_K_s XK_s 
#define snd_K_t XK_t 
#define snd_K_u XK_u 
#define snd_K_v XK_v 
#define snd_K_w XK_w 
#define snd_K_x XK_x 
#define snd_K_y XK_y 
#define snd_K_z XK_z 

#define snd_K_Home  XK_Home		 
#define snd_K_Left  XK_Left		 
#define snd_K_Up    XK_Up		 
#define snd_K_Right XK_Right	 
#define snd_K_Down  XK_Down		 

#define snd_keypad_Insert   XK_KP_Insert
#define snd_keypad_Delete   XK_KP_Delete
#define snd_keypad_Multiply XK_KP_Multiply
#define snd_keypad_Add      XK_KP_Add
#define snd_keypad_Subtract XK_KP_Subtract
#define snd_keypad_Divide   XK_KP_Divide
#define snd_keypad_Decimal  XK_KP_Decimal
#define snd_keypad_Enter    XK_KP_Enter
#define snd_keypad_Up       XK_KP_Up
#define snd_keypad_Down     XK_KP_Down
#define snd_keypad_Left     XK_KP_Left
#define snd_keypad_Right    XK_KP_Right

#define snd_keypad_0 XK_KP_0
#define snd_keypad_1 XK_KP_1
#define snd_keypad_2 XK_KP_2
#define snd_keypad_3 XK_KP_3
#define snd_keypad_4 XK_KP_4
#define snd_keypad_5 XK_KP_5
#define snd_keypad_6 XK_KP_6
#define snd_keypad_7 XK_KP_7
#define snd_keypad_8 XK_KP_8
#define snd_keypad_9 XK_KP_9

#endif
