#ifndef SND_NOGUI0_H
#define SND_NOGUI0_H

#define BACKGROUND_QUIT true
#define BACKGROUND_CONTINUE false
#define BACKGROUND_REMOVE(func)
#define BACKGROUND_ADD(func, data) func(data)

#define TIMEOUT_ARGS void *context
#define TIMEOUT_TYPE void
#define TIMEOUT_RESULT
#define timeout_result_t int
#define TIMEOUT_REMOVE(Id)
#define CALL_TIMEOUT(Func, Wait, Data) 0

#define widget_t int
#define widget_is_active(Wid) 0
#define activate_widget(Wid) Wid = 0
#define deactivate_widget(Wid) Wid = 0

#define Xen_wrap_widget(Value) Xen_false
#define Xen_wrap_window(Value) Xen_false
#define Xen_unwrap_widget(Value) 0
#define Xen_is_widget(Value) 0
#define NULL_WIDGET 0
#define LOTSA_PIXELS 10000

#define DEFAULT_TINY_FONT "Monospace 8"
#define DEFAULT_PEAKS_FONT "Serif 10"
#define DEFAULT_BOLD_PEAKS_FONT "Serif Bold 10"
#define DEFAULT_AXIS_NUMBERS_FONT "Monospace 10"
#define DEFAULT_AXIS_LABEL_FONT "Serif 14"

#define TINY_FONT(a) 0
#define AXIS_NUMBERS_FONT(a) 0
#define PEAKS_FONT(a) 0
#define DEFAULT_GRAPH_CURSOR 0

#define idle_t int
#define idle_func_t int
#define any_pointer_t void *
#define oclock_t int
#define color_t int
#define point_t int

#define rgb_t unsigned short
#define RGB_MAX 65535
#define float_to_rgb(Val) (rgb_t)(RGB_MAX * (Val))
#define rgb_to_float(Val) (float)((float)(Val) / (float)RGB_MAX)


typedef struct {
  int wn;
  int gc;
} graphics_context;

typedef struct {
  mus_header_t current_header_type;
  mus_sample_t current_sample_type;
  int sample_types, header_type_pos, sample_type_pos, scanf_widget, error_widget;
} file_data;

typedef enum {NOT_A_SCANF_WIDGET} scanf_widget_t;

#define snd_ShiftMask 0
#define snd_ControlMask 0
#define snd_MetaMask 0

#define main_shell(a) 0
#define main_pane(a) 0
#define sound_pane(a) 0
#define KEY_TO_NAME(key) "?"
/* not NULL here because that causes a segfault in solaris (key name null -> strlen of null in vsprintf) */

enum {snd_K_Shift_L, snd_K_space, snd_K_openparen, snd_K_closeparen, snd_K_plus, snd_K_minus, snd_K_period, snd_K_slash, snd_K_0, snd_K_1, snd_K_2, snd_K_3, snd_K_4, snd_K_5, snd_K_6, snd_K_7, snd_K_8, snd_K_9, snd_K_less, snd_K_greater, snd_K_A, snd_K_B, snd_K_C, snd_K_D, snd_K_E, snd_K_F, snd_K_G, snd_K_H, snd_K_I, snd_K_J, snd_K_K, snd_K_L, snd_K_M, snd_K_N, snd_K_O, snd_K_P, snd_K_Q, snd_K_R, snd_K_S, snd_K_T, snd_K_U, snd_K_V, snd_K_W, snd_K_X, snd_K_Y, snd_K_Z, snd_K_underscore, snd_K_a, snd_K_b, snd_K_c, snd_K_d, snd_K_e, snd_K_f, snd_K_g, snd_K_h, snd_K_i, snd_K_j, snd_K_k, snd_K_l, snd_K_m, snd_K_n, snd_K_o, snd_K_p, snd_K_q, snd_K_r, snd_K_s, snd_K_t, snd_K_u, snd_K_v, snd_K_w, snd_K_x, snd_K_y, snd_K_z, snd_K_Home, snd_K_Left, snd_K_Up, snd_K_Right, snd_K_Down, snd_keypad_Insert, snd_keypad_Delete, snd_keypad_Multiply, snd_keypad_Add, snd_keypad_Subtract, snd_keypad_Divide, snd_keypad_Decimal, snd_keypad_Enter, snd_keypad_0, snd_keypad_1, snd_keypad_2, snd_keypad_3, snd_keypad_4, snd_keypad_5, snd_keypad_6, snd_keypad_7, snd_keypad_8, snd_keypad_9, snd_keypad_Down, snd_keypad_Up, snd_keypad_Left, snd_keypad_Right};

#endif
