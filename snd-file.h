#ifndef SND_FILE_H
#define SND_FILE_H


char *dialog_get_title(widget_t dialog);

const char **header_type_and_sample_type_to_position(file_data *fdat, mus_header_t header_type, mus_sample_t sample_type);
void position_to_header_type_and_sample_type(file_data *fdat, int pos);
mus_sample_t position_to_sample_type(mus_header_t header, int pos);
mus_header_t position_to_header_type(int pos);
const char **short_writable_headers(int *len);
const char **short_readable_headers(int *len);
const char **short_builtin_headers(int *len);
bool header_is_encoded(mus_header_t header_type);
void snd_encode(mus_header_t type, const char *input_filename, const char *output_filename);
snd_info *file_is_open_elsewhere_and_has_unsaved_edits(snd_info *sp, const char *fullname);
snd_info *finish_opening_sound(snd_info *sp, bool selected);

bool edit_header_callback(snd_info *sp, file_data *edit_header_data, 
			  void (*outer_handler)(const char *error_msg, void *ufd),
			  void (*inner_handler)(const char *error_msg, void *ufd));

void raw_data_dialog_to_file_info(const char *filename, char *title, char *info, read_only_t read_only, bool selected);

#if USE_GTK
  #define position_t gdouble
  #define POSITION_UNKNOWN 0.0
#else
  #define position_t int
  #define POSITION_UNKNOWN 0
#endif

typedef struct {
  time_t time;
  mus_long_t samps;
  char *filename, *full_filename;
} sort_info;

typedef struct {
  sort_info **files;
  char *dir_name;
  int len;
  int size;
} dir_info;

enum {NO_FILE_FILTER, JUST_SOUNDS_FILTER};

dir_info *free_dir_info (dir_info *dp);
dir_info *find_files_in_dir(const char *name);
dir_info *find_filtered_files_in_dir(const char *name, int filter_choice);
dir_info *find_filtered_files_in_dir_with_pattern(const char *name, int filter_choice, const char *pattern);

const char *short_sample_type_name(mus_sample_t sndlib_sample_type, const char *filename);

#define FILENAME_LIST_SIZE 16

void remember_filename(const char *filename, char **names);
char **make_filename_list(void);
void preload_filenames(char **files);
dir_info *find_sound_files_in_dir(const char *name);

#endif
