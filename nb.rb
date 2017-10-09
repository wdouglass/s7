# nb.rb -- translation of nb.scm

# Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 02/12/10 22:08:15
# Changed: 14/11/13 03:02:23

# Tested with Snd 15.x, Ruby 2.x.x
#
# type nb = make_nb
#      nb.help
# or   xnb = make_nb_motif (installs a popup menu on info widget)
#
# global variable:
#   $nb_database
#
# make_nb(path)
#
# class NB
#   initialize(path)
#
# getter and setter:
#   name=(filename)
#   name
#   notes=(new_notes)
#   notes
#
# interactive methods:
#   unb
#   prune_db
#   open(path)
#   close
#   help           (alias info and description)
#
=begin
nb = make_nb
nb.name
nb.notes
nb.notes = "new text"
nb.prune_db                  # deletes empty entries
nb.unb                       # deletes entry of current file
nb.open                      # adds mouse hooks
nb.close                     # removes mouse hooks
nb.help                      # this help
=end

require "hooks"
with_silence do
  unless defined? DBM.open
    require "dbm"
  end
end

$nb_database = "nb" unless defined? $nb_database

if provided?("snd-motif") and (not provided?("xm"))
  with_silence(LoadError) do
    require "libxm"
  end
end

module Kernel
  # XM_NB should only create one instance of the popup menu on the
  # info_widget.
  @@XM_NB = false
  
  def Kernel.xm_nb
    @@XM_NB
  end

  def Kernel.xm_nb=(val)
    @@XM_NB = val
  end
end

def make_nb(path = $nb_database)
  NB.new(path)
end

def make_nb_motif(path = $nb_database)
  if Kernel.xm_nb.kind_of?(XM_NB)
    Kernel.xm_nb.open(path)
  else
    Kernel.xm_nb = XM_NB.new(path)
  end
end if provided?("xm")

class NB
  include Info
  
  Region_viewer = 2
  View_files_dialog = 8
  Info_dialog = 20
  
  def initialize(path)
    @nb_database = path
    @type = nil
    @position = nil
    @name = nil
    @notes = ""
    @alert_color = make_color(1.0, 1.0, 0.94)
    @db_hook_name = format("%s-nb-hook", @nb_database)
    set_help
    create
  end
  attr_reader :name, :notes
  alias help description
  
  def inspect
    format("#<%s: nb_database: %s, open: %s, name: %s>",
           self.class,
           @nb_database.inspect,
           $mouse_enter_label_hook.member?(@db_hook_name).inspect,
           @name.inspect)
  end
  
  def name=(filename)
    if filename and File.exist?(File.expand_path(filename))
      @name = filename
      show_popup_info
    else
      Snd.warning("no such file: %s", filename.inspect)
    end
  end
  
  def notes=(new_notes)
    @notes = new_notes
    nb
    @notes
  end

  def with_dbm(&body)
    ret = nil
    db = DBM.open(@nb_database)
    ret = body.call(db)
    db.close
    ret
  rescue
    Snd.warning("%s#%s", self.class, get_func_name)
  end

  def prune_db
    with_dbm do |db|
      db.delete_if do |k, v| k.empty? end
    end
    self
  end

  def open(path = @nb_database)
    @nb_database = path
    create
    self
  end
  
  def close
    $mouse_enter_label_hook.remove_hook!(@db_hook_name)
    $mouse_leave_label_hook.remove_hook!(@db_hook_name)
    self
  end
  
  def unb
    if @name and File.exist?(File.expand_path(@name))
      with_dbm do |db|
        db.delete(@name)
      end
      show_popup_info
    else
      Snd.warning("no such file: %s", @name.inspect)
    end
  end
  
  private  
  def create
    close
    $mouse_enter_label_hook.add_hook!(@db_hook_name) do |t, p, n|
      files_popup_info(t, p, n) unless t == Region_viewer
    end
  end

  def nb
    if @name and File.exist?(File.expand_path(@name))
      with_dbm do |db|
        db[@name] = @notes
      end
      show_popup_info
    else
      Snd.warning("no such file: %s", @name.inspect)
    end
  end

  def files_popup_info(type, position, name)
    @type = type
    @position = position
    @name = name
    show_popup_info
  end

  def show_popup_info
    let(dialog_widgets[Info_dialog]) do |info_exists_p|
      info_dialog(@name, file_info)
      if info_widget = dialog_widgets[Info_dialog]
        unless info_exists_p
          width, height = widget_size(dialog_widgets[View_files_dialog])
          set_widget_position(info_widget, [width + 10, 10])
        end
      end
    end
    @name
  end

  def file_info
    with_dbm do |db|
      @notes = (db[@name] or "")
    end
    cs = mus_sound_chans(@name)
    sr = mus_sound_srate(@name)
    len = format("%1.3f", mus_sound_samples(@name).to_f / (cs * sr.to_f))
    d_format = mus_sample_type_name(mus_sound_sample_type(@name))
    h_type = mus_header_type_name(mus_sound_header_type(@name))
    frms = mus_sound_framples(@name)
    max_amp = ""
    if mus_sound_maxamp_exists?(@name)
      str = ""
      mus_sound_maxamp(@name).each_pair do |s, v|
        str << format("%1.3f (%1.3fs), ", v, s / sr.to_f)
      end
      max_amp = format("\n maxamp: [%s]", str[0..-3])
    end
    fdate = Time.at(mus_sound_write_date(@name))
    date = fdate.localtime.strftime("%a %d-%b-%y %H:%M %z")
    info_string = format("\
  chans: %d, srate: %d
 length: %1.3f (%d frms)
 format: %s [%s]%s
written: %s\n", cs, sr, len, frms, d_format, h_type, max_amp, date)
    if defined?($info_comment_hook) and hook?($info_comment_hook)
      if $info_comment_hook.empty?
        if s = mus_sound_comment(@name)
          info_string += format("comment: %s\n", s)
        end
      else
        $info_comment_hook.run_hook do |prc|
          info_string = prc.call(@name, info_string)
        end
      end
    else
      if s = mus_sound_comment(@name)
        info_string += format("comment: %s\n", s)
      end
    end
    info_string += "\n" + @notes
  end
  
  def set_help
    self.description = "\
# global variable:
#   $nb_database (#{$nb_database})
#
# make_nb(path)
# make_nb_motif(path)
#
# class NB
#   initialize(path)
#
# getter and setter:
#   name=(filename)
#   name
#   notes=(new_notes)
#   notes
#
# interactive methods:
#   unb
#   prune_db
#   open(path)
#   close
#   help           (alias info and description)

nb = make_nb
nb.name
nb.notes
nb.notes = \"new text\"
nb.prune_db                  # deletes empty entries
nb.unb                       # deletes entry of current file
nb.open                      # adds mouse hooks
nb.close                     # removes mouse hooks
nb.help                      # this help
"
  end
end

class XM_NB < NB
  require "popup"

  def initialize(path)
    @dialog = nil
    @file_name = nil
    @text_widget = nil
    @message_widget = nil
    super
    @db_str = format("DB: %s", File.basename(@nb_database))
    @popup_nb_hook = Hook.new("@popup_nb_hook", 2, "\
lambda do |snd, info| ... \"new info\" end: called in popup.rb on
graph-popup-menu entry `Info'.  Its primary use is to communicate
between popup.rb and nb.rb.  To add your own information to the info
string, you may use $info_comment_hook.

The current selected SND is called with string INFO.  If more than one
hook procedures exists, each procedure's result is passed as input to
the next.  E.g. if an instance of NB or XM_NB is created (see nb.rb),
the $nb_database entries of SND will be returned.")
    @popup_nb_hook.add_hook!("initialize-nb-hook") do |snd, info|
      @name = file_name(snd)
      with_dbm do |db|
        @notes = (db[@name] or "")
      end
      unless @notes.empty?
        info += "\n" unless info.empty?
        info += @notes
      end
      info
    end
    install_menu
  end
  attr_reader :popup_nb_hook

  def close
    if @dialog.kind_of?(Dialog) and
       RWidget?(@dialog.dialog) and
       RXtIsManaged(@dialog.dialog)
      RXtUnmanageChild(@dialog.dialog)
    end
    super
  end

  protected
  def post_edit
    if !@name and RWidget?(@message_widget)
      @name = if File.exist?(file = current_label(@message_widget).split[0])
                file
              else
                format("no such file: %s", file.inspect)
              end
    end
    unless @dialog.kind_of?(Dialog) and RWidget?(@dialog.dialog)
      @dialog = make_dialog(@db_str,
                            :help_cb, lambda do |w, c, i|
                              help_cb
                            end, :clear_cb, lambda do |w, c, i|
                              RXmTextSetString(@text_widget, "")
                            end) do |w, c, i|
        self.notes = RXmTextGetString(@text_widget)
      end
      @file_name = @dialog.add_label(@name)
      @text_widget = @dialog.add_text(:rows, 16, :columns, 60,
                                      :wordwrap, true, :value, @notes)
      @dialog.doit_string("Submit")
    end
    activate_dialog(@dialog.dialog)
    show_edit_info
  end

  def help_cb
    help_dialog(@db_str,
                  "Edit info DB of sound files (see nb.scm).

Provides pop-up help in the Files viewer.  \
If you have `dbm', any data associated with \
the file in the dbm database will also be posted.  \
The database name is defined by $nb_database \
(#{$nb_database.inspect}).

o Edit info: opens the edit widget

o Prune DB:  clears non-existent file references
             out of the database

o Clear:     removes info entry from current file

o Close:     removes mouse hooks and popup menu;
             type `make_nb_motif' to reinstall
             the hooks and popup menu

o Submit:    submits info from edit widget
             to file info database

#{self.description}",
                  ["{Libxm}: graphics module",
                   "{Ruby}: extension language",
                   "{Motif}: Motif extensions via libxm"])
    end

  def post_popup?
    $mouse_enter_label_hook.member?(@db_hook_name) and
      File.exist?(current_label(@message_widget).split[0])
  end

  private
  def create
    @db_hook_name = format("%s-xm-nb-hook", @nb_database)
    super
  end
  
  def install_menu
    if RWidget?(wid = dialog_widgets[Info_dialog])
      setup_menu(wid)
    else
      $new_widget_hook.add_hook!("nb-edit-hook") do |w|
        if w == dialog_widgets[Info_dialog]
          setup_menu(w)
          $new_widget_hook.remove_hook!("nb-edit-hook")
        end
      end
    end
  end
  
  def setup_menu(wid)
    @message_widget = find_child(wid, "Message")
    make_snd_popup("NB Edit Info",
                   :where, :event,
                   :parent, find_child(wid, "post-it-text")) do
      entry("Edit Info") do |w, snd, chn| Kernel.xm_nb.post_edit end
      entry("Prune DB") do |w, snd, chn| Kernel.xm_nb.prune_db end
      entry("Clear current Info") do |w, snd, chn| Kernel.xm_nb.unb end
      entry("Close NB Edit") do |w, snd, chn| Kernel.xm_nb.close end
      separator
      entry("Help") do |w, snd, chn| Kernel.xm_nb.help_cb end
      before_popup_hook.add_hook!("NB Edit Info") do |d1, d2, d3|
        Kernel.xm_nb.post_popup?
      end
    end
  end

  def files_popup_info(type, position, name)
    super
    show_edit_info
  end

  def show_edit_info
    xfname = string2compound(@name)
    if RWidget?(@file_name)
      RXtVaSetValues(@file_name, [RXmNlabelString, xfname])
    end
    if RWidget?(@text_widget)
      RXtVaSetValues(@text_widget, [RXmNvalue, @notes])
    end
    RXmStringFree(xfname)
  end
end if provided?("xm")

# nb.rb ends here
