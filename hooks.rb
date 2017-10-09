# hooks.rb -- hook-related functions

# Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 03/12/21 13:48:01
# Changed: 15/02/27 23:12:08

# If class Hook isn't compiled in, here is the corresponding Ruby
# class and the initialization of all global hooks.

=begin
# example in extsnd.html, "Channel-specific hooks"

def protect(snd = false, chn = false)
  edit_pos = edit_position(snd, chn)
  hook = edit_hook(snd, chn)
  hook.reset_hook!
  hook.add_hook!("protect") do | |
    snd_print("protected") if val = edit_position(snd, chn) < edit_pos
    val
  end
end

def unprotect(snd = false, chn = false)
  edit_hook(snd, chn).reset_hook!
end
=end

# Snd_hooks             an array containing all global hook variables
# 
# $var_hook.member?("name of hook")
# $var_hook.show or
# describe_hook(hook)   prints code of hook procedures if file exists
# $var.run_hook_by_name(name, *args)
#
# with_local_hook(hook, *procs, &thunk)
# reset_all_hooks()     clears all hook procedures

require "clm"

unless defined?(Hook)
  class Hook
    include Enumerable

    def initialize(name, arity = 0, help = "")
      @name = name
      @arity = arity
      @procs = []
      if string?(help) and (not help.empty?)
        add_help(name, help)
      end
    end
    attr_reader :name, :arity
    
    def add_hook!(name, &body)
      @procs.push([name, body])
    end
    
    def remove_hook!(name)
      @procs.delete(@procs.assoc(name))
    end

    def reset_hook!
      @procs.clear
    end
    
    def run_hook(&body)
      self.to_a.each(&body)
    end
    alias each run_hook

    def call(*args)
      ret = nil
      self.run_hook do |prc|
        ret = prc.call(*args)
      end
      ret
    end
    
    def to_a
      @procs.map do |ary|
        ary.last
      end
    end
    
    def length
      @procs.length
    end
    alias size length

    def empty?
      @procs.empty?
    end

    def describe
      get_help(@name)
    end
    alias documentation describe
    
    def names
      @procs.map do |ary|
        ary.first
      end
    end
    
    def inspect
      format("#<%s name: %p, arity: %d, procs[%d]: %p>",
             self.class, @name, @arity, self.length, self.names)
    end
  end

  def make_hook(name, arity = 0, help = "", hook_name = nil, &body)
    error_str = "make_hook(name, arity=0, help=\"\", hook_name=nil, &body): \
need a String or Symbol, not %p"
    var_sym = case name
              when Symbol
                name
              when String
                name.intern
              else
                raise format(error_str, name)
              end
    if var_sym.to_s.split(//).first != "$"
      var_sym = format("$%s", var_sym.to_s).intern
    end
    unless (var = Hook.instance_eval("#{var_sym} if defined?(#{var_sym})"))
      var = Hook.new(var_sym.to_s, arity, help)
    end
    if block_given?
      unless string?(hook_name)
        hook_name = format("%s hook", var_sym.to_s)
      end
      var.add_hook!(hook_name, &body)
    end
    Hook.instance_eval("#{var_sym} = var")
  end
  
  def hook?(obj)
    obj.kind_of?(Hook)
  end
  
  $after_apply_controls_hook    = Hook.new("$after_apply_controls_hook", 1)
  $after_graph_hook             = Hook.new("$after_graph_hook", 2)
  $after_lisp_graph_hook        = Hook.new("$after_lisp_graph_hook", 2)
  $after_open_hook              = Hook.new("$after_open_hook", 1)
  $after_save_as_hook           = Hook.new("$after_save_as_hook", 3)
  $after_save_state_hook        = Hook.new("$after_save_state_hook", 1)
  $after_transform_hook         = Hook.new("$after_transform_hook", 3)
  $bad_header_hook              = Hook.new("$bad_header_hook", 1)
  $before_close_hook            = Hook.new("$before_close_hook", 1)
  $before_exit_hook             = Hook.new("$before_exit_hook", 0)
  $before_save_as_hook          = Hook.new("$before_save_as_hook", 7)
  $before_save_state_hook       = Hook.new("$before_save_state_hook", 1)
  $before_transform_hook        = Hook.new("$before_transform_hook", 2)
  $clip_hook                    = Hook.new("$clip_hook", 1)
  $close_hook                   = Hook.new("$close_hook", 1)
  $dac_hook                     = Hook.new("$dac_hook", 1)
  $draw_mark_hook               = Hook.new("$draw_mark_hook", 1)
  $draw_mix_hook                = Hook.new("$draw_mix_hook", 5)
  $drop_hook                    = Hook.new("$drop_hook", 1)
  $during_open_hook             = Hook.new("$during_open_hook", 3)
  $effects_hook                 = Hook.new("$effects_hook", 0)
  $enved_hook                   = Hook.new("$enved_hook", 5)
  $exit_hook                    = Hook.new("$exit_hook", 0)
  $graph_hook                   = Hook.new("$graph_hook", 4)
  $help_hook                    = Hook.new("$help_hook", 2)
  $initial_graph_hook           = Hook.new("$initial_graph_hook", 3)
  $key_press_hook               = Hook.new("$key_press_hook", 4)
  $lisp_graph_hook              = Hook.new("$lisp_graph_hook", 2)
  $listener_click_hook          = Hook.new("$listener_click_hook", 1)
  $mark_click_hook              = Hook.new("$mark_click_hook", 1)
  $mark_drag_hook               = Hook.new("$mark_drag_hook", 1)
  $mark_hook                    = Hook.new("$mark_hook", 4)
  $mix_click_hook               = Hook.new("$mix_click_hook", 1)
  $mix_drag_hook                = Hook.new("$mix_drag_hook", 3)
  $mix_release_hook             = Hook.new("$mix_release_hook", 2)
  $mouse_click_hook             = Hook.new("$mouse_click_hook", 7)
  $mouse_drag_hook              = Hook.new("$mouse_drag_hook", 6)
  $mouse_enter_graph_hook       = Hook.new("$mouse_enter_graph_hook", 2)
  $mouse_enter_label_hook       = Hook.new("$mouse_enter_label_hook", 3)
  $mouse_enter_listener_hook    = Hook.new("$mouse_enter_listener_hook", 1)
  $mouse_enter_text_hook        = Hook.new("$mouse_enter_text_hook", 1)
  $mouse_leave_graph_hook       = Hook.new("$mouse_leave_graph_hook", 2)
  $mouse_leave_label_hook       = Hook.new("$mouse_leave_label_hook", 3)
  $mouse_leave_listener_hook    = Hook.new("$mouse_leave_listener_hook", 1)
  $mouse_leave_text_hook        = Hook.new("$mouse_leave_text_hook", 1)
  $mouse_press_hook             = Hook.new("$mouse_press_hook", 6)
  $mus_error_hook               = Hook.new("$mus_error_hook", 2)
  $name_click_hook              = Hook.new("$name_click_hook", 1)
  $new_sound_hook               = Hook.new("$new_sound_hook", 1)
  $new_widget_hook              = Hook.new("$new_widget_hook", 1)
  $open_hook                    = Hook.new("$open_hook", 1)
  $open_raw_sound_hook          = Hook.new("$open_raw_sound_hook", 2)
  $orientation_hook             = Hook.new("$orientation_hook", 0)
  $output_comment_hook          = Hook.new("$output_comment_hook", 1)
  $play_hook                    = Hook.new("$play_hook", 1)
  $read_hook                    = Hook.new("$read_hook", 1)
  $save_hook                    = Hook.new("$save_hook", 2)
  $save_state_hook              = Hook.new("$save_state_hook", 1)
  $select_channel_hook          = Hook.new("$select_channel_hook", 2)
  $select_sound_hook            = Hook.new("$select_sound_hook", 1)
  $snd_error_hook               = Hook.new("$snd_error_hook", 1)
  $snd_warning_hook             = Hook.new("$snd_warning_hook", 1)
  $start_playing_hook           = Hook.new("$start_playing_hook", 1)
  $start_playing_selection_hook = Hook.new("$start_playing_selection_hook", 0)
  $stop_dac_hook                = Hook.new("$stop_dac_hook", 0)
  $stop_playing_hook            = Hook.new("$stop_playing_hook", 1)
  $stop_playing_selection_hook  = Hook.new("$stop_playing_selection_hook", 0)
  $update_hook                  = Hook.new("$update_hook", 1)
  # unless --without-gui
  $color_hook                   = Hook.new("$color_hook", 0)
end

class Hook
  def to_names
    @procs.map do |ary|
      ary.first
    end
  end

  def member?(name)
    to_names.member?(name)
  end
  alias included? member?

  # This works only with newer ruby versions (I assume >= 1.8.x).
  # Proc#to_s must return #<Proc:0x80c96a0@xxx:x> not only
  # #<Proc:0x80c96a0>!
  def to_str
    self.each do |prc|
      # cover printf's %x
      Snd.display(prc.to_str.gsub(/%/, "%%"))
    end
    nil
  end
  alias show to_str

  def run_hook_by_name(name, *args)
    if prc = @procs.assoc(name)
      prc.last.call(*args)
    end
  end
end

def describe_hook(hook)
  hook.show
end

add_help(:with_local_hook,
         "with_local_hook(hook, *procs, &thunk)  \
Evaluates THUNK with HOOK set to PROCS, \
then restores HOOK to its previous state.")
def with_local_hook(hook, *procs, &thunk)
  old_procs = []
  hook.to_names.each do |name|
    old_procs.push(hook.remove_hook!(name))
  end
  hook.reset_hook!
  procs.each do |prc|
    hook.add_hook!(prc.object_id.to_s, &prc)
  end
  thunk.call
rescue Interrupt, ScriptError, StandardError
  Snd.display(verbose_message_string(true, nil, get_func_name))
ensure
  hook.reset_hook!
  old_procs.each do |name, prc|
    hook.add_hook!(name, &prc)
  end
end

if defined? $after_graph_hook
  Snd_hooks = [$after_apply_controls_hook,
               $after_graph_hook,
               $after_lisp_graph_hook,
               $after_open_hook,
               $after_save_as_hook,
               $after_save_state_hook,
               $after_transform_hook,
               $bad_header_hook,
               $before_close_hook,
               $before_exit_hook,
               $before_save_as_hook,
               $before_save_state_hook,
               $before_transform_hook,
               $clip_hook,
               $close_hook,
               $draw_mark_hook,
               $draw_mix_hook,
               $drop_hook,
               $during_open_hook,
               $effects_hook,
               $enved_hook,
               $exit_hook,
               $graph_hook,
               $help_hook,
               $initial_graph_hook,
               $key_press_hook,
               $lisp_graph_hook,
               $listener_click_hook,
               $mark_click_hook,
               $mark_drag_hook,
               $mark_hook,
               $mix_click_hook,
               $mix_drag_hook,
               $mix_release_hook,
               $mouse_click_hook,
               $mouse_drag_hook,
               $mouse_enter_graph_hook,
               $mouse_enter_label_hook,
               $mouse_enter_listener_hook,
               $mouse_enter_text_hook,
               $mouse_leave_graph_hook,
               $mouse_leave_label_hook,
               $mouse_leave_listener_hook,
               $mouse_leave_text_hook,
               $mouse_press_hook,
               $mus_error_hook,
               $name_click_hook,
               $new_sound_hook,
               $new_widget_hook,
               $open_hook,
               $open_raw_sound_hook,
               $orientation_hook,
               $output_comment_hook,
               $play_hook,
               $read_hook,
               $save_hook,
               $save_state_hook,
               $select_channel_hook,
               $select_sound_hook,
               $snd_error_hook,
               $snd_warning_hook,
               $start_playing_hook,
               $start_playing_selection_hook,
               $stop_playing_hook,
               $stop_playing_selection_hook,
               $update_hook]

  unless provided? :snd_nogui
    Snd_hooks.push($color_hook)
  end
  
  def reset_all_hooks
    Snd_hooks.each do |h|
      h.kind_of?(Hook) and h.reset_hook!
    end
    Snd.sounds.each do |snd|
      channels(snd).times do |chn|
        (h = edit_hook(snd, chn)).kind_of?(Hook)       and h.reset_hook!
        (h = after_edit_hook(snd, chn)).kind_of?(Hook) and h.reset_hook!
        (h = undo_hook(snd, chn)).kind_of?(Hook)       and h.reset_hook!
      end
    end
  end
end

# hooks.rb ends here
