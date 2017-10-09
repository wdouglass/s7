# ws.rb -- with_sound and friends for Snd/Ruby

# Copyright (c) 2003-2017 Michael Scholz <mi-scholz@users.sourceforge.net>
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.
#
# Created: 03/04/08 17:05:03
# Changed: 17/09/25 22:15:33

# module WS
#   ws_getlogin
#   ws_gethostname
#   ws_break(*rest)
#   with_sound(*args) do ... end
#   with_dac(*args) do ... end
#   with_snd(*args) do ... end
#   with_clm(*args) do ... end
#   with_full_sound(*args) do ... end
#   with_temp_sound(*args) do ... end
#   with_temp_snd(snd) do |temp_snd_file_name| ... end
#   clm_load(rbm_file, *args)
#   make_default_comment
#   remove_file(file)
#   each_sample(start, dur) do |samp| ... end
#   process_times
#
# class XenSound
#   name
#   length
#   update
#   revert
#   save
#   close
#   snd_file_name
#   snd_maxamp(chn, edpos)
#   snd_framples(chn, edpos)
#   snd_sample_type    snd_sample_type=(v)
#   snd_header_type    snd_header_type=(v)
#   snd_comment        snd_comment=(v)
#   snd_srate          snd_srate=(v)
#   snd_channels       snd_channels=(v)
#
# class With_sound
#   initialize(*args, &body)
#
# properties:
#   output                    # file name (String)
#   out_snd                   # sound index (XenSound)
#   with_sound_note_hook      # note hook (Hook)
#
# methods:
#   help       (or description, info)
#   inspect
#   to_s
#   describe   (show_local_variables)
#   with_sound(*args) do ... end
#   with_current_sound(*args) do ... end
#   scaled_to(scale) do ... end
#   scaled_by(scale) do ... end
#   with_offset(secs) do ... end
#   sound_let(*args) do |*sl_args| ... end
#   with_mix(*args, file[, beg_time], body_string)
#   with_sound_info(instrument_name, start, dur, binding)
#   run_instrument(start, dur, *locsig_args) do |samp| ... end
#   run_reverb do |ho, samp| ... end
#   run do ... end
#   clm_mix(infile, *args)
#
# class Instrument
#   ws_simp(start, dur, freq, amp, amp_env)
#   ws_violin(start, dur, freq, amp, fm_index, amp_env)
#   make_ws_reader(file, *args)
#   ws_readin(rd)
#   close_ws_reader(rd)
#   ws_location(rd)
#   set_ws_location(rd, v)
#   ws_increment(rd)
#   set_ws_increment(rd, v)
#   ws_srate(file)
#   ws_channels(file)
#   ws_duration(file)
#
# Instruments have access to @ws_output and @ws_reverb variables as
# well as @srate, @channels, @reverb_channels etc.  It is no longer
# necessary (but possible) to use the global variables $output and
# $reverb.
#
# The classes (Snd_|CLM_)Instrument are not really necessary, but the
# name may be more clear if special instruments are added.
# 
#                With_sound
#                    |
#                    v    
#            --- Instrument ---
#            |                |
#            v                v
#      Snd_Instrument  CLM_Instrument
#       |         |           |
#       v         v           v
#    With_Snd  With_DAC    With_CLM
#
# Instruments can use the generalized run-loop 'run_instrument'.
#
# RUN_INSTRUMENT(start, dur, *locsig_args) do |samp|
#   ...
#   (return next sample)
# end
#
# The body (or block) of 'run_instrument' should return the next sample, e.g.:
#
# class Instrument
#   def my_simp(start, dur, freq, amp, amp_env = [0, 1, 1, 1])
#     os = make_oscil(:frequency, freq)
#     en = make_env(:envelope, amp_env, :scaler, amp, :duration, dur)
#     run_instrument(start, dur) do
#       env(en) * oscil(os)
#     end
#   end
# end
# 
# In Snd as well as in a Ruby script:
#      with_sound do my_simp(0, 1, 440, 0.2) end
#
# If the reverb file has only one channel, reverb instruments can use
# the generalized run-loop 'run_reverb'.
#
# RUN_REVERB() do |ho, samp| ... (return next frample) end
#
# HO is the sample on location SAMP of reverb file.
# It replaces
#
#   (beg...len).each do |i|
#     ho = ina(i, @ws_reverb)
#     ...
#   end
#
# with
#
#   run_reverb() do |ho, i|
#     ...
#   end
#
# The body should return a vct of @channels length, the out_frample.
# The body is called seconds2sample(dur + @decay_time) times.
#
# def my_reverb(start, dur, *rest)
#   ...
#   out_frample = Vct.new(@channels, 0.0)
#   run_reverb() do |ho, i|
#     ...
#     out_frample[0] = val0
#     if @channels > 1
#       out_frample[1] = val1
#     end
#     ...
#     out_frample
#   end
# end
#
# The classes Snd_Instrument and CLM_Instrument can be used to define
# special instruments, see FULLMIX in clm-ins.rb.
#
# Class Snd_Instrument instruments can only be used in Snd.  The
# buffer can be created by with_sound or you can provide vcts for
# output and revout.
#
# with_sound(:out_buffer, Vct.new(22050))
#
# Class CLM_Instrument instruments use sample2file output and can be
# used in Snd as well as in Ruby scripts, @ws_output and $output are
# the same, @ws_reverb and $reverb are the same too.

# Global variables can be set in ~/.snd_ruby or in other scripts
# before or after loading ws.rb.
#
# with_sound(:play, 3, :statistics, true, :reverb, :jc_reverb) do
#   fm_violin(0, 1, 440, 0.3)
# end
#
# clm_load("test.rbm", :play, 1, :statistics, true, :verbose, true)
#
# These functions can be called within with_sound or clm_load:
#   scaled_to(scale, &body)
#   scaled_by(scale, &body)
#   with_offset(secs, &body)
#   with_current_sound(*args, &body)
#   sound_let(*args, &body)
#   with_mix(*args) (does not use a block but a string as "body")
#
# SOUND_LET
#
# sound_let(*args) do |*sl_args| ... end
# args: [with_sound-args1, with_sound-body1],
#       [with_sound-args2, with_sound-body2],
#       with_sound_body3,
#       let_args1,
#       let_args2, ...
#
# sound_let works like let except for procedures which are handled by
# with_sound.  with_sound returns a filename which can be used in the
# sound_let body.
#
# sound_let(Proc.new do fm_violin(0, 1, 330, 0.5) end,
#           1024) do |tmp_file, val|
# end
#
# TMP_FILE is returned by with_sound and VAL is connected to 1024.
#
# If with_sound needs args, the args and the procedure must be in an
# array.
#
# sound_let([:scaled_to, 0.3, :output, "sl.snd",
#            Proc.new do fm_violin(0, 1, 330, 0.5) end],
#           1024) do |tmp_file, val|
# end
# 
# Examples:
# 
# One with_sound-call and one temporary file name, arbitrary called TMP:
# 
# sound_let([:reverb, :jc_reverb,
#            Proc.new do fm_violin(0, 1, 220, 0.2) end]) do |tmp|
#   mus_mix(@output, tmp)
# end
# 
# Two with_sound-calls and two temporary file names, arbitrary
# called TEMP_1 and TEMP_2:
# 
# sound_let([:reverb, :jc_reverb,
#            Proc.new do fm_violin(0, 1, 220, 0.2) end],
#           Proc.new do fm_violin(0.5, 1, 440, 0.3) end) do |temp_1, temp_2|
#   mus_mix(temp_1, temp_2)
#   mus_mix(@output, temp_1)
# end
#
# WITH_MIX
#
# with_mix(file, body_string)
# with_mix(file, beg_time, body_string)
# with_mix(with_sound-args, file, beg_time, body_string)
#
# The FILE and BODY_STRING are necessary, BEG_TIME and
# WITH_SOUND-ARGS are optional, BEG_TIME defaults to 0.
#
# Creates a text file named FILE.rbm with contents BODY_STRING and a
# sound file named FILE.snd, the result of clm_load(FILE.rbm).  If
# BODY_STRING is changed, clm_load(FILE.rbm) is called before mixing
# FILE.snd in the underlying @output of with_sound.
#
# Example:
#
# with_sound() do
#   with_mix(:reverb, :jc_reverb, "foo", 0.2, %Q{
#     fm_violin(0, 1, 440, 0.1)
#   })
# end
#
# with_sound's NOTEHOOK and INFO options:
#
# Every time an instrument starts computing, @with_sound_note_hook is
# called with the instrument name, the start time and the duration.
#
# def my_notehook(name, start, dur)
#   if name =~ /violin/
#     Snd.message("%s: start %1.3f, dur %1.3f", name, start, dur)
#   end
# end
#
# with_sound(:notehook, :my_notehook) do
#   fm_violin(0, 1, 440, 0.1)
#   fbell = [0, 1, 2, 1.1, 25, 0.75, 75, 0.5, 100, 0.2]
#   abell = [0, 0, 0.1, 1, 10, 0.6, 25, 0.3, 50, 0.15, 90, 0.1, 100, 0]
#   fm_bell(0.5, 2.0, 220, 0.5, abell, fbell, 1)
# end
# 
# installs the @with_sound_note_hook and prints the line
# 
#   '# fm_violin: start 0.000, dur 1.000'
#
# If option :info is true, every instrument-call prints the line
# mentioned above.
#
# with_sound's SAVE_BODY option:
# 
# Works only if proc (the with_sound-body) is located in a source file
# and proc.to_s returns something like
# #<Proc:0x00000000@(/path/to/source/file):2> (older Ruby versions
# return only #<Proc:0x00000000>, so we can't find the source file and
# line number).
#
# Tries to locate the body code and saves it in the comment string of
# the sound file.  Scanning the source code is very simple and may not
# work in every case.

# CLM examples (see clm.html) and their Snd/Ruby counterparts:
#
# ;; CLM examples
#  (with-sound () 
#    (mix (with-sound (:output "hiho.snd") 
#              (fm-violin 0 1 440 .1))
#            :amplitude .5))
# 
#  (with-sound ()
#    (with-mix () "s1" 0
#      (sound-let ((tmp ()
#                    (fm-violin 0 1 440 .1)))
#        (mix tmp))))
# 
#  (with-sound (:verbose t)
#    (with-mix () "s6" 0
#      (sound-let ((tmp ()
#                    (fm-violin 0 1 440 .1))
#                  (tmp1 (:reverb nrev)
#                    (mix "oboe.snd")))
#        (mix tmp1)
#        (mix tmp :amplitude .2 :output-frame *srate*))
#      (fm-violin .5 .1 330 .1)))
# 
#  (with-sound (:verbose t)
#    (sound-let ((tmp ()
#                  (with-mix () "s7" 0
#                    (sound-let ((tmp ()
#                                  (fm-violin 0 1 440 .1))
#                                (tmp1 ()
#                                  (mix "oboe.snd")))
#                     (mix tmp1)
#                     (mix tmp :output-frame *srate*))
#                   (fm-violin .5 .1 330 .1))))
#      (mix tmp :amplitude .5)))

=begin
# Snd/Ruby examples
with_sound() do
  clm_mix(with_sound(:output, "hiho.snd") do
            fm_violin(0, 1, 440, 0.1)
          end.output, :scaler, 0.5)
end

with_sound() do
  with_mix "s1", %Q{
  sound_let(Proc.new do fm_violin(0, 1, 440, 0.1) end) do |tmp|
    clm_mix(tmp)
  end
  }
end

with_sound(:verbose, true) do
  with_mix "s6", %Q{
  sound_let(Proc.new do fm_violin(0, 1, 440, 0.1) end,
            [:reverb, :nrev,
             Proc.new do clm_mix("oboe.snd") end]) do |tmp, tmp1|
    clm_mix(tmp1)
    clm_mix(tmp, :scaler, 0.2, :output_frame, seconds2samples(1))
  end
  fm_violin(0.5, 0.1, 330, 0.1)
  }
end

with_sound(:verbose, true) do
  sound_let(Proc.new do
                with_mix "s7", 0, %Q{
                  sound_let(Proc.new do fm_violin(0, 1, 440, 0.1) end,
                            Proc.new do clm_mix("oboe.snd") end) do |t1, t2|
                    clm_mix(t2)
                    clm_mix(t1, :output_frame, @srate)
                  end
                  fm_violin(0.5, 0.1, 330, 0.1)
                }
              end) do |t0|
    clm_mix(t0, :scaler, 0.5)
  end
end
=end

require "clm"
require "hooks"

def clm_find_sound_file(file)
  if File.exist?(file)
    file
  else
    fname = false
    if array?($clm_search_list)
      $clm_search_list.each do |path|
        if File.exist?(fs = path + "/" + file)
          fname = fs
          break
        end
      end
    end
    fname
  end
end

def clm_player(s)
  fs = nil
  if string?(s)
    fs = clm_find_sound_file(s)
    unless File.exist?(fs)
      Snd.raise(:no_such_file, s, "need a sound index or a file name")
    end
  elsif sound?(s)
    fs = s
  else
    Snd.raise(:no_such_sound, s, "need a sound index or a file name")
  end
  if provided?(:snd)
    play(fs, :wait, true)
  else
    system("sndplay #{fs}")
  end
end

trace_var(:$clm_table_size) do |val|
  set_clm_table_size(val)
end

with_silence do
  # warning: undefined variable
  $clm_version            = "ruby 2017/09/25"
  $output                 ||= false
  $reverb                 ||= false
  $clm_array_print_length ||= 8
  $clm_clipped            ||= true
  $clm_comment            ||= nil
  $clm_decay_time         ||= 1.0
  $clm_delete_reverb      ||= false
  $clm_file_buffer_size   ||= 65536
  $clm_file_name          ||= "test.snd"
  $clm_info               ||= false
  $clm_notehook           ||= nil
  $clm_play               ||= 0
  $clm_player             ||= false
  $clm_reverb             ||= nil
  $clm_reverb_channels    ||= 1
  $clm_reverb_data        ||= []
  $clm_reverb_file_name   ||= nil
  $clm_statistics         ||= false
  $clm_table_size         ||= 512
  $clm_verbose            ||= false
  $clm_default_frequency  ||= 440.0
  $clm_locsig_type        ||= locsig_type
  $clm_search_list        ||= (ENV["CLM_SEARCH_PATH"] or ".").split(/:/)

  if provided? :snd
    $clm_channels      ||= default_output_chans
    $clm_srate         ||= default_output_srate
    $clm_header_type   ||= default_output_header_type
    $clm_sample_type   ||= default_output_sample_type
    $clm_dac_size      ||= dac_size
  else
    $clm_channels      ||= 1
    $clm_srate         ||= 44100
    $clm_header_type   ||= Mus_next
    $clm_sample_type   ||= Mus_lfloat
    $clm_dac_size      ||= 1024
  end
end

# for backward compatibility
$clm_data_format = $clm_sample_type
trace_var(:$clm_data_format) do |val|
  $clm_sample_type = val
end
$clm_rt_bufsize = $clm_dac_size
trace_var(:$clm_rt_bufsize) do |val|
  $clm_dac_size = val
end

module WS
  ws_ht = mus_header_type2string($clm_header_type)
  ws_st = mus_sample_type2string($clm_sample_type)
  if string?(ws_fn = $clm_file_name)
    ws_fn = File.basename(ws_fn)
  end
  if string?(ws_rf = $clm_reverb_file_name)
    ws_rf = File.basename(ws_rf)
  end
  ws_doc = "\
with_sound(*args) do ... end
   :output             $clm_file_name        (#{ws_fn.inspect})
   :channels           $clm_channels         (#{$clm_channels})
   :srate              $clm_srate            (#{$clm_srate})
   :header_type        $clm_header_type      (#{ws_ht})
   :sample_type        $clm_sample_type      (#{ws_st})
   :reverb             $clm_reverb           (#{$clm_reverb.inspect})
   :reverb_data        $clm_reverb_data      (#{$clm_reverb_data})
   :reverb_channels    $clm_reverb_channels  (#{$clm_reverb_channels})
   :revfile            $clm_reverb_file_name (#{ws_rf.inspect})
   :delete_reverb      $clm_delete_reverb    (#{$clm_delete_reverb})
   :decay_time         $clm_decay_time       (#{$clm_decay_time})
   :scaled_to          false
   :scaled_by          false
   :continue_old_file  false
   :notehook           $clm_notehook         (#{$clm_notehook})
   :dac_size           $clm_dac_size         (#{$clm_dac_size})
   :play               $clm_play             (#{$clm_play})
   :statistics         $clm_statistics       (#{$clm_statistics})
   :comment            $clm_comment          (#{$clm_comment.inspect})
   :locsig_type        $clm_locsig_type      (Mus_interp_linear)
   :verbose            $clm_verbose          (#{$clm_verbose})
   :save_body          false
   :info               $clm_info             (#{$clm_info})
   :clipped            $clm_clipped          (#{$clm_clipped})
   :player             $clm_player           (#{$clm_player.inspect})
   :to_snd (true, false, nil)
   :clm    (true, false, nil)
   :help
Usage: with_sound(:play, 1, :statistics, true) do fm_violin end"

  with_silence do
    unless defined? Etc.getlogin
      require "etc"
    end
    unless defined? Socket.gethostname
      require "socket"
    end
  end

  def ws_getlogin
    if defined? Etc.getlogin
      Etc.getlogin
    else
      ENV["USER"] or "xen"
    end
  end

  def ws_gethostname
    if defined? Socket.gethostname
      Socket.gethostname
    else
      ENV["HOST"] or "localhost"
    end
  end

  def ws_break(*rest)
    msg = format("%s received Break", get_func_name(2))
    unless rest.null?
      msg += ":"
      rest.each do |s|
        msg += format(" %s,", s)
      end
    end
    raise(Break, msg.chomp(","), caller(1))
  end
  
  # :to_snd has the following arguments:
  #   :snd or true  => With_Snd
  #   :clm or false => With_CLM
  #   :dac or nil   => With_DAC
  # :clm has these (false and true swapped)
  #   :snd or false => With_Snd
  #   :clm or true  => With_CLM
  #   :dac or nil   => With_DAC
  # :output "dac"   => With_DAC
  add_help(:with_sound, ws_doc)
  def with_sound(*args, &body)
    if args.member?(:help)
      return Snd.display(get_help(:with_sound))
    end
    if (not provided?(:snd))
      klass = With_CLM
    elsif args.member?(:output) and args[args.index(:output) + 1] == "dac"
      klass = With_DAC
    elsif args.member?(:to_snd)
      case args[args.index(:to_snd) + 1]
      when :snd, TrueClass
        klass = With_Snd
      when :clm, FalseClass
        klass = With_CLM
      when :dac, NilClass
        klass = With_DAC
      end
    elsif args.member?(:clm)
      case args[args.index(:clm) + 1]
      when :snd, FalseClass
        klass = With_Snd
      when :clm, TrueClass
        klass = With_CLM
      when :dac, NilClass
        klass = With_DAC
      end
    else
      klass = With_CLM
    end
    ws = klass.new(*args, &body)
    ws.run(&body)
    ws
  end

  # require "v"
  # with_dac(:statistics, true,
  #          :info, true,
  #          :dac_size, 8192 * 4,
  #          :srate, 22050) do
  #   violin(0.5, 2.5, 440, 0.2, 0.1, [0, 0, 25, 1, 75, 1, 100, 0])
  #   fm_violin(0, 5, 330, 0.2, :fm_index, 10.5)
  #   fm_violin(1, 2, 660, 0.2, :fm_index, 0.8)
  # end
  add_help(:with_dac, ws_doc)
  def with_dac(*args, &body)
    with_sound(:to_snd, :dac, *args, &body)
  end

  add_help(:with_snd, ws_doc)
  def with_snd(*args, &body)
    with_sound(:to_snd, :snd, *args, &body)
  end

  add_help(:with_clm, ws_doc)
  def with_clm(*args, &body)
    with_sound(:to_snd, :clm, *args, &body)
  end

  add_help(:with_full_sound, ws_doc)
  def with_full_sound(*args, &body)
    ws = with_sound(:to_snd, :snd, *args, &body)
    len = framples($snd_opened_sound) / srate($snd_opened_sound).to_f
    set_x_bounds([0, len], $snd_opened_sound)
    ws
  end

  add_help(:with_temp_sound, ws_doc)
  def with_temp_sound(*args, &body)
    ws = with_sound(:to_snd, :clm, :output, tempnam(), *args, &body)
    remove_file(ws.output)
    ws
  end

  add_help(:with_temp_snd,
           "with_temp_snd(snd=false) do |temp_snd_file_name| ... end  \
Saves SND in a temporary file, which name can be accessed in the body code.  \
After finishing the body, the file will be removed.")
  def with_temp_snd(snd = false, &body)
    t = tempnam()
    save_sound_as(t, snd)
    ret = body.call(t)
    remove_file(t)
    ret
  end
 
  add_help(:clm_load,
           "clm_load(rbm_file, *with_sound_args)\n" + ws_doc)
  def clm_load(rbm_file, *args)
    assert_type(File.exist?(rbm_file), rbm_file, 1, "an existing file")
    with_sound(*args) do
      if @verbose
        Snd.message("Loading %s", rbm_file.inspect)
      end
      eval(File.open(rbm_file).read, nil, "(clm_load #{rbm_file})", 1)
    end
  end

  if defined? snd_tempnam
    alias tempnam snd_tempnam
  else
    $file_number = 0
    def tempnam
      dir = (ENV.map do |k, v|
        v if /TMP/ =~ k
      end.compact.first or "/tmp")
      format("%s/snd_%d_%d.snd", dir, $$, $file_number += 1)
    end
  end

  def make_default_comment
    format("# Written %s by %s at %s using clm (%s)",
           Time.new.localtime.strftime("%a %d-%b-%y %H:%M %z"),
           ws_getlogin,
           ws_gethostname,
           $clm_version)
  end
  
  def remove_file(file)
    if provided?(:snd) and sound?(snd = find_sound(file))
      snd.revert
      close_sound_extend(snd)
    end
     File.owned?(file) and File.unlink(file)
  end

  def each_sample(start, dur, &body)
    beg, ends = times2samples(start, dur)
    (beg...ends).each(&body)
  end

  def process_times
    if defined? Process.times
      Process.times
    else
      Time.times
    end
  end
end

class XenSound
  def name
    self.class.name
  end

  def length
    framples(self, false, false)
  end

  def update
    update_sound(self)
  end

  def revert
    revert_sound(self)
  end

  def save
    save_sound(self)
  end

  def close
    close_sound(self)
  end

  def snd_file_name
    file_name(self)
  end

  def snd_maxamp(chn = false, edpos = false)
    maxamp(self, chn, edpos)
  end

  def snd_framples(chn = false, edpos = false)
    framples(self, chn, edpos)
  end

  def snd_sample_type
    sample_type(self)
  end

  def snd_sample_type=(v)
    set_sample_type(self, v)
  end

  def snd_header_type
    header_type(self)
  end

  def snd_header_type=(v)
    set_header_type(self, v)
  end

  def snd_comment
    comment(self)
  end

  def snd_comment=(v)
    set_comment(self, v)
  end

  def snd_srate
    srate(self)
  end

  def snd_srate=(v)
    set_srate(self, v)
  end

  def snd_channels
    channels(self)
  end

  def snd_channels=(v)
    set_channels(self, v)
  end
end

class With_sound
  include Info
  include WS

  def initialize(*args, &body)
    @output          = get_args(args, :output,            $clm_file_name)
    @channels        = get_args(args, :channels,          $clm_channels)
    @srate           = get_args(args, :srate,             $clm_srate)
    @header_type     = get_args(args, :header_type,       $clm_header_type)
    # for backward compatibility
    @sample_type     = get_args(args, :data_format,       $clm_sample_type)
    @sample_type     = get_args(args, :sample_type,       $clm_sample_type)
    @out_buffer      = get_args(args, :out_buffer,        false)

    @reverb          = get_args(args, :reverb,            $clm_reverb)
    @reverb_data     = get_args(args, :reverb_data,       $clm_reverb_data)
    @reverb_channels = get_args(args, :reverb_channels,   $clm_reverb_channels)
    @revfile         = get_args(args, :reverb_file_name,  nil)
    @delete_reverb   = get_args(args, :delete_reverb,     $clm_delete_reverb)
    @rev_buffer      = get_args(args, :rev_buffer,        false)

    @decay_time      = get_args(args, :decay_time,        $clm_decay_time)
    @scaled_to       = get_args(args, :scaled_to,         false)
    @scaled_by       = get_args(args, :scaled_by,         false)

    @continue        = get_args(args, :continue_old_file, false)
    @notehook        = get_args(args, :notehook,          $clm_notehook)
    @dac_size        = get_args(args, :dac_size,          $clm_dac_size)
    @save_body       = get_args(args, :save_body,         false)
    @player          = get_args(args, :player,            $clm_player)

    @play            = get_args(args, :play,              $clm_play)
    @statistics      = get_args(args, :statistics,        $clm_statistics)
    @verbose         = get_args(args, :verbose,           $clm_verbose)
    @info            = get_args(args, :info,              $clm_info)
    @comment         = get_args(args, :comment,           $clm_comment)
    @locsig_type     = get_args(args, :locsig_type,       $clm_locsig_type)
    @clipped         = get_args(args, :clipped,           :undefined)
    @offset          = get_args(args, :offset,            0.0)

    @rtime = @utime = @stime = 0.0
    @stat_framples    = 0
    @stat_sample_type = false
    @stat_header_type = false
    @stat_comment     = nil
    @stat_maxamp      = nil
    @stat_revamp      = nil
    if @reverb and @revfile.null?
      @revfile = make_reverb_file_name
    end
    # play: either :play, true
    #       or     :play, false
    #       or     :play, nil
    #       or     :play, integer (times to play)
    @play = case @play
            when TrueClass
              1
            when FalseClass, NilClass
              0
            when Numeric
              Integer(@play).abs
            else
              0
            end
    # without reverb: either :reverb_channels, 0
    #                 or     :reverb, false
    if @reverb_channels.zero?
      @reverb = $reverb = false
    end
    unless string?(@comment)
      @comment = make_default_comment
    end
    if @save_body and proc?(body)
      @comment << "\n" << body.to_body
    end
    @old_srate = mus_srate
    @old_update_interval = if defined? auto_update_interval
                             auto_update_interval
                           else
                             false
                           end
    @start_frame = 0
    $output = @ws_output = false
    $reverb = @ws_reverb = false
    @out_snd = @rev_snd = false
    @clm_instruments = Hash.new
    @with_sound_note_hook = Hook.new("@with_sound_note_hook", 3, "\
lambda do |inst_name, start, dur| ... end: called if an instrument has \
the run_instrument/run_reverb loop included.
Every time an instrument starts computing, @with_sound_note_hook is \
called with the instrument name INST_NAME, the start time START, and \
the duration DUR.
def my_notehook(name, start, dur)
  if name =~ /violin/
    Snd.message(\"%s: start %1.3f, dur %1.3f\", name, start, dur)
  end
end
with_sound(:notehook, :my_notehook) do
  fm_violin(0, 1, 440, 0.1)
  fbell = [0, 1, 2, 1.1, 25, 0.75, 75, 0.5, 100, 0.2]
  abell = [0, 0, 0.1, 1, 10, 0.6, 25, 0.3, 50, 0.15, 90, 0.1, 100, 0]
  fm_bell(0.5, 2.0, 220, 0.5, abell, fbell, 1)
end
installs the @with_sound_note_hook and prints the line
'# fm_violin: start 0.000, dur 1.000'.")
    case @notehook
    when Proc
      @with_sound_note_hook.add_hook!("wsnh") do |name, start, dur|
        @notehook.call(name, start, dur)
      end
    when Symbol, String
      @with_sound_note_hook.add_hook!("wsnh") do |name, start, dur|
        snd_func(@notehook, name, start, dur)
      end
    end
    self.description = get_help(:with_sound)
  end
  attr_reader :output, :out_snd, :with_sound_note_hook
  alias help description

  def inspect
    s = self.to_s
    unless @clm_instruments.empty?
      s += "\n#<clm_instruments: "
      @clm_instruments.sort do |a, b|
        a[1][1] <=> b[1][1]
      end.each do |k, v|
        s += format("%s [%1.3f-%1.3f]\n", *v)
        s += " " * 19
      end
      s.strip!
      s += ">"
    end
    s
  end
  
  def to_s
    if @reverb
      s = format(", reverb: %s, reverb-channels: %d", @reverb, @reverb_channels)
    else
      s = ""
    end
    format("#<%s: output: %p, channels: %d, srate: %d%s>",
           self.class, @output, @channels, @srate.to_i, s)
  end

  def describe
    show_local_variables
  end
  
  def with_sound(*args, &body)
    com = format("%s#%s: temporary sound, args %p",
                 self.class, get_func_name, args)
    ws = self.class.new(:output, get_args(args, :output, @output),
           :comment, get_args(args, :comment, com),
           :play, get_args(args, :play, false),
           :statistics, get_args(args, :statistics, false),
           :reverb, get_args(args, :reverb, false),
           :reverb_data, get_args(args,
           :reverb_data, @reverb_data),
           :reverb_file_name, get_args(args, :reverb_file_name, @revfile),
           :reverb_channels, get_args(args, :reverb_channels, @reverb_channels),
           :delete_reverb, get_args(args, :delete_reverb, @delete_reverb),
           :decay_time, get_args(args, :decay_time, @decay_time),
           :continue_old_file, get_args(args, :continue_old_file, @continue),
           :out_buffer, get_args(args, :out_buffer, @out_buffer),
           :rev_buffer, get_args(args, :rev_buffer, @rev_buffer),
           :scaled_to, get_args(args, :scaled_to, @scaled_to),
           :scaled_by, get_args(args, :scaled_by, @scaled_by),
           :notehook, get_args(args, :notehook, @notehook),
           :save_body, get_args(args, :save_body, @save_body),
           :channels, get_args(args, :channels, @channels),
           :srate, get_args(args, :srate, @srate),
           :header_type, get_args(args, :header_type, @header_type),
           # for backward compatibility
           :sample_type, get_args(args, :data_format, @sample_type),
           :sample_type, get_args(args, :sample_type, @sample_type),
           :verbose, get_args(args, :verbose, @verbose),
           :info, get_args(args, :info, @info),
           :clipped, get_args(args, :clipped, @clipped),
           :offset, get_args(args, :offset, 0.0),
           &body)
    ws.run(&body)
    ws
  end

  def with_current_sound(*args, &body)
    output, offset, scaled_to, scaled_by = nil
    optkey(args, binding,
           [:output,    tempnam()],
           [:offset,    0.0],
           [:scaled_to, false],
           [:scaled_by, false])
    ws = with_sound(:output,    output,
                    :scaled_to, scaled_to,
                    :scaled_by, scaled_by,
                    :offset,    offset,
                    *args, &body)
    clm_mix(ws.output)
    remove_file(ws.output)
    ws
  end

  def scaled_to(scale, &body)
    with_current_sound(:scaled_to, scale, &body)
  end

  def scaled_by(scale, &body)
    with_current_sound(:scaled_by, scale, &body)
  end
  
  def with_offset(secs, &body)
    with_current_sound(:offset, secs, &body)
  end

  def sound_let(*args, &body)
    outfile_list = []
    arg_list = []
    args.each do |sl_args|
      if proc?(sl_args) or (array?(sl_args) and proc?(sl_args.last))
        if proc?(sl_args)
          ws_body = sl_args
          ws_args = []
        else
          ws_body = sl_args.pop
          ws_args = sl_args.dup
        end
        ws_args.push(:output, tempnam())
        outfile_list.push(with_sound(*ws_args, &ws_body).output)
        arg_list.push(outfile_list.last)
      else
        arg_list.push(sl_args)
      end
    end
    body.call(*arg_list)
  rescue
    raise
  ensure
    outfile_list.apply(:remove_file)
  end

  def with_mix(*args)
    body_str = args.pop
    assert_type(string?(body_str), body_str, 0, "a string (body string)")
    start = 0
    if number?(args[-1])
      start = args.pop
    end
    fname = args.pop
    assert_type(string?(fname), fname, 0, "a string (filename)")
    out_file = fname + ".snd"
    rbm_file = fname + ".rbm"
    snd_time = :load
    if File.exist?(out_file)
      snd_time = File.mtime(out_file)
    end
    old_body = ""
    if File.exist?(rbm_file)
      old_body = File.open(rbm_file).read
    end
    rbm_time = :load
    if old_body == body_str
      rbm_time = File.mtime(rbm_file)
    else
      File.open(rbm_file, "w") do |f|
        f << body_str
      end
      rbm_file = :load
    end
    if snd_time == :load or rbm_time == :load or snd_time < rbm_time
      if @verbose 
        Snd.message("mix remake %s at %1.3f", out_file, start)
      end
      comm  = format("# written %s (Snd: %s)\n", Time.now, snd_version)
      comm += format("[%s, %s]\n", args.to_s.inspect, body_str.inspect)
      self.with_sound(:output, out_file, :comment, comm, *args) do
        eval(File.open(rbm_file).read,
             nil,
             format("(with_mix_load %s)", rbm_file),
             1)
      end
    else
      if @verbose
        Snd.message("mix %s at %1.3f", out_file, start)
      end
    end
    clm_mix(out_file, :output_frame, seconds2samples(start))
  end

  def with_sound_info(name, start, dur, body = binding)
    @clm_instruments.store(body, [name, start, dur])
    if @info and (not @notehook)
      Snd.message("%s: start %1.3f, dur %1.3f", name, start, dur)
    end
    if @notehook
      @with_sound_note_hook.call(name, start, dur)
    end
  end
  
  def run_instrument(start, dur, body)
    @start_frame = seconds2samples(start + @offset)
    with_sound_info(get_func_name(3), start, dur, body)
  end

  # Handles only the first channel of a reverb file.
  # body.call(sample, current_sample_index) => vct of @channels' length
  def run_reverb(&body)
    name = get_func_name(3)
    dur = samples2seconds(@ws_reverb.length) + @decay_time
    with_sound_info("reverb " + name, 0, dur, body)
    if @verbose
      Snd.message("%s on %d in and %d out channels",
                  name, @reverb_channels, @channels)
    end
  end
  
  def run(&body)
    set_mus_file_buffer_size($clm_file_buffer_size)
    set_mus_array_print_length($clm_array_print_length)
    if @clipped == :undefined
      if (@scaled_by or @scaled_to) and
        [Mus_bfloat, Mus_lfloat,
          Mus_bdouble, Mus_ldouble].member?(@sample_type)
        set_mus_clipping(false)
      else
        set_mus_clipping($clm_clipped)
      end
    else
      set_mus_clipping(@clipped)
    end
    if defined? set_auto_update_interval
      set_auto_update_interval(0.0)
    end
    before_output
    frm1 = ws_frample_location
    init_process_time
    run_body(&body)
    after_output
    stop_process_time
    old_sync = false
    if provided? :snd
      if sound?(snd = find_sound(@output))
        old_sync = sync(snd)
        @out_snd = snd.update
      else
        if @header_type == Mus_raw
          @out_snd = open_raw_sound(@output, @channels,
                                    @srate.to_i, @sample_type)
        else
          @out_snd = open_sound(@output)
        end
      end
      set_sync(true, @out_snd)
    end
    set_statistics
    frm2 = ws_frample_location
    if @scaled_to
      scaled_to_sound(frm1, frm2 - frm1)
    end
    if @scaled_by
      scaled_by_sound(frm1, frm2 - frm1)
    end
    if provided? :snd
      if sound?(snd = find_sound(@output))
        if old_sync
          set_sync(old_sync, snd)
        end
        update_time_graph(snd)
      end
    end
    finish_sound
    if @statistics
      statistics
    end
    1.upto(@play) do
      play_it
    end
  end

  protected
  # INFO [ms]
  # Mon Nov 15 14:46:13 CET 2010
  #
  # previous (1.?.? ... 1.9.1)
  #   instance_eval do | | body end
  #
  # current (1.9.2 since July 2010)
  #   instance_eval do |self| body end
  #
  #   if BODY: lambda do | | ... end
  #     ArgumentError: wrong number of arguments (1 for 0)
  #   if BODY: Proc.new do | | ... end
  #     okay (Proc ignores arity)
  def run_body(&body)
    instance_eval(&body)
  rescue Interrupt, ScriptError, NameError, StandardError
    finish_sound
    show_local_variables
    case $!
    when Interrupt, Break
      # C-g, ws_break
      Snd.message("with_sound body: %s", $!.message)
    else
      raise $!
    end
  end
  
  def run_reverb_body
    case @reverb
    when Proc
      @reverb.call(*@reverb_data)
    when String, Symbol
      snd_func(@reverb, *@reverb_data)
    end
  rescue Interrupt, ScriptError, NameError, StandardError
    finish_sound
    show_local_variables
    case $!
    when Interrupt, Break
      # C-g, ws_break
      Snd.message("with_sound body (reverb): %s", $!.message)
    else
      raise $!
    end
  end
  
  def show_local_variables
    Snd.message()
    # run_instrument|reverb-proc => [instrument-name, start, dur]
    # sorted by values[start]
    @clm_instruments.sort do |a, b|
      a[1][1] <=> b[1][1]
    end.each do |proc, vals|
      Snd.message("=== %s [%1.3f-%1.3f] ===", *vals)
      each_variables(binding?(proc) ? proc : proc.binding) do |var, val|
        Snd.message("%s = %s", var, val)
      end
      Snd.message()
    end
    @clm_instruments.values
  end

  def make_reverb_file_name
    s = string?(@output) ? @output : @output.class.to_s
    path = File.split(s).first
    file = File.basename(s, ".*") + ".reverb"
    unless path == "."
      file = path + "/" + file
    end
    file
  end

  def init_process_time
    tms = process_times
    @rtime = Time.now
    @utime = tms.utime
    @stime = tms.stime
  end
  
  def stop_process_time
    tms = process_times
    @rtime = Time.now - @rtime
    @utime = tms.utime - @utime
    @stime = tms.stime - @stime
  end
  
  def before_output
    set_mus_srate(@srate)
  end
  
  def ws_frample_location
  end
  
  def scaled_to_sound(from, to)
  end

  def scaled_by_sound(from, to)
  end
  
  def statistics
    if string?(@ws_output)
      obj_name = ""
    else
      obj_name = " (" + @ws_output.name + ")"
    end
    Snd.message("filename: %p%s", @output, obj_name)
    Snd.message("   chans: %d, srate: %d", @channels, @srate.to_i)
    if @stat_sample_type and @stat_header_type
      Snd.message("  format: %s [%s]",
                  mus_sample_type_name(@stat_sample_type),
                  mus_header_type_name(@stat_header_type))
    end
    if @stat_framples > 0
      Snd.message("  length: %1.3f (%d frames)",
                  @stat_framples / @srate.to_f, @stat_framples)
    end
    Snd.message("    real: %1.3f  (utime %1.3f, stime %1.3f)",
                @rtime, @utime, @stime)
    if @stat_framples > 1
      rt = (@srate.to_f / @stat_framples)
      Snd.message("   ratio: %1.2f  (uratio %1.2f)", @rtime * rt, @utime * rt)
    end
    ws_maxamp_statistics
    unless (comm = @stat_comment).null?
      Snd.message(" comment: %s", comm)
    end
  end
  
  def finish_sound
    if defined? set_auto_update_interval
      set_auto_update_interval(@old_update_interval)
    end
    @reverb and @delete_reverb and (not @continue) and remove_file(@revfile)
    set_mus_srate(@old_srate)
    @stat_framples = ws_frample_location
  end

  def play_it
    if provided? :snd
      # Inside Snd we use a Proc or Method of one arg, a sound INDEX.
      case @player
      when Proc
        @player.call(@out_snd)
      when Method
        snd_func(@player, @out_snd)
      else
        play(@out_snd, :wait, true)
      end
    else
      # Outside Snd we use a Proc or Method of one arg, a sound FILE NAME.
      case @player
      when Proc
        @player.call(@output)
      when Method
        snd_func(@player, @output)
      else
        system("sndplay #{@output}")
      end
    end
  end
end

class Instrument < With_sound
  # Actually it isn't necessary to define instruments as methods of
  # class Instrument.
  def ws_simp(start = 0, dur = 1, freq = 440, amp = 0.5, amp_env = [0, 1, 1, 1])
    os = make_sum_of_cosines(:frequency, freq, :cosines, 3)
    en = make_env(:envelope, amp_env, :scaler, amp, :duration, dur)
    fs = hz2radians(freq)
    pv = make_triangle_wave(:frequency, 6.0, :amplitude, 0.0025 * fs)
    rv = make_rand_interp(:frequency, 8.0, :amplitude, 0.005 * fs)
    run_instrument(start, dur) do
      sum_of_cosines(os, triangle_wave(pv) + rand_interp(rv)) * env(en)
    end
  end

  # simple violin, see snd/fm.html
  def ws_violin(start = 0, dur = 1, freq = 440, amp = 0.5,
                fm_index = 1, amp_env = [0, 1, 1, 1])
    frq_scl = hz2radians(freq)
    maxdev = frq_scl * fm_index
    index1 = maxdev * (5.0 / log(freq))
    index2 = maxdev * 3.0 * ((8.5 - log(freq)) / (3.0 + freq / 1000.0))
    index3 = maxdev * (4.0 / sqrt(freq))
    carrier = make_oscil(:frequency, freq)
    fmosc1 = make_oscil(:frequency, freq)
    fmosc2 = make_oscil(:frequency, freq * 3.0)
    fmosc3 = make_oscil(:frequency, freq * 4.0)
    ampf = make_env(:envelope, amp_env, :scaler, amp, :duration, dur)
    indf1 = make_env(:envelope, [0, 1, 25, 0.4, 75, 0.6, 100, 0],
                     :scaler, index1, :duration, dur)
    indf2 = make_env(:envelope, [0, 1, 25, 0.4, 75, 0.6, 100, 0],
                     :scaler, index2, :duration, dur)
    indf3 = make_env(:envelope, [0, 1, 25, 0.4, 75, 0.6, 100, 0],
                     :scaler, index3, :duration, dur)
    pervib = make_triangle_wave(:frequency, 0.5, :amplitude, 0.0025 *  frq_scl)
    ranvib = make_rand_interp(:frequency, 16.0, :amplitude, 0.005 * frq_scl)
    run_instrument(start, dur) do
      vib = triangle_wave(pervib) + rand_interp(ranvib)
      env(ampf) *
        oscil(carrier,
              vib +
              env(indf1) * oscil(fmosc1, vib) +
              env(indf2) * oscil(fmosc2, 3.0 * vib) +
              env(indf3) * oscil(fmosc3, 4.0 * vib))
    end
  end
  alias violin ws_violin
end

# WSChannel2Vct and WSSampler are helper classes used by class
# Snd_Instrument.  sampler has no possibility to set location
# (and direction) which is needed by instrument grani (clm-ins.rb).
class WSChannel2Vct
  # (channel->vct (beg 0) (dur len) (snd #f) (chn #f) (edpos #f))
  def initialize(snd = false, chn = false, start = 0, dir = 1)
    @vct = channel2vct(0, framples(snd, chn), snd, chn, false)
    @location = ((dir > 0) ? (start - 1) : (start + 1))
    @start = start
    @direction = dir
    @snd = snd
    @chn = chn
    @last_location = @vct.length - 1
  end
  attr_accessor :direction
  
  def inspect
    format("%s.new(%p, %p, %s, %s)",
           self.class, @snd, @chn, @start, @direction)
  end
  
  def to_s
    format("#<%s snd: %p, chn: %p, location: %d, direction: %d, vct: %p>",
           self.class, @snd, @chn, location, @direction, @vct)
  end

  def next
    if @direction > 0
      if @location < @last_location
        @location += 1
        @vct[@location]
      else
        0.0
      end
    else
      if @location > 0
        @location -= 1
        @vct[@location]
      else
        0.0
      end
    end
  end
  
  def close
    # not needed
  end
  
  def location
    if @direction > 0
      @location + 1
    else
      @location - 1
    end
  end

  def location=(v)
    if @direction > 0
      @location = v - 1
    else
      @location = v + 1
    end
  end
end

class WSSampler
  # (make-sampler (start-samp 0) (snd #f) (chn #f) (dir 1) (edpos #f))
  def initialize(snd = false, chn = false, start = 0, dir = 1)
    @reader = make_sampler(start, snd, chn, dir, false)
    @direction = dir
    @start = start
    @direction = dir
    @snd = snd
    @chn = chn
  end
  attr_accessor :direction
  
  def inspect
    format("%s.new(%s, %s, %s, %s)",
           self.class, @snd.inspect, @chn.inspect, @start, @direction)
  end
  
  def to_s
    @reader.inspect
  end

  def next
    @reader.call
  end

  def close
    free_sampler(@reader)
    if sound?(@snd)
      close_sound_extend(@snd)
    end
  end
  
  def location
    sampler_position(@reader)
  end

  def location=(v)
    # sampler_position isn't settable
  end
end

class Snd_Instrument < Instrument
  # place holder for special Snd instruments, see FULLMIX in
  # clm-ins.rb.
  def make_ws_reader(file, *args)
    start, channel, direction = nil
    optkey(args, binding,
           [:start, 0],
           [:channel, 0],
           [:direction, 1])
    if get_args(args, :vct?, false)
      WSChannel2Vct.new(get_snd(file), channel, start, direction)
    else
      WSSampler.new(get_snd(file), channel, start, direction)
    end
  end

  # (read-sample reader)
  def ws_readin(rd)
    rd.next
  end

  def close_ws_reader(rd)
    rd.close
  end

  def ws_location(rd)
    rd.location
  end

  def set_ws_location(rd, v)
    rd.location = v.to_i
  end

  def ws_increment(rd)
    rd.direction
  end

  def set_ws_increment(rd, v)
    rd.direction = v.to_i
  end
  
  def ws_srate(file)
    srate(get_snd(file)).to_f
  end

  def ws_channels(file)
    channels(get_snd(file))
  end
  
  def ws_duration(file)
    snd = get_snd(file)
    framples(snd) / srate(snd).to_f
  end

  private
  def get_snd(file)
    snd = if integer?(file)
            integer2sound(file)
          elsif string?(file)
            if sound?(s = find_sound(file))
              s
            else
              open_sound(file)
            end
          end
    if sound?(snd)
      snd
    else
      Snd.snd(snd)
    end
  end
end

class CLM_Instrument < Instrument
  # place holder for special Snd instruments, see FULLMIX in
  # clm-ins.rb.

  # (make-readin (:file) (:channel 0) (:start 0) (:direction 1)
  def make_ws_reader(file, *args)
    start, channel, direction = nil
    optkey(args, binding,
           [:start, 0],
           [:channel, 0],
           [:direction, 1])
    make_readin(:file, file, :channel, channel,
                :start, start, :direction, direction)
  end

  # (readin gen)
  def ws_readin(rd)
    readin(rd)
  end

  def close_ws_reader(rd)
    mus_close(rd)
  end

  def ws_location(rd)
    mus_location(rd)
  end

  def set_ws_location(rd, v)
    set_mus_location(rd, v)
  end

  def ws_increment(rd)
    mus_increment(rd)
  end

  def set_ws_increment(rd, v)
    set_mus_increment(rd, v)
  end

  def ws_srate(file)
    mus_sound_srate(file)
  end

  def ws_channels(file)
    mus_sound_chans(file)
  end
  
  def ws_duration(file)
    mus_sound_duration(file)
  end
end

class With_Snd < Snd_Instrument
  def run_instrument(start, dur, *locsig_args, &body)
    super(start, dur, body)
    # locsig and Vct as :output and :revout handles only 1 channel.
    if @channels == 1 and @reverb_channels < 2
      run_inst_with_locsig(start, dur, *locsig_args, &body)
    else
      run_inst_with_map(start, dur, *locsig_args, &body)
    end
  end

  def run_reverb(&body)
    super(&body)
    # run_reverb handles only one reverb channel
    vo = channel2vct(0, false, @rev_snd, 0, false)
    vl = vo.length + seconds2samples(@decay_time)
    vr = Array.new(@channels) do
      Vct.new(vl, 0.0)
    end
    vo.each_with_index do |s, i|
      fr = body.call(s, i)
      @channels.times do |chn|
        vr[chn][i] = fr[chn]
      end
    end
    origin = format("%s(&body", get_func_name)
    @channels.times do |chn|
      mix_vct(vr[chn], 0, @out_snd, chn, false, origin)
    end
  end

  add_help(:clm_mix,
           "clm_mix(infile, *args)
        :output       = false
        :output_frame = 0
        :frames       = framples(infile)
        :input_frame  = 0
        :scaler       = false
Example: clm_mix(\"tmp\")")
  def clm_mix(infile, *args)
    output, output_frame, frames, input_frame, scaler = nil
    optkey(args, binding,
           [:output, false],            # dummy arg
           [:output_frame, 0],
           [:frames, framples(infile)],
           [:input_frame, 0],
           [:scaler, false])
    unless sound?(snd = find_sound(infile))
      unless snd = open_sound(infile)
        Snd.raise(:no_such_file, infile, "file name required")
      end
    end
    [channels(snd), @channels].min.times do |chn|
      if scaler and scaler.nonzero?
        scale_channel(scaler, input_frame, frames, snd, chn)
      end
      mix_vct(channel2vct(input_frame, frames, snd, chn),
              output_frame, snd, chn, false)
    end
    snd.revert
    close_sound_extend(snd)
    # INFO: output
    # silence "warning: assigned but unused variable - output"
    output
  end

  protected
  def run_inst_with_locsig(start, dur, *locsig_args, &body)
    degree, distance, reverb_amount = nil
    optkey(locsig_args, binding,
           [:degree, random(90.0)],
           [:distance, 1.0],
           [:reverb_amount, 0.05])
    vo = Vct.new(seconds2samples(dur), 0.0)
    vr = @reverb ? Vct.new(seconds2samples(dur), 0.0) : false
    @locsig = make_locsig(:degree,   degree,
                          :distance, distance,
                          :reverb,   reverb_amount,
                          :output,   vo,
                          :revout,   vr,
                          :channels, @channels,
                          :type,     @locsig_type)
    vo.length.times do |i|
      locsig(@locsig, i, body.call(@start_frame + i))
    end
    origin = format("run_instrument(%s, %s, %s, &body",
                    start, dur,
                    locsig_args.to_s[1..-2])     # get rid of '[' and ']'
    mix_vct(vo, @start_frame, @out_snd, 0, false, origin)
    if @reverb
      mix_vct(vr, @start_frame, @rev_snd, 0, false, origin)
    end
  end

  def run_inst_with_map(start, dur, *locsig_args, &body)
    origin = format("run_instrument(%s, %s, %s, &body",
                    start, dur,
                    locsig_args.to_s[1..-2])
    vo = Vct.new(seconds2samples(dur)) do |i|
      body.call(@start_frame + i)
    end
    @channels.times do |chn|
      mix_vct(vo, @start_frame, @out_snd, chn, false, origin)
    end
    if @reverb
      ra = get_args(locsig_args, :reverb_amount, 0.05)
      vr = vo.scale(ra)
      @reverb_channels.times do |chn|
        mix_vct(vr, @start_frame, @rev_snd, chn, false, origin)
      end
    end
  end

  def before_output
    super
    snd = rsnd = false
    sr = mus_srate.to_i
    snd = find_sound(@output)
    if sound?(snd = find_sound(@output))
      if @continue
        @srate = set_mus_srate(snd.snd_srate)
      else
        snd.snd_header_type = @header_type
        snd.snd_sample_type = @sample_type
        snd.snd_srate = sr
        snd.snd_channels = @channels
        snd.snd_comment = @comment
        snd.snd_channels.times do |chn|
          set_framples(1, snd, chn)
        end
        snd.save
        snd = snd.update
      end
    else
      unless @continue
        remove_file(@output)
      end
      snd = new_sound(@output, @channels, sr,
                      @sample_type, @header_type, @comment)
    end
    if @reverb
      if sound?(rsnd = find_sound(@revfile)) and (not @continue)
        rsnd.snd_header_type = @header_type
        rsnd.snd_sample_type = @sample_type
        rsnd.snd_srate = sr
        rsnd.snd_channels = @reverb_channels
        rsnd.snd_channels.times do |chn|
          set_framples(1, rsnd, chn)
        end
        rsnd.save
        rsnd = rsnd.update
      else
        unless @continue
          remove_file(@revfile)
        end
        rsnd = new_sound(@revfile, @reverb_channels, sr,
                         @sample_type, @header_type)
      end
    end
    $output = @ws_output = @out_snd = snd
    $reverb = @ws_reverb = @rev_snd = rsnd
  end

  def after_output
    @reverb and run_reverb_body
    @out_snd.save
  end
  
  def ws_frample_location
    @out_snd.length
  end
  
  def scaled_to_sound(beg, len)
    scl = @scaled_to / maxamp(@out_snd, true).max
    @channels.times do |chn|
      scale_channel(scl, beg, len, @out_snd, chn)
    end
    @out_snd.save
  end

  def scaled_by_sound(beg, len)
    @channels.times do |chn|
      scale_channel(@scaled_by, beg, len, @out_snd, chn)
    end
    @out_snd.save
  end
  
  def set_statistics
    @stat_framples    = @out_snd.length
    @stat_sample_type = @out_snd.snd_sample_type
    @stat_header_type = @out_snd.snd_header_type
    @stat_comment     = @out_snd.snd_comment
    @stat_maxamp      = @out_snd.snd_maxamp(true)
    if @reverb
      @stat_revamp    = @rev_snd.snd_maxamp(true)
    end
  end
  
  def ws_maxamp_statistics
    Snd.message(" max out: %s%s",
                @stat_maxamp.to_string,
                (@scaled_to or @scaled_by) ? " (before scaling)" : "")
    if @reverb
      Snd.message(" max rev: %s", @stat_revamp.to_string)
    end
  end

  # with_closed_sound(snd) do |snd_name| ... end
  # returns new snd index
  # see clm-ins.rb, run_fullmix
  def with_closed_sound(snd, &body)
    snd_name = snd.file_name
    snd = snd.update
    close_sound_extend(snd)
    body.call(snd_name)
    open_sound(snd_name)
  end
end

class With_CLM < CLM_Instrument
  def run_instrument(start, dur, *locsig_args, &body)
    super(start, dur, body)
    degree, distance, reverb_amount = nil
    optkey(locsig_args, binding,
           [:degree, random(90.0)],
           [:distance, 1.0],
           [:reverb_amount, 0.05])
    @locsig = make_locsig(:degree,   degree,
                          :distance, distance,
                          :reverb,   reverb_amount,
                          :output,   @ws_output,
                          :revout,   @ws_reverb,
                          :channels, @channels,
                          :type,     @locsig_type)
    @start_frame.upto((@start_frame + seconds2samples(dur)) - 1) do |i|
      locsig(@locsig, i, body.call(i))
    end
  end

  def run_reverb(&body)
    super(&body)
    (@ws_reverb.length + seconds2samples(@decay_time)).times do |i|
      frample2file(@ws_output, i, body.call(file2sample(@ws_reverb, i, 0), i))
    end
  end

  add_help(:clm_mix,
           "clm_mix(infile, *args)
        :output       = false
        :output_frame = 0
        :frames       = mus_sound_framples(infile)
        :input_frame  = 0
        :scaler       = false
Example: clm_mix(\"tmp\")")
  def clm_mix(infile, *args)
    output, output_frame, frames, input_frame, scaler = nil
    optkey(args, binding,
           [:output, false],
           [:output_frame, 0],
           [:frames, mus_sound_framples(infile)],
           [:input_frame, 0],
           [:scaler, false])
    chans = 0
    outgen = mus_output?(@ws_output)
    unless output
      if outgen
        chans = @ws_output.channels
        output = @ws_output.file_name
      else
        Snd.raise(:no_such_sound, @ws_output, "output generator required")
      end
    end
    unless infile = clm_find_sound_file(infile)
      Snd.raise(:no_such_file, infile, "file name required")
    end
    if outgen
      @ws_output.close
    end
    mx = false
    if chans > 0 and scaler and scaler != 0.0
      mx = Vct.new(chans * chans, scaler)
    end
    mus_file_mix(output, infile, output_frame, frames, input_frame, mx)
    if outgen
      @ws_output = continue_sample2file(output)
    end
  end
  
  protected
  def before_output
    super
    if @continue
      @ws_output = continue_sample2file(@output)
      @srate = set_mus_srate(mus_sound_srate(@output))
      if @reverb
        @ws_reverb = continue_sample2file(@revfile)
      end
      if provided?(:snd) and sound?(snd = find_sound(@output))
        close_sound_extend(snd)
      end
    else
      remove_file(@output)
      @ws_output = make_sample2file(@output, @channels,
                                    @sample_type, @header_type, @comment)
      if @reverb
        remove_file(@revfile)
        @ws_reverb = make_sample2file(@revfile, @reverb_channels,
                                      @sample_type, @header_type)
      end
    end
    $output = @ws_output
    $reverb = @ws_reverb
  end

  def after_output
    if @reverb
      mus_output?(@ws_reverb) and @ws_reverb.close
      old_reverb = @ws_reverb
      # non-RUN_REVERB...END functions need it here
      $reverb = @ws_reverb = make_file2sample(@revfile)
      run_reverb_body
      mus_input?(@ws_reverb) and @ws_reverb.close
      $reverb = @ws_reverb = old_reverb
    end
    mus_output?(@ws_output) and @ws_output.close
  end

  def ws_frample_location
    with_closed_output do
      mus_sound_framples(@output)
    end
  end
  
  def scaled_to_sound(beg, len)
    if provided? :snd
      if sound?(@out_snd = find_sound(@output))
        @channels.times do |chn|
          scale_to(@scaled_to, @out_snd, chn)
        end
        @out_snd.save
      end
    else
      omax = mus_sound_maxamp(@output)
      mx = 0.0
      1.step(omax.length - 1, 2) do |i|
        mx = [omax[i].abs, mx].max
      end
      if mx.zero?
        mx = 1.0
      end
      clm_mix(@output, :output_frame, beg,
              :frames, len, :scaler, @scaled_to / mx)
    end
  end

  def scaled_by_sound(beg, len)
    if provided? :snd
      if sound?(@out_snd = find_sound(@output))
        @channels.times do |chn|
          scale_channel(@scaled_by, beg, len, @out_snd, chn)
        end
        @out_snd.save
      end
    else
      clm_mix(@output, :output_frame, beg, :frames, len, :scaler, @scaled_by)
    end
  end

  def set_statistics
    @stat_framples    = mus_sound_framples(@output)
    @stat_sample_type = mus_sound_sample_type(@output)
    @stat_header_type = mus_sound_header_type(@output)
    @stat_comment     = mus_sound_comment(@output)
    @stat_maxamp      = mus_sound_maxamp(@output)
    if @reverb
      @stat_revamp    = mus_sound_maxamp(@revfile)
    end
  end
  
  def ws_maxamp_statistics
    sr = @srate.to_f
    ch = "@"
    @stat_maxamp.each_pair do |s, v|
      Snd.message("maxamp %s: %1.3f (near %1.3f secs)%s",
                  ch.next!, v, s / sr,
                  (@scaled_to or @scaled_by) ? " (before scaling)" : "")
    end
    if @reverb
      ch = "@"
      @stat_revamp.each_pair do |s, v|
        Snd.message("revamp %s: %1.3f (near %1.3f secs)", ch.next!, v, s / sr)
      end
    end
  end

  def with_closed_output(&body)
    mus_output?(@ws_output) and @ws_output.close
    ret = body.call()
    $output = @ws_output = continue_sample2file(@output)
    ret
  end
end

class With_DAC < Snd_Instrument
  def initialize(*args, &body)
    super(:output, "dac", :reverb, false, *args, &body)
  end

  def to_s
    format("#<%s: channels: %d, srate: %d, dac_size: %d>",
           self.class, @channels, @srate.to_i, @dac_size)
  end

  def run_instrument(start, dur, *locsig_args, &body)
    # dac.run_instrument needs get_func_name(2) here
    with_sound_info(get_func_name(2), start, dur, body)
    @locsig = false
  end

  def with_current_sound(*args, &body)
  end

  def run(&body)
    before_output
    set_mus_file_buffer_size($clm_file_buffer_size)
    init_process_time
    run_body(&body)
    play_it
    stop_process_time
    if @statistics
      statistics
    end
  end

  protected
  def statistics
    Snd.message("filename: %s", @output)
    Snd.message("   chans: %d, srate: %d", @channels, @srate.to_i)
    Snd.message("    real: %1.3f  (utime %1.3f, stime %1.3f)",
                @rtime, @utime, @stime)
    if @stat_framples > 0
      Snd.message("  length: %1.3f (%d frames)",
                  @stat_framples / @srate.to_f, @stat_framples)
    end
  end

  def play_it
    len = 0
    # [body, [name, start, dur]]
    insts = @clm_instruments.sort do |a, b|
      a[1][1] <=> b[1][1]
    end.to_a.map! do |body, args|
      start = args[1]
      dur = args[2]
      beg, ends = times2samples(start, dur)
      if len < ends
        len = ends
      end
      [beg, ends, body]
    end
    oc = mus_clipping()
    set_mus_clipping(true)
    od = dac_size()
    set_dac_size(@dac_size)
    len += @dac_size
    len += mus_file_buffer_size()
    @stat_framples = len
    s = 0
    play(lambda do
           if s < len
             sum = 0.0
             insts.each do |args|
               beg, ends, body = args
               if s.between?(beg, ends)
                 sum += body.call(s)
               end
             end
             s += 1
             sum
           else
             false
           end
         end)
    set_mus_clipping(oc)
    set_dac_size(od)
    set_mus_srate(@old_srate)
  end
end

include WS

# ws.rb ends here
