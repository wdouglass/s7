# extensions.rb -- various generally useful Snd extensions (see extensions.scm)

# Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 2004/01/03 17:30:23
# Changed: 2018/04/15 22:50:30

# module Compatibility
#  scan_sound_chans(beg, end, snd, edpos) do |y| ... end
#  map_sound_chans(beg, end, edname, snd, edpos) do |y| ... end
#  scan_all_chans(beg, end, edpos) do |y| ... end
#  map_all_chans(beg, end, edname, edpos) do |y| ... end
#  scan_chans(beg, end, edpos) do |y| ... end
#  map_chans(beg, end, edname, edpos) do |y| ... end
#  scan_across_all_chans(beg, end, snd, edpos) do |data, chns| ... end
#  map_across_all_chans(beg, end, edname, snd, edpos) do |data, chns| ... end
#  selection_to_temp(type = Mus_next, format = Mus_out_format)
#  syncd_sounds(val)
#  sound_to_temp(type = Mus_next, format = Mus_out_format, edpos = false)
#  selection_to_temps(type = Mus_next, format = Mus_out_format)
#  sound_to_temps(type = Mus_next, format = Mus_out_format, edpos = false)
#  temp_to_sound(data, filename, origin = false)
#  temps_to_sound(data, filenames, origin = false)
#  temp_to_selection(data, filename, origin = false)
#  temps_to_selection(data, filenames, origin = false)
#  
#  forward_sample(count, snd, chn)
#  backward_sample(count, snd, chn)
#  
#  back_or_forth_graph(count)
#  forward_graph(count)
#  backward_graph(count)
#  back_or_forth_mix(count)
#  forward_mix(count = 1, snd = false, chn = false)
#  backward_mix(count = 1, snd = false, chn = false)
#  back_or_forth_mark(count)
#  forward_mark(count = 1, snd = false, chn = false)
#  backward_mark(count = 1, snd = false, chn = false)
#  vct_convolve!(r1, r2)
#  old_map_channel(beg, dur, snd, chn, edpos, edname, &func)
#  mus_bank(gens, amps, in1, in2)
#  old_oscil_bank(amps, gens, in1, in2)
#  old_formant_bank(amps, gens, in1)
#  vct2samples(samp, samps, data, snd, chn)
#  samples2vct(samp, samps, snd, chn, nv, epos)
#  scale_sound_by(scl, beg, dur, snd, chn, edpos)
#  scale_sound_to(norm, beg, dur, snd, chn)
#  emacs_style_save_as
#  set_emacs_style_save_as(val)
#  make_iir_low_pass_1(fc)
#  make_iir_high_pass_1(fc)
#  sine_bank(amps, phases, size)
#
# module Extensions
#  remove_channel_property(key, snd, chn)
#  remove_sound_property(key, snd)
#
#  channel_sync(snd, chn)
#  set_channel_sync(val, snd, chn)
#
#  set_sound_property_save_state_ignore(key, snd)
#  set_channel_property_save_state_ignore(key, snd, chn)
#  show_sound_properties(snd)
#  show_channel_properties(snd, chn)
#
#  properties2string(snd)
#  save_properties(fname)
#  
#  remember_all_sound_properties(database, tmp_snd_p)
#  remember_properties
#  class Remember_sound_properties
#    initialize(database)
#    inspect
#    with_db do |db| ... end
#    load(snd)
#    save(snd)
#    each do |k, v| ... end
#    delete_if do |k, v| ... end
#    contents
#    help
#  
#  add_comment(samp, comm, snd, chn)
#  remove_comment(comm, snd, chn)
#  show_comments(snd, chn)
#  
#  marks?(more_than_one)
#  all_chans
#  all_chans_zipped
#  normalized_mix(fname, beg, in_chan, snd, chn)
#  enveloped_mix(fname, beg, env, del_tmp)
#  
#  map_sound_files(*args) do |f| ... end
#  for_each_sound_file(*args) do |f| ... end    alias each_sound_file
#  match_sound_files(*args) do |f| ... end
#  
#  selection_members
#  clear_selection
#  make_selection(beg, len, snd, chn)
#  delete_selection_and_smooth()
#
#  mix_channel(fdata, beg, dur, snd, chn, edpos)
#  insert_channel(fdata, beg, dur, snd, chn, edpos)
#  redo_channel(edits, snd, chn)
#  undo_channel(edits, snd, chn)
#  
#  any_env_channel(en, b, d, s, c, ep, ori) do |r0, r1, b, d, s, c, e| ... end
#  sine_ramp(rmp0, rmp1, beg, dur, snd, chn, edpos)
#  sine_env_channel(env, beg, dur, snd, chn, edpos)
#  blackman4_ramp(rmp0, rmp1, beg, dur, snd, chn, edpos)
#  blackman4_env_channel(env, beg, dur, snd, chn, edpos)
#  ramp_squared(rmp0, rmp1, symmetric, beg, dur, snd, chn, edpos)
#  env_squared_channel(env, symmetric, beg, dur, snd, chn, edpos)
#  ramp_expt(rmp0, rmp1, exponent, symmetric, beg, dur, snd, chn, edpos)
#  env_expt_channel(env, exponent, symmetric, beg, dur, snd, chn, edpos)
#  offset_channel(amount, beg, dur, snd, chn, edpos)
#  offset_sound(amount, beg, dur, snd)
#  pad_sound(beg, dur, snd)
#  dither_channel(amnt, beg, dur, snd, chn, edpos)
#  dither_sound(amnt, beg, dur, snd)
#  contrast_channel(index, beg, dur, snd, chn, edpos)
#  contrast_sound(index, beg, dur, snd)
#  channels_eql?(snd1, chn1, snd2, chn2, allowable_difference)
#  channels_equal?(snd1, chn1, snd2, chn2, allowable_difference)
#  mono2stereo(new_name, snd1, chn1, snd2, chn2)
#  mono_files2stereo(new_name, chan1_name, chan2_name)
#  stereo2mono(orig_snd, chan1_name, chan2_name)
#
#  with_reopen_menu
#  with_buffers_menu

# Comments are mostly taken from extensions.scm.

require "hooks"
include Math

module Compatibility
  # 
  # Snd-4 compatibility stuff
  # 
  add_help(:scan_sound_chans,
           "scan_sound_chans(beg=0, end=flase, snd=false, edpos=false, &proc)  \
Applies scan_chan with PROC to each channel in a sound.")
  def scan_sound_chans(beg = 0, fin = false, snd = false, edpos = false, &body)
    # Proc#arity returns -1 instead of 1 on older versions
    if body and body.arity.abs == 1
      val = nil
      c = (0...chans(snd)).detect do |chn|
        val = scan_chan(body, beg, fin, snd, chn, edpos)
      end
      if c
        val += [Snd.snd(snd), c]
      else
        nil
      end
    else
      Snd.raise(:bad_arity,
                "scan_sound_chans(beg, end, snd, edpos) do |y| ... end")
    end
  end

  add_help(:map_sound_chans,
           "map_sound_chans(beg=0, end=flase, snd=false, edpos=false, &proc)  \
Applies map_chan with PROC to each channel in a sound.")
  def map_sound_chans(beg = 0, fin = false, edname = false,
                      snd = false, edpos = false, &body)
    if body and body.arity.abs == 1
      channels(snd).times do |chn|
        map_chan(body, beg, fin, edname, snd, chn, edpos)
      end
    else
      Snd.raise(:bad_arity,
                "map_sound_chans(beg, end, ename, snd, edpos) do |y| ... end")
    end
  end

  add_help(:scan_all_chans,
           "scan_all_chans(beg=0, end=flase, edpos=false, &proc)  \
Applies scan_chan with PROC to all channels (all sounds).")
  def scan_all_chans(beg = 0, fin = false, edpos = false, &body)
    if body and body.arity.abs == 1
      catch(:done) do
        Snd.sounds.each do |snd|
          channels(snd).times do |chn|
            if res = scan_chan(body, beg, fin, snd, chn, edpos)
              throw(:done, res += [snd, chn])
            end
          end
        end
        false
      end
    else
      Snd.raise(:bad_arity,
                "scan_all_chans(beg, end, edpos) do |y| ... end")
    end
  end
  
  add_help(:map_all_chans,
           "map_all_chans(beg=0, end=flase, edpos=false, &proc)  \
Applies map_chan with proc to all channels (all sounds).")
  def map_all_chans(beg = 0, fin = false, edname = false, edpos = false, &body)
    if body and body.arity.abs == 1
      Snd.sounds.each do |snd|
        channels(snd).times do |chn|
          map_chan(body, beg, fin, edname, snd, chn, edpos)
        end
      end
    else
      Snd.raise(:bad_arity,
                "map_all_chans(beg, end, ename, edpos) do |y| ... end")
    end
  end

  add_help(:scan_chan,
           "scan_chans(beg=0, end=flase, edpos=false, &proc)  \
Applies scan_chan with PROC to all channels sharing current sound's sync.")
  def scan_chans(beg = 0, fin = false, edpos = false, &body)
    if body and body.arity.abs == 1
      current_sync = sync(selected_sound)
      catch(:done) do
        Snd.sounds.each do |snd|
          if sync(snd) == current_sync
            channels(snd).times do |chn|
              if res = scan_chan(body, beg, fin, snd, chn, edpos)
                throw(:done, res += [snd, chn])
              end
            end
          end
        end
        false
      end
    else
      Snd.raise(:bad_arity,
                "scan_chans(beg, end, edpos) do |y| ... end")
    end
  end

  add_help(:map_chan,
           "map_chans(beg=0, end=false, edpos=false, &proc)  \
Applies map_chan with PROC to all channels sharing current sound's sync.")
  def map_chans(beg = 0, fin = false, edname = false, edpos = false, &body)
    if body and body.arity.abs == 1
      current_sync = sync(selected_sound)
      Snd.sounds.each do |snd|
        if sync(snd) == current_sync
          channels(snd).times do |chn|
            map_chan(body, beg, fin, edname, snd, chn, edpos)
          end
        end
      end
    else
      Snd.raise(:bad_arity,
                "map_chans(beg, end, ename, edpos) do |y| ... end")
    end
  end

  add_help(:scan_across_all_chans,
           "scan_across_all_chans(beg=0, end=flase, \
snd=false, edpos=false, &proc)  \
Applies map_chan with PROC to all channels in parallel.")
  # body.call(data, chan_num)
  def scan_across_all_chans(beg = 0, fin = false,
                            snd = false, edpos = false, &body)
    chans = all_chans
    chan_num = chans.first.length
    maxlen = 0
    Snd.sounds.each do |s|
      channels(s).times do |chn|
        maxlen = [maxlen, framples(s, chn)].max
      end
    end
    len = integer?(fin) ? ([fin, maxlen].min - beg) : (maxlen - beg)
    data = make_array(chan_num, 0.0)
    fds = make_array(chan_num) do |chn|
      make_sampler(beg, chans[0].shift, chans[1].shift, 1, edpos)
    end
    catch(:done) do
      len.times do |i|
        data.map_with_index! do |d, chn|
          next_sample(fds[chn])
        end
        if newdata = body.call(data, chan_num)
          throw(:done, [newdata, i + beg])
        end
      end
    end
  end
  
  add_help(:map_across_all_chans,
           "map_across_all_chans(beg=0, end=flase, edname=false, \
snd=false, edpos=false, &proc)  \
Applies map_chan with PROC to all channels in parallel.")
  def map_across_all_chans(beg = 0, fin = false, edname = false,
                           snd = false, edpos = false, &body)
    snds, chans = all_chans
    chan_num = snds.length
    maxlen = 0
    Snd.sounds.each do |sd|
      channels(sd).times do |chn|
        maxlen = [maxlen, framples(sd, chn)].max
      end
    end
    len = integer?(fin) ? ([fin, maxlen].min - beg) : (maxlen - beg)
    data = make_array(chan_num)
    fds = make_array(chan_num) do |chn|
      make_sampler(beg, snds[chn], chans[chn], 1, edpos)
    end
    filenames = make_array(chan_num) do |chn|
      snd_tempnam
    end
    outgs = make_array(chan_num) do |chn|
      make_sample2file(filenames[chn], 1, Mus_out_format, Mus_next)
    end
    outsamp = 0
    len.times do |i|
      data.map_with_index! do |d, chn|
        next_sample(fds[chn])
      end
      if (newdata = body.call(data, chan_num))
        newdata.each_with_index do |dat, chn|
          out_any(outsamp, dat, 0, outgs[chn])
        end
        outsamp += 1
      end
    end
    filenames.each_with_index do |file, chn|
      mus_close(outgs[chn])
      free_sampler(fds[chn])
      if outsamp > 0
        delete_samples(beg, len, snds[chn], chans[chn]) if outsamp != len
        set_samples(beg, outsamp, file, snds[chn], chans[chn], true, edname)
        delete_file(file)
      end
    end
  end

  # 
  # Snd-4 external program support stuff
  # 
  add_help(:selection_to_temp,
           "selection_to_temp(type=Mus_next, format=Mus_out_format)  \
Writes selection data as (multichannel) file (for external program).")
  def selection_to_temp(type = Mus_next, format = Mus_out_format)
    data = [snd_tempnam]
    save_selection(data.first, 44100, format, type)
    data
  end

  def syncd_sounds(val)
    ctr = 0
    Snd.sounds.each do |snd|
      if sync(snd) == val
        ctr += 1
      end
    end
    ctr
  end

  add_help(:sound_to_temp,
           "sound_to_temp(type=Mus_next, format=Mus_out_format, edpos=false)  \
Writes sound data as (multichannel) file (for external program).")
  def sound_to_temp(type = Mus_next, format = Mus_out_format, edpos = false)
    cursnd = selected_sound
    cursync = sync(cursnd)
    if cursync.zero? or syncd_sounds(cursync) == 1
      data = [snd_tempnam]
      save_sound_as(data.first, selected_sound,
                    :header_type, type,
                    :sample_type, format,
                    :edit_position, edpos)
      data
    else
      Snd.raise(:snd_error,
                "re-implemented sound-to-temp doesn't \
handle sync bit correctly yet.")
    end
  end

  add_help(:selection_to_temps,
           "selection_to_temps(type=Mus_next, format=Mus_out_format)  \
Writes selection data as mono files (for external program).")
  def selection_to_temps(type = Mus_next, format = Mus_out_format)
    data = make_array(selection_chans) do |chn|
      outname = snd_tempnam
      save_selection(outname, 44100, format, type, :channel, chn)
      outname
    end
    data
  end

  add_help(:sound_to_temps,
           "sound_to_temps(type=Mus_next, format=Mus_out_format, edpos=false)  \
Writes sound data as mono files (for external program).")
  def sound_to_temps(type = Mus_next, format = Mus_out_format, edpos = false)
    cursnd = selected_sound
    cursync = sync(cursnd)
    if cursync.zero? or syncd_sounds(cursync) == 1
      make_array(channels(cursnd)) do |chn|
        outname = snd_tempnam
        save_sound_as(outname, selected_sound,
                      :header_type, type,
                      :sample_type, format,
                      :channel, chn,
                      :edit_position, edpos)
        outname
      end
    else
      Snd.raise(:snd_error,
                "re-implemented sound-to-temps doesn't \
handle sync bit correctly yet.")
    end
  end

  # filenames: String or Array of Strings
  def delete_temp_data_files(data, filenames)
    case filenames
    when String
      (data or []).each do |temp_file|
        if File.exist?(temp_file) and temp_file != filenames
          File.unlink(temp_file)
        end
      end
    when Array
      (data or []).each_with_index do |temp_file, i|
        if File.exist?(temp_file) and temp_file != filenames[i]
          File.unlink(temp_file)
        end
      end
    end
  end
  
  add_help(:temp_to_sound,
           "temp_to_sound(data, filename, origin=false)  \
Reads (multichannel) file as new sound data (from external program).")
  def temp_to_sound(data, filename, origin = false)
    cursnd = selected_sound
    delete_temp_data_files(data, filename)
    channels(cursnd).times do |chn|
      set_samples(0, mus_sound_framples(filename), filename,
                  cursnd, chn, true, origin, chn)
    end
  end

  add_help(:temps_to_sound,
           "temps_to_sound(data, filenames, origin=false)  \
Reads mono files as new sound data (from external program).")
  def temps_to_sound(data, filenames, origin = false)
    cursnd = selected_sound
    delete_temp_data_files(data, filenames)
    channels(cursnd).times do |chn|
      len = mus_sound_framples(filenames[chn])
      set_samples(0, len, filenames[chn], cursnd, chn, true, origin, chn)
    end
  end

  add_help(:temp_to_selection,
           "temp_to_selection(data, filename, origin=false)  \
Sets selection from (multichannel) file (from external program).")
  def temp_to_selection(data, filename, origin = false)
    delete_temp_data_files(data, filename)
    chan = 0
    len = mus_sound_framples(filename)
    Snd.sounds.each do |snd|
      channels(snd).times do |chn|
        if selection_member?(snd, chn)
          set_samples(selection_position(snd, chn), len, filename,
                      snd, chn, true, origin, chan)
          chan += 1
        end
      end
    end
  end

  add_help(:temps_to_selection,
           "temps_to_selection(data, filenames, origin=false)  \
Sets selection from mono files (from external program).")
  def temps_to_selection(data, filenames, origin = false)
    delete_temp_data_files(data, filenames)
    chan = 0
    Snd.sounds.each do |snd|
      channels(snd).times do |chn|
        if selection_member?(snd, chn)
          len = mus_sound_framples(filenames[chan])
          set_samples(selection_position(snd, chn), len, filenames[chan],
                      snd, chn, true, origin)
          chan += 1
        end
      end
    end
  end
  # 
  # Snd-5 compatibility stuff
  # 
  def forward_sample(count = 1, snd = false, chn = false)
    set_cursor(cursor(snd, chn) + count, snd, chn)
  end
  
  def backward_sample(count = 1, snd = false, chn = false)
    set_cursor(cursor(snd, chn) - count, snd, chn)
  end

  # 
  # Snd-7 compatibility stuff
  # 
  def back_or_forth_graph(count)
    if sounds
      cursnd = selected_sound or Snd.sounds.first
      curchn = selected_channel or 0
      curpos = 0
      pos = 0
      sndlst = []
      chnlst = []
      Snd.sounds.each do |snd|
        channels(snd).times do |chn|
          if cursnd.eql?(snd) and curchn == chn
            curpos = pos
          end
          pos += 1
          sndlst.push(snd)
          chnlst.push(chn)
        end
      end
      newpos = (curpos + count) % sndlst.length
      set_selected_sound(sndlst[newpos])
      set_selected_channel(chnlst[newpos])
      [selected_sound, selected_channel]
    end
  end
  
  def forward_graph(count = 1)
    back_or_forth_graph(count)
  end
  
  def backward_graph(count = 1)
    back_or_forth_graph(-count)
  end

  def back_or_forth_mix(count, snd, chn)
    if count.nonzero? and (mx = mixes(snd, chn))
      if mx.length == 1
        set_cursor(mix_position(mx.first), snd, chn)
        mx.first
      else
        sorted_mx = mx.sort do |a, b|
          mix_position(a) <=> mix_position(b)
        end
        pos = cursor(snd, chn)
        curpos = count > 0 ? -1 : 0
        if pos >= mix_position(sorted_mx.first)
          sorted_mx.each do |m|
            if (count > 0 and 
               pos < mix_position(m)) or 
               (count < 0 and pos <= mix_position(m))
              break
            else
              curpos += 1
            end
          end
        end
        curmx = sorted_mx[(curpos + count) % mx.length]
        set_cursor(mix_position(curmx), snd, chn)
        curmx
      end
    else
      false
    end
  end
  
  def forward_mix(count = 1, snd = false, chn = false)
    back_or_forth_mix(count, Snd.snd(snd), Snd.chn(chn))
  end

  def backward_mix(count = 1, snd = false, chn = false)
    back_or_forth_mix(-count, Snd.snd(snd), Snd.chn(chn))
  end

  def back_or_forth_mark(count, snd, chn)
    mk = Snd.marks(snd, chn)
    if count.nonzero? and (not mk.empty?)
      if mk.length == 1
        set_cursor(mark_sample(mk.first), snd, chn)
        mk.first
      else
        sorted_mk = mk.sort do |a, b|
          mark_sample(a) <=> mark_sample(b)
        end
        pos = cursor(snd, chn)
        curpos = count > 0 ? -1 : 0
        if pos >= mark_sample(sorted_mk.first)
          sorted_mk.each do |m|
            if (count > 0 and
               pos < mark_sample(m)) or
               (count < 0 and pos <= mark_sample(m))
              break
            else
              curpos += 1
            end
          end
        end
        curmk = sorted_mk[(curpos + count) % mk.length]
        set_cursor(mark_sample(curmk), snd, chn)
        curmk
      end
    else
      false
    end
  end

  def forward_mark(count = 1, snd = false, chn = false)
    back_or_forth_mark(count, Snd.snd(snd), Snd.chn(chn))
  end

  def backward_mark(count = 1, snd = false, chn = false)
    back_or_forth_mark(-count, Snd.snd(snd), Snd.chn(chn))
  end

  def vct_convolve!(r1, r2)
    convolution(r1, r2, r1.length)
  end

  def old_map_channel(beg = 0, dur = false,
                      snd = false, chn = false,
                      edpos = false, edname = false, &func)
    map_channel(lambda do |y|
                  if array?(val = func.call(y))
                    vector2vct(val)
                  else
                    val
                  end
                end, beg, dur, snd, chn, edpos, edname)
  end
  
  def mus_bank(gens, amps, in1 = false, in2 = false)
    amps = case amps
           when Array
             amps.to_vct
           when Vct
             amps
           else
             Vct.new(gens.length, 1.0)
           end
    inp1 = case in1
           when Array
             in1.to_vct
           when Vct
             in1
           else
             Vct.new(gens.length, 0.0)
           end
    inp2 = case in2
           when Array
             in2.to_vct
           when Vct
             in2
           else
             Vct.new(gens.length, 0.0)
           end
    sum = 0.0
    gens.each_with_index do |gen, i|
      sum = sum + amps[i] * mus_run(gen, inp1[i], inp2[i])
    end
    sum
  end

  def old_oscil_bank(amps, gens, in1 = false, in2 = false)
    amps = case amps
           when Array
             amps.to_vct
           when Vct
             amps
           else
             Vct.new(gens.length, 1.0)
           end
    inp1 = case in1
           when Array
             in1.to_vct
           when Vct
             in1
           else
             Vct.new(gens.length, 0.0)
           end
    inp2 = case in2
           when Array
             in2.to_vct
           when Vct
             in2
           else
             Vct.new(gens.length, 0.0)
           end
    sum = 0.0
    gens.each_with_index do |gen, i|
      sum = sum + amps[i] * oscil(gen, inp1[i], inp2[i])
    end
    sum
  end

  def old_formant_bank(amps, gens, in1 = 0.0)
    formant_bank(array?(amps) ? amps.to_vct : amps, gens, in1)
  end
  
  def vct2samples(samp, samps, data, snd = false, chn = false)
    vct2channel(data, samp, samps, snd, chn)
  end
  
  def samples2vct(samp, samps, snd = false, chn = false,
                  nv = false, epos = false)
    if nv
      vct_subseq(channel2vct(samp, samps, snd, chn, epos), 0, samps, nv)
    else
      channel2vct(samp, samps, snd, chn, epos)
    end
  end

  def scale_sound_by(scl, beg = false, dur = false,
                     snd = false, chn = false, edpos = false)
    if integer?(chn)
      scale_channel(scl, beg, dur, snd, chn, edpos)
    else
      channels(snd).times do |c|
        scale_channel(scl, beg, dur, snd, c)
      end
    end
  end

  def scale_sound_to(norm, beg = false, dur = false, snd = false, chn = false)
    if integer?(chn)
      mx = maxamp(snd, chn)
      if mx.nonzero? and mx != norm
        scale_channel(norm / mx, beg, dur, snd, chn)
      end
    else
      mx = maxamp(snd, true).max
      if mx.nonzero? and mx != norm
        channels(snd).times do |c|
          scale_channel(norm / mx, beg, dur, snd, c)
        end
      end
    end
  end
  Emacs_hookname = "after-save-as-hook-replace-sound"
  def emacs_style_save_as
    $after_save_as_hook.member?(Emacs_hookname)
  end

  def set_emacs_style_save_as(val = true)
    if val and (not emacs_style_save_as())
      $after_save_as_hook.add_hook!(Emacs_hookname) do |snd, fname, from_dialog|
        if from_dialog
          revert_sound(snd)
          close_sound(snd)
          open_sound(fname)
        end
      end
    else
      $after_save_as_hook.remove_hook!(Emacs_hookname)
    end
  end
  
  alias mus_error_to_string mus_error_type2string

  def make_iir_low_pass_1(fc)
    fc = fc.to_f
    theta = (2 * PI * fc) / mus_srate()
    gamma = cos(theta) / (1.0 + sin(theta))
    xc = (1.0 - gamma) / 2.0
    make_filter(2, vct(xc, xc), vct(0.0, -gamma))
  end

  def make_iir_high_pass_1(fc)
    fc = fc.to_f
    theta = (2 * PI * fc) / mus_srate()
    gamma = cos(theta) / (1.0 + sin(theta))
    xc = (1.0 + gamma) / 2.0
    make_filter(2, vct(xc, -xc), vct(0.0, -gamma))
  end

  # 
  # Snd-9 compatibility stuff
  # 
  def sine_bank(amps, phases, size = nil)
    len = (size or amps.length)
    sum = 0.0
    len.times do |i|
      sum = sum + amps[i] * sin(phases[i])
    end
    sum
  end
end

include Compatibility

module Extensions
  add_help(:remove_channel_property,
           "remove_channel_property(key, snd)  \
Deletes key-value pair.")
  def remove_channel_property(key, snd = false, chn = false)
    array?(props = channel_properties(snd, chn)) and
      array?(item = props.assoc(key)) and
      props.delete(item)
  end

  add_help(:remove_sound_property,
           "remove_sound_property(key, snd)  \
Deletes key-value pair.")
  def remove_sound_property(key, snd = false)
    array?(props = sound_properties(snd)) and
      array?(item = props.assoc(key)) and
      props.delete(item)
  end
  
  def channel_sync(snd = false, chn = false)
    channel_property(:sync, snd, chn)
  end
  
  def set_channel_sync(val, snd = false, chn = false)
    set_channel_property(:sync, val, snd, chn)
  end

  def set_sound_property_save_state_ignore(key, snd = false)
    set_sound_property(:save_state_ignore,
                       (sound_property(:save_state_ignore, snd) or
                          [:save_state_ignore]) + [key],
                       snd)
  end

  def set_channel_property_save_state_ignore(key, snd = false, chn = false)
    set_channel_property(:save_state_ignore,
                         (channel_property(:save_state_ignore, snd, chn) or
                            [:save_state_ignore]) + [key],
                         snd, chn)
  end
  
  def show_sound_properties(snd = false)
    if props = sound_properties(snd)
      Snd.display("sound-properties of %s", file_name(snd).inspect)
      props.each do |k, v|
        Snd.display("%s --> %s", k, v.inspect)
      end
    else
      Snd.display("%s has no sound-properties", file_name(snd).inspect)
    end
    nil
  end

  def show_channel_properties(snd = false, chn = false)
    Snd.display("channel-properties of %s", file_name(snd).inspect)
    if integer?(chn)
      if props = channel_properties(snd, chn)
        Snd.display("==> channel %d <==", chn)
        props.each do |k, v|
          Snd.display("%s --> %s", k, v.inspect)
        end
      else
        Snd.display("==> channel %d has no channel-properties <==", chn)
      end
    else
      channels(snd).times do |cn|
        if props = channel_properties(snd, cn)
          Snd.display("==> channel %d <==", cn)
          props.each do |k, v|
            Snd.display("%s --> %s", k, v.inspect)
          end
        else
          Snd.display("==> channel %d has no channel-properties <==", cn)
        end
      end
    end
    nil
  end

  # Used in remember_all_sound_properties and remember_sound_state
  $sound_funcs = [:sync, :with_tracking_cursor, :selected_channel,
    :show_controls, :read_only, :contrast_control?, :expand_control?,
    :reverb_control?, :filter_control?, :amp_control, :amp_control_bounds,
    :contrast_control, :contrast_control_amp, :contrast_control_bounds,
    :expand_control, :expand_control_bounds, :expand_control_hop,
    :expand_control_jitter, :expand_control_length, :expand_control_ramp,
    :filter_control_envelope, :filter_control_in_dB, :filter_control_in_hz,
    :filter_control_order, :reverb_control_decay, :reverb_control_feedback,
    :reverb_control_length, :reverb_control_length_bounds,
    :reverb_control_lowpass, :reverb_control_scale,
    :reverb_control_scale_bounds, :speed_control, :speed_control_bounds,
    :speed_control_style, :speed_control_tones]
  # transform_type: #<transform Xxx> transform2integer -->
  #   save/load --> integer2transform
  $channel_funcs = [:time_graph?, :transform_graph?, :lisp_graph?, :x_bounds,
    :y_bounds, :cursor, :cursor_size, :cursor_style, :show_marks, :show_y_zero,
    :show_grid, :wavo_hop, :wavo_trace, :max_transform_peaks,
    :show_transform_peaks, :fft_log_frequency, :fft_log_magnitude,
    :with_verbose_cursor, :zero_pad, :wavelet_type, :min_dB, :transform_size,
    :transform_graph_type, :time_graph_type, :fft_window, :transform_type,
    :transform_normalization, :time_graph_style, :show_mix_waveforms,
    :dot_size, :x_axis_style, :show_axes, :graphs_horizontal,
    :lisp_graph_style, :transform_graph_style, :grid_density,
    :tracking_cursor_style]
  
  # [ms] Based on the idea of remember_sound_state() below, here is
  # another approach, using a database file.
  #
  # remember_all_sound_properties() in ~/.snd-ruby.rb installs the hooks
  # default database is ~/.snd-properties.db
  # default tmp_snd_p is false
  #   (discarding `.*0000_00.snd' and `.*.rev.*' filenames)

  def remember_all_sound_properties(database = ENV['HOME'] + "/.snd-properties",
                                    tmp_snd_p = false)
    rsp = Remember_sound_properties.new(database)
    unless $after_open_hook.member?("save-property-hook")
      $after_open_hook.add_hook!("save-property-hook") do |snd|
        rsp.load(snd)
      end
      $close_hook.add_hook!("save-property-hook") do |snd|
        if tmp_snd_p or (not file_name(snd) =~ /(.+\d+_\d+\.snd$)|(.*\.rev.*$)/)
          rsp.save(snd)
        end
      end
    end
    set_property(:remember_properties, :object, rsp)
    rsp
  end

  # Returns the local object created by remember_all_sound_properties
  # to manipulate the database with methods mentioned below; it saves
  # a global variable.
  def remember_properties
    property(:remember_properties, :object)
  end

  class Remember_sound_properties
    add_help(:remember_sound_properties, "\
# class Remember_sound_properties
#   initialize(database)
#
# getter:
#   database
#
# methods:
#   inspect
#   with_db do ... end
#   load(snd)
#   save(snd)
#   each do |file, value| ... end
#   delete_if do |file, value| ... end
#   contents
#   help           (alias info and description)
#
# Usage:
#
# rsp = Remember_sound_properties.new(database)
# unless $after_open_hook.member?(\"save-property-hook\")
#   $after_open_hook.add_hook!(\"save-property-hook\") do |snd|
#     rsp.load(snd)
#   end
#   $close_hook.add_hook!(\"save-property-hook\") do |snd|
#     # discarding `.*0000_00.snd' and `.*.rev.*' filenames
#     if tmp_snd_p or
#        (not file_name(snd) =~ /(.+\\d+_\\d+\\.snd$)|(.*\\.rev.*$)/)
#       rsp.save(snd)
#     end
#   end
# end
#
# rsp.delete_if do |file, value|
#   file =~ /test/
# end                        # deletes all files containing the string \"test\"
# rsp.contents               # prints all filenames in database
# rsp.each do |file, value|
#   Snd.display(file)
# end                        # the same as rsp.contents")

    include Enumerable
    include Info
    with_silence do
      unless defined? DBM.open
        require "dbm"
      end
    end
    
    def initialize(database)
      @database      = database
      @sound_funcs   = $sound_funcs
      @channel_funcs = $channel_funcs
      set_help
    end
    attr_reader :database
    alias help description
    
    def inspect
      format("#<%s: database: %s>", self.class, @database.inspect)
    end

    def with_db
      db = DBM.open(@database)
      ret = yield(db)
      db.close
      ret
    end
    
    def load(snd)
      snd_name = file_name(snd)
      with_db do |db|
        Snd.catch do
          eval((db[snd_name] or ""))
        end
      end
      if file_write_date(snd_name) == sound_property(:current_file_time, snd)
        @sound_funcs.each do |prop|
          if (val = sound_property(prop, snd))
            if prop == :selected_channel # arguments swapped!
              Kernel.set_snd_func(prop, snd, val)
            else
              Kernel.set_snd_func(prop, val, snd)
            end
          end
        end
        channels(snd).times do |chn|
          set_squelch_update(true, snd, chn)
          @channel_funcs.each do |prop|
            if (val = channel_property(prop, snd, chn))
              if prop == :transform_type
                Kernel.set_snd_func(prop, integer2transform(val), snd, chn)
              else
                Kernel.set_snd_func(prop, val, snd, chn)
              end
            end
          end
          set_squelch_update(false, snd, chn)
        end
      end
    end
    
    # the resulting string looks like
    # let(find_sound("snd_name")) do |snd|
    #   set_sound_properties([...], snd)
    #   if channels(snd) > 0
    #     set_channel_properties([...], snd, 0)
    #   end
    #   if channels(snd) > 1
    #     set_channel_properties([...], snd, 1)
    #   end
    #   ...
    # end
    # self.load evals this string
    def save(snd)
      snd_name = file_name(snd)
      set_sound_property(:current_file_time, file_write_date(snd_name), snd)
      @sound_funcs.each do |prop|
        set_sound_property(prop, snd_func(prop, snd), snd)
      end
      props = (sound_properties(snd) or [])
      if reject = sound_property(:save_state_ignore, snd)
        props.reject! do |k, v|
          reject.member?(k)
        end
      end
      res = format("let(find_sound(%s)) do |snd_s|\n", snd_name.inspect)
      res += format("  set_sound_properties(%s, snd_s)\n", props.inspect)
      channels(snd).times do |chn|
        @channel_funcs.each do |prop|
          if prop == :transform_type
            set_channel_property(prop,
                                 transform2integer(snd_func(prop, snd, chn)),
                                 snd, chn)
          else
            set_channel_property(prop, snd_func(prop, snd, chn), snd, chn)
          end
        end
        props = (channel_properties(snd, chn) or [])
        if reject = channel_property(:save_state_ignore, snd, chn)
          props.reject! do |k, v|
            reject.member?(k)
          end
        end
        res += format("  if channels(snd_s) > %d\n", chn)
        res += format("    set_channel_properties(%s, snd_s, %d)\n",
                      props.inspect, chn)
        res += "  end\n"
      end
      res += "end\n"
      res += marks2string(snd)
      with_db do |db|
        db[snd_name] = res
      end
    end

    def each
      with_db do |db|
        db.each do |key, value|
          yield(key, value)
        end
      end
    end
    
   def delete_if(&body)
     with_db do |db|
       db.delete_if(&body)
     end
   end

    def contents
      Snd.display("contents of %s", @database.inspect)
      with_db do |db|
        db.each do |file, value|
          Snd.display(file)
        end
      end
      nil
    end
  
    private
    def set_help
      self.description = get_help(:remember_sound_properties)
    end
  end

  # add channel comments (see extsnd.html->Graphics->draw-string)
  def add_comment(samp, comm, snd = selected_sound(), chn = selected_channel())
    if array?(comments = channel_property(:comments, snd, chn))
      comments.push([samp, comm])
    else
      set_channel_property(:comments, [[samp, comm]], snd, chn)
    end
  end

  def remove_comment(comm, snd = selected_sound(), chn = selected_channel())
    (channel_property(:comments, snd, chn) or []).delete_if do |samp, text|
      text == comm
    end
  end

  def show_comments(snd, chn)
    (channel_property(:comments, snd, chn) or []).each do |samp, text|
      width = 6 * text.length
      if samp.between?(left_sample(snd, chn), right_sample(snd, chn))
        xpos = x2position(samp / srate(snd).to_f, snd, chn)
        ypos = y2position(sample(samp), snd, chn)
        cr = channel_widgets(snd, chn)[17]
        draw_line(xpos, 35, xpos, ypos - 4, snd, chn, Copy_context, cr)
        draw_string(text, xpos - width / 2, 18, snd, chn, Copy_context, cr)
      end
    end
  end
  # $after_graph_hook.add_hook!("show-comments") do |snd, chn|
  #   show_comments(snd, chn)
  # end
  
  # Have we marks?  Similar to selections?().
  def marks?(more_than_one = true)
    if (m = marks(selected_sound(), selected_channel())).nil?
      false
    else
      if more_than_one
        m.length > 1
      else
        true
      end
    end
  end
  
  add_help(:all_chans,
           "all_chans()  \
Returns two parallel lists, the first snd indices, \
the second channel numbers.  \
If we have two sounds open (indices 0 and 1 for example), \
and the second has two channels, \
all_chans returns [[0, 1, 1], [0, 0, 1]].")
  def all_chans
    sndlist = []
    chnlist = []
    Snd.sounds.each do |snd|
      channels(snd).times do |chn|
        sndlist << snd
        chnlist << chn
      end
    end
    [sndlist, chnlist]
  end

  # all_chans:        [[0, 1, 1], [0, 0, 1]]
  # all_chans_zipped: [[0, 0], [1, 0], [1, 1]]
  # fits with each or map
  # all_chans_zipped.each do |snd, chn| ... end
  def all_chans_zipped
    new_list = []
    Snd.sounds.each do |snd|
      channels(snd).times do |chn|
        new_list.push([snd, chn])
      end
    end
    new_list
  end
  
  add_help(:normalized_mix,
           "normalized_mix(filename, beg, in_chan, snd, chn)  \
Is like mix but the mix result has same peak amp \
as unmixed snd/chn (returns scaler).")
  def normalized_mix(fname, beg = 0, in_chan = 0, snd = false, chn = false)
    orig_maxamp = maxamp(snd, chn)
    mix(fname, beg, in_chan, snd, chn)
    new_maxamp = maxamp(snd, chn)
    if orig_maxamp == new_maxamp
      1.0
    else
      scl = orig_maxamp / new_maxamp
      old_sync = sync(snd)
      set_sync(false, snd)
      scale_by(scl, snd, chn)
      set_sync(old_sync, snd)
      scl
    end
  end

  add_help(:enveloped_mix,
           "enveloped_mix(filename, beg, env)  \
Mixes FILENAME starting at BEG with amplitude envelope ENV: \
enveloped_mix(\"pistol.snd\", 0, [0, 0, 1, 1, 2, 0])")
  def enveloped_mix(fname, beg, env, del_tmp = true)
    len = framples(fname)
    amp_env = make_env(env, :length, len)
    rd = make_readin(fname)
    map_channel(lambda do |y|
                  y + env(amp_env) * readin(rd)
                end, beg, len)
  end
  # enveloped_mix("pistol.snd", 0, [0, 0, 1, 1, 2, 0])

  add_help(:for_each_sound_file,
           "for_each_sound_file(*dir_args) do |file| ... end  \
Applies body to each sound file in dir.")
  add_help(:map_sound_files,
           "map_sound_files(*dir_array) do |file| ... end  \
Applies body to each sound file in dir.")
  add_help(:each_sound_file,
           "each_sound_file(*dir_args) do |file| ... end  \
Applies body to each sound file in dir.")
  def each_sound_file(*args, &func)
    if args.empty?
      args.push(Dir.pwd)
    end
    args.map do |dir|
      if File.exist?(dir)
        sound_files_in_directory(dir).each do |f|
          func.call(dir + "/" + f)
        end
      end
    end
  end
  alias for_each_sound_file each_sound_file
  alias map_sound_files each_sound_file
  # each_sound_file(".", "/usr/opt/sound/SFiles") do |f|
  #   play(f, :wait, true)
  # end

  add_help(:match_sound_files,
           "match_sound_files(*dir_args) do |file| ... end  \
Applies func to each sound file in dir and returns a list of files \
for which body does not return false.")
  def match_sound_files(*args, &func)
    res = []
    each_sound_file(*args) do |f|
      func.call(f) and res.push(f)
    end
    res
  end
  # match_sound_files() do |f| f =~ /\.(wav|snd)$/ end
  
  add_help(:selection_members,
           "selection_members()  \
Returns list of lists of [snd, chn] indicating the channels \
participating in the current selection.")
  def selection_members
    sndlist = []
    if selection?
      Snd.sounds.each do |snd|
        channels(snd).times do |chn|
          sndlist.push([snd, chn]) if selection_member?(snd, chn)
        end
      end
    end
    sndlist
  end

  add_help(:clear_selection,
           "clear_selection()  \
Unselects any currently selected samples.")
  def clear_selection()
    if selection?
      Snd.sounds.each do |snd|
        channels(snd).times do |chn|
          need_update = selection_member?(snd, chn)
          set_selection_member?(false, snd, chn)
          if need_update
            update_time_graph(snd, chn)
          end
        end
      end
    end
  end
  
  add_help(:make_selection,
           "make_selection(beg=0, end=false, snd=false, chn=false)  \
Makes a selection like make-region but without creating a region.  \
make_selection follows SND's sync field, \
and applies to all SND's channels if CHN is not specified.  \
END defaults to end of channel, BEG defaults to 0, \
SND defaults to the currently selected sound.")
  def make_selection(beg = 0, len = false, snd = false, chn = false)
    add_chan_to_selection = lambda do |s0, s1, s, c|
      unless s0 
        s0 = 0
      end
      set_selection_member?(true, s, c)
      set_selection_position(s0, s, c)
      val = if number?(s1)
              s1 + 1
            else
              framples(s, c)
            end
      set_selection_framples(val - s0, s, c)
    end
    current_sound = (snd or selected_sound() or Snd.sounds[0])
    unless sound?(current_sound)
      Snd.raise(:no_such_sound, beg, len, snd, chn)
    end
    current_sync = sync(current_sound)
    clear_selection()
    if number?(chn)
      add_chan_to_selection.call(beg, len, snd, chn)
    else
      Snd.sounds.each do |s|
        if snd == true or
            s.eql?(current_sound) or
            (current_sync.nonzero? and current_sync == sync(s))
          channels(s).times do |i|
            add_chan_to_selection.call(beg, len, s, i)
          end
        end
      end
    end
  end
  
  add_help(:mix_channel,
           "mix_channel(file, beg=0, dur=false, \
snd=false, chn=false, edpos=false)  \
Mixes in FILE.  \
FILE can be the file name or a list [file_name, beg = 0, chn = 0].")
  def mix_channel(fdata, beg = 0, dur = false,
                  snd = false, chn = false, edpos = false)
    assert_type((string?(fdata) or array?(fdata)), fdata, 0,
                "a string or an array")
    unless number?(beg) 
      beg = 0
    end
    fname = string?(fdata) ? fdata : fdata.first
    fbeg = if string?(fdata) or fdata.length < 2
             0
           else
             fdata[1]
           end
    fchan = if string?(fdata) or fdata.length < 3
              0
            else
              fdata[2]
            end
    len = (dur or (mus_sound_framples(fname) - fbeg))
    Snd.raise(:no_such_sample) if beg < 0
    if len > 0
      reader = make_sampler(fbeg, fname, fchan)
      map_channel(lambda do |val|
                    val + next_sample(reader)
                  end, beg, len, snd, chn, edpos,
                  format("%s(%p, %s, %s", get_func_name, fdata, beg, dur))
    end
  end
  
  add_help(:insert_channel,
           "insert_channel(file, beg=0, dur=false, \
snd=false, chn=false, edpos=false)  \
Inserts the FILE.  \
FILE can be the file name or a list [file_name, beg = 0, chn = 0].")
  def insert_channel(fdata, beg = 0, dur = false,
                     snd = false, chn = false, edpos = false)
    assert_type((string?(fdata) or array?(fdata)), fdata, 0,
                "a string or an array")
    unless number?(beg)
      beg = 0
    end
    fname = string?(fdata) ? fdata : fdata.first
    fbeg = if string?(fdata) or fdata.length < 2
             0
           else
             fdata[1]
           end
    fchan = if string?(fdata) or fdata.length < 3
              0
            else
              fdata[2]
            end
    len = (dur or (mus_sound_framples(fname) - fbeg))
    Snd.raise(:no_such_sample) if beg < 0
    if len > 0
      reader = make_sampler(fbeg, fname, fchan)
      insert_samples(beg, len,
                     Vct.new(len) do |i|
                       next_sample(reader)
                     end, snd, chn, edpos, false,
                     format("%s(%p, %s, %s", get_func_name, fdata, beg, dur))
    end
  end
  
  add_help(:redo_channel,
           "redo_channel(edits=1, snd=false, chn=false)  \
Is the regularized version of redo.")
  def redo_channel(edits = 1, snd = false, chn = false)
    if snd and sync(snd).nonzero? and chn
      set_edit_position(edit_position(snd, chn) + edits, snd, chn)
    else
      redo_edit(edits, snd)
    end
  end
  
  add_help(:undo_channel,
           "undo_channel(edits=1, snd=false, chn=false)  \
Is the regularized version of undo.")
  def undo_channel(edits = 1, snd = false, chn = false)
    if snd and sync(snd).nonzero? and chn
      set_edit_position([edit_position(snd, chn) - edits, 0].max, snd, chn)
    else
      undo_edit(edits, snd)
    end
  end
  
  # Take breakpoints in ENV, connect with FUNC, apply as envelope to
  # channel handled as a sequence of funcs and scales.
  def any_env_channel(env, beg = 0, dur = false, snd = false, chn = false,
                      edpos = false, origin = false, &func)
    if array?(env) and (not env.empty?)
      pts = env.length / 2
      if pts == 1
        scale_channel(env[0], beg, dur, snd, chn, edpos)
      else
        x0 = y0 = 0
        x1, y1 = env[0], env[1]
        xrange = (env[-2] - env[0]).to_f
        ramp_beg = beg
        unless number?(dur)
          dur = framples(snd, chn)
        end
        as_one_edit_rb(origin) do | |
          2.step(env.length - 1, 2) do |i|
            x0, y0, x1, y1 = x1, y1, env[i], env[i + 1]
            ramp_dur = (dur * ((x1 - x0) / xrange)).round
            if y0 == y1
              scale_channel(y0, ramp_beg, ramp_dur, snd, chn, edpos)
            else
              func.call(y0, y1, ramp_beg, ramp_dur, snd, chn, edpos)
            end
            ramp_beg += ramp_dur
          end
        end
      end
    end
  end
  
  add_help(:sine_ramp,
           "sine_ramp(rmp0, rmp1, beg=0, dur=false, \
snd=false, chn=false, edpos=false)  \
Produces a sinsusoidal connection from RMP0 to RMP1.")
  def sine_ramp(rmp0, rmp1, beg = 0, dur = false,
                snd = false, chn = false, edpos = false)
    angle = -PI
    len = if number?(dur)
            dur
          else
            framples(snd, chn) - beg
          end
    incr = PI / len
    map_channel(lambda do |y|
                  result = y * (rmp0 + (rmp1 - rmp0) * (0.5 + 0.5 * cos(angle)))
                  angle += incr
                  result
                end, beg, dur, snd, chn, edpos,
                format("%s(%s, %s, %s, %s",
                get_func_name, rmp0, rmp1, beg, dur))
  end

  add_help(:sine_env_channel,
           "sine_env_channel(env, beg=0, dur=false, \
snd=false, chn=false, edpos=false)  \
Connects ENV's dots with sinusoids.")
  def sine_env_channel(env, beg = 0, dur = false,
                       snd = false, chn = false, edpos = false)
    any_env_channel(env, beg, dur, snd, chn, edpos,
                    format("%s(%p, %s, %s",
                           get_func_name,
                           env, beg, dur)) do |r0, r1, b, d, s, c, e|
      sine_ramp(r0, r1, b, d, s, c, e)
    end
  end
  # sine_env_channel([0, 0, 1, 1, 2, -0.5, 3, 1])
  
  # An obvious extension of this idea is to use the blackman fft window
  # formulas to get sharper sinusoids (i.e. use the sum of n cosines,
  # rather than just 1).
  
  def blackman4_ramp(rmp0, rmp1, beg = 0, dur = false,
                     snd = false, chn = false, edpos = false)
    angle = 0.0
    len = if number?(dur)
            dur
          else
            framples(snd, chn) - beg
          end
    incr = PI / len
    map_channel(lambda do |y|
                  cx = cos(angle)
                  result = y * (rmp0 + (rmp1 - rmp0) *
                            (0.084037 + cx *
                              (-0.29145 + cx *
                                (0.375696 + cx *
                                  (-0.20762 + cx * 0.041194)))))
                  angle += incr
                  result
                end, beg, dur, snd, chn, edpos,
                format("%s(%s, %s, %s, %s",
                       get_func_name, rmp0, rmp1, beg, dur))
  end
  
  def blackman4_env_channel(env, beg = 0, dur = false,
                            snd = false, chn = false, edpos = false)
    any_env_channel(env, beg, dur, snd, chn, edpos,
                    format("%s(%p, %s, %s",
                           get_func_name, env,
                           beg, dur)) do |r0, r1, b, d, s, c, e|
      blackman4_ramp(r0, r1, b, d, s, c, e)
    end
  end
  
  add_help(:ramp_squared,
           "ramp_squared(rmp0, rmp1, symmetric=true, beg=0, dur=false, \
snd=false, chn=false, edpos=false)  \
Connects RMP0 and RMP1 with an x^2 curve.")
  def ramp_squared(rmp0, rmp1, symmetric = true, beg = 0, dur = false,
                   snd = false, chn = false, edpos = false)
    angle = 0.0
    len = if number?(dur)
            dur
          else
            framples(snd, chn) - beg
          end
    incr = 1.0 / len
    map_channel(lambda do |y|
                  result = y * (rmp0 + angle * angle * (rmp1 - rmp0))
                  angle += incr
                  result
                end, beg, dur, snd, chn, edpos,
                format("%s(%s, %s, %s, %s, %s", get_func_name, rmp0, rmp1,
                       symmetric, beg, dur))
  end

  add_help(:env_squared_channel,
           "env_squared_channel(env, symmetric=true, beg=0, dur=false, \
snd=false, chn=false, edpos=false)  \
Connects ENV's dots with x^2 curves.")
  def env_squared_channel(env, symmetric = true, beg = 0, dur = false,
                          snd = false, chn = false, edpos = false)
    any_env_channel(env, beg, dur, snd, chn, edpos,
                    format("%s(%p, %s, %s, %s",
                           get_func_name,
                           env, symmetric, beg, dur)) do |r0, r1, b, d, s, c, e|
      ramp_squared(r0, r1, symmetric, b, d, s, c, e)
    end
  end
  # env_squared_channel([0, 0, 1, 1, 2, -0.5, 3, 1])
  
  add_help(:ramp_expt,
           "ramp_expt(rmp0, rmp1, exponent, symmetric=true, beg=0, dur=false, \
snd=false, chn=false, edpos=false)  \
Connects RMP0 and RMP1 with an x^exponent curve.")
  def ramp_expt(rmp0, rmp1, exponent, symmetric = true,
                beg = 0, dur = false, snd = false, chn = false, edpos = false)
    angle = 0.0
    len = if number?(dur)
            dur
          else
            framples(snd, chn) - beg
          end
    incr = 1.0 / len
    map_channel(lambda do |y|
                  begin
                    result = y *
                      (rmp0 + exp(log(angle) * exponent) * (rmp1 - rmp0))
                  rescue
                    result = 0.0
                  end
                  angle += incr
                  result
                end, beg, dur, snd, chn, edpos,
                format("%s(%s, %s, %s, %s, %s, %s",
                       get_func_name, rmp0, rmp1,
                       exponent, symmetric, beg, dur))
  end

  add_help(:env_expt_channel,
           "env_expt_channel(env, exponent, symmetric=true, beg=0, dur=false, \
snd=false, chn=false, edpos=false)  \
Connects ENV's dots with x^exponent curves.")
  def env_expt_channel(env, exponent, symmetric = true, beg = 0, dur = false,
                       snd = false, chn = false, edpos = false)
    if exponent == 1.0
      env_channel(env, beg, dur, snd, chn, edpos)
    else
      any_env_channel(env, beg, dur, snd, chn, edpos,
                      format("%s(%p, %s, %s, %s, %s",
                             get_func_name,
                             env, exponent, symmetric,
                             beg, dur)) do |r0, r1, b, d, s, c, e|
        ramp_expt(r0, r1, exponent, symmetric, b, d, s, c, e)
      end
    end
  end
  
  add_help(:offset_channel,
           "offset_channel(amount, beg=0, dur=false, \
snd=false, chn=false, edpos=false)  \
Adds AMOUNT to each sample.")
  def offset_channel(amount, beg = 0, dur = false,
                     snd = false, chn = false, edpos = false)
    map_channel(lambda do |y|
                  y + amount
                end, beg, dur, snd, chn, edpos,
                format("%s(%s, %s, %s", get_func_name, amount, beg, dur))
  end

  add_help(:offset_sound,
           "offset_sound(off, beg=0, dur=false, snd=false)  \
Adds OFF to every sample in SND.")
  def offset_sound(off, beg = 0, dur = false, snd = false)
    snd = Snd.snd(snd)
    if sound?(snd)
      channels(snd).times do |chn|
        offset_channel(off, beg, dur, snd, chn)
      end
    else
      Snd.raise(:no_such_sound, snd)
    end
  end

  add_help(:pad_sound,
           "pad_sound(beg, dur, snd=false)  \
Places a block of DUR zeros in every channel of SND starting at BEG.")
  def pad_sound(beg, dur, snd = false)
    snd = Snd.snd(snd)
    if sound?(snd)
      channels(snd).times do |chn|
        pad_channel(beg, dur, snd, chn)
      end
    else
      Snd.raise(:no_such_sound, snd)
    end
  end

  add_help(:dither_channel,
           "dither_channel(amount=0.00006, beg=0, dur=false, \
snd=false, chn=false, edpos=false)  \
Adds AMOUNT dither to each sample.")
  def dither_channel(amnt = 0.00006, beg = 0, dur = false,
                     snd = false, chn = false, edpos = false)
    dither = 0.5 * amnt
    map_channel(lambda do |y|
                  mus_random(dither) + mus_random(dither) + y
                end, beg, dur, snd, chn, edpos,
                format("%s(%s, %s, %s", get_func_name, amnt, beg, dur))
  end

  add_help(:dither_sound,
           "dither_sound(amount=0.00006, beg=0, dur=false, snd=false)  \
Adds dithering to every channel of SND.")
  def dither_sound(amnt = 0.00006, beg = 0, dur = false, snd = false)
    snd = Snd.snd(snd)
    if sound?(snd)
      channels(snd).times do |chn|
        dither_channel(amnt, beg, dur, snd, chn)
      end
    else
      Snd.raise(:no_such_sound, snd)
    end
  end

  add_help(:contrast_channel,
           "contrast_channel(index, beg=0, dur=false, \
snd=false, chn=false, edpos=false)  \
Applies contrast-enhancement to the sound.")
  def contrast_channel(index, beg = 0, dur = false,
                       snd = false, chn = false, edpos = false)
    map_channel(lambda do |y|
                  sin(y * HALF_PI + index * sin(y * TWO_PI))
                end, beg, dur, snd, chn, edpos,
                format("%s(%s, %s, %s", get_func_name, index, beg, dur))
  end

  add_help(:contrast_sound,
           "contrast_sound(index, beg=0, dur=false, snd=false)  \
Applies contrast-enhancement to every channel of SND.")
  def contrast_sound(index, beg = 0, dur = false, snd = false)
    snd = Snd.snd(snd)
    if sound?(snd)
      channels(snd).times do |chn|
        contrast_channel(index, beg, dur, snd, chn)
      end
    else
      Snd.raise(:no_such_sound, snd)
    end
  end

  # channels-equal
  add_help(:channels_eql?,
           "channels_eql?(s1, c1, s2, c2, diff=0.0)  \
Returns true if the two channels \
are the same (within diff) modulo trailing 0's.")
  def channels_eql?(snd1, chn1, snd2, chn2, allowable_difference = 0.0)
    if snd1.eql?(snd2) and chn1 == chn2
      true
    else
      mx1 = maxamp(snd1, chn1)
      mx2 = maxamp(snd2, chn2)
      if (mx1 - mx2).abs > allowable_difference
        false
      else
        len1 = framples(snd1, chn1)
        len2 = framples(snd2, chn2)
        first_longer = (len1 >= len2)
        len = first_longer ? len1 : len2
        s1 = first_longer ? snd1 : snd2
        s2 = first_longer ? snd2 : snd1
        c1 = first_longer ? chn1 : chn2
        c2 = first_longer ? chn2 : chn1
        read2 = make_sampler(0, s2, c2)
        (not scan_channel(lambda do |y|
                            val = read_sample(read2)
                            (val - y).abs > allowable_difference
                          end, 0, len, s1, c1))
      end
    end
  end

  add_help(:channels_equal?,
           "channels_equal?(s1, c1, s2, c2, diff=0.0)  \
Returns true if the two channels are the same (within diff).")
  def channels_equal?(snd1, chn1, snd2, chn2, allowable_difference = 0.0)
    len1 = framples(snd1, chn1)
    len2 = framples(snd2, chn2)
    if len1 == len2
      channels_eql?(snd1, chn1, snd2, chn2, allowable_difference)
    else
      false
    end
  end

  # mono->stereo, mono-files->stereo
  def mono2stereo(new_name, snd1, chn1, snd2, chn2)
    old_ed1 = edit_position(snd1, chn1)
    old_ed2 = edit_position(snd2, chn2)
    ind = new_sound(new_name, :channels, 2, :srate, srate(snd1))
    swap_channels(ind, 0, snd1, chn1)
    swap_channels(ind, 1, snd2, chn2)
    set_edit_position(old_ed1, snd1, chn1)
    set_edit_position(old_ed2, snd2, chn2)
    ind
  end
  # mono2stereo("test.snd", 0, 0, 1, 0)

  def mono_files2stereo(new_name, chan1_name, chan2_name)
    ind1 = open_sound(chan1_name)
    ind2 = open_sound(chan2_name)
    ind3 = mono2stereo(new_name, ind1, 0, ind2, 0)
    close_sound(ind1)
    close_sound(ind2)
    ind3
  end
  # mono_files2stereo("test.snd", "oboe.snd", "pistol.snd")

  def stereo2mono(orig_snd, chan1_name, chan2_name)
    old_ed0 = edit_position(orig_snd, 0)
    old_ed1 = edit_position(orig_snd, 1)
    chan1 = new_sound(chan1_name, :srate, srate(orig_snd))
    chan2 = new_sound(chan2_name, :srate, srate(orig_snd))
    swap_channels(orig_snd, 0, chan1, 0)
    swap_channels(orig_snd, 1, chan2, 0)
    set_edit_position(old_ed0, orig_snd, 0)
    set_edit_position(old_ed1, orig_snd, 1)
    [chan1, chan2]
  end
  # stereo2mono(0, "hi1.snd", "hi2.snd")

  # 
  # === PREFERENCES-DIALOG ===
  # 

  # reopen menu

  $including_reopen_menu = false # for prefs

  def with_reopen_menu
    unless $including_reopen_menu
      menu = Reopen_menu.new("Reopen")
      $including_reopen_menu = true
      $close_hook.add_hook!("prefs-add-to-reopen-menu") do |snd|
        menu.add_to_reopen_menu(snd)
      end
      $open_hook.add_hook!("prefs-check-reopen-menu") do |file|
        menu.check_reopen_menu(file)
      end
    end
  end

  class Reopen_menu
    def initialize(name)
      @menu_name = name
      @reopen_menu = add_to_main_menu(name, lambda do | | end)
      @reopen_names = []
      @reopen_max_length = 16
      @reopen_empty = "empty"
    end

    def add_to_reopen_menu(snd)
      brief_name = short_file_name(snd)
      long_name = file_name(snd)
      unless(@reopen_names.member?(brief_name))
        if @reopen_names.member?(@reopen_empty)
          remove_from_menu(@reopen_menu, @reopen_empty)
          @reopen_names = []
        end
        add_to_menu(@reopen_menu, brief_name,
                    lambda do | |
                      remove_from_menu(@reopen_menu, brief_name)
                      if File.exist?(long_name)
                        open_sound(long_name)
                      end
                    end, 0)
        @reopen_names.push(brief_name)
        if @reopen_names.length > @reopen_max_length
          remove_from_menu(@reopen_menu, @reopen_names.shift)
        end
      end
      false
    end

    def check_reopen_menu(file)
      brief_name = File.basename(file)
      if @reopen_names.member?(brief_name)
        remove_from_menu(@reopen_menu, brief_name)
        @reopen_names.delete(brief_name)
      end
      if @reopen_names.empty?
        add_to_menu(@reopen_menu, @reopen_empty, lambda do | | end, 0)
        @reopen_names.push(@reopen_empty)
      end
    end
  end

  # buffers menu

  $include_buffers_menu = false # for prefs

  def with_buffers_menu
    unless $include_buffers_menu
      menu = Buffers_menu.new("Buffers")
      $include_buffers_menu = true
      $open_hook.add_hook!("prefs-open-buffer") do |file|
        menu.open_buffer(file)
      end
      $close_hook.add_hook!("prefs-close-buffer") do |file|
        menu.close_buffer(file)
      end
    end
  end
  
  class Buffers_menu
    def initialize(name)
      @menu_name = name
      @buffer_menu = add_to_main_menu(name, lambda do | | end)
      @buffer_names = []
      @buffer_empty = "empty"
    end

    def open_buffer(file)
      if @buffer_names.member?(@buffer_empty)
        remove_from_menu(@buffer_menu, @buffer_empty)
        @buffer_names = []
      end
      add_to_menu(@buffer_menu, file,
                  lambda do | |
                    if sound?(ind = find_sound(file))
                      select_sound(ind)
                    end
                  end, -1)
      @buffer_names.push(file)
    end

    def close_buffer(snd)
      remove_from_menu(@buffer_menu, file_name(snd))
      @buffer_names.delete(file_name(snd))
      if @buffer_names.empty?
        add_to_menu(@buffer_menu, @buffer_empty, lambda do | | end, 0)
        @buffer_names.push(@buffer_empty)
      end
      false
    end
  end
end

include Extensions

# extensions.rb ends here
