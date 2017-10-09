# mix.rb -- mix.scm --> mix.rb

# Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 05/02/22 13:40:33
# Changed: 14/11/13 05:01:39

# various mix related functions
#
# module Mix (see mix.scm)
#  mix_sound(file, start)
#  silence_all_mixes
#  find_mix(sample, snd, chn)
#  mix2vct(id)
#  mix_maxamp(id)
#  snap_mix_to_beat(at_tag_position)
#
#  mix_click_sets_amp(id)
#  mix_click_info(id)
#  mix_name2id(name)
#
#  delete_mix(id)
#  scale_mixes(mix_list, scl)
#  silence_mixes(mix_list)
#  move_mixes(mix_list, samps)
#  src_mixes(mix_list, sr)
#  transpose_mixes(mix_list, semitones)
#  color_mixes(mix_list, col)
#  set_mixes_tag_y(mix_list, new_y)
#  mixes_maxamp(mix_list)
#  scale_tempo(mix_list, tempo_scl)
#  mixes_length(mix_list)

require "clm"

module Mix
  # 
  # === MIX ===
  #
  add_help(:mix_sound,
           "mix_sound(file, start)  \
Mixes file (all chans) at start in the currently selected sound.")
  def mix_sound(file, start)
    mix(file, start, true)
  end

  add_help(:delete_all_mixes,
           "delete_all_mixes()  \
Sets all mix amps to 0.")
  def silence_all_mixes
    as_one_edit_rb(get_func_name) do
      (mixes or []).flatten.each do |id|
        set_mix_amp(id, 0.0)
      end
		end
  end

  add_help(:find_mix,
           "find_mix(sample, snd=false, chn=false)  \
Returns the id of the mix at the given sample, or nil.")
  def find_mix(sample, snd = false, chn = false)
    (mixes(Snd.snd(snd), Snd.chn(chn)) or []).detect do |n|
      mix_position(n) == sample
    end
  end

  add_help(:mix2vct,
           "mix2vct(id)  \
Returns mix's data in vct.")
  def mix2vct(id)
    Snd.raise(:no_such_mix, id) unless mix?(id)
    len = mix_length(id)
    rd = make_mix_sampler(id)
    v = Vct.new(len) do |i|
      read_mix_sample(rd)
    end
    free_sampler(rd)
    v
  end

  add_help(:mix_maxamp,
           "mix_maxamp(id)  \
Returns the max amp in the given mix.")
  def mix_maxamp(id)
    Snd.raise(:no_such_mix, id) unless mix?(id)
    len = mix_length(id)
    rd = make_mix_sampler(id)
    peak = read_mix_sample(rd).abs
    (1...len).each do
      peak = [peak, read_mix_sample(rd).abs].max
    end
    free_sampler(rd)
    peak
  end

  add_help(:snap_mix_to_beat,
           "snap_mix_to_beat()  \
Forces a dragged mix to end up on a beat (see beats-per-minute).  \
Reset $mix_release_hook to cancel.")
  def snap_mix_to_beat
    $mix_release_hook.add_hook!(get_func_name) do |id, samps_moved|
      samp = samps_moved + mix_position(id)
      snd = mix_home(id)[0]
      chn = mix_home(id)[1]
      bps = beats_per_minute(snd, chn) / 60.0
      sr = srate(snd).to_f
      beat = ((samp * bps) / sr).floor
      lower = ((beat * sr) / bps).floor
      higher = (((beat + 1) * sr) / bps).floor
      set_mix_position(id, if (samp - lower) < (higher - samp)
                             [0, lower].max
                           else
                             higher
                           end)
      true
    end
  end

  #
  # === Mix Property ===
  #
  def mix_click_sets_amp(id)
    unless mix_property(:zero, id)
      set_mix_property(:amp, id, mix_amp(id))
      set_mix_amp(id, 0.0)
      set_mix_property(:zero, id, true)
    else
      set_mix_amp(id, mix_property(:amp, id))
      set_mix_property(:zero, id, false)
    end
    true
  end
  # $mix_click_hook.add_hook!("mix-click-sets-amp",
  #                           &method(:mix_click_sets_amp).to_proc)

  # 
  # === Mix Click Info ===
  # 
  add_help(:mix_click_info,
           "mix_click_info(n)  \
Is a $mix_click_hook function that describes a mix and its properties.")
  def mix_click_info(id)
    Snd.raise(:no_such_mix, id) unless mix?(id)
    mnamestr = ""
    mname = mix_name(id)
    if mname
      mnamestr = format("\n    mix name: %p", mname)
    end
    msr = srate(mix_home(id)[0]).to_f
    propstr = ""
    props = mix_properties(id)
    if props
      propstr = format("\n  properties: %p", props)
    end
    mpos = mix_position(id)
    mlen = mix_length(id)
    info_dialog("Mix Info",
                format("\
      mix id: %s%s
    position: %d (%1.3f secs)
      length: %d (%1.3f secs)
          in: %s[%d]
      scaler: %s
       speed: %s
         env: %s%s",
                       id, mnamestr,
                       mpos, mpos / msr,
                       mlen, mlen / msr,
                       short_file_name(mix_home(id)[0]), mix_home(id)[1],
                       mix_amp(id),
                       mix_speed(id),
                       mix_amp_env(id), propstr))
    true
  end
  # $mix_click_hook.add_hook!("mix-click-info",
  #                           &method(:mix_click_info).to_proc)

  add_help(:mix_name2id,
           "mix_name2id(name)  \
Returns the mix id associated with NAME.")
  def mix_name2id(name)
    ret = :no_such_mix
    Snd.sounds.each do |snd|
      channels(snd).times do |chn|
        mixes(snd, chn).each do |m|
          if mix_name(m) == name
            ret = m
            break
          end
        end
      end
    end
    ret
  end

  # ;;; ---------------- backwards compatibilty

  def delete_mix(id)
    set_mix_amp(id, 0.0)
  end

  # ;;; -------- mix lists (used to be "tracks")

  def scale_mixes(mix_list, scl)
    as_one_edit_rb(get_func_name) do
      mix_list.each do |m|
        set_mix_amp(m, scl * mix_amp(m))
      end
    end
  end

  def silence_mixes(mix_list)
    scale_mixes(mix_list, 0.0)
  end

  def move_mixes(mix_list, samps)
    as_one_edit_rb(get_func_name) do
      mix_list.each do |m|
        set_mix_position(m, mix_position(m) + samps)
      end
    end
  end

  def src_mixes(mix_list, sr)
    as_one_edit_rb(get_func_name) do
      mix_list.each do |m|
        set_mix_speed(m, mix_speed(m) * sr)
      end
    end
  end

  add_help(:transpose_track,
           "transpose_mixes(mix_list, semitones)  \
Transposes each mix in MIX_LIST by SEMITONES.")
  def transpose_mixes(mix_list, semitones)
    src_mixes(mix_list, 2.0 ** (semitones / 12.0))
  end

  def color_mixes(mix_list, col)
    mix_list.each do |m|
      set_mix_color(m, col)
    end
  end

  def set_mixes_tag_y(mix_list, new_y)
    mix_list.each do |m|
      set_mix_tag_y(m, new_y)
    end
  end

  def mixes_maxamp(mix_list)
    mx = 0.0
    mix_list.each do |m|
      mx = [mx, mix_maxamp(m)].max
    end
    mx
  end

  def scale_tempo(mix_list, tempo_scl)
    first_beg = last_beg = mix_position(mix_list.car)
    mix_list.cdr.each do |m|
      pos = mix_position(m)
      first_beg = [first_beg, pos].min
      last_beg  = [last_beg,  pos].max
    end
    tempo_scl = tempo_scl.to_f
    as_one_edit_rb(get_func_name) do
      mix_list.each do |m|
        diff = (tempo_scl * (mix_position(m) - first_beg)).round
        if diff != 0
          set_mix_position(m, first_beg + diff)
        end
      end
    end
  end
  # reverse_mix_list is scale_tempo(mix_list, -1.0)

  def mixes_length(mix_list)
    max_len = mix_list.map do |m|
      mix_position(m) + mix_length(m)
    end.max
    min_len = mix_list.map do |m|
      mix_position(m)
    end.min
    max_len - min_len + 1
  end
end

include Mix

# mix.rb ends here
