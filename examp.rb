# examp.rb -- something from examp.scm

# Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 02/09/04 18:34:00
# Changed: 15/01/29 23:09:30

# module Examp (examp.scm)
#  selection_rms
#  region_rms(n)
#  window_samples(snd, chn)
#  display_energy(snd, chn)
#  display_db(snd, chn)
#  window_rms
#  fft_peak(snd, chn, scale)
#  finfo(file)
#  display_correlate(snd, chn, y0, y1)
#
#  zoom_spectrum(snd, chn, y0, y1)
#  zoom_fft(snd, chn, y0, y1)
#  superimpose_ffts(snd, chn, y0, y1)
#  locate_zero(limit)
#  shell(cmd, *rest)
#
#  mpg(mpgfile, rawfile)
#  read_ogg(filename)
#  write_ogg(snd)
#  read_speex(filename)
#  write_speex(snd)
#  read_flac(filename)
#  write_flac(snd)
#  read_ascii(in_filename, out_filename, out_type, out_format, out_srate)
#  auto_dot(snd, chn, y0, y1)
#
#  first_mark_in_window_at_left
#  flash_selected_data(interval)
#  mark_loops
#  do_all_chans(origin) do |y| ... end
#  update_graphs
#  do_chans(*origin) do |y| ... end
#  do_sound_chans(*origin) do |y| ... end
#  every_sample? do |y| ... end
#  sort_samples(nbins)
#  place_sound(mono_snd, stereo_snd, pan_env)
#
#  fft_edit(bottom, top, snd, chn)
#  fft_squelch(squelch, snd, chn)
#  fft_cancel(lo_freq, hi_freq, snd, chn)
#
#  class Ramp < Musgen
#   initialize(size)
#   run_func(up, dummy)
#   run(up)
#
#  make_ramp(size)
#  ramp(gen, up)
#  squelch_vowels(snd, chn)
#  fft_env_data(fft_env, snd, chn)
#  fft_env_edit(fft_env, snd, chn)
#  fft_env_interp(env1, env2, interp, snd, chn)
#  fft_smoother(cutoff, start, samps, snd, chn)
#
#  comb_filter(scaler, size)
#  comb_chord(scaler, size, amp, interval_one, interval_two)
#  zcomb(scaler, size, pm)
#  notch_filter(scaler, size)
#  formant_filter(radius, freq)
#  formants(r1, f1, r2, f2, r3, f3)
#  moving_formant(radius, move)
#  osc_formants(radius, bases, amounts, freqs)
#  echo(scaler, secs)
#  zecho(scaler, secs, freq, amp)
#  flecho(scaler, secs)
#  ring_mod(freq, gliss_env)
#  am(freq)
#  vibro(speed, depth)
#  hello_dentist(freq, amp, snd, chn)
#  fp(sr, osamp, osfreq, snd, chn)
#  compand()
#  expsrc(rate, snd, chn)
#  expsnd(gr_env, snd, chn)
#  cross_synthesis(cross_snd, amp, fftsize, r)
#  voiced2unvoiced(amp, fftsize, r, temp, snd, chn)
#  pulse_voice(cosin, freq, amp, fftsize, r, snd, chn)
#  cnvtest(snd0, snd1, amp)
#
#  swap_selection_channels
#  make_sound_interp(start, snd, chn)
#  sound_interp(func, loc)
#  sound_via_sound(snd1, snd2)
#  env_sound_interp(envelope, time_scale, snd, chn)
#  granulated_sound_interp(en, time_scale, grain_len, grain_env, out_hop, s, c)
#  filtered_env(en, snd, chn)
#
#  class Mouse
#   initialize
#   press(snd, chn, button, state, x, y)
#   drag(snd, chn, button, state, x, y)
#
#  files_popup_buffer(type, position, name)
#
#  find_click(loc)
#  remove_clicks
#  search_for_click
#  zero_plus
#  next_peak
#  find_pitch(pitch)
#  file2vct(file)
#  add_notes(notes, snd, chn)
#  region_play_list(data)
#  region_play_sequence(data)
#  replace_with_selection
#  explode_sf2
#
#  class Next_file
#   initialize
#   open_next_file_in_directory
#  click_middle_button_to_open_next_file_in_directory
#
#  chain_dsps(start, dur, *dsps)
#
#  scramble_channels(*new_order)
#  scramble_channel(silence)
#
#  reverse_by_blocks(block_len, snd, chn)
#  reverse_within_blocks(block_len, snd, chn)
#  sound2segment_data(main_dir, output_file)
#  channel_clipped?(snd, chn)
#  scan_sound(func, beg, dur, snd)
#  or scan_sound_rb(beg, dur, snd) do |y, chn| ... end
#  
# class Moog_filter < Musgen (moog.scm)
#   initialize(freq, q)
#   frequency=(freq)
#   filter(insig)
#
# module Moog
#  make_moog_filter(freq, q)
#  moog_filter(moog, insig)
#  moog(freq, q)

require "clm"

module Examp
  # (ext)snd.html examples made harder to break
  #
  # this mainly involves keeping track of the current sound/channel

  add_help(:selection_rms,
           "selection_rms()  \
Returns rms of selection data using samplers.")
  def selection_rms
    if selection?
      reader = make_sampler(selection_position, false, false)
      len = selection_framples()
      sum = 0.0
      len.times do
        val = next_sample(reader)
        sum = sum + val * val
      end
      free_sampler(reader)
      sqrt(sum / len)
    else
      Snd.raise(:no_active_selection)
    end
  end

  add_help(:region_rms,
           "region_rms(n=0)  \
Returns rms of region N's data (chan 0).")
  def region_rms(n = 0)
    if region?(n)
      data = region2vct(n, 0, 0)
      sqrt(dot_product(data, data) / data.length)
    else
      Snd.raise(:no_such_region)
    end
  end

  add_help(:window_samples,
           "window_samples(snd=false, chn=false)  \
Returns samples in snd channel chn in current graph window.")
  def window_samples(snd = false, chn = false)
    wl = left_sample(snd, chn)
    wr = right_sample(snd, chn)
    channel2vct(wl, 1 + (wr - wl), snd, chn)
  end

  add_help(:display_energy,
           "display_energy(snd, chn)  \
Is a $lisp_graph_hook function to display the time domain \
data as energy (squared):
$lisp_graph_hook.add_hook!(\"display-energy\", \
&method(:display_energy).to_proc)")
  def display_energy(snd, chn)
    ls = left_sample(snd, chn)
    rs = right_sample(snd, chn)
    datal = make_graph_data(snd, chn)
    data = vct?(datal) ? datal : datal[1]
    sr = srate(snd)
    y_max = y_zoom_slider(snd, chn)
    if data and ls and rs
      vct_multiply!(data, data)
      graph(data, "energy", ls / sr, rs / sr, 0.0, y_max * y_max, snd, chn)
    end
  end

  add_help(:display_db,
           "display_db(snd, chn)  \
Is a $lisp_graph_hook function to display the time domain data in dB:
$lisp_graph_hook.add_hook!(\"display-db\", &method(:display_db).to_proc)")
  def display_db(snd, chn)
    if datal = make_graph_data(snd, chn)
      dB = lambda do |val|
        if val < 0.001
          -60.0
        else
          20.0 * log10(val)
        end
      end
      data = vct?(datal) ? datal : datal[1]
      sr = srate(snd)
      ls = left_sample(snd, chn)
      rs = right_sample(snd, chn)
      data.map! do |val| 60.0 + dB.call(val.abs) end
      graph(data, "dB", ls / sr, rs / sr, 0.0, 60.0, snd, chn)
    end
  end

  add_help(:window_rms,
           "window_rms()  \
Returns rms of data in currently selected graph window.")
  def window_rms
    ls = left_sample
    rs = right_sample
    data = channel2vct(ls, 1 + (rs - ls))
    sqrt(dot_product(data, data), data.length)
  end

  add_help(:fft_peak,
           "fft_peak(snd, chn, scale)  \
Returns the peak spectral magnitude:
$after_transform_hook.add_hook!(\"fft-peak\") do |snd, chn, scale|
  fft_peak(snd, chn, scale)
end")
  def fft_peak(snd, chn, scale)
    if transform_graph? and transform_graph_type == Graph_once
      pk = (2.0 * vct_peak(transform2vct(snd, chn))) / transform_size
      status_report(pk.to_s, snd)
      pk
    else
      false
    end
  end

  # 'info' from extsnd.html using format

  add_help(:finfo,
           "finfo(file)  \
Returns description (as a string) of FILE.")
  def finfo(file)
    chans = mus_sound_chans(file)
    sr = mus_sound_srate(file)
    format("%s: chans: %d, srate: %d, %s, %s, len: %1.3f",
           file, chans, sr,
           mus_header_type_name(mus_sound_header_type(file)),
           mus_sample_type_name(mus_sound_sample_type(file)),
           mus_sound_samples(file).to_f / (chans * sr.to_f))
  end

  # Correlation
  #
  # correlation of channels in a stereo sound

  add_help(:display_correlate,
           "display_correlate(snd, chn, y0, y1)  \
Returns the correlation of SND's 2 channels (intended for use with $graph_hook):
$graph_hook.add_hook!(\"display_correlate\") do |snd, chn, y0, y1|
  display_correlate(snd, chn, y0, y1)
end")

  def display_correlate(snd, chn, y0, y1)
    if channels(snd) == 2 and framples(snd, 0) > 1 and framples(snd, 1) > 1
      ls = left_sample(snd, 0)
      rs = right_sample(snd, 0)
      ilen = 1 + (rs - ls)
      pow2 = (log(ilen) / log(2)).ceil
      fftlen = (2 ** pow2).to_i
      fftlen2 = fftlen / 2
      fftscale = 1.0 / fftlen
      rl1 = channel2vct(ls, fftlen, snd, 0)
      rl2 = channel2vct(ls, fftlen, snd, 1)
      im1 = make_vct(fftlen)
      im2 = make_vct(fftlen)
      fft(rl1, im1, 1)
      fft(rl2, im2, 1)
      tmprl = vct_copy(rl1)
      tmpim = vct_copy(im1)
      data3 = make_vct(fftlen2)
      vct_multiply!(tmprl, rl2)
      vct_multiply!(tmpim, im2)
      vct_multiply!(im2, rl1)
      vct_multiply!(rl2, im1)
      vct_add!(tmprl, tmpim)
      vct_subtract!(im2, rl2)
      fft(tmprl, im2, -1)
      vct_add!(data3, tmprl)
      vct_scale!(data3, fftscale)
      graph(data3, "lag time", 0, fftlen2)
    else
      snd_print("correlate wants stereo input")
    end
  end

  # set transform-size based on current time domain window size
  # 
  # also zoom spectrum based on y-axis zoom slider

  add_help(:zoom_spectrum,
           "zoom_spectrum(snd, chn, y0, y1)  \
Sets the transform size to correspond to the \
time-domain window size (use with $graph_hook):
$graph_hook.add_hook!(\"zoom-spectrum\") do |snd, chn, y0, y1|
  zoom_spectrum(snd, chn, y0, y1)
end")
  def zoom_spectrum(snd, chn, y0, y1)
    if transform_graph?(snd, chn) and
       transform_graph_type(snd, chn) == Graph_once
      set_transform_size((2 **
                         (log(right_sample(snd, chn) -
                              left_sample(snd, chn))/log(2.0))).to_i, snd, chn)
      set_spectrum_end(y_zoom_slider(snd, chn), snd, chn)
    end
    false
  end

  add_help(:zoom_fft,
           "zoom_fft(snd, chn, y0, y1)  \
Sets the transform size if the time domain is \
not displayed (use with $graph_hook).  \
It also sets the spectrum display start point \
based on the x position slider---this can be confusing \
if fft normalization is on (the default):
$graph_hook.add_hook!(\"zoom-fft\") do |snd, chn, y0, y1|
  zoom_fft(snd, chn, y0, y1)
end")
  def zoom_fft(snd, chn, y0, y1)
    if transform_graph?(snd, chn) and
        (not time_graph?(snd, chn)) and
        transform_graph_type(snd, chn) == Graph_once
      set_transform_size(2 ** 
                         (log(right_sample(snd, chn) - 
                              left_sample(snd, chn)) / log(2.0)).ceil, snd, chn)
      set_spectrum_start(x_position_slider(snd, chn), snd, chn)
      set_spectrum_end(y_zoom_slider(snd, chn), snd, chn)
    end
    false
  end

  # superimpose spectra of sycn'd sounds

  add_help(:superimpose_ffts,
           "superimpose_ffts(snd, chn, y0, y1)  \
Superimposes ffts of multiple (syncd) sounds (use with $graph_hook):
$graph_hook.add_hook!(\"superimpose-ffts\") do |snd, chn, y0, y1|
  superimpose_ffts(snd, chn, y0, y1)
end")
  def superimpose_ffts(snd, chn, y0, y1)
    maxsync = Snd.sounds.map do |s| sync(s) end.max
    if sync(snd) > 0 and
        snd == Snd.sounds.map do |s|
        sync(snd) == sync(s) ? sound2integer(s) : (maxsync + 1)
      end.min
      ls = left_sample(snd, chn)
      rs = right_sample(snd, chn)
      pow2 = (log(rs - ls) / log(2)).ceil
      fftlen = (2 ** pow2).to_i
      if pow2 > 2
        ffts = []
        Snd.sounds.each do |s|
          if sync(snd) == sync(s) and channels(s) > chn
            fdr = channel2vct(ls, fftlen, s, chn)
            fdi = make_vct(fftlen)
            spectr = make_vct(fftlen / 2)
            ffts.push(spectr.add(spectrum(fdr, fdi, false, 2)))
          end
        end
        graph(ffts, "spectra", 0.0, 0.5, y0, y1, snd, chn)
      end
    end
    false
  end

  # c-g? example (Anders Vinjar)

  add_help(:locate_zero,
           "locate_zero(limit)  \
Looks for successive samples that sum to less than LIMIT, \
moving the cursor if successful.")
  def locate_zero(limit)
    start = cursor
    sf = make_sampler(start, false, false)
    val0 = sf.call.abs
    val1 = sf.call.abs
    n = start
    until sampler_at_end?(sf) or val0 + val1 < limit
      val0, val1 = val1, sf.call.abs
      n += 1
    end
    free_sampler(sf)
    set_cursor(n)
  end
  
  # make a system call from irb or snd listener
  # 
  #   shell("df") for example
  # or to play a sound whenever a file is closed:
  #   $close-hook.add_hook!() do |snd| shell("sndplay wood16.wav"); false end

  add_help(:shell,
           "shell(cmd, *rest)  \
Sends CMD to a shell (executes it as a shell command) \
and returns the result string.")
  def shell(cmd, *rest)
    str = ""
    unless cmd.null?
      IO.popen(format(cmd, *rest), "r") do |f| str = f.readlines.join end
    end
    str
  end

  # translate mpeg input to 16-bit linear and read into Snd
  # 
  # mpg123 with the -s switch sends the 16-bit (mono or stereo) representation
  #   of an mpeg file to stdout.  There's also apparently a switch to write
  #   'wave' output.

  add_help(:mpg,
           "mpg(file, tmpname)  \
Converts file from MPEG to raw 16-bit samples using mpg123: \
mpg(\"mpeg.mpg\", \"mpeg.raw\")")
  def mpg(mpgfile, rawfile)
    b0 = b1 = b2 = b3 = 0
    File.open(mpgfile, "r") do |fd|
      b0 = fd.readchar
      b1 = fd.readchar
      b2 = fd.readchar
      b3 = fd.readchar
    end
    if b0 != 255 or (b1 & 0b11100000) != 0b11100000
      Snd.display("%s is not an MPEG file (first 11 bytes: %b %b",
                  mpgfile.inspect, b0, b1 & 0b11100000)
    else
      id = (b1 & 0b11000) >> 3
      layer = (b1 & 0b110) >> 1
      srate_index = (b2 & 0b1100) >> 2
      channel_mode = (b3 & 0b11000000) >> 6
      if id == 1
        Snd.display("odd: %s is using a reserved Version ID", mpgfile)
      end
      if layer == 0
        Snd.display("odd: %s is using a reserved layer description", mpgfile)
      end
      chans = channel_mode == 3 ? 1 : 2
      mpegnum = id.zero? ? 4 : (id == 2 ? 2 : 1)
      mpeg_layer = layer == 3 ? 1 : (layer == 2 ? 2 : 3)
      srate = [44100, 48000, 32000, 0][srate_index] / mpegnum
      Snd.display("%s: %s Hz, %s, MPEG-%s",
                  mpgfile, srate, chans == 1 ? "mono": "stereo", mpeg_layer)
      system(format("mpg123 -s %s > %s", mpgfile, rawfile))
      open_raw_sound(rawfile, chans, srate,
                     little_endian? ? Mus_lshort : Mus_bshort)
    end
  end

  # read and write OGG files

  add_help(:read_ogg,
           "read_ogg(filename)  \
Read OGG files:
$open_hook.add_hook!(\"read-ogg\") do |filename|
  if mus_sound_header_type(filename) == Mus_raw
    read_ogg(filename)
  else
    false
  end
end")
  def read_ogg(filename)
    flag = false
    File.open(filename, "r") do |fd|
      flag = fd.readchar == ?O and 
      fd.readchar == ?g and 
      fd.readchar == ?g and 
      fd.readchar == ?S
    end
    if flag
      aufile = filename + ".au"
      File.unlink(aufile) if File.exist?(aufile)
      system(format("ogg123 -d au -f %s %s", aufile, filename))
      aufile
    else
      false
    end
  end

  def write_ogg(snd)
    if edits(snd)[0] > 0 or header_type(snd) != Mus_riff
      file = file_name(snd) + ".tmp"
      save_sound_as(file, snd, :header_type, Mus_riff)
      system("oggenc " + file)
      File.unlink(file)
    else
      system("oggenc " + file_name(snd))
    end
  end

  # read and write Speex files

  def read_speex(filename)
    wavfile = filename + ".wav"
    File.unlink(wavfile) if File.exist?(wavfile)
    system(format("speexdec %s %s", filename, wavfile))
    wavfile
  end

  def write_speex(snd)
    if edits(snd)[0] > 0 or header_type(snd) != Mus_riff
      file = file_name(snd) + ".wav"
      spxfile = file_name(snd) + "spx"
      save_sound_as(file, snd, :header_type, Mus_riff)
      system(format("speexenc %s %s", file, spxfile))
      File.unlink(file)
    else
      system(format("speexenc %s %s", file_name(snd), spxfile))
    end
  end

  # read and write FLAC files

  def read_flac(filename)
    system(format("flac -d %s", filename))
  end

  def write_flac(snd)
    if edits(snd)[0] > 0 or header_type(snd) != Mus_riff
      file = file_name(snd) + ".wav"
      save_sound_as(file, snd, :header_type, Mus_riff)
      system(format("flac %s", file))
      File.unlink(file)
    else
      system(format("flac %s ", file_name(snd)))
    end
  end

  # read ASCII files
  #
  # these are used by Octave (WaveLab) -- each line has one integer,
  # apparently a signed short.

  def read_ascii(in_filename,
                 out_filename = "test.snd",
                 out_type = Mus_next,
                 out_format = Mus_bshort,
                 out_srate = 44100)
    in_buffer = IO.readlines(in_filename)         # array of strings
    com = format("created by %s: %s", get_func_name, in_filename)
    out_snd = new_sound(out_filename, 1, out_srate, out_format, out_type, com)
    bufsize = 512
    data = make_vct(bufsize)
    loc = 0
    frame = 0
    short2float = 1.0 / 32768.0
    as_one_edit_rb do | |
      in_buffer.each do |line|
        line.split.each do |str_val|
          val = eval(str_val)
          data[loc] = val * short2float
          loc += 1
          if loc == bufsize
            vct2channel(data, frame, bufsize, out_snd, 0)
            frame += bufsize
            loc = 0
          end
        end
      end
      if loc > 0
        vct2channel(data, frame, loc, out_snd, 0)
      end
    end
    out_snd
  end

  # make dot size dependent on number of samples being displayed
  #
  # this could be extended to set time_graph_style to Graph_lines if
  # many samples are displayed, etc

  add_help(:auto_dot,
           "auto_dot(snd, chn, y0, y1)  \
Sets the dot size depending on the number \
of samples being displayed (use with $graph_hook):
$graph_hook.add_hook!(\"auto-dot\") do |snd, chn, y0, y1|
  auto_dot(snd, chn, y0, y1)
end")
  def auto_dot(snd, chn, y0, y1)
    dots = right_sample(snd, chn) - left_sample(snd, chn)
    case dots
    when 100
      set_dot_size(1, snd, chn)
    when 50
      set_dot_size(2, snd, chn)
    when 25
      set_dot_size(3, snd, chn)
    else
      set_dot_size(5, snd, chn)
    end
    false
  end

  # move window left edge to mark upon 'm'
  # 
  # in large sounds, it can be pain to get the left edge of the window
  # aligned with a specific spot in the sound.  In this code, we
  # assume the desired left edge has a mark, and the 'm' key (without
  # control) will move the window left edge to that mark.

  add_help(:first_mark_in_window_at_left,
           "first_mark_in_window_at_left()  \
Moves the graph so that the leftmost visible mark is at the left edge:
bind_key(?m, 0, lambda do | | first_mark_in_window_at_left end)")
  def first_mark_in_window_at_left
    keysnd = Snd.snd
    keychn = Snd.chn
    current_left_sample = left_sample(keysnd, keychn)
    chan_marks = marks(keysnd, keychn)
    if chan_marks.null?
      snd_print("no marks!")
    else
      leftmost = chan_marks.map do |m|
        mark_sample(m)
      end.detect do |m|
        m > current_left_sample
      end
      if leftmost.null?
        snd_print("no mark in window")
      else
        set_left_sample(leftmost, keysnd, keychn)
        Keyboard_no_action
      end
    end
  end
  
  # flash selected data red and green

  add_help(:flash_selected_data,
           "flash_selected_data(millisecs)  \
Causes the selected data to flash red and green.")
  def flash_selected_data(interval)
    if selected_sound
      set_selected_data_color(selected_data_color == Red ? Green : Red)
      call_in(interval, lambda do | | flash_selected_data(interval) end)
    end
  end

  # use loop info (if any) to set marks at loop points

  add_help(:mark_loops,
           "mark_loops()  \
Places marks at loop points found in the selected sound's header.")
  def mark_loops
    loops = (sound_loop_info or mus_sound_loop_info(file_name))
    if loops and !loops.empty?
      unless loops[0].zero? and loops[1].zero?
        add_mark(loops[0])
        add_mark(loops[1])
        unless loops[2].zero? and loops[3].zero?
          add_mark(loops[2])
          add_mark(loops[3])
        end     
      end
    else
      Snd.display("%s has no loop info", short_file_name.inspect)
    end
  end

  # mapping extensions (map arbitrary single-channel function over
  # various channel collections)

  add_help(:do_all_chans,
           "do_all_chans(edhist) do |y| ... end  \
Applies func to all active channels, \
using EDHIST as the edit history indication: \
do_all_chans(\"double all samples\", do |val| 2.0 * val end)")
  def do_all_chans(origin, &func)
    Snd.sounds.each do |snd|
      channels(snd).times do |chn|
        map_channel(func, 0, false, snd, chn, false, origin)
      end
    end
  end

  add_help(:update_graphs,
           "update_graphs()  \
Updates (redraws) all graphs.")
  def update_graphs
    Snd.sounds.each do |snd|
      channels(snd).times do |chn|
        update_time_graph(snd, chn)
      end
    end
  end

  add_help(:do_chans,
           "do_chans(edhist) do |y| ... end  \
Applies func to all sync'd channels using EDHIST \
as the edit history indication.")
  def do_chans(*origin, &func)
    snc = sync
    if snc > 0
      Snd.sounds.each do |snd|
        channels(snd).times do |chn|
          if sync(snd) == snc
            map_channel(func, 0, false, snd, chn, false,
                        (origin.empty? ? "" : format(*origin)))
          end
        end
      end
    else
      snd_warning("sync not set")
    end
  end

  add_help(:do_sound_chans,
           "do_sound_chans(edhist) do |y| ... end  \
Applies func to all selected channels using EDHIST \
as the edit history indication.")
  def do_sound_chans(*origin, &func)
    if snd = selected_sound
      channels(snd).times do |chn|
        map_channel(func, 0, false, snd, chn, false,
                    (origin.empty? ? "" : format(*origin)))
      end
    else
      snd_warning("no selected sound")
    end
  end

  add_help(:every_sample?,
           "every_sample? do |y| ... end  \
Returns true if func is not false for all samples in the current channel, \
otherwise it moves the cursor to the first offending sample.")
  def every_sample?(&func)
    snd = Snd.snd
    chn = Snd.chn
    if baddy = scan_channel(lambda do |y|
                              (not func.call(y))
                            end, 0, framples(snd, chn), snd, chn)
      set_cursor(baddy[1])
    end
    (not baddy)
  end

  add_help(:sort_samples,
           "sort_samples(bins)  \
Provides a histogram in BINS bins.")
  def sort_samples(nbins)
    bins = make_array(nbins, 0)
    scan_channel(lambda do |y|
                   bin = (y.abs * nbins).floor
                   bins[bin] += 1
                   false
                 end)
    bins
  end

  # mix mono sound into stereo sound panning according to env

  add_help(:place_sound,
           "place_sound(mono_snd, stereo_snd, pan_env)  \
Mixes a mono sound into a stereo sound, splitting it into two copies \
whose amplitudes depend on the envelope PAN_ENV.  \
If PAN_ENV is a number, \
the sound is split such that 0 is all in channel 0 \
and 90 is all in channel 1.")
  def place_sound(mono_snd, stereo_snd, pan_env)
    len = framples(mono_snd)
    if number?(pan_env)
      pos = pan_env / 90.0
      rd0 = make_sampler(0, mono_snd, false)
      rd1 = make_sampler(0, mono_snd, false)
      map_channel(lambda do |y| y + pos * read_sample(rd1) end,
                  0, len, stereo_snd, 1)
      map_channel(lambda do |y| y + (1.0 - pos) * read_sample(rd0) end,
                  0, len, stereo_snd, 0)
    else
      e0 = make_env(:envelope, pan_env, :length, len)
      e1 = make_env(:envelope, pan_env, :length, len)
      rd0 = make_sampler(0, mono_snd, false)
      rd1 = make_sampler(0, mono_snd, false)
      map_channel(lambda do |y| y + env(e1) * read_sample(rd1) end,
                  0, len, stereo_snd, 1)
      map_channel(lambda do |y| y + (1.0 - env(e0)) * read_sample(rd0) end,
                  0, len, stereo_snd, 0)
    end
  end

  # FFT-based editing

  add_help(:fft_edit,
           "fft_edit(low_Hz, high_Hz)  \
Ffts an entire sound, removes all energy below low-Hz and all above high-Hz, \
then inverse ffts.")
  def fft_edit(bottom, top, snd = false, chn = false)
    sr = srate(snd).to_f
    len = framples(snd, chn)
    fsize = (2.0 ** (log(len) / log(2.0)).ceil).to_i
    rdata = channel2vct(0, fsize, snd, chn)
    idata = make_vct(fsize)
    lo = (bottom / (sr / fsize)).round
    hi = (top / (sr / fsize)).round
    fft(rdata, idata, 1)
    j = fsize - 1
    lo.times do |i|
      rdata[i] = rdata[j] = 0.0
      rdata[i] = rdata[j] = 0.0
      j -= 1
    end
    j = fsize - hi
    (hi..(fsize / 2)).each do |i|
      rdata[i] = rdata[j] = 0.0
      rdata[i] = rdata[j] = 0.0
      j -= 1
    end
    fft(rdata, idata, -1)
    vct_scale!(rdata, 1.0 / fsize)
    vct2channel(rdata, 0, len - 1, snd, chn, false,
                format("%s(%s, %s", get_func_name, bottom, top))
  end

  add_help(:fft_squelch,
           "fft_squelch(squelch, snd=false, chn=false)  \
Ffts an entire sound, sets all bins to 0.0 whose energy is below squelch, \
then inverse ffts.")
  def fft_squelch(squelch, snd = false, chn = false)
    len = framples(snd, chn)
    fsize = (2.0 ** (log(len) / log(2.0)).ceil).to_i
    rdata = channel2vct(0, fsize, snd, chn)
    idata = make_vct(fsize)
    fsize2 = fsize / 2
    fft(rdata, idata, 1)
    vr = vct_copy(rdata)
    vi = vct_copy(idata)
    rectangular2polar(vr, vi)
    scaler = vct_peak(vr)
    scl_squelch = squelch * scaler
    j = fsize - 1
    fsize2.times do |i|
      if sqrt(rdata[i] * rdata[i] + idata[i] * idata[i]) < scl_squelch
        rdata[i] = rdata[j] = 0.0
        rdata[i] = rdata[j] = 0.0
      end
      j -= 1
    end
    fft(rdata, idata, -1)
    vct_scale!(rdata, 1.0 / fsize)
    vct2channel(rdata, 0, len - 1, snd, chn, false,
                format("%s(%s", get_func_name, squelch))
    scaler
  end

  add_help(:fft_cancel,
           "fft_cancel(lo_freq, hi_freq, snd=false, chn=false)  \
Ffts an entire sound, sets the bin(s) representing lo_freq to hi_freq to 0.0, \
then inverse ffts.")
  def fft_cancel(lo_freq, hi_freq, snd = false, chn = false)
    sr = srate(snd).to_f
    len = framples(snd, chn)
    fsize = (2.0 ** (log(len) / log(2.0)).ceil).to_i
    rdata = channel2vct(0, fsize, snd, chn)
    idata = make_vct(fsize)
    fsize2 = fsize / 2
    fft(rdata, idata, 1)
    hz_bin = sr / fsize
    lo_bin = (lo_freq / hz_bin).round
    hi_bin = (hi_freq / hz_bin).round
    j = fsize - lo_bin - 1
    fsize2.times do |i|
      if i > hi_bin
        rdata[i] = rdata[j] = 0.0
        rdata[i] = rdata[j] = 0.0
      end
      j -= 1
    end
    fft(rdata, idata, -1)
    vct_scale!(rdata, 1.0 / fsize)
    vct2channel(rdata, 0, len - 1, snd, chn, false,
                format("%s(%s, %s", get_func_name, lo_freq, hi_freq))
  end

  # same idea but used to distinguish vowels (steady-state) from consonants

  class Ramp < Musgen
    def initialize(size = 128)
      super()
      @val = 0.0
      @size = size
      @incr = 1.0 / size
      @up = true
    end

    def inspect
      format("%s.new(%d)", self.class, @size)
    end

    def to_s
      format("#<%s size: %d, val: %1.3f, incr: %1.3f, up: %s>",
             self.class, @size, @val, @incr, @up)
    end

    def run_func(up = true, dummy = false)
      @up = up
      @val += (@up ? @incr : -@incr)
      @val = [1.0, [0.0, @val].max].min
    end

    def run(up = true)
      self.run_func(up, false)
    end
  end

  add_help(:make_ramp,
           "make_ramp(size=128)  \
Returns a ramp generator.")
  def make_ramp(size = 128)
    Ramp.new(size)
  end

  add_help(:ramp,
           "ramp(gen, up)  \
Is a kind of CLM generator that produces a ramp of a given length, \
then sticks at 0.0 or 1.0 until the UP argument changes.")
  def ramp(gen, up)
    gen.run_func(up, false)
  end

  add_help(:squelch_vowels,
           "squelch_vowels(snd=false, chn=false)  \
Suppresses portions of a sound that look like steady-state.")
  def squelch_vowels(snd = false, chn = false)
    fft_size = 32
    fft_mid = (fft_size / 2.0).floor
    ramper = Ramp.new(256)
    peak = maxamp(snd, chn) / fft_mid
    read_ahead = make_sampler(0, snd, chn)
    rl = Vct.new(fft_size) do
      read_sample(read_ahead)
    end
    im = Vct.new(fft_size, 0.0)
    ctr = fft_size - 1
    in_vowel = false
    map_channel(lambda do |y|
                  ctr += 1
                  if ctr == fft_size
                    ctr = 0
                    fft(rl, im, 1)
                    rl.multiply!(rl)
                    im.multiply!(im)
                    rl.add!(im)
                    im.fill(0.0)
                    in_vowel = (rl[0] + rl[1] + rl[2] + rl[3]) > peak
                    rl.map! do
                      read_sample(read_ahead)
                    end
                  end
                  y * (1.0 - ramper.run(in_vowel))
                end, 0, false, snd, chn, false, "squelch_vowels(")
  end

  add_help(:fft_env_data,
           "fft_env_data(fft-env, snd=false, chn=false)  \
Applies fft_env as spectral env to current sound, returning vct of new data.")
  def fft_env_data(fft_env, snd = false, chn = false)
    len = framples(snd, chn)
    fsize = (2 ** (log(len) / log(2.0)).ceil).to_i
    rdata = channel2vct(0, fsize, snd, chn)
    idata = make_vct(fsize)
    fsize2 = fsize / 2
    en = make_env(:envelope, fft_env, :length, fsize2)
    fft(rdata, idata, 1)
    j = fsize - 1
    fsize2.times do |i|
      val = env(en)
      rdata[i] *= val
      idata[i] *= val
      rdata[j] *= val
      idata[j] *= val
      j -= 1
    end
    fft(rdata, idata, -1)
    vct_scale!(rdata, 1.0 / fsize)
  end

  add_help(:fft_env_edit,
           "fft_env_edit(fft-env, snd=false, chn=false)  \
Edits (filters) current chan using fft_env.")
  def fft_env_edit(fft_env, snd = false, chn = false)
    vct2channel(fft_env_data(fft_env, snd, chn), 0, framples(snd, chn) - 1,
                snd, chn, false,
                format("%s(%s", get_func_name, fft_env.inspect))
  end

  add_help(:fft_env_interp,
           "fft_env_interp(env1, env2, interp, snd=false, chn=false)  \
Interpolates between two fft-filtered \
versions (env1 and env2 are the spectral envelopes) \
following interp (an env between 0 and 1).")
  def fft_env_interp(env1, env2, interp, snd = false, chn = false)
    data1 = fft_env_data(env1, snd, chn)
    data2 = fft_env_data(env2, snd, chn)
    len = framples(snd, chn)
    en = make_env(:envelope, interp, :length, len)
    new_data = make_vct!(len) do |i|
      pan = env(en)
      (1.0 - pan) * data1[i] + pan * data2[i]
    end
    vct2channel(new_data, 0, len - 1, snd, chn, false,
                format("%s(%s, %s, %s", get_func_name,
                       env1.inspect, env2.inspect, interp.inspect))
  end

  add_help(:fft_smoother,
           "fft_smoother(cutoff, start, samps, snd=false, chn=false)  \
Uses fft-filtering to smooth a section: \
vct2channel(fft_smoother(0.1, cursor, 400, 0, 0), cursor, 400)")
  def fft_smoother(cutoff, start, samps, snd = false, chn = false)
    fftpts = (2 ** (log(samps + 1) / log(2.0)).ceil).to_i
    rl = channel2vct(start, samps, snd, chn)
    im = make_vct(fftpts)
    top = (fftpts * cutoff.to_f).floor
    old0 = rl[0]
    old1 = rl[samps - 1]
    oldmax = vct_peak(rl)
    fft(rl, im, 1)
    (top...fftpts).each do |i| rl[i] = im[i] = 0.0 end
    fft(rl, im, -1)
    vct_scale!(rl, 1.0 / fftpts)
    newmax = vct_peak(rl)
    if newmax.zero?
      rl
    else
      if (scl = oldmax / newmax) > 1.5
        vct_scale!(rl, scl)
      end
      new0 = rl[0]
      new1 = rl[samps - 1]
      offset0 = old0 - new0
      offset1 = old1 - new1
      incr = offset0 == offset1 ? 0.0 : ((offset1 - offset0) / samps)
      trend = offset0 - incr
      rl.map do |val|
        trend += incr
        val += trend
      end
    end
  end

  # comb-filter 

  add_help(:comb_filter,
           "comb_filter(scaler, size)  \
Returns a comb-filter ready for map_channel etc: \
map_channel(comb_filter(0.8, 32))  \
If you're in a hurry use: clm_channel(make_comb(0.8, 32)) instead.")
  def comb_filter(scaler, size)
    cmb = make_comb(scaler, size)
    lambda do |inval| comb(cmb, inval) end
  end

  # by using filters at harmonically related sizes, we can get chords:

  add_help(:comb_chord,
           "comb_chord(scl, size, amp, interval_one=0.75, interval_two=1.2)  \
Returns a set of harmonically-related comb filters: \
map_channel(comb_chord(0.95, 100, 0.3))")
  def comb_chord(scaler, size, amp, interval_one = 0.75, interval_two = 1.2)
    c1 = make_comb(scaler, size.to_i)
    c2 = make_comb(scaler, (interval_one * size).to_i)
    c3 = make_comb(scaler, (interval_two * size).to_i)
    lambda do |inval|
      amp * (comb(c1, inval) + comb(c2, inval) + comb(c3, inval))
    end
  end

  # or change the comb length via an envelope:

  add_help(:zcomb,
           "zcomb(scaler, size, pm)  \
Returns a comb filter whose length varies according to an envelope: \
map_channel(zcomb(0.8, 32, [0, 0, 1, 10]))")
  def zcomb(scaler, size, pm)
    max_envelope_1 = lambda do |en, mx|
      1.step(en.length - 1, 2) do |i| mx = [mx, en[i]].max.to_f end
      mx
    end
    cmb = make_comb(scaler, size,
                    :max_size, (max_envelope_1.call(pm, 0.0) + size + 1).to_i)
    penv = make_env(:envelope, pm, :length, framples())
    lambda do |inval| comb(cmb, inval, env(penv)) end
  end

  add_help(:notch_filter,
           "notch_filter(scaler, size)  \
Returns a notch-filter: map_channel(notch_filter(0.8, 32))")
  def notch_filter(scaler, size)
    gen = make_notch(scaler, size)
    lambda do |inval| notch(gen, inval) end
  end

  add_help(:formant_filter,
                   "formant_filter(radius, frequency)  \
Returns a formant generator: map_channel(formant_filter(0.99, 2400))  \
Faster is: filter_sound(make_formant(2400, 0.99))")
  def formant_filter(radius, freq)
    frm = make_formant(radius, freq)
    lambda do |inval| formant(frm, inval) end
  end

  # to impose several formants, just add them in parallel:

  add_help(:formants,
           "formants(r1, f1, r2, f2, r3, f3)  \
Returns 3 formant filters in parallel: \
map_channel(formants(0.99, 900, 0.98, 1800, 0.99 2700))")
  def formants(r1, f1, r2, f2, r3, f3)
    fr1 = make_formant(f1, r1)
    fr2 = make_formant(f2, r2)
    fr3 = make_formant(f3, r3)
    lambda do |inval|
      formant(fr1, inval) + formant(fr2, inval) + formant(fr3, inval)
    end
  end

  add_help(:moving_formant,
           "moving_formant(radius, move)  \
Returns a time-varying (in frequency) formant filter: \
map_channel(moving_formant(0.99, [0, 1200, 1, 2400]))")
  def moving_formant(radius, move)
    frm = make_formant(move[1], radius)
    menv = make_env(:envelope, move, :length, framples())
    lambda do |inval|
      val = formant(frm, inval)
      frm.frequency = env(menv)
      val
    end
  end

  add_help(:osc_formants,
           "osc_formants(radius, bases, amounts, freqs)  \
Set up any number of independently oscillating formants, \
then calls map_channel: \
osc_formants(0.99, vct(400, 800, 1200), vct(400, 800, 1200), vct(4, 2, 3))")
  def osc_formants(radius, bases, amounts, freqs)
    len = bases.length
    frms = make_array(len) do |i| make_formant(bases[i], radius) end
    oscs = make_array(len) do |i| make_oscil(freqs[i]) end
    map_channel_rb() do |x|
      val = 0.0
      frms.each_with_index do |frm, i|
        val += formant(frm, x)
        set_mus_frequency(frm, bases[i] + amounts[i] * oscil(oscs[i]))
      end
      val
    end
  end

  # echo

  add_help(:echo,
           "echo(scaler, secs)  \
Returns an echo maker: map_channel(echo(0.5, 0.5), 0 44100)")
  def echo(scaler, secs)
    del = make_delay((secs * srate()).round)
    lambda do |inval|
      inval + delay(del, scaler * (tap(del) + inval))
    end
  end

  add_help(:zecho,
           "zecho(scaler, secs, freq, amp)  \
Returns a modulated echo maker: \
map_channel(zecho(0.5, 0.75, 6, 10.0), 0, 65000)")
  def zecho(scaler, secs, freq, amp)
    os = make_oscil(freq)
    len = (secs * srate()).round
    del = make_delay(len, :max_size, (len + amp + 1).to_i)
    lambda do |inval|
      inval + delay(del, scaler * (tap(del) + inval), amp * oscil(os))
    end
  end

  add_help(:flecho,
           "flecho(scaler, secs)  \
Returns a low-pass filtered echo maker: \
map_channel(flecho(0.5, 0.9), 0, 75000)" )
  def flecho(scaler, secs)
    flt = make_fir_filter(:order, 4, :xcoeffs, vct(0.125, 0.25, 0.25, 0.125))
    del = make_delay((secs * srate()).round)
    lambda do |inval|
      inval + delay(del, fir_filter(flt, scaler * (tap(del) + inval)))
    end
  end

  # ring-mod and am
  #
  # CLM instrument is ring-modulate.ins

  add_help(:ring_mod,
           "ring_mod(freq, gliss_env)  \
Returns a time-varying ring-modulation filter: \
map_channel(ring_mod(10, [0, 0, 1, hz2radians(100)]))")
  def ring_mod(freq, gliss_env)
    os = make_oscil(:frequency, freq)
    len = framples()
    genv = make_env(:envelope, gliss_env, :length, len)
    lambda do |inval| oscil(os, env(genv)) * inval end
  end

  add_help(:am,
           "am(freq)  \
returns an amplitude-modulator: map_channel(am(440))")
  def am(freq)
    os = make_oscil(freq)
    lambda do |inval| amplitude_modulate(1.0, inval, oscil(os)) end
  end

  def vibro(speed, depth)
    sine = make_oscil(speed)
    scl = 0.5 * depth
    offset = 1.0 - scl
    lambda do |inval| inval * (offset + scl * oscil(sine)) end
  end

  # hello-dentist
  #
  # CLM instrument version is in clm.html

  add_help(:hello_dentist,
           "hello_dentist(frq, amp, snd=false, chn=false)  \
Varies the sampling rate randomly, \
making a voice sound quavery: hello_dentist(40.0, 0.1)")
  def hello_dentist(freq, amp, snd = false, chn = false)
    rn = make_rand_interp(:frequency, freq, :amplitude, amp)
    i = 0
    len = framples()
    in_data = channel2vct(0, len, snd, chn)
    out_len = (len * (1.0 + 2.0 * amp)).to_i
    out_data = make_vct(out_len)
    rd = make_src(:srate, 1.0,
                  :input, lambda do |dir|
                    val = i.between?(0, len - 1) ? in_data[i] : 0.0
                    i += dir
                    val
                  end)
    vct2channel(out_data.map do |x| src(rd, rand_interp(rn)) end,
                0, len, snd, chn, false,
                format("%s(%s, %s", get_func_name, freq, amp))
  end

  # a very similar function uses oscil instead of rand-interp, giving
  # various "Forbidden Planet" sound effects:

  add_help(:fp,
           "fp(sr, osamp, osfrq, snd=false, chn=false)  \
Varies the sampling rate via an oscil: fp(1.0, 0.3, 20)")
  def fp(sr, osamp, osfreq, snd = false, chn = false)
    os = make_oscil(:frequency, osfreq)
    s = make_src(:srate, sr)
    len = framples(snd, chn)
    sf = make_sampler(0, snd, chn)
    out_data = make_vct!(len) do
      src(s, osamp * oscil(os),
          lambda do |dir|
            dir > 0 ? next_sample(sf) : previous_sample(sf)
          end)
    end
    free_sampler(sf)
    vct2channel(out_data, 0, len, snd, chn, false,
                format("%s(%s, %s, %s", get_func_name, sr, osamp, osfreq))
  end

  # compand

  Compand_table = vct(-1.0, -0.96, -0.9, -0.82, -0.72, -0.6, -0.45, -0.25, 
                       0.0, 0.25, 0.45, 0.6, 0.72, 0.82, 0.9, 0.96, 1.0)
  add_help(:compand,
           "compand()  \
Returns a compander: map_channel(compand())")
  def compand()
    lambda do |inval|
      array_interp(Compand_table, 8.0 + 8.0 * inval, Compand_table.length)
    end
  end

  # shift pitch keeping duration constant
  # 
  # both src and granulate take a function argument to get input
  # whenever it is needed.  in this case, src calls granulate which
  # reads the currently selected file.  CLM version is in expsrc.ins

  add_help(:expsrc,
           "expsrc(rate, snd=false, chn=false)  \
Uses sampling-rate conversion and granular synthesis to produce a sound \
at a new pitch but at the original tempo.  \
It returns a function for map_chan.")
  def expsrc(rate, snd = false, chn = false)
    gr = make_granulate(:expansion, rate)
    sr = make_src(:srate, rate)
    vsize = 1024
    vbeg = 0
    v = channel2vct(0, vsize)
    inctr = 0
    lambda do |inval|
      src(sr, 0.0, lambda do |dir|
            granulate(gr, lambda do |dr|
                        val = v[inctr]
                        inctr += dr
                        if inctr >= vsize
                          vbeg += inctr
                          inctr = 0
                          v = channel2vct(vbeg, vsize, snd, chn)
                        end
                        val
                      end)
          end)
    end
  end

  # the next (expsnd) changes the tempo according to an envelope; the
  # new duration will depend on the expansion envelope -- we integrate
  # it to get the overall expansion, then use that to decide the new
  # length.

  add_help(:expsnd,
           "expsnd(gr_env, snd=false, chn=false)  \
Uses the granulate generator to change tempo \
according to an envelope: expsnd([0, 0.5, 2, 2.0])")
  def expsnd(gr_env, snd = false, chn = false)
    dur = ((framples(snd, chn) / srate(snd)) *
           integrate_envelope(gr_env) / envelope_last_x(gr_env))
    gr = make_granulate(:expansion, gr_env[1], :jitter, 0)
    ge = make_env(:envelope, gr_env, :duration, dur)
    sound_len = (srate(snd) * dur).to_i
    len = [sound_len, framples(snd, chn)].max
    sf = make_sampler(0, snd, chn)
    out_data = make_vct!(len) do
      val = granulate(gr, lambda do |dir| next_sample(sf) end)
      gr.increment = env(ge)
      val
    end
    free_sampler(sf)
    vct2channel(out_data, 0, len, snd, chn, false,
                format("%s(%s", get_func_name, gr_env.inspect))
  end

  # cross-synthesis
  #
  # CLM version is in clm.html

  add_help(:cross_synthesis,
           "cross_synthesis(cross_snd, amp, fftsize, r)  \
Does cross-synthesis between CROSS_SND (a sound index) \
and the currently selected sound: \
map_channel(cross_synthesis(1, 0.5, 128, 6.0))")
  def cross_synthesis(cross_snd, amp, fftsize, r)
    freq_inc = fftsize / 2
    fdr = make_vct(fftsize)
    fdi = make_vct(fftsize)
    spectr = make_vct(freq_inc)
    inctr = 0
    ctr = freq_inc
    radius = 1.0 - r / fftsize.to_f
    bin = srate() / fftsize.to_f
    fmts = make_array(freq_inc) do |i|
      make_formant(i * bin, radius)
    end
    formants = make_formant_bank(fmts, spectr)
    lambda do |inval|
      if ctr == freq_inc
        fdr = channel2vct(inctr, fftsize, cross_snd, 0)
        inctr += freq_inc
        spectrum(fdr, fdi, false, 2)
        vct_subtract!(fdr, spectr)
        vct_scale!(fdr, 1.0 / freq_inc)
        ctr = 0
      end
      ctr += 1
      vct_add!(spectr, fdr)
      amp * formant_bank(formants, inval)
    end
  end

  # similar ideas can be used for spectral cross-fades, etc -- for example:

  add_help(:voiced2unvoiced,
           "voiced2unvoiced(amp, fftsize, r, tempo, snd=false, chn=false)  \
Turns a vocal sound into whispering: voiced2unvoiced(1.0, 256, 2.0, 2.0)")
  def voiced2unvoiced(amp, fftsize, r, tempo, snd = false, chn = false)
    freq_inc = fftsize / 2
    fdr = make_vct(fftsize)
    fdi = make_vct(fftsize)
    spectr = make_vct(freq_inc)
    noi = make_rand(:frequency, srate(snd) / 3.0)
    inctr = 0
    ctr = freq_inc
    radius = 1.0 - r.to_f / fftsize
    bin = srate(snd).to_f / fftsize
    len = framples(snd, chn)
    outlen = (len / tempo).floor
    hop = (freq_inc * tempo).floor
    out_data = make_vct([len, outlen].max)
    fmts = make_array(freq_inc) do |i|
      make_formant(i * bin, radius)
    end
    formants = make_formant_bank(fmts, spectr)
    old_peak_amp = new_peak_amp = 0.0
    outlen.times do |i|
      if ctr == freq_inc
        fdr = channel2vct(inctr, fftsize, snd, chn)
        if (pk = vct_peak(fdr)) > old_peak_amp
          old_peak_amp = pk
        end
        spectrum(fdr, fdi, false, 2)
        inctr += hop
        vct_subtract!(fdr, spectr)
        vct_scale!(fdr, 1.0 / freq_inc)
        ctr = 0
      end
      ctr += 1
      vct_add!(spectr, fdr)
      if (outval = formant_bank(formants, rand(noi))).abs > new_peak_amp
        new_peak_amp = outval.abs
      end
      out_data[i] = outval
    end
    vct_scale!(out_data, amp * (old_peak_amp / new_peak_amp))
    vct2channel(out_data, 0, [len, outlen].max, snd, chn, false,
                format("%s(%s, %s, %s, %s", get_func_name,
                       amp, fftsize, r, tempo))
  end

  # very similar but use sum-of-cosines (glottal pulse train?)
  # instead of white noise

  add_help(:pulse_voice,
           "pulse_voice(cosin, freq=440.0, amp=1.0, fftsize=256, r=2.0, \
snd=false, chn=false)  \
Uses sum-of-cosines to manipulate speech sounds.")
  def pulse_voice(cosin, freq = 440.0, amp = 1.0,
                  fftsize = 256, r = 2.0, snd = false, chn = false)
    freq_inc = fftsize / 2
    fdr = make_vct(fftsize)
    fdi = make_vct(fftsize)
    spectr = make_vct(freq_inc)
    pulse = make_sum_of_cosines(cosin, freq)
    inctr = 0
    ctr = freq_inc
    radius = 1.0 - r / fftsize
    bin = srate(snd) / fftsize
    len = framples(snd, chn)
    out_data = make_vct(len)
    old_peak_amp = new_peak_amp = 0.0
    fmts = make_array(freq_inc) do |i|
      make_formant(i * bin, radius)
    end
    formants = make_formant_bank(fmts, spectr)
    out_data.map do |i|
      outval = 0.0
      if ctr == freq_inc
        fdr = channel2vct(inctr, fftsize, snd, chn)
        pk = vct_peak(fdr)
        if pk > old_peak_amp then old_peak_amp = pk end
        spectrum(fdr, fdi, false, 2)
        inctr += freq_inc
        vct_subtract!(fdr, spectr)
        vct_scale!(fdr, 1.0 / freq_inc)
        ctr = 0
      end
      ctr += 1
      vct_add!(spectr, fdr)
      outval = formant_bank(formants, sum_of_cosines(pulse))
      if outval.abs > new_peak_amp then new_peak_amp = outval.abs end
      outval
    end
    vct_scale!(out_data, amp * (old_peak_amp / new_peak_amp))
    vct2channel(out_data, 0, len, snd, chn)
  end
  # pulse_voice(80,   20.0, 1.0, 1024, 0.01)
  # pulse_voice(80,  120.0, 1.0, 1024, 0.2)
  # pulse_voice(30,  240.0, 1.0, 1024, 0.1)
  # pulse_voice(30,  240.0, 1.0, 2048)
  # pulse_voice( 6, 1000.0, 1.0,  512)

  # convolution example

  add_help(:cnvtest,
           "cnvtest(snd0, snd1, amp)  \
Convolves snd0 and snd1, scaling by amp, \
returns new max amp: cnvtest(0, 1, 0.1)")
  def cnvtest(snd0, snd1, amp)
    flt_len = framples(snd0)
    total_len = flt_len + framples(snd1)
    cnv = make_convolve(:filter, channel2vct(0, flt_len, snd0))
    sf = make_sampler(0, snd1, false)
    out_data = make_vct!(total_len) do
      convolve(cnv, lambda do |dir| next_sample(sf) end)
    end
    free_sampler(sf)
    vct_scale!(out_data, amp)
    max_samp = vct_peak(out_data)
    vct2channel(out_data, 0, total_len, snd1)
    if max_samp > 1.0
      set_y_bounds(snd1, [-max_samp, max_samp])
    end
    max_samp
  end

  # swap selection chans

  add_help(:swap_selection_channels,
           "swap_selection_channels()  \
Swaps the currently selected data's channels.")
  def swap_selection_channels
    if (not selection?)
      Snd.raise(:no_active_selection)
    end
    if selection_chans != 2
      Snd.raise(:wrong_number_of_channels, "need a stereo selection")
    end  
    beg = selection_position()
    len = selection_framples()
    snd_chn0 = snd_chn1 = nil
    Snd.sounds.each do |snd|
      channels(snd).times do |chn|
        if selection_member?(snd, chn)
          if snd_chn0.nil?
            snd_chn0 = [snd, chn]
          elsif snd_chn1.nil?
            snd_chn1 = [snd, chn]
            break
          end
        end
      end
    end
    if snd_chn1.nil?
      Snd.raise(:wrong_number_of_channels, "need two channels to swap")
    end
    swap_channels(snd_chn0[0], snd_chn0[1], snd_chn1[0], snd_chn1[1], beg, len)
  end

  # sound interp
  # 
  # make-sound-interp sets up a sound reader that reads a channel at
  # an arbitary location, interpolating between samples if necessary,
  # the corresponding "generator" is sound-interp

  add_help(:make_sound_interp,
           "make_sound_interp(start, snd=false, chn=false)  \
An interpolating reader for SND's channel CHN.")
  def make_sound_interp(start, snd = false, chn = false)
    data = channel2vct(0, false, snd, chn)
    size = data.length
    lambda do |loc|
      array_interp(data, loc, size)
    end
  end

  add_help(:sound_interp,
           "sound_interp(func, loc)  \
Sample at LOC (interpolated if necessary) from FUNC \
created by make_sound_interp.")
  def sound_interp(func, loc)
    func.call(loc)
  end

  def sound_via_sound(snd1, snd2)
    intrp = make_sound_interp(0, snd1, 0)
    len = framples(snd1, 0) - 1
    rd = make_sampler(0, snd2, 0)
    mx = maxamp(snd2, 0)
    map_channel(lambda do |val|
                  sound_interp(intrp,
                               (len * 0.5 * 
                               (1.0 + (read_sample(rd) / mx))).floor)
                end)
  end
  
  # env_sound_interp takes an envelope that goes between 0 and 1
  # (y-axis), and a time-scaler (1.0 = original length) and returns a
  # new version of the data in the specified channel that follows that
  # envelope (that is, when the envelope is 0 we get sample 0, when
  # the envelope is 1 we get the last sample, envelope = 0.5 we get
  # the middle sample of the sound and so on. env_sound_interp([0, 0,
  # 1, 1]) will return a copy of the current sound;
  # env_sound_interp([0, 0, 1, 1, 2, 0], 2.0) will return a new sound
  # with the sound copied first in normal order, then reversed.
  # src_sound with an envelope could be used for this effect, but it
  # is much more direct to apply the envelope to sound sample
  # positions.

  add_help(:env_sound_interp,
           "env_sound_interp(env, time_scale=1.0, snd=false, chn=false)  \
Reads SND's channel CHN according to ENV and TIME_SCALE.")
  def env_sound_interp(envelope, time_scale = 1.0, snd = false, chn = false)
    len = framples(snd, chn)
    newlen = (time_scale.to_f * len).floor
    read_env = make_env(:envelope, envelope, :length, newlen + 1, :scaler, len)
    data = channel2vct(0, false, snd, chn)
    new_snd = Vct.new(newlen) do
      array_interp(data, env(read_env), len)
    end
    set_samples(0, newlen, new_snd, snd, chn, true,
                format("%s(%p, %s", get_func_name, envelope, time_scale),
                0, Current_edit_position, true)
  end
  # env_sound_interp([0, 0, 1, 1, 2, 0], 2.0)

  add_help(:granulated_sound_interp,
           "granulated_sound_interp(env, time_scale=1.0, grain_len=0.1, \
grain_env=[0, 0, 1, 1, 2, 1, 3, 0], out_hop=0.05, snd=false, chn=false)  \
Reads the given channel following ENV (as in env_sound_interp), \
using grains to create the re-tempo'd read.")
  def granulated_sound_interp(envelope,
                              time_scale = 1.0,
                              grain_length = 0.1,
                              grain_envelope = [0, 0, 1, 1, 2, 1, 3, 0],
                              output_hop = 0.05,
                              snd = false,
                              chn = false)
    len = framples(snd, chn)
    newlen = (time_scale.to_f * len).floor
    read_env = make_env(envelope, :length, newlen, :scaler, len)
    sr = srate(snd).to_f
    grain_frames = (grain_length * sr).to_i
    hop_frames = (output_hop * sr).to_i
    num_readers = (grain_length.to_f / output_hop).ceil
    cur_readers = 0
    next_reader = 0
    jitter = sr * 0.005
    readers = Array.new(num_readers, false)
    grain_envs = Array.new(num_readers) do
      make_env(grain_envelope, :length, grain_frames)
    end
    new_snd = Vct.new(newlen, 0.0)
    0.step(newlen, hop_frames) do |i|
      stop = [newlen, hop_frames + i].min
      read_env.location = i
      position_in_original = env(read_env)
      mx = [0, (position_in_original + mus_random(jitter)).round].max
      readers[next_reader] = make_sampler(mx, snd, chn)
      grain_envs[next_reader].reset
      next_reader += 1
      next_reader %= num_readers
      if cur_readers < next_reader
        cur_readers = next_reader
      end
      (0...cur_readers).each do |j|
        en = grain_envs[j]
        rd = readers[j]
        (i...stop).each do |k|
          new_snd[k] = env(en) * rd.call()
        end
      end
    end
    set_samples(0, newlen, new_snd, snd, chn, true,
                format("%s(%p, %s, %s, %p, %s",
                       get_func_name,
                       envelope,
                       time_scale,
                       grain_length,
                       grain_envelope,
                       output_hop), 0, Current_edit_position, true)
  end
  # granulated_sound_interp([0, 0, 1, 0.1, 2, 1], 1.0, 0.2, [0, 0, 1, 1, 2, 0])
  # granulated_sound_interp([0, 0, 1, 1], 2.0)
  # granulated_sound_interp([0, 0, 1, 0.1, 2, 1], 1.0, 0.2, [0, 0, 1, 1, 2, 0],
  #                         0.02)
  
  # filtered-env 

  add_help(:filtered_env,
           "filtered_env(env, snd=false, chn=false)  \
Is a time-varying one-pole filter: when ENV is at 1.0, no filtering, \
as ENV moves to 0.0, low-pass gets more intense; \
amplitude and low-pass amount move together.")
  def filtered_env(en, snd = false, chn = false)
    flt = make_one_pole(1.0, 0.0)
    amp_env = make_env(:envelope, en, :length, framples())
    map_channel(lambda do |val|
                  env_val = env(amp_env)
                  set_mus_xcoeff(flt, 0, env_val)
                  set_mus_ycoeff(flt, 1, env_val - 1.0)
                  one_pole(flt, env_val * val)
                end, 0, false, snd, chn, false,
                format("%s(%s", get_func_name, en.inspect))
  end

  # lisp graph with draggable x axis

  class Mouse
    def initialize
      @down = 0
      @pos = 0.0
      @x1 = 1.0
    end

    def press(snd, chn, button, state, x, y)
      @pos = x / @x1
      @down = @x1
    end

    def drag(snd, chn, button, state, x, y)
      xnew = x / @x1
      @x1 = [1.0, [0.1, @down + (@pos - xnew)].max].min
      graph(make_vct!((100 * @x1).floor) do |i| i * 0.01 end, "ramp", 0.0, @x1)
    end
  end

=begin  
  let(Mouse.new) do |mouse|
    $mouse_drag_hook.add_hook!("Mouse") do |snd, chn, button, state, x, y|
      mouse.drag(snd, chn, button, state, x, y)
    end
    $mouse_press_hook.add_hook!("Mouse") do |snd, chn, button, state, x, y|
      mouse.press(snd, chn, button, state, x, y)
    end
  end
=end

  # pointer focus within Snd
  # 
  # $mouse_enter_graph_hook.add_hook!("focus") do |snd, chn|
  #   focus_widget(channel_widgets(snd, chn)[0])
  # end
  # $mouse_enter_listener_hook.add_hook!("focus") do |widget|
  #   focus_widget(widget)
  # end
  # $mouse_enter_text_hook.add_hook!("focus") do |widget|
  #   focus_widget(widget)
  # end

  # View: Files dialog chooses which sound is displayed
  #
  # by Anders Vinjar

  add_help(:files_popup_buffer,
           "files_popup_buffer(type, position, name)  \
Hides all sounds but the one the mouse touched in the current files list.  \
Use with $mouse_enter_label_hook.
$mouse_enter_label_hook.add_hook!(\"files-popup\") do |type, position, name|
  files_popup_buffer(type, position, name)
end")
  def files_popup_buffer(type, position, name)
    if snd = find_sound(name)
      curr_buffer = Snd.snd
      vals = widget_size(sound_widgets(curr_buffer)[0])
      height = vals[1]
      Snd.sounds.each do |s| hide_widget(sound_widgets(s)[0]) end
      show_widget(sound_widgets(snd)[0])
      set_widget_size(sound_widgets(snd)[0], [widht, height])
      select_sound(snd)
    end
  end

  # remove-clicks

  add_help(:find_click,
           "find_click(loc)  \
Finds the next click starting at LOC.")
  def find_click(loc)
    reader = make_sampler(loc, false, false)
    samp0 = samp1 = samp2 = 0.0
    samps = make_vct(10)
    len = framples()
    samps_ctr = 0
    (loc...len).each do |ctr|
      samp0, samp1, samp2 = samp1, samp2, next_sample(reader)
      samps[samps_ctr] = samp0
      if samps_ctr < 9
        samps_ctr += 1
      else
        samps_ctr = 0
      end
      local_max = [0.1, vct_peak(samps)].max
      if ((samp0 - samp1).abs > local_max) and
          ((samp1 - samp2).abs > local_max) and
          ((samp0 - samp2).abs < (local_max / 2))
        return ctr - 1
      end
    end
    false
  end

  add_help(:remove_clicks,
           "remove_clicks()  \
Tries to find and smooth-over clicks.")
  def remove_clicks
    loc = 0
    while (click = find_click(loc))
      smooth_sound(click - 2, 4)
      loc = click + 2
    end
  end

  # searching examples (zero+, next-peak)

  add_help(:search_for_click,
           "search_for_click()  \
Looks for the next click (for use with C-s).")
  def search_for_click
    samp0 = samp1 = samp2 = 0.0
    samps = Vct.new(10)
    sctr = 0
    lambda do |val|
      samp0, samp1, samp2 = samp1, samp2, val
      samps[sctr] = val
      sctr += 1
      if sctr >= 10 then sctr = 0 end
      local_max = [0.1, samps.peak].max
      if ((samp0 - samp1).abs >= local_max) and
          ((samp1 - samp2).abs >= local_max) and
          ((samp0 - samp2).abs <= (local_max / 2))
        -1
      else
        false
      end
    end
  end

  add_help(:zero_plus,
           "zero_plus()  \
Finds the next positive-going \
zero crossing (if searching forward) (for use with C-s).")
  def zero_plus
    lastn = 0.0
    lambda do |n|
      rtn = lastn < 0.0 and n >= 0.0 and -1
      lastn = n
      rtn
    end
  end

  add_help(:next_peak,
           "next_peak()  \
Finds the next max or min point \
in the time-domain waveform (for use with C-s).")
  def next_peak
    last0 = last1 = false
    lambda do |n|
      rtn = number?(last0) and
        ((last0 < last1 and last1 > n) or (last0 > last1 and last1 < n)) and -1
      last0, last1 = last1, n
      rtn
    end
  end

  add_help(:find_pitch,
           "find_pitch(pitch)  \
Finds the point in the current sound where PITCH (in Hz) \
predominates -- C-s find_pitch(300).  \
In most cases, \
this will be slightly offset from the true beginning of the note.")
  def find_pitch(pitch)
    interpolated_peak_offset = lambda do |la, ca, ra|
      pk = 0.001 + [la, ca, ra].max
      logla = log([la, 0.0000001].max / pk) / log(10)
      logca = log([ca, 0.0000001].max / pk) / log(10)
      logra = log([ra, 0.0000001].max / pk) / log(10)
      0.5 * (logla - logra) / ((logla + logra) - 2 * logca)
    end
    data = make_vct(transform_size)
    data_loc = 0
    lambda do |n|
      data[data_loc] = n
      data_loc += 1
      rtn = false
      if data_loc == transform_size
        data_loc = 0
        if vct_peak(data) > 0.001
          spectr = snd_spectrum(data, Rectangular_window, transform_size)
          pk = 0.0
          pkloc = 0
          (transform_size / 2).times do |i|
            if spectr[i] > pk
              pk = spectr[i]
              pkloc = i
            end
          end
          pit = (pkloc + (pkloc > 0 ?
                 interpolated_peak_offset.call(*spectr[pkloc - 1, 3]) :
                 0.0) * srate()) / transform_size
          if (pitch - pit).abs < srate / (2 * transform_size)
            rtn = -(transform_size / 2)
          end
        end
        vct_fill!(data, 0.0)
      end
      rtn
    end
  end

  # file2vct and a sort of cue-list, I think

  add_help(:file2vct,
           "file2vct(file)  \
Returns a vct with FILE's data.")
  def file2vct(file)
    len = mus_sound_framples(file)
    reader = make_sampler(0, file)
    data = make_vct!(len) do next_sample(reader) end
    free_sampler(reader)
    data
  end

  add_help(:add_notes,
           "add_notes(notes, snd=false, chn=false)  \
Adds (mixes) NOTES which is a list of lists of the form: \
[file, offset=0.0, amp=1.0] starting at the cursor in the \
currently selected channel: \
add_notes([[\"oboe.snd\"], [\"pistol.snd\", 1.0, 2.0]])")
  def add_notes(notes, snd = false, chn = false)
    start = cursor(snd, chn)
    as_one_edit_rb("%s(%s", get_func_name, notes.inspect) do
      (notes or []).each do |note|
        file, offset, amp = note
        beg = start + (srate(snd) * (offset or 0.0)).floor
        if amp and amp != 1.0
          mix_vct(vct_scale!(file2vct(file), amp), beg, snd, chn, false,
                  format("%s(%s", get_func_name, notes.inspect))
        else
          mix(file, beg, 0, snd, chn, false)
        end
      end
    end
  end

  add_help(:region_play_list,
           "region_play_list(data)  \
DATA is list of lists [[time, reg], ...], time in secs, \
setting up a sort of play list: \
region_play_list([[0.0, 0], [0.5, 1], [1.0, 2], [1.0, 0]])")
  def region_play_list(data)
    (data or []).each do |tm, rg|
      tm = (1000.0 * tm).floor
      if region?(rg)
        call_in(tm, lambda do | | play(rg) end)
      end
    end
  end

  add_help(:region_play_sequence,
           "region_play_sequence(data)  \
DATA is list of region ids which will be played one after the other: \
region_play_sequence([0, 2, 1])")
  def region_play_sequence(data)
    time = 0.0
    region_play_list(data.map do |id|
                       cur = time
                       time = time + region_framples(id) / region_srate(id)
                       [cur, id]
                     end)
  end

  # replace-with-selection

  add_help(:replace_with_selection,
           "replace_with_selection()  \
Replaces the samples from the cursor with the current selection.")
  def replace_with_selection
    beg = cursor
    len = selection_framples()
    delete_samples(beg, len)
    insert_selection(beg)
  end

  # explode-sf2

  add_help(:explode_sf2,
           "explode_sf2()  \
Turns the currently selected soundfont file into \
a bunch of files of the form sample-name.aif.")
  def explode_sf2
    (soundfont_info() or []).each do |name, start, loop_start, loop_end|
      filename = name + ".aif"
      if selection?
        set_selection_member?(false, true)
      end
      set_selection_member?(true)
      set_selection_position(start)
      set_selection_framples(framples() - start)
      save_selection(filename, selection_srate(), Mus_bshort, Mus_aifc)
      temp = open_sound(filename)
      set_sound_loop_info([loop_start, loop_end], temp)
      close_sound(temp)
    end
  end

  # open-next-file-in-directory

  class Next_file
    def initialize
      @last_file_opened = ""
      @current_directory = ""
      @current_sorted_files = []
    end

    def open_next_file_in_directory
      unless $open_hook.member?("open-next-file-in-directory")
        $open_hook.add_hook!("open-next-file-in-directory") do |fname|
          self.get_current_directory(fname)
        end
      end
      if @last_file_opened.empty? and sounds
        @last_file_opened = file_name(Snd.snd)
      end
      if @current_directory.empty?
        unless sounds
          get_current_files(Dir.pwd)
        else
          get_current_files(File.split(@last_file_opened).first)
        end
      end
      if @current_sorted_files.empty?
        Snd.raise(:no_such_file)
      else
        next_file = find_next_file
        if find_sound(next_file)
          Snd.raise(:file_already_open, next_file)
        else
          sounds and close_sound(Snd.snd)
          open_sound(next_file)
        end
      end
      true
    end

    private
    def find_next_file
      choose_next = @last_file_opened.empty?
      just_filename = File.basename(@last_file_opened)
      f = callcc do |ret|
        @current_sorted_files.each do |file|
          ret.call(file) if choose_next
          if file == just_filename
            choose_next = true
          end
        end
        @current_sorted_files[0] # wrapped around
      end
      @current_directory + "/" + f
    end

    def get_current_files(dir)
      @current_directory = dir
      @current_sorted_files = sound_files_in_directory(dir).sort
    end

    def get_current_directory(filename)
      Snd.display(@last_file_opened = filename)
      new_path = File.split(mus_expand_filename(filename)).first
      if @current_directory.empty? or @current_directory != new_path
        get_current_files(new_path)
      end
      false
    end
  end

  def click_middle_button_to_open_next_file_in_directory
    nf = Next_file.new
    $mouse_click_hook.add_hook!("next-file") do |s, c, button, st, x, y, ax|
      if button == 2
        nf.open_next_file_in_directory
      end
    end
  end

  # chain-dsps

  def chain_dsps(start, dur, *dsps)
    dsp_chain = dsps.map do |gen|
      if array?(gen)
        make_env(:envelope, gen, :duration, dur)
      else
        gen
      end
    end
    run_instrument(start, dur) do
      val = 0.0
      dsp_chain.each do |gen|
        if env?(gen)
          val *= gen.run
        elsif readin?(gen)
          val += gen.run
        else
          val = gen.run(val)
        end
      end
      val
    end
  end

=begin
  with_sound() do
    chain_dsps(0, 1.0, [0, 0, 1, 1, 2, 0],
               make_oscil(:frequency, 440))
    chain_dsps(0, 1.0, [0, 0, 1, 1, 2, 0],
               make_one_pole(0.5), make_readin("oboe.snd"))
    chain_dsps(0, 1.0, [0, 0, 1, 1, 2, 0],
               let(make_oscil(:frequency, 220),
                   make_oscil(:frequency, 440)) do |osc1, osc2|
                  lambda do |val|
                    osc1.run(val) + osc2.run(2.0 * val)
                  end
               end)
  end
=end

  # re-order channels

  def scramble_channels(*new_order)
    len = new_order.length
    swap_once = lambda do |current, desired, n|
      if n != len
        cur_orig, cur_cur = current[n][0, 2]
        dst = desired[n]
        if cur_orig != dst
          swap_channels(false, cur_cur, false, dst)
          current[dst][0] = cur_orig
        end
        swape_once.call(current, desired, n + 1)
      end
    end
    swap_once.call(make_array(len) do |i| [i, i] end, new_order, 0)
  end

  def scramble_channel(silence)
    buffer = make_moving_average(128)
    silence = silence / 128.0
    edges = []
    in_silence = true
    old_max = max_regions
    old_tags = with_mix_tags
    set_max_regions(1024)
    set_with_mix_tags(false)
    rd = make_sampler()
    framples().times do |i|
      y = next_sample(rd)
      sum_of_squares = moving_average(buffer, y * y)
      now_silent = (sum_of_squares < silence)
      if now_silent != in_silence
        edges.push(i)
      end
      in_silence = now_silent
    end
    edges.push(framples())
    len = edges.length
    start = 0
    pieces = Array.new(len) do |i|
      en = edges[i]
      re = make_region(start, en)
      start = en
      re
    end
    start = 0
    fnc = lambda do | |
      scale_by(0.0)
      len.times do |i|
        this = random(len)
        reg = pieces[this]
        pieces[this] = false
        unless reg
          (this + 1).upto(len - 1) do |j|
            reg = pieces[j]
            if reg
              pieces[j] = false
              break
            end
          end
          unless reg
            (this - 1).downto(0) do |j|
              reg = pieces[j]
              if reg
                pieces[j] = false
                break
              end
            end
          end
        end
        mix_region(reg, start)
        start += framples(reg)
        forget_region(reg)
      end
    end
    as_one_edit(fnc)
    set_max_regions(old_max)
    set_with_mix_tags(old_tags)
  end
  # scramble_channel(0.01)

  # reorder blocks within channel

  add_help(:reverse_by_blocks,
           "reverse_by_blocks(block_len, snd=false, chn=false)  \
Divide sound into block-len blocks, recombine blocks in reverse order.")
  def reverse_by_blocks(block_len, snd = false, chn = false)
    len = framples(snd, chn)
    num_blocks = (len / (srate(snd).to_f * block_len)).floor
    if num_blocks > 1
      actual_block_len = len / num_blocks
      rd = make_sampler(len - actual_block_len, snd, chn)
      beg = 0
      ctr = 1
      map_channel(lambda do |y|
                    val = read_sample(rd)
                    if beg < 10
                      val = val * beg * 0.1
                    else
                      if beg > actual_block_len - 10
                        val = val * (actual_block_len - beg) * 0.1
                      end
                    end
                    beg += 1
                    if beg == actual_block_len
                      ctr += 1
                      beg = 0
                      rd = make_sampler([len - ctr * actual_block_len, 0].max,
                                        snd, chn)
                    end
                    val
                  end, 0, false,
                  snd, chn, false, format("%s(%s", get_func_name, block_len))
    end
  end

  add_help(:reverse_within_blocks,
           "reverse_within_blocks(block_len, snd=false, chn=false)  \
Divide sound into blocks, recombine in order, \
but each block internally reversed.")
  def reverse_within_blocks(block_len, snd = false, chn = false)
    len = framples(snd, chn)
    num_blocks = (len / (srate(snd).to_f * block_len)).floor
    if num_blocks > 1
      actual_block_len = len / num_blocks
      no_clicks_env = [0.0, 0.0, 0.01, 1.0, 0.99, 1.0, 1.0, 0.0]
      as_one_edit(lambda do | |
                    0.step(len, actual_block_len) do |beg|
                      reverse_channel(beg, actual_block_len, snd, chn)
                      env_channel(no_clicks_env, beg, actual_block_len,
                                  snd, chn)
                    end
                  end, format("%s(%s", get_func_name, block_len))
    else
      reverse_channel(0, false, snd, chn)
    end
  end

  def segment_maxamp(name, beg, dur)
    mx = 0.0
    rd = make_sampler(beg, name)
    dur.times do mx = [mx, next_sample(rd).abs].max end
    free_sampler(rd)
    mx
  end

  def segment_sound(name, high, low)
    len = mus_sound_framples(name)
    reader = make_sampler(0, name)
    avg = make_moving_average(:size, 128)
    lavg = make_moving_average(:size, 2048)
    segments = Vct.new(100)
    segctr = 0
    possible_end = 0
    in_sound = false
    len.times do |i|
      samp = next_sample(reader).abs
      val = moving_average(avg, samp)
      lval = moving_average(lavg, samp)
      if in_sound
        if val < low
          possible_end = i
          if lval < low
            segments[segctr] = possible_end + 128
            segctr += 1
            in_sound = false
          end
        else
          if val > high
            segments[segctr] = i - 128
            segctr += 1
            in_sound = true
          end
        end
      end
    end
    free_sampler(reader)
    if in_sound
      segments[segctr] = len
      [segctr + 1, segments]
    else
      [segctr, segments]
    end
  end

  def do_one_directory(fd, dir_name, ins_name, high = 0.01, low = 0.001)
    snd_print("# #{dir_name}")
    sound_files_in_directory(dir_name).each do |sound|
      sound_name = dir_name + "/" + sound
      boundary_data = segment_sound(sound_name, high, low)
      segments, boundaries = boundary_data
      fd.printf("\n\n#    ", sound)
      fd.printf("(%s %s", ins_name, sound_name.inspect)
      0.step(segments, 2) do |bnd|
        segbeg = boundaries[bnd].to_i
        segbeg = boundaries[bnd + 1].to_i
        fd.printf("(%s %s %s)", segbeg, segdur,
                  segment_maxamp(sound_name, segbeg, segdur))
        fd.printf(")")
      end
      mus_sound_forget(sound_name)
    end
  end
  
  def sound2segment_data(main_dir, output_file = "sounds.data")
    File.open(output_file, "w") do |fd|
      old_fam = with_file_monitor
      set_with_file_monitor(false)
      fd.printf("# sound data from %s", main_dir.inspect)
      if main_dir[-1] != "/" then main_dir += "/" end
      Dir[main_dir].each do |dir|
        ins_name = dir.downcase.tr(" ", "")
        fd.printf("\n\n# ---------------- %s ----------------", dir)
        if dir == "Piano"
          Dir[main_dir + dir].each do |inner_dir|
            do_one_directory(fd, main_dir + dir + "/" + inner_dir,
                             ins_name, 0.001, 0.0001)
          end
        else
          do_one_directory(fd, main_dir + dir, ins_name)
        end
      end
      set_with_file_monitor(old_fam)
    end
  end
  # sounds2segment_data(ENV['HOME'] + "/.snd.d/iowa/sounds/", "iowa.data")

  add_help(:channel_clipped?,
           "channel_clipped?(snd=false, chn=false)  \
Returns true and a sample number if it finds clipping.")
  def channel_clipped?(snd = false, chn = false)
    last_y = 0.0
    scan_channel(lambda do |y|
                   result = (y.abs >= 0.9999 and last_y.abs >= 0.9999)
                   last_y = y
                   result
                 end, 0, false, snd, chn)
  end

  # scan-sound
  
  def scan_sound(func, beg = 0, dur = false, snd = false)
    if sound?(index = Snd.snd(snd))
      if (chns = channels(index)) == 1
        scan_channel(lambda do |y| func.call(y, 0) end, beg, dur, index, 0)
      else
        len = framples(index)
        fin = (dur ? [len, beg + dur].min : len)
        readers = make_array(chns) do |chn| make_sampler(beg, index, chn) end
        result = false
        beg.upto(fin) do |i|
          local_result = true
          readers.each_with_index do |rd, chn|
            local_result = (func.call(rd.call, chn) and local_result)
          end
          if local_result
            result = [true, i]
            break
          end
        end
        result
      end
    else
      Snd.raise(:no_such_sound, get_func_name, snd)
    end
  end

  def scan_sound_rb(beg = 0, dur = false, snd = false, &func)
    scan_sound(func, beg, dur, snd)
  end
end

include Examp

module Moog
  class Moog_filter < Musgen
    Gaintable = vct(0.999969, 0.990082, 0.980347, 0.970764, 0.961304, 0.951996,
                    0.94281, 0.933777, 0.924866, 0.916077, 0.90741, 0.898865,
                    0.890442, 0.882141, 0.873962, 0.865906, 0.857941, 0.850067,
                    0.842346, 0.834686, 0.827148, 0.819733, 0.812378, 0.805145,
                    0.798004, 0.790955, 0.783997, 0.77713, 0.770355, 0.763672,
                    0.75708, 0.75058, 0.744141, 0.737793, 0.731537, 0.725342,
                    0.719238, 0.713196, 0.707245, 0.701355, 0.695557, 0.689819,
                    0.684174, 0.678558, 0.673035, 0.667572, 0.66217, 0.65686,
                    0.651581, 0.646393, 0.641235, 0.636169, 0.631134, 0.62619,
                    0.621277, 0.616425, 0.611633, 0.606903, 0.602234, 0.597626,
                    0.593048, 0.588531, 0.584045, 0.579651, 0.575287, 0.570953,
                    0.566681, 0.562469, 0.558289, 0.554169, 0.550079, 0.546051,
                    0.542053, 0.538116, 0.53421, 0.530334, 0.52652, 0.522736,
                    0.518982, 0.515289, 0.511627, 0.507996, 0.504425, 0.500885,
                    0.497375, 0.493896, 0.490448, 0.487061, 0.483704, 0.480377,
                    0.477081, 0.473816, 0.470581, 0.467377, 0.464203, 0.46109,
                    0.457977, 0.454926, 0.451874, 0.448883, 0.445892, 0.442932,
                    0.440033, 0.437134, 0.434265, 0.431427, 0.428619, 0.425842,
                    0.423096, 0.42038, 0.417664, 0.415009, 0.412354, 0.409729,
                    0.407135, 0.404572, 0.402008, 0.399506, 0.397003, 0.394501,
                    0.392059, 0.389618, 0.387207, 0.384827, 0.382477, 0.380127,
                    0.377808, 0.375488, 0.37323, 0.370972, 0.368713, 0.366516,
                    0.364319, 0.362122, 0.359985, 0.357849, 0.355713, 0.353607,
                    0.351532, 0.349457, 0.347412, 0.345398, 0.343384, 0.34137,
                    0.339417, 0.337463, 0.33551, 0.333588, 0.331665, 0.329773,
                    0.327911, 0.32605, 0.324188, 0.322357, 0.320557, 0.318756,
                    0.316986, 0.315216, 0.313446, 0.311707, 0.309998, 0.308289,
                    0.30658, 0.304901, 0.303223, 0.301575, 0.299927, 0.298309,
                    0.296692, 0.295074, 0.293488, 0.291931, 0.290375, 0.288818,
                    0.287262, 0.285736, 0.284241, 0.282715, 0.28125, 0.279755,
                    0.27829, 0.276825, 0.275391, 0.273956, 0.272552, 0.271118,
                    0.269745, 0.268341, 0.266968, 0.265594, 0.264252, 0.262909,
                    0.261566, 0.260223, 0.258911, 0.257599, 0.256317, 0.255035,
                    0.25375)
    Freqtable = [0, -1,
      0.03311111, -0.9,
      0.06457143, -0.8,
      0.0960272, -0.7,
      0.127483, -0.6,
      0.1605941, -0.5,
      0.1920544, -0.4,
      0.22682086, -0.3,
      0.2615873, -0.2,
      0.29801363, -0.1,
      0.33278003, -0.0,
      0.37086168, 0.1,
      0.40893877, 0.2,
      0.4536417, 0.3,
      0.5, 0.4,
      0.5463583, 0.5,
      0.5943719, 0.6,
      0.6556281, 0.7,
      0.72185487, 0.8,
      0.8096009, 0.9,
      0.87913835, 0.95,
      0.9933787, 1,
      1, 1]

    def initialize(freq, q)
      super()
      @frequency = freq
      @Q = q
      @state = make_vct(4)
      @A = 0.0
      @freqtable = envelope_interp(freq / (srate() * 0.5), Freqtable)
    end
    attr_reader :frequency, :state, :freqtable, :A
    attr_accessor :Q

    def inspect
      format("%s.new(%s, %s)", self.class, @frequency, @Q)
    end

    def to_s
      format("#<%s freq: %1.3f, Q: %s>", self.class, @frequency, @Q)
    end

    def run_func(val1 = 0.0, val2 = 0.0)
      filter(val1)
    end

    def frequency=(freq)
      @freqtable = envelope_interp(freq / (srate() * 0.5), Freqtable)
      @frequency = freq
    end
    
    def filter(insig)
      a = 0.25 * (insig - @A)
      @state.map! do |st|
        new_a = saturate(a + @freqtable * (a - st))
        a = saturate(new_a + st)
        new_a
      end
      ix = @freqtable * 99.0
      ixint = ix.floor
      ixfrac = ix - ixint
      @A = a * @Q * 
           ((1.0 - ixfrac) * Gaintable[ixint + 99] + 
            ixfrac * Gaintable[ixint + 100])
      a
    end

    private
    def saturate(x)
      [[x, -0.95].max, 0.95].min
    end
  end

  add_help(:make_moog_filter,
           "make_moog_filter(freq=440.0, Q=0)  \
Makes a new moog_filter generator. \
FREQUENCY is the cutoff in Hz, \
Q sets the resonance: 0 = no resonance, 1: oscillates at FREQUENCY.")
  def make_moog_filter(freq = 440.0, q = 0)
    Moog_filter.new(freq, q)
  end

  add_help(:moog_filter,
           "moog_filter(moog, insig=0.0)  \
Is the generator associated with make_moog_filter.")
  def moog_filter(mg, insig = 0.0)
    mg.filter(insig)
  end

  def moog(freq, q)
    mg = Moog_filter.new(freq, q)
    lambda do |inval| mg.filter(inval) end
  end
end

include Moog

# examp.rb ends here
