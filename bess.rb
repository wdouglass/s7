# bess.rb -- examples from clm-2/bess.cl and clm-2/bess1.cl
#
# Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 2002/09/15 22:20:25
# Changed: 2018/01/11 19:58:32
#
# snd-ruby-xm -batch bess.rb -e fm_forever
# or
# snd-ruby-xm bess.rb

require "snd-xm"

class FM_Forever
  FMname = "FM Forever!"
  FMinfo = "\
bess.rb provides mono real time output to DAC.
Frequency, amplitude, FM index, and C/M ratio
can be controlled via sliders."

  def initialize
    @freq = 220.0
    @amp = 0.5
    @fm_index1 = 1.0
    @fm_index2 = 0.0
    @fm_index3 = 0.0
    @cm_ratio1 = 1
    @cm_ratio2 = 1
    @cm_ratio3 = 1
    @low_freq = 40.0
    @high_freq = 2000.0
    @high_ratio = 10
    @high_index1 = 3.0
    @high_index2 = 1.0
    @high_index3 = 0.25
  end

  def start_dac(&body)
    set_playing(false)
    freq = @freq
    amp = @amp
    index1 = @fm_index1
    ratio1 = @cm_ratio1
    index2 = @fm_index2
    ratio2 = @cm_ratio2
    index3 = @fm_index3
    ratio3 = @cm_ratio3
    s1, s2, s3, s4, s5, s6, s7, s8 = false
    reset_cb = lambda do |w, c, i|
      set_scale_value(s1.scale, @freq = freq, 100.0)
      set_scale_value(s2.scale, @amp = amp, 100.0)
      set_scale_value(s3.scale, @fm_index1 = index1, 100.0)
      set_scale_value(s4.scale, @cm_ratio1 = ratio1, 100.0)
      if s5
        set_scale_value(s5.scale, @fm_index2 = index2, 100.0)
        set_scale_value(s6.scale, @cm_ratio2 = ratio2, 100.0)
        set_scale_value(s7.scale, @fm_index3 = index3, 100.0)
        set_scale_value(s8.scale, @cm_ratio3 = ratio3, 100.0)
      end
    end
    help_cb = lambda do |w, c, i| info_dialog(FMname, FMinfo) end
    #
    # -batch sets XtSetMappedWhenManaged(shell, 0)
    # The "Quit Snd" button is only realized if -batch was given.
    #
    if get_xtvalue(main_widgets[Top_level_shell], RXmNmappedWhenManaged)
      clear_cb = false
    else
      clear_cb = lambda do |w, c, i| exit(0) end
    end
    d = make_dialog(FMname,
                    :reset_cb, reset_cb,
                    :clear_cb, clear_cb,
                    :help_cb, help_cb) do |w, c, i|
      if set_playing(!playing)
        play(body)
        d.doit_string("Pause")
      else
        d.doit_string("Play")
      end
    end
    name = "carrier frequency"
    s1 = d.add_slider(name, @low_freq, freq, @high_freq, 100) do |w, c, i|
      @freq = get_scale_value(w, i, 100.0)
    end
    name = "amplitude"
    s2 = d.add_slider(name, 0, amp, 1, 100) do |w, c, i|
      @amp = get_scale_value(w, i, 100.0)
    end
    name = (@fm_index2 ? "1. " : "") + "fm index"
    s3 = d.add_slider(name, 0, index1, @high_index1, 100) do |w, c, i|
      @fm_index1 = get_scale_value(w, i, 100.0)
    end
    name = (@fm_index2 ? "1. " : "") + "c/m ratio"
    s4 = d.add_slider(name, 0, ratio1, @high_ratio, 100) do |w, c, i|
      @cm_ratio1 = get_scale_value(w, i, 100.0)
    end
    if @fm_index2
      name = "2. fm index"
      s5 = d.add_slider(name, 0, index2, @high_index2, 100) do |w, c, i|
        @fm_index2 = get_scale_value(w, i, 100.0)
      end
      name = "2. c/m ratio"
      s6 = d.add_slider(name, 0, ratio2, @high_ratio, 100) do |w, c, i|
        @cm_ratio2 = get_scale_value(w, i, 100.0)
      end
      name = "3. fm index"
      s7 = d.add_slider(name, 0, index3, @high_index3, 100) do |w, c, i|
        @fm_index3 = get_scale_value(w, i, 100.0)
      end
      name = "3. c/m ratio"
      s8 = d.add_slider(name, 0, ratio3, @high_ratio, 100) do |w, c, i|
        @cm_ratio3 = get_scale_value(w, i, 100.0)
      end
    end
    if clear_cb
      d.clear_string("Quit Snd")
    end
    d.doit_string("Play")
    set_sensitive(d.okay_button, true)
    RXtManageChild(d.dialog)
  end

  def make_fm_forever
    @fm_index2 = false
    @fm_index3 = false
    @car = make_oscil(0.0)
    @mod = make_oscil(0.0)
  end

  def fm_forever
    @amp *
      oscil(@car, hz2radians(@freq) +
        @fm_index1 * oscil(@mod, hz2radians(@cm_ratio1 * @freq)))
  end

  def make_ffm_forever
    @fm_index2 = 0.0
    @fm_index3 = 0.0
    @car = make_oscil(0.0)
    @md1 = make_oscil(0.0)
    @md2 = make_oscil(0.0)
    @md3 = make_oscil(0.0)
  end

  def ffm_forever
    @amp *
      oscil(@car, hz2radians(@freq) +
        @fm_index1 * oscil(@md1, hz2radians(@cm_ratio1 * @freq)) +
        @fm_index2 * oscil(@md2, hz2radians(@cm_ratio2 * @freq)) +
        @fm_index3 * oscil(@md3, hz2radians(@cm_ratio3 * @freq)))
  end
end

# test functions

def fm_forever
  f = FM_Forever.new
  f.make_fm_forever
  f.start_dac do f.fm_forever end
end

def ffm_forever
  f = FM_Forever.new
  f.make_ffm_forever
  f.start_dac do f.ffm_forever end
end

# fm_forever
# ffm_forever

# bess.rb ends here
