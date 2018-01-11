# bess1.rb -- examples from clm-2/rt.lisp and clm-2/bess5.cl

# Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 2002/09/15 19:11:12
# Changed: 2018/01/11 20:05:09
#
# snd-ruby-xm -batch bess1.rb -e agn
# or
# snd-ruby-xm bess1.rb

require "snd-xm"
require "v"
require "env"

class FM_Forever2
  FM2name = "FM Forever!"
  FM2info = "\
bess1.rb provides mono real time output to DAC.
Frequency, amplitude, FM index, and tempo
can be controlled via slider."

  def initialize
    @tempo = 0.1
    @freq = 1.0
    @amp = 1.0
    @index = 1.0
    @low_tempo = 0.05
    @high_tempo = 0.5
    @low_freq = 0.1
    @high_freq = 4.0
    @high_index = 2.0
  end
  
  def start_dac(&body)
    set_playing(false)
    freq = @freq
    amp = @amp
    tempo = @tempo
    index = @index
    s1, s2, s3, s4 = false
    reset_cb = lambda do |w, c, i|
      set_scale_value(s1.scale, @freq = freq, 100.0)
      set_scale_value(s2.scale, @amp = amp, 100.0)
      set_scale_value(s3.scale, @tempo = tempo, 100.0)
      set_scale_value(s4.scale, @index = index, 100.0)
    end
    help_cb = lambda do |w, c, i| info_dialog(FM2name, FM2info) end
    #
    # -batch sets XtSetMappedWhenManaged(shell, 0)
    # The "Quit Snd" button is only realized if -batch was given.
    #
    if get_xtvalue(main_widgets[Top_level_shell], RXmNmappedWhenManaged)
      clear_cb = false
    else
      clear_cb = lambda do |w, c, i| exit(0) end
    end
    d = make_dialog(FM2name,
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
    name = "carrier freqency"
    s1 = d.add_slider(name, @low_freq, freq, @high_freq, 100) do |w, c, i|
      @freq = get_scale_value(w, i, 100.0)
    end
    name = "amplitude"
    s2 = d.add_slider(name, 0, amp, 1, 100) do |w, c, i|
      @amp = get_scale_value(w, i, 100.0)
    end
    name = "tempo"
    s3 = d.add_slider(name, @low_tempo, tempo, @high_tempo, 100) do |w, c, i|
      @tempo = get_scale_value(w, i, 100.0)
    end
    name = "fm index"
    s4 = d.add_slider(name, 0, index, @high_index, 100) do |w, c, i|
      @index = get_scale_value(w, i, 100.0)
    end
    if clear_cb
      d.clear_string("Quit Snd")
    end
    d.doit_string("Play")
    set_sensitive(d.okay_button, true)
    RXtManageChild(d.dialog)
  end

  # see clm-2/bess5.cl
  def make_agn
    lim = 256
    mode = [0, 0, 2, 4, 11, 11, 5, 6, 7, 9, 2, 0, 0]
    @octs = Array.new(lim + 1) do |i|
      4 + 2 * rbell(rbm_random(1.0)).floor
    end
    @pits = Array.new(lim + 1) do |i|
      mode[(12.0 * rbm_random(1.0)).floor]
    end
    @rhys = Array.new(lim + 1) do |i|
      4 + 6 * rbm_random(1.0).floor
    end
    @begs = Array.new(lim + 1) do |i|
      if rbm_random(1.0) < 0.9
        4 + 2 * rbm_random(1.0).floor
      else
        6 * rbm_random(4.0).floor
      end
    end
    @amps = Array.new(lim + 1) do |i|
      1 + 8 * rbell(rbm_random(1.0)).floor
    end
    @cellctr = 0
    @cellsiz = 1
    @cellbeg = 0
    @whichway = 1
    @beg = 0.0
    @len = 0
    @v = lambda do |y| 0.0 end
  end

  def agn
    if @len > 1
      @len -= 1
    else
      bg = @beg
      @beg += [0.025, @tempo * (0.95 + rbm_random(0.1)) * @begs[@cellctr]].max
      dur = [0.025, @tempo * (0.85 + rbm_random(0.1)) * @rhys[@cellctr]].max
      freq = @freq * 16.351 * tune(@pits[@cellctr]) * 2.0 ** @octs[@cellctr]
      amp = @amp * [0.003, @amps[@cellctr] * 0.01].max
      ind = @index * rbm_random(1.0) * 3.0
      @len = seconds2samples(dur)
      @v = make_fm_violin(bg, dur, freq, amp, :fm_index, ind)
      @cellctr += 1
      if @cellctr > (@cellsiz + @cellbeg)
        @cellbeg += 1
        if rbm_random(1.0) > 0.5
          @cellsiz += @whichway
        end
        if @cellsiz > 10 and rbm_random(1.0) > 0.99
          @whichway = -2
        elsif @cellsiz > 6 and rbm_random(1.0) > 0.999
          @whichway = -1
        elsif @cellsiz < 4
          @whichway = 1
        end
        @beg += rbm_random(1.0)
        @cellctr = @cellbeg
      end
    end
    @v.call(0)
  end

  private
  def rbm_random(r)
    mus_random(r).abs
  end
    
  def tune(r)
    [1.0, 256.0 / 243, 9.0 / 8, 32.0 / 27, 81.0 / 64,
     4.0 / 3, 1024.0 / 729, 3.0 / 2, 128.0 / 81, 27.0 / 16,
     16.0 / 9, 243.0 / 128, 2.0].at(r % 12) * 2.0 ** r.divmod(12).first
  end
  
  def rbell(x)
    envelope_interp(x * 100.0, [0, 0.0, 10, 0.25, 90, 1.0, 100, 1.0])
  end
end

def agn
  f = FM_Forever2.new
  f.make_agn
  f.start_dac do f.agn end
end

# see clm-2/rt.lisp
def rt_test(time = 30)
  lim = 256
  mode = [0, 12, 2, 4, 14, 4, 5, 5, 0, 7, 7, 11, 11]
  pits = Array.new(lim + 1) do
    mus_random(12.0).abs.floor
  end
  begs = Array.new(lim + 1) do
    1 + mus_random(3.0).abs.floor
  end
  with_dac do
    cellbeg = 0
    cellsiz = 6
    cellctr = 0
    mytempo = 0.3
    beg = 0.0
    while beg < time and cellctr < lim
      dur = mytempo * begs[cellctr + 1]
      beg += dur
      frq = 16.351 * 16.0 * 2.0 ** (mode[pits[cellctr]] / 12.0)
      cellctr += 1
      if cellctr > (cellsiz + cellbeg)
        cellbeg += 1 if mus_random(1.0).abs > 0.5
        cellsiz += 1 if mus_random(1.0).abs > 0.5
        cellctr = cellbeg
      end
      fm_violin(beg, dur, frq, 0.25)
    end
  end
end

# agn
# rt_test

# bess1.rb ends here
