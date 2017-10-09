# play.rb -- play.scm -> play.rb

# Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 05/04/22 23:36:39
# Changed: 14/11/14 08:21:06

# playing-related examples
#
# play_often(n)
# play_region_forever(reg)
# start_dac(srate, chans)
# stop_dac
# play_with_amps(snd, *rest)
# play_sine(freq, amp)
# play_sines(freq_and_amps)

# play sound n times -- play_often(3) for example

add_help(:play_often,
         "play_often(n)  \
Plays the selected sound N times (interruptible via C-g).")
def play_often(n)
  plays = n - 1
  play_once = lambda do |reason|
    if plays > 0 and reason == 0
      plays -= 1
      play(selected_sound(), :start, 0, :stop, play_once)
    end
  end
  play(selected_sound(), :start, 0, :stop, play_once)
end
# bind_key(?p, 0, lambda do |n| play_often([1, n].max) end, false, "play often")

# play region over and over until C-g typed

add_help(:play_region_forever,
         "play_region_forever(reg)  \
Plays region REG until you interrupt it via C-g.")
def play_region_forever(reg)
  if integer?(reg)
    reg = integer2region(reg)
  end
  play_region_again = lambda do |reason|
    if reason == 0
      play(reg, :wait, false, :stop, play_region_again)
    end
  end
  play(reg, :wait, false, :stop, play_region_again)
end
# bind_key(?p, 0,
#          lambda do |n| play_region_forever(Snd.regions[[0, n].max]) end,
#          false, "play region forever")

# play while looping continuously between two movable marks

# hold DAC open and play sounds via keyboard

add_help(:start_dac,
         "start_dac(srate=44100, chans=1)  \
Starts the DAC running continuously in the background.")
def start_dac(sr = 44100, chans = 1)
  play(false, :srate, sr, :channels, chans)
end
alias stop_dac stop_playing

# play_with_amps -- play channels with individually settable amps

add_help(:play_with_amps,
         "play_with_amps(snd, *amps)  \
Plays snd with each channel scaled by the corresponding amp: \
play_with_amps(0, 1.0, 0.5) plays channel 2 of stereo sound at half amplitude.")
def play_with_amps(snd, *amps)
  chns = channels(snd)
  chns.times do |chn|
    player = make_player(snd, chn)
    set_amp_control(amps[chn], player)
    add_player(player)
  end
  start_playing(chns, srate(snd))
end

# play_sine and play_sines

add_help(:play_sine,
         "play_sine(freq, amp)  \
Plays a 1 second sinewave at freq and amp.")
def play_sine(freq, amp)
  len = 44100
  osc = make_oscil(freq)
  play(lambda do
         (len -= 1).positive? and (amp * oscil(osc))
       end)
end

add_help(:play_sines,
         "play_sines(*freq_and_amps)  \
Produces a tone given its spectrum: play_sines([[440, 0.4], [660, 0.3]])")
def play_sines(*freq_and_amps)
  len = 44100
  num_oscs = freq_and_amps.length
  frqs = Vct.new(num_oscs)
  amps = Vct.new(num_oscs)
  freq_and_amps.each_with_index do |fa, i|
    frqs[i] = hz2radians(fa[0])
    amps[i] = fa[1]
  end
  ob = make_oscil_bank(frqs, Vct.new(num_oscs, 0.0), amps)
  play(lambda do
         (len -= 1).positive? and oscil_bank(ob)
       end)
end
# play_sines([425, 0.05], [450, 0.01],
#            [470, 0.01], [546, 0.02],
#            [667, 0.01], [789, 0.034],
#            [910, 0.032])

# play.rb ends here
