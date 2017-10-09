# freeverb.rb -- CLM -> Snd/Ruby translation of freeverb.ins

# Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 03/04/08 03:53:20
# Changed: 14/11/13 15:47:00

# Original notes of Fernando Lopez-Lezcano

# ;; Freeverb - Free, studio-quality reverb SOURCE CODE in the public domain
# ;;
# ;; Written by Jezar at Dreampoint, June 2000
# ;; http://www.dreampoint.co.uk
# ;;
# ;; Translated into clm-2 by Fernando Lopez-Lezcano <nando@ccrma.stanford.edu>
# ;; Version 1.0 for clm-2 released in January 2001
# ;; http://ccrma.stanford.edu/~nando/clm/freeverb/
# ;;
# ;; Changes to the original code by Jezar (by Fernando Lopez-Lezcano):
# ;; - the clm version can now work with a mono input or an n-channel input
# ;;   stream (in the latter case the number of channels of the input and output
# ;;   streams must match.
# ;; - the "wet" parameter has been eliminated as it does not apply to the model
# ;;   that clm uses to generate reverberation
# ;; - the "width" parameter name has been changed to :global. It now controls
# ;;   the coefficients of an NxN matrix that specifies how the output of the
# ;;   reverbs is mixed into the output stream.
# ;; - predelays for the input channels have been added.
# ;; - damping can be controlled individually for each channel. 

# For more information see clm-x/freeverb.html.

require "ws"

# Snd-Ruby's freeverb and fcomb (see sndins.so for a faster one).

unless provided? :sndins
  class Fcomb
    def initialize(scaler, size, a0, a1)
      @feedback = scaler.to_f
      @delay = make_delay(size.to_i)
      @filter = make_one_zero(a0, a1)
    end
    attr_accessor :feedback

    def fcomb(input = 0.0)
      delay(@delay, input + one_zero(@filter, tap(@delay)) * @feedback)
    end

    def inspect
      format("#<%s: %p, %p, feedback: %0.3f>",
             self.class, @delay, @filter, @feedback)
    end
  end
  
  def make_fcomb(scaler = 0.0, size = 1, a0 = 0.0, a1 = 0.0)
    Fcomb.new(scaler, size, a0, a1)
  end

  def fcomb(gen, input = 0.0)
    gen.fcomb(input)
  end

  def fcomb?(obj)
    obj.kind_of?(Fcomb)
  end
end

add_help(:freeverb,
         "freeverb(*args)
        :room_decay,        0.5,
        :damping,           0.5,
        :global,            0.3,
        :predelay,          0.03,
        :output_gain,       1.0,
        :output_mixer,      nil,
        :scale_room_decay,  0.28,
        :offset_room_decay, 0.7,
        :combtuning,        [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617],
        :allpasstuning,     [556, 441, 341, 225],
        :scale_damping,     0.4,
        :stereo_spread,     23,
with_sound(:reverb, :freeverb) do fm_violin(0, 1, 440, 0.3) end
This is the Ruby version of freeverb.  For a faster one see sndins.so.")
def freeverb(*args)
  room_decay, damping, global, predelay, output_gain, output_mixer = nil
  scale_room_decay, offset_room_decay, combtuning, allpasstuning = nil
  scale_damping, stereo_spread = nil
  optkey(args, binding,
         [:room_decay, 0.5],
         [:damping, 0.5],
         [:global, 0.3],
         [:predelay, 0.03],
         [:output_gain, 1.0],
         [:output_mixer, nil],
         [:scale_room_decay, 0.28],
         [:offset_room_decay, 0.7],
         [:combtuning, [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617]],
         [:allpasstuning, [556, 441, 341, 225]],
         [:scale_damping, 0.4],
         [:stereo_spread, 23])
  if @reverb_channels > 1 and @reverb_channels != @channels
    error("input must be mono or input channels must equal output channels")
  end
  out_gain = output_gain
  local_gain = (1.0 - global) * (1.0 - 1.0 / @channels) + 1.0 / @channels
  global_gain = (@channels - local_gain * @channels) /
                [(@channels * @channels - @channels), 1].max
  srate_scale = @srate / 44100.0
  out_mix = output_mixer
  unless vct?(output_mixer)
    out_mix = Vct.new(@channels) do
       (out_gain * global_gain) / @channels
    end
  end
  predelays = make_array(@reverb_channels) do
    make_delay(:size, (@srate * predelay).to_i)
  end
  room_decay_val = room_decay * scale_room_decay + offset_room_decay
  combs = make_array(@channels) do |c|
    combtuning.map do |tuning|
      dmp = scale_damping * damping
      sz = (srate_scale * tuning).to_i
      if c.odd?
        sz += (srate_scale * stereo_spread).to_i
      end
      make_fcomb(room_decay_val, sz, 1.0 - dmp, dmp)
    end
  end
  allpasses = make_array(@channels) do |c|
    allpasstuning.map do |tuning|
      sz = (srate_scale * tuning).to_i
      if c.odd?
        sz += (srate_scale * stereo_spread).to_i
      end
      make_all_pass(:size, sz, :feedforward, -1.0, :feedback, 0.5)
    end
  end
  len = @ws_reverb.length + seconds2samples(@decay_time)
  name = get_func_name()
  # to satisfy with_sound-option :info and :notehook
  with_sound_info(name, 0, samples2seconds(len))
  if @verbose
    Snd.message("%s on %d in and %d out channels",
                name, @reverb_channels, @channels)
  end
  f_in = Vct.new(@reverb_channels, 0.0)
  f_out = Vct.new(@channels, 0.0)
  out_buf = Vct.new(@channels, 0.0)
  if @reverb_channels == 1
    len.times do |i|
      fin = delay(predelays[0], file2sample(@ws_reverb, i, 0))
      combs.each_with_index do |fcbs, c|
        f_out[c] = 0.0
        fcbs.each do |fcb|
          f_out[c] += fcomb(fcb, fin)
        end
      end
      allpasses.each_with_index do |apss, c|
        apss.each do |aps|
          f_out[c] = all_pass(aps, f_out[c])
        end
      end
      frample2file(@ws_output, i,
                   frample2frample(out_mix, f_out, @channels,
                                   out_buf, @channels))
    end
  else
    len.times do |i|
      fin = file2frample(@ws_reverb, i, f_in).map_with_index do |f, c|
        delay(predelays[c], f)
      end
      combs.each_with_index do |fcbs, c|
        f_out[c] = 0.0
        fcbs.each do |fcb|
          f_out[c] += fcomb(fcb, fin[c])
        end
      end
      allpasses.each_with_index do |apss, c|
        apss.each do |aps|
          f_out[c] = all_pass(aps, f_out[c])
        end
      end
      frample2file(@ws_output, i,
                   frample2frample(out_mix, f_out, @channels,
                                   out_buf, @channels))
    end
  end
end

=begin
with_sound(:reverb, :freeverb,
           :reverb_data, [:room_decay, 0.9],
           :channels, 2,
           :reverb_channels, 1,
           :output, "fvrb-test.snd",
           :play, 1,
           :statistics, true) do
  fm_violin(0, 1, 440, 0.5)
end
with_sound(:statistics, true,
           :reverb, :freeverb,
           :reverb_data, [:output_gain, 3.0]) do
  outa(0, 0.5, @ws_reverb)
end
with_sound(:channels, 2,
           :reverb_channels, 2,
           :statistics, true,
           :reverb, :freeverb,
           :reverb_data, [:output_gain, 3.0]) do
  outa(0, 0.5, @ws_reverb)
  outb(0, 0.1, @ws_reverb)
end
=end

# freeverb.rb ends here
