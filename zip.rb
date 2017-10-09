# zip.rb -- zip.scm -> zip.rb -*- snd-ruby -*-

# Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: Sun Apr 24 22:53:42 CEST 2005
# Changed: Sat Sep 26 02:08:01 CEST 2009

# Commentary:

# create the 'digital zipper' effect
# a not-very-debonair way to fade out file1 and fade in file2
# this is also good if the same file is used twice -- sort of like a
# CD player gone berserk
#
# safe_srate
#
# class Zipper
#  initialize(ramp_env, frame_size = 0.05, frame_env = false)
#  zipper(input1, input2)
#
# make_zipper(ramp_env, frame_size = 0.05, frame_env = false)
# zipper(zp, input1, input2)
# zip_sound(start, dur, file1, file2, ramp = [0, 0, 1, 1], size = 0.05)

# Code:

def safe_srate
  (sounds and srate()) or mus_srate()
end

class Zipper
  def initialize(ramp_env, frame_size = 0.05, frame_env = false)
    max_size = 1 + (safe_srate * frame_size).ceil
    @low_start = 20
    @frame_loc = 0
    @cursamples = 0
    @frame0 = Vct.new(max_size)
    @frame1 = Vct.new(max_size)
    @frame2 = Vct.new(max_size)
    @fe = (frame_env or make_env([0, safe_srate * 0.05], :length, ramp_env.length))
    @rampe = ramp_env
  end

  def zipper(input1, input2)
    ramp_loc = env(@rampe)
    frame_samples = env(@fe).floor
    if (chunk_len = (frame_samples.to_f * ramp_loc).round) <= @low_start
      @frame_loc = 0
      input1.call
    elsif chunk_len >= frame_samples - @low_start
      @frame_loc = 0
      input2.call
    else
      if @frame_loc >= @cursamples
        @frame_loc = 0
        @cursamples = frame_samples
        frame_samples.times do |i|
          @frame1[i] = input1.call
          @frame2[i] = input2.call
        end
        @frame0.fill(0.0)
        start_ctr = 0.0
        samp2 = frame_samples.to_f / chunk_len
        chunk_len.times do |i|
          ictr = start_ctr.floor
          y0 = @frame2[ictr]
          y1 = @frame2[ictr + 1]
          @frame0[i] = y0 + (y1 - y0) * (start_ctr - ictr)
          start_ctr += samp2
        end
        start_ctr = 0
        samp1 = frame_samples.to_f / (frame_samples.to_f - chunk_len)
        chunk_len.upto(frame_samples - 1) do |i|
          ictr = start_ctr.floor
          y0 = @frame1[ictr]
          y1 = @frame1[ictr + 1]
          @frame0[i] = y0 + (y1 - y0) * (start_ctr - ictr)
          start_ctr += samp1
        end
      end
      result = @frame0[@frame_loc]
      @frame_loc += 1
      result
    end
  end
end

add_help(:make_zipper,
         "make_zipper(ramp_env, [frame_size=0.05, [frame_env=false]])   \
makes a zipper generator.  'ramp_env' is an envelope (normally a ramp from 0 to 1) \
which sets where we are in the zipping process, \
'frame_size' is the maximum frame length during the zip in seconds (defaults to 0.05), \
and 'frame_env' is an envelope returning the current frame size during the zip process.")
def make_zipper(ramp_env, frame_size = 0.05, frame_env = false)
  Zipper.new(ramp_env, frame_size, frame_env)
end

add_help(:zipper,
         "zipper(zip, in1, in2)  \
creates the digital zipper sound effect using zipper generator 'zip' \
and the two samplers 'in1' and 'in2'")
def zipper(zp, input1, input2)
  zp.zipper(input1, input2)
end

add_help(:zip_sound,
         "zip_sound(start, dur, file1, file2, [ramp_env=[0, 0, 1, 1], [size=0.05]])  \
zips the two files and mixes the result into the current sound")
def zip_sound(start, dur, file1, file2, ramp = [0, 0, 1, 1], size = 0.05)
  beg = seconds2samples(start)
  len = seconds2samples(dur)
  zip = make_zipper(make_env(:envelope, ramp, :length, len),
                    size,
                    make_env(:envelope, [0, safe_srate * size], :length, len))
  read0 = make_sampler(0, file1)
  read1 = make_sampler(0, file2)
  map_channel(lambda do |y| y + zipper(zip, read0, read1) end, beg, len)
end
# zip_sound(0, 1, "fyow.snd", "now.snd", [0, 0, 1, 1], 0.05)
# zip_sound(0, 3, "mb.snd", "fyow.snd", [0, 0, 1, 0, 1.5, 1, 3, 1], 0.025)

# zip.rb ends here
