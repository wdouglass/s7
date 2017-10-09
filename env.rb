# env.rb -- snd/env.scm

# Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 03/09/20 23:24:17
# Changed: 14/11/13 04:52:42

# module Env (see env.scm)
#  envelope_interp(x, en, base)
#  window_envelope(beg, dur, en)
#  map_envelopes(en1, en2, &func)
#  multiply_envelopes(en1, en2)
#  add_envelopes(en1, en2)
#  max_envelope(en)
#  min_envelope(en)
#  integrate_envelope(en)
#  envelope_last_x(en)
#  stretch_envelope(fn, old_att, new_att, old_dec, new_dec)
#  scale_envelope(en, scale, offset)
#  reverse_envelope(en)              alias envelope_reverse
#  concatenate_envelopes(*envs)      alias envelope_concatenate
#  repeat_envelope(ur_env, repeats, reflect, x_normalized) alias envelope_repeat
#
#  class Power_env
#   initialize(*rest)
#   power_env
#   power_env_channel(beg, dur, snd, chn, edpos, edname)
#   
#  make_power_env(*rest)
#  power_env(pe)
#  power_env_channel(pe, beg, dur, snd, chn, edpos) 
#  powenv_channel(envelope, beg, dur, snd, chn, edpos)
#  
#  envelope_exp(en, power, xgrid)
#  rms_envelope(file, *rest)
#  
#  envelope_length(en)
#  normalize_envelope(en, new_max)
#  x_norm(en, xmax)
#
#  exp_envelope(env, *args)
#    [by Fernando Lopez-Lezcano (nando@ccrma.stanford.edu)]
#    db_envelope(env, cutoff, error)
#    make_db_env(env, *args)
#    semitones_envelope(env, around, error)
#    make_semitones_env(env, *args)
#    octaves_envelope(env, around, error)
#    make_octaves_env(env, *args)

require "clm"

module Env
  add_help(:envelope_interp,
           "envelope_interp(*args)
envelope_interp(x, env, base = 1.0) ==> value of env at x;  \
BASE controls connecting segment type: \
envelope_interp(0.3, [0, 0, 0.5, 1, 1, 0]) ==> 0.6")
  def envelope_interp(x, en, base = 1.0)
    unless en.empty?
      en.map! do |y|
        y.to_f
      end
    end
    if en.empty?
      0.0
    else
      if x <= en[0] or en[2..-1].empty?
        en[1]
      else
        if en[2] > x
          if en[1] == en[3] or base.zero?
            en[1]
          else
            if base == 1.0
              en[1] +
                (x - en[0]) *
                ((en[3] - en[1]) / (en[2] - en[0]))
            else
              en[1] +
                ((en[3] - en[1]) / (base - 1.0)) *
                ((base ** ((x - en[0]) / (en[2] - en[0]))) - 1.0)
            end
          end
        else
          envelope_interp(x, en[2..-1], base)
        end
      end
    end
  end

  add_help(:window_envelope,
           "window_envelope(beg, dur, env)  \
Portion of ENV lying between x axis values BEG and DUR: \
window_envelope(1.0, 3.0, [0.0, 0.0, 5.0, 1.0]) ==> [1.0, 0.2, 3.0, 0.6]")
  def window_envelope(beg, dur, en)
    unless en.empty?
      en.map! do |x|
        x.to_f
      end
    end
    nenv = []
    lasty = en.empty? ? 0.0 : en[1]
    len = en.length
    0.step(len - 1, 2) do |i|
      x = en[i]
      y = lasty = en[i + 1]
      if nenv.empty?
        if x >= beg
          nenv.push(beg, envelope_interp(beg, en))
          unless x == beg
            if x >= dur
              return nenv.push(dur, envelope_interp(dur, en))
            else
              nenv.push(x, y)
            end
          end
        end
      else
        if x <= dur
          nenv.push(x, y)
          return nenv if x == dur
        else
          if x > dur
            return nenv.push(dur, envelope_interp(dur, en))
          end
        end
      end
    end
    nenv.push(dur, lasty)
  end

  add_help(:map_envelopes,
           "map_envelopes(env1, env2, &func)  \
Maps FUNC over the breakpoints in ENV1 and ENV2 returning a new envelope.")
  def map_envelopes(en1, en2, &func)
    unless en1.empty?
      en1.map! do |x|
        x.to_f
      end
    end
    if array?(en2) and (not en2.empty?)
      en2.map! do |x|
        x.to_f
      end
    end
    xs = []
    at0 = lambda do |e|
      diff = e.first
      lastx = e[-2]
      0.step(e.length - 1, 2) do |i|
        x = (e[i] - diff) / lastx
        xs.push(x)
        e[i] = x
      end
      e
    end
    if en1.empty?
      at0.call(en2)
    else
      if en2.empty?
        at0.call(en1)
      else
        ee1 = at0.call(en1)
        ee2 = at0.call(en2)
        newe = []
        xs.uniq.sort.each do |x|
          newe.push(x,
                    func.call(envelope_interp(x, ee1), envelope_interp(x, ee2)))
        end
        newe
      end
    end
  end

  add_help(:multiply_envelopes,
           "multiply_envelopes(env1, env2)  \
Multiplies break-points of ENV1 and ENV2 returning a new envelope: \
multiply_envelopes([0, 0, 2, 0.5], [0, 0, 1, 2, 2, 1]) ==> \
[0.0, 0.0, 0.5, 0.5, 1.0, 0.5]")
  def multiply_envelopes(en1, en2)
    map_envelopes(en1, en2) do |x, y|
      x * y
    end
  end

  add_help(:add_envelopes,
           "add_envelopes(env1, env2)  \
Adds break-points of ENV1 and ENV2 returning a new envelope.")
  def add_envelopes(en1, en2)
    map_envelopes(en1, en2) do |x, y|
      x + y
    end
  end

  add_help(:max_envelope,
           "max_envelope(env)  \
Returns max y value in ENV.")
  def max_envelope(en)
    mx = en[1].to_f
    1.step(en.length - 1, 2) do |i|
      mx = [mx, en[i]].max.to_f
    end
    mx
  end
  
  add_help(:min_envelope,
           "min_envelope(env)  \
Returns min y value in ENV.")
  def min_envelope(en)
    mn = en[1].to_f
    1.step(en.length - 1, 2) do |i|
      mn = [mn, en[i]].min.to_f
    end
    mn
  end

  add_help(:integrate_envelope,
           "integrate_envelope(env)  \
Returns area under ENV.")
  def integrate_envelope(en)
    sum = 0.0
    0.step(en.length - 3, 2) do |i|
      sum = sum + (en[i + 1] + en[i + 3]) * (en[i + 2] - en[i]) * 0.5
    end
    sum
  end

  add_help(:envelope_last_x,
           "envelope_last_x(env)  \
Returns max x axis break point position.")
  def envelope_last_x(en)
    en.empty? ? 0.0 : en[-2]
  end
  
  add_help(:stretch_envelope,
           "stretch_envelope(fn, old_att, new_att, old_dec, new_dec)  \
Takes FN and returns a new envelope based on it but with the attack \
and optionally decay portions stretched or squeezed; \
OLD_ATT is the original x axis attack end point, \
NEW_ATT is where that section should end in the new envelope.  \
Similarly for OLD_DEC and NEW_DEC.  \
This mimics divseg in early versions of CLM \
and its antecedents in Sambox and Mus10 (linen).
stretch_envelope([0, 0, 1, 1], 0.1, 0.2) ==> [0, 0, 0.2, 0.1, 1.0, 1]
stretch_envelope([0, 0, 1, 1, 2, 0], 0.1, 0.2, 1.5, 1.6)
==> [0, 0, 0.2, 0.1, 1.1, 1, 1.6, 0.5, 2.0, 0]")
  def stretch_envelope(fn, old_att = false, new_att = false,
                       old_dec = false, new_dec = false)
    unless array?(fn)
      error("%s: need an envelope, %s", get_func_name, fn.inspect)
    end
    unless fn.empty?
      fn.map! do |x|
        x.to_f
      end
    end
    if old_att and (not new_att)
      Snd.raise(:wrong_number_of_args,
                old_att.inspect,
                "old_att but no new_att?")
    else
      if (not new_att)
        fn
      else
        if old_dec and (not new_dec)
          Snd.raise(:wrong_number_of_args,
                    format("%s %s %s", old_att, new_att, old_dec),
                    "old_dec but no new_dec?")
        else
          new_x = x0 = fn[0]
          last_x = fn[-2]
          y0 = fn[1]
          new_fn = [x0, y0]
          scl = (new_att - x0) / [0.0001, old_att - x0].max
          if old_dec and old_dec == old_att
            old_dec = old_dec + 0.000001 * last_x
          end
          fn[2..-1].each_pair do |x1, y1|
            if x0 < old_att and x1 >= old_att
              y0 = if x1 == old_att
                     y1
                   else
                     y0 + (y1 - y0) * ((old_att - x0) / (x1 - x0))
                   end
              x0 = old_att
              new_x = new_att
              new_fn.push(new_x, y0)
              scl = if old_dec
                      (new_dec - new_att) / (old_dec - old_att)
                    else
                      (last_x - new_att) / (last_x - old_att)
                    end
            end
            if old_dec and x0 < old_dec and x1 >= old_dec
              y0 = if x1 == old_dec
                     y1
                   else
                     y0 + (y1 - y0) * ((old_dec - x0) / (x1 - x0))
                   end
              x0 = old_dec
              new_x = new_dec
              new_fn.push(new_x, y0)
              scl = (last_x - new_dec) / (last_x - old_dec)
            end
            if x0 != x1
              new_x = new_x + scl * (x1 - x0)
              new_fn.push(new_x, y1)
              x0, y0 = x1, y1
            end
          end
          new_fn
        end
      end
    end
  end

  add_help(:scale_envelope,
           "scale_envelope(env, scale, offset = 0.0)  \
Scales y axis values by SCALER and optionally adds OFFSET.")
  def scale_envelope(en, scale, offset = 0.0)
    1.step(en.length - 1, 2) do |i|
      en[i] = en[i] * scale + offset
    end
    en
  end

  add_help(:reverse_envelope,
           "reverse_envelope(env)  \
Reverses the breakpoints in ENV.")
  def reverse_envelope(en1)
    len = en1.length
    if len.zero? or len == 2
      en1
    else
      en2 = en1.dup
      xmax = en1[-2]
      0.step(len - 2, 2) do |i|
        en2[-(i + 2)], en2[-(i + 1)] = xmax - en1[i], en1[i + 1]
      end
      en2
    end
  end
  alias envelope_reverse reverse_envelope

  add_help(:concatenate_envelopes,
           "concatenate_envelopes(*envs)  \
Concatenates its arguments into a new envelope.")
  def concatenate_envelopes(*envs)
    if envs.length == 1
      envs.first
    else
      xoff = 0.0
      ren = []
      envs.each do |en|
        (en or []).map! do |x|
          x.to_f
        end
        firstx = en.first
        if ren[-1] == en[1]
          xoff -= 0.01
          en = en[2..-1]
        end
        0.step(en.length - 1, 2) do |i|
          ren.push(xoff + (en[i] - firstx), en[i + 1])
        end
        xoff += 0.01 + ren[-2]
      end
      ren
    end
  end
  alias envelope_concatenate concatenate_envelopes

  add_help(:repeat_envelope,
           "repeat_envelope(ur_env, repeats, reflected = false, \
x_normalized = false)  \
Repeats ENV REPEATS times.
repeat_envelope([0, 0, 100, 1] 2) ==> [0, 0, 100, 1, 101, 0, 201, 1]
If the final y value is different from the first y value, \
a quick ramp is inserted between repeats. \
X_NORMALIZED causes the new envelope's x axis \
to have the same extent as the original's. \
REFLECTED causes every other repetition to be in reverse.")
  def repeat_envelope(ur_env, repeats, reflected = false, x_normalized = false)
    (ur_env or []).map! do |x|
      x.to_f
    end
    tms = (reflected ? (repeats / 2).floor : repeats)
    en = if reflected
           lastx = ur_env[-2]
           new_env = ur_env.dup
           rev_env = ur_env[0..-3].reverse
           0.step(rev_env.length - 1, 2) do |i|
             new_env.push(lastx + (lastx - rev_env[i + 1]), rev_env[i])
           end
           new_env
         else
           ur_env
         end
    (en or []).map! do |x|
      x.to_f
    end
    first_y = en[1]
    x_max = en[-2]
    x = en.first
    first_y_is_last_y = (first_y == en.last)
    new_env = [first_y, x]
    len = en.length
    tms.times do |i|
      2.step(len - 1, 2) do |j|
        x += en[j] - en[j - 2]
        new_env.push(x, en[j + 1])
      end
      if (i < tms - 1) and (not first_y_is_last_y)
        x = x + x_max / 100.0
        new_env.push(x, first_y)
      end
    end
    if x_normalized
      scl = x_max / x
      0.step(new_env.length - 1, 2) do |i|
        new_env[i] *= scl
      end
    end
    new_env
  end
  alias envelope_repeat repeat_envelope

  class Power_env
    def initialize(*rest)
      envelope, scaler, offset, duration = nil
      optkey(rest, binding,
             [:envelope, [0, 0, 1, 100, 1, 1]],
             [:scaler, 1.0],
             [:offset, 0.0],
             [:duration, 0.0])
      envelope.map! do |val|
        Float(val)
      end
      xext = envelope[-3] - envelope.first
      j = 0
      @envs = make_array(envelope.length / 3 - 1) do |i|
        x0 = envelope[j]
        x1 = envelope[j + 3]
        y0 = envelope[j + 1]
        y1 = envelope[j + 4]
        base = envelope[j + 2]
        j += 3
        make_env(:envelope, [0.0, y0, 1.0, y1],
                 :base, base,
                 :scaler, scaler,
                 :offset, offset,
                 :duration, duration * ((x1 - x0) / xext))
      end
      @current_pass = mus_length(@envs.first)
      @current_env = 0
    end

    def power_env
      val = env(@envs[@current_env])
      @current_pass -= 1
      if @current_pass.zero? and @current_env < (@envs.length - 1)
        @current_env += 1
        @current_pass = mus_length(@envs[@current_env])
      end
      val
    end

    def power_env_channel(beg, dur, snd, chn, edpos, edname)
      curbeg = beg
      as_one_edit_rb(edname) do | |
        @envs.each do |en|
          len = mus_length(en) + 1
          env_channel(en, curbeg, len, snd, chn, edpos)
          curbeg += len
        end
      end
    end
  end
  
  # Power envelope
  def make_power_env(*rest)
    Power_env.new(*rest)
  end

  def power_env(pe)
    pe.power_env
  end

  def power_env_channel(pe, beg = 0, dur = false,
                        snd = false, chn = false, edpos = false)
    pe.power_env_channel(beg, dur, snd, chn, edpos, get_func_name)
  end

  def powenv_channel(envelope, beg = 0, dur = false,
                     snd = false, chn = false, edpos = false)
    curbeg = beg
    fulldur = (dur or framples(snd, chn, edpos))
    len = envelope.length
    x1 = envelope[0]
    xrange = envelope[len - 3] - x1
    y1 = envelope[1]
    base = envelope[2]
    x0 = y0 = 0.0
    if len == 3
      scale_channel(y1, beg, dur, snd, chn, edpos)
    else
      as_one_edit_rb(get_func_name) do | |
        3.step(len - 1, 3) do |i|
          x0, x1 = x1, envelope[i]
          y0, y1 = y1, envelope[i + 1]
          curdur = (fulldur * ((x1 - x0) / xrange)).round
          xramp_channel(y0, y1, base, curbeg, curdur, snd, chn, edpos)
          curbeg += curdur
          base = envelope[i + 2]
        end
      end
    end
  end
  
  # by Anders Vinjar:
  # 
  # envelope-exp can be used to create exponential segments to include in
  # envelopes.  Given 2 or more breakpoints, it approximates the
  # curve between them using 'xgrid linesegments and 'power as the
  # exponent. 
  # 
  # env is a list of x-y-breakpoint-pairs,
  # power applies to whole envelope,
  # xgrid is how fine a solution to sample our new envelope with.

  def envelope_exp(en, power = 1.0, xgrid = 100)
    unless en.empty?
      en.map! do |x|
        x.to_f
      end
    end
    mn = min_envelope(en)
    largest_diff = max_envelope(en) - mn
    x_min = en.first
    x_max = en[-2]
    x_incr = (x_max - x_min) / xgrid.to_f
    new_en = []
    x_min.step(x_max, x_incr) do |x|
      y = envelope_interp(x, en)
      new_en.push(x,
                  (largest_diff.zero? ?
                    y :
                    (mn + largest_diff * (((y - mn) / largest_diff) ** power))))
    end
    new_en
  end

  def rms_envelope(file, *rest)
    beg, dur, rfreq, db = nil
    optkey(rest, binding,
           [:beg, 0.0],
           :dur,
           [:rfreq, 30.0],
           :db)
    e = []
    incr = 1.0 / rfreq
    fsr = mus_sound_srate(file)
    incrsamps = (incr * sfr).round
    start = (beg * fsr).round
    reader = make_sampler(start, file)
    fin = dur ?
            [start + (fsr * dur).round, mus_sound_framples(file)].min :
            mus_sound_framples(file)
    rms = make_moving_average(incrsamps)
    0.step(fin, incrsamps) do |i|
      rms_val = 0.0
      incrsamps.times do |j|
        val = reader.call
        rms_val = moving_average(rms, val * val)
      end
      e.push(i.to_f / fsr)
      rms_val = sqrt(rms_val)
      if db
        if rms_val < 0.00001
          e.push(-100.0)
        else
          e.push(20.0 * (log(rms_val) / log(10.0)))
        end
      else
        e.push(rms_val)
      end
    end
  end
  
  def envelope_length(en)
    en.length / 2
  end

  def normalize_envelope(en, new_max = 1.0)
    mx = en[1].abs.to_f
    1.step(en.length - 1, 2) do |i|
      mx = [mx, en[i].abs].max.to_f
    end
    scale_envelope(en, new_max / mx)
  end
  
  def x_norm(en, xmax)
    scl = xmax / en[-2].to_f
    en.each_pair do |x, y|
      [x * scl, y.to_f]
    end.flatten
  end

  # ;;;=======================================================================
  # ;;; Exponential envelopes
  # ;;;=======================================================================
  # 
  # ;;; Approximate an exponential envelope with a given base and error bound
  # ;;; by Fernando Lopez-Lezcano (nando@ccrma.stanford.edu)
  # ;;;
  # ;;; base:
  # ;;;   step size of the exponential envelope
  # ;;; error:
  # ;;;   error band of the approximation
  # ;;; scaler:
  # ;;;   scaling factor for the y coordinates
  # ;;; offset:
  # ;;;   offset for the y coordinates
  # ;;; cutoff:
  # ;;;   lowest value of the exponentially rendered envelope, values lower than
  # ;;;   this cutoff value will be approximated as cero.
  # ;;; out-scaler
  # ;;;   scaler for the converted values

  def exp_envelope(env, *args)
    base, error, scaler, offset, cutoff, out_scaler = nil
    optkey(args, binding,
           [:base, 2 ** (1.0 / 12)],
           [:error, 0.01],
           [:scaler, 1.0],
           [:offset, 0.0],
           :cutoff,
           [:out_scaler, 1.0])
    result = []
    ycutoff = (cutoff ? (base ** (offset + cutoff * scaler)) : false)
    interpolate = lambda do |xl, yl, xh, yh, xi|
      yl + (xi - xl) * ((yh - yl) / (xh - xl))
    end
    exp_seg = lambda do |xl, yle, xh, yhe, yl, yh, err|
      xint = (xl + xh) / 2.0
      yint = interpolate.call(xl, yl, xh, yh, xint)
      yinte = interpolate.call(xl, yle, xh, yhe, xint)
      yexp = base ** yint
      yerr = base ** (yint + err) - yexp
      if (yexp - yinte).abs > yerr and ((ycutoff and yinte > ycutoff) or true)
        xi, yi = exp_seg.call(xl, yle, xint, yexp, yl, yint, err)
        xj, yj = exp_seg.call(xint, yexp, xh, yhe, yint, yh, err)
        [xi + [xint] + xj, yi + [yexp] + yj]
      else
        [[], []]
      end
    end
    nx = nyscl = 0.0
    0.step(env.length - 4, 2) do |i|
      x  = env[i]
      y  = env[i + 1]
      nx = env[i + 2]
      ny = env[i + 3]
      yscl  = offset + y  * scaler
      nyscl = offset + ny * scaler
      result.push(x)
      result.push((((not ycutoff) or base ** yscl >= ycutoff) ?
                  (out_scaler * base ** yscl) : 0.0))
      xs, ys = exp_seg.call(x, base ** yscl,
                            nx, base ** nyscl, yscl, nyscl, error)
      unless xs.empty?
        ys_scaled = vct_scale!(list2vct(ys), out_scaler)
        xs.each_with_index do |xx, ii|
          result.push(xx)
          result.push(ys_scaled[ii])
        end
      end
    end
    result.push(nx)
    result.push((((not ycutoff) or base ** nyscl >= ycutoff) ?
                (out_scaler * base ** nyscl) : 0.0))
  end

  # ;;; Amplitude envelope in dBs
  # ;;;
  # ;;; The db scale is defined as:
  # ;;;    value(db)=(* 20 (log10 (/ vin vref)))
  # ;;;  where:
  # ;;;    vref=1.0 reference value = digital clipping
  def db_envelope(env, cutoff = -70, error = 0.01)
    exp_envelope(env, :base, 10.0, :scaler, 1.0 / 20,
                 :cutoff, cutoff, :error, error)
  end

  def make_db_env(env, *args)
    scaler, offset, base, duration, len, cutoff, error = nil
    optkey(args, binding,
           [:scaler, 1.0],
           [:offset, 0.0],
           [:base, 1.0],
           [:duration, 0.0],
           [:len, 0],
           [:cutoff, -70],
           [:error, 0.01])
    make_env(:envelope, db_envelope(env, cutoff, error), :scaler, scaler,
             :offset, offset, :base, base, :duration, duration, :length, len)
  end

  # ;;; Pitch envelopes (y units are semitone and octave intervals)
  def semitones_envelope(env, around = 1.0, error = 0.01)
    exp_envelope(env, :error, error, :out_scaler, around)
  end

  def make_semitones_env(env, *args)
    around, scaler, offset, base, dur, len, error = nil
    optkey(args, binding,
           [:around, 1.0],
           [:scaler, 1.0],
           [:offset, 0.0],
           [:base, 1.0],
           [:duration, 0.0],
           [:len, 0],
           [:error, 0.01])
    make_env(:envelope, semitones_envelope(env, around, error),
             :scaler, scaler, :offset, offset,
             :base, base, :duration, dur, :length, len)
  end

  def octaves_envelope(env, around = 1.0, error = 0.01)
    exp_envelope(env, :error, error, :base, 2.0, :out_scaler, around)
  end

  def make_octaves_env(env, *args)
    around, scaler, offset, base, dur, len, error = nil
    optkey(args, binding,
           [:around, 1.0],
           [:scaler, 1.0],
           [:offset, 0.0],
           [:base, 1.0],
           [:duration, 0.0],
           [:len, 0],
           [:error, 0.01])
    make_env(:envelope, octaves_envelope(env, around, error),
             :scaler, scaler, :offset, offset,
             :base, base, :duration, dur, :length, len)
  end
end

include Env

=begin
# power envelope test (clm/env.lisp)

def test_power_env(start, dur, en)
  os = make_oscil()
  pe = make_power_env(:envelope, en, :duration, dur, :scaler, 0.5)
  beg, len = times2samples(start, dur)
  (beg...len).each do |i| outa(i, power_env(pe) * oscil(os), $output) end
end

with_sound(:channels, 1, :play, 1) do | |
  test_power_env(0, 1, [0, 0, 0.325,  1, 1, 32,  2, 0, 0])
  test_power_env(1.2, 1,
                 [0, 0, 0.325,  1, 1, 32,  2, 0.5, 1,  3, 1, 0.1234,  4, 0, 0])
  test_power_env(2.4, 1,
                 [0, 0, 0,  1, 1, 1,  2, 0.5, 0.123,  3, 1, 321,  4, 0, 0])
end
=end

# env.rb ends here
