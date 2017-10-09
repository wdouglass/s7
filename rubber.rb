# rubber.rb -- Translation of rubber.scm

# Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 03/02/28 03:04:03
# Changed: 14/11/23 06:14:52

# module Rubber (see rubber.scm)
#  add_named_mark(samp, name, snd, chn)
#  derumble_sound(snd, chn)
#  sample_sound(snd, chn)
#  unsample_sound(snd, chn)
#  crossings()
#  env_add(s0, s1, samps)
#  rubber_sound(stretch, snd, chn)

require "clm"

module Rubber
  doc "#{self.class} #{self.name} is the translation of rubber.scm.\n"
  
  $zeros_checked = 8
  $extension = 10.0
  $show_details = false
  
  def add_named_mark(samp, name, snd = false, chn = false)
    m = add_mark(samp.round, snd, chn)
    set_mark_name(m, name)
    m
  end

  def derumble_sound(snd = false, chn = false)
    old_length = framples(snd, chn)
    pow2 = (log([old_length, srate(snd)].min) / log(2)).ceil
    fftlen = (2 ** pow2).round
    flt_env = [0.0, 0.0, 32.0 / srate(snd), 0.0,
               40.0 / srate(snd), 1.0, 1.0, 1.0]
    filter_sound(flt_env, fftlen, snd, chn)
    set_framples(old_length, snd, chn)
  end

  def sample_sound(snd = false, chn = false)
    if $extension != 1.0
      src_sound(1.0 / $extension, 1.0, snd, chn)
    end
  end

  def unsample_sound(snd = false, chn = false)
    if $extension != 1.0
      src_sound($extension, 1.0, snd, chn)
    end
  end

  def crossings
    crosses = 0
    sr0 = make_sampler(0)
    samp0 = next_sample(sr0)
    len = framples()
    sum = 0.0
    last_cross = 0
    silence = $extension * 0.001
    (0...len).each do |i|
      samp1 = next_sample(sr0)
      if samp0 <= 0.0 and
          samp1 > 0.0 and
          (i - last_cross) > 4 and
          sum > silence
        crosses += 1
        last_cross = i
        sum = 0.0
      end
      sum += samp0.abs
      samp0 = samp1
    end
    crosses
  end

  def env_add(s0, s1, samps)
    data = make_vct(samps.round)
    x = 1.0
    xinc = 1.0 / samps
    sr0 = make_sampler(s0.round)
    sr1 = make_sampler(s1.round)
    (0...samps).each do |i|
      data[i] = x * next_sample(sr0) + (1.0 - x) * next_sample(sr1)
      x += xinc
    end
    data
  end

  def rubber_sound(stretch, snd = false, chn = false)
    as_one_edit(lambda do | |
                  derumble_sound(snd, chn)
                  sample_sound(snd, chn)
                  crosses = crossings()
                  cross_samples = make_vct(crosses)
                  cross_weights = make_vct(crosses)
                  cross_marks = make_vct(crosses)
                  cross_periods = make_vct(crosses)
                  sr0 = make_sampler(0, snd, chn)
                  samp0 = next_sample(sr0)
                  len = framples()
                  sum = 0.0
                  last_cross = 0
                  cross = 0
                  silence = $extension * 0.001
                  (0...len).each do |i|
                    samp1 = next_sample(sr0)
                    if samp0 <= 0.0 and
                        samp1 > 0.0 and
                        (i - last_cross) > 4 and
                        sum > silence
                      last_cross = i
                      sum = 0.0
                      cross_samples[cross] = i
                      cross += 1
                    end
                    sum += samp0.abs
                    samp0 = samp1
                  end
                  (0...(crosses - 1)).each do |i|
                    start = cross_samples[i]
                    autolen = 0
                    s0 = start
                    pow2 = (log($extension * (srate() / 40.0)) / log(2)).ceil
                    fftlen = (2 ** pow2).round
                    len4 = fftlen / 4
                    data = make_vct(fftlen)
                    reader = make_sampler(s0.round)
                    (0...fftlen).each do |j|
                      data[j] = next_sample(reader)
                    end
                    autocorrelate(data)
                    autolen = 0
                    (1...len4).detect do |j|
                      if data[j] < data[j + 1] and data[j + 1] > data[j + 2]
                        autolen = j * 2
                        true
                      end
                    end
                    next_start = start + autolen
                    min_i = i + 1
                    min_samps = (cross_samples[min_i] - next_start).abs
                    ((i + 2)...[crosses, i + $zeros_checked].min).each do |k|
                      dist = (cross_samples[k] - next_start).abs
                      if dist < min_samps
                        min_samps = dist
                        min_i = k
                      end
                    end
                    current_mark = min_i
                    current_min = 0.0
                    s0 = start
                    s1 = cross_samples[current_mark]
                    len = autolen
                    sr0 = make_sampler(s0.round)
                    sr1 = make_sampler(s1.round)
                    ampsum = 0.0
                    diffsum = 0.0
                    (0...len).each do |dummy|
                      samp0 = next_sample(sr0)
                      samp1 = next_sample(sr1)
                      ampsum += samp0.abs
                      diffsum += (samp1 - samp0).abs
                    end
                    if diffsum == 0.0
                      current_min = 0.0
                    else
                      current_min = diffsum / ampsum
                    end
                    min_samps = 0.5 * current_min
                    top = [crosses - 1, current_mark, i + $zeros_checked].min
                    ((i + 1)...top).each do |k|
                      wgt = 0.0
                      s0 = start
                      s1 = cross_samples[k]
                      len = autolen
                      sr0 = make_sampler(s0.round)
                      sr1 = make_sampler(s1.round)
                      ampsum = 0.0
                      diffsum = 0.0
                      (0...len).each do |dummy|
                        samp0 = next_sample(sr0)
                        samp1 = next_sample(sr1)
                        ampsum += samp0.abs
                        diffsum += (samp1 - samp0).abs
                      end
                      if diffsum == 0.0
                        wgt = 0.0
                      else
                        wgt = diffsum / ampsum
                      end
                      if wgt < min_samps
                        min_samps = wgt
                        min_i = k
                      end
                    end
                    unless current_mark == min_i
                      cross_weights[i] = 1000.0
                    else
                      cross_weights[i] = current_min
                      cross_marks[i] = current_mark
                      cross_periods[i] = cross_samples[current_mark] -
                                         cross_samples[i]
                    end
                  end
                  len = framples(snd, chn)
                  adding = (stretch > 1.0)
                  samps = ((stretch - 1.0).abs * len).round
                  needed_samps = (adding ? samps : [len, samps * 2].min)
                  handled = 0
                  mult = 1
                  curs = 0
                  weigths = cross_weights.length
                  edits = make_vct(weigths)
                  until curs == weigths or handled >= needed_samps
                    best_mark = -1
                    old_handled = handled
                    cur = 0
                    curmin = cross_weights[0]
                    cross_weights.each_with_index do |cross_w, i|
                      if cross_w < curmin
                        cur = i
                        curmin = cross_w
                      end
                    end
                    best_mark = cur
                    handled += cross_periods[best_mark].round
                    if (handled < needed_samps) or
                       ((handled - needed_samps) < (needed_samps - old_handled))
                      edits[curs] = best_mark
                      curs += 1
                    end
                    cross_weights[best_mark] = 1000.0
                  end
                  mult = (needed_samps / handled).ceil if curs >= weigths
                  changed_len = 0
                  weights = cross_weights.length
                  (0...curs).detect do |i|
                    best_mark = edits[i].round
                    beg = cross_samples[best_mark].to_i
                    next_beg = cross_samples[cross_marks[best_mark].round]
                    len = cross_periods[best_mark].to_i
                    if len > 0
                      if adding
                        new_samps = env_add(beg, next_beg, len)
                        if $show_details
                          add_named_mark(beg,
                                         format("%d:%d",
                                                i, (len / $extension).round))
                        end
                        insert_samples(beg, len, new_samps)
                        if mult > 1
                          (1...mult).each do |k|
                            insert_samples(beg + k * len, len, new_samps)
                          end
                        end
                        changed_len = changed_len + mult * len
                        (0...weights).each do |j|
                          curbeg = cross_samples[j]
                          if curbeg > beg
                            cross_samples[j] = curbeg + len
                          end
                        end
                      else
                        frms = framples()
                        if beg >= frms
                          Snd.display("trouble at %d: %d of %d", i, beg, frms)
                        end
                        if $show_details
                          add_named_mark(beg - 1,
                                         format("%d:%d",
                                                i, (len / $extension).round))
                        end
                        delete_samples(beg, len)
                        changed_len += len
                        fin = beg + len
                        (0...weights).each do |j|
                          curbeg = cross_samples[j]
                          if curbeg > beg
                            if curbeg < fin
                              cross_periods[j] = 0
                            else
                              cross_samples[j] = curbeg - len
                            end
                          end
                        end
                      end
                    end
                    changed_len > samps
                  end
                  if $show_details
                    Snd.display("wanted: %d, got %d",
                                samps.round, changed_len.round)
                  end
                  unsample_sound(snd, chn)
                  if $show_details
                    frms0 = framples(snd, chn, 0)
                    Snd.display("%f -> %f (%f)",
                                frms0,
                                framples(snd,chn),
                                (stretch * frms0).round)
                  end
                end,
                format("rubber_sound(%f, %p, %p,", stretch, snd, chn))
  end
end

include Rubber

# rubber.rb ends here
