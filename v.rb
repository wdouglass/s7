# Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 02/11/20 02:24:34
# Changed: 14/11/23 03:36:55

# fm_violin(start, dur, freq, amp, *args)
# play_fm_violin(start, dur, freq, amp, *args)
# make_fm_violin(start, dur, freq, amp, *args)
# jc_reverb(*args)

require "ws"

v_opts_str = "fm_violin(start=0.0, dur=1.0, freq=440.0, amp=0.5, *args)
 :fm_index              = 1.0
 :amp_env               = [0, 0, 25, 1, 75, 1, 100, 0]
 :periodic_vibrato_rate = 5.0
 :random_vibrato_rate   = 16.0
 :periodic_vibrato_amp  = 0.0025
 :random_vibrato_amp    = 0.005
 :noise_amount          = 0.0
 :noise_freq            = 1000.0
 :ind_noise_freq        = 10.0
 :ind_noise_amount      = 0.0
 :amp_noise_freq        = 20.0
 :amp_noise_amount      = 0.0
 :gliss_env             = [0, 0, 100, 0]
 :gliss_amount          = 0.0
 :fm1_env               = [0, 1, 25, 0.4, 75, 0.6, 100, 0]
 :fm2_env               = [0, 1, 25, 0.4, 75, 0.6, 100, 0]
 :fm3_env               = [0, 1, 25, 0.4, 75, 0.6, 100, 0]
 :fm1_rat               = 1.0
 :fm2_rat               = 3.0
 :fm3_rat               = 4.0
 :fm1_index             = false
 :fm2_index             = false
 :fm3_index             = false
 :base                  = 1.0
 :index_type            = :violin"

add_help(:fm_violin,
         "#{v_opts_str}
 :reverb_amount         = 0.01
 :degree                = kernel_rand(90.0)
 :distance              = 1.0
  Ruby: fm_violin(0, 1, 440, 0.1, :fm_index, 2.0)
Scheme: (fm-violin 0 1 440 0.1 :fm-index 2.0)
Example: with_sound do fm_violin(0, 1, 440, 0.1, :fm_index, 2.0) end")
def fm_violin(start = 0.0, dur = 1.0, freq = 440.0, amp = 0.5, *args)
  r = get_args(args, :reverb_amount, 0.01)
  d = get_args(args, :degree, kernel_rand(90.0))
  s = get_args(args, :distance, 1.0)
  fm_vln = make_fm_violin_gen(start, dur, freq, amp, *args)
  run_instrument(start, dur, :reverb_amount, r, :degree, d, :distance, s) do
    fm_vln.call(0)
  end
end

add_help(:play_fm_violin,
         "play_#{v_opts_str}
Returns a fm_violin proc with no arg for play(proc).
v = play_fm_violin(0, 1, 440, 0.5)
play(v)
or
play(play_fm_violin(0, 1, 440, 0.5))")
def play_fm_violin(start = 0.0, dur = 1.0, freq = 440.0, amp = 0.5, *args)
  fm_vln = make_fm_violin_gen(start, dur, freq, amp, *args)
  len = seconds2samples(dur) 
	lambda do
    (len -= 1).positive? and fm_vln.call(0)
  end
end

#
# make_fm_violin: returns lambda do |y| ... end
#
add_help(:make_fm_violin,
         "make_#{v_opts_str}
Returns a proc with one arg for map_channel()
*args are like fm_violin's
ins = new_sound(:file, \"fmv.snd\", :srate, 22050, :channels, 2)
# proc with one arg
fmv1 = make_fm_violin(0, 1, 440, 0.5)
map_channel(fmv1, 0, 22050, ins, 1)")
def make_fm_violin(start = 0.0, dur = 1.0, freq = 440.0, amp = 1.0, *args)
  make_fm_violin_gen(start, dur, freq, amp, *args)
end

def make_fm_violin_gen(start, dur, freq, amp, *args)
    fm_index, amp_env, periodic_vibrato_rate, random_vibrato_rate = nil
    periodic_vibrato_amp, random_vibrato_amp, noise_amount, noise_freq = nil
    ind_noise_freq, ind_noise_amount, amp_noise_freq, amp_noise_amount = nil
    gliss_env, gliss_amount = nil
    fm1_env, fm2_env, fm3_env, fm1_rat, fm2_rat, fm3_rat = nil
    fm1_index, fm2_index, fm3_index, base, index_type = nil
    optkey(args, binding,
           [:fm_index, 1.0],
           [:amp_env, [0, 0, 25, 1, 75, 1, 100, 0]],
           [:periodic_vibrato_rate, 5.0],
           [:random_vibrato_rate, 16.0],
           [:periodic_vibrato_amp, 0.0025],
           [:random_vibrato_amp, 0.005],
           [:noise_amount, 0.0],
           [:noise_freq, 1000.0],
           [:ind_noise_freq, 10.0],
           [:ind_noise_amount, 0.0],
           [:amp_noise_freq, 20.0],
           [:amp_noise_amount, 0.0],
           [:gliss_env, [0, 0, 100, 0]],
           [:gliss_amount, 0.0],
           [:fm1_env, [0, 1, 25, 0.4, 75, 0.6, 100, 0]],
           [:fm2_env, [0, 1, 25, 0.4, 75, 0.6, 100, 0]],
           [:fm3_env, [0, 1, 25, 0.4, 75, 0.6, 100, 0]],
           [:fm1_rat, 1.0],
           [:fm2_rat, 3.0],
           [:fm3_rat, 4.0],
           [:fm1_index, false],
           [:fm2_index, false],
           [:fm3_index, false],
           [:base, 1.0],
           [:index_type, :violin])
  frq_scl = hz2radians(freq)
  modulate = fm_index.nonzero?
  maxdev = frq_scl * fm_index
  vln = (index_type == :violin or index_type == 1)
  logfreq = log(freq)
  sqrtfreq = sqrt(freq)
  index1 = (fm1_index or [PI, maxdev * (vln ? 5.0 : 7.5) / logfreq].min)
  index2 = (fm2_index or [PI, maxdev * 3.0 *
           (vln ? ((8.5 - logfreq) / (3.0 + freq * 0.001)) :
           (15.0 / sqrtfreq))].min)
  index3 = (fm3_index or [PI, maxdev * (vln ? 4.0 : 8.0) / sqrtfreq].min)
  easy_case = (noise_amount.zero? and
               (fm1_env == fm2_env) and 
               (fm1_env == fm3_env) and 
               (fm1_rat - fm1_rat.floor).zero? and 
               (fm2_rat - fm2_rat.floor).zero? and 
               (fm3_rat - fm3_rat.floor).zero?)
  norm = ((easy_case and modulate and 1.0) or index1)
  carrier = make_oscil(freq)
  fmosc1 = fmosc2 = fmosc3 = false
  indf1 = indf2 = indf3 = false
  if modulate
    indf1 = make_env(fm1_env, norm, dur)
    if easy_case
      a = [fm1_rat.to_i, index1,
          (fm2_rat / fm1_rat).floor, index2,
          (fm3_rat / fm1_rat).floor, index3]
      coe = partials2polynomial(a)
      fmosc1 = make_polyshape(:frequency, fm1_rat * freq, :coeffs, coe)
    else
      indf2 = make_env(fm2_env, index2, dur)
      indf3 = make_env(fm3_env, index3, dur)
      if (fm1_rat * freq * 2.0) > mus_srate
        fmosc1 = make_oscil(:frequency, freq)
      else
        fmosc1 = make_oscil(:frequency, fm1_rat * freq)
      end
      if (fm2_rat * freq * 2.0) > mus_srate
        fmosc2 = make_oscil(:frequency, freq)
      else
        fmosc2 = make_oscil(:frequency, fm2_rat * freq)
      end
      if (fm3_rat * freq * 2.0) > mus_srate
        fmosc3 = make_oscil(:frequency, freq)
      else
        fmosc3 = make_oscil(:frequency, fm3_rat * freq)
      end
    end
  end
  ampf = make_env(amp_env, amp, dur, 0.0, base)
  frqf = make_env(gliss_env, gliss_amount * frq_scl, dur)
  pervib = make_triangle_wave(periodic_vibrato_rate,
                              periodic_vibrato_amp * frq_scl)
  ranvib = make_rand_interp(random_vibrato_rate,
                            random_vibrato_amp * frq_scl)
  fm_noi = if noise_amount.nonzero?
             make_rand(noise_freq, PI * noise_amount)
           else
             false
           end
  ind_noi = if ind_noise_amount.nonzero? and ind_noise_freq.nonzero?
              make_rand_interp(ind_noise_freq, ind_noise_amount)
            else
              false
            end
  amp_noi = if amp_noise_amount.nonzero? and amp_noise_freq.nonzero?
              make_rand_interp(amp_noise_freq, amp_noise_amount)
            else
              false
            end
  fuzz = modulation = 0.0
  ind_fuzz = amp_fuzz = 1.0
  if modulate
    if easy_case
      lambda do |y|
        vib = env(frqf) + triangle_wave(pervib) + rand_interp(ranvib)
        ind_fuzz = 1.0 + rand_interp(ind_noi) if ind_noi
        amp_fuzz = 1.0 + rand_interp(amp_noi) if amp_noi
        modulation = env(indf1) * polyshape(fmosc1, 1.0, vib)
        env(ampf) * amp_fuzz * oscil(carrier, vib + ind_fuzz * modulation)
      end
    else
      lambda do |y|
        fuzz = rand(fm_noi) if fm_noi
        vib = env(frqf) + triangle_wave(pervib) + rand_interp(ranvib)
        ind_fuzz = 1.0 + rand_interp(ind_noi) if ind_noi
        amp_fuzz = 1.0 + rand_interp(amp_noi) if amp_noi
        modulation = (env(indf1) * oscil(fmosc1, fm1_rat * vib + fuzz) +
                      env(indf2) * oscil(fmosc2, fm2_rat * vib + fuzz) +
                      env(indf3) * oscil(fmosc3, fm3_rat * vib + fuzz))
        env(ampf) * amp_fuzz * oscil(carrier, vib + ind_fuzz * modulation)
      end
    end
  else
    lambda do |y|
      vib = env(frqf) + triangle_wave(pervib) + rand_interp(ranvib)
      ind_fuzz = 1.0 + rand_interp(ind_noi) if ind_noi
      amp_fuzz = 1.0 + rand_interp(amp_noi) if amp_noi
      env(ampf) * amp_fuzz * oscil(carrier, vib + ind_fuzz)
    end
  end
end

add_help(:jc_reverb,
         "jc_reverb(*args)
 :volume   = 1.0
 :delay1   = 0.013
 :delay2   = 0.011
 :delay3   = 0.015
 :delay4   = 0.017
 :low_pass = false
 :double   = false
 :amp_env  = false
Chowning reverb")
def jc_reverb(*args)
  low_pass, volume, amp_env, delay1, delay2, delay3, delay4, double = nil
  optkey(args, binding,
         [:volume, 1.0],
         [:delay1, 0.013],
         [:delay2, 0.011],
         [:delay3, 0.015],
         [:delay4, 0.017],
         [:low_pass, false],
         [:double, false],
         [:amp_env, false])
  chan2 = (@channels > 1)
  chan4 = (@channels == 4)
  if double and chan4
    error("%s: not set up for doubled reverb in quad", get_func_name)
  end
  allpass1 = make_all_pass(-0.7, 0.7, 1051)
  allpass2 = make_all_pass(-0.7, 0.7,  337)
  allpass3 = make_all_pass(-0.7, 0.7,  113)
  comb1 = make_comb(0.742, 4799)
  comb2 = make_comb(0.733, 4999)
  comb3 = make_comb(0.715, 5399)
  comb4 = make_comb(0.697, 5801)
  outdel1 = make_delay((delay1 * @srate).round)
  outdel2 = (chan2 ? make_delay((delay2 * @srate).round) : false)
  outdel3 = ((chan4 or double) ? make_delay((delay3 * @srate).round) : false)
  outdel4 = ((chan4 or (double and chan2)) ?
              make_delay((delay4 * @srate).round) : false)
  envA = if amp_env
           make_env(:envelope, amp_env,
                    :scaler, volume,
                    :duration, ws_duration(@revfile) + @decay_time)
         else
           false
         end
  comb_sum_1 = comb_sum = 0.0
  out_frample = Vct.new(@channels)
  run_reverb() do |ho, i|
    allpass_sum = all_pass(allpass3, all_pass(allpass2,
                    all_pass(allpass1, ho)))
    comb_sum_2, comb_sum_1 = comb_sum_1, comb_sum
    comb_sum = (comb(comb1, allpass_sum) + comb(comb2, allpass_sum) +
                comb(comb3, allpass_sum) + comb(comb4, allpass_sum))
    all_sums = if low_pass
                 0.25 * (comb_sum + comb_sum_2) + 0.5 * comb_sum_1
               else
                 comb_sum
               end
    delA = delay(outdel1, all_sums)
    if double
      delA += delay(outdel3, all_sums)
    end
    if envA
      volume = env(envA)
    end
    out_frample[0] = volume * delA
    if chan2
      delB = delay(outdel2, all_sums)
      if double
        delB += delay(outdel4, all_sums)
      end
     out_frample[1] = volume * delB
      if chan4
        out_frample[2] = volume * delay(outdel3, all_sums)
        out_frample[3] = volume * delay(outdel4, all_sums)
      end
    end
    out_frample
  end
end

# v.rb ends here
