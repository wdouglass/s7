# marks.rb -- marks.scm --> marks.rb

# Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 05/03/23 02:08:47
# Changed: 14/11/14 07:02:23

# examples of mark-related functions
#
# module Mark
#  mark_name2id(name)
#  move_syncd_marks(sync, diff)
#  describe_mark(id)
#
#  class Mark_sync
#   initialize
#   start_sync
#   stop_sync
#   click_to_sync(id)
#
#  syncup(ids)
#  fit_selection_between_marks(m1, m2)
#  pad_marks(ids, secs)
#  play_syncd_marks(sync)
#  play_between_marks(mark1 = false, mark2 = false)
#
#  class Mark_report
#   initialize(snd)
#   report_mark_names_play_hook(size)
#   report_mark_names_stop_playing_hook(snd)
#   
#  report_mark_names
#  eval_between_marks(&func)
#  snap_marks
#  define_selection_via_marks(m1, m2)
#  snap_mark_to_beat
#  mark_explode(htype = Mus_next, dformat = Mus_bfloat)
#  save_mark_properties
#  mark_click_info(id)
#  eval_header(sndf)
#  marks2string(sndf)

require "hooks"

module Mark
  # mark_name2id is a global version of find-mark

  add_help(:mark_name2id,
           "mark_name2id(name)  \
Is like find-mark but searches all currently accessible channels.")
  def mark_name2id(name)
    Snd.sounds.each do |snd|
      channels(snd).times do |chn|
        if mark?(m = find_mark(name, snd, chn))
          return m
        end
      end
    end
    :no_such_mark
  end

  # move_syncd_marks moves all syncd marks together

  add_help(:move_syncd_marks,
           "move_syncd_marks(sync, diff)  \
Moves all marks sharing sync by diff samples.")
  def move_syncd_marks(sync, diff)
    (syncd_marks(sync) or []).each do |m|
      set_mark_sample(m, mark_sample(m) + diff)
    end
  end

  # describe_mark shows mark history

  add_help(:describe_mark,
           "describe_mark(id)  \
Returns a description of the movements of mark ID over \
the channel's edit history.")
  def describe_mark(id)
    mark_setting = Snd.catch do
      mark_home(id)
    end.first
    if mark_setting == :no_such_mark
      Snd.sounds.each do |snd|
        break if array?(mark_setting)
        channels(snd).times do |chn|
          break if array?(mark_setting)
          max_edits = 0
          edits(snd, chn).each do |n|
            max_edits += n
          end
          0.upto(max_edits) do |ed|
            if (m = marks(snd, chn, ed)) and m.member?(id)
              mark_setting = [snd, chn]
              break
            end
          end
        end
      end
    end
    if array?(mark_setting)
      snd, chn = mark_setting
      max_edits = 0
      edits(snd, chn).each do |n|
        max_edits += n
      end
      descr = [[:mark, id, :sound, snd, short_file_name(snd), :channel, chn]]
      0.upto(max_edits) do |ed|
        if marks(snd, chn, ed).member?(id)
          descr.push(mark_sample(id, ed))
        else
          descr.push(false)
        end
      end
      descr
    else
      Snd.raise(:no_such_mark, id)
    end
  end

  # click marks between start-sync and stop-sync to sync them together
  # (easier than calling mark-sync over and over by hand)
  class Mark_sync
    def initialize
      @mark_sync_number = 0
    end

    def start_sync
      @mark_sync_number += 1
    end

    def stop_sync
      @mark_sync_number = 0
    end

    def click_to_sync(id)
      mark_sync(id, @mark_sync_number)
      false
    end
  end
  # ms = Mark_sync.new
  # $mark_click_hook.add_hook!("marks") do |id| ms.click_to_sync(id) end

  # syncronize sounds at a given mark

  add_help(:syncup,
           "syncup(*ids)  \
Pads the channels with zeros so that all the marks in IDS list \
occur at the same time.")
  def syncup(*args_ids)
    ids = args_ids.flatten
    samps = ids.map do |id|
      mark?(id) and mark_sample(id)
    end
    max_samp = samps.max
    ids.zip(samps) do |id, samp|
      if samp < max_samp
        nsamps = max_samp - samp
        snd, chn = mark_home(id)
        insert_samples(0, nsamps, Vct.new(nsamps), snd, chn)
      end
    end
  end

  # fit selection between marks, expanding via granulate (this
  # needs some tweaking...)

  add_help(:fit_selection_between_marks,
           "fit_selection_between_marks(m1, m2)  \
Fits (and mixes) the current selection (via granulate) \
between the given marks.")
  def fit_selection_between_marks(m1, m2)
    m1_samp = mark_sample(m1)
    m2_samp = mark_sample(m2)
    m1_home = mark_home(m1)
    m2_home = mark_home(m2)
    if m1_home != m2_home
      Snd.display("mark %s is in %s[%s] but mark %s is in %s[%s]?",
                  m1, m1_home[0], m1_home[1], m2, m2_home[0], m2_home[1])
    else
      mark_samps = m2_samp - m1_samp
      selection_samps = selection_framples()
      reg_data = region2vct
      reader = make_sampler(m1_samp)
      gr = make_granulate(:expansion, mark_samps / selection_samps.to_f)
      inctr = 0
      new_data = Vct.new(mark_samps) do
        next_sample(reader) + granulate(gr,
                                        lambda do |dir|
                                          if inctr >= selection_samps
                                            0.0
                                          else
                                            val = reg_data[inctr]
                                            inctr += dir
                                            val
                                          end
                                        end)
      end
      free_sampler(reader)
      vct2channel(new_data, m1_samp, mark_samps, m1_home[0], m1_home[1])
    end
  end

  # pad_marks inserts silence before each in a list of marks

  add_help(:pad_marks,
           "pad_marks(ids, secs)  \
Inserts SECS seconds of silence before each mark in IDS.")
  def pad_marks(ids, secs)
    silence_length = (secs * srate()).floor
    silence_samps = Vct.new(silence_length)
    as_one_edit_rb(get_func_name) do
      (ids or []).each do |id|
        samp = [0, mark_sample(id) - 1].max
        snd, chn = mark_home(id)
        insert_samples(samp, silence_length, silence_samps, snd, chn)
      end
    end
  end

  # play_syncd_marks

  add_help(:play_syncd_marks,
           "play_syncd_marks(sync)  \
Starts playing from all marks sharing SYNC.")
  def play_syncd_marks(sync)
    chans = 1
    rate = 22050
    (syncd_marks(sync) or []).each do |m|
      snd, chn = mark_home(m)
      new_player = make_player(snd, chn)
      add_player(new_player, mark_sample(m))
      chans = [chans, chn + 1].max
      rate = [rate, srate(snd)].max
    end
    start_playing(chans, rate)
  end

  add_help(:play_between_marks,
           "play_between_marks(mark1=false, mark2=false)  \
Plays the portion between the marks (searching for plausible default marks).")
  def play_between_marks(mark1 = false, mark2 = false)
    snd = Snd.snd
    chn = Snd.chn
    m1 = if mark1
           mark1
         else
           if ms = marks(snd, chn)
             ret = false
             ms.each do |m|
               if mark_sample(m) >= left_sample(snd, chn)
                 ret = m
                 break
               end
             end
             ret
           else
             Snd.display("no marks in current window?")
             false
           end
         end
    m2 = if mark?(m1)
           if mark2
             mark2
           else
             if ms = marks(snd, chn)
               ret = false
               ms.each do |m|
                 if mark_sample(m) > mark_sample(m1)
                   ret = m
                   break
                 end
               end
               ret
             else
               Snd.display("no second mark?")
               false
             end
           end
         else
           false
         end
    if mark?(m1) and mark?(m2)
      pos1 = mark_sample(m1)
      pos2 = mark_sample(m2)
      beg = [pos1, pos2].min
      len = [pos1, pos2].max
      play(mark_home(m1).car,
           :channel, mark_home(m1).cadr, :start, beg, :end, len)
    end
  end

  class Mark_report
    def initialize(snd)
      @snd = snd
      @marklist = marks(@snd, 0)
      @samplist = (@marklist or []).map do |m|
        mark_sample(m)
      end
      @samp = 0
    end
    
    def report_mark_names_play_hook(size)
      @samp += size
      if array?(@samplist) and @samp >= @samplist.first
        snd_print(mark_sample(@marklist.first), @snd)
        @marklist.unshift
        @samplist.unshift
      end
    end

    def report_mark_names_stop_playing_hook(snd)
      snd_print("", snd)
      $play_hook.remove_hook("report-mark-names-play")
      $stop_playing_hook.remove_hook("report-mark-names-stop-playing")
    end
  end

  # report_mark_names causes mark names to be posted in the minibuffer as a sound is played

  add_help(:report_mark_names,
           "report_mark_names()  \
Causes mark names to be printed as they are passed while playing.")
  def report_mark_names
    $start_playing_hook.add_hook!("marks.rb") do |snd|
      rp = Mark_report.new(snd)
      $stop_playing_hook.add_hook!("report-mark-names-stop-playing") do |s|
        rp.report_mark_names_stop_playing_hook(s)
      end
      $play_hook.add_hook!("report-mark-names-play") do |samps|
        rp.report_mark_names_play_hook(samps)
      end
    end
  end

  # eval_between_marks

  add_help(:eval_between_marks,
           "eval_between_marks(&func)  \
Evaluates FUNC between the leftmost marks; \
FUNC takes one arg, the original sample.")
  def eval_between_marks(func1 = nil, &func2)
    func = if block_given?
             func2
           else
             func1
           end
    if proc?(func)
      snd = Snd.snd
      chn = Snd.chn
      if chn < 0
        chn = 0
      end
      if array?(mlist = marks(snd, chn)) and mlist.length > 1
        left_samp = left_sample(snd, chn)
        winl = false
        mlist.each_with_index do |n, i|
          if mark_sample(n) > left_samp
            winl = mlist[i..-1]
            break
          end
        end
        if array?(winl) and winl.length > 1
          beg = mark_sample(winl[0])
          len = mark_sample(winl[1]) - beg
          old_data = channel2vct(beg, len, snd, chn)
          new_data = Vct.new(len) do |i|
            func.call(old_data[i])
          end
          vct2channel(new_data, beg, len, snd, chn)
        end
      else
        snd_print("need 2 marks")
      end
    end
  end
  # bind_key(?m, 0,
  #          lambda do | |
  #            prompt_in_minibuffer("mark eval:", eval_between_marks)
  #          end)

  # snap_marks

  add_help(:snap_marks,
           "snap_marks()  \
Places marks at current selection boundaries.")
  def snap_marks
    if selection?
      selection_members.each do |snd, chn|
        pos = selection_position(snd, chn)
        len = selection_framples(snd, chn)
        add_mark(pos, snd, chn)
        add_mark(pos + len, snd, chn)
      end
    end
  end

  # define_selection_via_marks

  add_help(:define_selection_via_marks,
           "define_selection_via_marks(m1, m2)  \
Defines the current selection to lie between the marks given.")
  def define_selection_via_marks(m1, m2)
    m1sc = mark_home(m1)
    m2sc = mark_home(m2)
    if m1sc.eql?(m2sc)
      beg = [mark_sample(m1), mark_sample(m2)].min
      fin = [mark_sample(m1), mark_sample(m2)].max
      snd, chn = m1sc
      if selection?
        set_selection_member?(false, true)
      end
      set_selection_member?(true, snd, chn)
      set_selection_position(beg, snd, chn)
      set_selection_framples(fin - beg + 1, snd, chn)
    else
      Snd.raise(:snd_error,
                "define_selection_via_marks assumes the marks are \
in the same channel")
    end
  end

  # snap_mark_to_beat

  add_help(:snap_mark_to_beat,
           "snap_mark_to_beat()  \
Ensures that when a mark is dragged, \
its released position is always on a beat.")
  def snap_mark_to_beat
    mark_release = 4
    $mark_hook.add_hook!(get_func_name) do |m, snd, chn, reason|
      if reason == mark_release
        samp = mark_sample(m)
        bps = beats_per_minute(snd, chn) / 60.0
        sr = srate(snd)
        beat = ((samp * bps) / sr).floor.to_f
        lower = ((beat * sr) / bps).floor
        higher = (((beat + 1.0) * sr) / bps).floor
        set_mark_sample(m, if (samp - lower) < (higher - samp)
                             lower
                           else
                             higher
                           end)
        
      end
    end
  end

  # mark_explode
  # write out each section of a file between marks as a separate file

  add_help(:mark_explode,
           "mark_explode(header_type=Mus_next, data_format=Mus_bfloat)  \
Splits a sound into a bunch of sounds based on mark placements.")
  def mark_explode(htype = Mus_next, dformat = Mus_bfloat)
    start = 0
    file_ctr = 0
    snd = Snd.snd
    if marks(snd)
      marks(snd).first.each do |m|
        last = mark_sample(m)
        if last > start
          filename = format("mark-%d.snd", file_ctr)
          file_ctr += 1
          channels(snd).times do |chn|
            set_selection_member?(true, snd, chn)
            set_selection_position(start, snd, chn)
            set_selection_framples(last - start, snd, chn)
          end
          save_selection(filename,
                         :header_type, htype,
                         :sample_type, dformat,
                         :srate, srate(snd))
          channels(snd).times do |chn|
            set_selection_member?(false, snd, chn)
          end
        end
        start = last
      end
    end
    update_time_graph(snd)
  end
  
  #
  # === Mark Properties ===
  #
  add_help(:save_mark_properties,
           "save_mark_properties()  \
Sets up an $after_save_state_hook function to save any mark-properties.")
  def save_mark_properties
    $after_save_state_hook.add_hook!(get_func_name) do |fname|
      File.open(File.expand_path(fname), "a+") do |f|
        f.printf("\n# from %s in %s\n", get_func_name, __FILE__)
        f.printf("require \"marks.rb\"\n")
        (marks or []).each do |snds|
          (snds or []).each do |chns|
            (chns or []).each do |m|
              if mp = mark_properties(m)
                snd, chn = mark_home(m)
                msamp = mark_sample(m)
                f.printf("if sound?(snd = find_sound(%s))\n",
                         file_name(snd).inspect)
                f.printf("  if mark?(m = find_mark(%d, snd, %d))\n",
                         msamp, chn)
                f.printf("    set_mark_properties(m, %p)\n", mp)
                f.printf("  end\n")
                f.printf("end\n")
              end
            end
          end
        end
      end
    end
  end

  # 
  # === Mark Click Info ===
  # 
  add_help(:mark_click_info,
           "mark_click_info(n)  \
Is a $mark_click_hook function that describes a mark and its properties.")
  def mark_click_info(id)
    Snd.raise(:no_such_mark, id) unless mark?(id)
    mname = mark_name(id)
    mnamestr = ""
    if mname
      mnamestr = format("\n   mark name: %p", mname)
    end
    msamp = mark_sample(id)
    msync = mark_sync(id)
    msyncstr = ""
    if msync.nonzero?
      msyncstr = format("\n        sync: %d", msync)
    end
    props = mark_properties(id)
    propstr = ""
    if props
      propstr = format("\n  properties: %p", props)
    end
    info_dialog("Mark info",
                format("\
     mark id: %s%s
      sample: %d (%1.3f secs)%s%s",
                       id, mnamestr,
                       msamp, msamp / srate(mark_home(id)[0]).to_f,
                       msyncstr, propstr))
    true
  end

  # this code saves mark info in the sound file header, and reads it
  # back in when the sound is later reopened

  def marks2string(sndf)
    str = "require \"marks\"\n"
    (marks(sndf) or []).each_with_index do |chan_marks, chn|
      (chan_marks or []).each do |m|
        str += format("m = add_mark(%s, false, %d, %s, %d)\n",
                      mark_sample(m),
                      chn,
                      mark_name(m).null? ? false : mark_name(m).inspect,
                      mark_sync(m))
        if props = mark_properties(m)
          str += format("set_mark_properties(m, %s)\n", props.inspect)
        end
      end
    end
    str
  end
  # $output_comment_hook.add_hook!("marks2string") do |str|
  #   marks2string(selected_sound())
  # end
  # $after_open_hook.add_hook!("marks2string") do |snd|
  #   if string?(str = comment(snd))
  #     Snd.catch do eval(str, TOPLEVEL_BINDING, "(eval-header)", 1) end.first
  #   end
  # end
end

include Mark


# marks.rb ends here
