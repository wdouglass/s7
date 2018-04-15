# draw.rb -- draw.scm --> draw.rb

# Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 2005/04/05 00:17:04
# Changed: 2018/04/15 22:49:29

# examples of extensions to Snd's graphics
# 
# module Draw
#  display_colored_samples(color, beg, dur, snd = false, chn = false)
#  display_samples_in_color(snd, chn)
#  color_samples(color, beg = 0, dur = false, snd = Snd.snd, chn = Snd.chn)
#  uncolor_samples(snd = Snd.snd, chn = Snd.chn)
#  display_previous_edits(snd, chn)
#  overlay_sounds(*rest)
#  samples_via_colormap(snd, chn)
#  click_for_listener_help(pos)

require "extensions"

module Draw
  add_help(:display_colored_samples,
           "display_colored_samples(color, beg, dur, snd=false, chn=false)  \
Displays samples from beg for dur in color \
whenever they're in the current view.")
  def display_colored_samples(color, beg, dur, snd = false, chn = false)
    unless array?(color) and number?(beg) and number?(dur)
      return
    end
    left = left_sample(snd, chn)
    right = right_sample(snd, chn)
    len = beg + dur
    old_color = foreground_color(snd, chn)
    if left < len and right > beg
      cr = channel_widgets(snd, chn)[17]
      if vct?(data = make_graph_data(snd, chn))
        samps = [right, len].min - [left, beg].max
        offset = [0, beg - left].max
        new_data = data.subseq(offset, offset + samps)
        set_foreground_color(color, snd, chn)
        graph_data(new_data,
                   snd, chn,
                   Copy_context,
                   [beg, left].max,
                   [len, right].min, Graph_lines, cr)
        set_foreground_color(old_color, snd, chn)
      else
        low_data, high_data = data[0, 2]
        size = low_data.length
        samps = right - left
        left_offset = [0, beg - left].max
        left_bin = ((size.to_f * left_offset) / samps).floor
        right_offset = [len, right].min - left
        right_bin = ((size.to_f * right_offset) / samps).floor
        new_low_data = low_data.subseq(left_bin, right_bin)
        new_high_data = high_data.subseq(left_bin, right_bin)
        set_foreground_color(color, snd, chn)
        graph_data([new_low_data, new_high_data],
                   snd, chn, Copy_context, left_bin, right_bin, Graph_lines, cr)
        set_foreground_color(old_color, snd, chn)
      end
    end
  end

  def display_samples_in_color(snd, chn)
    col, beg, dur = channel_property(:colored_samples, snd, chn)
    display_colored_samples(col, beg, dur, snd, chn)
  end

  add_help(:color_samples,
           "color_samples(color, beg=0, dur=false, snd=false, chn=false)  \
Causes samples from BEG to BEG+DUR to be displayed in COLOR.")
  def color_samples(color, beg = 0, dur = false, snd = Snd.snd, chn = Snd.chn)
    unless $after_graph_hook.member?("display-samples-in-color")
      $after_graph_hook.add_hook!("display-samples-in-color") do |s, c|
        display_samples_in_color(s, c)
      end
    end
    unless dur then dur = framples(snd, chn) - beg end
    set_channel_property(:colored_samples, [color, beg, dur], snd, chn)
    update_time_graph(snd, chn)
  end

  add_help(:uncolor_samples,
           "uncolor_samples(snd=false, chn=false)  \
Cancels sample coloring in the given channel.")
  def uncolor_samples(snd = Snd.snd, chn = Snd.chn)
    set_channel_property(:colored_samples, [], snd, chn)
    update_time_graph(snd, chn)
  end

  add_help(:display_previous_edits,
           "display_previous_edits(snd, chn)  \
Displays all edits of the current sound, \
with older versions gradually fading away.")
  def display_previous_edits(snd, chn)
    edits = edit_position(snd, chn)
    if edits > 0
      old_color = foreground_color(snd, chn)
      clist = color2list(old_color)
      r = clist[0]
      g = clist[1]
      b = clist[2]
      rinc = (1.0 - r) / (edits + 1)
      ginc = (1.0 - g) / (edits + 1)
      binc = (1.0 - b) / (edits + 1)
      cr = channel_widgets(snd, chn)[17]
      re = 1.0 - rinc
      ge = 1.0 - ginc
      be = 1.0 - binc
      0.upto(edits) do |pos|
        data = make_graph_data(snd, chn, pos)
        set_foreground_color(make_color(re, ge, be), snd, chn)
        graph_data(data,
                   snd, chn, Copy_context,
                   false, false, time_graph_style(snd, chn), cr)
        re -= rinc
        ge -= ginc
        be -= binc
      end
      set_foreground_color(old_color, snd, chn)
    end
  end

  add_help(:overlay_sounds,
           "overlay_sounds(*rest)  \
Overlays onto its first argument all \
subsequent arguments: overlay_sounds(1, 0, 3)")
  def overlay_sounds(*rest)
    base = rest.shift
    if integer?(base)
      base = integer2sound(base)
    end
    $after_graph_hook.add_hook!(get_func_name) do |snd, chn|
      if snd == base
        cr = channel_widgets(snd, chn)[17]
        rest.each do |s|
          graph_data(make_graph_data(s, chn),
                     base, chn, Copy_context, false, false, Graph_dots, cr)
        end
      end
    end
  end

  add_help(:samples_via_colormap,
           "samples_via_colormap(snd, chn)  \
Displays time domain graph using current \
colormap (just an example of colormap_ref).")
  def samples_via_colormap(snd, chn)
    left = left_sample(snd, chn)
    old_color = foreground_color(snd, chn)
    if data = make_graph_data(snd, chn)
      cr = channel_widgets(snd, chn)[17]
      if vct?(data)
        data = [data]
      end
      data.each do |cur_data|
        x0 = x2position(left / srate(snd))
        y0 = y2position(cur_data[0])
        colors = make_array(colormap_size)
        j = 1
        (left + 1).upto(left + cur_data.length - 1) do |i|
          x1 = x2position(i / srate(snd))
          y1 = y2position(cur_data[j])
          x = cur_data[j].abs
          ref = (colormap_size * x).floor
          unless colors[ref]
            colors[ref] = make_color(*colormap_ref(colormap, x))
          end
          set_foreground_color(colors[ref], snd, chn)
          draw_line(x0, y0, x1, y1, snd, chn, Time_graph, cr)
          x0, y0 = x1, y1
          j += 1
        end
      end
      set_foreground_color(old_color, snd, chn)
    end
  end

  # click-for-listener-help

  $last_click_time = 0.0

  def click_for_listener_help(pos)
    time = Time.now.to_f
    if time - $last_click_time < 0.2
      $last_click_time = 0.0
      text = widget_text(main_widgets[4])
      if string?(text)
        subject = text.slice(text.rindex(/\b/m, pos)...text.index(/\b/m, pos))
        if string?(subject)
          help = snd_help(subject, false)
          if string?(help)
            help_dialog(subject, help)
          end
        end
      end
    else
      $last_click_time = time
    end
  end
  # $listener_click_hook.add_hook!("listener-help",
  #   &method(:click_for_listener_help).to_proc)
end

include Draw

# draw.rb ends here
