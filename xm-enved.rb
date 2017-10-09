# xm-enved.rb -- Translation of xm-enved.scm and enved.scm

# Translator/Author: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 03/03/18 00:18:35
# Changed: 14/11/13 04:56:16

# Tested with Snd 15.x
#             Ruby 2.x.x
#             Motif 2.3.3 X11R6
#
# module Snd_enved
#  channel_enved(snd, chn)
#  set_channel_enved(new_env, snd, chn)
#  channel_envelope(snd, chn)
#  set_channel_envelope(new_env, snd, chn)
#  mouse_press_envelope(snd, chn, button, state, x, y)
#  mouse_drag_envelope(snd, chn, button, state, x, y)
#  mouse_release_envelope(snd, chn, button, state, x, y, axis)
#  create_initial_envelopes(snd)
#  enveloping_key_press(snd, chn, key, state)
#  start_enveloping
#  stop_enveloping
#  play_with_envs(snd = false)
#
#  envelope?(obj)
#  make_enved(enved)
#  enved?(obj)
#  make_graph_enved(enved, snd, chn)
#  graph_enved?(obj)
#  make_xenved(name, parent, *rest)
#  xenved?(obj)
#  xenved_test(name)
#
# class Enved
#  initialize(enved)
#  inspect
#  to_s
#  envelope
#  envelope=(new_env)
#  reset
#  interp(x, base)
#  stretch(old_att, new_att, old_dec, new_dec)
#  stretch!(old_att, new_att, old_dec, new_dec)
#  scale(scale, offset)
#  scale!(scale, offset)
#  normalize(new_max)
#  normalize!(new_max)
#  reverse
#  reverse!
#  point(idx, *args)
#  min
#  max
#  length
#  first
#  last
#  first_x
#  last_x
#  first_y
#  last_y
#  in_range?(x)
#  each
#  each_with_index
#  map
#  map!
#
# class Graph_enved < Enved
#  initialize(enved, snd, chn)
#  click_time
#  click_time=(val)
#  before_enved_hook
#  after_enved_hook
#  inspect
#  to_s
#  reset
#  redraw
#  mouse_press_cb(x, y)
#  mouse_drag_cb(x, y)
#  mouse_release_cb
#
# class Xenved < Graph_enved
#  initialize(name, parent, enved, bounds, args, axis_label)
#  inspect
#  to_s
#  clear
#  envelope=(new_env)
#  axis_bounds
#  axis_bounds=(bounds)
#  point(idx, *args)
#  create
#  close

require "env"
require "hooks"
require "extensions"

if provided?(:snd_motif) or provided?(:snd_gtk)
  require "snd-xm"
  include Snd_XM
else
  $with_motif = false
  $with_gtk   = false
end

#
# defined in snd-xm.rb:
#
# $with_motif
# $with_gtk
#

$with_gui = ($with_motif or $with_gtk) ? true : false

module Snd_enved
  # returns Graph_enved object or nil
  def channel_enved(snd = false, chn = false)
    channel_property(:enved_envelope, snd, chn)
  end

  # sets Graph_enved object
  def set_channel_enved(new_ge, snd = false, chn = false)
    set_channel_property(:enved_envelope, new_ge, snd, chn)
  end
  
  add_help(:channel_envelope,
           "channel_envelope(snd=false, chn=false)  \
Returns the current enved envelope associated with SND's channel CHN.")
  def channel_envelope(snd = false, chn = false)
    if graph_enved?(ge = channel_enved(snd, chn))
      ge.envelope
    elsif envelope?(en = channel_property(:channel_envelope, snd, chn))
      set_channel_enved(make_graph_enved(en, snd, chn), snd, chn)
      en
    else
      nil
    end
  end

  def set_channel_envelope(new_env, snd = false, chn = false)
    set_channel_property(:channel_envelope, new_env, snd, chn)
    if graph_enved?(ge = channel_enved(snd, chn))
      ge.envelope = new_env
    else
      ge = make_graph_enved(new_env, snd, chn)
      set_channel_enved(ge, snd, chn)
    end
    ge
  end

  #   left button: set/delete point
  # middle button: reset to original env
  def mouse_press_envelope(snd, chn, button, state, x, y)
    case button
    when 1
      graph_enved?(ge = channel_enved(snd, chn)) and ge.mouse_press_cb(x, y)
    when 2
      graph_enved?(ge = channel_enved(snd, chn)) and ge.reset
    end
  end

  def mouse_drag_envelope(snd, chn, button, state, x, y)
    graph_enved?(ge = channel_enved(snd, chn)) and ge.mouse_drag_cb(x, y)
  end

  def mouse_release_envelope(snd, chn, button, state, x, y, axis)
    if axis == Lisp_graph
      graph_enved?(ge = channel_enved(snd, chn)) and ge.mouse_release_cb
      true
    else
      false
    end
  end

  def create_initial_envelopes(snd)
    channels(snd).times do |chn|
      set_dot_size(8, snd, chn)
      set_channel_envelope([0.0, 1.0, 1.0, 1.0], snd, chn)
    end
  end

  def enveloping_key_press(snd, chn, key, state)
    # C-g returns to original env
    # C-. applies current env to amplitude
    if key == ?. and state == 4
      env_channel((channel_envelope(snd, chn) or [0, 1, 1, 1]),
                  0, framples(snd, chn), snd, chn)
      true
    else
      if key == ?g and state == 4
        graph_enved?(ge = channel_enved(snd, chn)) and ge.reset
        true
      else
        false
      end
    end
  end

  Hook_name = "graph-enved"
  
  add_help(:start_enveloping,
           "start_enveloping()  \
Starts the enved processes, displaying an envelope editor in each channel.")
  def start_enveloping
    unless $after_open_hook.member?(Hook_name)
      $after_open_hook.add_hook!(Hook_name) do |snd|
        create_initial_envelopes(snd)
      end
      $mouse_press_hook.add_hook!(Hook_name) do |snd, chn, button, state, x, y|
        mouse_press_envelope(snd, chn, button, state, x, y)
      end
      $mouse_drag_hook.add_hook!(Hook_name) do |snd, chn, button, state, x, y|
        mouse_drag_envelope(snd, chn, button, state, x, y)
      end
      $mouse_click_hook.add_hook!(Hook_name) do |snd, chn, but, st, x, y, axis|
        mouse_release_envelope(snd, chn, but, st, x, y, axis)
      end
      $key_press_hook.add_hook!(Hook_name) do |snd, chn, key, state|
        enveloping_key_press(snd, chn, key, state)
      end
      true
    else
      false
    end
  end

  add_help(:stop_enveloping,
           "stop_enveloping()  \
Turns off the enved channel-specific envelope editors.")
  def stop_enveloping
    $after_open_hook.remove_hook!(Hook_name)
    $mouse_press_hook.remove_hook!(Hook_name)
    $mouse_drag_hook.remove_hook!(Hook_name)
    $mouse_click_hook.remove_hook!(Hook_name)
    $key_press_hook.remove_hook!(Hook_name)
    Snd.catch do
      set_lisp_graph?(false, Snd.snd, Snd.chn)
    end
    nil
  end

  # some examples

  add_help(:play_with_envs,
           "play_with_envs(snd=false)  \
Sets channel amps during playback from the associated enved envelopes.")
  def play_with_envs(snd = false)
    channel_envelope(snd, 0) or create_initial_envelopes(snd)
    channels(snd).times do |chn|
      player = make_player(snd, chn)
      e = make_env(:envelope, channel_envelope(snd, chn),
                   :length, (framples(snd, chn).to_f / dac_size).floor)
      add_player(player, 0, -1, -1,
                 lambda do |reason|
                   $play_hook.reset_hook!
                 end)
      $play_hook.add_hook!(get_func_name) do |fr|
        set_amp_control(env(e), player)
      end
    end
    start_playing(channels(snd), srate(snd))
  end

  def envelope?(obj)
    array?(obj) and obj.length >= 4 and obj.length.even?
  end

  def make_enved(enved = [0, 0, 1, 1])
    assert_type(envelope?(enved), enved, 0,
                "an envelope, at least 2 points [x0, y0, x1, y1, ...]")
    Enved.new(enved)
  end

  def enved?(obj)
    obj.instance_of?(Enved)
  end

  def make_graph_enved(enved = [0, 0, 1, 1], snd = Snd.snd, chn = Snd.chn)
    assert_type(envelope?(enved), enved, 0,
                "an envelope, at least 2 points [x0, y0, x1, y1, ...]")
    (ge = Graph_enved.new(enved, snd, chn)).redraw
    ge
  end

  def graph_enved?(obj)
    obj.instance_of?(Graph_enved)
  end

  if $with_gui
    def make_xenved(name, parent, *rest)
      envelope, bounds, args, label = optkey(rest,
                                             [:envelope, [0, 0, 1, 1]],
                                             [:axis_bounds, [0, 1, 0, 1]],
                                             [:args, []],
                                             :axis_label)
      unless string?(name) and (not name.empty?)
        name = "xenved"
      end
      assert_type(widget?(parent), parent, 1, "a widget")
      assert_type((array?(bounds) and bounds.length == 4), bounds, 3,
                  "an array of 4 elements [x0, x1, y0, y1]")
      unless array?(label) and label.length == 4
        label = bounds
      end
      Xenved.new(name, parent, envelope, bounds, args, label)
    end

    def xenved?(obj)
      obj.instance_of?(Xenved)
    end

    if $with_motif
      Test_widget_type = RxmFormWidgetClass
      Test_widget_args = [RXmNheight, 200]
      Test_xenved_args = [RXmNleftAttachment,   RXmATTACH_WIDGET,
                          RXmNtopAttachment,    RXmATTACH_WIDGET,
                          RXmNbottomAttachment, RXmATTACH_WIDGET,
                          RXmNrightAttachment,  RXmATTACH_WIDGET]
    else
      Test_widget_type = false
      Test_widget_args = false
      Test_xenved_args = []
    end
    
    def xenved_test(name = "xenved")
      make_xenved(name,
                  add_main_pane(name, Test_widget_type, Test_widget_args),
                  :envelope,    [0, 0, 1, 1],
                  :axis_bounds, [0, 1, 0, 1],
                  :args,        Test_xenved_args)
    end
  end
end

include Snd_enved

class Enved
  include Enumerable
  include Info

  def initialize(enved = [0, 0, 1, 1])
    (@envelope = enved).map! do |x|
      x.to_f
    end
    @init = @envelope.dup
    set_enved_help
  end
  attr_reader :envelope
  
  def inspect
    format("%s.new(%s)", self.class, @envelope)
  end
  
  def to_s
    format("#<%s: envelope: %s>", self.class, @envelope.to_string)
  end

  def envelope=(enved)
    assert_type(envelope?(enved), enved, 0,
                "an envelope, at least 2 points [x0, y0, x1, y1, ...]")
    @envelope = enved.map do |x|
      x.to_f
    end
  end

  def reset
    @envelope = @init.dup
  end

  def interp(x, base = 0)
    envelope_interp(x, @envelope, base)
  end

  def stretch(old_att = nil, new_att = nil, old_dec = nil, new_dec = nil)
    stretch_envelope(@envelope, old_att, new_att, old_dec, new_dec)
  end

  def stretch!(old_att = nil, new_att = nil, old_dec = nil, new_dec = nil)
    self.envelope = self.stretch(old_att, new_att, old_dec, new_dec)
  end

  def scale(scale = 1.0, offset = 0.0)
    scale_envelope(@envelope, scale, offset)
  end

  def scale!(scale = 1.0, offset = 0.0)
    self.envelope = self.scale(scale, offset)
  end

  def normalize(new_max = 1.0)
    self.scale(new_max / self.max)
  end

  def normalize!(new_max = 1.0)
    self.envelope = self.normalize(new_max)
  end
  
  def reverse
    reverse_envelope(@envelope)
  end
  
  def reverse!
    self.envelope = self.reverse
  end

  def point(idx, *args)
    x, y = optkey(args, :x, :y)
    if x
      @envelope[idx * 2] = x
    end
    if y
      @envelope[idx * 2 + 1] = y
    end
    @envelope[idx * 2, 2]
  end
  
  def min
    min_envelope(@envelope)
  end

  def max
    max_envelope(@envelope)
  end

  def length
    @envelope.length / 2
  end
  
  def first
    if @envelope.length > 1
      @envelope[0, 2]
    else
      [0.0, 0.0]
    end
  end

  def last
    if @envelope.length > 3
      @envelope[-2, 2]
    else
      [1.0, 0.0]
    end
  end

  def first_x
    @envelope[0]
  end

  def first_y
    @envelope[1]
  end
  
  def last_x
    @envelope[-2]
  end
  
  def last_y
    @envelope[-1]
  end

  def in_range?(x)
    x > @envelope[0] and x < @envelope[-2]
  end

  def each
    0.step(@envelope.length - 1, 2) do |i|
      yield(@envelope[i, 2])
    end
    @envelope
  end

  def each_with_index
    0.step(@envelope.length - 1, 2) do |i|
      yield(@envelope[i, 2] + [i])
    end
    @envelope
  end

  def map
    res = make_array(@envelope.length)
    0.step(@envelope.length - 1, 2) do |i|
      res[i, 2] = yield(@envelope[i, 2])
    end
    res
  end

  def map!
    0.step(@envelope.length - 1, 2) do |i|
      @envelope[i, 2] = yield(@envelope[i, 2])
    end
    @envelope
  end
  
  private
  def set_enved_help
    self.description = "\
# make_enved(env)
#
# class Enved
#   initialize(env)
#
# getter and setter:
#   envelope=(new_env)
#   envelope
#
# methods:
#   interp(x, base)
#   stretch(oatt, natt, odec, ndec) stretch!(oatt, natt, odec, ndec)
#   scale(scale, offset)            scale!(scale, offset)
#   normalize(new_max)              normalize!(new_max)
#   reverse                         reverse!
#   max                             min
#   first   (first [x, y])          last   (last [x, y])
#   first_x                         last_x
#   first_y                         last_y
#   map do |x, y| ... end           map! do |x, y| ... end
#   each do |x, y| ... end          each_with_index do |x, y, i| ... end
#   length
#   point(idx, *args)               # point(idx) ==> [x, y]
#                                   # point(idx, :x, x_val, :y, y_val)
#                                   # sets x, y or both and returns new [x, y]
#   in_range?(x)   (x > first_x and x < last_x)
#   help           (alias info and description)
"
    add_help(:Enved, self.description)
  end
end

class Graph_enved < Enved
  def initialize(enved, snd = false, chn = false)
    super(enved)
    @graph_name = short_file_name(snd)
    @snd = snd
    @chn = chn
    @before_enved_hook = Hook.new("@before_enved_hook", 4, "\
lambda do |pos, x, y, reason| ... end: called before changing a \
breakpoint in @envelope.  This hook runs the global $enved_hook as \
first hook, subsequent procedures can directly manipulate @envelope.

This instance hook is like the global $enved_hook; POS is @envelope's \
x-position, X and Y are the new points, and REASON is one of \
Enved_add_point, Enved_delete_point, Enved_move_point.  If the last \
hook procedure in the hook list returns `false', the class changes the \
breakpoint, otherwise the hook procedures are responsible for \
manipulating @envelope itself.

From dlocsig.rb:

@velocity = make_xenved(\"velocity (v)\", frame,
                        :envelope, [0.0, 0.0, 1.0, 0.0],
                        :axis_bounds, [0.0, 1.0, 0.0, 1.0],
                        :axis_label, [-20.0, 20.0, 0.0, 2.0])
@velocity.before_enved_hook.reset_hook!   # to prevent running $enved_hook
@velocity.before_enved_hook.add_hook!(\"dlocsig-hook\") do |pos, x, y, reason|
  if reason == Enved_move_point
    if @velocity.in_range?(x)
      old_x = @velocity.point(pos).first
      @velocity.stretch!(old_x, x)
      @velocity.point(pos, :y, y)
    else
      false
    end
  else
    false
  end
end

In contrast the same procedure on the global $enved_hook:

$enved_hook.add_hook!(\"snd-init-hook\") do |env, pt, x, y, reason|
  if reason == Enved_move_point
    if x > 0.0 and x < env[-2]
      old_x = env[2 * pt]
      new_env = stretch_envelope(env, old_x, x)
      new_env[pt * 2 + 1] = y
      new_env
    else
      # env               # first and last points are fixed
      false               # first and last points can be moved
    end
  else
    false
  end
end")
    hn = "initialize-xm-enved-hook"
    @before_enved_hook.add_hook!(hn) do |pos, x, y, reason|
      if $enved_hook.empty?
        false
      else
        e = nil
        $enved_hook.run_hook do |prc|
          case e = prc.call(@envelope.dup, pos, x, y, reason)
          when Array
            self.envelope = e
          when Enved, Graph_enved, Xenved
            self.envelope = e.envelope
          end
        end
        e.class != FalseClass
      end
    end
    @after_enved_hook = Hook.new("@after_enved_hook", 2, "\
lambda do |pos, reason| ... end: called after redrawing new or changed \
breakpoints.  POS is @envelope's x-position, and REASON is one of \
Enved_add_point, Enved_delete_point, Enved_move_point.")
    init
    set_enved_help
  end
  alias help description
  attr_accessor :click_time
  attr_reader :before_enved_hook
  attr_reader :after_enved_hook
  
  def inspect
    format("%s.new(%s, %s, %s)", self.class, @envelope, @snd, @chn)
  end
  
  def to_s
    format("#<%s: %s[%s:%s]: %s>",
           self.class, @graph_name, @snd, @chn, @envelope.to_string)
  end

  def init
    @mouse_up   = 0.0
    @mouse_down = 0.0
    @click_time = 0.5
    @mouse_pos  = 0
    @mouse_new  = false
  end

  def envelope=(new_env)
    super
    self.redraw
    @envelope
  end
  
  def reset
    super
    self.redraw
    init
    @envelope
  end

  def redraw
    graph(@envelope, @graph_name, 0.0, 1.0, 0.0, 1.0, @snd, @chn)
    update_lisp_graph(@snd, @chn)
  end
  
  Mouse_radius = 0.03

  # To prevent unexpected point movements if position is near first or
  # last point.
  Secure_distance = 0.001

  def mouse_press_cb(x, y)
    x = [0.0, [x, 1.0].min].max
    y = [0.0, [y, 1.0].min].max
    pos = false
    self.each_with_index do |x1, y1, i|
      if (x1 - x).abs < Mouse_radius and (y1 - y).abs < Mouse_radius
        pos = i
        break
      end
    end
    @mouse_new = (not pos)
    @mouse_down = Time.now.to_f
    if pos
      @mouse_pos = pos
    else
      x = [Secure_distance, [x, 1.0 - Secure_distance].min].max
      if run_before_enved_hook(x, y, Enved_add_point)
        add_envelope_point(x, y)
      end
      self.redraw
      @after_enved_hook.call(@mouse_pos / 2, Enved_add_point)
    end
  end
  
  def mouse_drag_cb(x, y)
    lx = if @mouse_pos.zero?
           @envelope[0]
         elsif @mouse_pos >= (@envelope.length - 2)
           @envelope[-2]
         else
           [@envelope[@mouse_pos - 2] + Secure_distance,
            [x, @envelope[@mouse_pos + 2] - Secure_distance].min].max
         end
    #ly = [0.0, [y, 1.0].min].max
    ly = y
    if run_before_enved_hook(lx, ly, Enved_move_point)
      @envelope[@mouse_pos, 2] = [lx, ly]
    end
    self.redraw
    @after_enved_hook.call(@mouse_pos / 2, Enved_move_point)
  end
  
  def mouse_release_cb
    @mouse_up = Time.now.to_f
    if (not @mouse_new) and (@mouse_up - @mouse_down) <= @click_time and
        @mouse_pos.nonzero? and @mouse_pos < (@envelope.length - 2)
      if run_before_enved_hook(@envelope[@mouse_pos], @envelope[@mouse_pos + 1],
                               Enved_delete_point)
        @envelope.slice!(@mouse_pos, 2)
      end
      self.redraw
      @after_enved_hook.call(@mouse_pos / 2, Enved_delete_point)
    end
    @mouse_new = false
  end

  protected
  # If the last hook procedure returns false, change the envelope,
  # otherwise the hook procedure is responsible.
  def run_before_enved_hook(x, y, reason)
    if @before_enved_hook.empty?
      true
    else
      e = nil
      @before_enved_hook.run_hook do |prc|
        e = prc.call(@mouse_pos / 2, x, y, reason)
      end
      e.class == FalseClass
    end
  end
  
  def add_envelope_point(x, y)
    idx = @mouse_pos
    test_env = @envelope.to_pairs
    if cur_pair = test_env.assoc(x)
      idx = test_env.index(cur_pair) * 2
      @envelope[idx + 1] = y
    else
      cur_pair = test_env.detect do |pair|
        x < pair[0]
      end
      if cur_pair
        idx = test_env.index(cur_pair) * 2
        @envelope.insert(idx, x, y)
      end
    end
    @mouse_pos = idx
  end

  private
  def set_enved_help
    super
    self.description += "\
#
# make_graph_enved(enved, snd, chn)
#
# class Graph_enved < Enved (see enved.scm)
#   initialize(enved, snd, chn)
#   before_enved_hook              lambda do |pos, x, y, reason| ... end
#   after_enved_hook               lambda do |pos, reason| ... end
#
# getter and setter:
#   click_time=(val)
#   click_time
#
# interactive methods:
#   init
#   reset
#   redraw
#   mouse_press_cb(x, y)
#   mouse_drag_cb(x, y)
#   mouse_release_cb
#   help     (alias info and description)

# more examples in xm-enved.rb, module Snd_enved

ge = make_graph_enved([0, 0, 1, 1], 0, 0)
ge.envelope                         # ==> [0.0, 0.0, 1.0, 1.0]
ge.click_time                       # ==> 0.2
ge.envelope = [0, 1, 1, 1]
ge.help                             # this help
"
    add_help(:Graph_enved, self.description)
  end
end

if $with_gui
  class Xenved < Graph_enved
    def initialize(name, parent, enved, bounds, args, axis_label)
      super(enved)
      @name = name
      @parent = parent
      @x0, @x1, @y0, @y1 = bounds.map do |x|
        x.to_f
      end
      @args = args
      if $with_motif
        unless @args.member?(RXmNforeground)
          @args += [RXmNforeground, data_color]
        end
        unless @args.member?(RXmNbackground)
          @args += [RXmNbackground, graph_color]
        end
      end
      @lx0, @lx1, @ly0, @ly1 = if envelope?(axis_label)
                                 axis_label.map do |x|
                                   x.to_f
                                 end
                               else
                                 [0.0, 1.0, -1.0, 1.0]
                               end
      @gc = snd_gcs[0]
      @drawer = @dpy = @window = nil
      @px0 = @px1 = @py0 = @py1 = nil
      @dragging = false
      set_enved_help
      create
    end
    alias help description
    
    def inspect
      format("%s.new(%p, %s, %s, %s, %s, %s)",
             self.class,
             @name,
             @parent,
             @envelope,
             [@x0, @x1, @y0, @y1],
             @args,
             [@lx0, @lx1, @ly0, @ly1])
    end
    
    def to_s
      format("#<%s: name: %p, envelope: %s>",
             self.class, @name, @envelope.to_string)
    end

    def axis_bounds
      [@x0, @x1, @y0, @y1]
    end
    
    def axis_bounds=(bounds)
      assert_type((array?(bounds) and bounds.length == 4), bounds, 0,
                  "an array of 4 elements [x0, x1, y0, y1]")
      @x0, @x1, @y0, @y1 = bounds.map do |x|
        x.to_f
      end
      self.envelope = @init
    end

    def point(idx, *args)
      if args.length > 0
        super
        redraw
      end
      @envelope[idx * 2, 2]
    end

    def create
      if widget?(@drawer)
        show_widget(@drawer)
      else
        create_enved
      end
    end
    alias open create
    
    def close
      hide_widget(@drawer)
    end

    protected
    if $with_motif
      def redraw
        if is_managed?(@drawer) and @px0 and @py0 > @py1
          RXClearWindow(@dpy, @window)
          # Motif's DRAW-AXES takes 6 optional arguments.
          # '( x0 y0 x1 y1 ) = draw-axes(wid gc label
          #                              x0=0.0 x1=1.0 y0=-1.0 y1=1.0
          #                              style=x-axis-in-seconds
          #                              axes=show-all-axes)
          # arity #( 3 6 #f )
          draw_axes(@drawer, @gc, @name, @lx0, @lx1, @ly0, @ly1)
          lx = ly = nil
          @envelope.each_pair do |x, y|
            cx = grfx(x)
            cy = grfy(y)
            RXFillArc(@dpy, @window, @gc,
                      cx - Mouse_r, cy - Mouse_r, Mouse_d, Mouse_d, 0, 360 * 64)
            if lx
              RXDrawLine(@dpy, @window, @gc, lx, ly, cx, cy)
            end
            lx, ly = cx, cy
          end
        end
      end
    else
      def redraw
        if is_managed?(@drawer) and @px0 and @py0 > @py1
          size = widget_size(RGTK_WIDGET(@drawer))
          cairo = make_cairo(@drawer)
          Rcairo_push_group(cairo)
          Rcairo_set_source_rgb(cairo, 1.0, 1.0, 1.0)
          Rcairo_rectangle(cairo, 0, 0, size[0], size[1])
          Rcairo_fill(cairo)
          # Gtk's DRAW-AXES takes one more optional argument, a cairo object.
          # '( x0 y0 x1 y1 ) = draw-axes(wid gc label
          #                              x0=0.0 x1=1.0 y0=-1.0 y1=1.0
          #                              style=x-axis-in-seconds
          #                              axes=show-all-axes
          #                              cairo)
          # arity #( 3 7 #f )
          draw_axes(@drawer, @gc, @name, @lx0, @lx1, @ly0, @ly1,
                    X_axis_in_seconds, Show_all_axes, cairo)
          Rcairo_set_line_width(cairo, 1.0)
          Rcairo_set_source_rgb(cairo, 0.0, 0.0, 0.0)
          lx = ly = nil
          @envelope.each_pair do |x, y|
            cx = grfx(x)
            cy = grfy(y)
            Rcairo_arc(cairo, cx, cy, Mouse_r, 0.0, TWO_PI)
            Rcairo_fill(cairo)
            if lx
              Rcairo_move_to(cairo, lx, ly)
              Rcairo_line_to(cairo, cx, cy)
              Rcairo_stroke(cairo)
            end
            lx, ly = cx, cy
          end
          Rcairo_pop_group_to_source(cairo)
          Rcairo_paint(cairo)
          # Rcairo_destroy(cairo)
          free_cairo(cairo)
        end
      end
    end
    
    private
    def set_enved_help
      super
      self.description += "\
#
# make_xenved(name, parent, *rest)
#   name     String
#   parent   Widget
#   *rest
#     :envelope,    [0, 0, 1, 1]   x0, y0, x1, y1, ...
#     :axis_bounds, [0, 1, 0, 1]   x0, x1, y0, y1
#     :args,        []             Motif properties
#     :axis_label,  nil            if axes labels should have
#                                  other values than axis_bounds,
#                                  (see dlocsig.rb)
#
# class Xenved < Graph_enved (see xm-enved.scm)
#   initialize(name, parent, env, axis_bounds, args, axis_label)
#
# getter and setter:
#   axis_bounds=(new_bounds)
#   axis_bounds
#
# interactive methods:
#   create   (alias open)
#   close
#   reset
#   help     (alias info and description)

# more examples in effects.rb

xe = xenved_test
xe.envelope                         # ==> [0.0, 0.0, 1.0, 1.0]
xe.click_time                       # ==> 0.5
xe.envelope = [0, 1, 1, 1]
# some clicks later
xe.envelope                         # ==> [0.0, 0.0,
                                    #      0.190735694822888, 0.562264150943396,
                                    #      0.632152588555858, 0.932075471698113,
                                    #      0.848773841961853, 0.316981132075472,
                                    #      1.0, 1.0]
xe.help                             # this help
"
      add_help(:Xenved, self.description)
    end

    Mouse_d = 10
    Mouse_r = 5

    if $with_motif
      def create_enved
        @drawer = RXtCreateManagedWidget(@name, RxmDrawingAreaWidgetClass,
                                         @parent, @args)
        @dpy = RXtDisplay(@drawer)
        @window = RXtWindow(@drawer)
        RXtAddCallback(@drawer, RXmNresizeCallback,
                       lambda do |w, c, i|
                         draw_axes_cb
                       end)
        RXtAddCallback(@drawer, RXmNexposeCallback,
                       lambda do |w, c, i|
                         draw_axes_cb
                       end)
        RXtAddEventHandler(@drawer, RButtonPressMask, false,
                           lambda do |w, c, e, f|
                             mouse_press_cb(ungrfx(Rx(e)), ungrfy(Ry(e)))
                           end)
        RXtAddEventHandler(@drawer, RButtonReleaseMask, false,
                           lambda do |w, c, e, f|
                             mouse_release_cb
                           end)
        RXtAddEventHandler(@drawer, RButtonMotionMask, false,
                           lambda do |w, c, e, f|
                             mouse_drag_cb(ungrfx(Rx(e)), ungrfy(Ry(e)))
                           end)
        RXtAddEventHandler(@drawer, REnterWindowMask, false,
                           lambda do |w, cursor, e, f|
                             RXDefineCursor(@dpy, @window, cursor)
                           end, RXCreateFontCursor(@dpy, RXC_crosshair))
        RXtAddEventHandler(@drawer, RLeaveWindowMask, false,
                           lambda do |w, c, e, f|
                             RXUndefineCursor(@dpy, @window)
                           end)
      end

    else
      def create_enved
        @drawer = Rgtk_drawing_area_new()
        Rgtk_widget_set_events(@drawer, RGDK_ALL_EVENTS_MASK)
        Rgtk_box_pack_start(RGTK_BOX(@parent), @drawer, true, true, 10)
        Rgtk_widget_show(@drawer)
        Rgtk_widget_set_name(@drawer, @name)
        Rgtk_widget_set_size_request(@drawer, -1, 200)
        evname = provided?(:gtk3) ? "draw" : "expose_event"
        add_event_handler(@drawer, evname) do |w, e, d|
          draw_axes_cb
          false
        end
        add_event_handler(@drawer, "configure_event") do |w, e, d|
          draw_axes_cb
          false
        end
        add_event_handler(@drawer, "button_press_event") do |w, e, d|
          @dragging = true
          xy = Rgdk_event_get_coords(RGDK_EVENT(e))
          mouse_press_cb(ungrfx(xy[1]), ungrfy(xy[2]))
          false
        end
        add_event_handler(@drawer, "button_release_event") do |w, e, d|
          @dragging = false
          mouse_release_cb
          false
        end
        add_event_handler(@drawer, "motion_notify_event") do |w, e, d|
          if @dragging
            xy = Rgdk_event_get_coords(RGDK_EVENT(e))
            mouse_drag_cb(ungrfx(xy[1]), ungrfy(xy[2]))
          end
          false
        end
        add_event_handler(@drawer,
                          "enter_notify_event",
                          Rgdk_cursor_new(RGDK_CROSSHAIR)) do |w, e, cursor|
          Rgdk_window_set_cursor(Rgtk_widget_get_window(w), cursor)
          false
        end
        add_event_handler(@drawer,
                          "leave_notify_event",
                          Rgdk_cursor_new(RGDK_LEFT_PTR)) do |w, e, cursor|
          Rgdk_window_set_cursor(Rgtk_widget_get_window(w), cursor)
          false
        end
      end
    end

    if $with_motif
      # Motif's DRAW_AXES takes 3 required and 6 optional arguments.
      # draw_axes(wid, gc, label,
      #           x0=0.0, x1=1.0, y0=-1.0, y1=1.0,
      #           style=X_axis_in_seconds,
      #           axes=Show_all_axes)
      def draw_axes_cb
        @px0, @py0, @px1, @py1 = draw_axes(@drawer, @gc, @name,
                                           @lx0, @lx1, @ly0, @ly1,
                                           X_axis_in_seconds,
                                           Show_all_axes)
        redraw
      end
    end

    if $with_gtk
      # Gtk's DRAW_AXES takes 3 required and 7 optional arguments.
      # draw_axes(wid, gc, label,
      #           x0=0.0, x1=1.0, y0=-1.0, y1=1.0,
      #           style=X_axis_in_seconds,
      #           axes=Show_all_axes,
      #           cairo)
      def draw_axes_cb
        cairo = make_cairo(@drawer)
        @px0, @py0, @px1, @py1 = draw_axes(@drawer, @gc, @name,
                                           @lx0, @lx1, @ly0, @ly1,
                                           X_axis_in_seconds,
                                           Show_all_axes,
                                           cairo)
        free_cairo(cairo)
        redraw
      end
    end

    def ungrfx(x)
      if @px0 == @px1
        @x0
      else
        [@x1,
         [@x0, @x0 + ((@x1 - @x0) * ((x - @px0) / (@px1.to_f - @px0)))].max].min
      end
    end
    
    def ungrfy(y)
      if @py0 == @py1
        @y1
      else
        [@y1,
         [@y0, @y0 + ((@y1 - @y0) * ((@py0 - y) / (@py0.to_f - @py1)))].max].min
      end
    end

    def grfx(x)
      if @px0 == @px1
        @px0
      else
        [@px1,
         [@px0,
          (@px0 +
           ((@px1 - @px0) * ((x - @x0) / (@x1.to_f - @x0)))).round].max].min
      end
    end

    def grfy(y)
      if @py0 == @py1
        @py0
      else
        [@py0,
         [@py1,
          (@py1 +
           ((@py0 - @py1) * ((y - @y1) / (@y0.to_f - @y1)))).round].max].min
      end
    end
  end
end

# xm-enved.rb ends here
