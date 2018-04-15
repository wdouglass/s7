# musglyphs.rb -- musglyphs.scm and cmn-glyphs.lisp --> musglyphs.rb

# Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 2005/04/06 00:47:44
# Changed: 2018/04/15 22:56:24

# Commentary:
#
# module Musglyphs
# 
#  class SND_Draw
#   initialize(*args)
#   inspect
#   to_s
#   comment
#   snd
#   chn
#   moveto(x, y)
#   rmoveto(x, y)
#   curveto(x0, y0, x1, y1, x2, y2)
#   lineto(x, y)
#   rlineto(x, y)
#   draw
#   fill_in
#   draw_or_fill_in
#   circle(x0, y0, rad)
#
#  draw_treble_clef(*args)
#  draw_percussion_clef(*args)
#  draw_c_clef(*args)
#  draw_bass_clef(*args)
#
#  draw_turn(*args)
#  draw_mordent(*args)
#  draw_double_mordent(*args)
#  draw_trill_section(*args)
#  draw_trill_sections(count, *args)
#  draw_arpeggio(*args)
#  draw_arpeggios(count, *args)
#  draw_tr(*args)
#  draw_accent(*args)
#  draw_tnecca(*args)
#
#  draw_breath_mark(*args)
#  draw_caesura(*args)
#  draw_fermata(*args)
#  draw_upside_down_fermata(*args)
#
#  draw_repeat_sign(*args)
#  draw_upper_bracket(*args)
#  draw_lower_bracket(*args)
#  draw_segno(*args)
#  draw_coda(*args)
#  draw_pedal_off(*args)
#  draw_ped(*args)
#  draw_left_paren(*args)
#  draw_right_paren(*args)
#  draw_wedge(*args)
#  draw_down_bow(*args)
#  draw_up_bow(*args)
#
#  draw_zero(*args)
#  draw_one(*args)
#  draw_two(*args)
#  draw_three(*args)
#  draw_four(*args)
#  draw_five(*args)
#  draw_six(*args)
#  draw_seven(*args)
#  draw_eight(*args)
#  draw_nine(*args)
#  draw_common_time(*args)
#  draw_cut_time(*args)
#  draw_plus(*args)
#
#  draw_sharp(*args)
#  draw_flat(*args)
#  draw_double_sharp(*args)
#  draw_natural(*args)
#  draw_double_flat(*args)
#
#  draw_f(*args)
#  draw_p(*args)
#  draw_lig_p(*args)
#  draw_m(*args)
#  draw_n(*args)
#  draw_niente(*args)
#  draw_subito(*args)
#  draw_z(*args)
#  draw_s(*args)
#  draw_r(*args)
#
#  draw_double_whole_note(*args)
#  draw_whole_note(*args)
#  draw_half_note(*args)
#  draw_quarter_note(*args)
#  draw_diamond(*args)
#  draw_diamond_1(*args)
#  draw_filled_diamond_1(*args)
#  draw_rhythmX(*args)
#  draw_circled_x(*args)
#  draw_slash(*args)
#  draw_mslash(*args)
#  draw_triangle(*args)
#  draw_square(*args)
#  draw_8th_flag_up(*args)
#  draw_extend_flag_up(*args)
#  draw_8th_flag_down(*args)
#  draw_extend_flag_down(*args)
#
#  draw_draw_whole_rest(*args)
#  draw_half_rest(*args)
#  draw_quarter_rest(*args)
#  draw_8th_rest(*args)
#  draw_16th_rest(*args)
#  draw_32nd_rest(*args)
#  draw_64th_rest(*args)
#  draw_128th_rest(*args)
#  draw_measure_rest(*args)
#  draw_double_whole_rest(*args)
#
# Code:

module Musglyphs
  class SND_Draw
    def initialize(com, *args)
      @comment = com
      @xoff  = Float((args[0] or 0.0))
      @yoff  = Float((args[1] or 0.0))
      @size  = Float((args[2] or 50.0))
      @style = (args[3] or 0)                     # 0: fill_in else draw
      @snd   = (args[4] or 0)                     # (integer or true/false)
      @chn   = (args[5] or 0)                     # (integer or true/false)
      @ax    = (args[6] or 0)                     # axis-context (integer or false)
      @curx  = 0.0
      @cury  = 0.0
      @pathlist = []
    end
    attr_reader :comment, :snd, :chn

    def inspect
      format("#<%s[%s:%s] offset: [%1.1f, %1.1f], size: %1.1f, style: %s, %s>",
             self.class, @snd, @chn, @xoff, @yoff, @size, @style, @comment.inspect)
    end
    alias to_s inspect
    
    def moveto(x, y)
      @curx = x.to_f
      @cury = y.to_f
    end

    def rmoveto(x, y)
      @curx += x.to_f
      @cury += y.to_f
    end

    def curveto(x0, y0, x1, y1, x2, y2)
      @pathlist.push(make_bezier(to_x(@curx), to_y(@cury),
                                 to_x(x0), to_y(y0),
                                 to_x(x1), to_y(y1),
                                 to_x(x2), to_y(y2),
                                 50))
      @curx = x2.to_f
      @cury = y2.to_f
    end

    def lineto(x, y)
      @curx = x.to_f
      @cury = y.to_f
      @pathlist.push(to_x(x), to_y(y))
    end

    def rlineto(x, y)
      @curx += x.to_f
      @cury += y.to_f
      @pathlist.push(to_x(@curx), to_y(@cury))
    end

    def draw
      unless @pathlist.empty?
        cr = channel_widgets(@snd, @chn)[17]
        draw_lines(@pathlist.flatten.compact, @snd, @chn, @ax, cr)
      end
      @pathlist.clear
      self
    end

    def fill_in
      unless @pathlist.empty?
        cr = channel_widgets(@snd, @chn)[17]
        fill_polygon(@pathlist.flatten.compact, @snd, @chn, @ax, cr)
      end
      @pathlist.clear
      self
    end

    def draw_or_fill_in
      if number?(@style) and @style.nonzero?
        draw
      else
        fill_in
      end
    end
    
    def circle(x0, y0, rad)
      cr = channel_widgets(@snd, @chn)[17]
      draw_dot(to_x(x0), to_y(y0), (@size * rad * 2).floor, @snd, @chn, @ax, cr)
      self
    end

    private
    def to_x(x)
      (@xoff + @size * x).floor
    end

    def to_y(y)
      (@yoff - @size * y).floor
    end
  end

  # 
  # cmn-glyphs.lisp
  #
  # CLEFS
  #
  # all draw functions have optional *args
  # *args: x-offset (0.0)
  #        y-offset (0.0)
  #        size     (50.0)
  #        style    (0)     # 0: fill_in else draw
  #        snd      (0)     # (integer or true/false)
  #        chn      (0)     # (integer or true/false)
  #        ax       (0)     # axis-context (integer or false)
  def draw_treble_clef(*args)
    score = SND_Draw.new("treble clef", *args)
    score.moveto(0.490, -0.258)
    score.curveto(0.516, -0.430, 0.546, -0.672, 0.298, -0.590)
    score.curveto(0.404, -0.580, 0.432, -0.436, 0.320, -0.398)
    score.curveto(0.210, -0.398, 0.180, -0.518, 0.256, -0.600)
    score.curveto(0.290, -0.622, 0.310, -0.630, 0.338, -0.638)
    score.curveto(0.576, -0.668, 0.554, -0.402, 0.522, -0.252)
    score.curveto(0.892, -0.126, 0.746, 0.314, 0.442, 0.236)
    score.curveto(0.436, 0.286, 0.410, 0.388, 0.420, 0.440)
    score.curveto(0.430, 0.490, 0.484, 0.558, 0.510, 0.606)
    score.curveto(0.624, 0.814, 0.616, 1.000, 0.496, 1.108)
    score.curveto(0.410, 1.118, 0.348, 0.888, 0.348, 0.744)
    score.curveto(0.348, 0.696, 0.364, 0.618, 0.348, 0.576)
    score.curveto(0.332, 0.530, 0.290, 0.482, 0.264, 0.440)
    score.curveto(0.152, 0.260, 0.054, 0.082, 0.182, -0.120)
    score.curveto(0.256, -0.238, 0.358, -0.258, 0.490, -0.258)
    score.moveto(0.394, 0.622)
    score.curveto(0.374, 0.696, 0.370, 0.978, 0.512, 0.996)
    score.curveto(0.666, 0.948, 0.454, 0.668, 0.394, 0.622)
    score.moveto(0.382, 0.398)
    score.lineto(0.410, 0.224)
    score.curveto(0.252, 0.126, 0.252, -0.062, 0.358, -0.106)
    score.lineto(0.372, -0.088)
    score.curveto(0.284, 0.004, 0.346, 0.132, 0.424, 0.136)
    score.lineto(0.482, -0.222)
    score.curveto(0.382, -0.220, 0.306, -0.222, 0.236, -0.134)
    score.curveto(0.096, 0.038, 0.278, 0.242, 0.384, 0.406)
    score.moveto(0.516, -0.220)
    score.lineto(0.458, 0.146)
    score.curveto(0.678, 0.176, 0.744, -0.146, 0.516, -0.220)
    score.draw_or_fill_in
    score
  end

  def draw_percussion_clef(*args)
    score = SND_Draw.new("percussion clef", *args)
    score.moveto(0, 0)
    score.lineto(0, 0.5)
    score.rlineto(0.045, 0)
    score.rlineto(0, -0.5)
    score.rlineto(-0.045, 0)
    score.draw_or_fill_in
    score.moveto(0.085, 0)
    score.rlineto(0, 0.5)
    score.rlineto(0.045, 0)
    score.rlineto(0, -0.5)
    score.draw_or_fill_in
    score
  end

  def draw_c_clef(*args)
    score = SND_Draw.new("c clef", *args)
    score.moveto(0.465, 0.442)
    score.lineto(0.465, 0.475)
    score.curveto(0.765, 0.503, 0.643, 0.012, 0.515, 0.080)
    score.curveto(0.453, 0.132, 0.450, 0.197, 0.423, 0.257)
    score.curveto(0.382, 0.115, 0.352, 0.042, 0.305, 0.005)
    score.lineto(0.305, 0.500)
    score.lineto(0.282, 0.500)
    score.lineto(0.282, -0.500)
    score.lineto(0.305, -0.500)
    score.lineto(0.305, -0.005)
    score.curveto(0.353, -0.043, 0.380, -0.112, 0.423, -0.257)
    score.curveto(0.452, -0.192, 0.455, -0.132, 0.517, -0.080)
    score.curveto(0.643, -0.012, 0.765, -0.503, 0.467, -0.475)
    score.lineto(0.467, -0.438)
    score.curveto(0.533, -0.385, 0.503, -0.293, 0.417, -0.305)
    score.curveto(0.263, -0.383, 0.483, -0.645, 0.692, -0.437)
    score.curveto(0.825, -0.283, 0.693,  0.032, 0.440, -0.075)
    score.lineto(0.410, 0.000)
    score.lineto(0.440, 0.075)
    score.curveto(0.693, -0.032, 0.825, 0.283, 0.692, 0.437)
    score.curveto(0.493, 0.647, 0.263, 0.377, 0.430, 0.303)
    score.curveto(0.502, 0.297, 0.547, 0.383, 0.468, 0.442)
    score.draw_or_fill_in
    score.moveto(0.120, 0.500)
    score.lineto(0.120, -0.500)
    score.lineto(0.227, -0.500)
    score.lineto(0.227, 0.500)
    score.lineto(0.120, 0.500)
    score.draw_or_fill_in
    score
  end

  def draw_bass_clef(*args)
    score = SND_Draw.new("bass clef", *args)
    score.moveto(0.058, 0.075)
    score.curveto(0.115, 0.053, 0.108, 0.145, 0.223, 0.098)
    score.curveto(0.292, 0.052, 0.278, -0.057, 0.173, -0.072)
    score.curveto(0.085, -0.080, 0.003, -0.018, 0.007, 0.098)
    score.curveto(0.045, 0.262, 0.238, 0.288, 0.343, 0.250)
    score.curveto(0.492, 0.192, 0.532, 0.067, 0.517, -0.047)
    score.curveto(0.497, -0.292, 0.072, -0.522, 0.017, -0.535)
    score.lineto(0.007, -0.512)
    score.curveto(0.127, -0.452, 0.302, -0.342, 0.367, -0.152)
    score.curveto(0.437,  0.045, 0.377,  0.187, 0.275, 0.227)
    score.curveto(0.128,  0.267, 0.012,  0.165, 0.062, 0.075)
    score.draw_or_fill_in
    score.circle(0.6,  0.1, 0.0525)
    score.circle(0.6, -0.1, 0.0525)
    score
  end

  #
  # ORNAMENTS and ACCENTS
  #
  def draw_turn(*args)
    score = SND_Draw.new("turn", *args)
    score.moveto(-0.096, 0.062)
    score.curveto(-0.130,  0.104, -0.124,  0.208, -0.048, 0.214)
    score.curveto( 0.060,  0.204,  0.154,  0.040,  0.258, 0.010)
    score.curveto( 0.384, -0.028,  0.524,  0.116,  0.418, 0.220)
    score.curveto( 0.372,  0.266,  0.264,  0.262,  0.278, 0.180)
    score.curveto( 0.310,  0.122,  0.370,  0.154,  0.394, 0.196)
    score.curveto( 0.432,  0.144,  0.428,  0.054,  0.342, 0.044)
    score.curveto( 0.242,  0.054,  0.148,  0.216,  0.046, 0.248)
    score.curveto(-0.080,  0.296, -0.220,  0.142, -0.114, 0.036)
    score.curveto(-0.070, -0.008,  0.040, -0.004,  0.028, 0.080)
    score.curveto(-0.002,  0.142, -0.066,  0.102, -0.096, 0.062)
    score.draw_or_fill_in
    score
  end

  def draw_mordent(*args)
    score = SND_Draw.new("mordent", *args)
    score.moveto(0.310, 0.103)
    score.curveto(0.335, 0.129, 0.334, 0.129, 0.359, 0.104)
    score.lineto(0.425, 0.031)
    score.curveto(0.459, -0.001, 0.444, -0.002, 0.478, 0.028)
    score.lineto(0.606, 0.156)
    score.lineto(0.606, 0.193)
    score.lineto(0.563, 0.146)
    score.curveto(0.536, 0.121, 0.535, 0.121, 0.505, 0.151)
    score.lineto(0.436, 0.231)
    score.curveto(0.407, 0.262, 0.416, 0.258, 0.388, 0.232)
    score.lineto(0.307, 0.156)
    score.curveto(0.282, 0.132, 0.283, 0.130, 0.262, 0.153)
    score.lineto(0.190, 0.232)
    score.curveto(0.161, 0.265, 0.167, 0.265, 0.136, 0.237)
    score.lineto(0.000, 0.111)
    score.lineto(0.000, 0.075)
    score.lineto(0.051, 0.122)
    score.curveto(0.083, 0.153, 0.086, 0.152, 0.109, 0.120)
    score.lineto(0.189, 0.030)
    score.curveto(0.214, 0.003, 0.206, 0.003, 0.236, 0.031)
    score.lineto(0.310, 0.103)
    score.draw_or_fill_in
    score
  end

  def draw_double_mordent(*args)
    score = SND_Draw.new("double mordent", *args)
    score.moveto(0.560, 0.106)
    score.curveto(0.586, 0.132, 0.583, 0.132, 0.620, 0.092)
    score.lineto(0.681, 0.031)
    score.curveto(0.708, 0.000, 0.707, 0.004, 0.736, 0.027)
    score.lineto(0.870, 0.146)
    score.lineto(0.870, 0.187)
    score.lineto(0.821, 0.138)
    score.curveto(0.793, 0.115, 0.796, 0.111, 0.761, 0.145)
    score.lineto(0.691, 0.220)
    score.curveto(0.662, 0.257, 0.655, 0.257, 0.629, 0.224)
    score.lineto(0.558, 0.145)
    score.curveto(0.533, 0.121, 0.537, 0.118, 0.505, 0.151)
    score.lineto(0.439, 0.224)
    score.curveto(0.416, 0.251, 0.418, 0.252, 0.393, 0.227)
    score.lineto(0.320, 0.159)
    score.curveto(0.286, 0.121, 0.286, 0.125, 0.256, 0.156)
    score.lineto(0.189, 0.226)
    score.curveto(0.154, 0.266, 0.160, 0.266, 0.131, 0.238)
    score.lineto(0.000, 0.115)
    score.lineto(0.000, 0.075)
    score.lineto(0.050, 0.121)
    score.curveto(0.081, 0.151, 0.077, 0.146, 0.108, 0.117)
    score.lineto(0.188, 0.029)
    score.curveto(0.210, 0.000, 0.210, 0.006, 0.236, 0.030)
    score.lineto(0.310, 0.104)
    score.curveto(0.333, 0.130, 0.331, 0.128, 0.358, 0.100)
    score.lineto(0.425, 0.027)
    score.curveto(0.448, 0.004, 0.444, 0.007, 0.476, 0.031)
    score.lineto(0.561, 0.105)
    score.draw_or_fill_in
    score
  end

  def draw_trill_section(*args)
    score = SND_Draw.new("trill", *args)
    score.moveto(-0.045, 0.053)
    score.lineto(-0.045, 0.075)
    score.curveto(-0.028, 0.099, 0.058, 0.171, 0.113, 0.158)
    score.curveto(0.179, 0.142, 0.245, 0.076, 0.287, 0.079)
    score.curveto(0.339, 0.076, 0.354, 0.095, 0.383, 0.118)
    score.lineto(0.383, 0.097)
    score.curveto(0.366, 0.076, 0.279, 0.001, 0.223, 0.008)
    score.curveto(0.150, 0.014, 0.095, 0.091, 0.050, 0.094)
    score.curveto(0.002, 0.097, -0.017, 0.079, -0.045, 0.053)
    score.draw_or_fill_in
    score
  end

  def draw_trill_sections(count, *args)
    score = SND_Draw.new(count.to_s + " trill sections", *args)
    x0 = 0.0
    count.times do
      score.moveto(x0 + -0.045, 0.053)
      score.lineto(x0 + -0.045, 0.075)
      score.curveto(x0 + -0.028, 0.099, x0 + 0.058, 0.171, x0 + 0.113, 0.158)
      score.curveto(x0 + 0.179, 0.142, x0 + 0.245, 0.076, x0 + 0.287, 0.079)
      score.curveto(x0 + 0.339, 0.076, x0 + 0.354, 0.095, x0 + 0.383, 0.118)
      score.lineto(x0 + 0.383, 0.097)
      score.curveto(x0 + 0.366, 0.076, x0 + 0.279, 0.001, x0 + 0.223, 0.008)
      score.curveto(x0 + 0.150, 0.014, x0 + 0.095, 0.091, x0 + 0.050, 0.094)
      score.curveto(x0 + 0.002, 0.097, x0 + -0.017, 0.079, x0 + -0.045, 0.053)
      score.draw_or_fill_in
      x0 += 0.385
    end
    score
  end

  def draw_arpeggio(*args)
    score = SND_Draw.new("arpeggio", *args)
    score.moveto(0.005, 0.147)
    score.curveto(-0.004, 0.115, 0.042, 0.046, 0.047, 0.039)
    score.curveto(0.049, 0.034, 0.068, 0.005, 0.071, 0.005)
    score.curveto(0.097, 0.009, 0.074, 0.027, 0.070, 0.041)
    score.curveto(0.054, 0.071, 0.055, 0.083, 0.056, 0.098)
    score.curveto(0.067, 0.137, 0.112, 0.153, 0.132, 0.203)
    score.curveto(0.138, 0.261, 0.042, 0.318, 0.058, 0.347)
    score.curveto(0.061, 0.389, 0.125, 0.414, 0.141, 0.455)
    score.curveto(0.142, 0.497, 0.105, 0.527, 0.097, 0.539)
    score.curveto(0.094, 0.541, 0.074, 0.573, 0.070, 0.574)
    score.curveto(0.047, 0.568, 0.065, 0.547, 0.075, 0.534)
    score.curveto(0.085, 0.511, 0.099, 0.513, 0.085, 0.485)
    score.curveto(0.061, 0.451, 0.010, 0.432, 0.004, 0.389)
    score.curveto(0.015, 0.326, 0.084, 0.284, 0.081, 0.245)
    score.curveto(0.083, 0.206, 0.018, 0.193, 0.004, 0.146)
    score.draw_or_fill_in
    score
  end

  def draw_arpeggios(count, *args)
    score = SND_Draw.new(count.to_s + " arpeggios", *args)
    y0 = 0.0
    count.times do
      score.moveto(0.005, y0 + 0.147)
      score.curveto(-0.004, y0 + 0.115, 0.042, y0 + 0.046, 0.047, y0 + 0.039)
      score.curveto(0.049, y0 + 0.034, 0.068, y0 + 0.005, 0.071, y0 + 0.005)
      score.curveto(0.097, y0 + 0.009, 0.074, y0 + 0.027, 0.070, y0 + 0.041)
      score.curveto(0.054, y0 + 0.071, 0.055, y0 + 0.083, 0.056, y0 + 0.098)
      score.curveto(0.067, y0 + 0.137, 0.112, y0 + 0.153, 0.132, y0 + 0.203)
      score.curveto(0.138, y0 + 0.261, 0.042, y0 + 0.318, 0.058, y0 + 0.347)
      score.curveto(0.061, y0 + 0.389, 0.125, y0 + 0.414, 0.141, y0 + 0.455)
      score.curveto(0.142, y0 + 0.497, 0.105, y0 + 0.527, 0.097, y0 + 0.539)
      score.curveto(0.094, y0 + 0.541, 0.074, y0 + 0.573, 0.070, y0 + 0.574)
      score.curveto(0.047, y0 + 0.568, 0.065, y0 + 0.547, 0.075, y0 + 0.534)
      score.curveto(0.085, y0 + 0.511, 0.099, y0 + 0.513, 0.085, y0 + 0.485)
      score.curveto(0.061, y0 + 0.451, 0.010, y0 + 0.432, 0.004, y0 + 0.389)
      score.curveto(0.015, y0 + 0.326, 0.084, y0 + 0.284, 0.081, y0 + 0.245)
      score.curveto(0.083, y0 + 0.206, 0.018, y0 + 0.193, 0.004, y0 + 0.146)
      score.draw_or_fill_in
      y0 += 0.52
    end
    score
  end

  def draw_tr(*args)
    score = SND_Draw.new("tr", *args)
    score.moveto(0.162, 0.252)
    score.lineto(0.198, 0.380)
    score.lineto(0.183, 0.380)
    score.lineto(0.121, 0.341)
    score.lineto(0.093, 0.248)
    score.lineto(-0.005, 0.244)
    score.lineto(-0.020, 0.210)
    score.lineto(0.083, 0.216)
    score.lineto(0.032, 0.050)
    score.curveto(0.002, -0.038, 0.044, -0.034, 0.205, 0.021)
    score.lineto(0.198, -0.005)
    score.lineto(0.262, -0.005)
    score.lineto(0.324, 0.205)
    score.curveto(0.325, 0.207, 0.339, 0.239, 0.359, 0.248)
    score.curveto(0.384, 0.240, 0.368, 0.222, 0.369, 0.209)
    score.curveto(0.380, 0.164, 0.429, 0.169, 0.434, 0.215)
    score.curveto(0.435, 0.281, 0.333, 0.294, 0.305, 0.222)
    score.curveto(0.300, 0.276, 0.243, 0.260, 0.209, 0.256)
    score.lineto(0.162, 0.252)
    score.moveto(0.146, 0.211)
    score.curveto(0.228, 0.220, 0.269, 0.237, 0.266, 0.216)
    score.lineto(0.222, 0.076)
    score.curveto(0.224, 0.058, 0.075, 0.005, 0.099, 0.041)
    score.lineto(0.146, 0.211)
    score.draw_or_fill_in
    score
  end

  def draw_accent(*args)
    score = SND_Draw.new("accent", *args)
    score.moveto(0, 0)
    score.lineto(0.4, 0.124)
    score.lineto(0, 0.248)
    score.lineto(0, 0.216)
    score.lineto(0.3, 0.124)
    score.lineto(0.0, 0.032)
    score.lineto(0, 0)
    score.draw_or_fill_in
    score
  end

  def draw_tnecca(*args)
    score = SND_Draw.new("accent reversed", *args)
    score.moveto(0, 0.124)
    score.lineto(0.4, 0.248)
    score.lineto(0.4, 0.216)
    score.lineto(0.1, 0.124)
    score.lineto(0.4, 0.032)
    score.lineto(0.4, 0)
    score.lineto(0, 0.124)
    score.draw_or_fill_in
    score
  end

  #
  # PAUSES
  #
  def draw_breath_mark(*args)
    score = SND_Draw.new("breath mark", *args)
    score.moveto(0.027, -0.005)
    score.curveto(0.047, 0.093, 0.192, 0.085, 0.186, -0.055)
    score.curveto(0.183, -0.144, 0.104, -0.198, 0.049, -0.224)
    score.lineto(0.045, -0.215)
    score.curveto(0.138, -0.135, 0.161, -0.087, 0.127, -0.060)
    score.curveto(0.093, -0.094, 0.025, -0.075, 0.027, -0.005)
    score.draw_or_fill_in
    score
  end

  def draw_caesura(*args)
    score = SND_Draw.new("caesura", *args)
    score.moveto(0, 0)
    score.lineto(0.05, 0)
    score.lineto(0.33, 0.5)
    score.lineto(0.28, 0.5)
    score.lineto(0, 0)
    score.draw_or_fill_in
    score.moveto(0.17, 0)
    score.lineto(0.22, 0)
    score.lineto(0.5, 0.5)
    score.lineto(0.45, 0.5)
    score.lineto(0.17, 0)
    score.draw_or_fill_in
    score
  end

  def draw_fermata(*args)
    score = SND_Draw.new("fermata", *args)
    score.moveto(0, 0)
    score.curveto(-0.023, 0.197, 0.14, 0.38, 0.338, 0.38)
    score.curveto(0.535, 0.38, 0.698, 0.197, 0.675, 0.0)
    score.rlineto(-0.01, 0.0)
    score.curveto(0.652, 0.173, 0.513, 0.296, 0.338, 0.296)
    score.curveto(0.163, 0.296, 0.023, 0.173, 0.01, 0.0)
    score.lineto(0, 0)
    score.draw_or_fill_in
    score.circle(0.338, 0.0925, 0.05)
    score
  end

  def draw_upside_down_fermata(*args)
    score = SND_Draw.new("fermata reversed", *args)
    score.moveto(0.0000, -0.003)
    score.curveto(-0.023, -0.199, 0.14, -0.383, 0.338, -0.383)
    score.curveto(0.535, -0.383, 0.698, -0.199, 0.675, -0.003)
    score.rlineto(-0.01, 0)
    score.curveto(0.652, -0.176, 0.513, -0.298, 0.338, -0.298)
    score.curveto(0.163, -0.298, 0.023, -0.176, 0.01, -0.003)
    score.lineto(0, -0.003)
    score.draw_or_fill_in
    score.circle(0.338, -0.0925, 0.05)
    score
  end

  #
  # MISCELLANEOUS
  #
  def draw_repeat_sign(*args)
    score = SND_Draw.new("repeat", *args)
    score.moveto(0, 0)
    score.lineto(0.425, 0.5)
    score.rlineto(0.173, 0)
    score.rlineto(-0.425, -0.5)
    score.draw_or_fill_in
    score.circle(0.121, 0.375, 0.06)
    score.circle(0.490, 0.121, 0.06)
    score
  end

  def draw_upper_bracket(*args)
    score = SND_Draw.new("upper bracket", *args)
    score.moveto(0.100, 0.365)
    score.rlineto(0.000, -0.145)
    score.rlineto(0.075, 0.000)
    score.rlineto(0.000, 0.110)
    score.curveto(0.230, 0.350, 0.270, 0.370, 0.310, 0.430)
    score.rlineto(-0.012, 0.003)
    score.curveto(0.270, 0.400, 0.230, 0.370, 0.100, 0.365)
    score.draw_or_fill_in
    score
  end

  def draw_lower_bracket(*args)
    score = SND_Draw.new("lower bracket", *args)
    score.moveto(0.100, -0.365)
    score.rlineto(0.000, 0.145)
    score.rlineto(0.075, 0.000)
    score.rlineto(0.000, -0.110)
    score.curveto(0.230, -0.350, 0.270, -0.370, 0.310, -0.430)
    score.rlineto(-0.012, -0.003)
    score.curveto(0.270, -0.400, 0.230, -0.370, 0.100, -0.365)
    score.draw_or_fill_in
    score
  end

  def draw_segno(*args)
    score = SND_Draw.new("segno", *args)
    score.moveto(0.533, 0.688)
    score.lineto(0.479, 0.688)
    score.lineto(0.278, 0.396)
    score.curveto(0.144, 0.515, 0.086, 0.550, 0.093, 0.611)
    score.curveto(0.097, 0.705, 0.264, 0.704, 0.236, 0.626)
    score.curveto(0.237, 0.601, 0.210, 0.596, 0.204, 0.567)
    score.curveto(0.211, 0.476, 0.334, 0.494, 0.315, 0.587)
    score.curveto(0.271, 0.850, -0.209, 0.656, 0.220, 0.312)
    score.lineto(0.016, 0.007)
    score.lineto(0.068, 0.007)
    score.lineto(0.262, 0.287)
    score.curveto(0.340, 0.236, 0.486, 0.141, 0.450, 0.059)
    score.curveto(0.431, -0.023, 0.305, -0.005, 0.306, 0.060)
    score.curveto(0.303, 0.094, 0.344, 0.087, 0.338, 0.130)
    score.curveto(0.331, 0.209, 0.231, 0.211, 0.224, 0.121)
    score.curveto(0.261, -0.190, 0.776, 0.047, 0.319, 0.374)
    score.lineto(0.533, 0.688)
    score.draw_or_fill_in
    score.circle(0.09, 0.28, 0.05)
    score.circle(0.45, 0.40, 0.05)
    score
  end

  def draw_coda(*args)
    score = SND_Draw.new("coda", *args)
    score.moveto(0.241, 0.379)
    score.lineto(0.241, 0.429)
    score.lineto(0.216, 0.429)
    score.lineto(0.216, 0.379)
    score.curveto(0.130, 0.375, 0.071, 0.280, 0.075, 0.196)
    score.lineto(0.020, 0.196)
    score.lineto(0.020, 0.171)
    score.lineto(0.077, 0.171)
    score.curveto(0.071, 0.090, 0.132, -0.002, 0.216, -0.006)
    score.lineto(0.216, -0.056)
    score.lineto(0.241, -0.056)
    score.lineto(0.241, -0.006)
    score.curveto(0.321, 0.002, 0.381, 0.090, 0.376, 0.171)
    score.lineto(0.431, 0.171)
    score.lineto(0.431, 0.196)
    score.lineto(0.376, 0.196)
    score.curveto(0.382, 0.279, 0.324, 0.379, 0.241, 0.379)
    score.moveto(0.308, 0.169)
    score.curveto(0.308, 0.097, 0.294, 0.037, 0.241, 0.020)
    score.lineto(0.241, 0.171)
    score.lineto(0.307, 0.171)
    score.moveto(0.216, 0.020)
    score.curveto(0.163, 0.040, 0.144, 0.109, 0.141, 0.171)
    score.lineto(0.216, 0.171)
    score.lineto(0.216, 0.020)
    score.moveto(0.141, 0.196)
    score.curveto(0.145, 0.265, 0.166, 0.346, 0.216, 0.353)
    score.lineto(0.216, 0.196)
    score.lineto(0.137, 0.196)
    score.moveto(0.241, 0.353)
    score.curveto(0.286, 0.345, 0.308, 0.265, 0.312, 0.196)
    score.lineto(0.241, 0.196)
    score.lineto(0.241, 0.353)
    score.draw_or_fill_in
    score
  end

  def draw_pedal_off(*args)
    score = SND_Draw.new("pedal off", *args)
    score.moveto(0.219, 0.198)
    score.curveto(0.231, 0.172, 0.195, 0.138, 0.162, 0.173)
    score.curveto(0.149, 0.219, 0.206, 0.231, 0.219, 0.198)
    score.moveto(0.144, 0.242)
    score.curveto(0.166, 0.223, 0.193, 0.230, 0.181, 0.267)
    score.curveto(0.178, 0.306, 0.144, 0.302, 0.151, 0.335)
    score.curveto(0.160, 0.381, 0.225, 0.377, 0.224, 0.330)
    score.curveto(0.228, 0.302, 0.198, 0.306, 0.197, 0.267)
    score.curveto(0.194, 0.237, 0.213, 0.222, 0.237, 0.247)
    score.curveto(0.263, 0.276, 0.234, 0.297, 0.268, 0.322)
    score.curveto(0.314, 0.347, 0.354, 0.297, 0.316, 0.259)
    score.curveto(0.296, 0.237, 0.273, 0.266, 0.246, 0.237)
    score.curveto(0.223, 0.217, 0.232, 0.194, 0.266, 0.197)
    score.curveto(0.303, 0.202, 0.302, 0.232, 0.332, 0.228)
    score.curveto(0.381, 0.232, 0.388, 0.156, 0.332, 0.152)
    score.curveto(0.302, 0.148, 0.302, 0.185, 0.266, 0.183)
    score.curveto(0.231, 0.186, 0.228, 0.169, 0.245, 0.143)
    score.curveto(0.273, 0.116, 0.297, 0.141, 0.316, 0.117)
    score.curveto(0.350, 0.075, 0.303, 0.029, 0.258, 0.062)
    score.curveto(0.237, 0.082, 0.261, 0.102, 0.233, 0.133)
    score.curveto(0.212, 0.151, 0.194, 0.147, 0.197, 0.113)
    score.curveto(0.203, 0.075, 0.232, 0.075, 0.230, 0.043)
    score.curveto(0.223, -0.004, 0.159, -0.002, 0.152, 0.042)
    score.curveto(0.148, 0.075, 0.185, 0.076, 0.183, 0.113)
    score.curveto(0.183, 0.147, 0.163, 0.150, 0.141, 0.133)
    score.curveto(0.113, 0.104, 0.140, 0.079, 0.113, 0.059)
    score.curveto(0.069, 0.037, 0.033, 0.077, 0.063, 0.117)
    score.curveto(0.082, 0.141, 0.104, 0.117, 0.132, 0.142)
    score.curveto(0.153, 0.163, 0.144, 0.188, 0.113, 0.182)
    score.curveto(0.073, 0.182, 0.075, 0.147, 0.046, 0.152)
    score.curveto(-0.003, 0.152, -0.003, 0.227, 0.048, 0.227)
    score.curveto(0.075, 0.231, 0.075, 0.198, 0.113, 0.196)
    score.curveto(0.141, 0.197, 0.147, 0.207, 0.133, 0.237)
    score.curveto(0.102, 0.264, 0.082, 0.237, 0.062, 0.261)
    score.curveto(0.028, 0.302, 0.077, 0.347, 0.118, 0.318)
    score.curveto(0.138, 0.297, 0.116, 0.275, 0.144, 0.242)
    score.draw_or_fill_in
    score
  end

  def draw_ped(*args)
    score = SND_Draw.new("ped", *args)
    score.moveto(0.368, 0.074)
    score.curveto(0.341, 0.121, 0.335, 0.147, 0.371, 0.203)
    score.curveto(0.435, 0.289, 0.531, 0.243, 0.488, 0.155)
    score.curveto(0.472, 0.117, 0.434, 0.096, 0.414, 0.080)
    score.curveto(0.429, 0.038, 0.494, -0.006, 0.541, 0.075)
    score.curveto(0.559, 0.123, 0.558, 0.224, 0.663, 0.252)
    score.curveto(0.603, 0.354, 0.449, 0.393, 0.461, 0.405)
    score.curveto(0.902, 0.262, 0.705, -0.124, 0.555, 0.046)
    score.curveto(0.488, -0.032, 0.417, 0.021, 0.389, 0.055)
    score.curveto(0.303, -0.018, 0.303, -0.020, 0.248, 0.040)
    score.curveto(0.218, 0.108, 0.191, 0.062, 0.164, 0.047)
    score.curveto(0.010, -0.056, 0.032, 0.019, 0.124, 0.062)
    score.curveto(0.229, 0.117, 0.200, 0.091, 0.228, 0.195)
    score.curveto(0.240, 0.241, 0.149, 0.250, 0.166, 0.311)
    score.lineto(0.207, 0.493)
    score.curveto(-0.041, 0.441, 0.049, 0.261, 0.126, 0.387)
    score.lineto(0.138, 0.381)
    score.curveto(-0.020, 0.119, -0.100, 0.472, 0.220, 0.507)
    score.curveto(0.548, 0.486, 0.399, 0.171, 0.254, 0.374)
    score.lineto(0.264, 0.384)
    score.curveto(0.338, 0.259, 0.521, 0.449, 0.228, 0.488)
    score.lineto(0.198, 0.356)
    score.curveto(0.181, 0.304, 0.273, 0.294, 0.262, 0.241)
    score.lineto(0.229, 0.101)
    score.curveto(0.273, 0.070, 0.282, -0.038, 0.368, 0.074)
    score.moveto(0.391, 0.094)
    score.curveto(0.456, 0.130, 0.476, 0.171, 0.468, 0.213)
    score.curveto(0.452, 0.276, 0.333, 0.171, 0.391, 0.094)
    score.moveto(0.627, 0.019)
    score.curveto(0.533, 0.041, 0.586, 0.228, 0.678, 0.229)
    score.curveto(0.729, 0.170, 0.712, 0.025, 0.627, 0.019)
    score.draw_or_fill_in
    score.circle(0.8, 0.04, 0.04)
    score
  end

  def draw_left_paren(*args)
    score = SND_Draw.new("left paren", *args)
    score.moveto(0.157, 0.580)
    score.curveto(0.090, 0.540, -0.015, 0.442, -0.012, 0.287)
    score.curveto(-0.007, 0.145, 0.082, 0.040, 0.147, -0.005)
    score.lineto(0.153, 0.003)
    score.curveto(0.035, 0.122, 0.047, 0.193, 0.047, 0.297)
    score.curveto(0.042, 0.417, 0.067, 0.490, 0.162, 0.570)
    score.lineto(0.157, 0.580)
    score.draw_or_fill_in
    score
  end

  def draw_right_paren(*args)
    score = SND_Draw.new("right paren", *args)
    score.moveto(0.005, 0.580)
    score.curveto(0.072, 0.540, 0.177, 0.442, 0.174, 0.287)
    score.curveto(0.169, 0.145, 0.080, 0.040, 0.015, -0.005)
    score.lineto(0.009, 0.003)
    score.curveto(0.127, 0.122, 0.115, 0.193, 0.115, 0.297)
    score.curveto(0.120, 0.417, 0.095, 0.490, 0.0, 0.570)
    score.lineto(0.005, 0.580)
    score.draw_or_fill_in
    score
  end

  def draw_wedge(*args)
    score = SND_Draw.new("wedge", *args)
    score.moveto(0, 0)
    score.lineto(-0.075, 0.25)
    score.lineto(0.075, 0.25)
    score.lineto(0, 0)
    score.draw_or_fill_in
    score
  end

  def draw_down_bow(*args)
    score = SND_Draw.new("down bow", *args)
    score.moveto(0, 0)
    score.lineto(0, 0.15)
    score.lineto(0.3, 0.15)
    score.lineto(0.3, 0)
    score.lineto(0.29, 0)
    score.lineto(0.29, 0.075)
    score.lineto(0.01, 0.075)
    score.lineto(0.01, 0)
    score.lineto(0, 0)
    score.draw_or_fill_in
    score
  end

  def draw_up_bow(*args)
    score = SND_Draw.new("up bow", *args)
    score.moveto(0.075, 0.000)
    score.lineto(0.000, 0.250)
    score.lineto(0.010, 0.250)
    score.lineto(0.075, 0.055)
    score.lineto(0.140, 0.250)
    score.lineto(0.150, 0.250)
    score.lineto(0.075, 0.000)
    score.draw_or_fill_in
    score
  end

  #
  # NUMBERS and METERS
  # 
  def draw_zero(*args)
    score = SND_Draw.new("zero", *args)
    score.moveto(0.159, 0.233)
    score.curveto(0.272, 0.233, 0.333, 0.117, 0.333, 0.000)
    score.curveto(0.333, -0.130, 0.270, -0.235, 0.159, -0.233)
    score.curveto(0.041, -0.233, -0.013, -0.122, -0.019, -0.006)
    score.curveto(-0.015, 0.113, 0.043, 0.233, 0.159, 0.233)
    score.moveto(0.159, 0.207)
    score.curveto(0.218, 0.207, 0.228, 0.083, 0.229, -0.002)
    score.curveto(0.228, -0.085, 0.216, -0.207, 0.159, -0.207)
    score.curveto(0.100, -0.207, 0.089, -0.085, 0.085, 0.000)
    score.curveto(0.085, 0.085, 0.096, 0.207, 0.159, 0.207)
    score.draw_or_fill_in
    score
  end

  def draw_one(*args)
    score = SND_Draw.new("one", *args)
    score.moveto(0.070, -0.182)
    score.curveto(0.068, -0.199, 0.042, -0.203, 0.026, -0.203)
    score.lineto(0.026, -0.233)
    score.lineto(0.209, -0.233)
    score.lineto(0.209, -0.203)
    score.curveto(0.199, -0.202, 0.171, -0.203, 0.167, -0.187)
    score.lineto(0.167, 0.233)
    score.lineto(0.070, 0.233)
    score.lineto(-0.011, 0.034)
    score.lineto(0.010, 0.024)
    score.lineto(0.070, 0.130)
    score.lineto(0.070, -0.182)
    score.draw_or_fill_in
    score
  end

  def draw_two(*args)
    score = SND_Draw.new("two", *args)
    score.moveto(0.068, 0.170)
    score.curveto(0.093, 0.168, 0.130, 0.167, 0.130, 0.113)
    score.curveto(0.123, 0.042, 0.007, 0.044, 0.006, 0.131)
    score.curveto(0.007, 0.179, 0.074, 0.233, 0.146, 0.233)
    score.curveto(0.224, 0.233, 0.318, 0.202, 0.329, 0.107)
    score.curveto(0.320, 0.010, 0.238, -0.013, 0.152, -0.052)
    score.curveto(0.105, -0.071, 0.063, -0.114, 0.067, -0.137)
    score.curveto(0.113, -0.107, 0.157, -0.100, 0.205, -0.122)
    score.curveto(0.239, -0.138, 0.252, -0.170, 0.274, -0.170)
    score.curveto(0.329, -0.170, 0.316, -0.111, 0.316, -0.107)
    score.lineto(0.337, -0.107)
    score.curveto(0.335, -0.158, 0.326, -0.231, 0.244, -0.237)
    score.curveto(0.165, -0.235, 0.146, -0.172, 0.083, -0.172)
    score.curveto(0.015, -0.167, 0.024, -0.217, 0.019, -0.226)
    score.lineto(0.000, -0.226)
    score.lineto(0.000, -0.161)
    score.curveto(0.011, -0.072, 0.231, 0.014, 0.233, 0.104)
    score.curveto(0.233, 0.167, 0.194, 0.205, 0.111, 0.200)
    score.curveto(0.068, 0.198, 0.050, 0.168, 0.068, 0.170)
    score.draw_or_fill_in
    score
  end

  def draw_three(*args)
    score = SND_Draw.new("three", *args)
    score.moveto(0.094, 0.007)
    score.curveto(0.163, 0.028, 0.204, 0.039, 0.203, 0.102)
    score.curveto(0.212, 0.180, 0.159, 0.209, 0.117, 0.207)
    score.curveto(0.072, 0.210, 0.026, 0.165, 0.052, 0.155)
    score.curveto(0.113, 0.148, 0.113, 0.074, 0.050, 0.068)
    score.curveto(0.020, 0.070, -0.010, 0.092, -0.005, 0.147)
    score.curveto(0.034, 0.273, 0.291, 0.257, 0.292, 0.103)
    score.curveto(0.292, 0.039, 0.265, 0.019, 0.220, 0.000)
    score.curveto(0.259, -0.017, 0.291, -0.043, 0.289, -0.114)
    score.curveto(0.272, -0.281, -0.001, -0.262, -0.011, -0.134)
    score.curveto(-0.013, -0.091, 0.020, -0.063, 0.050, -0.067)
    score.curveto(0.098, -0.070, 0.113, -0.144, 0.052, -0.157)
    score.curveto(0.033, -0.172, 0.065, -0.207, 0.117, -0.207)
    score.curveto(0.170, -0.201, 0.208, -0.177, 0.204, -0.099)
    score.curveto(0.200, -0.038, 0.163, -0.028, 0.094, -0.007)
    score.lineto(0.094, 0.007)
    score.draw_or_fill_in
    score
  end

  def draw_four(*args)
    score = SND_Draw.new("four", *args)
    score.moveto(0.252, 0.233)
    score.lineto(0.113, 0.233)
    score.curveto(0.107, 0.128, 0.108, 0.003, -0.002, -0.097)
    score.lineto(-0.002, -0.122)
    score.lineto(0.159, -0.122)
    score.lineto(0.159, -0.188)
    score.curveto(0.157, -0.198, 0.130, -0.207, 0.115, -0.206)
    score.lineto(0.115, -0.233)
    score.lineto(0.309, -0.233)
    score.lineto(0.309, -0.207)
    score.curveto(0.299, -0.207, 0.274, -0.203, 0.268, -0.187)
    score.lineto(0.268, -0.122)
    score.lineto(0.309, -0.122)
    score.lineto(0.309, -0.096)
    score.lineto(0.265, -0.096)
    score.lineto(0.265, 0.146)
    score.lineto(0.159, 0.017)
    score.lineto(0.159, -0.096)
    score.lineto(0.043, -0.096)
    score.curveto(0.139, 0.021, 0.217, 0.149, 0.252, 0.233)
    score.draw_or_fill_in
    score
  end

  def draw_five(*args)
    score = SND_Draw.new("five", *args)
    score.moveto(0.022, 0.233)
    score.lineto(0.022, -0.002)
    score.lineto(0.050, -0.002)
    score.curveto(0.067, 0.026, 0.093, 0.041, 0.124, 0.041)
    score.curveto(0.178, 0.041, 0.207, -0.015, 0.207, -0.070)
    score.curveto(0.209, -0.165, 0.169, -0.211, 0.120, -0.212)
    score.curveto(0.098, -0.215, 0.076, -0.211, 0.056, -0.187)
    score.curveto(0.039, -0.149, 0.114, -0.196, 0.118, -0.126)
    score.curveto(0.104, -0.067, 0.015, -0.068, 0.008, -0.145)
    score.curveto(0.005, -0.275, 0.306, -0.297, 0.296, -0.063)
    score.curveto(0.283, 0.028, 0.224, 0.068, 0.137, 0.068)
    score.curveto(0.109, 0.068, 0.076, 0.056, 0.050, 0.030)
    score.lineto(0.050, 0.159)
    score.curveto(0.057, 0.152, 0.152, 0.135, 0.199, 0.155)
    score.curveto(0.248, 0.176, 0.264, 0.203, 0.269, 0.236)
    score.curveto(0.181, 0.212, 0.087, 0.211, 0.022, 0.233)
    score.draw_or_fill_in
    score
  end

  def draw_six(*args)
    score = SND_Draw.new("six", *args)
    score.moveto(0.168, -0.238)
    score.curveto(0.244, -0.235, 0.303, -0.182, 0.305, -0.096)
    score.curveto(0.305, -0.043, 0.274, 0.030, 0.205, 0.030)
    score.curveto(0.167, 0.030, 0.137, 0.015, 0.115, -0.015)
    score.curveto(0.093, -0.006, 0.089, 0.176, 0.150, 0.207)
    score.curveto(0.194, 0.235, 0.242, 0.198, 0.231, 0.181)
    score.curveto(0.207, 0.176, 0.188, 0.159, 0.190, 0.119)
    score.curveto(0.208, 0.062, 0.291, 0.069, 0.300, 0.128)
    score.curveto(0.299, 0.175, 0.276, 0.236, 0.179, 0.240)
    score.curveto(0.074, 0.235, 0.007, 0.117, 0.007, 0.004)
    score.curveto(0.011, -0.108, 0.057, -0.231, 0.168, -0.238)
    score.moveto(0.174, -0.215)
    score.curveto(0.220, -0.212, 0.228, -0.155, 0.228, -0.108)
    score.curveto(0.229, -0.061, 0.222, 0.006, 0.174, 0.006)
    score.curveto(0.124, 0.006, 0.115, -0.058, 0.117, -0.108)
    score.curveto(0.118, -0.157, 0.126, -0.215, 0.174, -0.215)
    score.draw_or_fill_in
    score
  end

  def draw_seven(*args)
    score = SND_Draw.new("seven", *args)
    score.moveto(0.068, -0.233)
    score.lineto(0.202, -0.233)
    score.curveto(0.202, -0.228, 0.185, -0.117, 0.229, -0.048)
    score.curveto(0.263, 0.007, 0.338, 0.076, 0.330, 0.234)
    score.lineto(0.306, 0.234)
    score.curveto(0.266, 0.147, 0.235, 0.205, 0.169, 0.225)
    score.curveto(0.080, 0.235, 0.061, 0.169, 0.028, 0.220)
    score.lineto(0.004, 0.220)
    score.lineto(0.004, 0.120)
    score.lineto(0.028, 0.120)
    score.curveto(0.037, 0.164, 0.106, 0.177, 0.151, 0.139)
    score.curveto(0.214, 0.099, 0.265, 0.112, 0.294, 0.142)
    score.curveto(0.286, 0.060, 0.218, 0.004, 0.159, -0.046)
    score.curveto(0.077, -0.112, 0.067, -0.226, 0.068, -0.233)
    score.draw_or_fill_in
    score
  end

  def draw_eight(*args)
    score = SND_Draw.new("eight", *args)
    score.moveto(0.146, 0.235)
    score.curveto(0.068, 0.233, 0.015, 0.198, -0.004, 0.125)
    score.curveto(-0.011, 0.048, 0.055, 0.024, 0.068, 0.009)
    score.curveto(0.042, -0.006, -0.006, -0.051, -0.007, -0.117)
    score.curveto(0.002, -0.189, 0.054, -0.234, 0.154, -0.238)
    score.curveto(0.228, -0.237, 0.295, -0.195, 0.296, -0.109)
    score.curveto(0.301, -0.073, 0.279, -0.025, 0.220, 0.015)
    score.curveto(0.265, 0.045, 0.290, 0.082, 0.287, 0.131)
    score.curveto(0.279, 0.193, 0.213, 0.233, 0.146, 0.235)
    score.moveto(0.146, 0.209)
    score.curveto(0.200, 0.208, 0.233, 0.170, 0.235, 0.114)
    score.curveto(0.237, 0.083, 0.208, 0.052, 0.185, 0.038)
    score.curveto(0.149, 0.070, 0.085, 0.083, 0.069, 0.140)
    score.curveto(0.069, 0.172, 0.101, 0.208, 0.146, 0.209)
    score.moveto(0.099, -0.014)
    score.curveto(0.132, -0.040, 0.191, -0.055, 0.224, -0.114)
    score.curveto(0.240, -0.166, 0.207, -0.207, 0.146, -0.207)
    score.curveto(0.080, -0.202, 0.051, -0.150, 0.047, -0.098)
    score.curveto(0.043, -0.059, 0.073, -0.024, 0.098, -0.013)
    score.draw_or_fill_in
    score
  end

  def draw_nine(*args)
    score = SND_Draw.new("nine", *args)
    score.moveto(0.129, 0.235)
    score.curveto(-0.113, 0.212, -0.049, -0.123, 0.189, -0.003)
    score.curveto(0.211, 0.006, 0.216, -0.176, 0.157, -0.198)
    score.curveto(0.106, -0.232, 0.072, -0.203, 0.079, -0.197)
    score.curveto(0.125, -0.179, 0.135, -0.142, 0.114, -0.105)
    score.curveto(0.096, -0.065, 0.019, -0.072, 0.003, -0.133)
    score.curveto(0.000, -0.193, 0.053, -0.244, 0.132, -0.235)
    score.curveto(0.220, -0.231, 0.307, -0.117, 0.307, -0.003)
    score.curveto(0.309, 0.111, 0.264, 0.240, 0.131, 0.235)
    score.moveto(0.131, 0.215)
    score.curveto(0.085, 0.213, 0.075, 0.155, 0.076, 0.109)
    score.curveto(0.076, 0.059, 0.080, -0.006, 0.131, -0.006)
    score.curveto(0.181, -0.007, 0.191, 0.059, 0.187, 0.109)
    score.curveto(0.187, 0.157, 0.178, 0.215, 0.131, 0.215)
    score.draw_or_fill_in
    score
  end

  def draw_common_time(*args)
    score = SND_Draw.new("common time", *args)
    score.moveto(0.004, 0.000)
    score.moveto(0.004, 0.000)
    score.curveto(0.004, -0.128, 0.096, -0.247, 0.228, -0.247)
    score.curveto(0.328, -0.246, 0.391, -0.171, 0.410, -0.070)
    score.lineto(0.383, -0.070)
    score.curveto(0.366, -0.160, 0.310, -0.215, 0.230, -0.223)
    score.curveto(0.136, -0.220, 0.110, -0.114, 0.113, -0.002)
    score.curveto(0.113, 0.102, 0.128, 0.200, 0.224, 0.220)
    score.curveto(0.266, 0.230, 0.312, 0.220, 0.344, 0.186)
    score.curveto(0.358, 0.172, 0.356, 0.162, 0.334, 0.156)
    score.curveto(0.300, 0.159, 0.263, 0.134, 0.263, 0.084)
    score.curveto(0.270, -0.004, 0.414, -0.001, 0.418, 0.096)
    score.curveto(0.418, 0.210, 0.310, 0.258, 0.218, 0.252)
    score.curveto(0.092, 0.246, 0.004, 0.128, 0.004, 0.000)
    score.draw_or_fill_in
    score
  end

  def draw_cut_time(*args)
    score = SND_Draw.new("", *args)
    draw_common_time(*args)
    score.moveto(0.194, 0.374)
    score.lineto(0.194, -0.414)
    score.lineto(0.222, -0.414)
    score.lineto(0.222, 0.374)
    score.draw_or_fill_in
    score
  end

  def draw_plus(*args)
    score = SND_Draw.new("plus", *args)
    score.moveto(0.000, -0.020)
    score.lineto(0.100, -0.020)
    score.lineto(0.100, -0.140)
    score.lineto(0.140, -0.140)
    score.lineto(0.140, -0.020)
    score.lineto(0.240, -0.020)
    score.lineto(0.240, 0.020)
    score.lineto(0.140, 0.020)
    score.lineto(0.140, 0.140)
    score.lineto(0.100, 0.140)
    score.lineto(0.100, 0.020)
    score.lineto(0.000, 0.020)
    score.lineto(0.000, -0.020)
    score.draw_or_fill_in
    score
  end

  #
  # ACCIDENTALS
  # 
  def draw_sharp(*args)
    score = SND_Draw.new("sharp", *args)
    score.moveto(0.168, 0.098)
    score.lineto(0.168, -0.050)
    score.lineto(0.210, -0.032)
    score.lineto(0.210, -0.136)
    score.lineto(0.168, -0.154)
    score.lineto(0.168, -0.338)
    score.lineto(0.140, -0.338)
    score.lineto(0.140, -0.166)
    score.lineto(0.072, -0.194)
    score.lineto(0.072, -0.380)
    score.lineto(0.044, -0.380)
    score.lineto(0.044, -0.206)
    score.lineto(0.000, -0.222)
    score.lineto(0.000, -0.116)
    score.lineto(0.044, -0.100)
    score.lineto(0.044, 0.048)
    score.lineto(0.000, 0.032)
    score.lineto(0.000, 0.138)
    score.lineto(0.044, 0.154)
    score.lineto(0.044, 0.338)
    score.lineto(0.072, 0.338)
    score.lineto(0.072, 0.166)
    score.lineto(0.140, 0.192)
    score.lineto(0.140, 0.380)
    score.lineto(0.168, 0.380)
    score.lineto(0.168, 0.204)
    score.lineto(0.210, 0.222)
    score.lineto(0.210, 0.116)
    score.lineto(0.168, 0.098)
    score.moveto(0.140, 0.088)
    score.lineto(0.072, 0.060)
    score.lineto(0.072, -0.088)
    score.lineto(0.140, -0.060)
    score.lineto(0.140, 0.088)
    score.draw_or_fill_in
    score
  end

  def draw_flat(*args)
    score = SND_Draw.new("flat", *args)
    score.moveto(0.027, 0.086)
    score.lineto(0.027, 0.483)
    score.lineto(0.000, 0.483)
    score.lineto(0.000, -0.193)
    score.curveto(0.012, -0.186, 0.070, -0.139, 0.097, -0.119)
    score.curveto(0.141, -0.086, 0.244, -0.024, 0.215, 0.082)
    score.curveto(0.165, 0.204, 0.027, 0.087, 0.027, 0.086)
    score.moveto(0.027, 0.037)
    score.lineto(0.027, -0.137)
    score.curveto(0.032, -0.140, 0.206, 0.029, 0.120, 0.090)
    score.curveto(0.112, 0.093, 0.085, 0.105, 0.027, 0.037)
    score.draw_or_fill_in
    score
  end

  def draw_double_sharp(*args)
    score = SND_Draw.new("double sharp", *args)
    score.moveto(0.000, 0.130)
    score.lineto(0.090, 0.130)
    score.curveto(0.091, -0.002, 0.174, -0.001, 0.170, 0.130)
    score.lineto(0.260, 0.130)
    score.lineto(0.260, 0.040)
    score.curveto(0.127, 0.028, 0.136, -0.040, 0.260, -0.040)
    score.lineto(0.260, -0.130)
    score.lineto(0.170, -0.130)
    score.curveto(0.165, 0.002, 0.091, 0.000, 0.090, -0.130)
    score.lineto(0.000, -0.130)
    score.lineto(0.000, -0.043)
    score.curveto(0.127, -0.042, 0.127, 0.027, 0.000, 0.040)
    score.lineto(0.000, 0.130)
    score.draw_or_fill_in
    score
  end

  def draw_natural(*args)
    score = SND_Draw.new("natural", *args)
    score.moveto(0.000, -0.180)
    score.lineto(0.144, -0.142)
    score.lineto(0.144, -0.348)
    score.lineto(0.170, -0.348)
    score.lineto(0.170, 0.176)
    score.lineto(0.026, 0.138)
    score.lineto(0.026, 0.348)
    score.lineto(0.000, 0.348)
    score.lineto(0.000, -0.180)
    score.moveto(0.026, -0.076)
    score.lineto(0.026, 0.048)
    score.lineto(0.144, 0.078)
    score.lineto(0.144, -0.046)
    score.lineto(0.026, -0.076)
    score.draw_or_fill_in
    score
  end

  def draw_double_flat(*args)
    score = SND_Draw.new("double flat", *args)
    draw_flat(*args)
    draw_flat(*args)
    score
  end

  #
  # DYNAMICS
  # 
  def draw_f(*args)
    score = SND_Draw.new("f", *args)
    score.moveto(0.420, 0.238)
    score.lineto(0.330, 0.238)
    score.curveto(0.330, 0.230, 0.334, 0.348, 0.418, 0.386)
    score.curveto(0.444, 0.398, 0.496, 0.400, 0.490, 0.374)
    score.curveto(0.488, 0.362, 0.462, 0.370, 0.452, 0.360)
    score.curveto(0.429, 0.342, 0.426, 0.308, 0.448, 0.292)
    score.curveto(0.478, 0.276, 0.521, 0.292, 0.514, 0.344)
    score.curveto(0.514, 0.400, 0.456, 0.432, 0.386, 0.414)
    score.curveto(0.258, 0.380, 0.232, 0.240, 0.234, 0.238)
    score.lineto(0.158, 0.238)
    score.lineto(0.158, 0.210)
    score.lineto(0.228, 0.210)
    score.curveto(0.228, 0.208, 0.172, -0.046, 0.122, -0.117)
    score.curveto(0.114, -0.133, 0.096, -0.158, 0.078, -0.158)
    score.curveto(0.056, -0.162, 0.046, -0.142, 0.060, -0.139)
    score.curveto(0.099, -0.138, 0.113, -0.074, 0.056, -0.059)
    score.curveto(0.022, -0.052, -0.018, -0.102, 0.007, -0.146)
    score.curveto(0.026, -0.176, 0.088, -0.188, 0.134, -0.162)
    score.curveto(0.244, -0.100, 0.287, 0.046, 0.324, 0.210)
    score.lineto(0.420, 0.210)
    score.lineto(0.420, 0.238)
    score.draw_or_fill_in
    score
  end

  def draw_p(*args)
    score = SND_Draw.new("p", *args)
    score.moveto(0.184, 0.047)
    score.curveto(0.203, 0.016, 0.218, 0.006, 0.248, 0.004)
    score.curveto(0.379, 0.002, 0.464, 0.230, 0.336, 0.290)
    score.curveto(0.280, 0.310, 0.228, 0.262, 0.211, 0.246)
    score.curveto(0.210, 0.333, 0.126, 0.312, 0.068, 0.153)
    score.lineto(0.089, 0.147)
    score.curveto(0.155, 0.310, 0.205, 0.279, 0.185, 0.224)
    score.lineto(0.040, -0.120)
    score.lineto(0.000, -0.120)
    score.lineto(0.000, -0.146)
    score.lineto(0.157, -0.146)
    score.lineto(0.157, -0.120)
    score.lineto(0.117, -0.120)
    score.lineto(0.184, 0.047)
    score.moveto(0.222, 0.047)
    score.curveto(0.160, 0.086, 0.245, 0.261, 0.321, 0.268)
    score.curveto(0.377, 0.218, 0.303, 0.029, 0.222, 0.047)
    score.draw_or_fill_in
    score
  end

  def draw_lig_p(*args)
    score = SND_Draw.new("P", *args)
    score.moveto(0.184, 0.047)
    score.curveto(0.203, 0.016, 0.218, 0.006, 0.248, 0.004)
    score.curveto(0.379, 0.002, 0.464, 0.230, 0.336, 0.290)
    score.curveto(0.280, 0.310, 0.228, 0.262, 0.211, 0.246)
    score.curveto(0.205, 0.285, 0.168, 0.304, 0.138, 0.296)
    score.lineto(0.136, 0.270)
    score.curveto(0.162, 0.278, 0.190, 0.257, 0.185, 0.224)
    score.lineto(0.040, -0.120)
    score.lineto(0.000, -0.120)
    score.lineto(0.000, -0.145)
    score.lineto(0.157, -0.145)
    score.lineto(0.157, -0.120)
    score.lineto(0.117, -0.120)
    score.lineto(0.184, 0.047)
    score.moveto(0.222, 0.047)
    score.curveto(0.160, 0.086, 0.245, 0.261, 0.321, 0.268)
    score.curveto(0.377, 0.218, 0.303, 0.029, 0.222, 0.047)
    score.draw_or_fill_in
    score
  end

  def draw_m(*args)
    score = SND_Draw.new("m", *args)
    score.moveto(0.188, 0.000)
    score.lineto(0.271, 0.226)
    score.curveto(0.287, 0.270, 0.252, 0.303, 0.207, 0.225)
    score.lineto(0.120, 0.000)
    score.lineto(0.052, 0.000)
    score.lineto(0.157, 0.261)
    score.curveto(0.160, 0.304, 0.072, 0.266, 0.045, 0.184)
    score.lineto(0.027, 0.184)
    score.curveto(0.056, 0.245, 0.100, 0.301, 0.159, 0.292)
    score.curveto(0.188, 0.286, 0.200, 0.262, 0.200, 0.243)
    score.curveto(0.228, 0.303, 0.327, 0.312, 0.333, 0.243)
    score.curveto(0.373, 0.317, 0.488, 0.303, 0.468, 0.236)
    score.lineto(0.396, 0.038)
    score.curveto(0.386, 0.001, 0.435, 0.024, 0.460, 0.074)
    score.lineto(0.472, 0.071)
    score.curveto(0.429, -0.026, 0.310, -0.065, 0.335, 0.019)
    score.lineto(0.403, 0.222)
    score.curveto(0.424, 0.273, 0.375, 0.291, 0.346, 0.228)
    score.lineto(0.264, 0.000)
    score.lineto(0.186, 0.000)
    score.draw_or_fill_in
    score
  end

  def draw_n(*args)
    score = SND_Draw.new("n", *args)
    score.moveto(0.210, 0.019)
    score.lineto(0.280, 0.226)
    score.curveto(0.287, 0.270, 0.253, 0.302, 0.207, 0.225)
    score.lineto(0.120, 0.000)
    score.lineto(0.052, 0.000)
    score.lineto(0.158, 0.261)
    score.curveto(0.160, 0.304, 0.072, 0.266, 0.045, 0.184)
    score.lineto(0.028, 0.184)
    score.curveto(0.056, 0.245, 0.100, 0.301, 0.159, 0.292)
    score.curveto(0.188, 0.286, 0.200, 0.263, 0.200, 0.242)
    score.curveto(0.228, 0.302, 0.338, 0.312, 0.341, 0.243)
    score.lineto(0.265, 0.024)
    score.curveto(0.264, 0.001, 0.331, 0.024, 0.340, 0.074)
    score.lineto(0.352, 0.071)
    score.curveto(0.33, -0.026, 0.210, -0.065, 0.210, 0.019)
    score.draw_or_fill_in
    score
  end

  def draw_niente(*args)
    score = SND_Draw.new("n.", *args)
    draw_n(*args)
    score.circle(0.425, 0.025, 0.03)
    score
  end

  def draw_subito(*args)
    score = SND_Draw.new("I", *args)
    score.moveto(0.3, -0.2)
    score.rlineto(0, 0.7)
    score.draw
    score
  end

  def draw_z(*args)
    score = SND_Draw.new("z", *args)
    score.moveto(0.082, 0.287)
    score.lineto(0.052, 0.222)
    score.lineto(0.069, 0.222)
    score.lineto(0.089, 0.240)
    score.lineto(0.238, 0.240)
    score.lineto(-0.003, 0.006)
    score.lineto(0.034, 0.006)
    score.curveto(0.072, 0.043, 0.156, 0.028, 0.191, 0.003)
    score.curveto(0.274, -0.021, 0.302, 0.046, 0.265, 0.101)
    score.curveto(0.251, 0.117, 0.219, 0.118, 0.205, 0.093)
    score.curveto(0.206, 0.050, 0.249, 0.075, 0.246, 0.041)
    score.curveto(0.206, 0.000, 0.196, 0.113, 0.092, 0.061)
    score.lineto(0.299, 0.261)
    score.lineto(0.299, 0.287)
    score.lineto(0.082, 0.287)
    score.draw_or_fill_in
    score
  end

  def draw_s(*args)
    score = SND_Draw.new("s", *args)
    score.moveto(0.148, 0.066)
    score.curveto(0.161, 0.028, 0.130, 0.002, 0.083, 0.009)
    score.curveto(0.032, 0.038, 0.107, 0.045, 0.077, 0.088)
    score.curveto(0.055, 0.113, 0.018, 0.101, 0.014, 0.068)
    score.curveto(0.019, -0.043, 0.216, -0.041, 0.208, 0.076)
    score.curveto(0.200, 0.141, 0.113, 0.167, 0.091, 0.211)
    score.curveto(0.072, 0.255, 0.134, 0.297, 0.172, 0.268)
    score.curveto(0.212, 0.237, 0.145, 0.225, 0.188, 0.188)
    score.curveto(0.223, 0.179, 0.242, 0.199, 0.236, 0.224)
    score.curveto(0.209, 0.327, 0.028, 0.306, 0.032, 0.212)
    score.curveto(0.037, 0.149, 0.142, 0.116, 0.148, 0.066)
    score.draw_or_fill_in
    score
  end

  def draw_r(*args)
    score = SND_Draw.new("r", *args)
    score.moveto(0.210, 0.010)
    score.curveto(0.243, 0.184, 0.262, 0.219, 0.295, 0.246)
    score.curveto(0.350, 0.261, 0.311, 0.213, 0.349, 0.198)
    score.curveto(0.398, 0.181, 0.431, 0.252, 0.368, 0.291)
    score.curveto(0.327, 0.308, 0.278, 0.268, 0.256, 0.235)
    score.curveto(0.303, 0.336, 0.172, 0.345, 0.072, 0.198)
    score.lineto(0.085, 0.186)
    score.curveto(0.123, 0.248, 0.206, 0.312, 0.185, 0.239)
    score.lineto(0.129, 0.010)
    score.lineto(0.210, 0.010)
    score.draw_or_fill_in
    score
  end

  #
  # NOTE HEADS and FLAGS
  # 
  def draw_double_whole_note(*args)
    score = SND_Draw.new("double whole note", *args)
    score.moveto(0.298, 0.127)
    score.curveto(0.393, 0.127, 0.501, 0.087, 0.505, 0.000)
    score.curveto(0.508, -0.095, 0.393, -0.128, 0.298, -0.127)
    score.curveto(0.207, -0.125, 0.100, -0.087, 0.100, 0.000)
    score.curveto(0.102, 0.091, 0.204, 0.127, 0.298, 0.127)
    score.moveto(0.263, 0.108)
    score.curveto(0.325, 0.124, 0.370, 0.065, 0.381, 0.001)
    score.curveto(0.389, -0.050, 0.387, -0.091, 0.342, -0.108)
    score.curveto(0.286, -0.125, 0.247, -0.075, 0.230, -0.023)
    score.curveto(0.214, 0.039, 0.212, 0.089, 0.263, 0.108)
    score.draw_or_fill_in
    score.moveto(0.000, 0.166)
    score.lineto(0.031, 0.166)
    score.lineto(0.031, -0.166)
    score.lineto(0.000, -0.166)
    score.lineto(0.000, 0.166)
    score.draw_or_fill_in
    score.moveto(0.069, 0.166)
    score.lineto(0.100, 0.166)
    score.lineto(0.100, -0.166)
    score.lineto(0.069, -0.166)
    score.lineto(0.069, 0.166)
    score.draw_or_fill_in
    score.moveto(0.505, 0.166)
    score.lineto(0.537, 0.166)
    score.lineto(0.537, -0.166)
    score.lineto(0.505, -0.166)
    score.lineto(0.505, 0.166)
    score.draw_or_fill_in
    score.moveto(0.576, 0.166)
    score.lineto(0.607, 0.166)
    score.lineto(0.607, -0.166)
    score.lineto(0.576, -0.166)
    score.lineto(0.576, 0.166)
    score.draw_or_fill_in
    score
  end

  def draw_whole_note(*args)
    score = SND_Draw.new("whole note", *args)
    score.moveto(0.198, 0.127)
    score.curveto(0.293, 0.127, 0.402, 0.087, 0.405, 0.000)
    score.curveto(0.408, -0.095, 0.293, -0.128, 0.198, -0.127)
    score.curveto(0.107, -0.125, 0.000, -0.087, 0.000, 0.000)
    score.curveto(0.002, 0.091, 0.104, 0.127, 0.198, 0.127)
    score.moveto(0.163, 0.108)
    score.curveto(0.225, 0.124, 0.270, 0.065, 0.281, 0.001)
    score.curveto(0.289, -0.050, 0.287, -0.091, 0.242, -0.108)
    score.curveto(0.186, -0.125, 0.147, -0.075, 0.130, -0.023)
    score.curveto(0.114, 0.039, 0.112, 0.089, 0.163, 0.108)
    score.draw_or_fill_in
    score
  end

  def draw_half_note(*args)
    score = SND_Draw.new("half note", *args)
    score.moveto(0.020, -0.101)
    score.curveto(-0.063, 0.037, 0.185, 0.197, 0.268, 0.107)
    score.curveto(0.379, -0.010, 0.137, -0.193, 0.021, -0.101)
    score.moveto(0.043, -0.082)
    score.curveto(0.015, -0.007, 0.207, 0.125, 0.239, 0.087)
    score.curveto(0.291, 0.027, 0.097, -0.139, 0.043, -0.082)
    score.draw_or_fill_in
    score
  end

  def draw_quarter_note(*args)
    score = SND_Draw.new("quarer note", *args)
    score.moveto(0.014, -0.088)
    score.curveto(-0.014, -0.030, 0.026, 0.056, 0.090, 0.096)
    score.curveto(0.144, 0.128, 0.230, 0.142, 0.270, 0.092)
    score.curveto(0.316, 0.024, 0.258, -0.060, 0.190, -0.100)
    score.curveto(0.130, -0.126, 0.066, -0.136, 0.014, -0.088)
    score.draw_or_fill_in
    score
  end

  def draw_diamond(*args)
    score = SND_Draw.new("diamond", *args)
    score.moveto(0, 0)
    score.rlineto(0.14, 0.14)
    score.rlineto(0.14, -0.14)
    score.rlineto(-0.14, -0.14)
    score.rlineto(-0.14, 0.14)
    score.draw_or_fill_in
    score
  end

  def draw_diamond_1(*args) # Anders Vinjar
    score = SND_Draw.new("diamond-1", *args)
    score.moveto(0, 0)
    score.curveto(0.04, 0.03, 0.10, 0.07, 0.14, 0.14)
    score.curveto(0.12, 0.05, 0.08, 0.03, 0.0, 0.0)
    score.moveto(0.28, 0.0)
    score.curveto(0.24, -0.03, 0.18, -0.07, 0.14, -0.14)
    score.curveto(0.16, -0.07, 0.20, -0.03, 0.28, 0.0)
    score.moveto(0.0, 0.0)
    score.lineto(0.05, 0.03)
    score.curveto(0.1, 0.0, 0.13, -0.02, 0.17, -0.08)
    score.lineto(0.14, -0.14)
    score.curveto(0.10, -0.07, 0.04, -0.03, 0, 0)
    score.moveto(0.28, 0.0)
    score.lineto(0.23, -0.03)
    score.curveto(0.20, -0.02, 0.14, 0.01, 0.11, 0.09)
    score.lineto(0.14, 0.14)
    score.curveto(0.18, 0.07, 0.24, 0.03, 0.28, 0.0)
    score.draw_or_fill_in
    score
  end

  def draw_filled_diamond_1(*args)
    score = SND_Draw.new("filled diamond-1", *args)
    score.moveto(0, 0)
    score.curveto(0.04, 0.03, 0.10, 0.07, 0.14, 0.14)
    score.curveto(0.18, 0.07, 0.24, 0.03, 0.28, 0.0)
    score.curveto(0.24, -0.03, 0.18, -0.07, 0.14, -0.14)
    score.curveto(0.10, -0.07, 0.04, -0.03, 0, 0)
    score.draw_or_fill_in
    score
  end

  def draw_rhythmX(*args)
    score = SND_Draw.new("X", *args)
    score.moveto(0.128, 0.000)
    score.lineto(0.002, 0.102)
    score.lineto(0.020, 0.124)
    score.lineto(0.150, 0.020)
    score.lineto(0.278, 0.124)
    score.lineto(0.296, 0.102)
    score.lineto(0.170, 0.000)
    score.lineto(0.298, -0.102)
    score.lineto(0.278, -0.124)
    score.lineto(0.150, -0.020)
    score.lineto(0.020, -0.124)
    score.lineto(0.004, -0.104)
    score.lineto(0.128, 0.000)
    score.draw_or_fill_in
    score
  end

  def draw_circled_x(*args)
    score = SND_Draw.new("circled-X", *args)
    off = 0.06
    size = 0.75
    score.moveto(size * (off + 0.020), size * 0.124)
    score.lineto(size * (off + 0.150), size * 0.020)
    score.lineto(size * (off + 0.278), size * 0.124)
    score.curveto(size * (off + 0.224), size * 0.190,
                  size * (off + 0.085), size * 0.188, size * (off + 0.020), size * 0.124)
    score.moveto(size * (off + 0.296), size * 0.102)
    score.lineto(size * (off + 0.170), size * 0.000)
    score.lineto(size * (off + 0.297), size * -0.101)
    score.curveto(size * (off + 0.341), size * -0.056,
                  size * (off + 0.339), size * 0.050, size * (off + 0.296), size * 0.102)
    score.moveto(size * (off + 0.278), size * -0.124)
    score.lineto(size * (off + 0.150), size * -0.020)
    score.lineto(size * (off + 0.020), size * -0.124)
    score.curveto(size * (off + 0.080), size * -0.189,
                  size * (off + 0.221), size * -0.194, size * (off + 0.278), size * -0.124)
    score.moveto(size * (off + 0.004), size * -0.104)
    score.lineto(size * (off + 0.128), size * 0.000)
    score.lineto(size * (off + 0.003), size * 0.102)
    score.curveto(size * (off + -0.048), size * 0.055,
                  size * (off + -0.044), size * -0.061, size * (off + 0.004), size * -0.104)
    score.moveto(size * (off + -0.065), size * 0.003)
    score.curveto(size * (off + -0.064), size * 0.264,
                  size * (off + 0.360), size * 0.280, size * (off + 0.361), size * 0.000)
    score.curveto(size * (off + 0.361), size * -0.278,
                  size * (off + -0.059), size * -0.279, size * (off + -0.064), size * 0.000)
    score.draw_or_fill_in
    score
  end

  def draw_slash(*args)
    score = SND_Draw.new("slash", *args)
    score.moveto(0, -0.15) 
    score.rlineto(0.275, 0.275) 
    score.draw
    score
  end

  def draw_mslash(*args)
    score = SND_Draw.new("m-slash", *args)
    score.moveto(0.05, -0.225) 
    score.rlineto(0.2, 0.45) 
    score.draw
    score
  end

  def draw_triangle(*args)
    score = SND_Draw.new("triangle", *args)
    score.moveto(0.14, -0.11)
    score.rlineto(-0.14, 0)
    score.rlineto(0.14, 0.25)
    score.rlineto(0.14, -0.25)
    score.rlineto(-0.14, 0)
    score.draw_or_fill_in
    score
  end

  def draw_square(*args)
    score = SND_Draw.new("square", *args)
    score.moveto(0.13, -0.13)
    score.rlineto(-0.13, 0)
    score.rlineto(0, 0.26)
    score.rlineto(0.26, 0)
    score.rlineto(0, -0.26)
    score.rlineto(-0.13, 0)
    score.draw_or_fill_in
    score
  end

  def draw_8th_flag_up(*args)
    score = SND_Draw.new("8th flag up", *args)
    score.moveto(0.000, 0.296)
    score.lineto(0.019, 0.296)
    score.curveto(0.022, 0.239, 0.031, 0.146, 0.101, 0.092)
    score.curveto(0.246, -0.053, 0.275, -0.121, 0.268, -0.260)
    score.curveto(0.260, -0.411, 0.213, -0.482, 0.182, -0.540)
    score.lineto(0.175, -0.535)
    score.curveto(0.221, -0.453, 0.247, -0.358, 0.244, -0.257)
    score.curveto(0.243, -0.119, 0.118, 0.000, 0.021, 0.000)
    score.lineto(0.021, -0.024)
    score.lineto(0.000, -0.024)
    score.lineto(0.000, 0.296)
    score.draw_or_fill_in
    score
  end

  def draw_extend_flag_up(*args)
    score = SND_Draw.new("extend flag up", *args)
    score.moveto(0.000, 0.296)
    score.lineto(0.021, 0.296)
    score.curveto(0.025, 0.196, 0.065, 0.158, 0.122, 0.094)
    score.curveto(0.213, -0.011, 0.400, -0.160, 0.264, -0.512)
    score.lineto(0.262, -0.494)
    score.curveto(0.319, -0.325, 0.290, -0.185, 0.240, -0.118)
    score.curveto(0.194, -0.051, 0.065, 0.076, 0.026, 0.033)
    score.lineto(0.026, -0.025)
    score.lineto(0.000, -0.025)
    score.lineto(0.000, 0.296)
    score.draw_or_fill_in
    score
  end

  def draw_8th_flag_down(*args)
    score = SND_Draw.new("8th flag down", *args)
    score.moveto(0.000, -0.296)
    score.lineto(0.019, -0.296)
    score.curveto(0.022, -0.239, 0.031, -0.146, 0.101, -0.092)
    score.curveto(0.246, 0.053, 0.275, 0.121, 0.268, 0.260)
    score.curveto(0.260, 0.411, 0.213, 0.482, 0.182, 0.540)
    score.lineto(0.175, 0.535)
    score.curveto(0.221, 0.453, 0.247, 0.358, 0.244, 0.257)
    score.curveto(0.243, 0.119, 0.118, 0.000, 0.021, 0.000)
    score.lineto(0.021, 0.024)
    score.lineto(0.000, 0.024)
    score.lineto(0.000, -0.296)
    score.draw_or_fill_in
    score
  end

  def draw_extend_flag_down(*args)
    score = SND_Draw.new("extend flag down", *args)
    score.moveto(0.000, -0.296)
    score.lineto(0.021, -0.296)
    score.curveto(0.025, -0.196, 0.065, -0.158, 0.122, -0.094)
    score.curveto(0.213, 0.011, 0.400, 0.160, 0.264, 0.512)
    score.lineto(0.262, 0.494)
    score.curveto(0.319, 0.325, 0.290, 0.185, 0.240, 0.118)
    score.curveto(0.194, 0.051, 0.065, -0.076, 0.026, -0.033)
    score.lineto(0.026, 0.025)
    score.lineto(0.000, 0.025)
    score.lineto(0.000, -0.296)
    score.draw_or_fill_in
    score
  end

  #
  # RESTS
  #
  def draw_draw_whole_rest(*args)
    score = SND_Draw.new("whole rest", *args)
    score.moveto(0.063, 0.253)
    score.lineto(0.063, 0.127)
    score.lineto(0.359, 0.127)
    score.lineto(0.359, 0.253)
    score.lineto(0.422, 0.253)
    score.lineto(0.422, 0.256)
    score.lineto(0.000, 0.256)
    score.lineto(0.000, 0.253)
    score.lineto(0.063, 0.253)
    score.draw_or_fill_in
    score
  end
  
  def draw_half_rest(*args)
    score = SND_Draw.new("half rest", *args)
    score.moveto(0.063, 0.127)
    score.lineto(0.063, 0.000)
    score.lineto(0.000, 0.000)
    score.lineto(0.000, -0.003)
    score.lineto(0.422, -0.003)
    score.lineto(0.422, 0.000)
    score.lineto(0.359, 0.000)
    score.lineto(0.359, 0.127)
    score.lineto(0.063, 0.127)
    score.draw_or_fill_in
    score
  end
  
  def draw_quarter_rest(*args)
    score = SND_Draw.new("quarter rest", *args)
    score.moveto(0.072, 0.358)
    score.curveto(0.120, 0.253, 0.160, 0.158, 0.038, 0.058)
    score.lineto(0.182, -0.103)
    score.curveto(-0.028, -0.032, -0.058, -0.248, 0.180, -0.343)
    score.lineto(0.187, -0.322)
    score.curveto(0.042, -0.222, 0.150, -0.103, 0.230, -0.142)
    score.lineto(0.243, -0.127)
    score.curveto(0.220, -0.082, 0.170, -0.043, 0.157, 0.012)
    score.curveto(0.165, 0.120, 0.290, 0.113, 0.237, 0.187)
    score.lineto(0.103, 0.355)
    score.lineto(0.072, 0.358)
    score.draw_or_fill_in
    score
  end
  
  def draw_8th_rest(*args)
    score = SND_Draw.new("8th rest", *args)
    score.moveto(0.164, 0.131)
    score.curveto(0.171, 0.222, 0.046, 0.236, 0.028, 0.136)
    score.curveto(0.032, 0.030, 0.159, 0.033, 0.224, 0.086)
    score.lineto(0.140, -0.256)
    score.lineto(0.174, -0.256)
    score.lineto(0.286, 0.200)
    score.lineto(0.259, 0.200)
    score.curveto(0.249, 0.140, 0.188, 0.074, 0.142, 0.079)
    score.curveto(0.154, 0.096, 0.166, 0.114, 0.164, 0.131)
    score.draw_or_fill_in
    score
  end
  
  def draw_16th_rest(*args)
    score = SND_Draw.new("16th rest", *args)
    score.moveto(0.102, -0.170)
    score.curveto(0.128, -0.152, 0.141, -0.128, 0.136, -0.113)
    score.curveto(0.144, -0.032, 0.017, -0.020, 0.000, -0.113)
    score.curveto(0.000, -0.224, 0.146, -0.224, 0.208, -0.163)
    score.lineto(0.118, -0.503)
    score.lineto(0.150, -0.503)
    score.lineto(0.334, 0.200)
    score.lineto(0.302, 0.200)
    score.curveto(0.292, 0.141, 0.221, 0.065, 0.168, 0.077)
    score.curveto(0.167, 0.079, 0.162, 0.081, 0.164, 0.082)
    score.curveto(0.186, 0.096, 0.196, 0.119, 0.194, 0.136)
    score.curveto(0.197, 0.221, 0.070, 0.234, 0.056, 0.136)
    score.curveto(0.058, 0.029, 0.205, 0.018, 0.276, 0.100)
    score.lineto(0.232, -0.068)
    score.curveto(0.221, -0.126, 0.165, -0.181, 0.106, -0.171)
    score.draw_or_fill_in
    score
  end
  
  def draw_32nd_rest(*args)
    score = SND_Draw.new("32nd rest", *args)
    score.moveto(0.164, 0.077)
    score.curveto(0.182, 0.099, 0.194, 0.114, 0.199, 0.136)
    score.curveto(0.201, 0.228, 0.070, 0.241, 0.058, 0.136)
    score.curveto(0.064, 0.034, 0.211, 0.028, 0.270, 0.083)
    score.lineto(0.228, -0.076)
    score.curveto(0.219, -0.126, 0.165, -0.174, 0.107, -0.168)
    score.curveto(0.102, -0.170, 0.109, -0.168, 0.104, -0.168)
    score.curveto(0.124, -0.151, 0.136, -0.133, 0.136, -0.115)
    score.curveto(0.142, -0.031, 0.007, -0.019, 0.000, -0.115)
    score.curveto(0.003, -0.216, 0.146, -0.216, 0.206, -0.170)
    score.lineto(0.128, -0.510)
    score.lineto(0.172, -0.510)
    score.lineto(0.400, 0.453)
    score.lineto(0.364, 0.453)
    score.curveto(0.354, 0.393, 0.295, 0.329, 0.229, 0.334)
    score.curveto(0.260, 0.372, 0.253, 0.354, 0.260, 0.388)
    score.curveto(0.268, 0.470, 0.129, 0.486, 0.117, 0.388)
    score.curveto(0.125, 0.285, 0.261, 0.281, 0.330, 0.333)
    score.lineto(0.294, 0.196)
    score.curveto(0.282, 0.133, 0.235, 0.077, 0.160, 0.077)
    score.draw_or_fill_in
    score
  end
  
  def draw_64th_rest(*args)
    score = SND_Draw.new("64th rest", *args)
    score.moveto(0.161, 0.081)
    score.curveto(0.188, 0.096, 0.196, 0.117, 0.194, 0.136)
    score.curveto(0.205, 0.210, 0.076, 0.231, 0.058, 0.136)
    score.curveto(0.054, 0.029, 0.214, 0.019, 0.270, 0.087)
    score.lineto(0.228, -0.076)
    score.curveto(0.216, -0.124, 0.163, -0.184, 0.102, -0.170)
    score.curveto(0.129, -0.151, 0.138, -0.134, 0.136, -0.115)
    score.curveto(0.147, -0.039, 0.018, -0.013, 0.000, -0.115)
    score.curveto(0.000, -0.220, 0.151, -0.228, 0.206, -0.170)
    score.lineto(0.122, -0.510)
    score.lineto(0.164, -0.510)
    score.lineto(0.454, 0.708)
    score.lineto(0.419, 0.708)
    score.curveto(0.409, 0.644, 0.341, 0.566, 0.281, 0.588)
    score.curveto(0.306, 0.610, 0.314, 0.630, 0.314, 0.642)
    score.curveto(0.325, 0.721, 0.189, 0.734, 0.176, 0.642)
    score.curveto(0.179, 0.536, 0.333, 0.532, 0.386, 0.590)
    score.lineto(0.347, 0.434)
    score.curveto(0.341, 0.384, 0.284, 0.325, 0.225, 0.334)
    score.curveto(0.246, 0.350, 0.254, 0.370, 0.254, 0.388)
    score.curveto(0.261, 0.466, 0.134, 0.486, 0.117, 0.388)
    score.curveto(0.119, 0.284, 0.266, 0.279, 0.328, 0.338)
    score.lineto(0.296, 0.207)
    score.curveto(0.285, 0.154, 0.235, 0.063, 0.164, 0.080)
    score.draw_or_fill_in
    score
  end
  
  def draw_128th_rest(*args)
    score = SND_Draw.new("128th rest", *args)
    score.moveto(0.277, 0.586)
    score.curveto(0.306, 0.604, 0.314, 0.623, 0.314, 0.641)
    score.curveto(0.320, 0.727, 0.189, 0.733, 0.176, 0.641)
    score.curveto(0.181, 0.536, 0.343, 0.526, 0.386, 0.589)
    score.lineto(0.350, 0.434)
    score.curveto(0.341, 0.400, 0.309, 0.323, 0.226, 0.333)
    score.curveto(0.246, 0.350, 0.254, 0.369, 0.254, 0.389)
    score.curveto(0.267, 0.469, 0.134, 0.480, 0.119, 0.389)
    score.curveto(0.111, 0.286, 0.284, 0.274, 0.326, 0.336)
    score.lineto(0.296, 0.209)
    score.curveto(0.286, 0.179, 0.253, 0.067, 0.164, 0.079)
    score.curveto(0.189, 0.096, 0.194, 0.114, 0.194, 0.136)
    score.curveto(0.200, 0.223, 0.073, 0.226, 0.059, 0.136)
    score.curveto(0.059, 0.023, 0.221, 0.027, 0.269, 0.079)
    score.lineto(0.229, -0.076)
    score.curveto(0.214, -0.117, 0.180, -0.183, 0.106, -0.170)
    score.curveto(0.129, -0.153, 0.136, -0.133, 0.136, -0.116)
    score.curveto(0.147, -0.033, 0.004, -0.020, 0.000, -0.116)
    score.curveto(0.004, -0.229, 0.160, -0.224, 0.204, -0.176)
    score.lineto(0.121, -0.510)
    score.lineto(0.164, -0.510)
    score.lineto(0.514, 0.954)
    score.lineto(0.481, 0.954)
    score.curveto(0.467, 0.903, 0.436, 0.820, 0.340, 0.836)
    score.curveto(0.367, 0.849, 0.377, 0.874, 0.371, 0.890)
    score.curveto(0.383, 0.967, 0.250, 0.993, 0.236, 0.890)
    score.curveto(0.240, 0.787, 0.406, 0.776, 0.446, 0.830)
    score.lineto(0.416, 0.704)
    score.curveto(0.406, 0.684, 0.370, 0.576, 0.279, 0.583)
    score.draw_or_fill_in
    score
  end
  
  def draw_measure_rest(*args)
    score = SND_Draw.new("measure rest", *args)
    score.moveto(0, 0.25)
    score.rlineto(0, -0.5)
    score.moveto(0.5, 0.25)
    score.rlineto(0, -0.5)
    score.draw
    score.moveto(0, 0)
    score.lineto(0.5, 0)
    score.draw
    score
  end
  
  def draw_double_whole_rest(*args)
    score = SND_Draw.new("double whole rest", *args)
    score.moveto(0, 0.5)
    score.rlineto(0, 0.25)
    score.rlineto(0.15, 0)
    score.rlineto(0, -0.25)
    score.rlineto(-0.15, 0)
    score.draw_or_fill_in
    score
  end
end

include Musglyphs

def musglyphs_test_1
  Musglyphs.public_methods.sort.each do |dr|
    next unless dr =~ /^draw_/
    clm_print("\n%s", dr)
    update_time_graph(0, 0)
    if dr == "draw_trill_sections" or dr == "draw_arpeggios"
      snd_func(dr.intern, 5, 100, 200, 200)
    else
      snd_func(dr.intern, 150, 150, 200)
    end
    sleep 1
  end
  nil
end

def musglyphs_test
  [:draw_treble_clef,
   :draw_percussion_clef,
   :draw_c_clef,
   :draw_bass_clef,
   :draw_turn,
   :draw_mordent,
   :draw_double_mordent,
   :draw_trill_section,
   :draw_trill_sections,
   :draw_arpeggio,
   :draw_arpeggios,
   :draw_tr,
   :draw_accent,
   :draw_tnecca,
   :draw_breath_mark,
   :draw_caesura,
   :draw_fermata,
   :draw_upside_down_fermata,
   :draw_repeat_sign,
   :draw_upper_bracket,
   :draw_lower_bracket,
   :draw_segno,
   :draw_coda,
   :draw_pedal_off,
   :draw_ped,
   :draw_left_paren,
   :draw_right_paren,
   :draw_wedge,
   :draw_down_bow,
   :draw_up_bow,
   :draw_zero,
   :draw_one,
   :draw_two,
   :draw_three,
   :draw_four,
   :draw_five,
   :draw_six,
   :draw_seven,
   :draw_eight,
   :draw_nine,
   :draw_common_time,
   :draw_cut_time,
   :draw_plus,
   :draw_sharp,
   :draw_flat,
   :draw_double_sharp,
   :draw_natural,
   :draw_double_flat,
   :draw_f,
   :draw_p,
   :draw_lig_p,
   :draw_m,
   :draw_n,
   :draw_niente,
   :draw_subito,
   :draw_z,
   :draw_s,
   :draw_r,
   :draw_double_whole_note,
   :draw_whole_note,
   :draw_half_note,
   :draw_quarter_note,
   :draw_diamond,
   :draw_diamond_1,
   :draw_filled_diamond_1,
   :draw_rhythmX,
   :draw_circled_x,
   :draw_slash,
   :draw_mslash,
   :draw_triangle,
   :draw_square,
   :draw_8th_flag_up,
   :draw_extend_flag_up,
   :draw_8th_flag_down,
   :draw_extend_flag_down,
   :draw_draw_whole_rest,
   :draw_half_rest,
   :draw_quarter_rest,
   :draw_8th_rest,
   :draw_16th_rest,
   :draw_32nd_rest,
   :draw_64th_rest,
   :draw_128th_rest,
   :draw_measure_rest,
   :draw_double_whole_rest].each do |dr|
    clm_print("\n%s", dr)
    update_time_graph(0, 0)
    if dr == :draw_trill_sections or dr == :draw_arpeggios
      snd_func(dr, 5, 100, 200, 200)
    else
      snd_func(dr, 150, 150, 200)
    end
    sleep 1
  end
  nil
end

# musglyphs.rb ends here
