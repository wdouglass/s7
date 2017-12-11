# poly.rb -- polynomial-related stuff; poly.scm --> poly.rb

# Translator: Michael Scholz <mi-scholz@users.sourceforge.net>
# Created: 05/04/09 23:55:07
# Changed: 17/11/30 22:57:04

# class Complex
#  to_f
#  to_f_or_c
#  
# class Poly < Vec
#  inspect
#  to_poly
#  reduce
#  +(other)
#  *(other)
#  /(other)
#  derivative
#  resultant(other)
#  discriminant
#  gcd(other)
#  roots
#  eval(x)
#
# class Float
#  +(other)
#  *(other)
#  /(other)
#
# class String
#  to_poly
#
# class Array
#  to_poly
#
# class Vct
#  to_poly
#
# Poly(obj)
# make_poly(len, init, &body)
# poly?(obj)
# poly(*vals)
# poly_reduce(obj)
# poly_add(obj1, obj2)
# poly_multiply(obj1, obj2)
# poly_div(obj1, obj2)
# poly_derivative(obj)
# poly_gcd(obj1, obj2)
# poly_roots(obj)

require "clm"
require "mix"
include Math

class Complex
  # XXX: attr_writer :real, :imag
  #      Doesn't work any longer.
  #      Complex objects are now frozen objects.
  #      (Thu Nov 30 21:29:10 CET 2017)
  with_silence do
    def to_f
      self.real.to_f
    end
  end

  def to_f_or_c
    self.imag.zero? ? self.to_f : self
  end
end

class Poly < Vec
  Poly_roots_epsilon = 1.0e-6

  def inspect
    @name = "poly"
    super
  end
  
  def to_poly
    self
  end
  
  def reduce
    if self.last.zero?
      i = self.length - 1
      while self[i].zero? and i > 0
        i -= 1
      end
      self[0, i + 1]
    else
      self
    end
  end
  # [1, 2, 3].to_poly.reduce             ==> poly(1.0, 2.0, 3.0)
  # poly(1, 2, 3, 0, 0, 0).reduce        ==> poly(1.0, 2.0, 3.0)
  # vct(0, 0, 0, 0, 1, 0).to_poly.reduce ==> poly(0.0, 0.0, 0.0, 0.0, 1.0)
  
  def poly_add(other)
    assert_type((array?(other) or vct?(other) or number?(other)),
                other, 0, "a poly, a vct an array, or a number")
    if number?(other)
      v = self.dup
      v[0] += other
      v
    else
      if self.length > other.length
        self.add(other)
      else
        Poly(other).add(self)
      end
    end
  end
  alias + poly_add
  # poly(0.1, 0.2, 0.3) + poly(0, 1, 2, 3, 4) ==> poly(0.1, 1.2, 2.3, 3.0, 4.0)
  # poly(0.1, 0.2, 0.3) + 0.5                 ==> poly(0.6, 0.2, 0.3)
  # 0.5 + poly(0.1, 0.2, 0.3)                 ==> poly(0.6, 0.2, 0.3)

  def poly_multiply(other)
    assert_type((array?(other) or vct?(other) or number?(other)),
                other, 0, "a poly, a vct, an array, or a number")
    if number?(other)
      Poly(self.scale(Float(other)))
    else
      len = self.length + other.length
      m = Poly.new(len, 0.0)
      self.each_with_index do |val1, i|
        other.each_with_index do |val2, j|
          m[i + j] = m[i + j] + val1 * val2
        end
      end
      m
    end
  end
  alias * poly_multiply
  # poly(1, 1) * poly(-1, 1)        ==> poly(-1.0, 0.0, 1.0, 0.0)
  # poly(-5, 1) * poly(3, 7, 2)     ==> poly(-15.0, -32.0, -3.0, 2.0, 0.0)
  # poly(-30, -4, 2) * poly(0.5, 1) ==> poly(-15.0, -32.0, -3.0, 2.0, 0.0)
  # poly(-30, -4, 2) * 0.5          ==> poly(-15.0, -2.0, 1.0)
  # 2.0 * poly(-30, -4, 2)          ==> poly(-60.0, -8.0, 4.0)

  def poly_div(other)
    assert_type((array?(other) or vct?(other) or number?(other)),
                other, 0, "a poly, a vct, an array, or a number")
    if number?(other)
      [self * (1.0 / other), poly(0.0)]
    else
      if other.length > self.length
        [poly(0.0), other.to_poly]
      else
        r = self.dup
        q = Poly.new(self.length, 0.0)
        n = self.length - 1
        nv = other.length - 1
        (n - nv).downto(0) do |i|
          q[i] = r[nv + i] / other[nv]
          (nv + i - 1).downto(i) do |j|
            r[j] = r[j] - q[i] * other[j - i]
          end
        end
        nv.upto(n) do |i|
          r[i] = 0.0
        end
        [q, r]
      end
    end
  end
  alias / poly_div
  # poly(-1.0, 0.0, 1.0) / poly(1.0, 1.0)
  #   ==> [poly(-1.0, 1.0, 0.0),       poly(0.0, 0.0, 0.0)]
  # poly(-15, -32, -3, 2) / poly(-5, 1)
  #   ==> [poly(3.0, 7.0, 2.0, 0.0),   poly(0.0, 0.0, 0.0, 0.0)]
  # poly(-15, -32, -3, 2) / poly(3, 1)
  #   ==> [poly(-5.0, -9.0, 2.0, 0.0), poly(0.0, 0.0, 0.0, 0.0)]
  # poly(-15, -32, -3, 2) / poly(0.5, 1)
  #   ==> [poly(-30.0, -4.0, 2.0, 0.0), poly(0.0, 0.0, 0.0, 0.0)]
  # poly(-15, -32, -3, 2) / poly(3, 7, 2)
  #   ==> [poly(-5.0, 1.0, 0.0, 0.0),  poly(0.0, 0.0, 0.0, 0.0)]
  # poly(-15, -32, -3, 2) / 2.0
  #   ==> [poly(-7.5, -16.0, -1.5, 1.0), poly(0.0)]

  def derivative
    len = self.length - 1
    pl = Poly.new(len, 0.0)
    j = len
    (len - 1).downto(0) do |i|
      pl[i] = self[j] * j
      j -= 1
    end
    pl
  end
  # poly(0.5, 1.0, 2.0, 4.0).derivative ==> poly(1.0, 4.0, 12.0)

  def resultant(other)
    m = self.length
    m1 = m - 1
    n = other.length
    n1 = n - 1
    d = n1 + m1
    mat = Array.new(d) do
      Vct.new(d, 0.0)
    end
    n1.times do |i|
      m.times do |j|
        mat[i][i + j] = self[m1 - j]
      end
    end
    m1.times do |i|
      n.times do |j|
        mat[i + n1][i + j] = other[n1 - j]
      end
    end
    determinant(mat)
  end
  # poly(-1, 0, 1).resultant([1, -2, 1]) ==> 0.0
  # poly(-1, 0, 2).resultant([1, -2, 1]) ==> 1.0
  # poly(-1, 0, 1).resultant([1, 1])     ==> 0.0
  # poly(-1, 0, 1).resultant([2, 1])     ==> 3.0

  def discriminant
    self.resultant(self.derivative)
  end
  # poly(-1, 0, 1).discriminant ==> -4.0
  # poly(1, -2, 1).discriminant ==>  0.0
  # (poly(-1, 1) * poly(-1, 1) * poly(3, 1)).reduce.discriminant
  #   ==> 0.0
  # (poly(-1, 1) * poly(-1, 1) * poly(3, 1) * poly(2, 1)).reduce.discriminant
  #   ==> 0.0
  # (poly(1, 1) * poly(-1, 1) * poly(3, 1) * poly(2, 1)).reduce.discriminant
  #   ==> 2304.0
  # (poly(1, 1) * poly(-1, 1) * poly(3, 1) * poly(3, 1)).reduce.discriminant
  #   ==> 0.0
  
  def gcd(other)
    assert_type((array?(other) or vct?(other)), other, 0,
                "a poly, a vct or an array")
    if self.length < other.length
      poly(0.0)
    else
      qr = self.poly_div(other).map do |m|
        m.reduce
      end
      if qr[1].length == 1
        if qr[1][0].zero?
          Poly(other)
        else
          poly(0.0)
        end
      else
        qr[0].gcd(qr[1])
      end
    end
  end
  # (poly(2, 1) * poly(-3, 1)).reduce.gcd(poly(2, 1))
  #   ==> poly(2.0, 1.0)
  # (poly(2, 1) * poly(-3, 1)).reduce.gcd(poly(3, 1))
  #   ==> poly(0.0)
  # (poly(2, 1) * poly(-3, 1)).reduce.gcd(poly(-3, 1))
  #   ==> poly(-3.0, 1.0)
  # (poly(8, 1) * poly(2, 1) * poly(-3, 1)).reduce.gcd(poly(-3, 1))
  #   ==> poly(-3.0, 1.0)
  # (poly(8, 1) * poly(2, 1) *
  #  poly(-3, 1)).reduce.gcd((poly(8, 1) * poly(-3, 1)).reduce)
  #   ==> poly(-24.0, 5.0, 1.0)
  # poly(-1, 0, 1).gcd(poly(2, -2, -1, 1))
  #   ==> poly(0.0)
  # poly(2, -2, -1, 1).gcd(poly(-1, 0, 1))
  #   ==> poly(1.0, -1.0)
  # poly(2, -2, -1, 1).gcd(poly(-2.5, 1))
  #   ==> poly(0.0)

  def roots
    rts = poly()
    deg = self.length - 1
    if deg.zero?
      rts
    else
      if self[0].zero?
        if deg == 1
          poly(0.0)
        else
          Poly.new(deg) do |i|
            self[i + 1]
          end.roots.unshift(0.0)
        end
      else
        if deg == 1
          linear_root(self[1], self[0])
        else
          if deg == 2
            quadratic_root(self[2], self[1], self[0])
          else
            if deg == 3 and
               (rts = cubic_root(self[3], self[2], self[1], self[0]))
              rts
            else
              if deg == 4 and
                 (rts = quartic_root(self[4], self[3],
                                     self[2], self[1], self[0]))
                rts
              else
                ones = 0
                1.upto(deg) do |i|
                  if self[i].nonzero?
                    ones += 1
                  end
                end
                if ones == 1
                  nth_root(self[deg], self[0], deg)
                else
                  if ones == 2 and deg.even? and self[deg / 2].nonzero?
                    n = deg / 2
                    poly(self[0], self[deg / 2], self[deg]).roots.each do |qr|
                      rts.push(*nth_root(1.0, -qr, n.to_f))
                    end
                    rts
                  else
                    if deg > 3 and
                        ones == 3 and
                        (deg % 3).zero? and
                        self[deg / 3].nonzero? and
                        self[(deg * 2) / 3].nonzero?
                      n = deg / 3
                      poly(self[0],
                           self[deg / 3],
                           self[(deg * 2) / 3],
                           self[deg]).roots.each do |qr|
                        rts.push(*nth_root(1.0, -qr, n.to_f))
                      end
                      rts
                    else
                      q = self.dup
                      pp = self.derivative
                      qp = pp.dup
                      n = deg
                      x = Complex(1.3, 0.314159)
                      v = q.eval(x)
                      m = v.abs * v.abs
                      20.times do # until c_g?
                        if (dx = v / qp.eval(x)).abs <= Poly_roots_epsilon
                          break
                        end
                        20.times do
                          if dx.abs <= Poly_roots_epsilon
                            break
                          end
                          y = x - dx
                          v1 = q.eval(y)
                          if (m1 = v1.abs * v1.abs) < m
                            x = y
                            v = v1
                            m = m1
                            break
                          else
                            dx /= 4.0
                          end
                        end
                      end
                      x = x - self.eval(x) / pp.eval(x)
                      x = x - self.eval(x) / pp.eval(x)
                      if x.imag < Poly_roots_epsilon
                        q = q.poly_div(poly(-x.real, 1.0))
                        n -= 1
                      else
                        q = q.poly_div(poly(x.abs, 0.0, 1.0))
                        n -= 2
                      end
                      rts = if n > 0
                              q.car.reduce.roots
                            else
                              poly()
                            end
                      rts << x.to_f_or_c
                      rts
                    end
                  end
                end
              end
            end
          end
        end
      end
    end
  end
  
  def eval(x)
    sum = self.last
    self.reverse[1..-1].each do |val|
      sum = sum * x + val
    end
    sum
  end

  private
  def submatrix(mx, row, col)
    nmx = Array.new(mx.length - 1) do
      Vct.new(mx.length - 1, 0.0)
    end
    ni = 0
    mx.length.times do |i|
      if i != row
        nj = 0
        mx.length.times do |j|
          if j != col
            nmx[ni][nj] = mx[i][j]
            nj += 1
          end
        end
        ni += 1
      end
    end
    nmx
  end

  def determinant(mx)
    if mx.length == 1
      mx[0][0]
    else
      if mx.length == 2
        mx[0][0] * mx[1][1] - mx[0][1] * mx[1][0]
      else
        if mx.length == 3
          ((mx[0][0] * mx[1][1] * mx[2][2] +
            mx[0][1] * mx[1][2] * mx[2][0] +
            mx[0][2] * mx[1][0] * mx[2][1]) -
           (mx[0][0] * mx[1][2] * mx[2][1] +
            mx[0][1] * mx[1][0] * mx[2][2] +
            mx[0][2] * mx[1][1] * mx[2][0]))
        else
          sum = 0.0
          sign = 1
          mx.length.times do |i|
            mult = mx[0][i]
            if mult != 0.0
              sum = sum + sign * mult * determinant(submatrix(mx, 0, i))
            end
            sign = -sign
          end
          sum
        end
      end
    end
  end

  # ax + b
  def linear_root(a, b)
    poly(-b / a)
  end

  # ax^2 + bx + c
  def quadratic_root(a, b, c)
    d = sqrt(b * b - 4.0 * a * c)
    poly((-b + d) / (2.0 * a), (-b - d) / (2.0 * a))
  end

  # ax^3 + bx^2 + cx + d
  def cubic_root(a, b, c, d)
    # Abramowitz & Stegun 3.8.2
    a0 = d / a
    a1 = c / a
    a2 = b / a
    q = (a1 / 3) - ((a2 * a2) / 9)
    r = ((a1 * a2 - 3 * a0) / 6) - ((a2 * a2 * a2) / 27)
    sq3r2 = sqrt(q * q * q + r * r)
    r1 = (r + sq3r2) ** (1 / 3.0)
    r2 = (r - sq3r2) ** (1 / 3.0)
    incr = (TWO_PI * Complex::I) / 3
    pl = poly(a0, a1, a2, 1)
    sqrt3 = sqrt(-3)
    3.times do |i|
      3.times do |j|
        s1 = r1 * exp(i * incr)
        s2 = r2 * exp(j * incr)
        z1 = simplify_complex((s1 + s2) - (a2 / 3))
        if pl.eval(z1).abs < Poly_roots_epsilon
          z2 = simplify_complex((-0.5 * (s1 + s2)) +
                                (a2 / -3) +
                                ((s1 - s2) * 0.5 * sqrt3))
          if pl.eval(z2).abs < Poly_roots_epsilon
            z3 = simplify_complex((-0.5 * (s1 + s2)) +
                                  (a2 / -3) +
                                  ((s1 - s2) * -0.5 * sqrt3))
            if pl.eval(z3).abs < Poly_roots_epsilon
              return poly(z1, z2, z3)
            end
          end
        end
      end
    end
    false
  end

  # ax^4 + bx^3 + cx^2 + dx + e
  def quartic_root(a, b, c, d, e)
    # Weisstein, "Encyclopedia of Mathematics"
    a0 = e / a
    a1 = d / a
    a2 = c / a
    a3 = b / a
    if yroot = poly((4 * a2 * a0) + -(a1 * a1) + -(a3 * a3 * a0),
                    (a1 * a3) - (4 * a0),
                    -a2,
                    1).roots
      yroot.each do |y1|
        r = sqrt((0.25 * a3 * a3) + (-a2 + y1))
        dd = if r.zero?
              sqrt((0.75 * a3 * a3) +
                   (-2 * a2) +
                   (2 * sqrt(y1 * y1 - 4 * a0)))
            else
              sqrt((0.75 * a3 * a3) + (-2 * a2) + (-(r * r)) +
                   (0.25 * ((4 * a3 * a2) + (-8 * a1) + (-(a3 * a3 * a3)))) / r)
            end
        ee = if r.zero?
              sqrt((0.75 * a3 * a3) +
                   (-2 * a2) +
                   (-2 * sqrt((y1 * y1) - (4 * a0))))
            else
              sqrt((0.75 * a3 * a3) + (-2 * a2) + (-(r * r)) +
                   (-0.25 *
                    ((4 * a3 * a2) + (-8 * a1) + (-(a3 * a3 * a3)))) / r)
            end
        z1 = (-0.25 * a3) + ( 0.5 * r) + ( 0.5 * dd)
        z2 = (-0.25 * a3) + ( 0.5 * r) + (-0.5 * dd)
        z3 = (-0.25 * a3) + (-0.5 * r) + ( 0.5 * ee)
        z4 = (-0.25 * a3) + (-0.5 * r) + (-0.5 * ee)
        if poly(e, d, c, b, a).eval(z1).abs < Poly_roots_epsilon
          return poly(z1, z2, z3, z4)
        end
      end
    end
    false
  end
  
  # ax^n + b
  def nth_root(a, b, deg)
    n = (-b / a) ** (1.0 / deg)
    incr = (TWO_PI * Complex::I) / deg
    rts = poly()
    deg.to_i.times do |i|
      rts.unshift(simplify_complex(exp(i * incr) * n))
    end
    rts
  end

  Poly_roots_epsilon2 = 1.0e-6
  def simplify_complex(a)
    if a.imag.abs < Poly_roots_epsilon2
      (a.real.abs < Poly_roots_epsilon2) ? 0.0 : a.real.to_f
    else
      if a.real.abs < Poly_roots_epsilon2
        # XXX: a.real = 0.0
        #      Doesn't work any longer (see above, class Complex).
        a = Complex(0.0, a.imag)
      end
      a
    end
  end
end

class Float
  unless defined? 0.0.poly_plus
    alias fp_plus +
    def poly_plus(other)
      case other
      when Poly
        other[0] += self
        other
      else
        self.fp_plus(other)
      end
    end
    alias + poly_plus
  end

  unless defined? 0.0.poly_times
    alias fp_times *
    def poly_times(other)
      case other
      when Poly
        Poly(other.scale(self))
      else
        self.fp_times(other)
      end
    end
    alias * poly_times
  end

  unless defined? 0.0.poly_div
    alias fp_div /
    def poly_div(other)
      case other
      when Poly
        [poly(0.0), other]
      else
        self.fp_div(other)
      end
    end
    alias / poly_div
  end
end

class String
  def to_poly
    if self.scan(/^poly\([-+,.)\d\s]+/).null?
      poly()
    else
      eval(self)
    end
  end
end

class Array
  def to_poly
    poly(*self)
  end
end

class Vct
  def to_poly
    poly(*self.to_a)
  end
end

def Poly(obj)
  if obj.nil?
    obj = []
  end
  assert_type(obj.respond_to?(:to_poly), obj, 0,
              "an object containing method 'to_poly'")
  obj.to_poly
end

def make_poly(len, init = 0.0, &body)
  Poly.new(len, init, &body)
end

def poly?(obj)
  obj.instance_of?(Poly)
end

def poly(*vals)
  Poly.new(vals.length) do |i|
    if integer?(val = vals[i])
      Float(val)
    else
      val
    end
  end
end

def poly_reduce(obj)
  assert_type(obj.respond_to?(:to_poly), obj, 0,
              "an object containing method 'to_poly'")
  Poly(obj).reduce
end

def poly_add(obj1, obj2)
  if number?(obj1)
    assert_type(obj2.respond_to?(:to_poly), obj2, 1,
                "an object containing method 'to_poly'")
    Float(obj1) + Poly(obj2)
  else
    assert_type(obj1.respond_to?(:to_poly), obj1, 0,
                "an object containing method 'to_poly'")
    Poly(obj1) + obj2
  end
end

def poly_multiply(obj1, obj2)
  if number?(obj1)
    assert_type(obj2.respond_to?(:to_poly), obj2, 1,
                "an object containing method 'to_poly'")
    Float(obj1) * Poly(obj2)
  else
    assert_type(obj1.respond_to?(:to_poly), obj1, 0,
                "an object containing method 'to_poly'")
    Poly(obj1) * obj2
  end
end

def poly_div(obj1, obj2)
  if number?(obj1)
    assert_type(obj2.respond_to?(:to_poly), obj2, 1,
                "an object containing method 'to_poly'")
    Float(obj1) / Poly(obj2)
  else
    assert_type(obj1.respond_to?(:to_poly), obj1, 0,
                "an object containing method 'to_poly'")
    Poly(obj1) / obj2
  end
end

def poly_derivative(obj)
  assert_type(obj.respond_to?(:to_poly), obj, 0,
              "an object containing method 'to_poly'")
  Poly(obj).derivative
end

def poly_gcd(obj1, obj2)
  assert_type(obj.respond_to?(:to_poly), obj, 0,
              "an object containing method 'to_poly'")
  Poly(obj1).gcd(obj2)
end

def poly_roots(obj)
  assert_type(obj.respond_to?(:to_poly), obj, 0,
              "an object containing method 'to_poly'")
  Poly(obj).roots
end

# poly.rb ends here
