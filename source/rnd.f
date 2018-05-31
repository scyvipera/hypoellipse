c rnd.for    []
      function rnd()
c     acm algorithm 267 by m.c.pike
c     ran3 is a random number generator from the book numerical recipes by
c     press and others that gives a random distribution over the
c     interval 0 to 1.
c     this function computes a normal distribution with mean zero and
c     standard deviation 1.0.
      f = ran3(0)
      x1 = sqrt(-2.*alog(f))
      t = 6.2831853072*ran3(0)
      rnd = x1*sin(t)
      return
      end
c end rnd
