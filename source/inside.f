c inside.for    []
      integer function inside(x0, y0, px, py, n)
c     check if point x0, y0 is inside polygon px(i), p(y), i = 1 to n
c     (n is the number of vertaces - eg. for a rectangle, n = 4)
c     based on godkin & pulli, bssa 74, p. 1845-1848, 1984.
c     converted to fortran from b. julian's c implementation
c     by j. c. lahr  -  18 may 1986
c     returns 0 if point outside polygon
c             1 if point at vertex
c           +-4 if point on an edge
c           +-2 if point inside
c
      logical vertex
      dimension px(n), py(n)
      inside = 0
      vertex = .false.
      x1 = px(n) - x0
      y1 = py(n) - y0
      do 20 i = 1, n
c       print *, '******************>> line segment ', i, vertex
        x2 = px(i) - x0
        y2 = py(i) - y0
        y1y2 = y1*y2
        if(y1y2 .le. 0.) then
c         print *, ' line crosses or at least touches x axis'
          ksi =  ksicr(x1, y1, x2, y2, y1y2, vertex)
c         print *, 'ksi = ', ksi
          if(iabs(ksi) .eq. 4) then
            inside = ksi
            return
          endif
          if(vertex) then
            inside = 1
            return
          endif
          inside = inside + ksi
        endif
        x1 = x2
        y1 = y2
20    continue
      return
      end
c end inside
