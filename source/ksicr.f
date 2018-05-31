c ksicr.for    []
      integer function ksicr(x1, y1, x2, y2, y1y2, vertex)
c     signed crossing number - converted to fortran by j. c. lahr from
c       b. julian's c code.    18 may 1986
c
c       0                          :  does not cross -x axis
c       0 with vertex set to true  :  one end at point
c       +/-4                       :  goes through point
c       +/-2                       :  crosses -x axis
c       +/-1                       :  half crosses -x axis
c     [test for y1*y2 .le. 0. is performed in calling routine.]
c     [otherwise y1*y2 .gt. 0. would be placed here and return zero.]
      logical vertex
      t = x1*y2 - x2*y1
c     print *, 't = ', t
      if( (t .eq. 0.) .and. (x1*x2 .le. 0.) ) then
c       print *, 'line passes through or to point'
        if(y1y2 .ne. 0.) then
c         print *, 'neither end of line terminates at point'
          if(y2 .gt. 0.) then
c           print *, 'line passes up through point'
            ksicr = 4
          else
c           print *, 'line passes down through point'
            ksicr = -4
          endif
          return
        else if(x1*x2 .eq. 0.) then
c         print *, 'y1*y2 = 0. and x1*x2 = 0.'
          vertex = .true.
c         print *, 'vertex = ', vertex
        else
          if(x1 .lt. x2) then
c           print *, 'line passes to right through point'
            ksicr = 4
          else
c           print *, 'line passes to left through point'
            ksicr = -4
          endif
          return
        endif
      else if(y1y2 .lt. 0.) then
c       print *, 'complete crossing of x axis'
        if(t*y2 .lt. 0) then
c         print *, 'complete crossng of -x asix'
          if(y2 .gt. 0.) then
            ksicr = 2
          else
            ksicr = -2
          endif
          return
        endif
      else if(y1 .eq. 0.) then
c       print *, 'half crossing, y1 equals 0'
        if((x1 .lt. 0) .and. (y2 .ne. 0.) ) then
          if(y2 .gt. 0.) then
            ksicr = 1
          else
            ksicr = -1
          endif
          return
        endif
      else
c       print *, 'half crossing, y2 must equal 0'
        if(x2 .lt. 0) then
          if(y1 .lt. 0) then
            ksicr = 1
          else
            ksicr = -1
          endif
          return
        endif
      endif
      ksicr = 0
      return
      end
c end ksicr
