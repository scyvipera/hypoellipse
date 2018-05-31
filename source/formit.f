c formit.for    []
      subroutine formit(a, aout, fmit, n, ifix)
c        squeez a positive real number (a) into a field of
c        length n where 2<n<9, and maintain the maximum number of
c        significant digits.
c     input: n (integer) and a (real).
c            ifix = 0, never change input number
c            ifix .ne. 0, if number is out of range, set equal to
c            maximum or minimum number that can be printed.
c     output: best format (fmit) of form (fn.x) (a6)
c             aout is the number to be printed
c
      character*6 fmit
      character*1 int(10), ilem(4)
      data int/'0','1','2','3','4','5','6','7','8','9'/
c
      ilem(1) = 'f'
      ilem(2) = int(n+1)
      ilem(3) = '.'
      nm1 = n - 1
      nm2 = n - 2
      aout = a

      if(ifix .eq. 0) go to 20

      top = 10.**nm1 - 0.5
      bot = -(10.**nm2) + 0.5
      if(aout .ge. top) aout = top - .1
      if(aout .le. bot) aout = bot + .1

20    if(aout .eq. -0.) then
        i = n - 1
      else if(aout .eq. 0.) then
	i = n
      else if(aout .lt. 0.) then
        do 30 i = 1,nm2
          nm = nm2 - i
          ip1 = i + 1
          if(aout .le. -(10.**nm - 5./10.**ip1)) go to 50
30      continue
        i = nm1
      else
        do 40 i = 1,nm1
          nm = nm1 - i
          ip1 = i + 1
          if(aout .ge. (10.**nm - 5./10.**ip1)) go to 50
40      continue
        i = n
      endif
50    ilem(4) = int(i)

      write(fmit, 70) ilem
70    format('(', 4a1, ')')
      return
      end
c end formit
