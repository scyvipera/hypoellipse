c rshft.for    []
      character*(*) function rshft(array)
c
c  program to right-justify a character variable
c
c  array - a character variable
      character*(*) array
      character*132 temp
c
c  get length of array
c
      n = len(array)
      rshft = array
      if (n .eq. 0) return
c
c  find the position of the first non-blank character from the right
c
      do 10 i = 1, n
        j1 = n -i + 1
        if (array(j1:j1) .ne. ' ') go to 20
   10 continue
c
c  all characters are blank, so return
c
      return
c
c  shift characters to right
c
   20 if (j1 .eq. n) return
      temp(1:j1) = rshft(1:j1)
      rshft(n-j1+1:n) = temp(1:j1)
      rshft(1:n-j1) = ' '
      return
      end
c end rshft
