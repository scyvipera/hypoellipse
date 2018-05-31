c dnstrg.for    []
      character*(*) function dnstrg(array)
c
c  program to change string to uppercase
c
c  array - a character variable
      character*(*) array
      integer offset
      data offset/32/
c
c  get length of array
c
      dnstrg = ' '
      lenstr = len(array)
      if (lenstr .eq. 0) return
      do 10 i = 1, lenstr
        ic = ichar(array(i:i))
        if ((ic .ge. 65) .and. (ic .le. 90)) then
          dnstrg(i:i) = char(ic + offset)
        else
          dnstrg(i:i) = array(i:i)
        endif
   10 continue
      return
      end
c end dnstrg
