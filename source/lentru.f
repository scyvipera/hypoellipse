c lentru.for    []
      integer function lentru(alph)
c     finds the true length of a character variable
      character alph*(*)
      l = len(alph)
      do 100 i = l, 1, -1
       if(alph(i:i).ne.' ' .and. alph(i:i).ne.'\0') then
         lentru = i
         return
        endif
100   continue
      lentru = 0
      return
      end
c end lentru
