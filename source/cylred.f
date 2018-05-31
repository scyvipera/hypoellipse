c cylred.for    []
      subroutine cylred(cyldm, cylwt, ntrans, sumwt)
c reduce delay model list, eliminating duplicates and sorting by weight
      integer cyldm(ntrans)
c             cyldm(i)            delay model for # i
      real    cylwt(ntrans)
c             cylwt(i)            weight for # i
      integer key(50)
c             key                 key to original order of weights
      real    sumwt
c             sumwt               sum of weights of up to first 3
      integer tmpdm(50)
c             tmpdm               temp array of delay models
      real    tmpwt(50)
c             tmpwt               temp array of weights
      ntr = 0
      do 20 i = 1, ntrans
        if(cylwt(i) .gt. 0.0) then
          ntr = ntr + 1
          if(i .lt. ntrans) then
            do 18 j = i+1, ntrans
              if((cyldm(j) .eq. cyldm(i)) .and. (cylwt(j) .ne. 0.)) then
                cylwt(i) = cylwt(i) + cylwt(j)
                cylwt(j) = 0.0
              endif
18          continue
          endif
        endif
20    continue
      ntrans = ntr
c sort the weights
      call sort(cylwt, key, ntrans)
      do 30 i = 1, ntrans
        tmpdm(i) = cyldm(i)
30    continue
      do 40 i = 1, ntrans
        cyldm(ntrans + 1 - i) = tmpdm(key(i))
40    continue
c save weights in temporary arrays
      do 50 i = 1, ntrans
        tmpwt(i) = cylwt(i)
50    continue
c change to ascending order
      if (ntrans .gt. 3) ntrans = 3
      sumwt = 0.0
      do 60 i = 1, ntrans
        cylwt(i) = tmpwt(ntrans + 1 - i)
        sumwt = sumwt + cylwt(i)
60    continue
      return
      end
c end cylred
