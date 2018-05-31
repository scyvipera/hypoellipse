c sort.for    []
      subroutine sort(x,key,no)
c w.h.k. lee sort
      dimension x(*),key(*)
      if (no .le. 0) return
      do 1 i=1,no
 1       key(i)=i
      mo=no
 2    if (mo-15) 21,21,23
 21   if (mo-1) 29,29,22
 22   mo=2*(mo/4)+1
      goto 24
 23   mo=2*(mo/8)+1
 24   ko=no-mo
      jo=1
 25   i=jo
 26   if (x(i)-x(i+mo)) 28,28,27
 27   temp=x(i)
      x(i)=x(i+mo)
      x(i+mo)=temp
      kemp=key(i)
      key(i)=key(i+mo)
      key(i+mo)=kemp
      i=i-mo
      if (i-1) 28,26,26
 28   jo=jo+1
      if (jo-ko) 25,25,2
 29   return
      end
c end sort
