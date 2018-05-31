c prplot.for    []
      subroutine prplot(x, y, xmax, xmin, ymax, ymin, lines, last,
     *      isym, no, most)
c     written by w. gawthrop
c     plot takes a set of x and y values and plots them on the printer
c     arguments
c           x     the array of x coordinates
c           y     the array of y coordinates
c           xmax  the largest value of x
c           xmin  the smallest value of x
c           ymax  the largest value of y
c           ymin  the smallest value of y
c           lines the number of lines for the y axis
c          igraph the matrix used for storage of the entire graph
c                 before printing
c           last  the number of data points
c           isym  the symbol to be used as a point on the graph
c           no,most     this is curve number no of the maximum number
c                       most to be printed on these axes
c
      character*4 isym(last)
      character*1 igraph(111, 56)
      dimension x(last), y(last), zx(12)
      integer punt
      common /punt/ punt
      yl=ymax
      ys=ymin
      xs=xmin
      xl=xmax
      xscale=(xl-xs)/110.
      a=lines-1
      yscale=(yl-ys)/a
      if (no-1)30,10,30
   10 continue
      do 20 i=1,111
      do 20 j=1,lines
   20 igraph(i,j)= ' '
      do 29 i=1,lines
      igraph(1,i)='i'
   29 igraph(111,i)='i'
c------- form s-p for plotting
   30 do 40 i=1,last
c     y(i)=y(i)-x(i)
      xx=x(i)
      yy=y(i)
  303 if (xl-xx) 40,31,31
   31 if (xx-xs)40,32,32
   32 if (yl-yy) 40,33,33
   33 if (yy-ys) 40,34,34
   34 ix=(xx-xs)/xscale+1.5
      iy=(yy-ys)/yscale+.5
      iy=lines-iy
      if (isym(i) .eq. ' ') goto 40
      if (ix.lt.0.or.ix.gt.111) goto 38
      if (iy.lt.0.or.iy.gt.111) goto 38
      igraph(ix,iy)=isym(i)(1:1)
      if (igraph(ix,iy) .eq. ' ') igraph(ix,iy) = '+'
      igraph(ix+1,iy)=isym(i)(2:2)
      igraph(ix+2,iy)=isym(i)(3:3)
      igraph(ix+3,iy)=isym(i)(4:4)
      goto 40
   38 write(punt,39) isym(i)
   39 format(1x,a4,' plots off of graph')
   40 continue
      if (no-most) 50,51,50
   50 return
   51 do 52 k=1,11
   52 zx(k)=10.*      (k-1)*xscale+xs
      zx(12)=zx(11)+10.*xscale
c     write(punt,1)
      write(punt,2)
      yes=yl+yscale
      do 60 i=1,lines
      yes=yes-yscale
   60 write(punt,4) yes,(igraph(j,i),j=1,111 )
      write(punt,6)
      write(punt,7) (zx(k),k=1,12)
    1 format(1h1)
    2 format(1h1, ' ts ',2x,23('i    '))
    4 format(1h ,f6.2,111a1)
    6 format(1h ,6x,22('i....'),'i')
    7 format(1h ,' tp',f6.2,11f10.2)
   80 return
      end
c end prplot
