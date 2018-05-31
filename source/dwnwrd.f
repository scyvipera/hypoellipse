c dwnwrd.for    []
      subroutine dwnwrd(alrms, altla, altlo, altz,
     * rmslim, zdn, axz)
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /phoqn/ inst,knst
      real lonep, latep
      common /qmost1/ lonep,ni,latep
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      dimension zinc(8)
      data zinc/20, 20, 20, 20, 20, 20, 20, 20/
      inst = 1
c set initial steps according to erz estimate
      zbas = axz
      if(zbas .lt. .2) zbas = .2
      if(zbas .gt. 20.) zbas = 20.
      zinc(1) = zbas
      zinc(2) = zbas*2.
c find downward shift in z with respect to altz that results in rms = rmslim
      savla = altla
      savlo = altlo
      savez = altz + zinc(1)
      alrmsl = alrms
      altzl = altz
      n = 1
38    call quakes
      if(rms .ge. rmslim) then
        zdn = (savez - altzl)*(rmslim - alrmsl) /
     *          (rms - alrmsl) + altzl - altz
      else
        n = n + 1
        altzl = savez
        savez = savez + zinc(n)
        if(savez - altz .gt. 110.) then
          zdn = 99.
          return
        endif
        alrmsl = rms
        savla = latep
        savlo = lonep
        goto 38
      endif
      return
      end
c end dwnwrd
