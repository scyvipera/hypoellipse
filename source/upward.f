c upward.for    []
       subroutine upward(alrms, altla, altlo, altz,
     * frms, rmslim, zup, axz)
      logical freor
      common /hopq/ savla,savlo,savez,savor,freor
      common /phoqn/ inst,knst
      real lonep, latep
      common /qmost1/ lonep,ni,latep
      common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      dimension zinc(8)
      data zinc/20, 20, 20, 20, 20, 20, 20, 20/
c set initial steps according to erz estimate
      zbas = axz
      if(zbas .lt. .2) zbas = .2
      if(zbas .gt. 20.) zbas = 20.
      zinc(1) = zbas
      zinc(2) = zbas*2.
      inst = 1
c find upward shift in z with respect to altz that results in rms = rmslim
        if(frms .le. rmslim) then
c the rms at the surface is within error limits
          zup = altz
        else if(altz .le. 5.) then
c depth is near surface and
c the rms at the surface is higher than error limit, so interpolate.
          zup = altz*(rmslim - alrms)/(frms - alrms)
        else
          savla = altla
          savlo = altlo
          if( (altz - zinc(1)) .lt. 0.0 ) zinc(1) = 0.7*altz
          savez = altz - zinc(1)
          alrmsl = alrms
          altzl = altz
          n = 2
20        call quakes
          if(rms .ge. rmslim) then
c interpolate to get zup
            zup = altz - altzl + (altzl - savez)*(rmslim - alrmsl) /
     *            (rms - alrmsl)
          else
            if(savez .gt. zinc(n)) then
              altzl = savez
              savez = savez - zinc(n)
              n = n + 1
              if(altz - savez .ge. 110.) then
                zup = 99.
                return
              endif
              alrmsl = rms
              savla = latep
              savlo = lonep
              goto 20
            else
c interpolate to get zup based on surface rms
              zup = altz - savez + savez*(rmslim - rms) /
     *        (frms - rms)
            endif
          endif
        endif
      return
      end
c end upward
