c qdist.for    []
      subroutine qdist(az, deldeg, delta, duse, idclc, kdx, keyd,
     * lat, lon, latep, lonep, ldx, near,
     * ndwt, nprwt, nrp, nrwt, wt)
c if idclc is true, compute distance and azimuth.  in any case sort by
c distance and set weights equal to reading weights.
      include 'params.inc' 
      parameter (pi = 3.14159265)
      parameter (twopi = 2.0*pi)
      parameter (halfpi = 0.5*pi)
      parameter (rad = pi/180.)
      parameter (deg = 1.0/rad)
c     real     az(npa)     azimuth
      real     az(npa)
c     real     deldeg      unused delaz argument
      real     deldeg
c     real     delta(npa)  distance (km)
      real     delta(npa)
c     real     dall(npa)   temporary distance array (all stations)
      real     dall(npa)
c     real     duse(npa)   distance array (stations used)
      real     duse(npa)
c     logical  idclc       if true, the compute new distances, otherwise
c                          just initialize weights and sort by distance.
      logical  idclc
c     integer  kdx(npa)    kdx(phase number) = station number.
      integer  kdx(npa)
c     integer  key(npa)    sort key for used stations
      integer  key(npa)
c     integer  keyd(npa)   sort key for all stations
      integer  keyd(npa)
c     integer  keyuse(npa) key to original phase key
      integer  keyuse(npa)
c     real     lat(nsn)    station latitude
      real     lat(nsn)
c     real     lon(nsn)    station longitude
      real     lon(nsn)
c     real     latep       eq latitude
      real     latep
c     real     lonep       eq longitude
      real     lonep
c     integer  ldx(npa)    0 if no s reading, else array index for s
      integer  ldx(npa)
c     integer  near        key to closest station used
      integer  near
c     integer  ndwt        # stations with p or s used
      integer  ndwt
c     integer  nprwt       # stations with p or s last iteration
      integer  nprwt
c     integer  nrp         # of p and s-p phases
      integer  nrp
c     integer  nrwt        # of phases with weight
      integer  nrwt
c     real     wt(npa)     phase weight
      real     wt(npa)
      parameter (ln=3)
c
      nprwt = nrwt
      ndwt = 0
c initialize distance calculations for current epicenter
c calculate epicentral distances and initialize weights
      do 121 i = 1,nrp
        ji = kdx(i)
c   given station lat & lon (radians) get delta (km) & azimuth (deg)
        if (idclc) then
          call delaz(latep,lonep,delta(i),deldeg,az(i),lat(ji),lon(ji))
        endif
        dall(i) = delta(i)
        if (ldx(i) .gt. 0) then
          k = ldx(i)
          delta(k) = delta(i)
          az(k) = az(i)
        endif
        if(wt(i) .ne. 0.0) then
          ndwt = ndwt + 1
          duse(ndwt) = delta(i)
          keyuse(ndwt) = i
        else if(ldx(i) .gt. 0) then
          if(wt(k) .ne. 0.0) then
            ndwt = ndwt + 1
            duse(ndwt) = delta(i)
            keyuse(ndwt) = i
          endif
        endif
121   continue
c
c sort all stations by distance
      call sort(dall, keyd, nrp)
      near = 1
      if(ndwt .eq. 0) then
        ndwt = 1
        return
      end if
      call sort(duse, key, ndwt)
      near = keyuse(key(1))
      return
      end
c end qdist
