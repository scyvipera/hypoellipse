c openfl.for    [unix]
      subroutine openfl(iunit, ifile, istat, izero, ishr, iform, irecl)
c system dependent program to open files for hypoelllipse
      integer iunit
c             iunit is unit number for this file
      character*(*) ifile
c                   ifile is name of file to be opened
      character*(*) istat
c                   istat is open status ('new' or 'old' or 'scratch'
c                     or 'unknown')
      character*(*) izero
c                   izero must be 'zero' or 'null'; if 'zero' then
c                     open with "blank='zero'"
      character*(*) ishr
c                   ishr indicates share status:
c                     must be 'readonly' or 'none' ('shared' is not coded)
      character*(*) iform
c                   iform = 'noprint' for some files on masscomp
c                     uacal file needs 'unformatted'
c                     otherwise = 'none'
      integer*4     irecl
c                   irecl is used as record length on vax, unless
c                     it is zero.
c
      if (ishr .ne. 'readonly' .and. ishr .ne.
     &       'none') then
        print *, 'openfl: invalid argument for ishr: ', ishr
	print *, 'iunit = ', iunit
	print *, 'ifile = ', ifile
	print *, 'istat = ', istat
	print *, 'izero = ', izero
	print *, 'ishr  = ', ishr
	print *, 'irecl = ', irecl

        stop
      endif
      if (izero .ne. 'zero' .and. izero .ne. 'null') then
        print *, 'openfl: invalid argument for izero: ', izero
        stop
      endif
      if (istat .ne. 'new' .and. istat .ne. 'old' .and.
     &    istat .ne. 'scratch' .and. istat .ne. 'unknown') then
        print *, 'openfl: invalid argument for istat: ', istat
        stop
      endif
c* (vax
c      if (irecl .le. 0) then
c        if (ishr .eq. 'readonly') then
c          if (izero .eq. 'zero') then
c            open(unit=iunit, file=ifile, status=istat,
c     &      blank='zero', readonly)
c          else
c            open(unit=iunit, file=ifile, status=istat,
c     &      readonly)
c          endif
c        else
c          if (izero .eq. 'zero') then
c            open(unit=iunit, file=ifile, status=istat,
c     &      blank='zero')
c          else
c            open(unit=iunit, file=ifile, status=istat)
c          endif
c        endif
c      else
c        if (ishr .eq. 'readonly') then
c          if (izero .eq. 'zero') then
c            open(unit=iunit, file=ifile, status=istat,
c     &      blank='zero', readonly, recl=irecl)
c          else
c            open(unit=iunit, file=ifile, status=istat,
c     &                    readonly, recl=irecl)
c          endif
c        else
c          if (izero .eq. 'zero') then
c            open(unit=iunit, file=ifile, status=istat,
c     &      blank='zero', recl=irecl)
c          else
c            open(unit=iunit, file=ifile, status=istat,
c     &      recl=irecl)
c          endif
c        endif
c      endif
c* vax)
c* (unix
c#if masscomp
c      if (iform .eq. 'none') then
c#else
      if (iform .eq. 'none' .or. iform .eq. 'noprint') then
c#endif
          if (izero .eq. 'zero') then
            open(unit=iunit, file=ifile, status=istat,
     &      blank='zero')
          else
            open(unit=iunit, file=ifile, status=istat)
          endif
      else
          if (izero .eq. 'zero') then
            open(unit=iunit, file=ifile, status=istat,
     &      blank='zero', form=iform)
          else
            open(unit=iunit, file=ifile, status=istat,
     &      form=iform)
          endif
      endif
c     write (0,'(" trying to open ",a," with status = ",a)')
c    &  ifile, istat
c* unix)
c* (pc
c      if (istat .eq. 'scratch') istat = 'new'
c      if (irecl .le. 0) then
c          if (izero .eq. 'zero') then
c            open(unit=iunit, file=ifile, status=istat,
c     &      blank='zero')
c          else
c            open(unit=iunit, file=ifile, status=istat)
c          endif
c      else
c          if (izero .eq. 'zero') then
c            open(unit=iunit, file=ifile, status=istat,
c     &      blank='zero', recl=irecl)
c          else
c            open(unit=iunit, file=ifile, status=istat,
c     &      recl=irecl)
c          endif
c      endif
c* pc)
      return
      end
c end openfl
