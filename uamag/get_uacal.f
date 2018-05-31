      SUBROUTINE get_uacaldata (filename, max_records, max_points,
     &             nrecords, station_list, amp_source, begin_date,
     &             end_date, npoints, period_sysmag)
C PC
C$NOTRUNCATE
C PC
C --- Input for this routine is a free formatted ASCII file of
C --- station calibration data.  It is a sequence of up to
C --- <max_records> (logical) records (which may span several
C --- physical records) ordered in the following manner:
C --- 'sta_code', 'source', begin_yymmdd, end_yymmdd, npairs,
C --- period(1), sysmag(1), period(2), sysmag(2), ...
C --- period(npairs), sysmag(npairs)
C --- NOTE: THE PERIOD, MAGNIFICATION PAIRS MUST BE IN ORDER OF
C --- INCREASING PERIOD (i.e. this is reversed from the old routine).
C     IMPLICIT UNDEFINED (a-z)
      INTEGER data_unit
      PARAMETER (data_unit = 24)
      CHARACTER*300 CALREC
      CHARACTER RSHFT*4, UPSTRG*4
      INTEGER max_records, max_points, nrecords
      CHARACTER filename*(*), station_list(max_records)*(*)
      CHARACTER amp_source(max_records)*1
      INTEGER  begin_date(*), end_date(*), npoints(*)
      REAL period_sysmag(2,max_points,max_records)
      INTEGER i
C     INTEGER STATUS
C     CALL OPENFL(    IUNIT,    IFILE, ISTAT,  IZERO, ISHR, 
C    *    IFORM, IRECL)
      CALL OPENFL(data_unit, filename, 'OLD', 'NULL', 'NONE',
     *    'NONE', 0)
C     OPEN (UNIT=data_unit, FILE=filename, STATUS='OLD',
C    &      IOSTAT=status)
C     IF (status.NE.0) THEN
C       WRITE (*, '('' Hypoellipse stopped: Failed to open UofA'',
C    &         '' calibration file:'', /, X, A)') filename
C       STOP
C     ENDIF
      nrecords = 0
100   IF (nrecords.LT.max_records) THEN
        nrecords = nrecords + 1
        READ (data_unit, '(A)', END=116) CALREC
C BECAUSE STANDARD FORTRAN 77 TO CAN NOT READ AN INTERNAL FILE WITH
C FREE FORMAT, FILE 14 MUST BE USED IN THE FOLLOWING CODE!
        REWIND 14
        WRITE(14, '(A)') CALREC
        station_list(nrecords) = CALREC(1:4)
        REWIND 14
        IF(CALREC(4:4) .EQ. ' ') THEN
          amp_source(nrecords) = UPSTRG(CALREC(5:5))
          WRITE(14, '(A)') CALREC(6:LENTRU(CALREC))
        ELSE
          amp_source(nrecords) = UPSTRG(CALREC(6:6))
          WRITE(14, '(A)') CALREC(7:LENTRU(CALREC))
        ENDIF
        REWIND 14
        READ (14, *, ERR=118)
     &    begin_date(nrecords), end_date(nrecords), npoints(nrecords),
     &    (period_sysmag(1,i,nrecords), period_sysmag(2,i,nrecords),
     &    i=1, npoints(nrecords))
C       READ (data_unit, *, IOSTAT=status)
C    &    station_list(nrecords), amp_source(nrecords),
C    &    begin_date(nrecords), end_date(nrecords), npoints(nrecords),
C    &    (period_sysmag(1,i,nrecords), period_sysmag(2,i,nrecords),
C    &     i=1, npoints(nrecords))
C ---   This doesn't solve the overflow problem, but it might help.
        IF (npoints(nrecords).GT.max_points) THEN
          npoints(nrecords) = max_points
          WRITE (*, '('' Warning: Too many data points on record #'',
     &      I6, '' of UofA calibration file:'', /, A)')
     &      nrecords, filename
        ENDIF
C ---   Right justify the 3-character station codes and shift to uppercase.
        station_list(nrecords) = UPSTRG(RSHFT(station_list(nrecords)))
C ---   Read next record
        GOTO 100			
116     CONTINUE
          nrecords = nrecords - 1	
          RETURN
118     CONTINUE
          WRITE (06, 120) nrecords, filename
120       format(' Warning: Error reading record #', I5,
     &           ' from UofA calibration file:', /, 1X, A, /,
     &           ' Remainder of file skipped.')
          RETURN
      ELSE
        WRITE (06, 130) MX_REC, filename
130     format(' Warning: Too many records (>',I5,
     &    ') in UofA calibration file:', /, 1X, A)
      ENDIF
      RETURN
      END
