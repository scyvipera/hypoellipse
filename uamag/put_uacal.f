      SUBROUTINE put_uacaldata (output_unit, max_records,
     &  max_points, nrecords, station_list, amp_source, begin_date,
     &  end_date, npoints, period_sysmag)

      INTEGER   i, j, lentru
      INTEGER   output_unit, max_records, max_points, nrecords
      CHARACTER station_list(max_records)*(*)
      CHARACTER amp_source(max_records)*1
      INTEGER   begin_date(*), end_date(*), npoints(*)
      REAL      period_sysmag(2,max_points,max_records)

      CHARACTER old_record*1024, new_record*1024

c     WRITE (6,'(" Read",I4," records")') nrecords
      DO i=1, nrecords

c       WRITE (6,'(" Processing record for ",A)') station_list(i)
        WRITE (old_record,10) station_list(i), amp_source(i),
     &    begin_date(i), end_date(i), npoints(i),
     &    (period_sysmag(1,j,i), period_sysmag(2,j,i), j=1, npoints(i))
10      FORMAT (A4,1X,A1,1X,2(I6,X),I3,30(X,F10.3,X,F12.2))
c       WRITE (6,'("   Formatted")')

        j = lentru(old_record) + 1
        old_record(j:j) = '\0'
        CALL squish_uacal (old_record, new_record)
c       WRITE (6,'("   Squished")')

        WRITE (output_unit, '(A)') new_record(1:lentru(new_record))
c       WRITE (6,'("   Written")')

      END DO

      RETURN
      END

