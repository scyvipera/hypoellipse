      SUBROUTINE put_bin_uacaldata (output_unit, max_records,
     &  max_points, nrecords, station_list, amp_source, begin_date,
     &  end_date, npoints, period_sysmag)

      INTEGER   i, j
      INTEGER   output_unit, max_records, max_points, nrecords
      CHARACTER station_list(max_records)*(*)
      CHARACTER amp_source(max_records)*1
      INTEGER   begin_date(*), end_date(*), npoints(*)
      REAL      period_sysmag(2,max_points,max_records)

      DO i=1, nrecords

        WRITE (6,'(" Writing record for ",A)') station_list(i)
        WRITE (output_unit) station_list(i), amp_source(i),
     &    begin_date(i), end_date(i), npoints(i),
     &    (period_sysmag(1,j,i), period_sysmag(2,j,i), j=1, npoints(i))

      END DO

      RETURN
      END
