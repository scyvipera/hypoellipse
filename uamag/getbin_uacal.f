      SUBROUTINE get_bin_uacaldata (input_unit, max_records,
     &  max_points, nrecords, station_list, amp_source, begin_date,
     &  end_date, npoints, period_sysmag)

      INTEGER   i, j
      INTEGER   input_unit, max_records, max_points, nrecords
      CHARACTER station_list(max_records)*(*)
      CHARACTER amp_source(max_records)*1
      INTEGER   begin_date(*), end_date(*), npoints(*)
      REAL      period_sysmag(2,max_points,max_records)

      i = 1

100   READ (input_unit,END=999) station_list(i), amp_source(i),
     &  begin_date(i), end_date(i), npoints(i),
     &  (period_sysmag(1,j,i), period_sysmag(2,j,i), j=1, npoints(i))
c     WRITE (6,'(" Read record for ",A)') station_list(i)
      i = i + 1
      GOTO 100

999   nrecords = i - 1
      RETURN
      END
