c getbin.f    []
      subroutine getbin (input_unit, max_records,
     &  max_points, nrecords, station_list, amp_source, begin_date,
     &  end_date, npoints, period_sysmag)

      integer   i, j
      integer   input_unit, max_records, max_points, nrecords
      character station_list(max_records)*(*)
      character amp_source(max_records)*1
      integer   begin_date(*), end_date(*), npoints(*)
      real      period_sysmag(2,max_points,max_records)

      i = 1

100   read (input_unit,end=999) station_list(i), amp_source(i),
     &  begin_date(i), end_date(i), npoints(i),
     &  (period_sysmag(1,j,i), period_sysmag(2,j,i), j=1, npoints(i))
      i = i + 1
      goto 100

999   nrecords = i - 1
      return
      end
c end getbin
