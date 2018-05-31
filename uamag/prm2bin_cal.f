      PROGRAM prm2bin_cal

      INTEGER   output_unit
      INTEGER   max_records, max_points, max_stacode
      PARAMETER (output_unit = 11)
      PARAMETER (max_records = 1000)
      PARAMETER (max_points = 30)
      PARAMETER (max_stacode = 4)

      CHARACTER old_filename*123, new_filename*123
      CHARACTER station_list(max_records)*(max_stacode)
      CHARACTER amp_source(max_records)*1
      INTEGER   nrecords, npoints(max_records)
      INTEGER   begin_date(max_records), end_date(max_records)
      REAL      period_sysmag(2,max_points, max_records)

      print *, 'prm2bin_cal converts the hypoellipse calibration'
      print *, 'file from ASCII to binary format.'
      print *, ' '

      WRITE (6,'(
     *"Name of ASCII calibration filename to read (caldata.prm)? ")')
      READ  (5,'(A)') old_filename

      WRITE (6,'(
     *"Name of binary calibration filename to create (caldata.bin)? ")')
      READ  (5,'(A)') new_filename

      OPEN (UNIT=output_unit, FILE=new_filename, STATUS='UNKNOWN',
     &      FORM='UNFORMATTED')

      WRITE (6,'(" Reading calibration data...")')
      CALL get_uacaldata (old_filename, max_records,
     &  max_points, nrecords, station_list, amp_source, begin_date,
     &  end_date, npoints, period_sysmag)

      CALL put_bin_uacaldata (output_unit, max_records,
     &  max_points, nrecords, station_list, amp_source, begin_date,
     &  end_date, npoints, period_sysmag)

      END
