      PROGRAM bin2prm_cal

      INTEGER   input_unit, output_unit
      INTEGER   max_records, max_points, max_stacode
      PARAMETER (input_unit = 7)
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

      print *, 'bin2prm_cal converts the HYPOELLIPSE calibration'
      print *, 'file from binary to ASCII format.'
      print *, ' '

      WRITE (6,'(
     *"Name of binary calibration file to read (caldata.bin)?")')
      READ  (5,'(A)') old_filename

      OPEN (UNIT=input_unit, FILE=old_filename, STATUS='OLD',
     &      FORM='UNFORMATTED')

      WRITE (6,'(
     *"Name of ASCII calibration file to write (caldata.prm)?")')
      READ  (5,'(A)') new_filename

      OPEN (UNIT=output_unit, FILE=new_filename, STATUS='UNKNOWN')
C    &      FORM='NOPRINT')

      WRITE (6,'(" Reading calibration data...")')

      CALL get_bin_uacaldata (input_unit, max_records,
     &  max_points, nrecords, station_list, amp_source, begin_date,
     &  end_date, npoints, period_sysmag)

      WRITE (6,'(" Processing calibration data...")')

      CALL put_uacaldata (output_unit, max_records,
     &  max_points, nrecords, station_list, amp_source, begin_date,
     &  end_date, npoints, period_sysmag)

      END
