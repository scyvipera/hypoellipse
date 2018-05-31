C OPENFL.FOR
      SUBROUTINE OPENFL(IUNIT, IFILE, ISTAT, IZERO, ISHR, IFORM, IRECL)
C ON UNIX SYSTEMS, MUST USE CAPITAL F IN SUFFIX FOR PROPER COMPILATION.
C SYSTEM DEPENDENT PROGRAM TO OPEN FILES FOR HYPOELLLIPSE
      INTEGER IUNIT
C             IUNIT IS UNIT NUMBER FOR THIS FILE
      CHARACTER*(*) IFILE
C                   IFILE IS NAME OF FILE TO BE OPENED
      CHARACTER*(*) ISTAT
C                   ISTAT IS OPEN STATUS ('NEW' OR 'OLD' OR 'SCRATCH'
C                     OR 'UNKNOWN')
      CHARACTER*(*) IZERO
C                   IZERO MUST BE 'ZERO' OR 'NULL'; IF 'ZERO' THEN
C                     OPEN WITH "BLANK='ZERO'"
      CHARACTER*(*) ISHR
C                   ISHR INDICATES SHARE STATUS:
C                     MUST BE 'READONLY' OR 'NONE' ('SHARED' IS NOT CODED)
      CHARACTER*(*) IFORM
C                   IFORM = 'NOPRINT' FOR SOME FILES ON MASSCOMP
C                     UACAL FILE NEEDS 'UNFORMATTED'
C                     OTHERWISE = 'NONE'
      INTEGER*4     IRECL
C                   IRECL IS USED AS RECORD LENGTH ON VAX, UNLESS
C                     IT IS ZERO.
C
      IF (ISHR .NE. 'READONLY' .AND. ISHR .NE.
     &       'NONE') THEN
        PRINT *, 'OPENFL: Invalid argument for ISHR: ', ISHR
        STOP
      ENDIF
      IF (IZERO .NE. 'ZERO' .AND. IZERO .NE. 'NULL') THEN
        PRINT *, 'OPENFL: Invalid argument for IZERO: ', IZERO
        STOP
      ENDIF
      IF (ISTAT .NE. 'NEW' .AND. ISTAT .NE. 'OLD' .AND.
     &    ISTAT .NE. 'SCRATCH' .AND. ISTAT .NE. 'UNKNOWN') THEN
        PRINT *, 'OPENFL: Invalid argument for ISTAT: ', ISTAT
        STOP
      ENDIF
C VAX
C      IF (IRECL .LE. 0) THEN
C        IF (ISHR .EQ. 'READONLY') THEN
C          IF (IZERO .EQ. 'ZERO') THEN
C            OPEN(UNIT=IUNIT, FILE=IFILE, STATUS=ISTAT,
C     &      BLANK='ZERO', READONLY)
C          ELSE
C            OPEN(UNIT=IUNIT, FILE=IFILE, STATUS=ISTAT,
C     &      READONLY)
C          ENDIF
C        ELSE
C          IF (IZERO .EQ. 'ZERO') THEN
C            OPEN(UNIT=IUNIT, FILE=IFILE, STATUS=ISTAT,
C     &      BLANK='ZERO')
C          ELSE
C            OPEN(UNIT=IUNIT, FILE=IFILE, STATUS=ISTAT)
C          ENDIF
C        ENDIF
C      ELSE
C        IF (ISHR .EQ. 'READONLY') THEN
C          IF (IZERO .EQ. 'ZERO') THEN
C            OPEN(UNIT=IUNIT, FILE=IFILE, STATUS=ISTAT,
C     &      BLANK='ZERO', READONLY, RECL=IRECL)
C          ELSE
C            OPEN(UNIT=IUNIT, FILE=IFILE, STATUS=ISTAT,
C     &                    READONLY, RECL=IRECL)
C          ENDIF
C        ELSE
C          IF (IZERO .EQ. 'ZERO') THEN
C            OPEN(UNIT=IUNIT, FILE=IFILE, STATUS=ISTAT,
C     &      BLANK='ZERO', RECL=IRECL)
C          ELSE
C            OPEN(UNIT=IUNIT, FILE=IFILE, STATUS=ISTAT,
C     &      RECL=IRECL)
C          ENDIF
C        ENDIF
C      ENDIF
C VAX
C UNIX
      IF (IFORM .EQ. 'NONE' .OR. IFORM .EQ. 'NOPRINT') THEN
          IF (IZERO .EQ. 'ZERO') THEN
            OPEN(UNIT=IUNIT, FILE=IFILE, STATUS=ISTAT,
     &      BLANK='ZERO')
          ELSE
            OPEN(UNIT=IUNIT, FILE=IFILE, STATUS=ISTAT)
          ENDIF
      ELSE
          IF (IZERO .EQ. 'ZERO') THEN
            OPEN(UNIT=IUNIT, FILE=IFILE, STATUS=ISTAT,
     &      BLANK='ZERO', FORM=IFORM)
          ELSE
            OPEN(UNIT=IUNIT, FILE=IFILE, STATUS=ISTAT,
     &      FORM=IFORM)
          ENDIF
      ENDIF
C     write (0,'(" Trying to open ",A," with status = ",A)')
C    &  ifile, istat
C UNIX
C PC
C     IF (ISTAT .EQ. 'SCRATCH') ISTAT = 'NEW'
C     IF (IRECL .LE. 0) THEN
C         IF (IZERO .EQ. 'ZERO') THEN
C           OPEN(UNIT=IUNIT, FILE=IFILE, STATUS=ISTAT,
C    &      BLANK='ZERO')
C         ELSE
C           OPEN(UNIT=IUNIT, FILE=IFILE, STATUS=ISTAT)
C         ENDIF
C     ELSE
C         IF (IZERO .EQ. 'ZERO') THEN
C           OPEN(UNIT=IUNIT, FILE=IFILE, STATUS=ISTAT,
C    &      BLANK='ZERO', RECL=IRECL)
C         ELSE
C           OPEN(UNIT=IUNIT, FILE=IFILE, STATUS=ISTAT,
C    &      RECL=IRECL)
C         ENDIF
C     ENDIF
C PC
      RETURN
      END
C END OPENFL
