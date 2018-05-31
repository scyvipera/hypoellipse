c opfls.for    [unix]
c ******************************************************************
c ***          subroutine opfls of program hypoellipse          ***
c ******************************************************************
c ***   a rewritten version of the opfls subroutine from usgs.   ***
c ***  this version works satisfactorily when run interactively  ***
c ***    or with its input "redirected" from a file or shell     ***
c ***               script.  ghcs at uaf, 5/17/88.               ***
c ******************************************************************
      subroutine opfls (iplt, inmain, root, nttab)
c* (pc
c$notruncate
c* pc)
c
c     for the masscomp compiler, open statements must have form='noprint'
c     for all files to be written, except the print and log files.
c --- the iplt and inmain arguments will be set to return the
c --- filecode from which the main input file should be read.
c --- that is, either from a separate file (unit 8), or the
c --- standard input stream (unit 5) which may be a redirected
c --- c-shell "here document" containing shell variables.
c --- the ilis argument will be set to return the initial, or
c --- default value of ilis, a new option variable controlling
c --- the verboseness of the "printer" output file (unit 9).
c --- it provides a mechanism for generating an output file
c --- which contains only the information vital to evaluating
c --- the "correctness" of a phaselist and location.
c --- 7/29/89 note, jcl:
c      ilis is now set by the constants print option (see 2.2.3.20 in
c      the hypoellipse manual.
      integer iplt, inmain
c --- lengths of filename and type strings and the input
c --- and output unit numbers for this routine.
      integer max_length, max_type, in, out
      parameter (max_length = 255)
      parameter (max_type = 4)
      parameter (in = 5)
      parameter (out = 6)
c --- logical unit numbers for the files to be opened.
      integer stdin_unit, main_unit, stderr_unit, punt
      integer print_unit, summary_unit, arch_unit, tb11_unit
      integer tb12_unit, tb13_unit, stacorr_unit, log_unit
      integer tmp1_unit, tmp2_unit, tmpsum_unit, tmparc_unit
      integer tmpmess_unit
c* (vax
c* (pc
c      parameter (stderr_unit  = 6)
c* pc)
c* vax)
c* (unix
      parameter (stderr_unit  = 0)
c* unix)
      parameter (stdin_unit   = 5)
      parameter (main_unit    = 8)
      parameter (print_unit   = 9)
      parameter (summary_unit = 4)
      parameter (arch_unit    = 11)
      parameter (tb11_unit    = 21)
      parameter (tb12_unit    = 22)
      parameter (tb13_unit    = 23)
      parameter (stacorr_unit = 13)
      parameter (log_unit     = 6)
      parameter (tmp1_unit    = 2)
      parameter (tmp2_unit    = 3)
      parameter (tmpsum_unit  = 14)
      parameter (tmparc_unit  = 15)
      parameter (tmpmess_unit = 16)
c --- default input file and root/base strings.
      character*(max_length) in_def, root_def
      parameter (in_def = 'stdin')
      parameter (root_def = 'hypoe')
c --- the default file types (extensions) for output files.
      character*(max_type) print_type, summary_type, arch_type
      character*(max_type) stacorr_type,log_type, tmp1_type
      character*(max_type) tmp2_type, tmpsum_type, tmparc_type
      character*(max_type) tmpmess_type
      parameter (print_type   = '.out')
      parameter (summary_type = '.sum')
      parameter (arch_type    = '.arc')
      parameter (stacorr_type = '.nst')
      parameter (log_type     = '.log')
      parameter (tmp1_type    = '.1st')
      parameter (tmp2_type    = '.2st')
      parameter (tmpsum_type  = '.3sc')
      parameter (tmparc_type  = '.4sc')
      parameter (tmpmess_type = '.5sc')
c --- the lentru function returns the number of
c --- non-blank characters in its string argument.
c --- it returns zero if the string is completely blank.
      integer lentru
c --- local variable declarations.
      character*(*) root
      character*(max_length) fname, default, log_file
      character*(max_length) dnstrg
      character answer*1, openstat*7
      integer status, length
      integer length_def, length_root, length_log
      common /punt/ punt
      default = in_def
      length_def = lentru(default)
      write(out, '(a)') ' begin hypoellipse'
      write (out,
     *  '(/'' hypoellipse input filename ('',a,'')? '',$)')
     *  default(1:length_def)
      read (in, '(a)', iostat=status) fname
      length = lentru(fname)
      if ((status.ne.0) .or. (length.eq.0)) then
        fname = default
        length = length_def
      endif
      write (out, '(1x,a)') fname(1:length)
      if (dnstrg(fname(1:length)).eq.'stdin') then
        inmain = stdin_unit
        iplt = stdin_unit
      else
        inmain = main_unit
        iplt = main_unit
c       call openfl(iunit, ifile, istat,  izero,   ishr,
c    *          iform, irecl)
        call openfl( iplt, fname, 'old', 'zero', 'readonly',
     *          'none', 0)
c       open (unit=iplt, file=fname(1:length), status='old',
c    *  blank='zero')
c ---   previous version used blank='zero' which is very troublesome
c ---   (e.g. integers must be right justified in their fields
c ---   which is not obvious when looking at an input file).
c ---   however, some summary records have imbedded blanks with the
c ---   yrmodyhrmn field, and will not be read correctly unless
c ---   blank = 'zero' is specified!
c ---   also, iostat=status gives less information than the operating
c ---   system when the open fails.
c       if (status.ne.0) then
c         write (out,
c    *      '('' hypoellipse failed to open input file "'',a,''"'')')
c    *      fname(1:length)
c         stop 'abort from module opfls.'
c       endif
      endif
c --- should the output files be opened with a status of
c --- "new" to prohibit overwriting existing ones of the
c --- same name, or "unknown" to allow overwriting?
      openstat = 'new'
c* (unix
      write (out,
     * '('' allow overwriting of existing output files (no)? '',$)')
      read (in, '(a)', iostat=status) answer
      answer = dnstrg( answer )
      if ((status.ne.0) .or. (answer.eq.' ')) answer = 'n'
      if (answer.eq.'y') then
        answer = 'y'
        openstat = 'unknown'
      else
        answer = 'n'
        openstat = 'new'
      endif
      write (out, '(1x,a)') answer
c* unix)
c --- output filename's root or basename used in the default
c --- names for the remaining files.
      default = root_def
      length_def = lentru(default)
      write (out, '('' root for output filenames ('',a,'')? '',$)')
     *  default(1:length_def)
      read (in, '(a)', iostat=status) root
      length_root = lentru(root)
      if ((status.ne.0) .or. (length_root.eq.0)) then
        root = root_def
      endif
      length_root = lentru(root)
c --- a kludge to fix the file names not passed as arguments.
c     if (root(length_root:length_root) .eq. 'p') then
c       root = root(1:length_root-1)
c       length_root = length_root - 1
c     endif
      write (out, '(1x,a)') root(1:length_root)
c --- summary of warning messages and final summary/log filename
      default = root(1:length_root)//log_type
      length_def = lentru(default)
c* (vax
c* (pc
c      write (out, '('' log filename or "screen" ('',a,'')? '',$)')
c* pc)
c* vax)
c* (unix
      write (out, '('' log filename or "stdout" ('',a,'')? '',$)')
c* unix)
     *  default(1:length_def)
      read (in, '(a)', iostat=status) log_file
      length_log = lentru(log_file)
      if ((status.ne.0) .or. (length_log.eq.0)) then
        log_file = default
        length_log = length_def
      endif
      write (out, '(1x,a)') log_file(1:length_log)
c --- we can't actually open the log file now, since we're still
c --- in the process of using unit 6 interactively...
c
c --- printer output file --------------------------------------
      punt = print_unit
      default = root(1:length_root)//print_type
      length_def = lentru(default)
c* (unix
      write (out,
     * '('' filename for printer output or "screen" ('',
     * a,'')? '',$)')
     *  default(1:length_def)
      read (in, '(a)', iostat=status) fname
c* unix)
c* (vax
c* (pc
c      write (out, '(a)') ' filename for printer output or "log"'
c      write (out,
c     * '('' answer "log" to combine with log file ('',
c     * a,'')? '',$)')
c     *  default(1:length_def)
c      read (in, '(a)', iostat=status) fname
c      fname = dnstrg( fname )
c      if (fname .eq. 'log') fname = 'screen'
c* pc)
c* vax)
      length = lentru(fname)
      if ((status.ne.0) .or. (length.eq.0)) then
        fname = default
        length = length_def
      endif
      write (out, '(1x,a)') fname(1:length)
      if (dnstrg(fname(1:length)) .eq. 'screen') then
        punt = stderr_unit
      else
c       call openfl(     iunit, ifile,    istat,  izero,   ishr,
c    *          iform, irecl)
        call openfl(punt, fname, openstat, 'null', 'none',
     *         'none', 0)
      endif
c     open (print_unit, file=fname(1:length), status=openstat)
c
c --- summary record filename --------------------------------------
      write (out,
     * '('' will this job generate a summary file (no)? '',$)')
      read (in, '(a)', iostat=status) answer
      if ((status.ne.0) .or. (answer.eq.' ')) answer = 'n'
      write (out, '(1x,a)') answer
      answer = dnstrg( answer )
      if (answer .eq. 'y') then
        default = root(1:length_root)//summary_type
        length_def = lentru(default)
        write (out,
     *    '('' summary output filename ('',a,'')? '',$)')
     *    default(1:length_def)
        read (in, '(a)', iostat=status) fname
        length = lentru(fname)
        if ((status.ne.0) .or. (length.eq.0)) then
          fname = default
          length = length_def
        endif
        write (out, '(1x,a)') fname(1:length)
c       call openfl(       iunit, ifile,    istat,  izero,   ishr,
c    *          iform, irecl)
        call openfl(summary_unit, fname, openstat, 'null', 'none',
     *      'noprint', 0)
c       open (summary_unit,file=fname(1:length),status=openstat)
c    *        form = 'noprint')
      endif
c
c --- archive phase filename --------------------------------------
      write (out,
     *  '('' will this job generate an archive file (no)? '',$)')
      read (in, '(a)', iostat=status) answer
      if ((status.ne.0) .or. (answer.eq.' ')) answer = 'n'
      write (out, '(1x,a)') answer
      answer = dnstrg( answer )
      if (answer .eq. 'y') then
        default = root(1:length_root)//arch_type
        length_def = lentru(default)
        write (out, '('' archive output filename ('',a'')? '',$)')
     *    default(1:length_def)
        read (in, '(a)', iostat=status) fname
        length = lentru(fname)
        if ((status.ne.0) .or. (length.eq.0)) then
          fname = default
          length = length_def
        endif
        write (out, '(1x,a)') fname(1:length)
c       call openfl(    iunit, ifile,    istat,  izero,   ishr,
c    *          iform, irecl)
        call openfl(arch_unit, fname, openstat, 'null', 'none',
     *      'noprint', 0)
c       open (arch_unit, file=fname, status=openstat)
c     *        form = 'noprint')
      endif
c
c --- travel time table filenames --------------------------------------
      write (out, '('' use travel time tables (no)? '',$)')
      read (in, '(a)', iostat=status) answer
      if ((status.ne.0) .or. (answer.eq.' ')) answer = 'n'
      write (out, '(1x,a)') answer
      answer = dnstrg( answer )
      if (answer .eq. 'y') then
        default = 'none'
        length_def = lentru(default)
        write (out, '('' filename for first table ('',a,'')? '',$)')
     *    default(1:length_def)
        read (in, '(a)', iostat=status) fname
        length = lentru(fname)
        if ((status.ne.0) .or. (length.eq.0)) then
          fname = default
          length = length_def
        endif
        write (out, '(1x,a)') fname(1:length)
c       call openfl(    iunit, ifile, istat,  izero,   ishr,
c    *          iform, irecl)
        call openfl(tb11_unit, fname, 'old', 'null', 'readonly',
     *         'none', 0)
c       open (tb11_unit, file=fname(1:length), status='old')
	nttab = 1
        default = 'none'
        length_def = lentru(default)
        write (out, '('' filename for second table ('',a,'')? '',$)')
     *    default(1:length_def)
        read (in, '(a)', iostat=status) fname
        length = lentru(fname)
        if ((status.ne.0) .or. (length.eq.0)) then
          fname = default
          length = length_def
        endif
        write (out, '(1x,a)') fname(1:length)
        if (dnstrg(fname(1:4)) .ne. 'none') then
	  nttab = 2
          call openfl(tb12_unit, fname, 'old', 'null', 'readonly',
     *      'none', 0)
c         call openfl(    iunit, ifile, istat,  izero,   ishr,
c    *      iform, irecl)
	endif
        default = 'none'
        length_def = lentru(default)
        write (out, '('' filename for third table ('',a,'')? '',$)')
     *    default(1:length_def)
        read (in, '(a)', iostat=status) fname
        length = lentru(fname)
        if ((status.ne.0) .or. (length.eq.0)) then
          fname = default
          length = length_def
        endif
        write (out, '(1x,a)') fname(1:length)
        if (dnstrg(fname(1:4)) .ne. 'none') then
          call openfl(tb13_unit, fname, 'old', 'null', 'readonly',
     *      'none', 0)
c         call openfl(    iunit, ifile, istat,  izero,   ishr,
c    *      iform, irecl)
	  if(nttab .ne. 2) then
	    write (out, '(a)') 'You may not define tables 11 and 13'
	    write (out, '(a)') 'without defining travel time table 12!'
	    stop
	  endif
	  nttab = 3
	endif
      endif
c
c --- optional relocation of events with revised delays --------------
      write (out, '('' does this job relocate events with revised '',
     *  ''station delays (no)? '',$)')
      read (in, '(a)', iostat=status) answer
      if ((status.ne.0) .or. (answer.eq.' ')) answer = 'n'
      write (out, '(1x,a)') answer
      answer = dnstrg( answer )
      if (answer .eq. 'y') then
        default = root(1:length_root)//stacorr_type
        length_def = lentru(root)
        write (out,
     *    '('' station delay output filename ('',a,'')? '',$)')
     *    default(1:length_def)
        read (in, '(a)', iostat=status) fname
        length = lentru(fname)
        if ((status.ne.0) .or. (length.eq.0)) then
          fname = default
          length = length_def
        endif
        write (out, '(1x,a)') fname(1:length)
c       open (stacorr_unit, file=fname(1:length), status=openstat)
c    *        form = 'noprint')
c       call openfl(       iunit, ifile,    istat,  izero,   ishr,
c    *          iform, irecl)
        call openfl(stacorr_unit, fname, openstat, 'null', 'none',
     *      'noprint', 0)
      endif
c --- write out a blank record --------------------------------------
      write (out, *)
c --- we're done with interactive i/o using stdout (unit 6) so
c --- we can finally open the "log" file, if there is to be one.
c     fname = dnstrg( log_file(1:length_log) )
      fname = log_file(1:length_log)
      if((fname .ne. 'stdout') .and.
     *   (fname .ne. 'nolog')  .and.
     *   (fname .ne. 'screen'))
     *  call openfl(log_unit, log_file(1:length_log), openstat, 'null', 
     *   'none', 'none', 0)
c       call openfl(   iunit,                  ifile,    istat,  izero, 
c         ishr,   iform, irecl)
c    *  open (log_unit,file=log_file(1:length_log),status=openstat)
c --- use of status='scratch' is not standard and causes the files
c --- to be opened with "unique" names, which are uninformative.
c --- furthermore, it's sometimes useful to keep the temporary files
c --- when the program aborts.  therefore, you may wich to handle the
c --- "automatic" deletion another way (external to hypoellipse).
      write (out, *) 'root name = ', root(1:length_root)
c --- station scratch file number 1
      fname = root(1:length_root)//tmp1_type
c     call openfl(    iunit, ifile,     istat,  izero,   ishr,
c    *          iform, irecl)
      call openfl(tmp1_unit, fname, 'scratch', 'null', 'none',
     *      'noprint', 0)
c     open (tmp1_unit, file=root(1:length_root)//tmp1_type,
c    *      status='scratch')
c    *      form = 'noprint')
c --- station scratch file number 2
      fname = root(1:length_root)//tmp2_type
c     call openfl(    iunit, ifile,    istat,  izero,   ishr,
c    *          iform, irecl)
      call openfl(tmp2_unit, fname, 'scratch', 'null', 'none',
     *      'noprint', 0)
c     open (tmp2_unit, file=root(1:length_root)//tmp2_type,
c    *      status='scratch')
c    *      form = 'noprint')
c --- temporary summary storage and uamag storage.  length must
c --- be 300 for uamag records.
      fname = root(1:length_root)//tmpsum_type
c     call openfl(       iunit, ifile,    istat,  izero,   ishr,
c    *          iform, irecl)
      call openfl(tmpsum_unit, fname, 'scratch', 'null', 'none',
     *      'noprint', 300)
c     open (tmpsum_unit, file=root(1:length_root)//tmpsum_type,
c    *      status='scratch', recl=300)
c    *      form = 'noprint')
c --- temporary archive storage
      fname = root(1:length_root)//tmparc_type
c     call openfl(      iunit, ifile,     istat,  izero,   ishr,
c    *          iform, irecl)
      call openfl(tmparc_unit, fname, 'scratch', 'null', 'none',
     *      'noprint', 0)
c     open (tmparc_unit, file=root(1:length_root)//tmparc_type,
c    *      status='scratch')
c    *      form = 'noprint')
c --- temporary message storage
      fname = root(1:length_root)//tmpmess_type
c     call openfl(       iunit, ifile,    istat,  izero,   ishr,
c    *          iform, irecl)
      call openfl(tmpmess_unit, fname, 'scratch', 'null', 'none',
     *      'noprint', 0)
c     open (tmpmess_unit, file=root(1:length_root)//tmpmess_type,
c    *      status='scratch')
c    *      form = 'noprint')
      return
      end
c end opfls
