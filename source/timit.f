c timit.for    [unix]
      subroutine timit(ikey)
c non standard code for finding cpu time on a vax/vms computer
c* (vax
c      if (ikey .eq. 0) then
c        handle = 0
c        it = lib$init_timer(handle)
c      else
c        it = lib$show_timer(handle)
c      endif
c* vax)
c* (pc
c* pc)
c* (unix
c* unix)
      return
      end
c end timit
