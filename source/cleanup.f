c cleanup.for    []      
      subroutine cleanup   

      integer arch_unit
      parameter (arch_unit = 11)
      common /dhip/ inpt,isa,ilis,inmain,injump

c --- close input(inmain), jump(inmain), archive(11)

      inmain = 8
      inpt = inmain

      close(inmain)
      close(injump)
      close (arch_unit)

      return
      end
c end cleanup
