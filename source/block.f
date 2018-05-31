c block.for   []
      block data
c initialize constants in common statements
      include 'params.inc'
      parameter (ndly = 11)
      logical good, eoff, supout
      character*4 iahead*60, msta*5, nsta*5, icard*110, uacal*50
      common /char/ iahead, msta(npa), nsta(nsn), icard
      character*1 iclass
      common /dbio/ iclass(0:4)
      common /dbiln/ ioldq
      common /dhio/ nedit,good
      common /dio/ rmsmx,presmx,sresmx,noutmx,nimx,iprun,semx
      common /dph/ noswt, eoff
      common /dhin/ iglob, zup, zdn
      common /dhip/ inpt,isa,ilis,inmain,injump
      common /dhil/ iq,ilat,kms,icat
      character*1 bksrc
      common /dipu/ ipkdly, igsum, bksrc
      common /dit/ tid(lmax,lmmax),did(lmax,lmmax),lowv,modin(mmax+3)
      logical medmag
      common /dinx/ imag,imagsv,medmag
      common /dix/ iexcal, uacal
      common /dmost/ ipun,ivlr,blank
      common /imost/ test(100)
      character*1 iqcls
      common /il1/ iqcls
      common /imost1/ dly(ndly,nsn),iprn,kno,klas(5, nsn)
      common /logfil/ logfil
      common /idno/ ksel,ksort
      common /idt/ v(lmax2)
      common /iox/ prr(nsn),iuses
      common /it/ vpvs(lmax2),vpvsm(mmax+3),nttab
      common /ix/ ir,qspa(9,40)
      common /ohq/ gap, supout
      common /pot/ ts(npa),ihr,model(npa), keyphi(npa)
      common /reloc/ irelo, nreloc
      common /rioq/ been,damp,dmpinc,igo
      character*1 mgndx
      common /xfmno1/ mgndx
      common /ip1/ rsew(4)
      common /ilmpu/ ns
      data uacal/'pub1:[alaska.data]uofacal.dat'/, iexcal/0/
c     data rsew/1., 3., 7.5, 15./, inpt/8/, inmain/8/, injump/12/
c revised default relative weights 4/22/89
      data rsew/1., 5., 10., 20./, inpt/8/, inmain/8/, injump/12/
      data logfil/6/,isa/0/,iqcls/'b'/,damp/0.0010/,bksrc/' '/
      data iprun/1/, supout/.false./, ns/0/
      data iclass/'0','a','b','c','d'/, nttab/0/
      data iahead/'    '/,mgndx/' '/,good/.true./
      data blank/1.e20/
      data test/100*1.23456/, v/lmax2*0.001/, model/npa*1/
      data ivlr,kms,iprn,ipun,imag,iq,ksort,ksel,icat/0,1,1,0,0,2,0,1,1/
      data ioldq/0/,ir/0/,lowv/1/,imagsv/0/,iuses/0/,ipkdly/1/
cds      data tid,did/narray*0.0/, modin/13*0/, iglob/1/
      data tid,did/narray*0.0/, modin/mmaxp3*0/, iglob/1/
      data nedit/0/, eoff/.false./, igsum/1/, irelo/0/, nreloc/0/
      data ilis/1/,medmag/.false./
      end
c end block
