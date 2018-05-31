c lisinc.for    []
      subroutine lisinc(qno, sumrms, rms, jav, iq, nrwt, avwt, nr,
     * kdx, wt, nrp, xmag, fmag, blank, avxm, avfm, nxm, nfm, sxm,
     * sxmsq, sfm, sfmsq, sw, ipcod, ww, ksmp, nsmpr, ssmpr, x,
     * ssmpq, ssmpw, nres, sr, srsq, srwt, nresp, srp, srpsq, srpwt,
     * iscod, nsres, ssr, ssrsq, ssrwt, nsresp, ssrp, ssrpsq, ssrpwt)
c increment summary after each earthquake
      save
      include 'params.inc' 
      dimension kdx(npa), wt(npa), xmag(npa), fmag(npa), sw(nsn)
      dimension ipcod(nsn), ww(npa), ksmp(npa), x(4,npa), nres(nsn)
      dimension iscod(nsn)
      dimension nxm(nsn), nfm(nsn), sr(nsn), srsq(nsn),
     * srwt(nsn),sxm(nsn),sxmsq(nsn),sfm(nsn),sfmsq(nsn),qno(4)
      dimension nresp(nsn), srp(nsn), srpsq(nsn), srpwt(nsn)
      dimension nsresp(nsn), ssrp(nsn), ssrpsq(nsn), ssrpwt(nsn)
      dimension nsres(nsn),ssr(nsn),ssrsq(nsn),ssrwt(nsn),nsmpr(nsn),
     * ssmpr(nsn),ssmpq(nsn),ssmpw(nsn)
c
      qno(jav) = qno(jav) + 1
      sumrms = sumrms + rms
      if(jav .gt. iq) return
c compute the sqrt(sum of weights squared) for pavlis average residual calc
      srswq = 0.0
      if(nrwt .gt. 4) srswq = avwt*(nrwt - 4)
c accumulate summary of travel time residuals --- include p, s, and s-p
      do 825 i = 1, nr
c ji is index to station list for phase number i
        ji = kdx(i)
        wtu = wt(i)
        if(i .le. nrp) then
          if((xmag(i) .ne. blank) .and. (avxm .ne. blank)) then
            dxmag = xmag(i)-avxm
            nxm(ji) = nxm(ji)+1
            sxm(ji) = sxm(ji)+dxmag
            sxmsq(ji) = sxmsq(ji)+dxmag**2
          endif
          if((fmag(i) .ne. blank) .and. (avfm .ne. blank)) then
            dfmag = fmag(i)-avfm
            nfm(ji) = nfm(ji)+1
            sfm(ji) = sfm(ji)+dfmag
            sfmsq(ji) = sfmsq(ji)+dfmag**2
          endif
          if( (sw(ji) .eq. 0.) .or. ((ipcod(ji) .gt. 3) .and.
     *    (ipcod(ji) .lt. 9)) ) wtu = ww(i)
          if(wtu .eq. 0.) goto 825
          if(ksmp(i) .eq. 0) then
c smp residual calculation
            nsmpr(ji) = nsmpr(ji) + 1
            ssmpr(ji) = ssmpr(ji) + x(4,i)*wtu
            ssmpq(ji) = ssmpq(ji) + x(4,i)**2*wtu
            ssmpw(ji) = ssmpw(ji) + wtu
            goto 825
          else
c p-residual calculation
            nres(ji) = nres(ji) + 1
            sr(ji) = sr(ji) + x(4,i)*wtu
            srsq(ji) = srsq(ji) + x(4,i)**2*wtu
            srwt(ji) = srwt(ji) + wtu
c pavlis p-residual calculation
            if(srswq .gt. 0.0) then
              wtu = wtu*srswq
              nresp(ji) = nresp(ji) + 1
              srp(ji) = srp(ji) + x(4,i)*wtu
              srpsq(ji) = srpsq(ji) + x(4,i)**2*wtu
              srpwt(ji) = srpwt(ji) + wtu
            endif
            goto 825
          endif
        else
c s-residual calculation
          if( (sw(ji) .eq. 0.) .or. ((iscod(ji) .gt. 3) .and.
     *    (iscod(ji) .lt. 9)) ) wtu = ww(i)
          if(wtu .eq. 0.0) goto 825
          nsres(ji) = nsres(ji) + 1
          ssr(ji) = ssr(ji) + x(4,i)*wtu
          ssrsq(ji) = ssrsq(ji) + x(4,i)**2*wtu
          ssrwt(ji) = ssrwt(ji) + wtu
c pavlis s-residual calculation
          if(srswq .gt. 0.0) then
            wtu = wtu*srswq
            nsresp(ji) = nsresp(ji) + 1
            ssrp(ji) = ssrp(ji) + x(4,i)*wtu
            ssrpsq(ji) = ssrpsq(ji) + x(4,i)**2*wtu
            ssrpwt(ji) = ssrpwt(ji) + wtu
          endif
        endif
  825 continue
      return
      end
c end lisinc
