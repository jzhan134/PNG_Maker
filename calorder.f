      subroutine calorder(psiloc, rmin, polygon,
     + np, xtemp, ytemp, phi)
            
      INTEGER, PARAMETER  ::  NMAX = 1000
      double precision rg, psi, xmean, ymean,rxij, ryij
      double precision RP, rmin,accumpsir,accumpsii
      integer i,j,k, nb(nmax),np, polygon
      double precision psir(nmax), psii(nmax),psiloc(nmax),
     + xtemp(nmax), ytemp(nmax), phi(nmax)
      
      psir=0.0
	psii=0.0
      accumpsii=0
      accumpsir=0

      !  GET PSI6 FOR ALL PARTICLES
      
!  GET PSI6 FOR ALL PARTICLES
            if(polygon .eq. 3) then
          scale=3
      elseif(polygon .eq. 4) then
          scale=4
      elseif(polygon .eq. 6) then
          scale=6
      else
          scale=1
      end if
	do i=1, NP
            nb(i) = 0
            psir(i) = 0
            psii(i) = 0
            do j=1, NP
                if (i.ne.j) then 
                        rxij=xtemp(j)-xtemp(i)		
                        ryij=ytemp(j)-ytemp(i)
                        RP=sqrt(RXIJ**2+RYIJ**2)
                        if (RP.le.rmin) then
                                nb(i)=nb(i)+1
                                psir(i)=psir(i)+cos(scale*phi(j))
                                psii(i)=psii(i)+sin(scale*phi(j))
                        end if
                end if
            end do
            if (nb(i).ne.0) then
                psir(i)=psir(i)/dble(nb(i))
                psii(i)=psii(i)/dble(nb(i))
            end if
	end do
        psi=0
        accumpsir=0
        accumpsii=0
        do i=1,np
            psiloc(i)=sqrt(psir(i)**2+psii(i)**2)
            accumpsir=accumpsir+psir(i)
            accumpsii=accumpsii+psii(i)
        end do
        accumpsir=accumpsir/np
        accumpsii=accumpsii/np
        psi=sqrt(accumpsir**2+accumpsii**2)
      end
      
