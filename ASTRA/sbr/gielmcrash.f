! Trying to make the ELM crash in here
!    GIELMCRASH( option, length_of_ELM, ELM_period )
!                option 
!                length_of_ELM is the duration of the ELM [ms]
!                ELM_period is the period between 2 ELMs
!                           meaning between the end of the previous and
!                           the beginning of the next [ms]
!    When transiting to MHD limits, the only needed changes should be
!      with the conditions 
!    CSOL1 : amplitude
!        2 : length
!        3 : period
!        4 : radius (on rho from edge)
!
!     GI 2010
!
      subroutine GIELMCRASH(option,length,period)
      implicit none
      include 'for/parameter.inc'
      include 'for/const.inc'
      include 'for/status.inc'
      double precision length,period,lengthSI,periodSI,elmtime,
     & lastelm
      integer option,j
      save elmtime,lastelm
      data elmtime /-1.d0/ lastelm /-1.d0/
      periodSI=period/1.d3
      lengthSI=length/1.d3
      if( lastelm+periodSI .le. TIME )then ! time to crash
           ! Time elapsed during ELM
           if( elmtime .lt. 0 )then
                 elmtime = 0
           else
                 elmtime=elmtime+TAU
                 ! TAU is the previous timestep (watch the units)
           endif
           ! Doing the crash
           if( elmtime .lt. lengthSI )then ! still crashing
!              The following must be rewritten in fortran-compatible
!                HE=HE+CSOL1*XSTEP(CSOL4)
!                XI=XI+CSOL1*XSTEP(CSOL4)
!                CN=-CPEL1*(1-XSTEP(CRAD1))
!              Will it be something like this ??
!                do j = 1,NA1
!                      if( j*HRO/ROC .gt. CSOL4 ) then ! crash region
!                            CN(j)=0
!                            HE(j)=HE(j)+CSOL1
!                            XI(j)=XI(j)+CSOL1
!                      else
!                            CN(j)=-CPEL1
!                      endif
!                enddo
           else ! done... See you next time ;-)
                 ! check if pedestal reconstruction occurs at once or
                 ! gently... Maybe something to do here...
                 lastelm=TIME
                 elmtime=-1.d0
           endif
      endif
      end
