! Sets a new time value for the ELM to occur
!  CSOL2 = CSOL2 + NEWTIME[ms]
      subroutine GINEWELM(t_0,interval,deltaELM)
      implicit none
      double precision interval,t_0,deltaELM
      include 'for/parameter.inc'
      include 'for/const.inc'
      include 'for/status.inc'
      if( TIME .gt. t_0+deltaELM/1.d3 ) then
        t_0=t_0+interval/1.d3
        write(*,'(a,f6.4,a,f5.1,a)') '   New ELM will occur @ t=',t_0,
     &   ' (in ',(t_0-TIME)*1000,'ms from now)'
      endif
      return
      end
