! Sets the constants needed for ELM crash to be recorded
      subroutine GICELMPHASE(tau_min,tau_max,start_write,stop_write)
      implicit none
      double precision tau_min,tau_max,start_write,stop_write
      start_write=2.2006
      stop_write=2.2015
      tau_min=.01
      tau_max=1.
      write(*,'(a,f4.2,a,f4.2,a,f6.4,a,f6.4,a)') 'taus=[',tau_min,' ',
     &         tau_max,'], t_write=[',start_write,' ',stop_write,']'
      return
      end
