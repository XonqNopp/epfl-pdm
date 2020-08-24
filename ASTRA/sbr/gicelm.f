! Sets the constants needed for ELM crash
      subroutine GICELM(amplitude_t,amplitude_n,t_0,tauELM,rhoELM,
     &                  interval)
      implicit none
      include 'for/parameter.inc'
      include 'for/const.inc'
      include 'for/status.inc'
      double precision next_elm,amplitude_t,amplitude_n,t_0,
     & tauELM,rhoELM,interval,desired_elm
      integer how_many_before
      desired_elm=2.201
      how_many_before=10
      amplitude_t=10000 ! ELM amplitude
      amplitude_n=20
      interval=20.        ! interval [ms] between 2 ELMs
      next_elm=desired_elm-how_many_before*interval/1000.
      t_0=next_elm    ! time for ELM to occur
      tauELM=.1     ! ELM duration [ms]
      rhoELM=.78      ! rho_astra left boundary for ELM
      write(*,'(a,f6.4,a,f4.1,a)') ' ELM ready to occur at t=',next_elm,
     &        ' inter-ELM period is ',interval,'ms'
      return
      end
