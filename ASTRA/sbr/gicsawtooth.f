! Changes CMHD4 parameters with tau's according to current time
      subroutine GICSAWTOOTH(c_sawtooth,tau_min,tau_max)
      implicit none
      include 'for/parameter.inc'
      include 'for/const.inc'
      include 'for/status.inc'
      double precision c_sawtooth,tau_min,tau_max
      if( TIME .gt. 1.8 ) then
        c_sawtooth=.35
        tau_min=.01
        tau_max=4.
      elseif( TIME .gt. 1.3 ) then
        c_sawtooth=.5
        tau_min=.1
        tau_max=10.
      elseif( TIME .gt. .7 ) then
        c_sawtooth=1.
        tau_min=1.
        tau_max=100.
      endif
      write(*,'(a,f4.2,a,f4.2,a,f5.1)') '   C=',c_sawtooth,
     & ' t_min=',tau_min,' t_max=',tau_max
      end
