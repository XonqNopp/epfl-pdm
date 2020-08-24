! Sets the general parameters to HD
      subroutine GICHD
      implicit none
      include 'for/parameter.inc'
      include 'for/const.inc'
      include 'for/status.inc'
      CF13=4.d0  ! TAUMAX [us]
      CF14=1.d-2 ! TAUMIN [us]
      CF15=0.d0  ! interval at which WOUTP writes, 0 means TAUMAX
      write(*,*) ' ! HD data saving !'
      end
