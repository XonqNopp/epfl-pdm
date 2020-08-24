! To have the standard output writing
      subroutine GICWRITESTD
      implicit none
      include 'for/parameter.inc'
      include 'for/const.inc'
      include 'for/status.inc'
      CF13=1.d3     ! TAUMAX [us]
      CF14=1.d2     ! TAUMIN [us]
      CF15=1.d-1    ! interval at which WOUTP writes
      CF16=0.d0     ! starting time for WOUTP
      CF12=9.2d-1   ! stop writing
      write(*,*) ' Standard writing.'
      end
