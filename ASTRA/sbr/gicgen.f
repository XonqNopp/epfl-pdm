! Sets the general parameters to default
      subroutine GICGEN
      implicit none
      include 'for/parameter.inc'
      include 'for/const.inc'
      include 'for/status.inc'
      CF5=2         ! ECH=0 @ rho>CF5 (X3 replaces X2)
      CF6=2         ! ECH=0 @ rho>CF5 (X2 suppressed)
      CF13=1.d3     ! TAUMAX [us]
      CF14=1.d2     ! TAUMIN [us]
      CF15=0.       ! interval at which WOUTP writes
!     CF16=2.2      ! starting time for WOUTP
!     CF12=2.222    ! Stop writing
      CF16=2.0      ! starting time for WOUTP
      CF12=2.022    ! Stop writing
      CHI1=0.       ! For now, XI is only anomalous
      CMHD4=1.2     ! For sawteeth period
      CV5=0.73      ! rho_ped for scaling (on rho_vol)
!     CV6=1.        ! ped scaling active for Te
      CV9=0.5       ! start time for pedestal scaling
      CV8=0.6       ! cut grad p before CV8
      CV10=0.1      ! energy scaling
      CRF1=0.3      ! starting time for sawteeth

      CRF1=100      ! NO SAWTEETH FOR NOW !

      ! Using same chie for each case (#40080 0.8s)
      CV6=0
      CV9=100
      CV10=100
      ZRD4=1.128746
      ZRD33=0.816030
      end
