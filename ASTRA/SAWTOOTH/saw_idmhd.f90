SUBROUTINE saw_idmhd(eps1,kappa1, s1,betapB1,gamta_mhd)
  !
  ! Calulates the ideal internal kink growth rate from Martynov et al, Plasma Phys. Contr. Fus 47 (2005) 1743
  !
  USE pref_const
  USE params_const
  IMPLICIT NONE
  REAL(RKIND),INTENT(IN) :: eps1,kappa1, s1,betapB1
  REAL(RKIND), INTENT(OUT) :: gamta_mhd
  REAL(RKIND) :: dum1,dum2,dum3,dum4
  !
  dum1=0.9_rkind-(0.6_rkind+0.1_rkind*s1)*kappa1
  dum2=betapB1-dum1
  dum3=eps1*kappa1/(1+7.0_rkind*eps1*s1)
  dum4=0.44_rkind*dum3*dum2
  gamta_mhd=dum4
END SUBROUTINE saw_idmhd
