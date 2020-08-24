SUBROUTINE saw_KO(indx_res,a_in,ni_in,ti_in,nrpnts,eps1,bgeom,s1,dw_KO)
  !
  ! Computes dW_KO from Porcelli et al, Plasma Phys. Contr. Fusion 38 (1996) 2163, which is given by
  ! dW_KO = integral( ...)
  !
  USE pref_const 
  USE params_const
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: indx_res,nrpnts
  REAL(RKIND), DIMENSION(nrpnts), INTENT(IN) :: a_in, &
    &  ni_in,ti_in
  REAL(RKIND), INTENT(IN) :: eps1,bgeom,s1
  REAL(RKIND), INTENT(OUT) :: dw_KO 
  REAL(RKIND), DIMENSION(nrpnts) :: x,pi
  REAL(RKIND) :: pi0,beti0,cp
  pi=e_charge*ni_in*ti_in
  pi0=pi(1)
  beti0=2._rkind*mu0*pi0/bgeom**2
  x=a_in/a_in(indx_res)
  cp=5._rkind/2._rkind*sum(((x(1:indx_res-1)+x(2:indx_res))/2._rkind)**(3._rkind/2._rkind)* &
   &  (((pi(1:indx_res-1)+pi(2:indx_res))/2._rkind)/pi0*(x(2:indx_res)-x(1:indx_res-1))))
  dw_KO=0.6_rkind*cp*eps1**(0.5_rkind)*beti0/s1  
END SUBROUTINE saw_KO
