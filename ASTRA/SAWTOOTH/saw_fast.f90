SUBROUTINE saw_fast(rgeom,bgeom,q_in,nrpnts,indx_res,a_in,eps1,pfast_in,s1,dw_fast)
  !
  ! Computes dW_fast from Porcelli et al, Plasma Phys. Contr. Fusion 38 (1996) 2163, which is given by
  ! dW_fast = integral( ...)
  ! This formula is an approximation. Its relevance to experimental results was discussed in Angioni et al, Plasma Phys. Contr Fus. 44 (2002) 205
  !
  USE pref_const 
  USE params_const
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: indx_res,nrpnts
  REAL(RKIND), DIMENSION(nrpnts), INTENT(IN) :: a_in, &
    &  pfast_in,q_in
  REAL(RKIND), INTENT(IN) :: rgeom,eps1,bgeom,s1
  REAL(RKIND), INTENT(OUT) :: dw_fast 
  REAL(RKIND), DIMENSION(nrpnts) :: bp,x 
  REAL(RKIND) :: bpas
  bp=a_in*bgeom/(rgeom*q_in)
  x=a_in/a_in(indx_res)
  bpas=-2._rkind*mu0/bp(indx_res)**2._rkind* &
    & sum(((x(1:indx_res-1)+x(2:indx_res))/2._rkind)**(3._rkind/2._rkind)* &
     & (pfast_in(2:indx_res)-pfast_in(1:indx_res-1)))
  dw_fast=eps1**(3._rkind/2._rkind)*bpas/s1 
END SUBROUTINE saw_fast
