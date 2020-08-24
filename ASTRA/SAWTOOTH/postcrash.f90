SUBROUTINE postcrash(jmax,jmix,fps_p, & 
     &  q_in,psi_in, &
     &  phi_in,q_out, &
     &  psi_out)  
  !
  ! to describe
  !
  USE pref_const
  USE params_const
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: jmax,jmix
  REAL(RKIND), DIMENSION(jmax), INTENT(IN) :: fps_p, & 
       &  q_in,psi_in, &
       &  phi_in
  REAL(RKIND), DIMENSION(jmax), INTENT(OUT) :: q_out,psi_out
  INTEGER :: i,jk,iflag,nbc(2),m
  REAL(RKIND), DIMENSION(jmax) :: sigma,fi1,fi11
  REAL(RKIND) :: rk,ybc(2),dum,ypr2
  !Initialize
  q_out=q_in
  psi_out=psi_in
	psi_out=fps_p+phi_in
  !Construct psi  
  nbc(1)=0
  nbc(2)=0
  ybc(1)=0._rkind
  ybc(2)=0._rkind
  sigma(1:jmax)=1e-6
	m=1
  call cbsplgen0(phi_in(1:jmax),psi_out(1:jmax),jmax, & 
          &   phi_in(1:jmax),fi1(1:jmax),fi11(1:jmax),ypr2, &
          &   jmax,1,1,sigma(1:jmax),nbc,ybc,iflag)
  q_out(1:jmax)=1._rkind/fi11(1:jmax)
  dum=psi_in(jmax)-psi_out(jmax)
	psi_out=psi_out+dum
END SUBROUTINE postcrash
