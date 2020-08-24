SUBROUTINE saw_resknk(a1,te1,ti1,rLarmori1,s1, &
     &  Slundqst1,tau_alf1,omdiai1,omdiae1,cr_resknk,s1_crits,gamma_res)
  !
  ! Calculates gamma_rho and gamma_eta from Angioni et al, Nucl. Fus 43 (2003) 455
  !
  USE pref_const
  USE params_const
  IMPLICIT NONE
  REAL(RKIND), INTENT(IN) :: a1,te1,ti1,rLarmori1,s1, &
       &  Slundqst1,tau_alf1,omdiai1,omdiae1,cr_resknk
  REAL(RKIND), DIMENSION(ncrits), INTENT(OUT) :: s1_crits, gamma_res
  REAL(RKIND) :: dum1
  gamma_res=0.0_rkind
  dum1=cr_resknk*((omdiai1*omdiae1)**(0.5_rkind)) & 
       &  /((2._rkind*(1._rkind+te1/ti1)/pi_cst)**(2._rkind/7._rkind)) & 
       &  /((rLarmori1/a1)**(4._rkind/7._rkind)) & 
       &  /(Slundqst1**(-1._rkind/7._rkind)) & 
       &  /(1/tau_alf1)
  s1_crits(1)=dum1**(7._rkind/6._rkind)
  dum1=cr_resknk*((omdiai1*omdiae1)**(0.5_rkind)) & 
       &  /(Slundqst1**(-1._rkind/3._rkind)) & 
       &  /(1/tau_alf1)
  s1_crits(2)=dum1**(3._rkind/2._rkind)
  dum1=((2._rkind*(1._rkind+te1/ti1)/pi_cst)**(2._rkind/7._rkind)) & 
       &  *((rLarmori1/a1)**(4._rkind/7._rkind)) & 
       &  *(Slundqst1**(-1._rkind/7._rkind)) & 
       &  *(1/tau_alf1)*s1**(6._rkind/7._rkind)
  gamma_res(1)=dum1 
  dum1=Slundqst1**(-1._rkind/3._rkind)/tau_alf1 &
       &  *s1**(2._rkind/3._rkind)
  gamma_res(2)=dum1
END SUBROUTINE saw_resknk
