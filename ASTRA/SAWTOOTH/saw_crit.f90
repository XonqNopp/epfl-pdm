SUBROUTINE saw_crit(rgeom,bgeom,Zi_charge,amassi,nrpnts,rho_in,ne_in,te_in,ni_in, & 
     &   ti_in,pfast_in,Efast_in,btp_in,a_in,q_in,s_in,grho_in,volume_in,etaneo_in, &
     &   kappa_in,sawcoef,icrash,nbres,indx_res,s1,s1_crits,gamma_res, &
     &   dw_mhd,dw_KO,dw_fast,dw_th)
  !
  ! Calculates relevant contributions required for deciding if a sawtooth crash should occur
  ! It is based on Porcelli et al model: Plasma Phys. Contr. Fusion 38 (1996) 2163, slightly modified for the
  ! resistive criteria as in Angioni et al, Nucl. Fus 43 (2003) 455
  ! 
  ! Now only one resonant surface is allowed, outermost is taken as valid
  !
  !
  USE pref_const
  USE params_const
  IMPLICIT NONE
  !     Inputs in SI units, except temperatures in [eV]
  ! btp_in is betap_Bussac, defined as:
  ! Bp^2/2mu0 = mu0 I^2 rgeom / (4 volume)
  ! where I is the total current inside the rho surface
  !
  ! a_in is the minor radius, and rho_in is sqrt(psi)  
  ! ne, te, etc are the plasma parameters profiles. 
  ! grho = <|grad(rho)|>
  ! etaneo is the neoclassical resistivity
  !
  ! sawcoef provides the free parameters in the crash conditions.
  ! If sawcoef not given uses default values.
  ! sawcoef(1) : chot for the condition -dW_core > chot * om_dia_hot * tau_a (default = 1)
  !              if sawcoef(1)=-1, skips the condition, if sawcoef(1)=0 uses default value
  ! sawcoef(2) : crho for the condition -dW_tot  > - crho * rho_hat (default = 1)
  !              if sawcoef(2)=-1, skips the condition, if sawcoef(2)=0 uses default value
  ! sawcoef(3) : cr_resknk for the condition max(gamma_res) > cr_resknk * sqrt(omstar_i * omstar_e) (default = 0.5)
  !              if sawcoef(3) = 0 uses default value
  !              if sawcoef(3) < 0 then uses condition: s1 > ABS(sawcoef(3))
  !              (-0.2 good value (s1>0.2), use sawcoef(3)=-100 to skip condition, since s1 never larger than 100)
  !
  INTEGER, INTENT(IN) :: nrpnts
  REAL(RKIND), INTENT(IN) ::  bgeom,amassi,rgeom,Zi_charge,Efast_in	
  REAL(RKIND), DIMENSION(nrpnts), INTENT(IN) ::  ne_in,te_in,ni_in, &
       & ti_in,grho_in,a_in,volume_in, pfast_in, &
       & q_in,s_in,etaneo_in, &
       & rho_in,btp_in, &
       & kappa_in
  REAL(RKIND), DIMENSION(3), INTENT(IN) ::  sawcoef
  !
  !     Outputs
  ! dW are the delta_W_hat defined in the relevant papers normalised such that gamma = -pi delta_W_hat/tau_alfven/s1
  !
  INTEGER, INTENT(OUT) :: indx_res,nbres,icrash
  REAL(RKIND), INTENT(OUT) :: dw_mhd, dw_fast, dw_KO, s1
  REAL(RKIND), DIMENSION(ncrits),  INTENT(OUT) :: s1_crits, gamma_res
  REAL(RKIND), DIMENSION(3),  INTENT(OUT) :: dw_th  
  !     Calculation
  INTEGER :: i, indxq1
  REAL(RKIND) ::  crho,chot,cr_resknk
  REAL(RKIND) , DIMENSION(nrpnts) :: pel, pion
  REAL(RKIND) :: omdiai1,omdiae1,tau_alf1, &
       & tau_res1,Slundqst1,rLarmori1
  REAL(RKIND) :: s1_criteff, betapB1, dum1, rhores
  REAL(RKIND) :: ni1, etaneo1, ti1, te1, a1, kappa1
  REAL(RKIND) :: dw_core,gamta_mhd,omdiah1,dw_tot,eps1, q1_inter, factlin
  REAL(RKIND) , DIMENSION(nrpnts) :: yfun
  ! Statement function to do linear interpolation on q=1 surface
  q1_inter(indxq1)=yfun(indxq1) + factlin * (yfun(indxq1+1)-yfun(indxq1))
  !
  !====VARIABLES INITIALIZATION=====================================!
  ! from inputs
  chot = 1._RKIND
  crho = 1._RKIND
  cr_resknk = 0.5_RKIND
  IF ( sawcoef(1).gt.0.0_rkind ) then
     chot=sawcoef(1)
     crho=sawcoef(2)
     cr_resknk=sawcoef(3)
  end IF
  !
  !Scalars 
  icrash=0
  nbres=0
  indx_res=0  
  !Vectors
  pel=e_charge*ne_in*te_in 
  pion=e_charge*ni_in*ti_in
  !====CHECK FOR RESONANT SURFACE================================!  
  i=nrpnts
  DO WHILE ((nbres .le. 1) .and. (i .gt. 1))
     i=i-1
     if ((q_in(i)-qreson)*(q_in(i+1)-qreson) .le. 0._rkind) then
        nbres=nbres+1
        if (nbres .eq. 1) indx_res=i
     endif
  ENDDO
  !
  ! No resonance found
  if (nbres .eq. 0) return 
  !
  !====Case for more than 1 resonant surface not allowed (yet) ===============================!
  if (nbres .gt. 1) then
     write(42,*) ' More than 1 q=1 surfaces, take the outermost'
  end if
  !
  !====Case for 1 resonant surface===============================!
  ! compute local values on rho(q=1)
  factlin = (qreson - q_in(indx_res)) / (q_in(indx_res+1)-q_in(indx_res))
  yfun=rho_in
  rhores=q1_inter(indx_res)
  !
  !
  ! Compute derivatives between i and i+1, thus "central derivative" for i+1/2 which
  ! should be close to q=1 surface, since q=1 is in between rho(i) and rho(i+1).
  !
  dum1=1._rkind/e_charge/bgeom
  omdiai1=dum1/Zi_charge*grho_in(indx_res)/ni_in(indx_res)/a_in(indx_res) &
       & * (pion(indx_res+1)-pion(indx_res))/(rho_in(indx_res+1)-rho_in(indx_res))
  omdiae1=dum1*grho_in(indx_res)/ne_in(indx_res)/a_in(indx_res) &
       & * (pel(indx_res+1)-pel(indx_res))/(rho_in(indx_res+1)-rho_in(indx_res))
  omdiai1=abs(omdiai1)
  omdiae1=abs(omdiae1)
  !
  ! Values of profiles on q=1
  !
  factlin = (rhores-rho_in(indx_res)) / (rho_in(indx_res+1)-rho_in(indx_res))
  yfun = ni_in
  ni1=q1_inter(indx_res)
  yfun = etaneo_in
  etaneo1=q1_inter(indx_res)
  yfun = ti_in
  ti1=q1_inter(indx_res)
  yfun = te_in
  te1=q1_inter(indx_res)
  yfun = a_in
  a1=q1_inter(indx_res)
  yfun = kappa_in
  kappa1=q1_inter(indx_res)
  yfun = s_in
  s1=q1_inter(indx_res) 
  yfun = btp_in
  betapB1=q1_inter(indx_res)
  !
  tau_alf1=rgeom/bgeom*(mu0*ni1*mi*amassi)**(0.5_rkind) 
  tau_res1=mu0/etaneo1*rhores**2._rkind  
  Slundqst1=tau_res1/tau_alf1 
  rLarmori1=3.235E-3_rkind*(amassi*ti1/1.E3_rkind)**(0.5_rkind)/bgeom 
  !DeltaW calculations
  ! Calculate ideal MHD internal kink growth rate gamma_ideal=-pi dw_mhd/tau_alf/s1
  eps1 = a1 / rgeom
  call saw_idmhd(eps1,kappa1, s1,betapB1,gamta_mhd)
  dw_mhd=-gamta_mhd
  ! Calculate internal resistive kink growth rateS and related formulas derived for s1crit
  if (cr_resknk .lt. 0._rkind) then
     s1_crits(1) = abs(cr_resknk)
     gamma_res(1) = -1._rkind
  else
     call saw_resknk(a1,te1,ti1,rLarmori1,s1, &
          &  Slundqst1,tau_alf1,omdiai1,omdiae1,cr_resknk,s1_crits,gamma_res)
  end if
  ! Kinetic effects: thermal trapped ions: Kruskal-Oberman contribution
  call saw_KO(indx_res,a_in,ni_in,ti_in,nrpnts,eps1,bgeom,s1,dw_KO)
  ! Fast particles dW, calculates dW_fast  & precession frequency
  call saw_fast(rgeom,bgeom,q_in,nrpnts,indx_res,a_in,eps1,pfast_in,s1,dw_fast)
  omdiah1=1.0E6_rkind*Efast_in/4._rkind/bgeom/rgeom/a_in(indx_res)
 
  !==============================================================!
  !====CHECK FOR CRITERIA========================================!
  dw_core=dw_mhd + dw_KO
  dw_tot=dw_core + dw_fast
  dw_th(1)=abs(chot*omdiah1*tau_alf1)
  dw_th(2)=abs(0.5_rkind*omdiai1*tau_alf1)
  dw_th(3)=-crho*rLarmori1/rhores
  if (-dw_core .gt. abs(chot*omdiah1*tau_alf1)) then
     !Fast particle threshold overcome
     !     icrash=2
     !     return
  endif
  dum1=abs(0.5_rkind*omdiai1*tau_alf1)
  if (-dw_tot .gt. dum1) then
     !Ideal kink is unstable
          icrash=1
          return
  endif
  if ((-dw_tot .gt. -crho*rLarmori1/rhores) .and. (-dw_tot .le. dum1)) then
     ! if criteria should be on gamma_res (if only gamma_res is known and not its function of s1), then
     ! change the criteria s1>s1crit to gamma_res > coeff_res * sqrt(omdiai1*omdiae1)
     ! if cr_resknk (=sawcoef(3)) is negative simply use condition: s1 > abs(cr_resknk)
     s1_criteff=minval(s1_crits)
     if (s1 .gt. s1_criteff) then
        icrash=3
	!Resistive kink is unstable
     endif
  endif
END SUBROUTINE saw_crit
