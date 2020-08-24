C  Interface ASTRA-sawmod.f

C Emiliano 2005

      SUBROUTINE ASTSAW(OPTION,X1,X2,X3,X4)
      implicit none
      include 'for/parameter.inc'              
      include 'for/const.inc'                
      include 'for/status.inc'      
      real*8 ne_i(1:NA1),te_i(1:NA1),ni_i(1:NA1),ti_i(1:NA1),
     1 		grho_i(1:NA1),a_i(1:NA1),q_i(1:NA1),s_i(1:NA1),
     2		eta_i(1:NA1),psi_i(1:NA1),rho_i(1:NA1),j_i(1:NA1),
     3		ne_o(1:NA1),te_o(1:NA1),ni_o(1:NA1),ti_o(1:NA1),
     4 		q_o(1:NA1),s_o(1:NA1),a_o(1:NA1),grho_o(1:NA1),
     5          g3_i(1:NA1),bpq_i(1:NA1),jj2(1:NA1),iplt(1:NA1)
      real*8 eta_o(1:NA1),psi_o(1:NA1),rho_o(1:NA1),
     1		phi_i(1:NA1),g2_i(1:NA1),pc_i(1:NA1),vp_i(1:NA1),
     2		k_i(1:NA1),d_i(1:NA1),btp_i(1:NA1),j_o(1:NA1),
     3		volume_i(1:NA1),iprho_i(1:NA1),ptot_i(1:NA1),
     4		pfast_i(1:NA1),fps_a(1:NA1),bp_o(1:NA1),bp_i(1:NA1),
     5          bpb(1:NA1),pp(1:NA1),gg22(1:NA1),jj1(1:NA1),
     6 		bpc(1:NA1),pn(1:NA1),jj3(1:NA1),jj4(1:NA1),spsi(1:NA1)
      integer	k,jmax,OPTION,J,BETPV,WTOT,i,WE,WI,ITOT,merd
      real*8 zi,M,zbtor,zrtor,zpi,s1,dum1,dum2
      real*8  X1,X2,X3,X4,t,dum3
      real*8 ikcrit,rcrit,fcrit,rres,mu0,efast_in, 
     1   fps_p(1:NA1),dphi(1:NA1),fps(1:NA1),
     2   dwfp,dwrk,dwik,shro,shet(1:2),shcr1(1:2),gamms(1:2),
     3   rkuth,rklth,fpthrs,ikthrs,sawcoef(1:3),dwth(1:3)
      integer jres,jmix,icrash,nres,istep
      double precision TMIX
      data istep/0/
      save istep
      save TMIX
      data TMIX/-99999./
      istep=istep+1
      if (TMIX .lt. -99998.) TMIX = TSTART
!      if (istep .lt. 2) return
      !Define inputs
      t=TIME
      jmax=NA1
      icrash=0
      zi=ZMJ
      zpi=3.141592D0
      M=AMJ
      zbtor=BTOR
      zrtor=RTOR
      mu0=4*zpi*1e-7
      do J=1,jmax
	  ne_i(J)=NE(J)*1.D19
	  te_i(J)=TE(J)*1.D3
	  ni_i(J)=NI(J)*1.D19
	  ti_i(J)=TI(J)*1.D3
 	  grho_i(J)=GRADRO(J)
	  a_i(J)=AMETR(J)
	  q_i(J)=1.D0/MU(J)
	  pc_i(J)=IPOL(J)
	  g2_i(J)=G22(J)
	  gg22(J)=G22(J)/zrtor*IPOL(J)
	  g3_i(J)=G33(J)
	  vp_i(J)=VR(J)
	  volume_i(J)=VOLUM(J)
	  ptot_i(J)=0.D0
	  pfast_i(J)=0.D0
	  s_i(J)=SHEAR(J)
	  eta_i(J)=1.D0/CC(J)/1.D6
	  psi_i(J)=FP(J)
	  spsi(J)=FP(J)/(abs(FP(J))+1E-6)
	  rho_i(J)=RHO(J)
	  j_i(J)=CU(J)*1.D6
	  phi_i(J)=zpi*zbtor*rho_i(J)**2.D0
	  k_i(J)=ELON(J)
	  d_i(J)=TRIA(J)
	  include 'fml/betpv'
	  include 'fml/itot'
	  include 'fml/we'
	  include 'fml/wi'
	  btp_i(J)=BETPV
	  iplt(J)=ITOT
      enddo
      do i=1,jmax
         bpq_i(i)=zbtor*rho_i(i)/zrtor
      enddo
      sawcoef(1)=X1
      sawcoef(2)=X2
      sawcoef(3)=X3
      efast_in=0.D0
!			write(*,*) sawcoef(1),sawcoef(2),sawcoef(3)
      call saw_crit(zrtor,abs(zbtor),zi,M,jmax,rho_i,ne_i,te_i,ni_i, 
     1   ti_i,pfast_i,efast_in,btp_i,a_i,q_i,s_i,grho_i,volume_i, 
     2   eta_i,k_i,sawcoef,icrash,nres,jres,s1,shcr1,gamms,
     3   dwik,dwrk,dwfp,dwth)
!      write(*,*) s1,shcr1
!      call print_things(t,shcr1(1),shcr1(2),dwik,dwfp,dwrk,
!     2   icrash)
!      if (t.le.1) then
      if (t.le.10) then
      write(17,*) t,s1,shcr1(1),shcr1(2),dwik,dwrk,dwfp,
     1   dwth(1),dwth(2),dwth(3),nres,icrash
      endif
      if (icrash.eq.0) return !No crash needed
      if (nres.gt.1) return !If more than 1 q=1 return
!      if (jres.lt.1) return !too central sawteeth
!			if (s1.lt.0.2) return
      call saw_psispar(OPTION,X4, 
     1   jmax,nres,jres,a_i,volume_i,  
     2   abs(phi_i),abs(psi_i),q_i,ne_i,ni_i,te_i,ti_i,jmix,  
     3   fps_a,fps_p,ne_o,ni_o,te_o,ti_o)
!			call saw_psistar(OPTION,X4, 
!     1   jmax,nres,jres,a_i,abs(bpq_i),volume_i,  
!     2   q_i,ne_i,ni_i,te_i,ti_i,jmix,  
!     3   fps_a,fps_p,ne_o,ni_o,te_o,ti_o)
      call postcrash(jmax,jmix,
     1  fps_p,q_i,abs(psi_i),abs(phi_i),
     2  q_o,psi_o)
!      do J=1,jmax
!           write(*,*) a_i(J),phi_i(J),psi_i(J),psi_o(J)
!      enddo
!      stop
      CFUS2=RHO(jres)/ROC
      CNBI1=jres
      do J=1,jmax
	if (LEQ(1).gt.0)  NE(J)=ne_o(J)/1.D19
	if (LEQ(1).gt.0)  NI(J)=ni_o(J)/1.D19
	if (LEQ(2).gt.0)  TE(J)=te_o(J)/1.D3
	if (LEQ(3).gt.0)  TI(J)=ti_o(J)/1.D3
        MU(J)=1.D0/q_o(J)
        FP(J)=spsi(J)*psi_o(J)
!        write(*,*) a_i(J)/a_i(jmax),fps_a(J),fps_p(J)
			 enddo
      end
