SUBROUTINE saw_psistar(model,w_rmix,nrpnts,nres,indx_res,a_in, &
     &  bpq_in,vol_in,q_in,ne_in,ni_in,te_in,ti_in,imix, &
     &  fps_a,fps_p,ne_out,ni_out,te_out,ti_out)
  !
  ! Computes the basic quantities (Psi*, Ne, Ni, Te, Ti) after the crash, according to the model
  ! chosen (at the moment only Kadomtsev full reconnection model and an incomplete reconnection model are 
  ! implemented).
  ! Psi* is calculated as Psi*(a) = integral(Bp q*(1/q-1) da) between 0 and a
  ! where a is the minor radius.
  !
  ! model is 1 for complete reconnection and 2 for partial reconnection a la Porcelli et al. (
  ! paper above): in the case of 2 w_rmix is the island critical width parameter
  ! model 3 is free for implementation
  !
  USE pref_const
  USE params_const
  IMPLICIT NONE
  !Inputs
  INTEGER, INTENT(IN) :: nrpnts,nres,indx_res,model
  REAL(RKIND), INTENT(IN) :: w_rmix
  REAL(RKIND), DIMENSION(nrpnts), INTENT(IN) :: a_in, &
       & ne_in,ni_in,te_in,ti_in,q_in,bpq_in,vol_in
  !Outputs
  INTEGER, INTENT(OUT) :: imix
  REAL(RKIND), DIMENSION(nrpnts), INTENT(OUT) :: fps_p, &
       & fps_a,ne_out,ni_out,te_out,ti_out
  !Locals
  REAL(RKIND), DIMENSION(nrpnts) :: fps,xy1,xy2,netot,fi11
  REAL(RKIND), DIMENSION(nrpnts) :: fpi,fsi,ai, &
       & nei,nii,tei,tii,ypr2,fpo,neo,nio,teo,tio, &
       & rw,rw2,fpo2,neo2,teo2,nio2,tio2,neo1,nio1,teo1,tio1
  REAL(RKIND), DIMENSION(nrpnts) :: sigma,rk,rall,r2, &
       & r3,r4,r5,r6,fps_p2,r1,fps1
  REAL(RKIND) :: fpr,ra,dum1,tension,ybc(2),alp,bet, &
       & nint,eint,w_isl
  INTEGER, DIMENSION(nrpnts) :: mm
  INTEGER :: i,jkk,jk,iflag,nbc(2),i1,i2,i3,i4, &
       &  Isort(npnts),jp,ip2,imax, &
       &  nout,jro,imx,jmix,ii,ii2,irate,icmax
  !call system_clock(ii, irate, icmax)
  !Initialize output
  ne_out=ne_in
  ni_out=ni_in
  te_out=te_in
  ti_out=ti_in
  tension=0.0_rkind
  !Create psi_star
  fps=0._rkind
  do i=2,nrpnts
     fps(i)=fps(i-1)+ bpq_in(i)*(1._rkind/q_in(i)-1) &
          &  * (a_in(i)-a_in(i-1))
  enddo
  ! find max value and mixing radius (first value<0)
  i=2
  do while (fps(i) .gt. 0._rkind)
     if (fps(i)>fps(i-1)) imax=i
     i=i+1
  end do
  imix=i-1
  w_isl=w_rmix*a_in(imix)
  fps_a=fps 
  netot=0._rkind
  do i=2,nrpnts
     netot(i)=netot(i-1)+ne_in(i-1)*(vol_in(i)-vol_in(i-1))
  enddo
  SELECT CASE (MODEL)
     !COMPLETE RECONNECTION MODEL
  CASE (1)
     r1(1:imax)=a_in(1:imax)
     fps1(1:imax)=fps(1:imax)
     ne_out=ne_in
     ni_out=ni_in
     te_out=te_in
     ti_out=ti_in     
     nbc(1)=0
     nbc(2)=0
     ybc(1)=0._rkind
     ybc(2)=0._rkind
     tension=1E-16_rkind
     do i=imax,imix
        xy1(i)=fps(imax-i+imix)
        xy2(i)=a_in(imax-i+imix)
     enddo
     sigma(1:nrpnts)=tension
     sigma(1:imix-imax+1)=tension
     !Calculates rk
     call cbsplgen0(xy1(imax:imix),xy2(imax:imix),imix-imax+1, & 
          &   fps1(1:imax),r2(1:imax),ypr2,ypr2, &
          &   imax,0,0,sigma(1:imix-imax+1),nbc,ybc,iflag)
     rk(1:imax)=sqrt(r2(1:imax)**2._rkind-r1(1:imax)**2._rkind)
     do i=1,imax
        xy1(i)=rk(imax-i+1)
     enddo
     !Find e-i density and temperature r1
     call cbsplgen0(a_in(1:imax),ne_in(1:imax),imax, & 
          &   r1(1:imax),neo1(1:imax),ypr2,ypr2, &
          &   imax,0,0,sigma(1:imax),nbc,ybc,iflag)
     call cbsplgen0(a_in(1:imax),ni_in(1:imax),imax, & 
          &   r1(1:imax),nio1(1:imax),ypr2,ypr2, &
          &   imax,0,0,sigma(1:imax),nbc,ybc,iflag)
     call cbsplgen0(a_in(1:imax),te_in(1:imax),imax, & 
          &   r1(1:imax),teo1(1:imax),ypr2,ypr2, &
          &   imax,0,0,sigma(1:imax),nbc,ybc,iflag)
     call cbsplgen0(a_in(1:imax),ti_in(1:imax),imax, & 
          &   r1(1:imax),tio1(1:imax),ypr2,ypr2, &
          &   imax,0,0,sigma(1:imax),nbc,ybc,iflag)
     !Find e-i density and temperature r2
     call cbsplgen0(a_in(imax:imix),ne_in(imax:imix),imix-imax+1, & 
          &   r2(1:imax),neo2(1:imax),ypr2,ypr2, &
          &   imax,0,0,sigma(1:imix-imax+1),nbc,ybc,iflag)
     call cbsplgen0(a_in(imax:imix),ni_in(imax:imix),imix-imax+1, & 
          &   r2(1:imax),nio2(1:imax),ypr2,ypr2, &
          &   imax,0,0,sigma(1:imix-imax+1),nbc,ybc,iflag)
     call cbsplgen0(a_in(imax:imix),te_in(imax:imix),imix-imax+1, & 
          &   r2(1:imax),teo2(1:imax),ypr2,ypr2, &
          &   imax,0,0,sigma(1:imix-imax+1),nbc,ybc,iflag)
     call cbsplgen0(a_in(imax:imix),ti_in(imax:imix),imix-imax+1, & 
          &   r2(1:imax),tio2(1:imax),ypr2,ypr2, &
          &   imax,0,0,sigma(1:imix-imax+1),nbc,ybc,iflag)
     !Do average
     neo=0.5_rkind*(neo1+neo2)     
     teo=0.5_rkind*(teo1*neo1+teo2*neo2)/neo  
     nio=0.5_rkind*(nio1+nio2)  
     tio=0.5_rkind*(tio1*nio1+tio2*nio2)/nio  
     ip2=nrpnts-(imix-imax)
     rall(1:imax)=rk(1:imax)
     rall(imax+1:ip2)=a_in(imix+1:nrpnts)
     do i=1,imax
        fps_p2(i)=fps(i)
     enddo
     fps_p2(imax+1:ip2)=fps(imix+1:nrpnts)
     do i=1,imax
        xy1(i)=rall(imax-i+1)
        xy2(i)=fps_p2(imax-i+1)
     enddo
     do i=imax+1,ip2
        xy1(i)=rall(i)
        xy2(i)=fps_p2(i)
     enddo
     fps_p=fps
     nbc(1)=1
     tension=1.e-06_rkind
     sigma(1:ip2)=tension
     !New psistar on real grid
     call cbsplgen0(xy1(1:ip2),xy2(1:ip2),ip2,a_in, & 
          &   fps_p,ypr2,ypr2, &
          &   nrpnts,0,0,sigma(1:ip2),nbc,ybc,iflag)
     !Reinterpolates e-i density and temperature
     do i=1,imax
        xy2(i)=neo(i)
     enddo
     do i=imax+1,ip2
        xy2(i)=ne_in(imix-imax+i)
     enddo
     call cbsplgen0(xy1(1:ip2),xy2(1:ip2),ip2,a_in, & 
          &   ne_out,ypr2,ypr2, &
          &   nrpnts,0,0,sigma(1:ip2),nbc,ybc,iflag)
     do i=1,imax
        xy2(i)=teo(i)
     enddo
     do i=imax+1,ip2
        xy2(i)=te_in(imix-imax+i)
     enddo
     call cbsplgen0(xy1(1:ip2),xy2(1:ip2),ip2,a_in, & 
          &   te_out,ypr2,ypr2, &
          &   nrpnts,0,0,sigma(1:ip2),nbc,ybc,iflag)
     do i=1,imax
        xy2(i)=nio(i)
     enddo
     do i=imax+1,ip2
        xy2(i)=ni_in(imix-imax+i)
     enddo
     call cbsplgen0(xy1(1:ip2),xy2(1:ip2),ip2,a_in, & 
          &   ni_out,ypr2,ypr2, &
          &   nrpnts,0,0,sigma(1:ip2),nbc,ybc,iflag)
     do i=1,imax
        xy2(i)=tio(i)
     enddo
     do i=imax+1,ip2
        xy2(i)=ti_in(imix-imax+i)
     enddo
     call cbsplgen0(xy1(1:ip2),xy2(1:ip2),ip2,a_in, & 
          &   ti_out,ypr2,ypr2, &
          &   nrpnts,0,0,sigma(1:ip2),nbc,ybc,iflag)
     fi11=0._rkind
     do i=2,nrpnts
     fi11(i)=fi11(i-1)+ne_out(i-1)*(vol_in(i)-vol_in(i-1))
     enddo
     !do i=1,nrpnts
     !   write(*,*) a_in(i),ne_in(i),ne_out(i),netot(i),fi11(i)
     !enddo
     !stop
     !END OF COMPLETE RECONNECTION MODE
  CASE (2)
     !INCOMPLETE RECONNECTION-Porcelli,Rosenbluth
     !
     !Calculate new mixing radius from r2-r1=w/2 and psi(r1)=psi(r2)
     r1(1:imax)=a_in(1:imax)
     fps1(1:imax)=fps(1:imax)
     ne_out=ne_in
     ni_out=ni_in
     te_out=te_in
     ti_out=ti_in     
     nbc(1)=0
     nbc(2)=0
     ybc(1)=0._rkind
     ybc(2)=0._rkind
     tension=1E-16_rkind
     do i=imax,imix
        xy1(i)=fps(imix-i+imax)
        xy2(i)=a_in(imix-i+imax)
     enddo
     sigma(1:nrpnts)=tension
     sigma(1:imix-imax+1)=tension
     !Calculates i1
     call cbsplgen0(xy1(imax:imix),xy2(imax:imix),imix-imax+1, & 
          &   fps1(1:imax),r2(1:imax),ypr2,ypr2, &
          &   imax,0,0,sigma(1:imix-imax+1),nbc,ybc,iflag)
     rk(1:imax)=r2(1:imax)-r1(1:imax)
     mm=minloc(abs(rk(1:imax)-w_isl/2*r2(1:imax)/r2(1:imax)))
     i1=mm(1)
     !Calculates i2
     do i=1,imax
       xy1(i)=fps(i)
       xy2(i)=a_in(i)
     enddo
     sigma(1:imax)=tension
     call cbsplgen0(xy1(1:imax),xy2(1:imax),imax, & 
          &   fps(imax:imix),r3(1:imix-imax+1),ypr2,ypr2, &
          &   imix-imax+1,0,0,sigma(1:imax),nbc,ybc,iflag)
     r4(1:imix-imax+1)=a_in(imax:imix)-r3(1:imix-imax+1)
     mm=minloc(abs(r4(1:imix-imax+1)-w_isl/2*r1(1:imix-imax+1)/r1(1:imix-imax+1)))
     i2=mm(1)+imax-1;
     !New psistar
     fps_p(i1:i2)=fps(i2)
     fps_p(i2:)=fps(i2:)
     alp=1._rkind/(2._rkind*a_in(i1))* &
       & (fps(i1+1)-fps(i1-1))/(a_in(i1+1)-a_in(i1-1))
     bet=fps(i1)-alp*a_in(i1)**2._rkind
     fps_p(1:i1-1)=alp*(a_in(1:i1-1)**2._rkind-a_in(i1)**2._rkind)+fps_p(i1)
     r1=a_in	
     !Density and temperature are flat up to r2
     !Particle content up to r2
     nint=sum(ne_in(1:i2)*r1(1:i2)*(r1(2:i2+1)-r1(1:i2)))
     ne_out(1:i2)=(2._rkind/r1(i2)**2)*nint
     nint=sum(ni_in(1:i2)*r1(1:i2)*(r1(2:i2+1)-r1(1:i2)))
     ni_out(1:i2)=(2._rkind/r1(i2)**2)*nint
     !Energy content up to r2
     eint=sum(te_in(1:i2)*ne_in(1:i2)*r1(1:i2)*(r1(2:i2+1)-r1(1:i2)))
     te_out(1:i2)=(2._rkind/r1(i2)**2)*eint/ne_out(1:i2)
     eint=sum(ti_in(1:i2)*ni_in(1:i2)*r1(1:i2)*(r1(2:i2+1)-r1(1:i2)))
     ti_out(1:i2)=(2._rkind/r1(i2)**2)*eint/ni_out(1:i2)
     ne_out(i2+1:)=ne_in(i2+1:)
     ni_out(i2+1:)=ni_in(i2+1:)
     te_out(i2+1:)=te_in(i2+1:)
     ti_out(i2+1:)=ti_in(i2+1:)
     !END of incomplete reconnection model
  CASE (3)
     !You can implement a new model here
  stop
  END SELECT
END SUBROUTINE saw_psistar

