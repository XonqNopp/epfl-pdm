! Scaling for W_ped
! By G. Induni (2010)
      subroutine PEDSCALING(back,do_scaling,rho_in,corr_coeff)
      implicit none
      include 'for/parameter.inc'
      include 'for/const.inc'
      include 'for/status.inc'
      real*8, dimension(1:NA1) :: we_now,wi_now,rho_vol
      double precision wtot_now,w_ped,w_core,wcoreped_scal,
     & WE,WI,rho_ped,back,new_factor,do_scaling,
     & rho_in,corr_coeff
      integer j,irho_ped
      if( back .eq. 0 ) then ! initializing
        back=1.
      endif
      ! Storing WE and WI 'profiles'
      do j = 1,NA1
        include 'fml/we'
        we_now(j)=WE
        include 'fml/wi'
        wi_now(j)=WI
      enddo

      rho_vol=SQRT(VOLUM/MAXVAL(VOLUM))
      ! Rounded index of pedestal position
      irho_ped=MINLOC(ABS(rho_vol-rho_in),1)
      rho_ped=rho_vol(irho_ped)

      ! Total energy
      wtot_now=we_now(NA1)+wi_now(NA1)
      ! Core energy
      w_core=we_now(irho_ped)+wi_now(irho_ped)
      ! Pedestal energy
      w_ped=wtot_now-w_core

      ! Core to pedestal scaling
      wcoreped_scal=3.5*corr_coeff

      write(19,'(a,f7.5,a,i2,a,f5.3,a,f4.1,a,f5.2,a,f5.2)')
     &     't=',TIME,' irho=',irho_ped,' rhovol_ped=',rho_ped,
     &     ' ',wcoreped_scal,'=',w_core/w_ped,' fac=',back
      if( do_scaling .ne. 0 ) then
        new_factor=(w_core/w_ped)/wcoreped_scal
        back=MAX(back/new_factor,0.01)
      endif
      return
      end
