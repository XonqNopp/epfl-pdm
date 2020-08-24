! Scaling for W changing HE
! By G. Induni (2010)
      subroutine WSCALING(int_p_input,scaling_factor)
      implicit none
      include 'for/parameter.inc'
      include 'for/const.inc'
      include 'for/status.inc'
      double precision THQ99,NECHR,WE,WI,int_p_input,
     & tau_scal,tau_inst,ratio,scaling_factor
      double precision, dimension(1:NA1) :: we_now,wi_now
      integer j
      do j = 1,NA1
        include 'fml/we'
        we_now(j)=WE
        include 'fml/wi'
        wi_now(j)=WI
      enddo
      include 'fml/thq99'
      tau_scal=THQ99/int_p_input**0.69            ! Scaling tau_E,H98
      tau_inst=(wi_now(NA1)+we_now(NA1))/int_p_input ! Instant. tau_E
      scaling_factor=tau_inst/tau_scal
      return
      end
