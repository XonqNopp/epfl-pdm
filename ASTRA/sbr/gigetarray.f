! Prints array
      subroutine GIGETARRAY(vector,name_vec,wth)
      implicit none
      include 'for/parameter.inc'
      include 'for/const.inc'
      include 'for/status.inc'
      double precision, dimension(1:NA1) ::  vector,vector2
      character*9 name_vec
      integer j,wth
      if( wth .eq. 1 ) then
        do j=2,NA1
          vector2(j)=vector(j-1)
        enddo
        vector2(1)=vector(1)
        vector2(NA1)=vector(NA1)
      else
        vector2=vector
      endif
!---- A ----!
!     write(71,'(a,f5.3,a,f5.3,a)') 'a (gridtype 11, abc=',ABC,')'
!     write(71,'(f6.4)') (AMETR(j),j=1,NA1)
!---- RHO_VOL ----!
!     write(71,'(a)') 'rho_V (gridtype 14)'
!     write(71,'(f6.4)') (SQRT((VOLUM(j)-VOLUM(1))/(VOLUM(NA1))),
!    &                   j=1,NA1)
!---- RHO_PSI ----!
!     write(71,'(a)') 'rho_psi (gridtype 13)'
!     write(71,'(f6.4)') (SQRT((FP(j)-FP(1))/(FP(NA1)-FP(1))),
!    &                   j=1,NA1)
!---- RHO_TOR ---!
      write(71,'(a)') ' rho/roc (gridtype 12) :'
      write(71,'(f6.4)') (RHO(j)/ROC,j=1,NA1)
!*** ARRAY ***!
      write(71,'(a,a,a)') ' ',name_vec,': '
      write(71,'(f10.6)') (vector2(j),j=1,NA1)
      end

