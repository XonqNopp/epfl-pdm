! Prints scalar
      subroutine GIGETSCAL(scalar,name_scal)
      implicit none
      double precision scalar
      character*10 name_scal
      write(*,'(a,a,a,f8.6)') ' ',name_scal,': ',scalar
      end
