! Sets the ELM to occur now
      subroutine GINOW(param)
      implicit none
      include 'for/parameter.inc'
      include 'for/const.inc'
      include 'for/status.inc'
      double precision param
      param=TIME
      write(*,'(a,f6.4)') '  t=',TIME
      end
