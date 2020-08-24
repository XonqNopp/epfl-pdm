! Sets the constants needed for HE
      subroutine GICTE(shot,t)
      implicit none
      include 'for/parameter.inc'
      include 'for/const.inc'
      include 'for/status.inc'
      real*8 shot ! using ZRD30X as SHOT means this is not an integer
      double precision t
      if( shot .eq. 40346 ) then ! t0=
           CNB1=2.
           CNB2=6.
           CNB3=.95
           CNB4=1
           CHI4=1
           CHE3=1
           CHE4=.2
      elseif( shot .eq. 40894 ) then ! t0=
           CNB1=1.
           CNB2=4.
           CNB3=.9
           CNB4=2.2
           CHI4=2.2
           CHE3=.9
           CHE4=.25
!     elseif( shot .eq. 39863 ) then ! t0=1.3
!          CNB1=6.
!          CNB2=3.
!          CNB3=.9
!          CNB4=2.5
!          CHI4=2.
!          CHE3=1.
!          CHE4=.2
      elseif( shot .eq. 39874 ) then
           if( t .eq. 1.3 ) then
           !!!!! 39874 SF+
                 CNB1=6.
                 CNB2=3.
                 CNB3=.9
                 CNB4=2.2
                 CHI4=1.3
                 CHE3=1.
                 CHE4=.2
           else
            write(*,*) 'OK no1'
           !!!!! 39874 SN
                 CNB1=6.
                 CNB2=3.
                 CNB3=.9
                 CNB4=2.5
                 CHI4=3
                 CHE3=1.
                 CHE4=.2
           endif
      elseif( shot .eq. 39863 ) then ! t0=0.86
            CNB1=1.
            CNB2=2.
            CNB3=.9
            CNB4=5.
            CHI4=3.
            CHE3=1.
            CHE4=.2
      elseif( shot .eq. 39857 ) then ! t0=0.6
            CNB1=4.
            CNB2=3.
            CNB3=.95
            CNB4=3.
            CHI4=4.3
            CHE3=1.
            CHE4=.4
      elseif( shot .eq. 40103 ) then ! t0=1.1
            CNB1=1.
            CNB2=2.
            CNB3=.9
            CNB4=1.
            CHI4=.8
            CHE3=1.
            CHE4=.2
      elseif( shot .eq. 40080 ) then ! t0=0.8
            CNB1=1.
            CNB2=2.
            CNB3=.9
            CNB4=1.
            CHI4=.8
            CHE3=1.
            CHE4=.2
            CV5=0.75
      elseif( shot .eq. 40045 ) then ! t0=1.1
            CNB1=6.
            CNB2=3.
            CNB3=.9
            CNB4=2.5
            CHI4=.5
            CHE3=1.
            CHE4=.2
            ! Replacing edge X2 by central X3
            CF5=0.6
      else
            write(*,*) ' Sorry, shot not implemented in GICTE...'
            write(*,*) '  * Putting values from #39874 SN.'
            CNB1=6.
            CNB2=3.
            CNB3=.9
            CNB4=2.5
            CHI4=3
            CHE3=1.
            CHE4=.2
      endif
      write(*,'(a,f6.0)') ' Setting chie constants for shot ',shot
      write(*,'(a,f3.1,a,f3.1,a,f3.1,a,f3.1,a,f3.1,a,f3.1,
     &          a,f3.1)')
     &     ' CNB1=',CNB1,' CNB2=',CNB2,' CNB3=',CNB3,' CNB4=',CNB4,
     &     ' CHI4=',CHI4,' CHE3=',CHE3,' CHE4=',CHE4
      end
