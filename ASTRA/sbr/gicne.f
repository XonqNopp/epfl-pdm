! Sets the constants needed for CN
      subroutine GICNE(shot,t)
      implicit none
      include 'for/parameter.inc'
      include 'for/const.inc'
      include 'for/status.inc'
      real*8 shot ! using ZRD30X as SHOT means this is not an integer
      double precision t
      if( shot .eq. 40346 ) then ! t0=
     !!!!! 40346
           CRAD1=.95
           CRAD2=30.
           CRAD3=3.5
           CRAD4=.1
           CPEL1=2.2
      elseif( shot .eq. 40894 ) then ! t0=
     !!!!! 40894
           CRAD1=.9
           CRAD2=17.5
           CRAD3=0.
           CRAD4=.4
           CPEL1=2.2
!     elseif( shot .eq. 39863 ) then ! t0=1.3
!          CRAD1=.85
!          CRAD2=7.
!          CRAD3=.4
!          CRAD4=.25
!          CPEL1=2.2
      elseif( shot .eq. 39874 ) then
           if( t .eq. 1.3 ) then
           !!!!! 39874 SF+ t0=1.3
                 CRAD1=.87
                 CRAD2=9.2
                 CRAD3=.8
                 CRAD4=.2
                 CPEL1=2.2
           else
           !!!!! 39874 SN t0=0.5
                 CRAD1=.87
                 CRAD2=10.
                 CRAD3=1.5
                 CRAD4=.2
                 CPEL1=2.2
           endif
      elseif( shot .eq. 39863 ) then ! t0=0.86
            CRAD1=.87
            CRAD2=10.
            CRAD3=1.3
            CRAD4=.2
            CPEL1=2.2
      elseif( shot .eq. 39857 ) then ! t0=0.65
            CRAD1=.87
            CRAD2=14.
            CRAD3=0.4
            CRAD4=.2
            CPEL1=2.2
      elseif( shot .eq. 40103 ) then ! t0=1.1
!           CRAD2=14.
!           CRAD4=.2
! trying grad(Te)/Te for pedestal
            CRAD1=.57
            CRAD3=1.
            CPEL2=.75
            CPEL1=1.
      elseif( shot .eq. 40080 ) then ! t0=0.8
            CRAD2=11.
            CRAD4=.1
!           CRAD1=.8
! trying grad(Te)/Te for pedestal
            CRAD1=.72
            CRAD3=1.
            CPEL2=1.15
            CPEL1=1.
      elseif( shot .eq. 40045 ) then ! t0=1.1
            CRAD1=.87
            CRAD2=5.
            CRAD3=0.5
            CRAD4=.4
            CPEL1=2.2
      else
            write(*,*) ' Sorry, shot not implemented in GICNE...'
            write(*,*) '  * Putting values from #39874 SN.'
            CRAD1=.87
            CRAD2=10
            CRAD3=1.5
            CRAD4=.2
            CPEL1=2.2
      endif
      write(*,'(a,f6.0)') ' Setting Vn constants for shot ',shot
      write(*,'(a,f3.1,a,f4.1,a,f3.1,a,f3.1,a,f3.1)')
     &     ' CRAD1=',CRAD1,' CRAD2=',CRAD2,' CRAD3=',CRAD3,
     &     ' CRAD4=',CRAD4,' CPEL1=',CPEL1
      end
