      SUBROUTINE WOUTP

      implicit none
      
      integer j,shot
!     real*8 t,t0
      real*8 t0
      double precision t
      real*8 POH,WE,RHOS,LTE,LNE,CNSA,CCSP,NUEE,COULG,FTLLMR
      real*8 ROTSH,OMDE,OMDI,VDIA,VDIE,CS,RLS,WI,PEICL
      real*8 NUES,HCSA,DCSA,XCSA,BETPL,TPF,NUI,IBS,ICD,ITOT,IOHM
      real*8 HCEE,HCEI,ZFTI3,ZFTI4,NUIS,ALP,A1,A0,ZFTI2
      real*8 ZDF,ZFTE,ZZ,ZFT,ZFTE4,ZFTI,ZFTE2,ZFTE3
      real*8 zpoh(100),zwe(100),zcnsa(100)
      real*8 zlte(100),zlne(100),zrhos(100),zlint(100)
      real*8 zrotsh(100),znuee(100),zomdi(100),zomde(100)
      real*8 zcs(100),zrls(100),zwi(100),zpeicl(100),zcoulg(100)
      real*8 znues(100),zhcsa(100),zdcsa(100)
      real*8 zxcsa(100),zbetpl(100),znuis(100)
      real*8 zibs(100),zicd(100),ziohm(100),zitot(100)
      character*180 filed,direc
      character*180 pollo
      character*10 wfmt
      
      include 'for/parameter.inc'
      include 'for/const.inc'
      include 'for/status.inc'

      ! EXTRA STUFF TO CALCULATE
      do j=1,NA1
      include 'fml/we'
      zwe(j)=WE
      include 'fml/rhos'
      zrhos(j)=RHOS
      include 'fml/lte'
      zlte(j)=LTE
      include 'fml/poh'
      zpoh(j)=POH
      include 'fml/lne'
      zlne(j)=LNE
      include 'fml/rotsh'
      zrotsh(j)=ROTSH
      include 'fml/nuee'
      znuee(j)=NUEE
      include 'fml/omdi'
      zomdi(j)=OMDI
      include 'fml/cs'
      zcs(j)=CS
      include 'fml/rls'
      zrls(j)=RLS
      include 'fml/omde'
      zomde(j)=OMDE
      include 'fml/wi'
      zwi(j)=WI
      include 'fml/peicl'
      zpeicl(j)=PEICL
      include 'fml/coulg'
      zcoulg(j)=COULG
      include 'fml/nues'
      znues(j)=NUES
      include 'fml/hcsa'
      zhcsa(j)=HCSA
      include 'fml/dcsa'
      zdcsa(j)=DCSA
      include 'fml/xcsa'
      zxcsa(j)=XCSA
      include 'fml/betpl'
      zbetpl(j)=BETPL
      include 'fml/nuis'
      znuis(j)=NUIS
      include 'fml/iohm'
      ziohm(j)=IOHM
      include 'fml/icd'
      zicd(j)=ICD
      include 'fml/ibs'
      zibs(j)=IBS
      include 'fml/itot'
      zitot(j)=ITOT
      end do 

      shot=ZRD30X
      t=TIME
      t0=ZRD13X
   
      write(direc,'(A)') 'dirresults'
      open(32,file='dirresults')
      read(32,'(A)') pollo
      close(32)
      do j=1,80
         if (pollo(j:j) .eq. ' ') go to 100
      end do
    
 100  write(filed,'(A,A,i5,A,F9.7,A)') pollo(1:j-1),'tcvsim_',shot,'_',
     & t,'.dat'
      open(32,file=filed)

      wfmt="(ES17.8E3)"
      !write(*,*) NA1 ! for diagnostic purposes
      !Miscellaneous parameters    
      write(32,wfmt) t0
      write(32,wfmt) t
      write(32,wfmt) IPL
      write(32,wfmt) ZEF(1)
      write(32,wfmt) RTOR
      write(32,wfmt) BTOR
      write(32,wfmt) SHIFT
      write(32,wfmt) UPDWN
      ! ============================
      ! ==== Profile parameters ====
      ! ============================
      ! rho-related parameters
      write(32,wfmt)(FP(j),j=1,NA1) !Psi
      write(32,wfmt)(RHO(j),j=1,NA1) !Radial variable toroidal flux 
      write(32,wfmt)(AMETR(j),j=1,NA1) !Radial variable minor radius    
      write(32,wfmt)(VOLUM(j),j=1,NA1) !Volum
      write(32,wfmt)(VR(j),j=1,NA1) !Vprime
      ! Plasma profile quantities
      ! Heat and density
      write(32,wfmt)(TE(j),j=1,NA1) !Electron temperature    
      write(32,wfmt)(TI(j),j=1,NA1) !Ion temperature    
      write(32,wfmt)(NE(j),j=1,NA1) !Electron density    
      write(32,wfmt)(NI(j),j=1,NA1) !Ion density    
      write(32,wfmt)(TEX(j),j=1,NA1) !TEX
      write(32,wfmt)(TIX(j),j=1,NA1) !TIX
      write(32,wfmt)(NEX(j),j=1,NA1) !NEX
      write(32,wfmt)(NIX(j),j=1,NA1) !NIX
      write(32,wfmt)(QE(j),j=1,NA1) !Electron heat flow
      ! Current
      write(32,wfmt)(CU(j),j=1,NA1) !Parallel current density    
      write(32,wfmt)(CUBS(j),j=1,NA1) !Bootstrap
      write(32,wfmt)(CD(j),j=1,NA1) !ECCD Current density    
      write(32,wfmt)(MU(j),j=1,NA1) !Rotational transform    
      write(32,wfmt)(SHEAR(j),j=1,NA1) !Magnetic shear
      write(32,wfmt)(MUX(j),j=1,NA1) !MUX
      write(32,wfmt)(UPL(j),j=1,NA1) !Loop voltage 
      write(32,wfmt)(ULON(j),j=1,NA1) !Parallel loop voltage
      write(32,wfmt)(IPOL(j),j=1,NA1) !Normalized poloidal current 
      write(32,wfmt)(ziohm(j),j=1,NA1) !IOHM
      write(32,wfmt)(zicd(j),j=1,NA1) !ICD
      write(32,wfmt)(zibs(j),j=1,NA1) !IBS
      write(32,wfmt)(zitot(j),j=1,NA1) !ITOT
      ! Transport coefficients
      write(32,wfmt)(CAR5X(j),j=1,NA1) !HEX
      write(32,wfmt)(CN(j),j=1,NA1) !Ware pinch
      write(32,wfmt)(DN(j),j=1,NA1) !Density transport coefficient
      write(32,wfmt)(QN(j),j=1,NA1) !Particle flux
      write(32,wfmt)(HE(j),j=1,NA1) !Heat transport coefficient HE
      write(32,wfmt)(XI(j),j=1,NA1) !Ion heat transport anomalous+neoclassical
      write(32,wfmt)(CC(j),j=1,NA1) !Conductivity
      write(32,wfmt)(zhcsa(j),j=1,NA1) !HCSA
      write(32,wfmt)(zdcsa(j),j=1,NA1) !DCSA
      write(32,wfmt)(zxcsa(j),j=1,NA1) !XCSA
      ! Sources
      write(32,wfmt)(PE(j),j=1,NA1) !Electron Power deposition    
      write(32,wfmt)(PI(j),j=1,NA1) !Total ion power density
      write(32,wfmt)(PEECR(j),j=1,NA1) ! ECRH    
      write(32,wfmt)(zpoh(j),j=1,NA1) !P_Ohm    
      write(32,wfmt)(SNN(j),j=1,NA1) !Source of density: SN=SNN*NE	
      ! Geometry
      write(32,wfmt)(SHIF(j),j=1,NA1) !Shafranov shift of the surfaces
      write(32,wfmt)(ELON(j),j=1,NA1) !Elongation of the surfaces
      write(32,wfmt)(TRIA(j),j=1,NA1) !Triangularity of the surfaces
      write(32,wfmt)(SHIV(j),j=1,NA1) !Vertical displacement of the surfaces
      write(32,wfmt)(SLAT(j),j=1,NA1) !Surface area
      write(32,wfmt)(SQEPS(j),j=1,NA1) !Sqrt of the aspect ratio
      write(32,wfmt)(G11(j),j=1,NA1) !Metric <|grad(rho)|> V' 
      write(32,wfmt)(G22(j),j=1,NA1) !G2=V'*R0/(4*pi^2J)<(Drho/r)^2>
      write(32,wfmt)(G33(j),j=1,NA1) !G3=<R0^2/r^2>
      write(32,wfmt)(GRADRO(j),j=1,NA1) !<grad(rho)>
      !Other
      write(32,wfmt)(zbetpl(j),j=1,NA1) !Beta poloidal local
      write(32,wfmt)(zrhos(j),j=1,NA1) !Rhostar     
      write(32,wfmt)(zlte(j),j=1,NA1) !Temperature scale length        
      write(32,wfmt)(zlne(j),j=1,NA1) !Normalized density gradient
      write(32,wfmt)(CAR1(j),j=1,NA1) !Electron energy confinement time        
      write(32,wfmt)(CAR2(j),j=1,NA1) !Ion energy confinement time
      write(32,wfmt)(CAR3(j),j=1,NA1) !Particle confinement time        
      write(32,wfmt)(ER(j),j=1,NA1) !Radial electric field Er    
      write(32,wfmt)(zwe(j),j=1,NA1) !Electron energy W_e
      write(32,wfmt)(zwi(j),j=1,NA1) !Ion energy
      write(32,wfmt)(zpeicl(j),j=1,NA1) !Classical equipartition energy
      write(32,wfmt)(CAR21(j),j=1,NA1) !Internal inductance
      write(32,wfmt)(zrotsh(j),j=1,NA1) !Rotational EXB shear
      write(32,wfmt)(VTOR(j),j=1,NA1) !Toroidal rotation velocity
      write(32,wfmt)(znuee(j),j=1,NA1) !Electron-ion collision frequency
      write(32,wfmt)(zomde(j),j=1,NA1) !Electron diamagnetic frequency
      write(32,wfmt)(zomdi(j),j=1,NA1) !Ion diamagnetic frequency
      write(32,wfmt)(zcs(j),j=1,NA1) !Sound velocity
      write(32,wfmt)(zrls(j),j=1,NA1) !Ion Sound larmor radius
      write(32,wfmt)(CAR28(j),j=1,NA1) !Trapped particle fraction
      write(32,wfmt)(zcoulg(j),j=1,NA1) !Coulomb logaritm
      write(32,wfmt)(znues(j),j=1,NA1) !Nu*
      write(32,wfmt)(znuis(j),j=1,NA1) !NuI star
      close(32)  
      !Close file
      return
      end

     
