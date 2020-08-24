MODULE params_const
  !Contains basic physical constants and some related to the model
  USE pref_const
  REAL(RKIND) ::  e_charge,mu0,mi,qreson
  INTEGER :: npnts,ncrits
  PARAMETER (npnts=70,ncrits=2)
  PARAMETER (e_charge=1.602E-19_rkind,mu0=4*pi_cst*1E-7_rkind)
  PARAMETER (mi=1.627E-27_rkind,qreson=1._rkind)
END MODULE params_const
