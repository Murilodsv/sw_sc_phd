        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:12 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE READWOFOST__genmod
          INTERFACE 
            SUBROUTINE READWOFOST(CRPFIL,PATHCROP,SWCF,CFTB,IDSL,DLO,DLC&
     &,TSUMEA,TSUMAM,DTSMTB,DVSEND,TDWI,LAIEM,RGRLAI,SLATB,SPA,SSA,SPAN,&
     &TBASE,KDIF,KDIR,EFF,AMAXTB,TMPFTB,TMNFTB,CVL,CVO,CVR,CVS,Q10,RML, &
     &RMO,RMR,RMS,RFSETB,FRTB,FLTB,FSTB,FOTB,PERDL,RDRRTB,RDRSTB,HLIM1, &
     &HLIM2U,HLIM2L,HLIM3H,HLIM3L,HLIM4,RSC,ADCRH,ADCRL,COFAB,RDI,RRI,  &
     &RDC,RDCTB,LOGF,SCHEDULE,CUMDENS,CHTB,ALBEDO,SWETR,FLSOLUTE,ECMAX, &
     &ECSLOP,C2ECA,C2ECB,C2ECF,NUMLAY,RELNI,CFET,ALPHACRIT,SWROOTTYP,   &
     &WILTPOINT,ROOTRADIUS,ROOTCOEFA,RSW)
              CHARACTER(*) :: CRPFIL
              CHARACTER(*) :: PATHCROP
              INTEGER(KIND=4) :: SWCF
              REAL(KIND=8) :: CFTB(732)
              INTEGER(KIND=4) :: IDSL
              REAL(KIND=8) :: DLO
              REAL(KIND=8) :: DLC
              REAL(KIND=8) :: TSUMEA
              REAL(KIND=8) :: TSUMAM
              REAL(KIND=8) :: DTSMTB(30)
              REAL(KIND=8) :: DVSEND
              REAL(KIND=8) :: TDWI
              REAL(KIND=8) :: LAIEM
              REAL(KIND=8) :: RGRLAI
              REAL(KIND=8) :: SLATB(30)
              REAL(KIND=8) :: SPA
              REAL(KIND=8) :: SSA
              REAL(KIND=8) :: SPAN
              REAL(KIND=8) :: TBASE
              REAL(KIND=8) :: KDIF
              REAL(KIND=8) :: KDIR
              REAL(KIND=8) :: EFF
              REAL(KIND=8) :: AMAXTB(30)
              REAL(KIND=8) :: TMPFTB(30)
              REAL(KIND=8) :: TMNFTB(30)
              REAL(KIND=8) :: CVL
              REAL(KIND=8) :: CVO
              REAL(KIND=8) :: CVR
              REAL(KIND=8) :: CVS
              REAL(KIND=8) :: Q10
              REAL(KIND=8) :: RML
              REAL(KIND=8) :: RMO
              REAL(KIND=8) :: RMR
              REAL(KIND=8) :: RMS
              REAL(KIND=8) :: RFSETB(30)
              REAL(KIND=8) :: FRTB(30)
              REAL(KIND=8) :: FLTB(30)
              REAL(KIND=8) :: FSTB(30)
              REAL(KIND=8) :: FOTB(30)
              REAL(KIND=8) :: PERDL
              REAL(KIND=8) :: RDRRTB(30)
              REAL(KIND=8) :: RDRSTB(30)
              REAL(KIND=8) :: HLIM1
              REAL(KIND=8) :: HLIM2U
              REAL(KIND=8) :: HLIM2L
              REAL(KIND=8) :: HLIM3H
              REAL(KIND=8) :: HLIM3L
              REAL(KIND=8) :: HLIM4
              REAL(KIND=8) :: RSC
              REAL(KIND=8) :: ADCRH
              REAL(KIND=8) :: ADCRL
              REAL(KIND=8) :: COFAB
              REAL(KIND=8) :: RDI
              REAL(KIND=8) :: RRI
              REAL(KIND=8) :: RDC
              REAL(KIND=8) :: RDCTB(22)
              INTEGER(KIND=4) :: LOGF
              INTEGER(KIND=4) :: SCHEDULE
              REAL(KIND=8) :: CUMDENS(202)
              REAL(KIND=8) :: CHTB(732)
              REAL(KIND=8) :: ALBEDO
              INTEGER(KIND=4) :: SWETR
              LOGICAL(KIND=4) :: FLSOLUTE
              REAL(KIND=8) :: ECMAX
              REAL(KIND=8) :: ECSLOP
              REAL(KIND=8) :: C2ECA
              REAL(KIND=8) :: C2ECB
              REAL(KIND=8) :: C2ECF(200)
              INTEGER(KIND=4) :: NUMLAY
              REAL(KIND=8) :: RELNI
              REAL(KIND=8) :: CFET
              REAL(KIND=8) :: ALPHACRIT
              INTEGER(KIND=4) :: SWROOTTYP
              REAL(KIND=8) :: WILTPOINT
              REAL(KIND=8) :: ROOTRADIUS
              REAL(KIND=8) :: ROOTCOEFA
              REAL(KIND=8) :: RSW
            END SUBROUTINE READWOFOST
          END INTERFACE 
        END MODULE READWOFOST__genmod
