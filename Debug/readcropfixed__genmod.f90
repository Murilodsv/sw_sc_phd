        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:55 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE READCROPFIXED__genmod
          INTERFACE 
            SUBROUTINE READCROPFIXED(CRPFIL,PATHCROP,IDEV,LCC,TSUMEA,   &
     &TSUMAM,TBASE,KDIF,KDIR,GCTB,SWGC,CFTB,SWCF,RDTB,RDCTB,HLIM1,HLIM2U&
     &,HLIM2L,HLIM3H,HLIM3L,HLIM4,RSC,ADCRH,ADCRL,KYTB,COFAB,LOGF,      &
     &SCHEDULE,SWINTER,PFREETB,PSTEMTB,SCANOPYTB,AVPRECTB,AVEVAPTB,     &
     &CUMDENS,CHTB,ALBEDO,SWETR,FLSOLUTE,ECMAX,ECSLOP,C2ECA,C2ECB,C2ECF,&
     &NUMLAY,ALPHACRIT,SWROOTTYP,WILTPOINT,ROOTRADIUS,ROOTCOEFA,RSW)
              CHARACTER(*) :: CRPFIL
              CHARACTER(*) :: PATHCROP
              INTEGER(KIND=4) :: IDEV
              INTEGER(KIND=4) :: LCC
              REAL(KIND=8) :: TSUMEA
              REAL(KIND=8) :: TSUMAM
              REAL(KIND=8) :: TBASE
              REAL(KIND=8) :: KDIF
              REAL(KIND=8) :: KDIR
              REAL(KIND=8) :: GCTB(732)
              INTEGER(KIND=4) :: SWGC
              REAL(KIND=8) :: CFTB(732)
              INTEGER(KIND=4) :: SWCF
              REAL(KIND=8) :: RDTB(732)
              REAL(KIND=8) :: RDCTB(22)
              REAL(KIND=8) :: HLIM1
              REAL(KIND=8) :: HLIM2U
              REAL(KIND=8) :: HLIM2L
              REAL(KIND=8) :: HLIM3H
              REAL(KIND=8) :: HLIM3L
              REAL(KIND=8) :: HLIM4
              REAL(KIND=8) :: RSC
              REAL(KIND=8) :: ADCRH
              REAL(KIND=8) :: ADCRL
              REAL(KIND=8) :: KYTB(732)
              REAL(KIND=8) :: COFAB
              INTEGER(KIND=4) :: LOGF
              INTEGER(KIND=4) :: SCHEDULE
              INTEGER(KIND=4) :: SWINTER
              REAL(KIND=8) :: PFREETB(732)
              REAL(KIND=8) :: PSTEMTB(732)
              REAL(KIND=8) :: SCANOPYTB(732)
              REAL(KIND=8) :: AVPRECTB(732)
              REAL(KIND=8) :: AVEVAPTB(732)
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
              REAL(KIND=8) :: ALPHACRIT
              INTEGER(KIND=4) :: SWROOTTYP
              REAL(KIND=8) :: WILTPOINT
              REAL(KIND=8) :: ROOTRADIUS
              REAL(KIND=8) :: ROOTCOEFA
              REAL(KIND=8) :: RSW
            END SUBROUTINE READCROPFIXED
          END INTERFACE 
        END MODULE READCROPFIXED__genmod
