        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:29 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CHECKMASSBAL__genmod
          INTERFACE 
            SUBROUTINE CHECKMASSBAL(DAYCUM,NRLEVS,NUMNODNEW,SWMACRO,    &
     &OUTFIL,PATHWORK,FLOPENFILEDEV,DZNEW,CRITDEVMASBAL,IEVAP,IGIRD,    &
     &IGRAI,IGSNOW,INIRD,INRAI,INQDRANEW,IQEXCMTXDM1CPNEW,              &
     &IQEXCMTXDM2CPNEW,INQNEW,IQOUTDRRAPCPNEW,INQROTNEW,IPONDBEG,       &
     &IQINTOPPREDM1,IQINTOPLATDM1,IQINTOPPREDM2,IQINTOPLATDM2,ISSNOWBEG,&
     &ITHETABEGNEW,IRUNO,IRUNON,ISNRAI,ISUBL,POND,SSNOW,THETANEW,       &
     &IWASRDM1BEG,IWASRDM2BEG,WASRDM1,WASRDM2)
              INTEGER(KIND=4) :: DAYCUM
              INTEGER(KIND=4) :: NRLEVS
              INTEGER(KIND=4) :: NUMNODNEW
              INTEGER(KIND=4) :: SWMACRO
              CHARACTER(*) :: OUTFIL
              CHARACTER(*) :: PATHWORK
              LOGICAL(KIND=4) :: FLOPENFILEDEV
              REAL(KIND=8) :: DZNEW(5000)
              REAL(KIND=8) :: CRITDEVMASBAL
              REAL(KIND=8) :: IEVAP
              REAL(KIND=8) :: IGIRD
              REAL(KIND=8) :: IGRAI
              REAL(KIND=8) :: IGSNOW
              REAL(KIND=8) :: INIRD
              REAL(KIND=8) :: INRAI
              REAL(KIND=8) :: INQDRANEW(5,5000)
              REAL(KIND=8) :: IQEXCMTXDM1CPNEW(5000)
              REAL(KIND=8) :: IQEXCMTXDM2CPNEW(5000)
              REAL(KIND=8) :: INQNEW(5001)
              REAL(KIND=8) :: IQOUTDRRAPCPNEW(5000)
              REAL(KIND=8) :: INQROTNEW(5000)
              REAL(KIND=8) :: IPONDBEG
              REAL(KIND=8) :: IQINTOPPREDM1
              REAL(KIND=8) :: IQINTOPLATDM1
              REAL(KIND=8) :: IQINTOPPREDM2
              REAL(KIND=8) :: IQINTOPLATDM2
              REAL(KIND=8) :: ISSNOWBEG
              REAL(KIND=8) :: ITHETABEGNEW(5000)
              REAL(KIND=8) :: IRUNO
              REAL(KIND=8) :: IRUNON
              REAL(KIND=8) :: ISNRAI
              REAL(KIND=8) :: ISUBL
              REAL(KIND=8) :: POND
              REAL(KIND=8) :: SSNOW
              REAL(KIND=8) :: THETANEW(5000)
              REAL(KIND=8) :: IWASRDM1BEG
              REAL(KIND=8) :: IWASRDM2BEG
              REAL(KIND=8) :: WASRDM1
              REAL(KIND=8) :: WASRDM2
            END SUBROUTINE CHECKMASSBAL
          END INTERFACE 
        END MODULE CHECKMASSBAL__genmod
