        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:14 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTBMA__genmod
          INTERFACE 
            SUBROUTINE OUTBMA(TASK,BMA,OUTFIL,PATHWORK,Z,DZ,NUMNOD,     &
     &IOUTDAT,T1900,OUTDAT,TSTART,PROJECT,CQMPININTSATDM1,              &
     &CQMPININTSATDM2,CQMPINMTXSATDM1,CQMPINMTXSATDM2,CQMPOUTDRRAP,     &
     &CQMPOUTMTXSATDM1,CQMPOUTMTXSATDM2,CQMPOUTMTXUNSDM1,               &
     &CQMPOUTMTXUNSDM2,CQMPINTOPPREDM1,CQMPINTOPPREDM2,CQMPINTOPLATDM1, &
     &CQMPINTOPLATDM2,WASRDM1,WASRDM1INI,WASRDM2,WASRDM2INI)
              INTEGER(KIND=4) :: TASK
              INTEGER(KIND=4) :: BMA
              CHARACTER(*) :: OUTFIL
              CHARACTER(*) :: PATHWORK
              REAL(KIND=8) :: Z(5000)
              REAL(KIND=8) :: DZ(5000)
              INTEGER(KIND=4) :: NUMNOD
              INTEGER(KIND=4) :: IOUTDAT
              REAL(KIND=8) :: T1900
              REAL(KIND=8) :: OUTDAT(3000)
              REAL(KIND=8) :: TSTART
              CHARACTER(*) :: PROJECT
              REAL(KIND=8) :: CQMPININTSATDM1
              REAL(KIND=8) :: CQMPININTSATDM2
              REAL(KIND=8) :: CQMPINMTXSATDM1
              REAL(KIND=8) :: CQMPINMTXSATDM2
              REAL(KIND=8) :: CQMPOUTDRRAP
              REAL(KIND=8) :: CQMPOUTMTXSATDM1
              REAL(KIND=8) :: CQMPOUTMTXSATDM2
              REAL(KIND=8) :: CQMPOUTMTXUNSDM1
              REAL(KIND=8) :: CQMPOUTMTXUNSDM2
              REAL(KIND=8) :: CQMPINTOPPREDM1
              REAL(KIND=8) :: CQMPINTOPPREDM2
              REAL(KIND=8) :: CQMPINTOPLATDM1
              REAL(KIND=8) :: CQMPINTOPLATDM2
              REAL(KIND=8) :: WASRDM1
              REAL(KIND=8) :: WASRDM1INI
              REAL(KIND=8) :: WASRDM2
              REAL(KIND=8) :: WASRDM2INI
            END SUBROUTINE OUTBMA
          END INTERFACE 
        END MODULE OUTBMA__genmod
