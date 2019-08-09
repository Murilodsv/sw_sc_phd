        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:54 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RAPIDDRAIN__genmod
          INTERFACE 
            SUBROUTINE RAPIDDRAIN(ID,ICPBTDM,ICPTPWASRDM,FLDRATUB,      &
     &FRMPWALWET,FRREDUQ,KDCRRLREF,VLMPDMCP,WASRMPDM,ZBTDM,ZWALEVDM,    &
     &FLWOUTDRRAPCPPOT,FLWOUTDRRAPPOT,VLMPUNDRDRL)
              INTEGER(KIND=4) :: ID
              INTEGER(KIND=4) :: ICPBTDM
              INTEGER(KIND=4) :: ICPTPWASRDM
              LOGICAL(KIND=4) :: FLDRATUB(5)
              REAL(KIND=8) :: FRMPWALWET(20,5000)
              REAL(KIND=8) :: FRREDUQ
              REAL(KIND=8) :: KDCRRLREF(5)
              REAL(KIND=8) :: VLMPDMCP(20,5000)
              REAL(KIND=8) :: WASRMPDM
              REAL(KIND=8) :: ZBTDM
              REAL(KIND=8) :: ZWALEVDM
              REAL(KIND=8) :: FLWOUTDRRAPCPPOT(5000)
              REAL(KIND=8) :: FLWOUTDRRAPPOT
              REAL(KIND=8) :: VLMPUNDRDRL
            END SUBROUTINE RAPIDDRAIN
          END INTERFACE 
        END MODULE RAPIDDRAIN__genmod
