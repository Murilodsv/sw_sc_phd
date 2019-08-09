        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:54 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SATFLOW__genmod
          INTERFACE 
            SUBROUTINE SATFLOW(ITASK,ICPBTDM,ICPBTZON,ICPSATLEV,        &
     &ICPTPWASRDM,ICPTPZON,ID,FRMPWALWET,FRREDUQ,LEV,QINMTXSATDMCP,     &
     &QOUTMTXSATDMCP,ZWALEVDM,FLWINDMCPPOT,FLWINDMPOT,FLWOUTDMCPPOT,    &
     &FLWOUTDMPOT)
              INTEGER(KIND=4) :: ITASK
              INTEGER(KIND=4) :: ICPBTDM
              INTEGER(KIND=4) :: ICPBTZON
              INTEGER(KIND=4) :: ICPSATLEV
              INTEGER(KIND=4) :: ICPTPWASRDM
              INTEGER(KIND=4) :: ICPTPZON
              INTEGER(KIND=4) :: ID
              REAL(KIND=8) :: FRMPWALWET(20,5000)
              REAL(KIND=8) :: FRREDUQ
              REAL(KIND=8) :: LEV
              REAL(KIND=8) :: QINMTXSATDMCP(20,5000)
              REAL(KIND=8) :: QOUTMTXSATDMCP(20,5000)
              REAL(KIND=8) :: ZWALEVDM
              REAL(KIND=8) :: FLWINDMCPPOT(5000)
              REAL(KIND=8) :: FLWINDMPOT
              REAL(KIND=8) :: FLWOUTDMCPPOT(20,5000)
              REAL(KIND=8) :: FLWOUTDMPOT
            END SUBROUTINE SATFLOW
          END INTERFACE 
        END MODULE SATFLOW__genmod
