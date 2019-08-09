        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:08 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MACROINTEGRAL__genmod
          INTERFACE 
            SUBROUTINE MACROINTEGRAL(FLBEGIN,FRMPWALWET,QININTSATDMCP,  &
     &QINMTXSATDMCP,QINTOPLATDM,QINTOPPREDM,QOUTDRRAPCP,QOUTMTXSATDMCP, &
     &QOUTMTXUNSDMCP)
              LOGICAL(KIND=4) :: FLBEGIN
              REAL(KIND=8) :: FRMPWALWET(20,5000)
              REAL(KIND=8) :: QININTSATDMCP(20,5000)
              REAL(KIND=8) :: QINMTXSATDMCP(20,5000)
              REAL(KIND=8) :: QINTOPLATDM(20)
              REAL(KIND=8) :: QINTOPPREDM(20)
              REAL(KIND=8) :: QOUTDRRAPCP(5000)
              REAL(KIND=8) :: QOUTMTXSATDMCP(20,5000)
              REAL(KIND=8) :: QOUTMTXUNSDMCP(20,5000)
            END SUBROUTINE MACROINTEGRAL
          END INTERFACE 
        END MODULE MACROINTEGRAL__genmod
