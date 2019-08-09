        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:08 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MACROINIT__genmod
          INTERFACE 
            SUBROUTINE MACROINIT(ICPBTDM,ICPTPWASRDM,NNCRAR,FLDRATUB,   &
     &FLENDSRPEVT,AWLCORFAC,KDCRRLREF,QEXCMTXDMCP,QINTOPLATDM,          &
     &QINTOPPREDM,QOUTDRRAPCP,SORPDMCP,THTSRPREFDMCP,TIMABSCUMDMCP,     &
     &VLMPDMCP,VLMPDYCP,WASRMPDM)
              INTEGER(KIND=4) :: ICPBTDM(20)
              INTEGER(KIND=4) :: ICPTPWASRDM(20)
              INTEGER(KIND=4) :: NNCRAR
              LOGICAL(KIND=4) :: FLDRATUB(5)
              LOGICAL(KIND=4) :: FLENDSRPEVT(20,5000)
              REAL(KIND=8) :: AWLCORFAC(5000)
              REAL(KIND=8) :: KDCRRLREF(5)
              REAL(KIND=8) :: QEXCMTXDMCP(20,5000)
              REAL(KIND=8) :: QINTOPLATDM(20)
              REAL(KIND=8) :: QINTOPPREDM(20)
              REAL(KIND=8) :: QOUTDRRAPCP(5000)
              REAL(KIND=8) :: SORPDMCP(20,5000)
              REAL(KIND=8) :: THTSRPREFDMCP(20,5000)
              REAL(KIND=8) :: TIMABSCUMDMCP(20,5000)
              REAL(KIND=8) :: VLMPDMCP(20,5000)
              REAL(KIND=8) :: VLMPDYCP(5000)
              REAL(KIND=8) :: WASRMPDM(20)
            END SUBROUTINE MACROINIT
          END INTERFACE 
        END MODULE MACROINIT__genmod
