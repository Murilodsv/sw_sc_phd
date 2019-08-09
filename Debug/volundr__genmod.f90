        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:54 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE VOLUNDR__genmod
          INTERFACE 
            FUNCTION VOLUNDR(ID,ICPBTDM,LEVEL,VLMPDMCP,ZBTDM)
              INTEGER(KIND=4) :: ID
              INTEGER(KIND=4) :: ICPBTDM
              REAL(KIND=8) :: LEVEL
              REAL(KIND=8) :: VLMPDMCP(20,5000)
              REAL(KIND=8) :: ZBTDM
              REAL(KIND=8) :: VOLUNDR
            END FUNCTION VOLUNDR
          END INTERFACE 
        END MODULE VOLUNDR__genmod
