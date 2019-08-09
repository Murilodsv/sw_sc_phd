        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:20 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RDINLV__genmod
          INTERFACE 
            SUBROUTINE RDINLV(SETFLAG,VARLIS,VARLIS_MN,VARLIS_AN)
              INTEGER(KIND=4) :: VARLIS_MN
              LOGICAL(KIND=4) :: SETFLAG
              CHARACTER(*) :: VARLIS(VARLIS_MN)
              INTEGER(KIND=4) :: VARLIS_AN
            END SUBROUTINE RDINLV
          END INTERFACE 
        END MODULE RDINLV__genmod
