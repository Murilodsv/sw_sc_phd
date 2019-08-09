        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:08 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DTSYS__genmod
          INTERFACE 
            SUBROUTINE DTSYS(ITASK,DATEA,FSEC,DPDTTM,ERR,MESSAG)
              INTEGER(KIND=4) :: ITASK
              INTEGER(KIND=4) :: DATEA(6)
              REAL(KIND=4) :: FSEC
              REAL(KIND=8) :: DPDTTM
              LOGICAL(KIND=4) :: ERR
              CHARACTER(*) :: MESSAG
            END SUBROUTINE DTSYS
          END INTERFACE 
        END MODULE DTSYS__genmod
