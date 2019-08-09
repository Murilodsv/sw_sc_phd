        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:13 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE REDUCEVA__genmod
          INTERFACE 
            SUBROUTINE REDUCEVA(TASK,SWREDU,EMPREVA,PEVA,NRAI,NIRD,     &
     &COFRED,RSIGNI,SWINCO,LDWET,SPEV,SAEV,FLDAYSTART,DT,POND)
              INTEGER(KIND=4) :: TASK
              INTEGER(KIND=4) :: SWREDU
              REAL(KIND=8) :: EMPREVA
              REAL(KIND=8) :: PEVA
              REAL(KIND=8) :: NRAI
              REAL(KIND=8) :: NIRD
              REAL(KIND=8) :: COFRED
              REAL(KIND=8) :: RSIGNI
              INTEGER(KIND=4) :: SWINCO
              REAL(KIND=8) :: LDWET
              REAL(KIND=8) :: SPEV
              REAL(KIND=8) :: SAEV
              LOGICAL(KIND=4) :: FLDAYSTART
              REAL(KIND=8) :: DT
              REAL(KIND=8) :: POND
            END SUBROUTINE REDUCEVA
          END INTERFACE 
        END MODULE REDUCEVA__genmod
