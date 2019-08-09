        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:53 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE WATSTOR__genmod
          INTERFACE 
            SUBROUTINE WATSTOR(VOLM1,VOLACT,NUMNOD,THETA,DZ,FRARMTRX)
              REAL(KIND=8) :: VOLM1
              REAL(KIND=8) :: VOLACT
              INTEGER(KIND=4) :: NUMNOD
              REAL(KIND=8) :: THETA(5000)
              REAL(KIND=8) :: DZ(5000)
              REAL(KIND=8) :: FRARMTRX(5000)
            END SUBROUTINE WATSTOR
          END INTERFACE 
        END MODULE WATSTOR__genmod
