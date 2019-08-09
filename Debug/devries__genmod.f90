        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:11 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DEVRIES__genmod
          INTERFACE 
            SUBROUTINE DEVRIES(NUMNOD,THETA,THETAS,HEACAP,HEACON,FQUARTZ&
     &,FCLAY,FORG)
              INTEGER(KIND=4) :: NUMNOD
              REAL(KIND=8) :: THETA(1:5000)
              REAL(KIND=8) :: THETAS(1:5000)
              REAL(KIND=8) :: HEACAP(5000)
              REAL(KIND=8) :: HEACON(5000)
              REAL(KIND=8) :: FQUARTZ(5000)
              REAL(KIND=8) :: FCLAY(5000)
              REAL(KIND=8) :: FORG(5000)
            END SUBROUTINE DEVRIES
          END INTERFACE 
        END MODULE DEVRIES__genmod
