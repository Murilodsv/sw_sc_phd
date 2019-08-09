        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:08 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE WBALLEV__genmod
          INTERFACE 
            SUBROUTINE WBALLEV(WLS,WLSTAB,SWST,STTAB,DT,RUNOTS,QRAPDRA, &
     &QDRD,CQDRD,CWSUPP,CWOUT,WLSOLD,T1900)
              REAL(KIND=8) :: WLS
              REAL(KIND=8) :: WLSTAB(7320)
              REAL(KIND=8) :: SWST
              REAL(KIND=8) :: STTAB(22,2)
              REAL(KIND=8) :: DT
              REAL(KIND=8) :: RUNOTS
              REAL(KIND=8) :: QRAPDRA
              REAL(KIND=8) :: QDRD
              REAL(KIND=8) :: CQDRD
              REAL(KIND=8) :: CWSUPP
              REAL(KIND=8) :: CWOUT
              REAL(KIND=8) :: WLSOLD
              REAL(KIND=8) :: T1900
            END SUBROUTINE WBALLEV
          END INTERFACE 
        END MODULE WBALLEV__genmod
