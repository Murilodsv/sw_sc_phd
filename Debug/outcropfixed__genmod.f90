        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:14 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTCROPFIXED__genmod
          INTERFACE 
            SUBROUTINE OUTCROPFIXED(TASK,DATE,T,DAYCROP,DVS,LAI,CF,RD,  &
     &CRT0,CRELY,CRP,CH)
              INTEGER(KIND=4) :: TASK
              CHARACTER(LEN=11) :: DATE
              REAL(KIND=8) :: T
              INTEGER(KIND=4) :: DAYCROP
              REAL(KIND=8) :: DVS
              REAL(KIND=8) :: LAI
              REAL(KIND=8) :: CF
              REAL(KIND=8) :: RD
              REAL(KIND=8) :: CRT0
              REAL(KIND=8) :: CRELY
              INTEGER(KIND=4) :: CRP
              REAL(KIND=8) :: CH
            END SUBROUTINE OUTCROPFIXED
          END INTERFACE 
        END MODULE OUTCROPFIXED__genmod
