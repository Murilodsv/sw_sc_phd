        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:14 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTGRASS__genmod
          INTERFACE 
            SUBROUTINE OUTGRASS(TASK,DATE,DAYCROP,CRP,T,LAI,RD,DVS,CF,CH&
     &,CRT0,CRT1,TAGPPOT,TAGP,TAGPTPOT,TAGPT)
              INTEGER(KIND=4) :: TASK
              CHARACTER(LEN=11) :: DATE
              INTEGER(KIND=4) :: DAYCROP
              INTEGER(KIND=4) :: CRP
              REAL(KIND=8) :: T
              REAL(KIND=8) :: LAI
              REAL(KIND=8) :: RD
              REAL(KIND=8) :: DVS
              REAL(KIND=8) :: CF
              REAL(KIND=8) :: CH
              REAL(KIND=8) :: CRT0
              REAL(KIND=8) :: CRT1
              REAL(KIND=8) :: TAGPPOT
              REAL(KIND=8) :: TAGP
              REAL(KIND=8) :: TAGPTPOT
              REAL(KIND=8) :: TAGPT
            END SUBROUTINE OUTGRASS
          END INTERFACE 
        END MODULE OUTGRASS__genmod
