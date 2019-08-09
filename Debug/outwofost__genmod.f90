        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:14 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTWOFOST__genmod
          INTERFACE 
            SUBROUTINE OUTWOFOST(TASK,DATE,DAYCROP,CRP,T,DVS,LAI,CF,RD, &
     &CH,CRT0,CRELY,CRT1,CWDMPOT,CWDM,WSOPOT,WSO)
              INTEGER(KIND=4) :: TASK
              CHARACTER(LEN=11) :: DATE
              INTEGER(KIND=4) :: DAYCROP
              INTEGER(KIND=4) :: CRP
              REAL(KIND=8) :: T
              REAL(KIND=8) :: DVS
              REAL(KIND=8) :: LAI
              REAL(KIND=8) :: CF
              REAL(KIND=8) :: RD
              REAL(KIND=8) :: CH
              REAL(KIND=8) :: CRT0
              REAL(KIND=8) :: CRELY
              REAL(KIND=8) :: CRT1
              REAL(KIND=8) :: CWDMPOT
              REAL(KIND=8) :: CWDM
              REAL(KIND=8) :: WSOPOT
              REAL(KIND=8) :: WSO
            END SUBROUTINE OUTWOFOST
          END INTERFACE 
        END MODULE OUTWOFOST__genmod
