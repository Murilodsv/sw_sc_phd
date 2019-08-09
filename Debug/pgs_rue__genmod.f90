        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:55 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PGS_RUE__genmod
          INTERFACE 
            SUBROUTINE PGS_RUE(PAR,CO2,T_MEAN,THOUR,FLUSETHOUR,SWFACP,  &
     &AGEFPG,LAI,EXTCOEF,TB,TO_PG1,TO_PG2,TBM,RUE,PG,LI,TSTRESS,CO2_FAC)
              REAL(KIND=4) :: PAR
              REAL(KIND=8) :: CO2
              REAL(KIND=4) :: T_MEAN
              REAL(KIND=4) :: THOUR(24)
              LOGICAL(KIND=4) :: FLUSETHOUR
              REAL(KIND=4) :: SWFACP
              REAL(KIND=4) :: AGEFPG
              REAL(KIND=8) :: LAI
              REAL(KIND=4) :: EXTCOEF
              REAL(KIND=4) :: TB
              REAL(KIND=4) :: TO_PG1
              REAL(KIND=4) :: TO_PG2
              REAL(KIND=4) :: TBM
              REAL(KIND=4) :: RUE
              REAL(KIND=4) :: PG
              REAL(KIND=4) :: LI
              REAL(KIND=4) :: TSTRESS
              REAL(KIND=4) :: CO2_FAC
            END SUBROUTINE PGS_RUE
          END INTERFACE 
        END MODULE PGS_RUE__genmod
