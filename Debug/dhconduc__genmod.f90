        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:33 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DHCONDUC__genmod
          INTERFACE 
            FUNCTION DHCONDUC(NODE,THETA,DIMOCAP,COFG,SWFROST,RFCP,     &
     &SWSOPHY,NUMTAB,SPTAB)
              INTEGER(KIND=4) :: NODE
              REAL(KIND=8) :: THETA
              REAL(KIND=8) :: DIMOCAP
              REAL(KIND=8) :: COFG(12)
              INTEGER(KIND=4) :: SWFROST
              REAL(KIND=8) :: RFCP
              INTEGER(KIND=4) :: SWSOPHY
              INTEGER(KIND=4) :: NUMTAB(5000)
              REAL(KIND=8) :: SPTAB(5,5000,1000)
              REAL(KIND=8) :: DHCONDUC
            END FUNCTION DHCONDUC
          END INTERFACE 
        END MODULE DHCONDUC__genmod
