        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:55 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ROOTGROWTH__genmod
          INTERFACE 
            SUBROUTINE ROOTGROWTH(TASK,PLANTDEPTH,INITCROPDEPTH,        &
     &RTCUMDENS,SRLMAX,SRLMIN,DWR,RDENSHAPE,ROOTSHAPE,RPUP,EFFECTIVE_RD,&
     &RLD,DIAC,DI,RDEPTH,FLEMERGED,RDM,ROOTDRATE,DRLD,DRDEPTH)
              USE VARIABLES
              INTEGER(KIND=4) :: TASK
              REAL(KIND=4) :: PLANTDEPTH
              REAL(KIND=4) :: INITCROPDEPTH
              INTEGER(KIND=4) :: RTCUMDENS
              REAL(KIND=4) :: SRLMAX
              REAL(KIND=4) :: SRLMIN
              REAL(KIND=4) :: DWR
              INTEGER(KIND=4) :: RDENSHAPE
              REAL(KIND=4) :: ROOTSHAPE
              REAL(KIND=4) :: RPUP
              REAL(KIND=4) :: EFFECTIVE_RD
              REAL(KIND=4) :: RLD(5000)
              REAL(KIND=4) :: DIAC
              REAL(KIND=4) :: DI
              REAL(KIND=4) :: RDEPTH
              LOGICAL(KIND=4) :: FLEMERGED
              REAL(KIND=4) :: RDM
              REAL(KIND=4) :: ROOTDRATE
              REAL(KIND=4) :: DRLD(5000)
              REAL(KIND=4) :: DRDEPTH
            END SUBROUTINE ROOTGROWTH
          END INTERFACE 
        END MODULE ROOTGROWTH__genmod
