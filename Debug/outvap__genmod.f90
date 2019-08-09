        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:14 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTVAP__genmod
          INTERFACE 
            SUBROUTINE OUTVAP(TASK,VAP,DAYNR,NUMNOD,DAYCUM,Z,CML,T1900, &
     &THETA,H,K,TSOIL,Q,OUTFIL,PATHWORK,PROJECT,SWHEADER,ISQTOP,ISQBOT, &
     &QDRAINCOMP,QROT,CMSY,DZ,DATE,FLPRINTSHORT)
              INTEGER(KIND=4) :: TASK
              INTEGER(KIND=4) :: VAP
              INTEGER(KIND=4) :: DAYNR
              INTEGER(KIND=4) :: NUMNOD
              INTEGER(KIND=4) :: DAYCUM
              REAL(KIND=8) :: Z(5000)
              REAL(KIND=8) :: CML(5000)
              REAL(KIND=8) :: T1900
              REAL(KIND=8) :: THETA(5000)
              REAL(KIND=8) :: H(5000)
              REAL(KIND=8) :: K(5001)
              REAL(KIND=8) :: TSOIL(5000)
              REAL(KIND=8) :: Q(5001)
              CHARACTER(LEN=16) :: OUTFIL
              CHARACTER(*) :: PATHWORK
              CHARACTER(LEN=80) :: PROJECT
              INTEGER(KIND=4) :: SWHEADER
              REAL(KIND=8) :: ISQTOP
              REAL(KIND=8) :: ISQBOT
              REAL(KIND=8) :: QDRAINCOMP(5000)
              REAL(KIND=8) :: QROT(5000)
              REAL(KIND=8) :: CMSY(5000)
              REAL(KIND=8) :: DZ(5000)
              CHARACTER(LEN=11) :: DATE
              LOGICAL(KIND=4) :: FLPRINTSHORT
            END SUBROUTINE OUTVAP
          END INTERFACE 
        END MODULE OUTVAP__genmod
