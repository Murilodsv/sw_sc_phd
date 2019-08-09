        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:08 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE WLEVBAL__genmod
          INTERFACE 
            SUBROUTINE WLEVBAL(TCUM,NRPRI,IMPEND,NMPER,SWMAN,IMPER,     &
     &WLSTAR,WLS,HBWEIR,GWL,WLSMAN,GWLCRIT,NPHASE,DROPR,STTAB,WSCAP,DT, &
     &RUNOTS,QRAPDRA,SWST,ZBOTDR,ALPHAW,BETAW,QDRD,CQDRD,CWSUPP,CWOUT,  &
     &WLSBAK,OSSWLM,T,NUMNOD,THETAS,THETA,DZ,VCRIT,NODHD,HCRIT,H,SWQHR, &
     &QQHTAB,HQHTAB,WLDIP,HWLMAN,VTAIR,OVERFL,NUMADJ,INTWL,T1900,LOGF,  &
     &SWSCRE,FLDECDT,FLDTMIN,RSRO,POND,PONDMX)
              REAL(KIND=8) :: TCUM
              INTEGER(KIND=4) :: NRPRI
              REAL(KIND=8) :: IMPEND(3660)
              INTEGER(KIND=4) :: NMPER
              INTEGER(KIND=4) :: SWMAN(3660)
              INTEGER(KIND=4) :: IMPER
              REAL(KIND=8) :: WLSTAR
              REAL(KIND=8) :: WLS
              REAL(KIND=8) :: HBWEIR(3660)
              REAL(KIND=8) :: GWL
              REAL(KIND=8) :: WLSMAN(3660,25)
              REAL(KIND=8) :: GWLCRIT(3660,25)
              INTEGER(KIND=4) :: NPHASE(3660)
              REAL(KIND=8) :: DROPR(91500)
              REAL(KIND=8) :: STTAB(22,2)
              REAL(KIND=8) :: WSCAP(3660)
              REAL(KIND=8) :: DT
              REAL(KIND=8) :: RUNOTS
              REAL(KIND=8) :: QRAPDRA
              REAL(KIND=8) :: SWST
              REAL(KIND=8) :: ZBOTDR(5)
              REAL(KIND=8) :: ALPHAW(3660)
              REAL(KIND=8) :: BETAW(3660)
              REAL(KIND=8) :: QDRD
              REAL(KIND=8) :: CQDRD
              REAL(KIND=8) :: CWSUPP
              REAL(KIND=8) :: CWOUT
              REAL(KIND=8) :: WLSBAK(4)
              REAL(KIND=8) :: OSSWLM
              REAL(KIND=8) :: T
              INTEGER(KIND=4) :: NUMNOD
              REAL(KIND=8) :: THETAS(5000)
              REAL(KIND=8) :: THETA(5000)
              REAL(KIND=8) :: DZ(5000)
              REAL(KIND=8) :: VCRIT(3660,25)
              INTEGER(KIND=4) :: NODHD(3660)
              REAL(KIND=8) :: HCRIT(3660,25)
              REAL(KIND=8) :: H(5000)
              INTEGER(KIND=4) :: SWQHR
              REAL(KIND=8) :: QQHTAB(3660,25)
              REAL(KIND=8) :: HQHTAB(3660,25)
              REAL(KIND=8) :: WLDIP(3660)
              REAL(KIND=8) :: HWLMAN
              REAL(KIND=8) :: VTAIR
              LOGICAL(KIND=4) :: OVERFL
              INTEGER(KIND=4) :: NUMADJ
              INTEGER(KIND=4) :: INTWL(3660)
              REAL(KIND=8) :: T1900
              INTEGER(KIND=4) :: LOGF
              INTEGER(KIND=4) :: SWSCRE
              LOGICAL(KIND=4) :: FLDECDT
              LOGICAL(KIND=4) :: FLDTMIN
              REAL(KIND=8) :: RSRO
              REAL(KIND=8) :: POND
              REAL(KIND=8) :: PONDMX
            END SUBROUTINE WLEVBAL
          END INTERFACE 
        END MODULE WLEVBAL__genmod
