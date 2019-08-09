        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:55 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE BOCODRE__genmod
          INTERFACE 
            SUBROUTINE BOCODRE(SWSEC,SWSRF,NRLEVS,NRPRI,GWL,ZBOTDR,     &
     &TALUDR,WIDTHR,POND,PONDMX,SWDTYP,DT,WLS,WLP,DRAINL,L,RDRAIN,RINFI,&
     &RENTRY,REXIT,GWLINF,WETPER,QDRAIN,QDRD,IMPEND,NMPER,WSCAP,SWST,   &
     &SWNRSRF,RSURFDEEP,RSURFSHALLOW,COFINTFL,EXPINTFL,T1900,SWMACRO,   &
     &NUMLEVRAPDRA,ZDRABAS,DH)
              INTEGER(KIND=4) :: SWSEC
              INTEGER(KIND=4) :: SWSRF
              INTEGER(KIND=4) :: NRLEVS
              INTEGER(KIND=4) :: NRPRI
              REAL(KIND=8) :: GWL
              REAL(KIND=8) :: ZBOTDR(5)
              REAL(KIND=8) :: TALUDR(5)
              REAL(KIND=8) :: WIDTHR(5)
              REAL(KIND=8) :: POND
              REAL(KIND=8) :: PONDMX
              INTEGER(KIND=4) :: SWDTYP(5)
              REAL(KIND=8) :: DT
              REAL(KIND=8) :: WLS
              REAL(KIND=8) :: WLP
              REAL(KIND=8) :: DRAINL(5)
              REAL(KIND=8) :: L(5)
              REAL(KIND=8) :: RDRAIN(5)
              REAL(KIND=8) :: RINFI(5)
              REAL(KIND=8) :: RENTRY(5)
              REAL(KIND=8) :: REXIT(5)
              REAL(KIND=8) :: GWLINF(5)
              REAL(KIND=8) :: WETPER(5)
              REAL(KIND=8) :: QDRAIN(5)
              REAL(KIND=8) :: QDRD
              REAL(KIND=8) :: IMPEND(3660)
              INTEGER(KIND=4) :: NMPER
              REAL(KIND=8) :: WSCAP(3660)
              REAL(KIND=8) :: SWST
              INTEGER(KIND=4) :: SWNRSRF
              REAL(KIND=8) :: RSURFDEEP
              REAL(KIND=8) :: RSURFSHALLOW
              REAL(KIND=8) :: COFINTFL
              REAL(KIND=8) :: EXPINTFL
              REAL(KIND=8) :: T1900
              INTEGER(KIND=4) :: SWMACRO
              INTEGER(KIND=4) :: NUMLEVRAPDRA
              REAL(KIND=8) :: ZDRABAS
              REAL(KIND=8) :: DH
            END SUBROUTINE BOCODRE
          END INTERFACE 
        END MODULE BOCODRE__genmod
