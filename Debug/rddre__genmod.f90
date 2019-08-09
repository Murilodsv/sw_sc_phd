        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:13 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RDDRE__genmod
          INTERFACE 
            SUBROUTINE RDDRE(DRFIL,PATHDRAIN,NRLEVS,NRPRI,NRSEC,L,ZBOTDR&
     &,WIDTHR,TALUDR,RDRAIN,RINFI,RENTRY,REXIT,GWLINF,SWDTYP,WLPTAB,    &
     &SWSEC,WLS1,OSSWLM,NMPER,WLSTAR,IMPEND,SWMAN,WLSMAN,WSCAP,SWQHR,   &
     &HBWEIR,ALPHAW,BETAW,NQH,HQHTAB,QQHTAB,DROPR,GWLCRIT,WLSTAB,STTAB, &
     &SWSTINI,SWST,WLSBAK,SWSRF,NPHASE,HCRIT,HDEPTH,VCRIT,WLP1,NODHD,   &
     &NUMNOD,DZ,WLDIP,NUMADJ,LOGF,INTWL,SWNRSRF,RSURFDEEP,RSURFSHALLOW, &
     &T1900,COFINTFL,EXPINTFL,SWSCRE,SWDIVD,COFANI,NUMLAY,TSTART,TEND,  &
     &SWDISLAY,SWTOPDISLAY,ZTOPDISLAY,FTOPDISLAY,SWTOPNRSRF,SWDIVDINF,  &
     &FACDPTHINF)
              CHARACTER(LEN=16) :: DRFIL
              CHARACTER(LEN=80) :: PATHDRAIN
              INTEGER(KIND=4) :: NRLEVS
              INTEGER(KIND=4) :: NRPRI
              INTEGER(KIND=4) :: NRSEC
              REAL(KIND=8) :: L(5)
              REAL(KIND=8) :: ZBOTDR(5)
              REAL(KIND=8) :: WIDTHR(5)
              REAL(KIND=8) :: TALUDR(5)
              REAL(KIND=8) :: RDRAIN(5)
              REAL(KIND=8) :: RINFI(5)
              REAL(KIND=8) :: RENTRY(5)
              REAL(KIND=8) :: REXIT(5)
              REAL(KIND=8) :: GWLINF(5)
              INTEGER(KIND=4) :: SWDTYP(5)
              REAL(KIND=8) :: WLPTAB(7320)
              INTEGER(KIND=4) :: SWSEC
              REAL(KIND=8) :: WLS1
              REAL(KIND=8) :: OSSWLM
              INTEGER(KIND=4) :: NMPER
              REAL(KIND=8) :: WLSTAR
              REAL(KIND=8) :: IMPEND(3660)
              INTEGER(KIND=4) :: SWMAN(3660)
              REAL(KIND=8) :: WLSMAN(3660,25)
              REAL(KIND=8) :: WSCAP(3660)
              INTEGER(KIND=4) :: SWQHR
              REAL(KIND=8) :: HBWEIR(3660)
              REAL(KIND=8) :: ALPHAW(3660)
              REAL(KIND=8) :: BETAW(3660)
              INTEGER(KIND=4) :: NQH(3660)
              REAL(KIND=8) :: HQHTAB(3660,25)
              REAL(KIND=8) :: QQHTAB(3660,25)
              REAL(KIND=8) :: DROPR(91500)
              REAL(KIND=8) :: GWLCRIT(3660,25)
              REAL(KIND=8) :: WLSTAB(7320)
              REAL(KIND=8) :: STTAB(22,2)
              REAL(KIND=8) :: SWSTINI
              REAL(KIND=8) :: SWST
              REAL(KIND=8) :: WLSBAK(4)
              INTEGER(KIND=4) :: SWSRF
              INTEGER(KIND=4) :: NPHASE(3660)
              REAL(KIND=8) :: HCRIT(3660,25)
              REAL(KIND=8) :: HDEPTH(91500)
              REAL(KIND=8) :: VCRIT(3660,25)
              REAL(KIND=8) :: WLP1
              INTEGER(KIND=4) :: NODHD(3660)
              INTEGER(KIND=4) :: NUMNOD
              REAL(KIND=8) :: DZ(5000)
              REAL(KIND=8) :: WLDIP(3660)
              INTEGER(KIND=4) :: NUMADJ
              INTEGER(KIND=4) :: LOGF
              INTEGER(KIND=4) :: INTWL(3660)
              INTEGER(KIND=4) :: SWNRSRF
              REAL(KIND=8) :: RSURFDEEP
              REAL(KIND=8) :: RSURFSHALLOW
              REAL(KIND=8) :: T1900
              REAL(KIND=8) :: COFINTFL
              REAL(KIND=8) :: EXPINTFL
              INTEGER(KIND=4) :: SWSCRE
              INTEGER(KIND=4) :: SWDIVD
              REAL(KIND=8) :: COFANI(200)
              INTEGER(KIND=4) :: NUMLAY
              REAL(KIND=8) :: TSTART
              REAL(KIND=8) :: TEND
              INTEGER(KIND=4) :: SWDISLAY
              INTEGER(KIND=4) :: SWTOPDISLAY(5)
              REAL(KIND=8) :: ZTOPDISLAY(5)
              REAL(KIND=8) :: FTOPDISLAY(5)
              INTEGER(KIND=4) :: SWTOPNRSRF
              INTEGER(KIND=4) :: SWDIVDINF
              REAL(KIND=8) :: FACDPTHINF
            END SUBROUTINE RDDRE
          END INTERFACE 
        END MODULE RDDRE__genmod
