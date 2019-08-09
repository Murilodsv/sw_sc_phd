        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:12 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RDDRB__genmod
          INTERFACE 
            SUBROUTINE RDDRB(SWALLO,SWDTYP,SWMACRO,DRAMET,DRA,IPOS,     &
     &NRLEVS,LOGF,SWDIVD,L,ZBOTDR,OWLTAB,DRARES,INFRES,QDRTAB,BASEGW,   &
     &WETPER,KHTOP,KHBOT,KVTOP,KVBOT,ZINTF,ENTRES,GEOFAC,DRFIL,PATHDRAIN&
     &,NUMLAY,COFANI,SWNRSRF,SWSCRE,COFINTFL,EXPINTFL,SWDISLAY,         &
     &SWTOPDISLAY,ZTOPDISLAY,FTOPDISLAY,SHAPE,SWTOPNRSRF,SWDIVDINF,     &
     &FACDPTHINF)
              INTEGER(KIND=4) :: SWALLO(5)
              INTEGER(KIND=4) :: SWDTYP(5)
              INTEGER(KIND=4) :: SWMACRO
              INTEGER(KIND=4) :: DRAMET
              INTEGER(KIND=4) :: DRA
              INTEGER(KIND=4) :: IPOS
              INTEGER(KIND=4) :: NRLEVS
              INTEGER(KIND=4) :: LOGF
              INTEGER(KIND=4) :: SWDIVD
              REAL(KIND=8) :: L(5)
              REAL(KIND=8) :: ZBOTDR(5)
              REAL(KIND=8) :: OWLTAB(5,7320)
              REAL(KIND=8) :: DRARES(5)
              REAL(KIND=8) :: INFRES(5)
              REAL(KIND=8) :: QDRTAB(50)
              REAL(KIND=8) :: BASEGW
              REAL(KIND=8) :: WETPER(5)
              REAL(KIND=8) :: KHTOP
              REAL(KIND=8) :: KHBOT
              REAL(KIND=8) :: KVTOP
              REAL(KIND=8) :: KVBOT
              REAL(KIND=8) :: ZINTF
              REAL(KIND=8) :: ENTRES
              REAL(KIND=8) :: GEOFAC
              CHARACTER(LEN=16) :: DRFIL
              CHARACTER(LEN=80) :: PATHDRAIN
              INTEGER(KIND=4) :: NUMLAY
              REAL(KIND=8) :: COFANI(200)
              INTEGER(KIND=4) :: SWNRSRF
              INTEGER(KIND=4) :: SWSCRE
              REAL(KIND=8) :: COFINTFL
              REAL(KIND=8) :: EXPINTFL
              INTEGER(KIND=4) :: SWDISLAY
              INTEGER(KIND=4) :: SWTOPDISLAY(5)
              REAL(KIND=8) :: ZTOPDISLAY(5)
              REAL(KIND=8) :: FTOPDISLAY(5)
              REAL(KIND=8) :: SHAPE
              INTEGER(KIND=4) :: SWTOPNRSRF
              INTEGER(KIND=4) :: SWDIVDINF
              REAL(KIND=8) :: FACDPTHINF
            END SUBROUTINE RDDRB
          END INTERFACE 
        END MODULE RDDRB__genmod
