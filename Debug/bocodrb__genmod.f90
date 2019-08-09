        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:55 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE BOCODRB__genmod
          INTERFACE 
            SUBROUTINE BOCODRB(DRAMET,GWL,ZBOTDR,BASEGW,L,QDRAIN,IPOS,  &
     &KHTOP,KHBOT,KVTOP,KVBOT,ENTRES,WETPER,ZINTF,GEOFAC,SWDTYP,OWLTAB, &
     &T1900,SWALLO,DRARES,INFRES,QDRTAB,NRLEVS,SWNRSRF,COFINTFL,EXPINTFL&
     &,DT,SHAPE,SWMACRO,NUMLEVRAPDRA,ZDRABAS,DH)
              INTEGER(KIND=4) :: DRAMET
              REAL(KIND=8) :: GWL
              REAL(KIND=8) :: ZBOTDR(5)
              REAL(KIND=8) :: BASEGW
              REAL(KIND=8) :: L(5)
              REAL(KIND=8) :: QDRAIN(5)
              INTEGER(KIND=4) :: IPOS
              REAL(KIND=8) :: KHTOP
              REAL(KIND=8) :: KHBOT
              REAL(KIND=8) :: KVTOP
              REAL(KIND=8) :: KVBOT
              REAL(KIND=8) :: ENTRES
              REAL(KIND=8) :: WETPER(5)
              REAL(KIND=8) :: ZINTF
              REAL(KIND=8) :: GEOFAC
              INTEGER(KIND=4) :: SWDTYP(5)
              REAL(KIND=8) :: OWLTAB(5,7320)
              REAL(KIND=8) :: T1900
              INTEGER(KIND=4) :: SWALLO(5)
              REAL(KIND=8) :: DRARES(5)
              REAL(KIND=8) :: INFRES(5)
              REAL(KIND=8) :: QDRTAB(50)
              INTEGER(KIND=4) :: NRLEVS
              INTEGER(KIND=4) :: SWNRSRF
              REAL(KIND=8) :: COFINTFL
              REAL(KIND=8) :: EXPINTFL
              REAL(KIND=8) :: DT
              REAL(KIND=8) :: SHAPE
              INTEGER(KIND=4) :: SWMACRO
              INTEGER(KIND=4) :: NUMLEVRAPDRA
              REAL(KIND=8) :: ZDRABAS
              REAL(KIND=8) :: DH
            END SUBROUTINE BOCODRB
          END INTERFACE 
        END MODULE BOCODRB__genmod
