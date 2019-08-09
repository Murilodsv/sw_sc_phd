        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:56 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PENMON__genmod
          INTERFACE 
            SUBROUTINE PENMON(LOGF,SWSCRE,DAYMETEO,LAT,ALT,ALTW,A,B,RCS,&
     &RAD,TAV,HUM,WIN,RSC,ES0,ET0,EW0,SWCF,CH,FLBARESOIL,DAYLP,         &
     &FLMETDETAIL,IRECORD,NMETDETAIL,ALBEDO,TMN,TMX,RSW)
              INTEGER(KIND=4) :: LOGF
              INTEGER(KIND=4) :: SWSCRE
              INTEGER(KIND=4) :: DAYMETEO
              REAL(KIND=8) :: LAT
              REAL(KIND=8) :: ALT
              REAL(KIND=8) :: ALTW
              REAL(KIND=8) :: A
              REAL(KIND=8) :: B
              REAL(KIND=8) :: RCS
              REAL(KIND=8) :: RAD
              REAL(KIND=8) :: TAV
              REAL(KIND=8) :: HUM
              REAL(KIND=8) :: WIN
              REAL(KIND=8) :: RSC
              REAL(KIND=8) :: ES0
              REAL(KIND=8) :: ET0
              REAL(KIND=8) :: EW0
              INTEGER(KIND=4) :: SWCF
              REAL(KIND=8) :: CH
              LOGICAL(KIND=4) :: FLBARESOIL
              REAL(KIND=8) :: DAYLP
              LOGICAL(KIND=4) :: FLMETDETAIL
              INTEGER(KIND=4) :: IRECORD
              INTEGER(KIND=4) :: NMETDETAIL
              REAL(KIND=8) :: ALBEDO
              REAL(KIND=8) :: TMN
              REAL(KIND=8) :: TMX
              REAL(KIND=8) :: RSW
            END SUBROUTINE PENMON
          END INTERFACE 
        END MODULE PENMON__genmod
