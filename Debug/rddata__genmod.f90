        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:36 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RDDATA__genmod
          INTERFACE 
            SUBROUTINE RDDATA(ITASK,CALPRG,IUNIT,IULOG,FILNAM,IS,XNAME, &
     &VARTYP,DX,RX,IX,CX,LX,NDDEC,NRDEC,NIDEC,NCDEC,NLDEC,NREQ,XDM,XRM, &
     &XIM,XCM,XLM)
              INTEGER(KIND=4) :: NLDEC
              INTEGER(KIND=4) :: NCDEC
              INTEGER(KIND=4) :: NIDEC
              INTEGER(KIND=4) :: NRDEC
              INTEGER(KIND=4) :: NDDEC
              INTEGER(KIND=4) :: ITASK
              CHARACTER(*) :: CALPRG
              INTEGER(KIND=4) :: IUNIT
              INTEGER(KIND=4) :: IULOG
              CHARACTER(*) :: FILNAM
              INTEGER(KIND=4) :: IS
              CHARACTER(*) :: XNAME
              CHARACTER(LEN=1) :: VARTYP
              REAL(KIND=8) :: DX(NDDEC)
              REAL(KIND=4) :: RX(NRDEC)
              INTEGER(KIND=4) :: IX(NIDEC)
              CHARACTER(*) :: CX(NCDEC)
              LOGICAL(KIND=4) :: LX(NLDEC)
              INTEGER(KIND=4) :: NREQ
              REAL(KIND=8) :: XDM
              REAL(KIND=4) :: XRM
              INTEGER(KIND=4) :: XIM
              CHARACTER(LEN=80) :: XCM
              LOGICAL(KIND=4) :: XLM
            END SUBROUTINE RDDATA
          END INTERFACE 
        END MODULE RDDATA__genmod
