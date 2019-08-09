        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:33 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RDINDX__genmod
          INTERFACE 
            SUBROUTINE RDINDX(IERR,IUNIT,SETS,TOSCRX,TOLOGX,IUL,DATFIL, &
     &TMPFIL,NAMLIS,VARTYP,VARRAY,ILIND,IARLEN,INDPNT,ILPNT,INFND,INSETS&
     &)
              INTEGER(KIND=4) :: ILPNT
              INTEGER(KIND=4) :: ILIND
              INTEGER(KIND=4) :: IERR
              INTEGER(KIND=4) :: IUNIT
              LOGICAL(KIND=4) :: SETS
              LOGICAL(KIND=4) :: TOSCRX
              LOGICAL(KIND=4) :: TOLOGX
              INTEGER(KIND=4) :: IUL
              CHARACTER(*) :: DATFIL
              CHARACTER(*) :: TMPFIL
              CHARACTER(*) :: NAMLIS(ILIND)
              CHARACTER(LEN=1) :: VARTYP(ILIND)
              CHARACTER(LEN=1) :: VARRAY(ILIND)
              INTEGER(KIND=4) :: IARLEN(ILPNT)
              INTEGER(KIND=4) :: INDPNT(ILPNT)
              INTEGER(KIND=4) :: INFND
              INTEGER(KIND=4) :: INSETS
            END SUBROUTINE RDINDX
          END INTERFACE 
        END MODULE RDINDX__genmod
