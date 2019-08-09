        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:11 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RDTMP2__genmod
          INTERFACE 
            SUBROUTINE RDTMP2(ITASK,IUNIT,NAMLIS,VARTYP,VARRAY,ILIND,   &
     &IARLEN,INDPNT,ILPNT,INFND,INSETS)
              INTEGER(KIND=4) :: ILPNT
              INTEGER(KIND=4) :: ILIND
              INTEGER(KIND=4) :: ITASK
              INTEGER(KIND=4) :: IUNIT
              CHARACTER(*) :: NAMLIS(ILIND)
              CHARACTER(LEN=1) :: VARTYP(ILIND)
              CHARACTER(LEN=1) :: VARRAY(ILIND)
              INTEGER(KIND=4) :: IARLEN(ILPNT)
              INTEGER(KIND=4) :: INDPNT(ILPNT)
              INTEGER(KIND=4) :: INFND
              INTEGER(KIND=4) :: INSETS
            END SUBROUTINE RDTMP2
          END INTERFACE 
        END MODULE RDTMP2__genmod
