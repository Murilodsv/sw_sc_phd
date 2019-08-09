        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:31 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTAR2__genmod
          INTERFACE 
            SUBROUTINE OUTAR2(NAME,ARRAY,LDEC,UDEC,I1,I2)
              INTEGER(KIND=4) :: UDEC
              INTEGER(KIND=4) :: LDEC
              CHARACTER(*) :: NAME
              REAL(KIND=4) :: ARRAY(LDEC:UDEC)
              INTEGER(KIND=4) :: I1
              INTEGER(KIND=4) :: I2
            END SUBROUTINE OUTAR2
          END INTERFACE 
        END MODULE OUTAR2__genmod
