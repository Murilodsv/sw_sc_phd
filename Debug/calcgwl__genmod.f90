        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:01 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CALCGWL__genmod
          INTERFACE 
            SUBROUTINE CALCGWL(LOGF,SWSCRE,SWBOTB,FLMACROPORE,NUMNOD,   &
     &CRITUNDSATVOL,GWLINP,H,Z,DZ,POND,T1900,THETA,THETAS,GWL,NODGWL,   &
     &BPEGWL,NPEGWL,PEGWL,NODGWLFLCPZO,GWLFLCPZO)
              INTEGER(KIND=4) :: LOGF
              INTEGER(KIND=4) :: SWSCRE
              INTEGER(KIND=4) :: SWBOTB
              LOGICAL(KIND=4) :: FLMACROPORE
              INTEGER(KIND=4) :: NUMNOD
              REAL(KIND=8) :: CRITUNDSATVOL
              REAL(KIND=8) :: GWLINP
              REAL(KIND=8) :: H(5000)
              REAL(KIND=8) :: Z(5000)
              REAL(KIND=8) :: DZ(5000)
              REAL(KIND=8) :: POND
              REAL(KIND=8) :: T1900
              REAL(KIND=8) :: THETA(5000)
              REAL(KIND=8) :: THETAS(5000)
              REAL(KIND=8) :: GWL
              INTEGER(KIND=4) :: NODGWL
              INTEGER(KIND=4) :: BPEGWL
              INTEGER(KIND=4) :: NPEGWL
              REAL(KIND=8) :: PEGWL
              INTEGER(KIND=4) :: NODGWLFLCPZO
              REAL(KIND=8) :: GWLFLCPZO
            END SUBROUTINE CALCGWL
          END INTERFACE 
        END MODULE CALCGWL__genmod
