        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:17 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FLUXES__genmod
          INTERFACE 
            SUBROUTINE FLUXES(Q,QBOT,DT,INQ,NUMNOD,THETM1,THETA,DZ,QROT,&
     &QDRA,QIMMOB,QTOP,QROSUM,QDRTOT,VOLACT,VOLM1,SWBOTB,FRARMTRX,      &
     &QEXCMPMTX,QMAPO,NRLEVS,FLLOWGWL)
              REAL(KIND=8) :: Q(5001)
              REAL(KIND=8) :: QBOT
              REAL(KIND=8) :: DT
              REAL(KIND=8) :: INQ(5001)
              INTEGER(KIND=4) :: NUMNOD
              REAL(KIND=8) :: THETM1(5000)
              REAL(KIND=8) :: THETA(5000)
              REAL(KIND=8) :: DZ(5000)
              REAL(KIND=8) :: QROT(5000)
              REAL(KIND=8) :: QDRA(5,5000)
              REAL(KIND=8) :: QIMMOB(5000)
              REAL(KIND=8) :: QTOP
              REAL(KIND=8) :: QROSUM
              REAL(KIND=8) :: QDRTOT
              REAL(KIND=8) :: VOLACT
              REAL(KIND=8) :: VOLM1
              INTEGER(KIND=4) :: SWBOTB
              REAL(KIND=8) :: FRARMTRX(5000)
              REAL(KIND=8) :: QEXCMPMTX(5000)
              REAL(KIND=8) :: QMAPO
              INTEGER(KIND=4) :: NRLEVS
              LOGICAL(KIND=4) :: FLLOWGWL
            END SUBROUTINE FLUXES
          END INTERFACE 
        END MODULE FLUXES__genmod
