! File VersionID:
!   $Id: macroporeoutput.for 176 2010-03-13 11:36:14Z kroes006 $
! ----------------------------------------------------------------------
      subroutine MacroPoreOutput(task) 
! ----------------------------------------------------------------------
!     Date               : Aug 2004   
!     Purpose            : open and write macropore output files 
! ----------------------------------------------------------------------

      use Variables
      implicit none

      integer task
!     local
      integer getun, ic, id, mpgeom, i
      real*8 cumdz
      character filnam*300,filtext*181,comma*1,fmt*202,fmt1*2
! ----------------------------------------------------------------------
      comma = ','

      goto (1000, 2000, 3000) task

1000  continue
! --  write geometry of macropores
!      open(unit=3,file='MacroGeom.csv',status='unknown')
      filnam = trim(pathwork)//'MacroGeom.Csv'
      mpgeom = getun (50,90)
      call fopens(mpgeom,filnam,'new','del')
      filtext='Macropore geometry: Pp = proportion (-); Vls = '
     &//'volume static macropores(cm3/cm2). Bot.depth = bottom detpth'
     &//' compartment (cm).'
      call writehead (mpgeom,1,filnam,filtext,project)
! --  geometry of macropores
      fmt(1:19) = 'Compartm.,Bot.depth'
      do i=1,NumDm
         write(fmt1(1:2),'(i2.2)')i
         fmt(1:i*8+19) = fmt(1:(i-1)*8+19) // ', PpDm'//fmt1
      end do
      fmt(1:NumDm*8+43) = fmt(1:NumDm*8+19)// ', VlsTot,  VlsMB,  VlsIC'
      write(mpgeom,'(a)') trim(fmt)
      cumdz = 0.0d0
      do ic= 1, NumNod
         cumdz = cumdz + dz(ic)
         write(mpgeom,'(I4,a1,F8.4,23(a1,F7.4))') 
     &                  ic,comma,cumdz,(comma,PpDmCp(id,ic),id=1,NumDM),
     &                  comma,VlMpStCp(ic),comma,VlMpStDm1(ic), 
     &                  comma,VlMpStDm2(ic)
      enddo
      close(mpgeom)

! --- output of generated shrinkage characteristics
      call outshrinkchar ()

! --  bma file (macropore output)
      if (swbma .eq. 1) 
     &  call outbma(task,bma,outfil,pathwork,z,dz,
     &     numnod,ioutdat,t1900,outdat,tstart,project,
     &     CQMpInIntSatDm1,CQMpInIntSatDm2,CQMpInMtxSatDm1,   
     &     CQMpInMtxSatDm2,CQMpOutDrRap,CQMpOutMtxSatDm1,
     &     CQMpOutMtxSatDm2,CQMpOutMtxUnsDm1,CQMpOutMtxUnsDm2,
     &     CQMpInTopPreDm1,CQMpInTopPreDm2,CQMpInTopLatDm1,
     &     CQMpInTopLatDm2,WaSrDm1,WaSrDm1Ini,WaSrDm2,WaSrDm2Ini)
      return

2000  continue

! === write actual data ===============================
! --  bma file (macropore output)
      if (swbma .eq. 1 .and. flbaloutput) 
     &  call outbma(task,bma,outfil,pathwork,z,dz,
     &     numnod,ioutdat,t1900,outdat,tstart,project,
     &     CQMpInIntSatDm1,CQMpInIntSatDm2,CQMpInMtxSatDm1,   
     &     CQMpInMtxSatDm2,CQMpOutDrRap,CQMpOutMtxSatDm1,
     &     CQMpOutMtxSatDm2,CQMpOutMtxUnsDm1,CQMpOutMtxUnsDm2,
     &     CQMpInTopPreDm1,CQMpInTopPreDm2,CQMpInTopLatDm1,
     &     CQMpInTopLatDm2,WaSrDm1,WaSrDm1Ini,WaSrDm2,WaSrDm2Ini)
      return

3000  continue

! === close output files ================================
      if (swbma .eq. 1) close (bma)

      return
      end 

!=======================================================================
      SUBROUTINE OUTBMA(task,bma,outfil,pathwork,z,dz,
     &     numnod,ioutdat,t1900,outdat,tstart,project,
     &     CQMpInIntSatDm1,CQMpInIntSatDm2,CQMpInMtxSatDm1,   
     &     CQMpInMtxSatDm2,CQMpOutDrRap,CQMpOutMtxSatDm1,
     &     CQMpOutMtxSatDm2,CQMpOutMtxUnsDm1,CQMpOutMtxUnsDm2,
     &     CQMpInTopPreDm1,CQMpInTopPreDm2,CQMpInTopLatDm1,
     &     CQMpInTopLatDm2,WaSrDm1,WaSrDm1Ini,WaSrDm2,WaSrDm2Ini)
! ----------------------------------------------------------------------
!     date               : 05/8/02
!     purpose            : write macropore balances to outnam.bma file
!     functions called   : -
!     file usage         : 
! ---------------------------------------------------------------------
      implicit none
      include 'arrays.fi'

!     global
      integer   bma,numnod,ioutdat,getun, task
      real*8    dz(macp),z(macp), tstart,t1900,outdat(maout)  
      real*8    CQMpInIntSatDm1, CQMpInIntSatDm2, CQMpInMtxSatDm1   
      real*8    CQMpInMtxSatDm2, CQMpInTopPreDm1, CQMpInTopPreDm2
      real*8    CQMpInTopLatDm1, CQMpInTopLatDm2, CQMpOutDrRap
      real*8    CQMpOutMtxSatDm1, CQMpOutMtxSatDm2
      real*8    CQMpOutMtxUnsDm1, CQMpOutMtxUnsDm2
      real*8    WaSrDm1, WaSrDm1Ini, WaSrDm2, WaSrDm2Ini 
    
      character outfil*(*),pathwork*(*),project*(*)
      
!     local
!      integer   ilev
      real*8    SumInDm1, SumInDm2, SumOutDm1, SumOutDm2
      character filnam*300,datbegin*11,datend*11,filtext*80
! ----------------------------------------------------------------------
      goto (1000, 2000) task

1000  continue
! --- open output file
      filnam = trim(pathwork)//trim(outfil)//'.bma'
      bma = getun (50,90)
      call fopens(bma,filnam,'new','del')
      filtext='overview of macropore water balance components (cm)'
      call writehead (bma,1,filnam,filtext,project)

      return

2000  continue
! --- begin date of balance period
      if (ioutdat .eq. 2) then
        call dtdpst ('day-monthst-year',tstart,datbegin)
      else
        call dtdpst ('day-monthst-year',
     &                                outdat(ioutdat-2)+1.1d0,datbegin)
      endif

! --- end date of sub-run
      call dtdpst ('day-monthst-year',t1900+0.1d0,datend)

c --- write output record
      write (bma,20) datbegin,datend
      write (bma,22) (-z(numnod) + 0.5*dz(numnod))
      write (bma,40)      
      write (bma,41) WaSrDm1Ini, WaSrDm2Ini, WaSrDm1Ini + WaSrDm2Ini, 
     &               WaSrDm1, WaSrDm2, WaSrDm1 + WaSrDm2
      write (bma,42) CQMpInTopPreDm1, CQMpInTopPreDm2, 
     &               CQMpInTopPreDm1 + CQMpInTopPreDm2, CQMpInTopLatDm1, 
     &               CQMpInTopLatDm2, CQMpInTopLatDm1 + CQMpInTopLatDm2
      write (bma,43) CQMpInIntSatDm1, CQMpInIntSatDm2, CQMpInIntSatDm1 + 
     &               CQMpInIntSatDm2, CQMpOutMtxUnsDm1,CQMpOutMtxUnsDm2,
     &               CQMpOutMtxUnsDm1 + CQMpOutMtxUnsDm2, 
     &               CQMpInMtxSatDm1, CQMpInMtxSatDm2, CQMpInMtxSatDm1 + 
     &               CQMpInMtxSatDm2, CQMpOutMtxSatDm1,CQMpOutMtxSatDm2, 
     &               CQMpOutMtxSatDm1 + CQMpOutMtxSatDm2
      write (bma,44) CQMpOutDrRap, CQMpOutDrRap


!      if (nrlevs .ge. 1) then 
!        write (bma,55)
!        do ilev=1,nrlevs
!          write (bma,56) ilev, cqdrainin(ilev), ilev, cqdrainout(ilev)
!        enddo
!      endif
!      write (bma,61) cqbotup,cqbotdo
!      write (bma,62) ssnow,volact 
      SumInDm1= WaSrDm1Ini + CQMpInTopPreDm1 + CQMpInTopLatDm1 + 
     &          CQMpInIntSatDm1 + CQMpInMtxSatDm1 
      SumInDm2= WaSrDm2Ini + CQMpInTopPreDm2 + CQMpInTopLatDm2 +
     &          CQMpInIntSatDm2 + CQMpInMtxSatDm2
      SumOutDm1= WaSrDm1 + CQMpOutMtxSatDm1 + CQMpOutMtxUnsDm1 + 
     &           CQMpOutDrRap
      SumOutDm2= WaSrDm2 + CQMpOutMtxSatDm2 + CQMpOutMtxUnsDm2
! --- sum
      write (bma,45) SumInDm1, SumInDm2, SumInDm1 + SumInDm2,
     &               SumOutDm1, SumOutDm2, SumOutDm1 + SumOutDm2
      write (bma,46) WaSrDm1-WaSrDm1Ini, WaSrDm2-WaSrDm2Ini, 
     &               WaSrDm1-WaSrDm1Ini + WaSrDm2-WaSrDm2Ini
      write (bma,47) SumOutDm1-SumInDm1, SumOutDm2-SumInDm2, 
     &               SumOutDm1-SumInDm1 + SumOutDm2-SumInDm2

 20   format(/'Period',t20,':',t23,a11,' until  ',a11)
 22   format('Depth soil profile',t20,':',f8.2,' cm')
 40   format(48('='),'+',50('='),/,'INPUT',t22,'MAIN     INTERN.  TOTAL'
     &,t49,'|',t52,'OUTPUT',t73,'MAIN     INTERN.  TOTAL',/,t22,
     &'BYPASS   CATCHM.  MACROP.',t49,'|',t73,
     &'BYPASS   CATCHM.  MACROP.',/,48('='),'+',50('='))
 41   format('Initially Present',t19,3f9.2,t49,'|',t52,'Finally present'
     &,t70,3f9.2,/,t49,'|')
 42   format('Inflow top:',t49,'|',/,'- direct precipit.',t19,3f9.2,
     &       t49,'|',/,'- overland flow',t19,3f9.2,t49,'|')
 43   format('Exfiltration matrix:',t49,'|',t52,'Infiltration matrix:',/
     &,'- interflow',t20,f8.2,2f9.2,t49,'|',t52,'- unsaturated',
     &t71,f8.2,2f9.2,/,'- saturated',t20,f8.2,2f9.2,t49,'|',t52,
     &'- saturated',t71,f8.2,2f9.2)
 44   format(t49,'|',t52,'Rapid drainage',t70,f9.2,9x,f9.2,/,
     &48('='),'+',50('='))
 45   format('Sum',t19,3f9.2,t49,'|',t52,'Sum',t70,3f9.2,/,48('='),'+',
     &50('='))
 46   format('Storage Change',t19,3f9.2)
 47   format('Balance Deviation',t19,3f9.2,/,99('='),/)
  
      return
      end

! ----------------------------------------------------------------------
      subroutine outshrinkchar ()
! ----------------------------------------------------------------------
!     date               : April 2008
!     purpose            : Output of shrinkage characteristics as generated 
!                          on basis of input parameters
! ---------------------------------------------------------------------
! --- global
      use Variables
      implicit none 

! --- local variables ------------------
      character filnam*300,filtext*80,comma*1
      integer   getun,shr,lay,i
      real*8    MoisR, SHRINK, Thet, VlSolidRel, VoidR, VRhlp
! ---------------------------------------------------------------------
      comma = ',' 

! === open output file =================================================
      filnam = trim(pathwork)//'SoilShrinkChar.csv'
      shr = getun (20,90)
      call fopens(shr,filnam,'new','del')
      filtext = 'soil shrinkage characteristics'
      call writehead (shr,1,filnam,filtext,project)

! --- write header
      write(shr,'(a)') 
     &    'layer,Moist Ratio (-),Void Ratio (-)'

! --- generate and write shrink characteristics for each soil layer
      do lay = 1,numlay
         if (SwSoilShr(lay).ne.0) then
            VlSolidRel= 1.d0 - Thetsl(lay)
            MoisR = 0.d0            
            do i = 1, 101
               Thet =  MoisR * (1.d0-Thetsl(lay))
               VRhlp = SHRINK(SwSoilShr(lay),SwShrInp(lay),ShrParA(lay),
     &                        ShrParB(lay),ShrParC(lay),ShrParD(lay),
     &                        ShrParE(lay),Thet,Thetsl(lay))
               VoidR = (Thetsl(lay) - VRhlp) / VlSolidRel
               write (shr,22) lay, comma, MoisR, comma, VoidR
 22            format(i10,2(a,f9.4))
!
               MoisR = MoisR + Thetsl(lay) / (1.d0-Thetsl(lay)) / 100.d0
            enddo
         endif
      enddo

! --- close soil shrinkage characteristics file
      close (shr)

      return
      end
