! File VersionID:
!   $Id: cropgrowth.for 197 2011-02-18 10:42:47Z kroes006 $
! ----------------------------------------------------------------------
      subroutine CropGrowth(task) 
! ----------------------------------------------------------------------
!     Date               : Aug 2004   
!     Purpose            : Call proper crop routines for initialization,
!                          calculation of rate/state variables and output
! ----------------------------------------------------------------------

      use variables
      implicit none

      integer task,numcrop
      character messag*200

      goto (1000, 2000, 3000) task

1000  continue

! === initialization =========================================================

      if (flbaresoil) then
! --- set bare soil condition  -----------------------------------------------
        call nocrop (rd,lai,cf,ch,albedo,rsc)
      else
! ---   fixed crop development -----------------------------------------------
        if (croptype(icrop) .eq. 1) call CropFixed(1)
! ---   detailed crop growth -------------------------------------------------
        if (croptype(icrop) .eq. 2) call Wofost(1)
! ---   detailed grass growth  -----------------------------------------------
        if (croptype(icrop) .eq. 3) call Grass(1)
! ---   detailed sugarcane growth --------------------------------------------
        if (croptype(icrop) .eq. 4) call Samuca(1)
      endif

!     error message in case of wofost crop growht and initial data from file 
      if(.not.flbaresoil) then
        if(swinco.eq.3 .and. croptype(icrop).ge.2) then
          write(messag,'(2a)')
     &     'Warning: Wofost crop growth in combination with ',
     &     'initial input from file is not implemented yet !'
          call warn ('CropGrowth',messag,logf,swscre)
        endif
      endif

      return

2000  continue
      
! === calculation of potential crop rate and state variables =================

! --- number of crop
      if (flcropend) then
        numcrop = icrop-1
      else
        numcrop = icrop
      endif

! --- detailed crop growth -------------------------------------------------
      if(numcrop.gt.0) then
        if (croptype(numcrop) .eq. 2) call Wofost(2)
        if (croptype(numcrop) .eq. 3) call Grass(2)
        if (croptype(numcrop) .eq. 4) call Samuca(2)
      endif

      return

3000  continue

! === calculation of actual crop rate and state variables =================

! --- number of crop
      if (flcropend) then
        numcrop = icrop-1
      else
        numcrop = icrop
      endif

      if(numcrop.gt.0) then
! ---   fixed crop development -----------------------------------------------
        if (croptype(numcrop) .eq. 1) call CropFixed(3)
! ---   detailed crop growth -------------------------------------------------
        if (croptype(numcrop) .eq. 2) call Wofost(3)
! ---   detailed grass growth  -----------------------------------------------
        if (croptype(numcrop) .eq. 3) call Grass(3)
! ---   detailed sugarcane growth --------------------------------------------
        if (croptype(icrop) .eq. 4) call Samuca(3)
      endif

      return
      end 

! ----------------------------------------------------------------------
      subroutine astro 
     &   (logf,swscre,daynr,lat,dayl,daylp,sinld,cosld,dsinb,dsinbe,dso)
! ----------------------------------------------------------------------
! Subroutine astro (daynr,lat,dayl,daylp,sinld,cosld)
! Authors: this routine Astro is based on Sastro (Daniel van Kraalingen)
! Date   : 28-November-2005 
! Purpose: This subroutine calculates solar constant, daily
!          extraterrestrial radiation, daylength and some intermediate
!          variables required by other routines. The routine has been
!          rewritten such that latitudes from pole to pole can be used.
!
! Formal parameters:  (I=input,O=output,C=control,IN=init,T=time)
! name   type meaning                                     units  class
! ----   ---- -------                                     -----  -----
! logf    I4  Internal number of logbook output file *.LOG   -      I
! swscre  I4  Switch of screen display:  0 = no display;     -      I
!             1 = summary water balance; 2 = daynumber
! daynr   I4  Day of year (Jan 1st = 1)                      d      I  
! lat     R8  Latitude of the site                       degrees    I  
! dayl    R8  Astronomical daylength (base = 0 degrees)      h      O  
! daylp   R8  Photoperiodic daylength (base = -4 degrees)    h      O  
! sinld   R8  Intermediate variable for other subroutine     -      O  
! cosld   R8  Intermediate variable for other subroutine     -      O  
! dsinb   R8  Daily total of sine of solar height            s      O  
! dsinbe  R8  Daily integral of sine of solar height         s      O  
!             corrected for lower transmission at low                  
!             elevation                                                
! sc      R8  Solar constant at day=daynr                   W/m2     O  
! dso     R8  Daily extraterrestrial radiation            J/m2/d    O  
!                                                                      
! Fatal error checks (on input): lat > 90, lat < -90
! Warnings          : lat above polar circle, lat within polar circle  
! Subprograms called: Warning
! File usage        : none
!----------------------------------------------------------------------
      implicit none
 
!     formal parameters
      integer logf,swscre,daynr
      real*8  lat,dayl,daylp,sinld,cosld,dsinb,dsinbe,dso

!     local parameters
      real*8  angle,aob,dec,pi,rad,zza,zzcos,zzsin,sc,help1
      character messag*200

      data    pi /3.1415926d0/,angle /-4.0d0/
! ----------------------------------------------------------------------
! --- declination of the sun as a function of daynr
!     (see ref.manual: Radiation term: 23.45*rad=0.409 en (90-10)*rad=1.39)
      rad = pi/180.d0
      dec = -asin(sin(23.45d0*rad)*cos(2.d0*pi*dble(daynr+10)/365.0d0))
! --- some intermediate variables
      sinld = sin(rad*lat)*sin(dec)
      cosld = cos(rad*lat)*cos(dec)
      aob = sinld/cosld
! --- calculation of daylenght and photoperiodic daylength
!     solution for polar circle altutude adopted from 
!     Daniel van Kraalingen (routine Sastro, dd 12-june-1996,version 1.1)
      if (aob.lt.-1.0d0) then
        messag = 'Warning: latitude above polar circle, daylength= 0hrs'
        call warn ('Astro',messag,logf,swscre)
        dayl = 0.0d0
        zzcos =  0.0d0
        zzsin =  1.0d0
      else if (aob.gt.1.0d0) then
        messag = 'Warning: latitude within polar circle,daylength=24hrs'
        call warn ('Astro',messag,logf,swscre)
        dayl = 24.0d0
        zzcos =  0.0d0
        zzsin = -1.0d0
      else
        dayl  = 12.0d0*(1.0d0+2.0d0*asin(aob)/pi)
        help1 = (-sin(angle*rad)+sinld)/cosld
        if (help1.gt.1.0d0) then
          daylp = 24.0d0
        else
          daylp = 12.0d0*(1.0d0+2.0d0*asin(help1)/pi)
        endif
!        write(logf,*) 'help1=',help1,'daylp=',daylp
        zza   = pi*(12.0d0+dayl)/24.0d0
        zzcos = cos (zza)
        zzsin = sin (zza)
      endif

!     Daily integral of sine of solar height (DSINB) with a
!     correction for lower atmospheric transmission at lower solar
!     elevations (DSINBE)
      dsinb  = 2.0d0*3600.0d0*(dayl*0.50d0*sinld-12.0d0*cosld*zzcos/pi)
      dsinbe = 2.0d0*3600.0d0*(dayl*(0.50d0*sinld+0.20d0*sinld**2.0d0+
     &      0.10d0*cosld**2.0d0)-(12.0d0*cosld*zzcos+
     &      9.6d0*sinld*cosld*zzcos+2.4d0*cosld**2.0d0*zzcos*zzsin)/pi)

!     Solar constant and daily extraterrestrial radiation
      sc = 1370.0d0*(1.0d0+0.033d0*cos (2.0d0*pi*daynr/365.d0))
      dso  = sc*dsinb

      return
      end
! ----------------------------------------------------------------------
      subroutine cropfixed (task)
! ----------------------------------------------------------------------
!     date               : august 2004                           
!     purpose            : simple crop growth routine for swap 
! ----------------------------------------------------------------------
      use variables
      implicit none

! --- local variables
      integer stepnr,i,icgs,task,lcc,node
      
      real*8  gctb(2*magrs),rdtb(2*magrs),kytb(2*magrs),nihil,
     &        afgen,cptr0,ctr0,cptr(magrs),ctr(magrs),crt(magrs),
     &        rely(magrs),help(magrs),dtsum,dvr,phead                   ! NwRootExtr

      parameter (nihil=1.0d-7)

      save
! ----------------------------------------------------------------------

      goto (1000,2000,3000) task

1000  continue

! === initialization ===================================================

! --- read crop data
      gctb = 0.0d0
      cftb = 0.0d0
      chtb = 0.0d0
      rdtb = 0.0d0
      call readcropfixed (cropfil(icrop),pathcrop,idev,lcc,tsumea,
     & tsumam,tbase,kdif,kdir,gctb,swgc,cftb,swcf,rdtb,rdctb,hlim1,
     & hlim2u,hlim2l,hlim3h,hlim3l,hlim4,rsc,adcrh,adcrl,kytb,
     & cofab,logf,schedule,swinter,pfreetb,pstemtb,scanopytb,
     & avprectb,avevaptb,cumdens,chtb,albedo,swetr,
     & flsolute,ecmax,ecslop,c2eca,c2ecb,c2ecf,numlay,
     & alphacrit,swroottyp,wiltpoint,rootradius,rootcoefa,rsw)           ! NwRootExtr

! --- development stage
      dvs = 0.0d0

! --- initial lai or sc
      lai = afgen (gctb,(2*magrs),dvs)
      if (swgc.eq.2) then
        gc = lai
        lai = lai*3.0d0
      endif

! --- initial crop factor or crop height
      cf = afgen (cftb,(2*magrs),dvs)
      ch = afgen (chtb,(2*magrs),dvs)

! --- actual rooting depth [cm]
      rd = min (rds,afgen (rdtb,(2*magrs),dvs))

! --- initial summation variables of the crop
      cptr0 = 0.
      ctr0 = 0.

! --- init arrays with cum. pot. and act. transpiration 
      cptr = 0.0d0
      ctr = 0.0d0

! --- initialize matric flux potential                                  ! NwRootExtr
      phead = 0.d0   ! dummy                                            !
      if (swroottyp .eq. 2) then                                        !
        node = 1                                                        ! dummy node nr
        call MatricFlux(1,phead,node)                                   !
! ---   output of matric flux potential                                 !
!        if (swmfp.eq.1) call outmatricflux(2,mfp,numnod,tcum,          !
!     &   mflux,z,outfil,pathwork,project,ptra,h)                       !
      endif                                                             ! NwRootExtr

      return          

2000  continue

! === calculate potential rate and state variables ======================

3000  continue

! === calculate actual rate and state variables ======================

! --- increase in temperature sum
      dtsum = max (0.0d0,tav-tbase)

! --- development rate
      if (idev.eq.1) then
        dvr = 2.0/lcc
      elseif (idev.eq.2) then
        if (dvs.lt.1.0d0) then
          dvr = dtsum/tsumea
        else
          dvr = dtsum/tsumam
        endif
      endif

! --- determination of current growing stage
      do i = 1,magrs
        help(i) = kytb(2*i-1)
      end do
      icgs = stepnr(help,magrs,dvs)

! --- water stress
      if(abs(ptra).lt.nihil) then
        reltr = 1.0d0
      else
        reltr = max(min(tra/ptra,1.0d0),0.0d0)
      endif

! ----integrals of the crop --------------------------------------------

! --- phenological development stage
      dvs = dvs+dvr

! --- leaf area index or soil cover fraction    
      lai = afgen (gctb,(2*magrs),dvs)
      if (swgc.eq.2) then
        gc = lai
        lai = lai*3.0d0
      endif

! --- crop factor or crop height
      cf = afgen (cftb,(2*magrs),dvs)
      ch = afgen (chtb,(2*magrs),dvs)

! --- rooting depth [cm]
      rd = min (rds,afgen (rdtb,(2*magrs),dvs))

! --- cumulative relative transpiration, total growing season 
      cptr0 = cptr0 + ptra  
      ctr0 = ctr0  + tra
      if (cptr0.le.nihil) then
        crt0 = 0.0d0
      else
        crt0 = max(min(ctr0/cptr0,1.0d0),0.0d0)
      endif

! --- cumulative relative transpiration, current growing stage
      cptr(icgs) = cptr(icgs) + ptra
      ctr(icgs) = ctr(icgs)  + tra
      if (cptr(icgs).le.nihil) then
        crt(icgs) = 0.0d0
      else
        crt(icgs) = max(min(ctr(icgs)/cptr(icgs),1.0d0),0.0d0)
      endif

! --- relative yield per growing stage and cumulated
      crely = 1.0d0 
      do i = 1,icgs
        rely(i) = 1.0d0-((1.0d0-crt(i))*kytb(2*i))
        crely = crely*rely(i)
      end do

      return
      end

! ----------------------------------------------------------------------
      subroutine cropoutput(task) 
! ----------------------------------------------------------------------
!     Date               : Aug 2004   
!     Purpose            : open and write crop output files 
! ----------------------------------------------------------------------

      use variables
      implicit none

! --- local variables ------------------
      integer task,getun,numcrop
      character messag*200
      character*160 filnam,filtext

      goto (1000, 2000, 3000) task

1000  continue

! === open crop output file and write headers =====================
      
! --- open crop output file
      if (flopencropoutput) then
! ---   open crop output file and write general header
        if (trim(outfil).eq.trim(cropfil(1))) then
          Messag = 'The name of the input crop-file 
     &    ('//trim(cropfil(icrop))//') cannot be equal to the name of '
     &   //'the output crop-file '//trim(outfil)//' Adjust a filename !'
          call fatalerr ('crops',messag)
        endif
        filnam = trim(pathwork)//trim(outfil)//'.crp'
        crp = getun (20,90)
        call fopens(crp,filnam,'new','del')
        filtext = 'output data of simple or detailed crop growth model'
        call writehead (crp,1,filnam,filtext,project)

! ---   write header fixed crop growth
        if (croptype(icrop) .eq. 1) 
     &    call OutCropFixed(1,date,t,daycrop,dvs,lai,cf,rd,
     &          crt0,crely,crp,ch)
! ---   write header detailed crop growth 
        if (croptype(icrop) .eq. 2) 
     &    call OutWofost(1,date,daycrop,crp,t,dvs,lai,cf,rd,ch,
     &                        crt0,crely,crt1,cwdmpot,cwdm,wsopot,wso)
! ---   write header detailed grass growth
        if (croptype(icrop) .eq. 3) 
     &    call OutGrass(1,date,daycrop,crp,t,lai,rd,dvs,cf,ch,
     &                    crt0,crt1,tagppot,tagp,tagptpot,tagpt)

          flopencropoutput = .false.

      else
! ---   header for second and subsequent crops

! ---   write header fixed crop growth
        if (croptype(icrop).eq.1 .and. swheader.eq.1) 
     &    call OutCropFixed(1,date,t,daycrop,dvs,lai,cf,rd,
     &           crt0,crely,crp,ch)

! ---   write header detailed crop growth 
        if (croptype(icrop).eq.2 .and. swheader.eq.1) 
     &    call OutWofost(1,date,daycrop,crp,t,dvs,lai,cf,rd,ch,
     &                        crt0,crely,crt1,cwdmpot,cwdm,wsopot,wso)
! ---   write header detailed grass growth
        if (croptype(icrop).eq.3 .and. swheader.eq.1) 
     &    call OutGrass(1,date,daycrop,crp,t,lai,rd,dvs,cf,ch,
     &                    crt0,crt1,tagppot,tagp,tagptpot,tagpt)

      endif

      return

2000  continue

! --- write actual data ----------------------------------------------------

! --- number of crop
      if (flcropend) then
        numcrop = icrop-1
      else
        numcrop = icrop
      endif

! --- fixed crop file
      if (croptype(numcrop) .eq. 1) 
     &  call OutCropFixed(2,date,t,daycrop,dvs,lai,cf,rd,
     &           crt0,crely,crp,ch)

! --- detailed crop growth 
      if (croptype(numcrop) .eq. 2) 
     &  call OutWofost(2,date,daycrop,crp,t,dvs,lai,cf,rd,ch,
     &                        crt0,crely,crt1,cwdmpot,cwdm,wsopot,wso)

! --- detailed grass growth
      if (croptype(numcrop) .eq. 3) 
     &  call OutGrass(2,date,daycrop,crp,t,lai,rd,dvs,cf,ch,
     &                    crt0,crt1,tagppot,tagp,tagptpot,tagpt)

      return

3000  continue
! --- close crop output file ------------------------------------------------

      close (crp)

      return
      end 

! ----------------------------------------------------------------------
      subroutine nocrop (rd,lai,cf,ch,albedo,rsc)
! ----------------------------------------------------------------------
      real*8  rd,lai,cf,ch,albedo,rsc
! ----------------------------------------------------------------------
      rd = 0.0d0
      lai = 0.0d0
      cf = 0.0d0
      ch = 12.d0
      albedo = 0.23d0
      rsc = 70.d0

      return
      end

! ----------------------------------------------------------------------
      subroutine readcropfixed (crpfil,pathcrop,idev,lcc,tsumea,
     & tsumam,tbase,kdif,kdir,gctb,swgc,cftb,swcf,rdtb,rdctb,hlim1,
     & hlim2u,hlim2l,hlim3h,hlim3l,hlim4,rsc,adcrh,adcrl,kytb,
     & cofab,logf,schedule,swinter,pfreetb,pstemtb,scanopytb,
     & avprectb,avevaptb,cumdens,chtb,albedo,swetr,
     & flsolute,ecmax,ecslop,c2eca,c2ecb,c2ecf,numlay,
     & alphacrit,swroottyp,wiltpoint,rootradius,rootcoefa,rsw)              ! NwRootExtr
! ----------------------------------------------------------------------
!     Update             : July 2009
!     date               : July 2002             
!     purpose            : get crop parameters from cropfile
! ----------------------------------------------------------------------
      implicit  none
      include  'arrays.fi'
      
      integer   crp,idev,i,lcc,swgc,swcf,logf,ifnd,getun2,schedule
      integer   swinter,swetr,numlay,swroottyp                              ! NwRootExtr
      logical   flsolute 
      real*8    gctb(2*magrs),cftb(2*magrs),rdtb(2*magrs),kytb(2*magrs)
      real*8    adcrh,adcrl,tbase,tsumam,tsumea,rdctb(22)
      real*8    hlim1,hlim2u,hlim2l,hlim3h,hlim3l,hlim4,rsc
      real*8    sum,afgen,kdif,kdir,cofab,chtb(2*magrs)
      real*8    tinter(magrs),pfree(magrs),pstem(magrs)
      real*8    scanopy(magrs),avprec(magrs),avevap(magrs)
      real*8    pfreetb(2*magrs),pstemtb(2*magrs),scanopytb(2*magrs)
      real*8    avprectb(2*magrs),avevaptb(2*magrs)
      real*8    depth,rootdis(202),cumdens(202)
      real*8    dvsinput(magrs),cfinput(magrs),chinput(magrs)
      real*8    ecmax,ecslop,c2eca,c2ecb,c2ecf(maho),albedo,alphacrit
      real*8    wiltpoint,rootradius,rootcoefa,rsw                          ! NwRootExtr
      logical   rdinqr
      character crpfil*(*),pathcrop*(*)
! locals
      integer   swc2ecf
      character message*200,filnam*200
! ----------------------------------------------------------------------

! --- initialise and start reading
      filnam = trim(pathcrop)//trim(crpfil)//'.crp'
      crp = getun2 (10,90,2)
      call rdinit(crp,logf,filnam)

! --- phenology
      call rdsinr ('idev',1,2,idev)
      if (idev.eq.1) then
        call rdsinr ('lcc',1,366,lcc)
      elseif (idev.eq.2) then
        call rdsdor ('tsumea',0.0d0,10000.0d0,tsumea)
        call rdsdor ('tsumam',0.0d0,10000.0d0,tsumam)
        call rdsdor ('tbase',-10.0d0, 30.0d0,tbase)
      endif

! --- assimilation                        
      call rdsdor ('kdif',0.0d0,2.0d0,kdif)
      call rdsdor ('kdir',0.0d0,2.0d0,kdir)
     
! --- LAI or soil cover fraction 
      call rdsinr ('swgc',1,2,swgc)
      if (swgc.eq.1) then
        call rdador ('gctb',0.0d0,12.0d0,gctb,(2*magrs),ifnd)
      elseif (swgc.eq.2) then
        call rdador ('gctb',0.0d0,2.0d0,gctb,(2*magrs),ifnd)
      endif

! --- Crop factor or crop height
      call rdsinr ('swcf',1,2,swcf)

! --- check use of crop factors in case of ETref
      if (swetr.eq.1 .and. swcf.eq.2) then
        message = 'If ETref is used (SWETR = 1), always define crop '//
     &           'factors (SWCF = 1)' 
        call fatalerr ('ReadCropFixed',message)
      endif

      if (swcf.eq.1) then
! ---   crop factor is input
        call rdador ('dvs',0.0d0,2.0d0,dvsinput,(magrs),ifnd)
        call rdfdor ('cf',0.0d0,2.0d0,cfinput,(magrs),ifnd)
! ---   store values in cftb
        do i = 1,ifnd
          cftb(i*2) = cfinput(i) 
          cftb(i*2-1) = dvsinput(i)
        enddo
        chtb = -99.99d0
      else
! ---   crop height is input
        call rdador ('dvs',0.0d0,2.0d0,dvsinput,(magrs),ifnd)
        call rdfdor ('ch',0.0d0,1.0d4,chinput,(magrs),ifnd)
! ---   store values in chtb
        do i = 1,ifnd
          chtb(i*2) = chinput(i) 
          chtb(i*2-1) = dvsinput(i)
        enddo
        cftb = -99.99d0
      endif

! --- reflection coefficient and crop resistance
      if (swcf.eq.1) then
! ---   use standard values for ETref
        albedo = 0.23d0
        rsc = 70.0d0
        rsw = 0.0d0
      else
! ---   use crop specific values
        call rdsdor ('albedo',0.0d0,1.0d0,albedo)
        call rdsdor ('rsc',0.0d0,1.0d6,rsc)
        call rdsdor ('rsw',0.0d0,1.0d6,rsw)
      endif

! --- rooting depth
      call rdador ('rdtb',0.0d0,1000.0d0,rdtb,(2*magrs),ifnd)

! --- yield response
      call rdador ('kytb',0.0d0,5.0d0,kytb,(2*magrs),ifnd)

! --- water use
      swroottyp = 1
      if(rdinqr('swroottyp')) then
        call rdsinr ('swroottyp',1,2,swroottyp)                         ! NwRootExtr
      endif
      if (swroottyp.eq.1) then                                          !
         call rdsdor ('hlim1' ,-100.0d0,100.0d0,hlim1)                  !
         call rdsdor ('hlim2u',-1000.0d0,100.0d0,hlim2u)                !
         call rdsdor ('hlim2l',-1000.0d0,100.0d0,hlim2l)                !
         call rdsdor ('hlim3h',-10000.0d0,100.0d0,hlim3h)               !
         call rdsdor ('hlim3l',-10000.0d0,100.0d0,hlim3l)               !
         call rdsdor ('hlim4' ,-16000.0d0,100.0d0,hlim4)                !
         call rdsdor ('adcrh',0.0d0,5.0d0,adcrh)                        !
         call rdsdor ('adcrl',0.0d0,5.0d0,adcrl)                        !
!       Criticial stress index for compensation of root water uptake (-)
        alphacrit = 1.0d0
        if(rdinqr('alphacrit')) then
          call rdsdor ('alphacrit',0.2d0,1.0d0,alphacrit)
        endif
      else                                                              !
        call rdsdor ('wiltpoint',-1.0d6,-1.0d2,wiltpoint)               !
        call rdsdor ('rootradius',0.0001d0,1.0d0,rootradius)            !
        call rdsdor ('rootcoefa',0.0d0,1.0d0,rootcoefa)                 !
      endif                                                             ! NwRootExtr


! --- salt stress
      if (flsolute) then
        call rdsdor ('ecmax', 0.0d0,20.0d0,ecmax)
        call rdsdor ('ecslop',0.0d0,40.0d0,ecslop)
        call rdsdor ('c2eca', 0.0d0,1000.0d0,c2eca)
        call rdsdor ('c2ecb', 0.0d0,10.0d0,c2ecb)
        call rdsinr ('swc2ecf',1,2,swc2ecf)
        if (swc2ecf.eq.1) then
          call rdsdor ('c2ecf', 0.d0, 10.d0, c2ecf(1))
          if(numlay.ge.2) then
            do i = 2,numlay
              c2ecf(i) = c2ecf(1)
            enddo
          endif
        else if (swc2ecf.eq.2) then
          call rdfdor ('c2ecf', 0.d0, 10.d0, c2ecf,maho,numlay)
        endif
      endif

! --- interception
      call rdsinr ('swinter',0,2,swinter)
      if (swinter .eq. 1) then
        call rdsdor ('cofab',0.0d0,1.0d0,cofab)
      else if (swinter .eq. 2) then
        call rdador ('t',0.d0,366.d0,tinter,(magrs),ifnd)
        call rdfdor ('pfree',0.d0,1.d0,pfree,(magrs),ifnd)
        call rdfdor ('pstem',0.d0,1.d0,pstem,(magrs),ifnd)
        call rdfdor ('scanopy',0.d0,10.d0,scanopy,(magrs),ifnd)
        call rdfdor ('avprec',0.d0,100.d0,avprec,(magrs),ifnd)
        call rdfdor ('avevap',0.d0,10.d0,avevap,(magrs),ifnd)
        do i = 1, ifnd
          pfreetb(i*2) = pfree(i)
          pfreetb(i*2-1) = tinter(i)
          pstemtb(i*2) = pstem(i)
          pstemtb(i*2-1) = tinter(i)
          scanopytb(i*2) = scanopy(i)
          scanopytb(i*2-1) = tinter(i)
          avprectb(i*2) = avprec(i)
          avprectb(i*2-1) = tinter(i)
          avevaptb(i*2) = avevap(i)
          avevaptb(i*2-1) = tinter(i)
        end do
      endif

! --- read table with root distribution coefficients
      call rdador ('rdctb',0.0d0,100.0d0,rdctb,22,ifnd)
      
! --- determine whether irrigation scheduling is applied
      call rdsinr ('schedule',0,1,schedule)

! --- close file with crop data
      close (crp)


! --- CALCULATE NORMALIZED CUMULATIVE ROOT DENSITY FUNCTION
      if (swroottyp .eq. 1) then                                        !
! ---   root water extraction according to Feddes function              ! NwRootExtr
              

! ---   specify array ROOTDIS with root density distribution
        do i = 0,100
          depth = 0.01d0 * dble(i)
          rootdis(i*2+1) = depth
          rootdis(i*2+2) = afgen(rdctb,22,depth)
        enddo
        
! ---   calculate cumulative root density function
        do i = 1,202,2
! ---     relative depths
          cumdens(i) = rootdis(i)
        enddo
        sum = 0.d0
        cumdens(2) = 0.d0
        do i = 4,202,2
! ---     cumulative root density
          sum = sum + (rootdis(i-2)+rootdis(i)) * 0.5d0
     &               * (cumdens(i-1)-cumdens(i-3))
          cumdens(i) = sum
        enddo

! ---   normalize cumulative root density function to one
        do i = 2,202,2
          cumdens(i) = cumdens(i) / sum
        enddo
        endif                                                             ! NwRootExtr
      
      return
      end
! ----------------------------------------------------------------------
      subroutine wofost(task)
! ----------------------------------------------------------------------
!     date               : october 2004
!     purpose            : detailed crop growth routine
! ----------------------------------------------------------------------
      use variables
      implicit none
 
      integer   i1,ilvold,task,ilvoldpot,node

      real*8    lv(366),lvage(366),sla(366)
      real*8    lvpot(366),lvagepot(366),slapot(366)
      real*8    wlv,wrt,wst
      real*8    admi,afgen,amax,asrc,ccheck,cosld,cvf
      real*8    laicr,laiexp,laimax,lasum,mres,mrest,rdm
      real*8    dalv,dayl,delt,dmi,drlv,cptr0,ctr0,cptr1,ctr1
      real*8    drrt,drst,dslv,dslv1,dslv2,dslvt,dteff,dtga,dtsum,dvr
      real*8    dvred,dwlv,dwrt,dwst,fcheck,fl,fo,fr,fs
      real*8    fysdel,gass,gasst,gla,glaiex,glasol,grlv,grrt,grst
      real*8    gwrt,gwso,gwst,pgass,rest,rmres,rr
      real*8    sinld,slat,tadw,teff,twlv,twst
      real*8    wlvpot,lasumpot,wrtpot,wstpot
      real*8    dwrtpot,dwlvpot,dwstpot,dtgapot,tadwpot
      real*8    pgasspot,gasspot,rmrespot,mrespot,asrcpot,dmipot,rrpot
      real*8    admipot,grrtpot,drrtpot,gwrtpot,grlvpot
      real*8    dslvpot,restpot,dalvpot,drlvpot,gwsopot
      real*8    glasolpot,slatpot,glapot,grstpot,drstpot,gwstpot
      real*8    dslvtpot,twlvpot,twstpot
      real*8    dsinb,dsinbe,dso
      real*8    nihil,phead,dvspast1
      character tmp*11,messag*200
d     real*8    co2rootfix,co2rootloss,co2shootfix
d     character komma*1

      parameter (nihil=1.0d-7)
      parameter (delt=1.0d0)

      save
! ----------------------------------------------------------------------
      goto (1000,2000,3000) task

1000  continue

! === initialization ====================================================

! --- read crop data
      call readwofost (cropfil(icrop),pathcrop,swcf,cftb,idsl,dlo,dlc,
     &  tsumea,tsumam,dtsmtb,dvsend,tdwi,laiem,rgrlai,slatb,spa,
     &  ssa,span,tbase,kdif,kdir,eff,amaxtb,tmpftb,tmnftb,cvl,cvo,cvr,
     &  cvs,q10,rml,rmo,rmr,rms,rfsetb,frtb,fltb,fstb,fotb,perdl,rdrrtb,
     &  rdrstb,hlim1,hlim2u,hlim2l,hlim3h,hlim3l,hlim4,rsc,adcrh,adcrl,
     &  cofab,rdi,rri,rdc,rdctb,logf,schedule,cumdens,chtb,albedo,swetr,
     &  flsolute,ecmax,ecslop,c2eca,c2ecb,c2ecf,numlay,relni,cfet,
     &  alphacrit,swroottyp,wiltpoint,rootradius,rootcoefa,rsw)          ! NwRootExtr


d     open(unit=87,file='crop_CO2.csv',status='unknown')
d     write(87,*) 'date,co2rootfix,co2rootloss,co2shootfix'
d     open(unit=88,file='crop_dm.csv',status='unknown')
d     write(88,*) 'date,wrtpot,wrt,wstpot,wst,rdpot,rd,laipot,lai,
d    &gwrt,gwst,drrt,drlv,drst'

! --- maximum rooting depth & actual rooting depth
      rdm = min(rds,rdc)
      rd = min(rdi,rdm)
      rdpot = min(rdi,rdm)

! --- initial values of crop parameters
      swinter = 1
      dvs = 0.
      fr = afgen (frtb,30,dvs)
      fl = afgen (fltb,30,dvs)
      fs = afgen (fstb,30,dvs)
      fo = afgen (fotb,30,dvs)
      sla(1) = afgen (slatb,30,dvs)
      lvage(1) = 0.0d0
      ilvold = 1
      slapot(1) = afgen (slatb,30,dvs)
      lvagepot(1) = 0.0d0
      ilvoldpot = 1

! --- initial state variables of the crop
      wrt = fr*tdwi
      wrtpot = wrt
      tadw = (1.0d0-fr)*tdwi
      tadwpot = tadw
      wst = fs*tadw
      wstpot = wst
      wso = fo*tadw
      wsopot = wso
      wlv = fl*tadw
      wlvpot = wlv
!      laiem = wlv*sla(1)  is input !
      lv(1) = wlv
      lvpot(1) = wlv
      lasum = laiem     
      lasumpot = laiem     
      laiexp = laiem     
      glaiex = 0.0d0
      laimax = laiem
      lai = lasum+ssa*wst+spa*wso 
      laipot = lai 
      dwrt = 0.0d0
      dwrtpot = 0.0d0
      dwlv = 0.0d0
      dwlvpot = 0.0d0
      dwst = 0.0d0
      dwstpot = 0.0d0
      cf = afgen (cftb,(2*magrs),dvs)
      ch = afgen (chtb,(2*magrs),dvs)

! --- initial summation variables of the crop
      gasst = 0.0d0
      gasstpot = 0.0d0
      mrest = 0.0d0 
      mrestpot = 0.0d0 
      cptr0 = 0.0d0
      ctr0 = 0.0d0
      cptr1 = 0.0d0
      ctr1 = 0.0d0
      cwdm = 0.0d0
      cwdmpot = 0.0d0

! --- initialize matric flux potential                                  ! NwRootExtr
      phead = 0.d0   ! dummy                                            !
      if (swroottyp .eq. 2) then                                        !
        node = 1                                                        ! dummy node nr
        call MatricFlux(1,phead,node)                                   !
! ---   output of matric flux potential                                 !
!        if (swmfp.eq.1) call outmatricflux(2,mfp,numnod,tcum,          !
!     &   mflux,z,outfil,pathwork,project,ptra,h)                       !
      endif                                                             ! NwRootExtr

      return

2000  continue

! === calculate potential rate and state variables =====================

! --- rates of change of the crop variables ----------------------------

! --- phenological development rate  
      call astro(logf,swscre,daymeteo,lat,
     &           dayl,daylp,sinld,cosld,dsinb,dsinbe,dso)

! --- increase in temperature sum
      dtsum = afgen (dtsmtb,30,tav)

      if (dvs.lt.1.0d0) then     
! --- development during vegetative phase
        dvred = 1.0d0
        if (idsl.ge.1) 
     &          dvred = max(0.0d0,min(1.0d0,(daylp-dlc)/(dlo-dlc)))
        dvr = dvred*dtsum/tsumea
      else
! --- development during generative phase
        dvr = dtsum/tsumam
      endif    

! --- Correction for transition to values above 1.0 
!     (as suggested by PVWalsum, 20090821)
      if (dvs .lt. 1.0d0 .and. (dvs+dvr*delt) .gt. 1.0d0) then
        dvspast1 = (dvs+dvr*delt) - 1.0d0
        dvspast1 = dvspast1*(tsumea/dvred)/tsumam
        dvr      = ((1.0d0 + dvspast1) - dvs)/delt
      endif


! == = daily dry matter production 

! --- gross assimilation

      amax = afgen (amaxtb,30,dvs)
! --- correction for sub-optimum average daytemperature
      amax = amax * afgen (tmpftb,30,tavd)
      call totass (daynr,dayl,amax,eff,laipot,kdif,rad,sinld,cosld,
     &             dtgapot)
! --- correction for low minimum temperature
      dtgapot = dtgapot * afgen (tmnftb,30,tmnr)
! --- potential assimilation in kg ch2o per ha
      pgasspot = dtgapot * 30.0d0/44.0

! --- water stress reduction of pgass to gass (not for potential conditions)
      reltr = 1.0d0
      gasspot = pgasspot * reltr

! --- respiration and partitioning of carbohydrates between growth and
! --- maintenance respiration
      rmrespot = (rmr*wrtpot+rml*wlvpot+rms*wstpot+rmo*wsopot)*
     &            afgen(rfsetb,30,dvs)
      teff = q10**((tav-25.0d0)/10.0d0)
      mrespot = dmin1(gasspot,rmrespot*teff)
      asrcpot = gasspot - mrespot

! --- partitioning factors
      fr = afgen(frtb,30,dvs)
      fl = afgen(fltb,30,dvs)
      fs = afgen(fstb,30,dvs)
      fo = afgen(fotb,30,dvs)
! --- check on partitioning
      fcheck = fr+(fl+fs+fo)*(1.0d0-fr) - 1.0d0
      if (abs(fcheck).gt.0.0001d0) then
        write(tmp,'(f6.3)') dvs
        tmp = adjustl (tmp)
        Messag ='The sum of partitioning factors for leaves, stems'//
     &    ' and storage organs is not equal to one at development stage'
     &    //trim(tmp)//'.'
        call fatalerr ('cropd',messag)
      endif

! --- dry matter increase
      cvf = 1.0d0/((fl/cvl+fs/cvs+fo/cvo)*(1.0d0-fr)+fr/cvr)
      dmipot = cvf*asrcpot
! --- check on carbon balance
      ccheck = (gasspot-mrespot-(fr+(fl+fs+fo)*(1.0d0-fr))*dmipot/cvf)
     &         /max(0.0001d0,gasspot)      
      if (abs(ccheck).gt.0.0001d0) then
        Messag ='The carbon balance is not correct'
        call fatalerr ('cropd',messag)
      endif

! == = growth rate by plant organ

! --- root extension
      rrpot = min (rdm-rdpot,rri)
      if (fr.le.0.0d0.or.pgasspot.lt.1.0d0) rrpot = 0.0d0

! --- growth rate roots and aerial parts
      admipot = (1.0d0-fr)*dmipot
      grrtpot = fr*dmipot
      drrtpot = wrtpot*afgen (rdrrtb,30,dvs)
      gwrtpot = grrtpot - drrtpot

! --- weight of new leaves
      grlvpot = fl*admipot

! --- death of leaves due to water stress or high lai
      laicr = 3.2d0/kdif
      dslvpot = wlvpot*max(0.0d0,min(0.03d0,0.03d0*
     &           (laipot-laicr)/laicr))

! --- death of leaves due to exceeding life span:

! --- first: leaf death due to water stress or high lai is imposed 
! ---        on array until no more leaves have to die or all leaves
! ---        are gone

      restpot = dslvpot*delt
      i1 = ilvoldpot

      do while (restpot.gt.lvpot(i1).and.i1.ge.1)
        restpot = restpot - lvpot(i1) 
        i1 = i1-1
      enddo

! --- then: check if some of the remaining leaves are older than span,
! ---       sum their weights

      dalvpot = 0.0d0
      if (lvagepot(i1).gt.span .and. restpot.gt.0.0d0 .and.i1.ge.1) then
        dalvpot = lvpot(i1) - restpot
        restpot = 0.0d0
        i1 = i1-1
      endif

      do while (i1.ge.1.and.lvagepot(i1).gt.span)
        dalvpot = dalvpot+lvpot(i1)
        i1 = i1-1
      enddo

      dalvpot = dalvpot/delt

! --- finally: calculate total death rate leaves
      drlvpot = dslvpot + dalvpot

! --- physiologic ageing of leaves per time step
      fysdel = max (0.0d0,(tav-tbase)/(35.0d0-tbase))

! --- specific leaf area valid for current timestep
      slatpot = afgen (slatb,30,dvs)

! --- calculation of specific leaf area in case of exponential growth:
! --- leaf area not to exceed exponential growth curve
      if (laiexp.lt.6.0d0) then
        dteff = max (0.0d0,tav-tbase)
! ---   increase in leaf area during exponential growth
        glaiex = laiexp*rgrlai*dteff
! ---   source-limited increase in leaf area
        glasolpot = grlvpot*slatpot
! ---   actual increase is determined by lowest value
        glapot = min (glaiex,glasolpot)
! ---   slat will be modified in case gla equals glaiex
        if (grlvpot.gt.0.0d0) slatpot = glapot/grlvpot
      endif  

! --- growth rate stems
      grstpot = fs*admipot
! --- death rate stems
      drstpot = afgen (rdrstb,30,dvs)*wstpot
! --- net growth rate stems
      gwstpot = grstpot - drstpot

! --- growth rate storage organs
      gwsopot = fo*admipot

! ----integrals of the crop --------------------------------------------

! --- leaf death (due to water stress or high lai) is imposed on array 
! --- untill no more leaves have to die or all leaves are gone

      dslvtpot = dslvpot*delt
      i1 = ilvoldpot
      do while (dslvtpot.gt.0.and.i1.ge.1)
        if (dslvtpot.ge.lvpot(i1)) then
          dslvtpot = dslvtpot-lvpot(i1)
          lvpot(i1) = 0.0d0
          i1 = i1-1
        else
          lvpot(i1) = lvpot(i1)-dslvtpot
          dslvtpot = 0.0d0
        endif
      enddo

! --- leaves older than span die
      do while (lvagepot(i1).ge.span.and.i1.ge.1)
        lvpot(i1) = 0.0d0
        i1 = i1-1
      enddo

! --- oldest class with leaves
      ilvoldpot = i1

! --- shifting of contents, updating of physiological age
      do i1 = ilvoldpot,1,-1
        lvpot(i1+1) = lvpot(i1)
        slapot(i1+1) = slapot(i1)
        lvagepot(i1+1) = lvagepot(i1)+fysdel*delt
      enddo
      ilvoldpot = ilvoldpot + 1

! --- new leaves in class 1
      lvpot(1) = grlvpot*delt
      slapot(1) = slatpot
      lvagepot(1) = 0.0d0 

! --- calculation of new leaf area and weight
      lasumpot = 0.0d0
      wlvpot = 0.0d0
      do i1 = 1,ilvoldpot
        lasumpot = lasumpot + lvpot(i1)*slapot(i1)
        wlvpot = wlvpot + lvpot(i1)
      enddo

! --- leaf area index in case of exponential growth
      laiexp = laiexp+glaiex*delt

! --- dry weight of living plant organs
      wrtpot = wrtpot + gwrtpot*delt
      wstpot = wstpot + gwstpot*delt
      wsopot = wsopot + gwsopot*delt

! --- total above ground biomass
      tadwpot = wlvpot + wstpot + wsopot
      tadwpot = tadwpot ! for Forcheck

! --- dry weight of dead plant organs (roots,leaves & stems)
      dwrtpot = dwrtpot + drrtpot*delt
      dwlvpot = dwlvpot + drlvpot*delt
      dwstpot = dwstpot + drstpot*delt

! --- dry weight of dead and living plant organs
      twlvpot = wlvpot + dwlvpot
      twstpot = wstpot + dwstpot
      cwdmpot = twlvpot + twstpot + wsopot

! --- total gross assimilation and maintenance respiration
      gasstpot = gasspot + gasstpot
      mrestpot = mrespot + mrestpot

! --- leaf area index
      laipot = lasumpot + ssa*wstpot + spa*wsopot

! --- rooting depth
      rdpot = rdpot + rrpot

      return

3000  continue

! === calculate actual rate and state variables =====================

! --- rates of change of the crop variables ----------------------------

!     correction of potential transpiration in relation to reference crop
!     (default = 1.0, range = 0.8 - 1.2)
      ptra = cfet*ptra

! --- gross assimilation

      call totass (daynr,dayl,amax,eff,lai,kdif,rad,sinld,cosld,dtga)
! --- correction for low minimum temperature
      dtga = dtga * afgen (tmnftb,30,tmnr)
! --- potential assimilation in kg ch2o per ha
      pgass = dtga * 30.0d0/44.0

! --- water stress reduction of pgass to gass
      if(abs(ptra).lt.nihil) then
        reltr = 1.0d0
      else
        reltr = max(0.0d0,min(1.0d0,tra/ptra))
      endif
      gass = pgass * reltr
! --- management factor 
!     (nitrogen and other forms of stress, not accounted for)
      gass = gass * relni

! --- respiration and partitioning of carbohydrates between growth and
! --- maintenance respiration
      rmres = (rmr*wrt+rml*wlv+rms*wst+rmo*wso)*afgen(rfsetb,30,dvs)
      mres = dmin1(gass,rmres*teff)
      asrc = gass-mres

! --- dry matter increase
      dmi = cvf*asrc
! --- check on carbon balance
      ccheck = (gass-mres-(fr+(fl+fs+fo)*(1.0d0-fr))*dmi/cvf)
     &         /max(0.0001d0,gass)      
      if (abs(ccheck).gt.0.0001d0) then
        Messag ='The carbon balance is not correct'
        call fatalerr ('cropd',messag)
      endif

! --- growth rate by plant organ

! --- root extension
      rr = min (rdm-rd,rri)
      if (fr.le.0.0d0.or.pgass.lt.1.0d0) rr = 0.0d0

! --- growth rate roots and aerial parts
      admi = (1.0d0-fr)*dmi
      grrt = fr*dmi
      drrt = wrt*afgen (rdrrtb,30,dvs)
      gwrt = grrt-drrt

!       CO2 fixation and loss
d       co2rootfix = grrt*44.0d0/33.0d0
d       co2rootloss = drrt*44.0d0/33.0d0


! --- weight of new leaves
      grlv = fl*admi

! --- death of leaves due to water stress or high lai
      if(abs(ptra).lt.nihil) then
        dslv1 = 0.0d0
      else
        dslv1 = wlv*(1.0d0-tra/ptra)*perdl
      endif
      laicr = 3.2d0/kdif
      dslv2 = wlv*max(0.0d0,min(0.03d0,0.03d0*(lai-laicr)/laicr))
      dslv = max (dslv1,dslv2) 

! --- death of leaves due to exceeding life span:

! --- first: leaf death due to water stress or high lai is imposed on array
! ---        until no more leaves have to die or all leaves are gone

      rest = dslv*delt
      i1 = ilvold

      do while (rest.gt.lv(i1).and.i1.ge.1)
        rest = rest-lv(i1) 
        i1 = i1-1
      enddo

! --- then: check if some of the remaining leaves are older than span,
! ---       sum their weights

      dalv = 0.0d0
      if (lvage(i1).gt.span .and. rest.gt.0.0d0 .and.i1.ge.1) then
        dalv = lv(i1)-rest
        rest = 0.0d0
        i1 = i1-1
      endif

      do while (i1.ge.1.and.lvage(i1).gt.span)
        dalv = dalv+lv(i1)
        i1 = i1-1
      enddo

      dalv = dalv/delt

! --- finally: calculate total death rate leaves
      drlv = dslv+dalv

! --- specific leaf area valid for current timestep
      slat = afgen (slatb,30,dvs)

! --- calculation of specific leaf area in case of exponential growth:
! --- leaf area not to exceed exponential growth curve
      if (laiexp.lt.6.0d0) then
! ---   source-limited increase in leaf area
        glasol = grlv*slat
! ---   actual increase is determined by lowest value
        gla = min (glaiex,glasol)
! ---   slat will be modified in case gla equals glaiex
        if (grlv.gt.0.0d0) slat = gla/grlv
      endif  

! --- growth rate stems
      grst = fs*admi
! --- death rate stems
      drst = afgen (rdrstb,30,dvs)*wst
! --- net growth rate stems
      gwst = grst-drst

! --- growth rate storage organs
      gwso = fo*admi

! ----integrals of the crop --------------------------------------------

! --- phenological development stage
      dvs = dvs+dvr*delt

! --- leaf death (due to water stress or high lai) is imposed on array 
! --- untill no more leaves have to die or all leaves are gone

      dslvt = dslv*delt
      i1 = ilvold
      do while (dslvt.gt.0.and.i1.ge.1)
        if (dslvt.ge.lv(i1)) then
          dslvt = dslvt-lv(i1)
          lv(i1) = 0.0d0
          i1 = i1-1
        else
          lv(i1) = lv(i1)-dslvt
          dslvt = 0.0d0
        endif
      enddo

! --- leaves older than span die
      do while (lvage(i1).ge.span.and.i1.ge.1)
        lv(i1) = 0.0d0
        i1 = i1-1
      enddo

! --- oldest class with leaves
      ilvold = i1

! --- shifting of contents, updating of physiological age
      do i1 = ilvold,1,-1
        lv(i1+1) = lv(i1)
        sla(i1+1) = sla(i1)
        lvage(i1+1) = lvage(i1)+fysdel*delt
      enddo
      ilvold = ilvold+1

! --- new leaves in class 1
      lv(1) = grlv*delt
      sla(1) = slat
      lvage(1) = 0.0d0 

! --- calculation of new leaf area and weight
      lasum = 0.0d0
      wlv = 0.0d0
      do i1 = 1,ilvold
        lasum = lasum+lv(i1)*sla(i1)
        wlv = wlv+lv(i1)
      enddo

! --- dry weight of living plant organs
      wrt = wrt+gwrt*delt
      wst = wst+gwst*delt
      wso = wso+gwso*delt

! --- total above ground biomass
      tadw = wlv+wst+wso

! --- dry weight of dead plant organs (roots,leaves & stems)
      dwrt = dwrt+drrt*delt
      dwlv = dwlv+drlv*delt
      dwst = dwst+drst*delt

! --- dry weight of dead and living plant organs
!     twrt = wrt+dwrt
      twlv = wlv+dwlv
      twst = wst+dwst
      cwdm = twlv+twst+wso

! --- total gross assimilation and maintenance respiration
      gasst = gass + gasst
      mrest = mres + mrest

! --- leaf area index
      lai = lasum+ssa*wst+spa*wso
! --- determine maximum lai
      laimax = max (lai,laimax)

! --- rooting depth
      rd = rd+rr

! --- crop factor or crop height
      cf = afgen (cftb,(2*magrs),dvs)
      ch = afgen (chtb,(2*magrs),dvs)

!     CO2 fixation and root,shoot developm
d     co2shootfix = tagp * 44.0d0/33.0d0
d     komma = ","
d     write(87,'(a12,1x,3(a,f12.4))') 
d    &  date,komma,co2rootfix,komma,co2rootloss,komma,co2shootfix
d     write(88,'(a12,1x,20(a,f12.4))') 
d    &  date,komma,wrtpot,komma,wrt,komma,wstpot,komma,wst,
d    &  komma,rdpot,komma,rd,komma,laipot,komma,lai,
d    &  komma,gwrt,komma,gwst,komma,drrt,komma,drlv,komma,drst

! --- cumulative relative transpiration
      cptr0 = cptr0 + ptra  
      ctr0 = ctr0  + tra
      if (cptr0.le.nihil) then
        crt0 = 0.0d0
      else 
        crt0 = max(0.0d0,min(1.0d0,ctr0/cptr0))
      endif

      if (dvs.ge.1.0d0) then
        cptr1 = cptr1+ptra
        ctr1 = ctr1 + tra
        if (cptr1.le.nihil) then
          crt1 = 0.0d0
        else 
          crt1 = max(0.0d0,min(1.0d0,ctr1/cptr1))
        endif
      else
        crt1 = 1.0d0
      endif

! --- crop finish conditions based on dvs or lai
      if ( ((dvs.ge.dvsend) .or. (lai.le.0.002d0.and.dvs.gt.0.5d0)) 
     &     .and. (.not. flCropEnd) ) then
        flCropOutput =.true.
        flCropEnd = .true.
        icrop = icrop + 1
        flBareSoil = .true.
      endif

      return
      end

! ----------------------------------------------------------------------
      subroutine totass (daynr,dayl,amax,eff,lai,kdif,avrad,
     $sinld,cosld,dtga)
! ----------------------------------------------------------------------
! --- author: daniel van kraalingen, 1986
! --- calculates daily total gross assimilation (dtga) by performing
! --- a gaussian integration over time. at three different times of 
! --- the day, irradiance is computed and used to calculate the instan- 
! --- taneous canopy assimilation, whereafter integration takes place.
! --- more information on this routine is given by spitters et al./1988
! --- subroutines and functions called: assim, radiat
! ----------------------------------------------------------------------
      implicit none

      integer i,daynr

      real*8  lai,kdif
      real*8  amax,avrad,cosld,dayl,dtga,eff,fgros,gausr,hour,pardif
      real*8  pardir,sinb,sinld

      data    gausr /0.3872983d0/
! ----------------------------------------------------------------------
! --- three point gaussian integration over day
      dtga = 0.
      if (amax.lt.1.0d-10) return
      do 10 i=1,3
        hour = 12.0d0+dayl*0.5*(0.5d0+(i-2)*gausr)
! --- at a specified hour, diffuse and direct irradiance is computed
        call radiat (daynr,hour,dayl,sinld,cosld,avrad,sinb,pardir,
     $              pardif)
! --- irradiance and crop properties determine assimilation
        call assim (amax,eff,lai,kdif,sinb,pardir,pardif,fgros)
        if(i.eq.2) fgros=fgros*1.6
        dtga = dtga+fgros
10    continue
      dtga =dtga*dayl/3.6

      return
      end

! ----------------------------------------------------------------------
      subroutine radiat (daynr,hour,dayl,sinld,cosld,avrad,sinb,
     $                   pardir,pardif)
! ----------------------------------------------------------------------
! --- author: daniel van kraalingen, 1986
! --- calculates the fluxes of diffuse and direct photosynthetically
! --- active radiation from the total daily shortwave radiation actually
! --- received (avrad) for a given day of the year and hour of the day.
! --- the input variables dayl, sinld and cosld are calculated in astro.
! --- for more information: see spitters et al. (1988).
! ----------------------------------------------------------------------
      implicit none

      integer daynr

      real*8  aob,atmtr,avrad,cosld,dayl,dsinb,dsinbe,dso,frdif,hour
      real*8  par,pardif,pardir,pi,sc,sinb,sinld

      data    pi /3.1415926d0/
! ----------------------------------------------------------------------
! --- calculations on solar elevation
! --- sine of solar elevation sinb
      aob = sinld/cosld
      sinb = max (0.0d0,sinld+cosld*cos(2.0*pi*(hour+12.0d0)/24.0))
! --- integral of sinb
      dsinb = 3600.*(dayl*sinld+24.*cosld*sqrt(1.0d0-aob*aob)/pi)
! --- integral of sinb, corrected for lower atmospheric transmission
! --- at low solar elevations
      dsinbe = 3600.*(dayl*(sinld+0.4*(sinld*sinld+cosld*cosld*0.5))+
     $         12.0*cosld*(2.0d0+3.0*0.4*sinld)*sqrt(1.0d0-aob*aob)/pi)

! --- solar constant and daily extraterrestrial radiation
      sc = 1370.*(1.0d0+0.033*cos(2.0*pi*daynr/365.))
      dso = sc*dsinb

! --- diffuse light fraction from atmospheric transmission
      atmtr = avrad/dso
      if (atmtr.gt.0.75d0) frdif = 0.23d0
      if (atmtr.le.0.75d0.and.atmtr.gt.0.35d0) frdif = 1.33d0-1.46*atmtr
      if (atmtr.le.0.35d0.and.atmtr.gt.0.07d0) 
     $ frdif = 1.0d0-2.3*(atmtr-0.07d0)**2
      if (atmtr.le.0.07d0) frdif = 1.0d0

! --- photosynthetic active radiation, diffuse and direct
      par = 0.5*avrad*sinb*(1.0d0+0.4*sinb)/dsinbe
      pardif = min (par,sinb*frdif*atmtr*0.5*sc)
      pardir = par-pardif

      return
      end

! ----------------------------------------------------------------------
      subroutine assim (amax,eff,lai,kdif,sinb,pardir,pardif,fgros)
! ----------------------------------------------------------------------
!     author: daniel van kraalingen, 1986
!     calculates the gross co2 assimilation rate of the whole crop, 
!     fgros, by performing a gaussian integration over depth in the 
!     crop canopy. at three different depths in the canopy, i.e. for
!     different values of lai, the assimilation rate is computed for
!     given fluxes of photosynthetically active radiation, whereafter
!     integration over depth takes place. for more information: see 
!     spitters et al. (1988). the input variables sinb, pardir and 
!     pardif are calculated in radiat.
! ----------------------------------------------------------------------
      implicit none

      integer i

      real*8  lai,laic,kdif,kdirbl,kdirt
      real*8  amax,eff,fgl,fgros,fgrsh,fgrsun,fslla,gausr,pardif,pardir
      real*8  refh,refs,scv,sinb,visd,visdf,vispp,visshd,vist

      data    gausr /0.3872983d0/
! ----------------------------------------------------------------------
! --- extinction coefficients kdif,kdirbl,kdirt
      scv = 0.2d0
      refh = (1.0d0-sqrt(1.0d0-scv))/(1.0d0+sqrt(1.0d0-scv))
      refs = refh*2.0/(1.0d0+1.6*sinb)
      kdirbl = (0.5/sinb)*kdif/(0.8*sqrt(1.0d0-scv))
      kdirt = kdirbl*sqrt(1.0d0-scv)

! --- three point gaussian integration over lai
      fgros = 0.
      do 10 i = 1,3
        laic = 0.5*lai+gausr*(i-2)*lai
! --- absorbed diffuse radiation (vidf),light from direct
! --- origine (vist) and direct light(visd)
        visdf = (1.0d0-refs)*pardif*kdif  *exp(-kdif  *laic)
        vist = (1.0d0-refs)*pardir*kdirt *exp(-kdirt *laic)
        visd = (1.0d0-scv) *pardir*kdirbl*exp(-kdirbl*laic)
! --- absorbed flux in w/m2 for shaded leaves and assimilation
        visshd = visdf+vist-visd
        fgrsh = amax*(1.0d0-exp(-visshd*eff/amax))
! --- direct light absorbed by leaves perpendicular on direct
! --- beam and assimilation of sunlit leaf area
        vispp = (1.0d0-scv)*pardir/sinb
        if (vispp.le.0.0d0) fgrsun = fgrsh
        if (vispp.gt.0.0d0) fgrsun = amax*(1.0d0-
     $    (amax-fgrsh)*(1.0d0-exp(-vispp*eff/amax))/ (eff*vispp))
! --- fraction of sunlit leaf area (fslla) and local
! --- assimilation rate (fgl)
        fslla = exp(-kdirbl*laic)
        fgl = fslla*fgrsun+(1.0d0-fslla)*fgrsh
! --- integration
        if (i.eq.2) fgl = fgl*1.6
        fgros = fgros+fgl
10    continue
      fgros = fgros*lai/3.6

      return
      end


! ----------------------------------------------------------------------
      subroutine grass(task)
! ----------------------------------------------------------------------
!     Date               : November 2004
!     Purpose            : detailed grass growth routine 
! ----------------------------------------------------------------------
      use variables
      implicit none
 
      integer   i1,ilvold,task,ilvoldpot,idregr,idregrpot
      integer   idelaypot,idelay,node

      real*8    laicr,laiexp,laimax,lasum,mres,rid
!     real*8    rdm,rr,rrpot
      real*8    lv(366),lvage(366),sla(366)
      real*8    admi,afgen,amax,asrc,ccheck,cosld,cvf,rlwtb(22)
      real*8    dalv,dayl,delt,dmi,drlv,cptr0,ctr0
      real*8    drrt,drst,dslv,dslv1,dslv2,dslvt,dteff,dtga
      real*8    dwlv,dwrt,dwst,fcheck,fl,fr,fs,laiexppot
      real*8    fysdel,gass,gla,glaiex,glasol,grlv,grrt,grst
      real*8    gwrt,gwst,pgass,rest,rmres
      real*8    sinld,slat,teff,twlv,twst,wlv,wrt,wst,glaiexpot
      real*8    lvpot(366),lvagepot(366),slapot(366)
      real*8    wlvpot,lasumpot,wrtpot,wstpot,drst1,drst2
      real*8    dwrtpot,dwlvpot,dwstpot,dtgapot,drst1pot,drst2pot
      real*8    pgasspot,gasspot,rmrespot,mrespot,asrcpot,dmipot
      real*8    admipot,grrtpot,drrtpot,gwrtpot,grlvpot,dslv1pot
      real*8    dslv2pot,dslvpot,restpot,dalvpot,drlvpot
      real*8    glasolpot,slatpot,glapot,grstpot,drstpot,gwstpot
      real*8    dslvtpot,twlvpot,twstpot,tagpspot,tagps
      real*8    dsinb,dsinbe,dso
      real*8    nihil,phead
      real*8    dmharvest1,dmharvest2,dmlastharvest,dateharvest(999)
      real*8    wrtmax,rdm
      real*8    grazingfactor
      real*8    nsuptab(magrs),dmfac(magrs),relnitab(2*magrs),nsupply        ! Nwgrassland
      integer   daylastharvest,iharvest,iharvestpot,swharvest,swgrazing
      logical   flharvestpot,flharvest,flgrazing,flgrazingpot
      character tmp*11,messag*200
d     real*8    co2rootfix,co2rootloss,co2shootfix
d     character komma*1

      parameter (nihil=1.0d-7)
      parameter (delt=1.0d0)

      save


! ----------------------------------------------------------------------
      goto (1000,2000,3000) task

1000  continue

! === initialization ====================================================

! --- read grass input data
      call readgrass (cropfil(icrop),pathcrop,tdwi,laiem,rgrlai,slatb,
     &  ssa,span,tbase,kdif,kdir,eff,amaxtb,tmpftb,tmnftb,cvl,cvr,cvs,
     &  q10,rml,rmr,rms,rfsetb,frtb,fltb,fstb,perdl,rdrrtb,
     &  rdrstb,hlim1,hlim2u,hlim2l,hlim3h,hlim3l,hlim4,rsc,adcrh,adcrl,
     &  cofab,rdi,rri,rdc,rdctb,rlwtb,logf,schedule,cumdens,
     &  flsolute,ecmax,ecslop,c2eca,c2ecb,c2ecf,numlay,dateharvest,
     &  swharvest,dmharvest1,dmharvest2,swgrazing,grazingfactor,
     &  daylastharvest,dmlastharvest,wrtmax,
     &  nsuptab,dmfac,relnitab,nsupply,
     &  swcf,swetr,cftb,chtb,cfet,alphacrit,
     &  swroottyp,wiltpoint,rootradius,rootcoefa,rsw)      

         
! --- initial values
      swinter = 1
      iharvest = 1
      iharvestpot = 1
      flharvest = .false.
      flharvestpot = .false.
      
      flgrazing = .false.
      flgrazingpot = .false.

d     open(unit=87,file='grass_CO2.csv',status='unknown')
d     write(87,*) 'date,co2rootfix,co2rootloss,co2shootfix'
d     open(unit=88,file='grass_dm.csv',status='unknown')
d     write(88,*) 'date,wrtpot,wrt,wstpot,wst,rdpot,rd,laipot,lai,
d    &gwrt,gwst,drrt,drlv,drst'

! --- development stage (not used by Grassland, instead Daynrs are used)
      dvs = -99.99d0

! --- maximum rooting depth & actual rooting depth
      rdm = min(rds,rdc)
      rd = min(rdi,rdm)
      rdpot = min(rdi,rdm)

! --- initial values of crop parameters
      rid = 1.0d0
      fr = afgen (frtb,30,rid)
      fl = afgen (fltb,30,rid)
      fs = afgen (fstb,30,rid)
      sla(1) = afgen (slatb,30,rid)
      lvage(1) = 0.d0
      ilvold = 1
      idregr = 0
      slapot(1) = afgen (slatb,30,rid)
      lvagepot(1) = 0.d0
      ilvoldpot = 1
      idregrpot = 0

! --- initial state variables of the crop
      wrt = fr*tdwi
      wrtpot = wrt
      wst = fs*(1.0d0-fr)*tdwi
      wstpot = wst
      wlv = laiem/sla(1)
      wlvpot = wlv
      lv(1) = wlv
      lvpot(1) = lv(1)
      lasum = laiem
      lasumpot = lasum     
      glaiex = 0.0d0
      laiexp = laiem
      laiexppot = laiem
      laimax = laiem
      lai = lasum+ssa*wst
      laipot = lai
      dwrt = 0.d0
      dwrtpot = dwrt
      dwlv = 0.d0
      dwlvpot = dwlv
      dwst = 0.d0
      dwstpot = dwst
      rid = dble(daycrop)
      cf = afgen (cftb,(2*magrs),rid)
      ch = afgen (chtb,(2*magrs),rid)

! --- initial summation variables of the crop
      tagp = wlv+wst
      tagppot = tagp
      cptr0 = 0.0d0
      ctr0 = 0.0d0
      tagpt = 0.0d0
      tagptpot = tagpt

! --- initialize matric flux potential                                  ! NwRootExtr
      phead = 0.d0   ! dummy                                            !
      if (swroottyp .eq. 2) then                                        !
        node = 1                                                        ! dummy node nr
        call MatricFlux(1,phead,node)                                   !
! ---   output of matric flux potential                                 !
!        if (swmfp.eq.1) call outmatricflux(2,mfp,numnod,tcum,          !
!     &   mflux,z,outfil,pathwork,project,ptra,h)                       !
      endif                                                             ! NwRootExtr

      return

2000  continue

! === calculate potential rate and state variables ======================================

! --- rates of change of the grass variables ---------------------------------------------

      rid = dble(daycrop)
      cf = afgen (cftb,(2*magrs),rid)
      ch = afgen (chtb,(2*magrs),rid)

! --- skip in case of regrowth
!      if (daycrop.ne.0.and.daycrop.lt.idregrpot) goto 2100
      if (daycrop.eq.0 .or.daycrop.ge.idregrpot) then

! ===   daily dry matter production ===

! ---   gross assimilation
        amax = afgen (amaxtb,30,rid)
! ---   correction for sub-optimum average daytemperature
        amax = amax * afgen (tmpftb,30,tavd)
        call astro(logf,swscre,daymeteo,lat,
     &           dayl,daylp,sinld,cosld,dsinb,dsinbe,dso)
        call totass (daynr,dayl,amax,eff,laipot,kdif,rad,sinld,cosld,
     &             dtgapot)
! ---   correction for low minimum temperature
        dtgapot = dtgapot * afgen (tmnftb,30,tmnr)
! ---   potential assimilation in kg ch2o per ha
        pgasspot = dtgapot * 30.0d0/44.0d0

! --- water stress reduction of pgass to gass (not for potential conditions)
        reltr = 1.0d0
        gasspot = pgasspot * reltr

! ---   respiration and partitioning of carbohydrates between growth and
! ---   maintenance respiration
        rmrespot=(rmr*wrtpot+rml*wlvpot+rms*wstpot)*afgen(rfsetb,30,rid)
        teff = q10**((tav-25.0d0)/10.0d0)
        mrespot = min (gasspot,rmrespot*teff)
        asrcpot = gasspot-mrespot

! ---   partitioning factors
        fr = afgen(frtb,30,rid)
        fl = afgen(fltb,30,rid)
        fs = afgen(fstb,30,rid)
! ---   check on partitioning
        fcheck = fr+(fl+fs)*(1.0d0-fr) - 1.0d0
        if (abs(fcheck).gt.0.0001d0) then
          write(tmp,'(f6.3)') rid
          tmp = adjustl (tmp)
          Messag ='The sum of partitioning factors for leaves, stems'//
     &    ' and storage organs is not equal to one at time '
     &    //trim(tmp)//'.'
          call fatalerr ('cropd',messag)
        endif

! ---   dry matter increase
        cvf = 1.0d0/((fl/cvl+fs/cvs)*(1.0d0-fr)+fr/cvr)
        dmipot = cvf*asrcpot
! ---   check on carbon balance
        ccheck = (gasspot-mrespot-(fr+(fl+fs)*(1.0d0-fr))*dmipot/cvf)
     &         /max(0.0001d0,gasspot)      
        if (abs(ccheck).gt.0.0001d0) then
          Messag ='The carbon balance is not correct'
          call fatalerr ('cropd',messag)
        endif


! ===   growth rate by plant organ ===

! ---   root length (not used, because Rooting depth is (for grassland) 
!       dependent on available root biomass (weight)
!        rrpot = min (rdm-rdpot,rri)
!        if (fr.le.0.or.pgasspot.lt.1.0d0) rrpot = 0.0d0

! ---   growth rate roots and aerial parts
! ---   after reaching a live weight of wrtmax (default 2500 kg), the
! ---   growth of the roots is balanced by the death of root tissue
        grrtpot = fr*dmipot
        if (wrtpot.gt.wrtmax) then
          drrtpot = grrtpot
        else
          drrtpot = wrtpot*afgen (rdrrtb,30,rid)
        endif
        gwrtpot = grrtpot-drrtpot

! ---   growth rate leaves

! ---   weight of new leaves
        admipot = (1.0d0-fr)*dmipot
        grlvpot = fl*admipot

! ---   death of leaves due to water stress or high lai
        dslv1pot = 0.0d0
        laicr = 3.2d0/kdif
        dslv2pot=wlvpot*max(0.0d0,
     &                  min(0.03d0,0.03d0*(laipot-laicr)/laicr))
        dslvpot = max (dslv1pot,dslv2pot) 

! ---   death of leaves due to exceeding life span;
! ---   leaf death is imposed on array until no more leaves have
! ---   to die or all leaves are gone

        restpot = dslvpot*delt
        i1 = ilvoldpot

        do while (restpot.gt.lvpot(i1).and.i1.ge.1)
          restpot = restpot-lvpot(i1) 
          i1 = i1-1
        enddo

! ---   check if some of the remaining leaves are older than span,
! ---   sum their weights

        dalvpot = 0.0d0
        if (lvagepot(i1).gt.span.and.restpot.gt.0.and.i1.ge.1) then
          dalvpot = lvpot(i1)-restpot
          restpot = 0.0d0
          i1 = i1-1
        endif

        do while (i1.ge.1.and.lvagepot(i1).gt.span)
          dalvpot = dalvpot+lvpot(i1)
          i1 = i1-1
        enddo

        dalvpot = dalvpot/delt

! ---   death rate leaves and growth rate living leaves
        drlvpot = dslvpot+dalvpot

! ---   physiologic ageing of leaves per time step
        fysdel = max (0.0d0,(tav-tbase)/(35.0d0-tbase))

! ---   leaf area not to exceed exponential growth curve
        slatpot = afgen (slatb,30,rid)
        if (laiexppot.lt.6.0d0) then
          dteff = max (0.0d0,tav-tbase)
          glaiexpot = laiexppot*rgrlai*dteff
! ---   source-limited increase in leaf area
          glasolpot = grlvpot*slatpot
          glapot = min (glaiexpot,glasolpot)
! ---   adjustment of specific leaf area of youngest leaf class
          if (grlvpot.gt.0.0d0) slatpot = glapot/grlvpot
        endif  

! ---   growth rate stems
        grstpot = fs*admipot
! ---   death of stems due to water stress is zero in case of potential growth
        drst1pot = 0.0d0
! ---   death of stems due to ageing
        drst2pot = afgen (rdrstb,30,rid)*wstpot
        drstpot = (drst1pot+drst2pot)/delt 
        gwstpot = grstpot-drstpot

! ----  integrals of the crop --------------------------------------------
!       after cutting, growth is initialized again and the weight of the sward is stored
! ---   harvest criteria (open to personal choice of user) 
!  INPUT    dmharvest, daylastharvest, dmlastharvest
!      if (tagppot.gt.4200.0d0 .or. 
!     &             (daycrop.gt.210 .and. tagppot.gt.3700.0d0)) then
        if (swharvest.eq.1) then
          if(tagppot.gt.dmharvest1 .or. (daycrop.gt.daylastharvest .and. 
     &                                 tagppot.gt.dmlastharvest).or. 
     &                                 flgrazingpot .eqv. .true.) then
            if (swgrazing.eq.1) then        ! grazing
                flgrazingpot = .true.
                      
                grlvpot = 0.0d0                           ! growing rate leaves adaption
                gwstpot = -1.0d0*grazingfactor* wstpot    ! growing rate stems adaption 
                 i1 = ilvoldpot
                 do while (i1.ge.1)
                    lvpot(i1) = (1.0d0-grazingfactor)*lvpot(i1)
                    i1 = i1 -1
                 end do
                 if(tagppot.lt.800.0d00) then            ! assumed last grazing day
                    flgrazingpot = .false.
                    flharvestpot = .true.
                 endif 
            else if (swgrazing.eq.2) then         ! growth continues followed by mowing
                if (tagppot.gt.dmharvest2.or. 
     &                             (daycrop.gt.daylastharvest
     &                              .and.tagppot.gt.dmlastharvest)) then
                    flharvestpot = .true.
                else
                    flharvestpot = .false.
                endif
            end if
          else
            flharvestpot = .false.
          endif
        endif
        if (swharvest.eq.2) then                         ! mowing using mowing dates
          if(t1900.gt.dateharvest(iharvestpot)) then
            iharvestpot = iharvestpot + 1
            flharvestpot = .true.
          else
            flharvestpot = .false.
          endif
        endif
        if (flharvestpot) then
          flharvestpot = .false.
          lasumpot = laiem
          slapot(1) = afgen (slatb,30,rid)
          wlvpot = lasumpot/slapot(1)
          fl = afgen (fltb,30,rid)
          fs = afgen (fstb,30,rid)
          wstpot = fs/fl*wlvpot
          dwlvpot = 0.0d0
          dwstpot = 0.0d0
          lvagepot(1) = 0.0d0
          ilvoldpot = 1
          laiexppot = laiem
          lvpot(1) = wlvpot

          gwstpot = 0.0d0
          gwrtpot = 0.0d0
          drlvpot = 0.0d0
          drstpot = 0.0d0
          drrtpot = 0.0d0

          tagpspot =max(0.0d0,(tagppot-(wlvpot+dwlvpot+wstpot+dwstpot)))
          tagptpot = tagptpot + tagpspot

! ---     regrowth delay after handbook p.r.

          if (tagpspot.lt.2000.0d0) idelaypot=1
          if (tagpspot.ge.2000.0d0.and.tagpspot.lt.2500.0d0) idelaypot=2
          if (tagpspot.ge.2500.0d0.and.tagpspot.lt.3000.0d0) idelaypot=3
          if (tagpspot.ge.3000.0d0.and.tagpspot.lt.3500.0d0) idelaypot=4
          if (tagpspot.ge.3500.0d0.and.tagpspot.lt.4000.0d0) idelaypot=5
          if (tagpspot.ge.4000.0d0) idelaypot = 6

          idregrpot = daycrop + idelaypot + 3

        endif

        if (daycrop.ge.idregrpot) then

! ---     leaf death is imposed on array untill no more leaves have to die or all leaves are gone

          dslvtpot = dslvpot*delt
          i1 = ilvoldpot
          do while (dslvtpot.gt.0.and.i1.ge.1)
            if (dslvtpot.ge.lvpot(i1)) then
              dslvtpot = dslvtpot-lvpot(i1)
              lvpot(i1) = 0.0d0
              i1 = i1-1
            else
              lvpot(i1) = lvpot(i1)-dslvtpot
              dslvtpot = 0.0d0
            endif
          enddo

          do while (lvagepot(i1).ge.span.and.i1.ge.1)
            lvpot(i1) = 0.0d0
            i1 = i1-1
          enddo

          ilvoldpot = i1

! ---     shifting of contents, integration of physiological age
          do i1 = ilvoldpot,1,-1
            lvpot(i1+1) = lvpot(i1)
            slapot(i1+1) = slapot(i1)
            lvagepot(i1+1) = lvagepot(i1)+fysdel*delt
          enddo
          ilvoldpot = ilvoldpot+1

! ---     new leaves in class 1
          lvpot(1) = grlvpot*delt
          slapot(1) = slatpot
          lvagepot(1) = 0.d0 

! ---     calculation of new leaf area and weight
          lasumpot = 0.d0
          wlvpot = 0.d0
          do i1 = 1,ilvoldpot
            lasumpot = lasumpot+lvpot(i1)*slapot(i1)
            wlvpot = wlvpot+lvpot(i1)
          enddo

          laiexppot = laiexppot+glaiexpot*delt

        endif
      endif

! --- dry weight of living plant organs
      wrtpot = wrtpot+gwrtpot*delt
      wstpot = wstpot+gwstpot*delt

! --- dry weight of dead plant organs (roots,leaves & stems)
      dwrtpot = dwrtpot+drrtpot*delt
      dwlvpot = dwlvpot+drlvpot*delt
      dwstpot = dwstpot+drstpot*delt

! --- dry weight of dead and living plant organs
      twlvpot = wlvpot+dwlvpot
      twstpot = wstpot+dwstpot
      tagppot = twlvpot+twstpot

! --- leaf area index
      laipot = lasumpot+ssa*wstpot

! --- rooting depth as function of available root weight (not root rate!)
!      rdpot = rdpot+rrpot
      rdpot = afgen (rlwtb,22,wrtpot)
      rdpot = min(rdpot,rdm)

      return

3000  continue

! === calculate actual rate and state variables ======================================

! --- rates of change of the crop variables ---------------------------------------------

!     correction of potential transpiration in relation to reference crop
!     (default = 1.0, range = 0.8 - 1.2)
      ptra = cfet*ptra

! --- skip in case of regrowth
      if (daycrop.eq.0.or.daycrop.ge.idregr) then

! ===   daily dry matter production ===

! ---   gross assimilation
        amax = afgen (amaxtb,30,rid)
! ---   correction for sub-optimum average daytemperature
        amax = amax * afgen (tmpftb,30,tavd)
! ---   gross assimilation
        call astro(logf,swscre,daymeteo,lat, 
     &           dayl,daylp,sinld,cosld,dsinb,dsinbe,dso)
        call totass (daynr,dayl,amax,eff,lai,kdif,rad,sinld,cosld,dtga)
! ---   correction for low minimum temperature
        dtga = dtga * afgen (tmnftb,30,tmnr)
! ---   potential assimilation in kg ch2o per ha
        pgass = dtga * 30.0d0/44.0d0

! ---   water stress reduction of pgass to gass
        if(abs(ptra).lt.nihil) then
          reltr = 1.0d0
        else
          reltr = max(0.0d0,min(1.0d0,tra/ptra))
        endif
        gass = pgass * reltr
! ---   nitrogen stress reduction of pgass to gass
        relni = afgen(relnitab,magrs,nsupply)
        gass = gass * relni

! ---   respiration and partitioning of carbohydrates between growth and
! ---   maintenance respiration
        rmres = (rmr*wrt+rml*wlv+rms*wst)*afgen(rfsetb,30,rid)
        teff = q10**((tav-25.0d0)/10.0d0)
        mres = min (gass,rmres*teff)
        asrc = gass-mres

! ---   dry matter increase
        cvf = 1.0d0/((fl/cvl+fs/cvs)*(1.0d0-fr)+fr/cvr)
        dmi = cvf*asrc
! ---   check on carbon balance
        ccheck = (gass-mres-(fr+(fl+fs)*(1.0d0-fr))*dmi/cvf)
     &         /max(0.0001d0,gass)      
        if (abs(ccheck).gt.0.0001d0) then
          Messag ='The carbon balance is not correct'
          call fatalerr ('cropd',messag)
        endif

! ===   growth rate by plant organ ===

! ---   root length
!        rr = min (rdm-rd,rri)
!        if (fr.le.0.or.pgass.lt.1.0d0) rr = 0.0d0
!        rr = 0.0d0

! ---   growth rate roots and aerial parts
! ---   after reaching a live weight of wrtmax (default 2500 kg), the
! ---   growth of the roots is balanced by the death of root tissue
        grrt = fr*dmi
        if (wrt.gt.wrtmax) then
!original drrt = wrt - wrtmax
          drrt = grrt
!         CO2 loss
d         co2rootloss = drrt*44.0d0/33.0d0
!original  grrt = 0.0d0
        else
          drrt = wrt*afgen (rdrrtb,30,rid)
        endif
        admi = (1.0d0-fr)*dmi
!original drrt = 0.0d0
        gwrt = grrt-drrt

!       CO2 fixation and loss
d       co2rootfix = grrt*44.0d0/33.0d0
d       co2rootloss = co2rootloss + drrt*44.0d0/33.0d0

! ---   growth rate leaves

! ---   weight of new leaves
        grlv = fl*admi

! ---   death of leaves due to water stress or high lai
        if(abs(ptra).lt.nihil) then
          dslv1 = 0.0d0
        else
          dslv1 = wlv*(1.0d0-tra/ptra)*perdl
        endif
        laicr = 3.2d0/kdif
        dslv2 = wlv*max(0.0d0,min(0.03d0,0.03d0*(lai-laicr)/laicr))
        dslv = max (dslv1,dslv2) 

! ---   death of leaves due to exceeding life span;
! ---   leaf death is imposed on array until no more leaves have
! ---   to die or all leaves are gone

        rest = dslv*delt
        i1 = ilvold

        do while (rest.gt.lv(i1).and.i1.ge.1)
          rest = rest-lv(i1) 
          i1 = i1-1
        enddo

! ---   check if some of the remaining leaves are older than span,
! ---   sum their weights

        dalv = 0.0d0
        if (lvage(i1).gt.span.and.rest.gt.0.and.i1.ge.1) then
          dalv = lv(i1)-rest
          rest = 0.0d0
          i1 = i1-1
        endif

        do while (i1.ge.1.and.lvage(i1).gt.span)
          dalv = dalv+lv(i1)
          i1 = i1-1
        enddo

        dalv = dalv/delt

! ---   death rate leaves and growth rate living leaves
        drlv = dslv+dalv

! ---   physiologic ageing of leaves per time step
        slat = afgen (slatb,30,rid)

! ---   leaf area not to exceed exponential growth curve
        if (laiexp.lt.6.0d0) then
          dteff = max (0.0d0,tav-tbase)
          glaiex = laiexp*rgrlai*dteff
! ---     source-limited increase in leaf area
          glasol = grlv*slat
          gla = min (glaiex,glasol)
! ---     adjustment of specific leaf area of youngest leaf class
          if (grlv.gt.0.0d0) slat = gla/grlv
        endif  

! ---   growth rate stems
        grst = fs*admi
! ---   death of stems due to water stress
        if(abs(ptra).lt.nihil) then
          drst1 = 0.0d0
        else
          drst1 = wst*(1.0d0-tra/ptra)*perdl
        endif
! ---   death of stems due to ageing
        drst2 = afgen (rdrstb,30,rid)*wst
        drst = (drst1+drst2)/delt 
        gwst = grst-drst

! ----  integrals of the crop --------------------------------------------

!       after cutting, growth is initialized again and the weight of the sward is stored

        if (swharvest.eq.1) then
          if(tagp.gt.dmharvest1 .or. (daycrop.gt.daylastharvest .and. 
     &                              tagp.gt.dmlastharvest).or. 
     &                              flgrazing .eqv. .true.) then
            if (swgrazing.eq.1) then               ! grazing
                flgrazing = .true.
                grlv = 0.0d0                       ! growing rate leaves adaption
                gwst = -1.0d0*grazingfactor* wst   ! growing rate stems adaption
                 i1 = ilvold
                 do while (i1.ge.1)
                    lv(i1) = (1.0d0-grazingfactor)*lv(i1) ! reduction of leaf weight
                    i1 = i1 -1
                 end do
                 if(tagp.lt.800.0d00) then  ! assumed last grazing day
                    flgrazing = .false.
                    flharvest = .true.
                 endif 
            else if (swgrazing.eq.2) then   ! growth continues followed by harvest
                if (tagp.gt.dmharvest2.or. 
     &                         (daycrop.gt.daylastharvest .and. 
     &                                   tagp.gt.dmlastharvest)) then
                    flharvest = .true.
                else
                    flharvest = .false.
                endif
            end if
          else
            flharvest = .false.
          endif
        endif
        if (swharvest.eq.2) then            ! mowing using mowing dates
          if(t1900.gt.dateharvest(iharvest)) then
            iharvest = iharvest + 1
            flharvest = .true.
          else
            flharvest = .false.
          endif
        endif
        if (flharvest) then
          flharvest = .false.
          lasum = laiem
          sla(1) = afgen (slatb,30,rid)
          wlv = lasum/sla(1)
          wst = fs/fl*wlv
          dwlv = 0.0d0
          dwst = 0.0d0
          lvage(1) = 0.0d0
          ilvold = 1
          laiexp = laiem
          lv(1) = wlv

          gwst = 0.0d0
          gwrt = 0.0d0
          drlv = 0.0d0
          drst = 0.0d0
          drrt = 0.0d0

          tagps = max (0.0d0,(tagp-(wlv+dwlv+wst+dwst)))
          tagpt = tagpt + tagps

! ---     regrowth delay after handbook p.r.

          if (tagps.lt.2000.0d0) idelay = 1           
          if (tagps.ge.2000.0d0.and.tagps.lt.2500.0d0) idelay = 2
          if (tagps.ge.2500.0d0.and.tagps.lt.3000.0d0) idelay = 3
          if (tagps.ge.3000.0d0.and.tagps.lt.3500.0d0) idelay = 4
          if (tagps.ge.3500.0d0.and.tagps.lt.4000.0d0) idelay = 5
          if (tagps.ge.4000.0d0) idelay = 6

          idregr = daycrop + idelay + 3

        endif

        if (daycrop.ge.idregr) then

! ---     physiologic ageing of leaves per time step
          fysdel = max (0.0d0,(tav-tbase)/(35.0d0-tbase))

! ---     leaf death is imposed on array untill no more leaves have to die or all leaves are gone

          dslvt = dslv*delt
          i1 = ilvold
          do while (dslvt.gt.0.and.i1.ge.1)
            if (dslvt.ge.lv(i1)) then
              dslvt = dslvt-lv(i1)
              lv(i1) = 0.0d0
              i1 = i1-1
            else
              lv(i1) = lv(i1)-dslvt
              dslvt = 0.0d0
            endif
          enddo

          do while (lvage(i1).ge.span.and.i1.ge.1)
            lv(i1) = 0.0d0
            i1 = i1-1
          enddo

          ilvold = i1

! ---     shifting of contents, integration of physiological age
          do i1 = ilvold,1,-1
            lv(i1+1) = lv(i1)
            sla(i1+1) = sla(i1)
            lvage(i1+1) = lvage(i1)+fysdel*delt
          enddo
          ilvold = ilvold+1

! ---     new leaves in class 1
          lv(1) = grlv*delt
          sla(1) = slat
          lvage(1) = 0.d0 

! ---     calculation of new leaf area and weight
          lasum = 0.d0
          wlv = 0.d0
          do i1 = 1,ilvold
            lasum = lasum+lv(i1)*sla(i1)
            wlv = wlv+lv(i1)
          enddo

          laiexp = laiexp+glaiex*delt

        endif
      endif

! --- dry weight of living plant organs
      wrt = wrt+gwrt*delt
      wst = wst+gwst*delt

! --- dry weight of dead plant organs (roots,leaves & stems)
      dwrt = dwrt+drrt*delt
      dwlv = dwlv+drlv*delt
      dwst = dwst+drst*delt

! --- dry weight of dead and living plant organs
!     twrt = wrt+dwrt
      twlv = wlv+dwlv
      twst = wst+dwst
      tagp = twlv+twst

! --- leaf area index
      lai = lasum+ssa*wst
      laimax = max (lai,laimax)

! --- rooting depth as function of root weight
!      rd = rd+rr
      rd = afgen (rlwtb,22,wrt)
      rd= min(rd,rdm)

!     CO2 fixation and root,shoot developm
d     co2shootfix = tagp * 44.0d0/33.0d0
d     komma = ","
d     write(87,'(a12,1x,3(a,f12.4))') 
d    &  date,komma,co2rootfix,komma,co2rootloss,komma,co2shootfix
d     write(88,'(a12,1x,20(a,f12.4))') 
d    &  date,komma,wrtpot,komma,wrt,komma,wstpot,komma,wst,
d    &  komma,rdpot,komma,rd,komma,laipot,komma,lai,
d    &  komma,gwrt,komma,gwst,komma,drrt,komma,drlv,komma,drst

! --- cumulative relative transpiration
      cptr0 = cptr0 + ptra  
      ctr0 = ctr0  + tra
      if (cptr0.le.nihil) then
        crt0 = 0.0d0
      else
        crt0 = max(0.0d0,min(1.0d0,ctr0/cptr0))
      endif

      return
      end

! NwRootExtr New Subroutine for new Rootextraction ! NwRootExtr 

! ----------------------------------------------------------------------
      subroutine MatricFlux(task,phead,node) 
! ----------------------------------------------------------------------
!     Date               : October 2006   
!     Purpose            : Initialize and calculate matric flux potential
! ----------------------------------------------------------------------

      use variables
      implicit none

      integer task,lay,count,start,node
      real*8  phead1,phead2,wcontent,conduc1,conduc2,watcon,hconduc
      real*8  phead,logphead

      goto (1000, 2000) task

1000  continue

! === initialization =========================================================

      do lay = 1,numlay
        do count = 1,600
          mfluxtable(lay,count) = 0.0d0
        enddo
      enddo

      start = int(100.d0*log10(-wiltpoint))
      do lay = 1,numlay
        phead1 = -10.d0**(dble(start)/100.d0)

!       find first Node of the Layer
        Node = nod1lay(lay)

        wcontent = watcon(Node,phead1,cofgen(1,Node),
     &                    swsophy,numtab,sptab)
        conduc1 = hconduc (Node,wcontent,cofgen(1,Node),swfrost,10.d0,
     &                     swsophy,numtab,sptab)

        do count = start-1,1,-1
          phead2 = -10.d0**(dble(count)/100.d0)
          wcontent = watcon(Node,phead2,cofgen(1,Node),
     &                      swsophy,numtab,sptab)
          conduc2 = hconduc (Node,wcontent,cofgen(1,Node),swfrost,10.d0,
     &                       swsophy,numtab,sptab)
          mfluxtable(lay,count) = mfluxtable(lay,count+1) + 
     &                 0.5d0 * (conduc1 + conduc2) * (phead2 - phead1) 
          phead1 = phead2
          conduc1 = conduc2
        enddo
      enddo

      return

2000  continue

! === calculation of matric flux potential ===================================

      lay = layer(node)
      if (phead .lt. wiltpoint) then
! ---   very dry range 
         mflux(node) = 0.0d0
      elseif (phead .gt. -1.023293d0) then
! ---   very wet range (> -10^0.01)
         mflux(node) = mfluxtable(lay,1)
      else  
! ---   direct access table, with linear interpolation
        logphead = 100.d0*log10(-phead)
        count = int(logphead)
        mflux(node) = (logphead-dble(count))*mfluxtable(lay,count+1) +
     &                (dble(count+1)-logphead)*mfluxtable(lay,count)
      endif

      return
      end 

        
      
      subroutine Samuca(task)            
      !-------------------------------------------------------------------------
      !  SAMuCA - AGRONOMIC MODULAR SIMULATOR FOR SUGARCANE     
      !  Written in Microsoft Visual Studio FORTRAN for PC-compatible machines              
	!  Author: FABIO R. MARIN Date: 11/10/2010
      !  Literature Ref 1st Version: dx.doi.org/10.1590/S0103-90162014000100001
      !
	!  Edited in: Sep-2015 by Murilo dos S. Vianna  -> Model Review and Assessment
      !  Edited in: Feb-2016 by Murilo dos S. Vianna  -> Coupled to SWAP: https://scisoc.confex.com/crops/2017am/webprogram/Paper105395.html
      !  Edited in: Jan-2018 by Murilo dos S. Vianna  -> Structure Review and Improvements: LAIS, rootgrowth, TempHour, stk_li, DIAMPERS                                            
	!-------------------------------------------------------------------------

      use Variables 
	implicit none
      
      !--- Crop Variables and Parameters (P)
      
      integer     task                !Controler of call ordering from the main program
      integer     i                   !Counter
      integer     nf                  !Format Counter Real
      integer     ni                  !Format Counter Integer
      integer     nhd                 !Format Counter lines of file header
      integer     rdenshape           !User can choose to use a shapefactor for root density (=1) or not (=0)     
      integer     node                !Node Counter 
      integer     noden               !Total Node Number
      integer     ngrnodes            !Number of growing internodes (P)
      integer     ratcount            !Number of ratooning cuts
      integer     tl                  !Tiller Number
      integer     it                  !Internode Number
      integer     lf                  !Leaf number
      integer     rtcumdens           !Root cumulative density (1 = function, 2 = input, 3 = Two SRL derived from Laclau&Laclau 2009) 
      integer     sl                  !Soil layer or compartment
      integer     pgmethod            !Gross photosynthesis method (1=RUE, 2=Canopy Photos)
      integer     pfmethod            !Partitioning factor method (1 = Marin & Jones (2014), 2 = Modified Marin & Jones (2014)
      integer     outstk_rank         !1 - 60 : stalk rank based on it order of emergence
      integer     itoutnumber         !Number of internodes  to be printed on detailed output (always sugested to be more than the crop maximum)
      integer     sucaccfac           !Sucrose Accumulation Factor (P)
      integer     init_nlf            !Initial lf number of tiller (P)
      integer     maxdevgl            !Maximum developed (DewLap) green lf stk-1 (P)
      integer  :: defout_io   = 45    !I/O Default output
      integer  :: detrtout_io = 44    !I/O RootSystem Detailed output
      integer  :: detlfout_io = 43    !I/O LeafProfile Detailed output
      integer  :: detitout_io = 42    !I/O InternodeProfile Detailed output
      integer  :: detpgout_io = 41    !I/O InternodeProfile Detailed pg
      integer  :: parinp_io   = 302   !I/O Crop Parameters
      integer  :: calibr_io   = 911   !I/O Calibration Parameters
      
      logical     flemerged           !Crop emergence flag
      logical     flcropalive         !Crop is alive? (T or F)
      logical     flcarboncheck       !Carbon Check
      logical     flstalkemerged      !Stalk Emerged
      logical     flcalibrate         !Calibration
      logical     fldetrtsyout
      logical     fldetlfprout
      logical     fldetitprout
      logical     fldetpgfaout        !Detailed pg and factors output file?
      logical     flsink_fbres        !Simulate Feedback response of sink to photosynthesis rate? (T or F)
      
      real        agefactor           !Relative age factor to reduce crop processes 
      real        chudec              !Termal time for tillering deccay (P)
      real        chuem               !Cumulative heat units to emergence (Degree-days) (P)
      real        chumat              !Termal time for tillering maturation stage (P)
      real        chupeak             !Termal time for tillering peak (P)
      real        chustk              !Cumulative heat units for stalk emergence (Degree-days) (P)
      real        cumla(150)          !Cumulative Leaf Area m2 m-2 
      real        cumlw(150)          !Cumulative Leaf dry weight t ha-1
      real        ddealla             !Daily dead Leaf area (m2 m-2)
      real        ddeallw             !Daily dead Leaf biomass (t ha-1)       
      real        deadln              !DAily dead leaf number
      real        di                  !daily accumulated temperature above TB (degree days)
      real        diac                !Cumulative Degree-Days
      real        diaclf              !Cumulative Degree-Days for leaf dev
      real        diam                !average stem diameter (cm)
      real        dileaf              !
      real        dla                 !
      real        dleafdm             !Daily incremental leaf dry mass
      real        dnleaf              !Incremental leaf number
      real        dnstk               !
      real        dpercoeff           !Maximum plant extension(P)
      real        dsuc                !Daily Sucrose Increment
      real        dw                  !incremental total plant dry matter weight (kg m-2)
      real        dwa                 !incremental canopy dry matter weight (kg m-2)
      real        dwl                 !incremental leaf dry matter weight (kg m-2)
      real        dwr                 !incremental root dry matter weight (kg m-2)
      real        dws                 !incremental stalk dry matter weight (kg m-2)
      real        dwsuc               !incremental sucrose mass (kg m-2)
      real        dwwater             !incremental stalk fresh matter weight (kg m-2)
      real        e                   !conversion efficiency of CH2O to plant tissue (g g-1) (1-gresp)
      real        epp                 !
      real        esred               !
      real        esw                 !
      real        etp                 !
      real        extcoef             !Extintion Coeficcient
      real        la                  !leaf area
      real        la_stk              !leaf area stalk
      real        lgpf                !Light gross photosynthesis
      real        li                  !Light Interception - non-dimensional factor to be multoplied be PAR
      real        ln                  !Number of Green Leaves (# Leaves/stalk)
      real        ln_stk              !
      real        lntotal             !number of leaves (green + senesced)
      real        maxgl               !maximum number of leaves
      real        mla                 !Maximum leaf Area (cm2) as a function of leaf number
      real        nstk                !
      real        par                 !
      real        perday              !plant elongation rate per day
      real        pg                  !canopy gross photosynthesis rate (t ha-2 day-1)
      real        phyloc              !(P)
      real        plantdepth          !(P)
      real        pleng               !
      real        pol                 !ratio between WSUC and WSFRESH (%)
      real        popmat              !(P)
      real        poppeak             !(P)
      real        rdepth              !
      real        resp                !
      real        rgpf                !
      real        rowsp               !row spacing
      real        rue                 !Potential RUE (at optimum temp, swc, age, CO2=Actual Conditions) [gDW MJ-1] Note: DW is total (aerial+below ground biomass) (P)
      real        arue_dw             !Daily actual RUE [gDW MJ-1] (dw/dIPAR) 
      real        arue_w              !Total actual RUE
      real        arue_dwa
      real        arue_wa
      real        sgpf                !
      real        shootdepth          !Shoot depth before emergence
      real        sla                 !specific leaf area (m2 kg-1) (P)
      real        srad                !Daily solar radiation (MJ m-2)
      real        srl                 !(P)
      real        stalkgpf            !
      real        stkdmc              !Dry matter fraction in the stalk fresh mass
      real        suc_stk             !
      real        sucmax              !
      real        swface              !soil water excess stress factor
      real        swfacp              !soil water deficit stress factor
      real        rwuep1              !
      real        rwuep2              !
      real        thour(24)           !Hourly temperature
      real        tmax                !Daily maximum temperature (oC)
      real        tmin                !Daily manimum temperature (oC)
      real        t_mean              !Daily mean temperature (oC)
      real        trwu                !
      real        tstress             !Temperature Stress Fac
      real        w                   !total plant dry matter weight (ton ha-1)
      real        w_pg                !total plant dry matter weight gained from photosynthesis (ton ha-1)
      real        wa                  !aerial dry matter weight (ton ha-1)
      real        wa_pg               !aerial dry matter weight gained from photosynthesis (ton ha-1)
      real        wl                  !
      real        wr                  !root dry matter weight (ton ha-1)
      real        wsdm                !stalk dry matter weight (ton ha-1)
      real        wsfresh             !stalk fresh matter weight (ton ha-1)
      real        wstkwat             !
      real        wsuc                !
      real        ws                  !
      real        qrotts              !     
      real        rue_modifier        !Co2 Effect on RUE
      real        rdm                 !
      real        ptrats              !
      real        epp_cm              !
      real*8      afgen               !
      real*8      watcon              !
      real*8      depth               !
      real*8      rootdis(202)        !
      real*8      sum                 !
      real        trwuafter           !
      real        rootshape           !Factor related to the root density shape in soil profile
      real*8      top                 !
      real*8      bot                 !
      real        prwu(macp)          !
      real        tqropot             !
      real        rld(macp)           !
      real        rgf(macp,2)         !
      real        wpp(macp)           !
      real        fcp(macp)           !
      real        stp(macp)           !
      real        rootsene            !
      real        pdep_Rat            !Initial Plant depth on ratooning (cm)
      real        swcon1              !
      real        swcon2(macp)        !
      real        swcon3              !
      real        laythi(macp)        !
      real        prwulay(macp)       !
      real        wuf                 !
      real        wuf_swap            ! Water uptake factor based on SWAP rwu
      real        eoratio             !
      real        kc_min              !
      real        maxlai              !
      real*8      co2                 !CO2 concentration at atmosphere [ppm]
      real        co2_fac             !Co2 relative effect on PG
      real        srlmax              !Maximum Specific root length, used at downward root front (Crop parameter) (m g-1)
      real        srlmin              !Minimum Specific root length, used above downward root front (Crop parameter) (m g-1)
      real        f_rts               !Maximum Fraction of root senescence rate
      real        rootprof(500)       !Root profile with 1 cm resolution (up to 5 meters) 
      real        initcropdepth       !Initial crop depth (planting or ratooning depth) (cm)
      real        effective_rd        !Effective root depth (cm)
      real        rpup                !Upward root front (cm)
      real        drdepth             !Daily root depth rate (cm)
      real        botsoil             !Soil bottom depth (cm)
      real        top_sl              !Top soil layer depth (cm)
      real        bot_sl              !Bottom soil layer depth (cm)
      real        thk_sl              !Soil layer thickness (cm)
      real        drld(macp)          !Root lenght density rate (cm.root cm-3.soil)
      real        dwr_prof(macp)      !
      real        soma                !Cumulative function
      real        shootrate_bg        !
      real        rootdrate           !
      real        initrootdwrate      !
      real        minrgpf             !
      real        rgpf_be             !
      real        maxlgpf             !
      real        gresp               !
      real        mresp               !
      real        dnetbiomass         !
      real        init_stalk_fw       !
      real        init_stalk_ht       !
      real        nstalks_planting    !
      real        init_pop            !
      real        initbiomass         !
      real        availw_bg           !
      real        biomass_useRate     !
      real        rate_af             !
      real        dfib                !
      real        dper                !
      real        tb                  !base temperature for crop development and photosynthesis (P)
      real        tbi                 !base temperature for stalk expansion (P)
      real        topt                !optimum temperature for stalk expasion (P) - Based on N. G. Inman-Bamber et al. 2008 Australian Journal of Agricultural Research, Fig.3
      real        to_pg1              !bottom optimum temperature for photsynthesis (1st threshold) (P)
      real        to_pg2              !upper optimum temperature for photsynthesis (2nd threshold) (P)
      real        tbM                 !Maximum temperature for crop development and photosynthesis (P)
      real        sucexceeded         !
      real        fib                 !
      real        suc                 !
      real        dnoden              !
      real        ddeadln             !
      real        wdead               !
      real        dwdead              !
      real        initnstk            !
      real        carbcheck           !
      real        ckthreshold         !
      real        rgpf_ck             !
      real        lgpf_ck             !
      real        sgpf_ck             !
      real        tgpf_ck             !
      real        BG_CarbonRes        !Below Ground Carbon Reserves (Surplus biomass)
      real        diacem              !Cumulative heat unit after emergence
      real        midtime_rgpf        !
      real        pfshp               !
      real        AScurv              !FUNCTION
      real        agpf                !Above ground partitioning factor
      real        maxsgpf_ap          !
      real        midtime_sgpf        !
      real        tppf                !
      real        sgpf_ap             !
      real        lgpf_ap             !
      real        tppf_ap             !
      real        lfcb_ratio          !
      real        cbpf                !
      real        stk_agefac          !
      real        devgl               !
      real        dwt                 !
      real        wt                  !
      real        dw_ss               !
      real        emerg_tillers       !Number of tillers emerged per linear meter [# m-1] Warning: Crop Parameter
      real        init_la             !
      real        ddeadtop            !
      real        frac_rtRes          !Fraction of roots biomass will become reserves for next ratooning
      real        frac_rtRem          !Fraction of roots biomass will remaing for next ratooning
      real        age_stkemer         !Age in which a Tiller emerges it Stalks
      real        dstkdm              !DAily stalk biomass gain after allocation t ha-1      
      real        stk_age(60,4)       !Stalk age (1) and fraction of age in relation to the oldest stalk (2) Number of Green Leaves (3)
      real        lfage(60,20)        !
      real        lfarea(60,20)       !
      real        lfweight(60,20)     !
      real        ngl
      real        av_lfage(20)        !Averaged lfage(60,20) among stalks
      real        av_lfarea(20)       !Averaged lfarea(60,20) among stalks
      real        av_lfweight(20)     !Averaged lfweight(60,20) among stalks
      real        stk_dnetbiomass(60) !
      real        dwat_dws            !
      real        dwat_dsuc           !
      real        itnumber            !Internode Number stalk-1
      real        stk_itn(60)         !Internode Number
      real        itage(60,60)        !Internode Age
      real        itpf(60,60)         !Internode Partitioning Factor
      real        itlen(60,60)        !Internode Length (mm)
      real        av_itlen(60)        !Avg Internode Length (mm)
      real        itsuc(60,60)        !Internode Sucrose Mass
      real        av_itsuc(60)        !Avg Internode Sucrose Mass
      real        itfib(60,60)        !Internode Fiber Mass
      real        ittdw(60,60)        !Internode Total Biomass
      real        av_ittdw(60)        !Avg Internode Total Biomass
      real        itshp               !Internode growth shp (P)
      real        sucshp              !Sucrose fraction shp (P)
      real        lfshp               !Lf growth shp (P)
      real        frac_suc_pl         !Fraction of DW that is sucrose+CH2O on planting chopped stalks
      real        frac_dw_pl          !Fraction of Fresh mass that is DW on planting chopped stalks
      real        maxitdw             !Maximum Internode Biomass (Sink Strength) g it-1 (P)
      real        internode(100,6)    !internode array
      real        dRGP_pg             !Daily Reduced Growth due to sink feedback response to PG
      real        RGP_pg              !Total Reduced Growth due to sink feedback response to PG
      real        RGP_fac             !Daily Fraction of reduction (dw/pg) 
      real        IPAR_acc            !Accumulated PAR intercepted (MJ m-2)
      real    ::  CH2O_M  = 30.d0     !CH2O Molar weigth
      real    ::  CO2_M   = 44.d0     !CO2  Molar weigth      
      real*8      real_host(2)        !Calibration purpose dimension is the number of parameters      
      real*8  ::  hwpp    = -15000.d0 !Wilting Point pressure
      real*8  ::  hfcp    = -330.d0   !Field Capacity pressure
      real*8  ::  hstp    = -10.d0    !Saturation Point pressure
      
      character   messag*400          !
      character   canetype*6          !
      character   canestatus*6        !
      character   canestage*6         !
      character   outfile*4           !4-character name
      character   path_out*90         !path to the output file
      character   skip*30             !skip string for reading
              
      save      
             
      goto (1000,2000,3000) task

1000  continue
      
      !Warnings to include:
      
      !Dont let the below variables or its results equal to zero to avoid undesired NaN errors (x/0.d0)
      !(chupeak-chuem)
      !(chumat-chudec)
      !(1./rwuep1)
      !(1./rwuep2)
      !5. / (rowsp/100.d0)
      !la/ (mla / 1.e4 * 2.)
      !(6/(chustk/2.))
      !lai / (maxlai)
      !(rgpf_be-minrgpf) cannot be negative!
      ! midtime_rgpf cannot be .le. than 0.d0
      
          flsink_fbres    = .true.!Simulate Feedback response of sink to photosynthesis rate? (T or F)
      !--- Output Options (put this on a control input file)          
          fldetrtsyout    = .true.!Detailed Root System output?
          fldetlfprout    = .true.!Detailed Leaf Profile (architecture) output?
          fldetitprout    = .true.!Detailed Stalk Profile output?
          fldetpgfaout    = .true.
          outstk_rank     = 1     !Rank of detailed stalk (1 <= outstk_rank <= peakpop)
          itoutnumber     = 35    !Up to 35 internodes will be printed out on outstk_rank and averaged stalks
      
      
      !--- Reading Crop Parameters
          open(parinp_io,FILE='Param_Set.OUT',
     &      STATUS='OLD',READONLY)
          
          nhd  = 4    !Number of lines of file header   
          nf   = 72   !Number of Real parameters
          ni   = 11   !Number of Integer parameters
119       format(<nhd>(/),<ni>(I15,a30,/),<nf-1>(f15.5,a30,/),f15.5,a30)
          
          read (parinp_io,119)   !Type  Debuging values (RB867515) - Not totally calibrated yet
     &	idev 			,skip, !(I)   1          
     &	swgc 			,skip, !(I)   1          
     &	swcf    		,skip, !(I)   1
     &	swinter 		,skip, !(I)   1        
     &	swcfbs  		,skip, !(I)   1                 
     &	swroottyp 		,skip, !(I)   1
     &	pgmethod        ,skip, !(I)   1
     &	pfmethod        ,skip, !(I)   2                           
     &	ngrnodes    	,skip, !(I)   5  
     &	init_nlf    	,skip, !(I)   2  
     &	maxdevgl		,skip, !(I)   5
     &	co2     		,skip, !(R)   390. 
     &	rowsp           ,skip, !(R)   140. 
     &	Topt            ,skip, !(R)   32.        
     &	Tbi         	,skip, !(R)   16.             
     &	tb              ,skip, !(R) 11.0530
     &    to_pg1          ,skip, !(R)  30.d0 !Value From Sugarcane: Physiology,Biochemistry, and Functional Biology (Book) [Chapter 6] -> Sage et al. (2013)  
     &    to_pg2          ,skip, !(R)  35.d0 !Value From Sugarcane: Physiology,Biochemistry, and Functional Biology (Book) [Chapter 6] -> Sage et al. (2013)
     &    tbM             ,skip, !(R)  42.d0 !Value From Sugarcane: Physiology,Biochemistry, and Functional Biology (Book) [Chapter 6] -> Sage et al. (2013)
     &	rue             ,skip, !(R)  3.2         
     &	sla             ,skip, !(R)  80.0        
     &	extcoef         ,skip, !(R)   0.6500     
     &	sgpf            ,skip, !(R)   0.8800     
     &	dpercoeff       ,skip, !(R)   4.0000     
     &	sucmax          ,skip, !(R)   0.7000
     &	SucAccFac       ,skip, !(R)   10.        
     &	chustk          ,skip, !(R) 900.0000     
     &	chupeak         ,skip, !(R) 200.0000     
     &	chudec          ,skip, !(R) 230.0000     
     &	chumat          ,skip, !(R) 500.0000     
     &	popmat          ,skip, !(R)  11.0000     
     &	poppeak         ,skip, !(R)  22.0000
     &	maxgl           ,skip, !(R) 10.0000     
     &	phyloc          ,skip, !(R)  95.0000     
     &	mla             ,skip, !(R) 675.0000
     &	maxitdw     	,skip, !(R)   10.
     &	itshp       	,skip, !(R)   5.         
     &	sucshp      	,skip, !(R)   3.5
     &	lfshp       	,skip, !(R)    7.
     &	srl             ,skip, !(R)  18.0000
     &	srlmax          ,skip, !(R)   15.        
     &	srlmin          ,skip, !(R)   10.   
     &	rdc 			,skip, !(R)   200.  
     &	alphacrit 		,skip, !(R)   1.         
     &	HLIM1  			,skip, !(R)  -0.1        
     &	HLIM2U 			,skip, !(R)  -5.         
     &	HLIM2L 			,skip, !(R)  -5.         
     &	HLIM3H 			,skip, !(R) 790.0        
     &	HLIM3L 			,skip, !(R) 1000.0       
     &	HLIM4  			,skip, !(R) 15000.0      
     &	ADCRH  			,skip, !(R)   0.         
     &	ADCRL  			,skip, !(R)   0.
     &	rwuep1          ,skip, !(R)   2.         
     &	rwuep2          ,skip, !(R)   1.  
     &	cfbs    		,skip, !(R)   0.4        
     &	kc_min  		,skip, !(R)   0.70       
     &	eoratio 		,skip, !(R)   1.5          
     &	albedo  		,skip, !(R)   .23        
     &	rsc     		,skip, !(R)   70.        
     &	rsw     		,skip, !(R)   0.         
     &	cofab 			,skip, !(R)   0.25
     &	pdep_Rat        ,skip, !(R)   10.        
     &	init_stalk_fw   ,skip, !(R)   1.5        
     &	init_stalk_ht   ,skip, !(R)   2.         
     &	nstalks_planting,skip, !(R)   2.         
     &	frac_dw_pl      ,skip, !(R)   0.3        
     &	frac_suc_pl     ,skip, !(R)   0.5
     &	emerg_tillers   ,skip, !(R)   4.
     &	shootrate_bg    ,skip, !(R)   0.088      
     &	rootdrate       ,skip, !(R)   0.048      
     &	rootshape       ,skip, !(R)   3.      
     &	f_rts           ,skip, !(R)   0.6        
     &	initrootdwrate  ,skip, !(R)   0.0015     
     &	biomass_useRate ,skip, !(R)   0.00025    
     &	minrgpf         ,skip, !(R)   0.05       
     &	maxlgpf         ,skip, !(R)   0.45
     &	rate_af         ,skip, !(R)   0.05       
     &	rgpf_be         ,skip, !(R)   0.5
     &	pfshp           ,skip, !(R)   5.         
     &	maxsgpf_ap      ,skip, !(R)   0.6        
     &	lfcb_ratio      ,skip, !(R)   5.         
     &	dwat_dws        ,skip, !(R)   8.         
     &	dwat_dsuc       ,skip  !(R)   3.            
          
          close(parinp_io)


      !Initializing crop state variables        
      nstk        = 0.d0
      initnstk    = 0.d0
      ln          = 0.d0
      lntotal     = 0.d0
      devgl       = 0.d0
      deadln      = 0.d0
      la          = 0.d0
      lai         = 0.d0
      w           = 0.d0
      wl          = 0.d0
      wt          = 0.d0
      !wr          = 0.d0 !initalized after on
      ws          = 0.d0
      wa          = 0.d0
      wdead       = 0.d0
      wstkwat     = 0.d0
      wsfresh     = 0.d0
      stkdmc      = 0.3d0
      suc         = 0.d0
      fib         = 0.d0
      pol         = 0.d0
      pleng       = 0.d0          
      noden       = 0.d0
	shootdepth  = 0.d0
      diac        = 0.d0
      diacem      = 0.d0
      diaclf      = 0.d0
      ddeadln     = 0.d0
      rootsene    = 0.d0      
      rgpf_ck     = 0.d0  !This is a daily state
      lgpf_ck     = 0.d0  !This is a daily state
      sgpf_ck     = 0.d0  !This is a daily state
      tgpf_ck     = 0.d0
      midtime_rgpf= 0.d0
      midtime_sgpf= 0.d0
      age_stkemer = 0.d0
      itnumber    = 0.d0
      ngl         = 0.d0
      RGP_pg      = 0.d0
      IPAR_acc    = 0.d0
      w_pg        = 0.d0
      wa_pg       = 0.d0
      arue_dw     = rue
      arue_w      = rue
      arue_dwa    = rue
      arue_wa     = rue
      
      !Reduction factors
      agefactor   = 1.d0
      swfacp      = 1.d0  
      swface      = 1.d0
      wuf         = 1.d0
      co2_fac     = 1.d0
      tstress     = 1.d0
      RGP_fac     = 1.d0
      stk_agefac  = 1.d0
      rootsene    = 1.d0      
      
      rgpf        = 0.d0
      lgpf        = 0.d0
      sgpf        = 0.d0
      agpf        = 0.d0
      tppf        = 0.d0
      cbpf        = 0.d0
      sgpf_ap     = 0.d0
      lgpf_ap     = 0.d0
      tppf_ap     = 0.d0
          
      flemerged       = .false.
      flcarboncheck   = .true.
      flstalkemerged  = .false.
                
      !--- Arrays initiation
      cumlw       = 0.d0
      cumla       = 0.d0
      wpp         = 0.d0
      fcp         = 0.d0
      stp         = 0.d0
      swcon2      = 0.d0
      laythi      = 0.d0
      prwulay     = 0.d0
      internode   = 0.d0
      stk_age     = 0.d0
      lfage       = 0.d0
      lfarea      = 0.d0
      lfweight    = 0.d0
      stk_itn     = 0.d0      
      itage       = 0.d0
      itpf        = 0.d0
      itlen       = 0.d0
      itsuc       = 0.d0
      itfib       = 0.d0
      ittdw       = 0.d0
                
      !Check whether it is the plant or ratoon
          if(iyear .eq. 2012)then          
                  
                  !--- Plant Cane                  
                  !--- Chopped stalks planting method
                  init_stalk_fw       = 1.5d0 !kg
                  init_stalk_ht       = 2.d0  !m
                  nstalks_planting    = 2.d0  !#
                  init_pop            = emerg_tillers * 1. /(rowsp/100.) !assuming emerg_tillers m-1
                                 
                  initbiomass = (init_stalk_fw / init_stalk_ht * 
     & nstalks_planting) / (rowsp/100.)
                  
                  !--- Considering 70% of water and converting to t ha-1
                  initbiomass = initbiomass * frac_dw_pl / 1.e3 * 1.e4
                  
                  !--- Daily increment in Dry biomass due to planting
                  availw_bg   = initbiomass * frac_suc_pl !Considering that half of dry biomass is inert material (fiber) and remaining is sucrose (reserve)
                  wr          = 0.d0
                  w           = wr + availw_bg    !All plant biomass is its reserves (Go bud,Go!)
                  plantdepth  = 25. !Warning: Crop/Management parameter
                  rld         = 0.d0
                  
                  !--- Crop Status
                  canetype    = ' Plant'
                  flcropalive = .true.
                  canestatus  = ' Alive'
                  canestage   = 'Sprout'
                  
                  !--- Ratooning count
                  ratcount = 0
                  
          else
                  
                  !--- Ratooning count
                  ratcount = ratcount + 1
                  
                  frac_rtRes  = 0.5 !Warning: Crop parameter
                  frac_rtRem  = 0.1
                  !Warning: Crop parameter
              
                  !--- Ratooning                  
                  availw_bg   = wr  * frac_rtRes
                  wr          = wr  * frac_rtRem
                  rld         = rld * frac_rtRem  !Array Algebra
                  w           = wr + availw_bg    !All plant biomass is roots and reserves
                  plantdepth  = pdep_Rat          !Set to crop depth on ratooning
                  init_pop    = emerg_tillers * 1. / (rowsp/100.) !assuming emerg_tillers m-1
                  
                  !Crop Status
                  write(canetype,'(A3,I2.2)') 'Rat',ratcount               
                  canestage   = 'Sprout'
                  if(flcropalive) canestatus  = ' Alive'
                  
              endif
              
              !Initial crop depth
              initcropdepth = plantdepth          
      
      !    !Part7 ---> soil water extraction by plant roots (Feddes approach)      !    
      !    !----------------------------------------------------------------------------------------------------------------------------------
      !    !----------------------------------------------------------------------------------------------------------------------------------
      !    !Values From Scarpare PHD Dissertation (2011): "Simulação do crescimento da cana-de-açúcar pelo modelo agrohidrologico SWAP/WOFOST" - Quoted as Qureshi Dissertation (1999)
      !    !HLIM1  =     -10.  ! No water extraction at higher pressure heads, [-100..100 cm, R]
      !    !HLIM2U =     -25.  ! h below which optimum water extr. starts for top layer, [-1000..100 cm, R]
      !    !HLIM2L =     -25.  ! h below which optimum water extr. starts for sub layer, [-1000..100 cm, R]
      !    !HLIM3H =    -1000.0  ! h below which water uptake red. starts at high Tpot, [-10000..100 cm, R]
      !    !HLIM3L =    -2000.0  ! h below which water uptake red. starts at low Tpot, [-10000..100 cm, R]
      !    !HLIM4  =   -10000.0 ! No water extraction at lower pressure heads, [-16000..100 cm, R]
      !    !ADCRH  =       0.5  ! Level of high atmospheric demand, [0..5 cm/d, R]     
      !    !ADCRL  =       0.1  ! Level of low atmospheric demand,  [0..5 cm/d, R]     
      !    !----------------------------------------------------------------------------------------------------------------------------------
      !    !----------------------------------------------------------------------------------------------------------------------------------
      !    
      !    !----------------------------------------------------------------------------------------------------------------------------------
      !    !----------------------------------------------------------------------------------------------------------------------------------
      !    !Values From Qureshi PHD Dissertation (1999) in pg.64: "Simulation modeling of irrigation requirements for sugarcane production in Sindh province, Pakistan" - Quoted by Scarpare (above)
      !    !In the sensitivity analysis of the same work, no variation was found on results by changing feddes limits (pg. 108)!
      !    !HLIM1  =     -0.1  ! No water extraction at higher pressure heads, [-100..100 cm, R]
      !    !HLIM2U =     -5.   ! h below which optimum water extr. starts for top layer, [-1000..100 cm, R]
      !    !HLIM2L =     -5.   ! h below which optimum water extr. starts for sub layer, [-1000..100 cm, R]
      !    !HLIM3H =   -790.0  ! h below which water uptake red. starts at high Tpot, [-10000..100 cm, R]
      !    !HLIM3L =  -1000.0  ! h below which water uptake red. starts at low Tpot, [-10000..100 cm, R]
      !    !HLIM4  = -24000.0  ! No water extraction at lower pressure heads, [-16000..100 cm, R]
      !    !ADCRH  =       0.5  ! Level of high atmospheric demand, [0..5 cm/d, R]     
      !    !ADCRL  =       0.1  ! Level of low atmospheric demand,  [0..5 cm/d, R]     
      !    !----------------------------------------------------------------------------------------------------------------------------------
      !    !----------------------------------------------------------------------------------------------------------------------------------
      
      !    !----------------------------------------------------------------------------------------------------------------------------------
      !    !----------------------------------------------------------------------------------------------------------------------------------
      !    !Inconplete values From SWAP Manual (2009) pg. 245 - Quoted after Taylor and Ashcroft (1972)
      !    !SUGARCANE HAS TWO SETS OF VALUES (TENSIOMETERS and BLOCKS):
      !    
      !    !TENSIOMETERS:
      !    !HLIM1  =     -?.    ! No water extraction at higher pressure heads, [-100..100 cm, R]
      !    !HLIM2U =     -?.    ! h below which optimum water extr. starts for top layer, [-1000..100 cm, R]
      !    !HLIM2L =     -?.    ! h below which optimum water extr. starts for sub layer, [-1000..100 cm, R]
      !    !HLIM3H =    -150.0  ! h below which water uptake red. starts at high Tpot, [-10000..100 cm, R]
      !    !HLIM3L =    -500.0  ! h below which water uptake red. starts at low Tpot, [-10000..100 cm, R]
      !    !HLIM4  =     -?.0   ! No water extraction at lower pressure heads, [-16000..100 cm, R]
      !    !ADCRH  =       ?    ! Level of high atmospheric demand, [0..5 cm/d, R]     
      !    !ADCRL  =       ?    ! Level of low atmospheric demand,  [0..5 cm/d, R]
      !    
      !    !BLOCKS:
      !    !HLIM3H =    -1000.0  ! h below which water uptake red. starts at high Tpot, [-10000..100 cm, R]
      !    !HLIM3L =    -2000.0  ! h below which water uptake red. starts at low Tpot, [-10000..100 cm, R]
      !    
      !    !----------------------------------------------------------------------------------------------------------------------------------
      !    !----------------------------------------------------------------------------------------------------------------------------------
      !    
      !    !Using Values From Qureshi PHD Dissertation (1999) in pg.64 -> BUT HLIM4 WAS SET TO -15000 (More physiologically accepted)
      !    HLIM1  =     -0.1  ! No water extraction at higher pressure heads, [-100..100 cm, R]
      !    HLIM2U =     -5.   ! h below which optimum water extr. starts for top layer, [-1000..100 cm, R]
      !    HLIM2L =     -5.   ! h below which optimum water extr. starts for sub layer, [-1000..100 cm, R]
      !    HLIM3H =   -790.0  ! h below which water uptake red. starts at high Tpot, [-10000..100 cm, R]
      !    HLIM3L =  -1000.0  ! h below which water uptake red. starts at low Tpot, [-10000..100 cm, R]
      !    HLIM4  = -15000.0  ! No water extraction at lower pressure heads, [-16000..100 cm, R]
      !    ADCRH  =       0.5  ! Level of high atmospheric demand, [0..5 cm/d, R]     
      !    ADCRL  =       0.1  ! Level of low atmospheric demand,  [0..5 cm/d, R]
      !    
          
          !Initial Leaf Area of a Single Leaf [cm2]
          !Assuming (15 x 2 cm and 0.75 shape factor)
          init_la         = ((15.d0 * 2.d0 * 0.75))          
          
          !--- Calibration Section         
          flcalibrate = .false.
          
          if(flcalibrate)then
          !--------------------------------------------------
          !WARNING: ADDED HERE TO CALIBRATE PARAMETERS
          !Set the R function to :
          !(i)    Call this model.exe 
          !(ii)   Compute objective function
          !(iii)  Optmize below parameters  
          ! More info in my github repository (Murilo Vianna)
          ! https://github.com/Murilodsv/R-scripts/blob/master/Optimization_SWAP_Sugarcane_v1.R    
          !--------------------------------------------------
          
          open(calibr_io,FILE='Param_optim.opt',
     &      STATUS='OLD',READONLY)          
          
          call readrea('Calibration',2,real_host,1,.false.,
     &.false.,calibr_io,'Param_optim.opt',messag)
          
          rootshape = real_host(1)
          
          close(calibr_io) 
          endif
       
      !--- Roots Initiation
          call    rootgrowth(1,plantdepth,initcropdepth,rtcumdens,     
     &    srlmax,srlmin,dwr,rdenshape,rootshape,rpup,effective_rd,rld,
     &    diac,di,rdepth,flemerged,rdm,rootdrate,drld,drdepth)
          
      !--- calculate swc in wilting point(hlim4), field capacity, saturated point and swcon2 for potential rwu    
          do node = 1, numnod
          wpp(node) = watcon(node,hwpp,cofgen(1,node),swsophy,
     &    numtab,sptab)          
          fcp(node) = watcon(node,hfcp,cofgen(1,node),swsophy,
     &    numtab,sptab)    
          stp(node) = watcon(node,hstp,cofgen(1,node),swsophy,
     &    numtab,sptab)   
          
              if (wpp(node) .GT. 0.30) then
              swcon2(node) = 45.0
              end if
          
          swcon2(node) = 120. - 250. * wpp(node)          
          !Alternatively: swcon2(node) = 62.
          
          laythi(node) = abs(abs((z(node)+ 0.5*dz(node))) - 
     &     abs((z(node) - 0.5*dz(node))))
                      
          end do
       
          swcon1 = 1.32E-3 !L/?/?
          swcon3 = 7.01    !L/?/?          
          
          !swcon1 = 2.67E-3 !L/?/?
          !swcon3 = 6.68    !L/?/?
          
      !--- Max green leaf area (to water consumption)
          maxlai      = 0.d0
          do i =1, maxgl
              maxlai      = maxlai + poppeak * mla * AScurv(1,
     & phyloc*i,0.,1.,lfshp,((maxgl-maxdevgl)*phyloc/2.),1.)  / 1.e4 
          enddo
      
      !--- Linking with SWAP crop parameters
      kdif    = extcoef
      kdir    = kdif
                    
      !--- Linking with SWAP variables
      tmax = tmx                  !(oC)
      tmin = tmn                  !(oC)
      t_mean = .5 * (tmax + tmin) !(oC)
      par = (rad * .5)/0.1E7      !(MJ/m2.d
      epp = ptra                  !(cm/d)
      ch = pleng * 100.           !(cm) Used for Penmam-M. method for et0
          
      !--- Opening output file         
      if(icrop .eq. 1)then
          open(defout_io,FILE='
     &    Plant_'//trim(project)//'.OUT',STATUS='REPLACE',RECL=1180)   
          
          !--- Detailed output  
          if(fldetrtsyout)then              
              open(detrtout_io,FILE='
     &    DetRootSy_'//trim(project)//'.OUT',STATUS='REPLACE',RECL=1180)
          endif
          
          if(fldetlfprout)then              
              open(detlfout_io,FILE='
     &    DetLeafPr_'//trim(project)//'.OUT',STATUS='REPLACE',RECL=1180)
          endif
          
          if(fldetitprout)then              
              open(detitout_io,FILE='
     &    DetIntePr_'//trim(project)//'.OUT',STATUS='REPLACE',RECL=3180)
          endif
          
          if(fldetpgfaout)then              
              open(detpgout_io,FILE='
     &    DetPGFac_'//trim(project)//'.OUT',STATUS='REPLACE',RECL=1180)
          endif
          
      endif
          
          !--- Writing the Main Output file header            
		write(defout_io,11) '#Simulating for ', project
		write(defout_io,13) 
		write(defout_io,14)
		write(defout_io,15)
		write(defout_io,16)
		write(defout_io,17)
          
          !Outputs Header
          !        Day   Days   Days  Accum    Plant   Avail    Root    Leaf    Tops   Stalk  Sucros   Fiber   Stalk          Aerial                   Plant   Devel  Number  Stress  Stress  RootGr  LeafGr  StalGr  TopPGr
          !         of  after  after  D.Day   Weight  Reserv  Weight  Weight  Weight  Weight  Weight  Weight  Weight     POL     Dry     LAI  Tiller  Height   GLeaf  Intern  Factor  Factor   PFact   PFact   PFact   PFact
          !Year   Year  Simul  Plant   oC.d   DMt/ha  DMt/ha  DMt/ha  DMt/ha  DMt/ha  DMt/ha  DMt/ha  DMt/ha  FMt/ha       %  DMt/ha   m2/m2    #/m2       m   #/Stk   #/Stk  Expans  Photos   (0-1)   (0-1)   (0-1)   (0-1)
          !----  -----  -----  -----  ------ -------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------
	
11        format(2A,/) 		
12        format(2A,/)
13        format('Result of sugarcane growth simulation in daily step:')
14        format('        Day   Days   Days  Accum    Plant   Avail',
     & '    Root    Leaf    Tops   Stalk  Sucros   Fiber   ',
     & 'Stalk          Aerial                   Plant   Devel  ',
     & 'Number  Stress  Stress  RootGr  LeafGr  StalGr  TopPGr')
          
15        format('         of  after  after  D.Day   Weight  ',
     & 'Reserv  Weight  Weight  Weight  Weight  Weight  Weight  ',
     & 'Weight     POL     Dry     LAI  Tiller  Height   GLeaf  ',
     & 'Intern  Factor  Factor   PFact   PFact   PFact   PFact',
     & '   Cane     Cane    Cane')
     
16        format('Year   Year  Simul  Plant   oC.d   DMt/ha  ',
     & 'DMt/ha  DMt/ha  DMt/ha  DMt/ha  DMt/ha  DMt/ha  DMt/ha  ',
     & 'FMt/ha       %  DMt/ha   m2/m2    #/m2       m   #/Stk   ',
     & '#/Stk  Expans  Photos   (0-1)   (0-1)   (0-1)   (0-1)   Type ',
     & '  Status   Stage')
     
17        format('----  -----  -----  -----  ------ -------  ------  ',
     & '------  ------  ------  ------  ------  ------  ------  ',
     & '------  ------  ------  ------  ------  ------  ------  ',
     & '------  ------  ------  ------  ------  ------  ------  ',
     & '------  ------')
          
          !--- Detailed Ouputs Headers
          if(fldetrtsyout)then              
              !--- Writing the Main Output file header            
		    write(detrtout_io,11) '#Simulating for ', project      
              write(detrtout_io,18)
          endif
              
          if(fldetlfprout)then              
              !--- Writing the Main Output file header            
		    write(detlfout_io,11) '#Simulating for ', project      
              write(detlfout_io,19)
          endif
              
          if(fldetitprout)then              
              !--- Writing the Main Output file header            
		    write(detitout_io,11) '#Simulating for ', project      
              write(detitout_io,20)
          endif
              
          if(fldetpgfaout)then              
              !--- Writing the Main Output file header            
		    write(detpgout_io,11) '#Simulating for ', project      
              write(detpgout_io,21)
          endif
              
18        format('Result of sugarcane RootSystem:')
19        format('Result of sugarcane LeafProfile:')
20        format('Result of sugarcane StalkProfile:')
21        format('Result of sugarcane PG Factors:') 
            
	return      
      
2000  continue
          
      !--- Linking with SWAP Variables          
      tmax = tmx                  !(oC)
      tmin = tmn                  !(oC)
      t_mean = .5 * (tmax + tmin) !(oC)
      par = (rad * .5)/0.1E7      !(MJ/m2.d)
      epp = ptra                  !(cm/d)
      ch = pleng * 100.           !(cm) Used for Penmam-M. method
      
      !Hourly Temperature
      call TempHour(tmax,tmin,thour)
      
      !Reset rates      
      di          = 0.d0
      dw          = 0.d0
      dw_ss       = 0.d0
      mresp       = 0.d0
      gresp       = 0.d0
      resp        = 0.d0
      dwdead      = 0.d0
      dwr         = 0.d0
      dwl         = 0.d0
      dleafdm     = 0.d0
      dwt         = 0.d0
      dws         = 0.d0
      dstkdm      = 0.d0
      dwa         = 0.d0
      dla         = 0.d0
      ddealla     = 0.d0
      ddeallw     = 0.d0
      ddeadtop    = 0.d0
      deadln      = 0.d0
      pg          = 0.d0
      dRGP_pg     = 0.d0
      dnetbiomass = 0.d0
      dwwater     = 0.d0
      dsuc        = 0.d0
      dfib        = 0.d0
      dper        = 0.d0
      dnstk       = 0.d0
      drdepth     = 0.d0
      drld        = 0.d0  !Array
      dnoden      = 0.d0
      dnleaf      = 0.d0
      ddeadln     = 0.d0
      rgpf_ck     = 0.d0  !This is a daily state
      lgpf_ck     = 0.d0  !This is a daily state
      sgpf_ck     = 0.d0  !This is a daily state
      tgpf_ck     = 0.d0
      li          = 0.d0
      swfacp      = 1.d0
      swface      = 1.d0
      RGP_fac     = 1.d0
      
      !--- Crop Development      
	!--- Calculating degree-days using a unique tb          
		if (t_mean .ge. tb .and. t_mean .le. tbM) then
			    di = (t_mean-tb)     ! Degree-Days for today
          else              
			    di = 0.d0
          endif          
		diac = diac + di            !Cumulative Degree-Days
          if(flemerged) diacem = diacem + di
          
          !Check if stalks emerged
          if(diac .gt. chustk .and. .not. flstalkemerged)then
              flstalkemerged  = .true.
              canestage       = 'StkGro'
              
              !initiate Stalks
              wstkwat     = wa * (1.d0/0.3d0 - 1.d0) !Water content in stalks (70% water ~ potential condition)
              noden       = 1.d0
              age_stkemer = stk_age(1,1)
          endif
          
          !Age Factor
          !Empirical factor to reduce crop processes due to crop aging, e.g.: Stalk extension and photosynthesis (RGP: Reduced Growth Phenomena)
          agefactor = exp(-rate_af*(diac/1000.)) 
          agefactor = min(1.,agefactor)
          
          if(.not. flemerged)then
              
          !------------------------------!
          !-------Before Emergence-------!
          !------------------------------!
              
              !--- Shoot expansion towards soil surface
		    shootdepth  = plantdepth - diac * shootrate_bg !Assuming the primary shoot growth rate of 0.08 cm/DG (Keating et. al. 1999)
		    
              !--- Maintenance Respiration from Liu & Bull (2001) [Fig3]
              !--- doi.org/10.1016/S0304-3800(01)00372-6
              !--- Note: q10 curves are more apropriate to follow classical Crop Models (Thornley & Campbel, 1990)
              mresp   = 3.991 * exp(0.046 * t_mean)                       !g CO2 g-1 w min-1 
              mresp   = mresp *((24.d0*60.d0)/10.d0**6)*(CH2O_M/CO2_M)    !g CH2O g-1 w d-1
              mresp   = mresp * w ! t ha-1 d-1
              
              !--- Reduce Available Sugars (Maintenance is priority)
              availw_bg   = max(0.d0, availw_bg - mresp)
              
              !--- Sink Strength
              !--- Assuming 30% of cost (Growth Respiration)
              dw_ss       = availw_bg * (biomass_useRate * diac) * 
     & 1.d0/(1.d0-0.3d0)
              
              !--- Carbon Balance
              if(availw_bg .ge. dw_ss)then
                  !Reserves can supply for potential growth
                  gresp       = dw_ss * 0.3d0
                  dw          = dw_ss - gresp                  
                  availw_bg   = availw_bg - dw_ss
              else
                  if(dw_ss .gt. 0.d0)then !To Avoid NaN
                      gresp       = dw_ss * 0.3d0     *(availw_bg/dw_ss)
                      dw          = dw_ss*(1.d0-0.3d0)*(availw_bg/dw_ss)
                      availw_bg   = 0.d0
                  endif                      
              endif
              
              !Total Respiration
              resp = mresp + gresp !t ha-1
		    
              !Partitioning factors
              rgpf        = rgpf_be   !biomass to roots
              lgpf        = 0.d0      !No blade leaves
              tppf        = 1. - rgpf !Top parts (Below ground shoot)
              sgpf        = 0.d0      !No stalks
              agpf        = 0.d0      !No aerial parts
                  
              !Allocate to crop pools
              dwr         = dw * rgpf !Roots
              dwl         = 0.d0      !No Above ground Leaves biomass
              dwt         = dw * tppf !Top parts (Below ground shoot)
              dws         = 0.d0      !No Above ground Stalks biomass
              
              !There is no Biomass Gain, only re-allocation of reserves
              dw  = 0.d0
              
              if(availw_bg .le. 0.d0)then
                  !Not enough biomass for shoot emergence                
                  flcropalive = .false.   !Crop is dead
                  canestatus  = 'DeadLC'  ! LC - Lack of Carbon
                  !Warning Msg: Plant it shallow or increase chopped stalks biomass  
              endif
              
              !For checking purpose    
              rgpf_ck   = rgpf
              lgpf_ck   = lgpf
              sgpf_ck   = sgpf
              tgpf_ck   = tppf
                  
              !Use soil Kc until crop is below ground
              cf          = cfbs              
              
          else	    
		
          !------------------------------!   
          !--------After Emergence-------!
          !------------------------------!
          
          !Crop coefficient as function of leaf area index
          cf = kc_min + (eoratio - kc_min) * lai / (maxlai)
          cf = max(0.d0,cf)
          
          select case(pgmethod)                  
          case(1)
              !--- Compute Potential Canopy Photosynthesis based on RUE (pg in tDW ha-1)              
              if(di .gt. 0.d0)then !No photosynthesis below Tb                  
              call pgs_rue(par,co2,tmax,tmin,thour,swfacp,agefactor,lai,
     & extcoef,rue,diac,w,pg,li,tstress,co2_fac,tb,to_pg1,to_pg2,tbM)
              endif           
      
      
              case(2)
              !To be included...
              !Can call totass() -> Layered Canopy Method (J. Goudriaan)
          end select 
                 
      !Dry biomass partitioning to roots, leaves and stalks - With no Water Stress                 
              select case(pfmethod)
              case(1)
                  
                  !--- From original Marin & Jones (2014)
                  
                  if(diac .lt. chustk)then              
                  
                  !Mid time for rgpf decay
                  midtime_rgpf = (chustk) / 2.d0
                  
                  if(midtime_rgpf .gt. 0.d0)then                  
                  rgpf = ((rgpf_be-minrgpf)/(1.d0+exp(diacem*(6.d0/
     & (midtime_rgpf)) - 6.d0))) + minrgpf
                  rgpf = max(minrgpf,min(1.d0,rgpf)) !Constraining to minrgpf and 1
                  else
                      rgpf    = minrgpf
          !Warning:
          write(*,*) "Warning:Stalks start growing before emergence"
          write(*,*) "Check: CHUSTK or Planting Depth parameters"
                  endif
                  
				lgpf = 1. - rgpf !Complementary
                  sgpf = 0.d0 !No above ground Stalks yet. Warning: physiologically this may not be true.    
        
          elseif(diac .ge. chustk)then              
                  
                  !Mid time for rgpf decay
                  midtime_rgpf = chustk / 2.d0
                  
                  if(midtime_rgpf .gt. 0.d0)then                  
                  rgpf = ((rgpf_be-minrgpf)/(1.d0+exp(diacem*(6.d0/
     & (midtime_rgpf)) - 6.d0))) + minrgpf
                  rgpf = max(minrgpf,min(1.d0,rgpf)) !Constraining to minrgpf and 1
                  else
                      rgpf    = minrgpf
          !Warning:
          write(*,*) "Warning:Stalks start growing before emergence"
          write(*,*) "Check: CHUSTK or Planting Depth parameters"
                  endif
                  
                  lgpf = 1. - rgpf !Complementary
                  !Constraint to leaf maximum sink capacity
                  if(lgpf .gt. maxlgpf)then
                      lgpf = maxlgpf
                  endif                 
                  
                  sgpf = (1. - rgpf - lgpf) / (1. + chustk * exp(-diac))
                  
                  !Carbon Balance                  
                  if(rgpf + lgpf + sgpf .lt. 1.d0)then
                      !Allocate to stalks
                      sgpf = 1. - (rgpf + lgpf)
                  elseif(rgpf + lgpf + sgpf .gt. 1.d0)then
                      !Canopy is priority (reduce sgpf to match the unit)                       
                      sgpf = 1. - (rgpf + lgpf)
                  endif		
          endif
                  
                  
              case(2)
                 
                  !--- Partitioning factors as funtion of thermal age
                  !--- Based on experimental data 
                  !--- Implemnted by Murilo Vianna
                 
                  !Mid time for rgpf decay
                  midtime_rgpf = chustk / 2.d0
                  
                  !Root partitioning
                  rgpf = AScurv(1,diacem,rgpf_be,minrgpf,pfshp,
     & midtime_rgpf,1.)
                  
                  !Aerial Parts
                  agpf = 1.d0 - rgpf
                  
                  !Mid time for maxsgpf (aerial parts)
                  midtime_sgpf = chustk + 0.5d0*(chumat-chustk)
                  
                  !Stalks Partitioning (aerial parts)
                  sgpf_ap = AScurv(1,diacem,0.,maxsgpf_ap,pfshp,
     & midtime_sgpf,1.)
                  
                  if(.not. flstalkemerged)then
                      sgpf_ap = 0.d0 !When stalks did not emerged yet (the inital rate may start slitly before chustk and carbon balance will not match)
                  endif
                  
                  !Partitioning factors
                  tppf_ap = 1.d0      - sgpf_ap
                  sgpf    = sgpf_ap   * agpf
                  tppf    = tppf_ap   * agpf                  
                  lgpf    = (lfcb_ratio / (lfcb_ratio + 1.d0)) * tppf
                  cbpf    = tppf - lgpf  
              
              end select
		
          endif
                     
      return
      
3000  continue    
      
      if(.not. flemerged)then
          
          !------------------------------!   
          !-------Before Emergence-------!
          !------------------------------!
          
          !Check if crop is emerged
          if(shootdepth .LT. 1.d-8)then
			    flemerged   = .true. !Emerged
			    shootdepth  = 0.d0
                  chuem       = diac
                  initnstk    = init_pop
                  nstk        = initnstk
                  la          = init_la / 1.e4 !One leaf area in m2 (dim: 15 x 2 cm)
                  ln          = 2.d0
			    lntotal     = ln
                  lai         = nstk * ln * la !Considering two developed leaves per stalk
                  canestage   = 'Emergd'
                  
          endif             
          
      else
          
          !------------------------------!   
          !--------After Emergence-------!
          !------------------------------!
        
        ! --- reset root water extraction array
        do node = 1,numnod
          prwu(node) = 0.0d0
        end do
        tqropot = 0.
        
        !---  root senescence
        !--- Depicted as the fraction of effective roots as function of total roots biomass
        rootsene = 1.d0 - ((f_rts * wr) / (wr + 1.d0))
          
       !--- potential RWU 
       !--- Note that rootsene is been used here
       !--- It can be depicted as the efficiency part of root system. 
       !Warning: No evidences for this and very empirical!          
       do node = 1, noddrz
          prwu(node) = max(0.,min(0.07, 
     &  swcon1*EXP(MIN((swcon2(node)*(theta(node)-wpp(node))),40.))/
     &     (swcon3-LOG(rld(node)))))
          
        prwulay(node) = prwu(node) * laythi(node) * rld(node)
          
          tqropot = tqropot + prwulay(node)
       enddo
      !--- Correction of potential transpiration in relation to reference crop
      !     (default = 1.0, range = 0.8 - 1.2)
      !--- Following Wofost()
      ptra = cf*ptra      
      
      ! --- water stress factors in growth and photosynthesis 
      if (ptra .le. 1.d-5) then
          !No atmospheric demand
        wuf       = 0.d0
        swfacp    = 1.d0
        swface    = 1.d0
        
      else
      
      swfacp = max(0.d0, min(1.d0, tra / ptra))        
              
        wuf = max(tqropot/ptra,0.d0)
        
        
          if (wuf .lt. rwuep1) then          
          swface = max(0.,min((1./rwuep1) * wuf,1.))      
          else
          swface = 1.
          endif
          
      wuf_swap = max(tra/ptra,0.d0)
      
          if (wuf_swap .lt. rwuep2) then          
          swfacp = max(0.,min((1./rwuep2) * wuf_swap,1.))      
          else
          swfacp = 1.
          endif     
        
      endif
      
      !--- Water Stress      
      swfacp = max(0.d0, min(1.d0, tra / ptra))
      swface = swfacp * 0.5
      
      
      !--- Crop Growth      
      !--- Actual Gross Photosynthesis (Recall that pg is priory affected by Radiation, Temperature, Age and CO2 - "Defining Factors")
      !--- Here the "Limiting factor" is only water stress (swfacp), nutrients effect are not included (Note that when crop is under water stress, nutrients are not absorbed - increase the uncertainty)
      !--- None "Reducing Factors" are simulated due to the high level of uncertainty/unknow effect
              pg = pg * swfacp 
              
              !--- Crop Respiration
              select case(pgmethod)
                  
              case(1)
              
              !RUE       
              !NOTE: Respiration is set to zero here because we are using RUE method (gDW MJ-1).
              mresp = 0.d0
              gresp = 0.d0
              resp = mresp + gresp    
              
              case(2)
          
              end select
              
      !--- Net Biomass Gain Due to Photosynthesis
           dw = max(0.d0, pg - resp)
          
      !--- Crop Development    
      !--- Tillers Rate calculated in Stalks/m2	
			if(diac .gt. chuem .and. diac .lt. chupeak)then
				dnstk = ((poppeak-initnstk)/(chupeak-chuem)) * di               !initial tiller grow
			elseif (diac .ge. chupeak .and. diac .lt. chudec)then
				dnstk = 0.d0								          ! peak tiller 
			elseif (diac .ge. chudec .and. diac .lt. chumat)then
				dnstk = (-(poppeak - popmat) / (chumat-chudec)) * di  ! reduction phase to mature
			elseif (diac .ge. chumat) then
				dnstk = 0.d0		            					  ! late stable tiller population
              endif
              
              !--- Update tillers age
              do tl = 1, aint(nstk + dnstk)
                  stk_age(tl, 1) = stk_age(tl, 1) + di
                  stk_age(tl, 2) = stk_age(tl, 1) / stk_age(1, 1) 
              enddo
              
              !--- Age factor
              if((nstk + dnstk) .lt. init_pop)then
                  stk_agefac  = 1.d0
              else                  
                  stk_agefac  = sum(stk_age(1:aint(nstk + dnstk), 2)) / 
     & (aint(nstk + dnstk))
              endif
              
              !--- Stalk Canopy Fraction
              call stk_li(stk_age,lai,kdif,nstk)
              
              !--- Canopy Development
              call lais(diac,di,phyloc,nstk,swface,stk_agefac,ln,maxgl,
     & cumla,ddealla,mla,sla,cumlw,ddeallw,dnleaf,dla,dleafdm,dw,lgpf,
     & dnetbiomass,dnoden,ddeadln,devgl,stk_age,lfage,lfarea,lfweight,
     & dnstk,stk_dnetbiomass,init_nlf,lfshp,maxdevgl)
                           
              !Check whether a tiller senesced              
              if(dnstk .lt. 0.d0)then
                  !Top parts dead biomass (t ha-1)
                  ddeadtop = wt * (stk_age(aint(nstk+dnstk),2) / 
     & sum(stk_age(1:aint(nstk+dnstk), 2))) * (-1.d0 * dnstk)
              endif
              
      !--- Dry Biomass Allocation
              if(.not. flstalkemerged)then
              !--- Before Stalks emergence
              !The surplus biomass from leaves are allocated to roots
              !Note that only surplus of biomass is considered, 
              !and the lack of carbon (dnetbiomass < 0) cannot be provided 
              !by reserves using this approach (Unless a constrain in root growth is imposed)                  
                  dwr     = dw * rgpf + max(0.d0, dnetbiomass) !Daily root biomass gain (t ha-1)
                  dwl     = dleafdm
                  dwt     = dw * cbpf
                  dws     = 0.d0
                  dwa     = dwl + dws + dwt
                  dwdead  = ddeallw + ddeadtop
                  
                  !No Stalks expansion or sucrose
                  dnoden      = 0.d0
                  dper        = 0.d0
                  dsuc        = 0.d0
                  dfib        = 0.d0
                  sucexceeded = 0.d0                  
                  
                  !--- Biomass water content assumed to be constant
                  dwwater = 0.d0
			    stkdmc  = frac_dw_pl    
              
              else
      !--- After Stalks emergence                  
              !The surplus biomass from leaves are allocated to stalk biomass which in turn can be Fiber Or Sucrose (DIAMPERS)
                  dwr     = dw * rgpf
                  dwl     = dleafdm
                  dwt     = dw * cbpf
                  dws     = dw * sgpf + max(0.d0, dnetbiomass)                  
                  dwa     = dwl + dws + dwt
                  dwdead  = ddeallw + ddeadtop
                  
                  dnetbiomass = 0.d0 !Transfered to stalks. Recomputed on DIAMPERS, if there is still surplus of C it will be added to roots
                  
      !--- Stalk expansion and sucrose acumulation within internodes
			call DIAMPERS(thour,Tbi,Topt,dpercoeff,swface,agefactor,nstk, !Input
     & pleng,noden,sucmax,SucAccFac,dws,di,diac,phyloc,dw,sgpf,    !Input
     & age_stkemer,dper,dsuc,dfib,sucexceeded,internode,stk_dnetbiomass,
     & dnetbiomass,dstkdm,stk_itn,stk_age,itage,itpf,itlen,itsuc,itfib,
     & ittdw,ngrnodes,itshp,sucshp,maxitdw,flsink_fbres,dRGP_pg)                        !Crop Parameters
              
              !--- Check whether is still biomass to be allocated
              if(dnetbiomass .gt. 0.d0)then
                  dws = dws - dnetbiomass
                  dwr = dwr + dnetbiomass !Allocate to roots                  
                  dnetbiomass = 0.d0
                  dstkdm      = 0.d0
              endif              
                  
              !--- Feedback response on photosynthesis?
              if(flsink_fbres)then
                  !--- This was implemented based on results of McCormick whereas sugarcane sinks may regulate photosynthesis rate (doi : 10.1111/j.1469-8137.2006.01785.x)
                  !--- Here we assume that all surplus biomass that couldnt be allocated to stalks sinks have to be reduced by dRGP_pg
                  !--- This should be modelled numerically or computed earlier by [dRGP_pg = max(0.d0, dw - (TotalSink + Resp))]
                  !--- As we are using RUE this method may work well, although not theoreticaly true (crop may regulate pg dynamicaly not allocate and surplus reduce pg, doesnt make sense)
                  
                  !--- Reduce PG
                  dws = max(0.d0, dws - dRGP_pg)
                  dwa = max(0.d0, dwa - dRGP_pg)
                  dw  = max(0.d0, dw  - dRGP_pg)
                  
              endif
                  
                  !--- Stalk Fresh Mass
                  !--- 0.9 and 0.5 threshold from 1st version of SAMUCA (Marin) - No documentation
                  if(swfacp .lt. 0.9 .and. dws .lt. 0.5 .and. wsfresh 
     & .gt. 0.d0)then
				    
                      !Water Stress
                      !Thresholds from 1st version of SAMUCA (Marin) - No documentation
				    if((wstkwat/wsfresh) .gt. 0.75)then
					
					    dwwater = -( 0.003 * wstkwat) 
					    !write (*,*) "umido"
				
				    elseif((wstkwat/wsfresh) .gt. 0.7 )then 
					
					    dwwater = -( 0.003 * wstkwat) 
					    !write (*,*) "umido"
					
				    elseif((wstkwat/wsfresh) .GT. 0.6 )then
					    dwwater = -( 0.002 * wstkwat) 
					    !write (*,*) "medio"
					
				    elseif((wstkwat/wsfresh) .GT. 0.45 )then
					    dwwater = -( 0.001 * wstkwat) 
					    !write (*,*) "medio"
	
				    elseif((wstkwat/wsfresh) .LT. 0.45 )then
					    dwwater = 0.0
				    !write (*,*) "seco"
                      endif
                  
                  else 
                      !No Water Stress
				    dwwater = (dwat_dws * dws) + (dwat_dsuc * dsuc)
			    endif                      
              
      endif
      
          endif

          !--- Check Carbon Balance
          !--- Note: Carbon Balance will be positive before emergence (dw = 0.d0)
          if(dw .gt. 0.d0)then
              
            flcarboncheck   = .true.
            carbcheck       = 0.d0
            ckthreshold     = 1.d-3 !0.001 t ha-1 or 0.1 g m-2 mass balance error acceptance
            
            if(((dwr+dwl+dws+dwt)-dw).gt.ckthreshold)then
               !--- More allocated carbon than produced
                carbcheck       = ((dwr+dwl+dws+dwt)-dw)
                flcarboncheck   = .false.
            elseif(((dwr+dwl+dws+dwt)-dw).lt.-ckthreshold)
     & then
               !--- Less allocated carbon than produced
                carbcheck       = ((dwr+dwl+dws+dwt)-dw)
                flcarboncheck   = .false.
            endif
                
              !--- Re-Compute Partitioning Factors (only for checking)
              rgpf_ck   = dwr / dw
              lgpf_ck   = dwl / dw
              sgpf_ck   = dws / dw
              tgpf_ck   = dwt / dw
            endif
                
		!--- Integration of daily state variable		
		nstk      = nstk     + dnstk                     !Number of stalks
		ln        = ln       + dnleaf - ddeadln          !Number of green leaves per Stem                   
		noden     = noden    + dnoden                    !Number of internodes
          lai       = lai      + dla - ddealla             !Leaf Area Index
		w         = w        + dw  - dwdead - resp       !Total Dry Biomass
          wl        = wl       + dwl - ddeallw             !Leaf Dry Biomass
          wt        = wt       + dwt - ddeadtop            !Top parts Dry Biomass
          wr        = wr       + dwr                       !Root Dry Biomass
          ws        = ws       + dws                       !Stalk Dry Biomass
		wa        = wa       + dwa - dwdead              !Aerial Dry Biomass	
		wstkwat   = wstkwat  + dwwater                   !Stalks water mass		
		wsfresh   = ws       + wstkwat                   !Stalk Dry + Water Mass = Stalk Fresh Mass
          pleng     = pleng    + (dper / 1000.d0)          !Plant Height
          wdead     = wdead    + dwdead                    !Dead Biomass
          RGP_pg    = RGP_pg   + dRGP_pg                   !Reduced Growth due to sink FeedBack Response
          IPAR_acc  = IPAR_acc + li * par                  !Acculated intecepted PAR
          
          !--- Compute Stalks Sucrose, Fiber (t ha-1), Height (m) and internode number(#stk-1)
          if(ws .gt. 0.d0 .and. flstalkemerged)then
              suc     = 0.d0
              fib     = 0.d0
              pleng   = 0.d0
              do tl = 1, aint(nstk-dnstk)
                  do it = 1, (aint(stk_itn(tl)) + 1)
                      suc     = suc   + itsuc(tl,it) * (1.e4/1.e6)!Sucrose Mass (t ha-1)
                      fib     = fib   + itfib(tl,it) * (1.e4/1.e6)!Fiber Mass   (t ha-1)
                      pleng   = pleng + itlen(tl,it)              !Total Height (mm)
                  enddo
              enddo
              
              pleng   = (pleng / aint(nstk)) / 1.e3        !Height m stk-1
              itnumber= aint(sum(stk_itn(1:aint(nstk))) + 1)/aint(nstk)
          endif
          
          !Averaged total green leaf number per stalk
          if(flemerged) ngl = sum(stk_age(1:aint(nstk),3)) / aint(nstk)
          
          !--- Compute POL only when sucrose and stalk fresh mass are > 0.d0
          if(flstalkemerged) then
              if(suc .gt. 0.d0 .and. wsfresh .gt. 0.d0)then
                  pol = suc / wsfresh * 100.d0
              else
                  pol = 0.d0
              endif              
          endif          
          
          !--- Check Fraction of Dry Biomass
          if(wsfresh .gt. 0.d0) stkdmc = ws/wsfresh !Dry biomass content
          
          !--- Total Water Stored in Stalks (t ha-1)
          if(wstkwat .lt. 0.d0 .and. flstalkemerged)then
              !Critical Water stress: No Water on Stalks
              wstkwat     = 0.d0
              flcropalive = .false. !died of thirst
              canestatus  = 'DeadNW' !NW - No Water
          endif
              
      !--- Root expansion (With incoming dwr)
          call rootgrowth(2,plantdepth,initcropdepth,rtcumdens,     
     &    srlmax,srlmin,dwr,rdenshape,rootshape,rpup,effective_rd,rld,
     &    diac,di,rdepth,flemerged,rdm,rootdrate,drld,drdepth)
                    
      !--- Root length density integration (cm cm-3)
          rld = rld + drld * rootsene
              
          !Root depth    
          rd              = min(rd + drdepth,rdm)
          effective_rd    = min(effective_rd + drdepth, rdm)
          
          rpup    = rpup + drdepth
          rpup    = min(initcropdepth, rpup)
          !if(.not. flemerged)then
          !    rpup    = rpup + drdepth
          !    rpup    = min(initcropdepth, rpup)
          !endif              
		
	!--- write default outputs        
      write(defout_io,9) iyear,daynr,daycum,daycrop,diac,w,availw_bg,wr,
     & wl,wt,ws,suc,fib,wsfresh,pol,wdead,lai,nstk,pleng,devgl,
     & itnumber,swface,swfacp,rgpf_ck, lgpf_ck,sgpf_ck,tgpf_ck,
     & canetype,canestatus,canestage     
      
9     format(I4,4X,I3,3X,I4,4X,I3,1F8.1,22F8.2,2X,A6,2X,A6,2X,A6)

      !--- Detailed output
      !--- Root System Outputs
          if(fldetrtsyout)then
              !--- Only RootSystem
              write(detrtout_io,90) iyear,daynr,daycum,daycrop,diac,wr,
     & rd,rootsene,rld(1:numnod),tqropot,ptra
          endif
              
      !--- Leaf Profile Outputs
          if(fldetlfprout)then
              
              av_lfage    = 0.d0       
              av_lfarea   = 0.d0
              av_lfweight = 0.d0
              
              if(aint(nstk-dnstk) .gt. 0)then
              do tl = 1, aint(nstk-dnstk)
                  do lf  = 1, maxgl+1
                      av_lfage(lf)   = av_lfage(lf) + lfage(tl,lf) / 
     &                                aint(nstk-dnstk)
                      
                      av_lfarea(lf)  = av_lfarea(lf)+lfarea(tl,lf) / 
     &                                aint(nstk-dnstk)
                      
                      av_lfweight(lf)= av_lfweight(lf)+lfweight(tl,lf)/ 
     &                                aint(nstk-dnstk)
                  enddo                  
              enddo              
              endif
              
              write(detlfout_io,90) iyear,daynr,daycum,daycrop,diac,
     & ngl,                                   !Green Leaves stalk-1 (avg)
     & devgl,                                 !Developed Green Leaves stalk-1 (avg)
     & lfarea(outstk_rank,1:(maxgl+1)),       !cm2 green leaf Leaf-1 
     & av_lfarea(1:(maxgl+1)),                !cm2 green leaf Leaf-1 (avg)
     & lfweight(outstk_rank,1:(maxgl+1)),     !g[dm] Leaf-1
     & av_lfweight(1:(maxgl+1))               !g[dm] Leaf-1 (avg)
              
          endif
          
      !--- Internode Profile Outputs
          if(fldetitprout)then
              
              av_itlen    = 0.d0
              av_itsuc    = 0.d0
              av_ittdw    = 0.d0
              
              !--- Internodes averaged profile
                  if(flstalkemerged)then                      
                      do tl = 1, aint(nstk-dnstk)
                          do it  = 1, itoutnumber
                      av_itlen(it) = av_itlen(it) + itlen(tl,it) / 
     &                                aint(nstk-dnstk)
                      
                      av_itsuc(it) = av_itsuc(it) + itsuc(tl,it) / 
     &                                aint(nstk-dnstk)
                      
                      av_ittdw(it) = av_ittdw(it) + ittdw(tl,it)/ 
     &                                aint(nstk-dnstk)
                          enddo                  
                      enddo                  
                  endif
                  
              write(detitout_io,90) iyear,daynr,daycum,daycrop,diac,
     & itnumber                                        ,  !internodes stalk-1 (avg)
     & itlen(outstk_rank,1:itoutnumber)   / 10.d0      ,  !cm internode-1
     & av_itlen(1:itoutnumber)            / 10.d0      ,  !cm internode-1 (avg)
     & itsuc(outstk_rank,1:itoutnumber)                ,  !g  internode-1
     & av_itsuc(1:itoutnumber)                         ,  !g  internode-1 (avg)
     & ittdw(outstk_rank,1:itoutnumber)                ,  !g  internode-1
     & av_ittdw(1:itoutnumber)                            !g  internode-1 (avg)
              
          endif
          
          !--- PG factors output
          if(fldetpgfaout)then
              
              if(flemerged)then
                  
                  arue_dw     = rue
                  arue_dwa    = rue                  
                  
              if((li*par) .gt. 0.d0)then
                  arue_dw     = dw  / (li*par)  * (1.e6/1.e4)
                  arue_dwa    = dwa / (li*par)  * (1.e6/1.e4)
              endif
              
              if(IPAR_acc .gt. 0.d0)then
                  w_pg        = w_pg  + dw
                  wa_pg       = wa_pg + dwa
                  arue_w      = w_pg  / IPAR_acc  * (1.e6/1.e4)                  
                  arue_wa     = wa_pg / IPAR_acc  * (1.e6/1.e4)
              endif              
              
              if(pg .gt. 0.d0) RGP_fac = dw / pg !Avoid NaN
              
              endif
              
              write(detpgout_io,90) iyear,daynr,daycum,daycrop,diac,
     & par                        , !Daily par MJ m-2 d-1
     & extcoef                    , !k
     & lai                        , !LAI  
     & li                         , !Fraction Light Intercepted 
     & co2                        , !Atm CO2 concentration
     & rue                        , !Maximum RUE (gDW MJ-1) - Crop Parameter
     & co2_fac                    , !CO2           Factor
     & tstress                    , !Temperature   Factor
     & agefactor                  , !Age           Factor
     & swfacp                     , !Avl Water     Factor 
     & RGP_fac                    , !Sink Feedback Factor
     & pg                         , !Daily photosynthesis before RGP_fac reduction and resp = 0.d0 (t ha-1)
     & dRGP_pg                    , !Daily photosynthesis reduction due to sink feedback (t ha-1)
     & dw                         , !Daily total allocated biomass (t ha-1) 
     & RGP_pg                     , !Cumulative photosynthesis reduction due to sink feedback (t ha-1)
     & IPAR_acc                   , !Cumulative PAR intercepted (MJ m-2)
     & w*(1.e6/1.e4)              , !Crop total live dry biomass (g m-2)
     & wa*(1.e6/1.e4)             , !Crop total live aerial dry biomass (g m-2)
     & (w  + wdead) * (1.e6/1.e4) , !Crop total dry biomass produced (g m-2)
     & (wa + wdead) * (1.e6/1.e4) , !Crop total aerial dry biomass produced (g m-2)
     & arue_dw                    , !Daily Actual RUE (g MJ-1) Total  Dw Basis (deadbiomass not accounted)
     & arue_w                     , !Total Actual RUE (g MJ-1) Total   w Basis (deadbiomass not accounted)
     & arue_dwa                   , !Daily Actual RUE (g MJ-1) Total Dwa Basis (deadbiomass not accounted)
     & arue_wa                    , !Total Actual RUE (g MJ-1) Total  wa Basis (deadbiomass not accounted)
     & carbcheck                    !Carbon Balance Check (if <> 0.d0, check partitioning)
          endif

90    format(I4,4X,I3,3X,I4,4X,I3,1F8.1,1000F8.2)

      !--- update flags for sequencial run
      if (flcropend) then
          if(icrop-1 .eq. 4)then
              close(defout_io)
              if(fldetrtsyout) close(detrtout_io)
              if(fldetlfprout) close(detlfout_io)
              if(fldetitprout) close(detitout_io)
              if(fldetpgfaout) close(detpgout_io)
          endif
      endif
      
	    return
      
          end subroutine Samuca
	!************************************************************************
          
          subroutine stk_li(stk_age,lai,kdif,nstk)              
          !--- Subroutine implemented to partition the Daily Carbon among stalks
          !--- The main assumptions: 
          !--- (i)  RUE is constant for every leaf
          !--- (ii) Vertical Canopy LI is fractioned among tillers based on its thermal-age
          
          !--- Stalk height could alternatively be used on Item (ii), but would need to implement stalk height for every single stalk.
          !--- Moreover, stalk height is also driven by temperature. Another alternative would be to use the diameter of the spherical model
          !--- of LI used by Unsworth and Campbel and apply the J. Goudriaan Model, however, this could be quite lot detail and work for perhaps no expressive
          !--- or none improvements, besides, the uncertainty would be even higher when including other abiotic stresses
          
          !--- Murilo Vianna Jan-2018
              
          implicit none
          
          integer n
          integer i
          integer stk
          integer nsh_stk          
          
          real    stk_age(60,4)
          real    dx
          real    dlai
          real    lai*8
          real    laiacc
          real    fcanopy
          real    kdif*8
          real    dli
          real    liacc
          real    nstk
          real    fshare
          
          save
          
          dx      = 0.005     !Relative change in LAI
          n       = 1 / dx    
          dlai    = dx * lai
          
          laiacc          = 0.d0
          liacc           = 0.d0
          stk_age(1:60,4) = 0.d0 
          do i = 1, n
              
              laiacc  = laiacc + dlai
              dli     = kdif * exp(-kdif * laiacc) * dlai
              liacc   = liacc + dli
              
              nsh_stk = 0
              fcanopy = (lai - laiacc) / lai
              !Count the number of stalks that will share light at this canopy-layer (dx)
              do stk = 1, aint(nstk)
                  if(stk_age(stk,2) .ge. fcanopy) nsh_stk = nsh_stk + 1                  
              enddo
              
              if(nsh_stk .gt. 0) fshare  = dli / nsh_stk
              
              do stk = 1, aint(nstk)
                  if(stk .le. nsh_stk) stk_age(stk,4) = stk_age(stk,4) 
     & + fshare
              enddo              
          enddo
          
          !Normalize to intercepeted light 
          if(liacc .gt. 0.d0)then
              stk_age(1:60,4) = stk_age(1:60,4) / liacc
          endif
          
          return
          end subroutine          
          
          
          
          
          subroutine rootgrowth(task,plantdepth,initcropdepth,rtcumdens,     
     &    srlmax,srlmin,dwr,rdenshape,rootshape,rpup,effective_rd,rld,
     &    diac,di,rdepth,flemerged,rdm,rootdrate,drld,drdepth)
          !----------------------------------------------------------------------------------------
          ! Subroutine implemented for root expansion
          ! Three ways of root expasion is simulated:
          ! 1) Cumulative function
          ! 2) Input root factors (Similar to DSSAT)
          ! 3) Use of two SRL and root depth rate (Murilo Vianna phd dissertation, 2017)
          !       Main assumptions: 
          !       -There are two Specific Root Length (cm g-1):
          !       -One on the root front (SRLf) and other above rootfront (SRLa)
          !       -In the root front, roots are more explorable like than above it thus: SRLf > SRLa
          !       -Roots will start growing from the planting depth layer (up and downwards)
          !
          !       This approach was derived from Laclau & Laclau (2009) results of sugarcane root system assessment
          !
          ! Author: Murilo Vianna Jan-2018
          !-----------------------------------------------------------------------------------------
          
          use variables
          implicit none
          
          !Variables
          integer task
          integer i
          integer sl
          integer rtcumdens
          integer node
          integer rdenshape
          
          real    rdm
          real    diac
          real    di
          real    rdepth
          real    drdepth
          real    effective_rd
          real    initcropdepth
          real    plantdepth
          real    rpup
          real    rootprof(1000)
          real    botsoil
          real    rgf(numnod+1,2)
          real    srlmax
          real    srlmin
          real    srl          
          real    top_sl(numnod)
          real    bot_sl(numnod)
          real    thk_sl(numnod)
          real    drld(macp)
          real    rld(macp)
          real    dwr_prof(numnod)
          real    dwr
          real    rootdis(202)
          real*8  afgen          
          real    rootshape
          real    soma
          real*8  depth
          real*8  rld_prof(noddrz*2)
          real*8  rld_tocumdens(202)
          real    rootdrate
          real    rtfrontsize
          
          logical flemerged
          logical flrtfront
          logical flrtprof
          
          save
          
          goto (10,20) task
      
          !Initalization
10        continue
          
           ! --- maximum rooting depth & actual rooting depth
          rdi     = plantdepth
          rdm     = min(rds,rdc) !rdc is set on crop file and rds in master file
          rd      = min(rdi,rdm)
          rdpot   = min(rdi,rdm)          
                 
          ! --- CALCULATE NORMALIZED CUMULATIVE ROOT DENSITY FUNCTION
          !User can choose the root density shape type
          !rtcumdens = 1 use shape factor (Geotropism function) - use of rootshape parameter to model the curve
          !rtcumdens = 2 provide the table with relative root density as function of relative root depth     
          !rtcumdens = 3 Use of two SRL values based on Laclau & Laclau (2009) results
                 
      rtcumdens   = 3
      select case(rtcumdens)          
      
          case(1)          
          !Geotropism function
            do i = 1,22,2
            rdctb(i) = real(i)/(22.)                    
            end do
                  
            do i = 2,22,2
            rdctb(i) = (1-rdctb(i-1))**rootshape
            end do
            
            ! ---   specify array ROOTDIS with root density distribution
            do i = 0,100
          
                depth = 0.01d0 * dble(i)
                rootdis(i*2+1) = depth
                rootdis(i*2+2) = afgen(rdctb,22,depth)
                   
            enddo     
      
            ! ---   calculate cumulative root density function
            do i = 1,202,2
            ! ---     relative depths
                cumdens(i) = rootdis(i)
            enddo
      
            soma = 0.d0
            cumdens(2) = 0.d0
            do i = 4,202,2
            ! ---     cumulative root density
                soma = soma + (rootdis(i-2)+rootdis(i)) * 0.5d0
     &               * (cumdens(i-1)-cumdens(i-3))
                cumdens(i) = soma
            enddo
      
            ! ---   normalize cumulative root density function to one
            do i = 2,202,2
                cumdens(i) = cumdens(i) / soma
            enddo           
                      
          case(2)          
          !Input density values (made this readable from input file)
            rdctb(1) = 0.
            rdctb(2) = 1.
          
            rdctb(3) = 0.25
            rdctb(4) = 0.9
          
            rdctb(5) = 0.5
            rdctb(6) = 0.7
          
            rdctb(7) = 0.75
            rdctb(8) = 0.4
          
            rdctb(9) = 1.
            rdctb(10)= 0.01
            
            ! ---   specify array ROOTDIS with root density distribution
            do i = 0,100
          
                depth = 0.01d0 * dble(i)
                rootdis(i*2+1) = depth
                rootdis(i*2+2) = afgen(rdctb,22,depth)
                   
            enddo     
      
            ! ---   calculate cumulative root density function
            do i = 1,202,2
            ! ---     relative depths
                cumdens(i) = rootdis(i)
            enddo
      
            soma = 0.d0
            cumdens(2) = 0.d0
            do i = 4,202,2
            ! ---     cumulative root density
                soma = soma + (rootdis(i-2)+rootdis(i)) * 0.5d0
     &               * (cumdens(i-1)-cumdens(i-3))
                cumdens(i) = soma
            enddo
      
            ! ---   normalize cumulative root density function to one
            do i = 2,202,2
                cumdens(i) = cumdens(i) / soma
            enddo  
          
          case(3)
              
          !Use Laclau & Laclau Results with two SRL values          
          effective_rd = initcropdepth
          rpup         = 3.d0 !Chopped Stalk's diameter (cm)
          
          !Check root profile with 1 cm resolution 
          rootprof = 0.d0 
          botsoil  = aint(z(numnod) - 0.5*dz(numnod)) * -1 
          
          do sl = 1, botsoil     
              if(sl .gt. (initcropdepth - rpup) .and. sl .le. 
     &  effective_rd) rootprof(sl) = 1.d0 !there is root here             
          enddo          
          
          !Root deepening per soil layer (vertical fraction [CMroot / CMsoil])          
          rgf = 0.d0
          do sl = 1, numnod              
              top_sl(sl) = aint(z(sl) * - 1 - 0.5*dz(sl))
              bot_sl(sl) = aint(z(sl) * - 1 + 0.5*dz(sl)) 
              thk_sl(sl) = bot_sl(sl) - top_sl(sl)
              
              rgf(sl,2) = sum(rootprof(top_sl(sl)+1:bot_sl(sl))) / 
     & thk_sl(sl)
          enddo
          
          drld        = 0.d0
          dwr_prof    = 0.d0
          
          !Note: nod thickness is used to encapsulate srlMax (This might be very small)
          do sl = 1, numnod
              if((sl .eq. numnod .and. rgf(sl, 2) .gt. 0.d0) .or. 
     & (rgf(sl+1,2) .le. 0.d0 .and. rgf(sl,2) .gt. 0.d0))then
                  
                  srl             = srlmax !We are at root front! Increased specific root length
                  rgf(sl,1)       = (rgf(sl,2) * thk_sl(sl)) / (
     & (effective_rd - initcropdepth) + rpup)
                  dwr_prof(sl)    = rgf(sl,1) * (dwr * (1.e6/1.e4)) /
     & (thk_sl(sl) / 100.) ! g m-3
                  drld(sl)        = dwr_prof(sl) * srl    !m m-3
                  drld(sl)        = drld(sl) * 100./1.e6  !cm cm-3                  
                  
              elseif(rgf(sl,2) .gt. 0.d0)then     
                  srl             = srlmin !We are not at root front! Increased specific root length
                  rgf(sl,1)       = (rgf(sl,2) * thk_sl(sl)) / (
     & (effective_rd - initcropdepth) + rpup)
                  dwr_prof(sl)    = rgf(sl,1) * (dwr * (1.e6/1.e4)) /
     & (thk_sl(sl) / 100.) ! g m-3
                  drld(sl)        = dwr_prof(sl) * srl    !m m-3
                  drld(sl)        = drld(sl) * 100./1.e6  !cm cm-3
              else
                  !no roots within this layer
                  srl         = 0.d0
                  rgf(sl,1)   = 0.d0
                  dwr_prof(sl)= 0.d0
                  drld(sl)    = 0.d0
              endif
                    
                  do i = 1, noddrz
                      rld_prof((i-1)*2+1) =(z(i)-dz(i)*0.5d0)/-rd
                      rld_prof((i-1)*2+2) =rld(i)
                  enddo
                  
              
            do i = 1,22,2
            rdctb(i) = (real(i)/(22.))                  
            end do
                  
            do i = 2,22,2
            rdctb(i) = (1-rdctb(i-1))**rootshape
            end do               
                  
          ! ---   specify array ROOTDIS with root density distribution
            do i = 0,100
          
                depth = 0.01d0 * dble(i)
                rootdis(i*2+1) = depth
                rootdis(i*2+2) = afgen(rdctb,22,depth)
                   
            enddo  
               
              ! ---   specify array ROOTDIS with root distribution
            do i = 0,100
          
                depth = 0.01d0 * dble(i)
                rootdis(i*2+1) = depth
                rootdis(i*2+2) = afgen(rdctb,22,depth)
                   
            enddo         
              
                 
          enddo
          
      end select
                  
            return    
            
            
20          continue         
          !Daily Rate 
            
          !Calculating root depth as function of the cumulative degree-days
          !PLANTDEPTH = 25.0   ! PLANTING DEPTH
          !Laclau and Laclau (2009) rate of deepening of the root front (0.53 cm day-1 or 0.048 cm oC-1 day-1) over the first 4 months after planting, and an increase thereafter to 1.75 cm day-1 (0.22 cm oC-1 day-1) in the irrigated crop and 1.86 cm day-1 (0.24 cm oC-1 day-1)
            
          IF (diac .LT. 1000) THEN   ! A variation in RDEPTH should be computed dur the water stress - the higher WS, the deeper the root goes
            drdepth = (rootdrate * di) 
            !RDEPTH = 0.048 * DIAC !- Original results from Laclau & Laclau (2009)
          ELSE
            drdepth = (rootdrate * di)
            !RDEPTH = .22 * DIAC !- Original results from Laclau & Laclau (2009)
          END IF
          
          ! --- determine lowest compartment containing roots
          node = 1
          do while ((z(node) - dz(node)*0.5d0) .gt. (-rd + 1.d-8))
            node = node + 1
          end do
          noddrz = node
                 
          ! --- CALCULATE NORMALIZED CUMULATIVE ROOT DENSITY FUNCTION
          !User can choose the root density shape type
          !rtcumdens = 1 use shape factor (Geotropism function) - use of rootshape parameter to model the curve
          !rtcumdens = 2 provide the table with relative root density as function of relative root depth     
          !rtcumdens = 3 Use of two SRL values based on Laclau & Laclau (2009) results 
                 
      rtcumdens   = 3      
      select case(rtcumdens)          
      
          case(1)          
          !Geotropism function
            do i = 1,22,2
            rdctb(i) = real(i)/(22.)                    
            end do
                  
            do i = 2,22,2
            rdctb(i) = (1-rdctb(i-1))**rootshape
            end do
            
            ! ---   specify array ROOTDIS with root density distribution
            do i = 0,100
          
                depth = 0.01d0 * dble(i)
                rootdis(i*2+1) = depth
                rootdis(i*2+2) = afgen(rdctb,22,depth)
                   
            enddo     
      
            ! ---   calculate cumulative root density function
            do i = 1,202,2
            ! ---     relative depths
                cumdens(i) = rootdis(i)
            enddo
      
            soma = 0.d0
            cumdens(2) = 0.d0
            do i = 4,202,2
            ! ---     cumulative root density
                soma = soma + (rootdis(i-2)+rootdis(i)) * 0.5d0
     &               * (cumdens(i-1)-cumdens(i-3))
                cumdens(i) = soma
            enddo
      
            ! ---   normalize cumulative root density function to one
            do i = 2,202,2
                cumdens(i) = cumdens(i) / soma
            enddo           
                      
          case(2)          
          !Input density values (made this readable from input file)
            rdctb(1) = 0.
            rdctb(2) = 1.
          
            rdctb(3) = 0.25
            rdctb(4) = 0.9
          
            rdctb(5) = 0.5
            rdctb(6) = 0.7
          
            rdctb(7) = 0.75
            rdctb(8) = 0.4
          
            rdctb(9) = 1.
            rdctb(10)= 0.01
            
            ! ---   specify array ROOTDIS with root density distribution
            do i = 0,100
          
                depth = 0.01d0 * dble(i)
                rootdis(i*2+1) = depth
                rootdis(i*2+2) = afgen(rdctb,22,depth)
                   
            enddo     
      
            ! ---   calculate cumulative root density function
            do i = 1,202,2
            ! ---     relative depths
                cumdens(i) = rootdis(i)
            enddo
      
            soma = 0.d0
            cumdens(2) = 0.d0
            do i = 4,202,2
            ! ---     cumulative root density
                soma = soma + (rootdis(i-2)+rootdis(i)) * 0.5d0
     &               * (cumdens(i-1)-cumdens(i-3))
                cumdens(i) = soma
            enddo
      
            ! ---   normalize cumulative root density function to one
            do i = 2,202,2
                cumdens(i) = cumdens(i) / soma
            enddo  
          
          case(3)
          
          !Check root profile with 1 cm resolution 
          rootprof = 0.d0 
          botsoil  = aint(z(numnod) - 0.5*dz(numnod)) * -1 
          
          do sl = 1, botsoil     
              if(sl .gt. (initcropdepth - rpup) .and. sl .le. 
     &  effective_rd) rootprof(sl) = 1.d0 !there is root here             
          enddo          
          
          !Root deepening per soil layer (vertical fraction [CMroot / CMsoil])          
          rgf = 0.d0
          do sl = 1, numnod              
              top_sl(sl) = aint(z(sl) * - 1 - 0.5*dz(sl))
              bot_sl(sl) = aint(z(sl) * - 1 + 0.5*dz(sl)) 
              thk_sl(sl) = bot_sl(sl) - top_sl(sl)
              
              rgf(sl,2) = sum(rootprof(top_sl(sl)+1:bot_sl(sl))) / 
     & thk_sl(sl)
          enddo   
          
          drld        = 0.d0
          dwr_prof    = 0.d0
          
          !Note: nod thickness is used to encapsulate srlMax (This might be very small)
          !size of root front (Laclau & Laclau measuring range)          
          rtfrontsize = 30.d0 !(cm)
          
      do sl = 1, noddrz  
          
          !Check whether the soil layers are lower than rtfrontsize
          if(thk_sl(sl) .gt. rtfrontsize)then
              rtfrontsize = thk_sl(sl) !Update root front size to soil layer thickness
          endif
          
          !Flag for root front 
          flrtfront   = (bot_sl(noddrz) - top_sl(sl)) .lt. rtfrontsize
     & .and. top_sl(sl) .gt. rdi 
          
          !Flag for root profile
          flrtprof    = (.not. flrtfront) .and. rgf(sl,2) .gt. 0.d0
                    
          if(flrtfront)then
                  srl             = srlmax     !We are at root front! Use the increased specific root length
                  if(rd .eq. rdm) srl = srlmin !Root depth has reached to maximum
                  rgf(sl,1)       = (rgf(sl,2) * thk_sl(sl)) / (
     & (effective_rd - initcropdepth) + rpup)
                  dwr_prof(sl)    = rgf(sl,1) * (dwr * (1.e6/1.e4)) /
     & (thk_sl(sl) / 100.) ! g m-3
                  drld(sl)        = dwr_prof(sl) * srl    !m m-3
                  drld(sl)        = drld(sl) * 100./1.e6  !cm cm-3           
              
          elseif(flrtprof)then
                  srl             = srlmin !We are not at root front! Use the lower SRL 
                  rgf(sl,1)       = (rgf(sl,2) * thk_sl(sl)) / (
     & (effective_rd - initcropdepth) + rpup)
                  dwr_prof(sl)    = rgf(sl,1) * (dwr * (1.e6/1.e4)) /
     & (thk_sl(sl) / 100.) ! g m-3
                  drld(sl)        = dwr_prof(sl) * srl    !m m-3
                  drld(sl)        = drld(sl) * 100./1.e6  !cm cm-3          
          else              
              !no roots within this layer
                  srl         = 0.d0
                  rgf(sl,1)   = 0.d0
                  dwr_prof(sl)= 0.d0
                  drld(sl)    = 0.d0
          endif
                          
      enddo
          
          do i = 1, noddrz
            rld_prof((i-1)*2+1) =(z(i)-dz(i)*0.5d0)/-rd                      
            if(i .eq. noddrz) rld_prof((i-1)*2+1) = 1.                      
            rld_prof((i-1)*2+2) =rld(i)
          enddo   
          
          do i = 0,100          
                depth = 0.01d0 * dble(i)
                rld_tocumdens(i*2+1) = depth
                rld_tocumdens(i*2+2) = afgen(rld_prof,noddrz*2,depth)                
          enddo     
          
            ! ---   calculate cumulative root density function
            do i = 1,202,2
            ! ---     relative depths
                cumdens(i) = rld_tocumdens(i)
            enddo
              
            if(sum(rld(1:numnod)) .gt. 0.d0)then
                
            soma = 0.d0
            cumdens(2) = 0.d0
            do i = 4,202,2
            ! ---     cumulative root density
                soma = soma +(rld_tocumdens(i-2)+rld_tocumdens(i))*0.5d0
     &               * (cumdens(i-1)-cumdens(i-3))
                cumdens(i) = soma
            enddo
      
            ! ---   normalize cumulative root density function to one
            do i = 2,202,2
                cumdens(i) = cumdens(i) / soma
            enddo
            
            else
                
                ! ---   normalize cumulative root density function to one
            do i = 2,202,2
                cumdens(i) = 0.d0
            enddo
                
            endif
          
      end select  
          
          end subroutine 
              
              
		
	subroutine pgs_rue(par,co2,tmax,tmin,thour,swfacp,agefactor,lai,
     & extcoef,rue,diac,w,pg,li,tstress,co2_fac,tb,to_pg1,to_pg2,tbM)
      
	implicit none
	
	!*****************************************************************************
	!*     Subroutine PGS
	!*     Calculates the canopy gross photosysntesis rate (PG) based on Leaf Radiation Use Efficiency Method
	!*     Input: SWFACP,PAR,LAI,DIAC,TMN,W,EXTCOEF)
	!*     Output: PG,RESP
      !      Reviwed and adapted by Murilo Vianna: Jan-2018
	!******************************************************************************
	!-----------------------------------------------------------------------
          integer i
		REAL par
		REAL*8 lai
		REAL diac
		REAL li                      !Canopy Light interception
		REAL pg
		REAL e_fac
		REAL pratio
		REAL ct
		REAL swfacp
		REAL rowspc
		REAL w
		REAL agefactor
		REAL extcoef
		REAL chudec
		REAL chustk
		REAL t_mean
		REAL rue
		REAL tstress
          real tfac_h(24)
          real tmax
          real tmin
          real thour(24)
		REAL*8 co2
		REAL co2_fac
          REAL epp
          REAL trwu
          REAL rwuep2
		REAL rwuep1
		REAL rdm
		REAL qr
		REAL trwuafter
          REAL ptrats
          REAL epp_cm
          
          real tb
          real to_pg1
          real to_pg2
          real tbM
          
          save
          
	    !Fraction Canopy light interception
		li    = 1.d0 - EXP(-extcoef * lai)
          
          !Effect of CO2	
		co2_fac =  ((0.0001282051  * co2) + 0.95)
          
          tstress = 1.d0          
          do i = 1, 24
		!Computing the Temperature Stress on Photosynthesis
          !Optimum Thresholds (tb,to_pg1,to_pg2,tbM) - Murilo Vianna
              
              if(thour(i) .lt. tb)then
                  !No photosynthesis below tb
                  tfac_h(i) = 0.d0                  
              elseif(thour(i) .ge. tb .and. thour(i) .lt. to_pg1)then
                  !Below optimum temperature range
                  tfac_h(i) = (thour(i) - tb) / (to_pg1 - tb) 
              elseif(thour(i) .ge. to_pg1 .and.thour(i) .lt. to_pg2)then
                  !Optimun temperature range    
                  tfac_h(i) = 1.d0
              elseif(thour(i) .ge. to_pg2 .and. thour(i) .lt. tbM)then
                  !Above optimun temperature range    
                  tfac_h(i) = 1.d0-(thour(i) - to_pg2) / (tbM - to_pg2)
              elseif(thour(i) .gt. tbM)then
                  !No photosynthesis above tbM
                  tfac_h(i) = 0.d0    
              endif
                  
          enddo
          
          !Averaged DAily Tempererature stress    
          tstress = sum(tfac_h(1:24)) / 24.d0
		
          !Gross photosynthesis
          !Factors imposed to reduce gross photosynthesis
          !Atm CO2, crop age, temperature and soil water 
		pg = par * li * rue * co2_fac * agefactor * tstress * swfacp  
				
		pg = pg  * (1.e4/1.e6) !(t ha-1)
	
		return
		end subroutine pgs_rue
	
	
	
	SUBROUTINE PARTS(diac,chustk,chuem,lgpf,sgpf,rgpf,stalkgpf)
	
      Use Variables
      
      IMPLICIT NONE 
	!*****************************************************************************
	!*     Subroutine PARTS
	!*     Calculates the Partitioning rules for stalk, root and leaf
	!******************************************************************************
      !*     Original From Fabio Marin Thesis (2014) 
	!-----------------------------------------------------------------------  
		
          
		REAL diac
		REAL chustk
		REAL chuem
		REAL lgpf
		REAL sgpf
		REAL rgpf
		REAL contador
		REAL minrgpf
		REAL maxlgpf
		REAL stalkgpf
		
          save
          
		IF (diac .LT. chustk) THEN  !.AND. DIAC > CHUEM
				rgpf = .3			
				lgpf = .1
				!stalkgpf = .9
		
		ELSEIF (diac .GE. chustk) THEN
				stalkgpf = sgpf
				lgpf = (1-(sgpf)) 
				rgpf = .2
				
	
				
		END IF
		RETURN
		
		END SUBROUTINE PARTS
	
	
	
	subroutine LAIS(diac,di,phyloc,nstk,swface,stk_agefac,ln,maxgl,cumla,
     & ddealla,mla,sla,cumlw,ddeallw,dnleaf,dla,dleafdm,dw,lgpf,
     & dnetbiomass,dnoden,ddeadln,devgl,stk_age,lfage,lfarea,lfweight,
     & dnstk,stk_dnetbiomass,init_nlf,lfshp,maxdevgl)
      
      !--- Reviewed in jan-2018 (Murilo Vianna)      
	implicit none	
		
		REAL ln
		REAL dla
		REAL dnleaf
		REAL sla
		REAL la
		REAL agefactor
		REAL cumla(150) !cumulative leaf area       (m2 m-2)
          real cumlw(150) !cumulative leaf dry weight (t ha-1)
		REAL dleafdm
		REAL dw
		REAL lgpf
		REAL maxgl
		REAL phyloc
		REAL swface
		REAL di
		REAL nstk
		REAL ddeadlm
		REAL ddealla
          real ddeallw
		REAL deadln
		REAL lntotal
		REAL diac
		REAL mla
          real dleafdm_ss
          real dnetbiomass
          real dintleaf
          real dnoden
          real ddeadln
          real stk_agefac
          real init_la
          real lf_lifespam
          real ddealla_stk
          real ddeallw_stk
          real deadln_stk
          real dnoden_stk
          real dintleaf_stk
          real dnleaf_stk
          real dla_ss
          real dla_ss_stk
          real lfgrp
          real lfgrp_fac
          real mla_rate
          real dla_lf
          real devgl
          real ssfac
          real dnstk
          real AScurv
          real lfshp          
          
          !Leaf profile
          real lfage(60,20)
          real lfarea(60,20)
          real lfweight(60,20)
          real stk_age(60, 4)
          real stk_dleafdm(60)
          real stk_dleafdm_ss(60)
          real stk_ssfac(60)
          real stk_dnetbiomass(60)
          
		INTEGER daynr
		INTEGER i
          integer lf
          integer Glf         !Growing Lf
          integer nNewLf      !Number of new Lfs
          integer maxdevgl
          integer tl
          integer init_nlf
          integer lfdevmethod !1 = leaf profile method, 2 = leaf number and growth are assumed to be equal per stalk
          integer gRatefunc
          
          logical new_lf
          logical shed_lf
          logical new_tl
          
          save
                             			
      !--- Calculating the daily leaf number increment per stalk
		dnleaf = di / phyloc 
          
      !--- Integer leaf number increment
          dintleaf = dintleaf + dnleaf
          
      !--- Compute integer leaf and internode development
          if(dintleaf .gt. 1.d0)then          
              dintleaf = dintleaf - 1.d0
              dnoden   = 1.d0              
          endif       
          
      !--- Method selected
          lfdevmethod = 1
          gRatefunc   = 2
          
          select case(lfdevmethod)
              
              case(1)
      !--------------------------------------------
      !--------------------------------------------

          !init_nlf    = 2                         !Warning: Crop parameter
          !lfshp       = 7.d0                      !Lf growing shape
          init_la     = ((15.d0 * 2.d0 * 0.75))   !Inital Leaf area (cm2) considering a leaf of 15 x 2cm dimensions (0.75 is the leaf blade shape parameter)
          lf_lifespam = maxgl * phyloc            !Leaf life spam (C°days)
          !maxdevgl    = 5                         !Maximum Developed Green Leaves Warning: Crop parameter
          lfgrp_fac   = 1.d0                      !Lf Growing point factor        Warning: Crop parameter
          lfgrp       = (maxgl-maxdevgl)*lfgrp_fac!Lf Growing point (Lf rank where leaf is fully developed)
          mla_rate    = mla / (lfgrp * phyloc)    !Lf Max Area Rate (cm2 dd-1) (Note that the SLA is assumed to be constant)
          devgl       = 0.d0                      !Developed Green Lf per stalk
          dla_ss      = 0.d0
          dnetbiomass = 0.d0
          stk_dnetbiomass = 0.d0
          stk_ssfac   = 1.d0
          stk_dleafdm = 0.d0
          
          !--- How much Carbon is been partioned before sinking demand
          dleafdm     = dw * lgpf                 ! t ha-1
          stk_dleafdm = dleafdm * stk_age(1:60, 4)! Partition for each stalk as function of light intercepetion
          dleafdm     = 0.d0                      ! Recalculated on carbon balance
      
          do tl = 1, aint(nstk)
              
              new_lf      = .false. !rate init
              shed_lf     = .false. !rate init
              new_tl      = .false. !rate init
              Glf         = 1       !All leaves are growing
              nNewLf      = 0       !New Lfs number
              ddealla_stk = 0.d0    !Stalk Rate init
              deadln_stk  = 0.d0    !Stalk Rate init
              ddeallw_stk = 0.d0    !Stalk Rate init
              dnoden_stk  = 0.d0
              dnleaf_stk  = dnleaf
              
              !------------------------!
              !--- Leaf Development ---!
              !------------------------!
              
              !Check whether its a young tiller
              if(aint(stk_age(tl,3)) .lt. init_nlf)then
                  
                  new_tl                  = .true.            !New leaf appeared
                  dnleaf_stk              = init_nlf          !New tiller must have the inital leaf conditions
                  
                  do lf = 1, init_nlf
                      lfage(tl,lf)    = (lf-1) * phyloc          !Init Lf age
                      lfarea(tl,lf)   = 0.d0 !No lf area until check whether it is available carbon
                      lfweight(tl,lf) = 0.d0 !No lf weight either
                  enddo
                  
              else              
              
              !Check whether a leaf exceeded its life spam
              if(lfage(tl,aint(stk_age(tl,3))) .gt. lf_lifespam)then
                  
                  shed_lf     = .true.                            !Shed a leaf due to life spam
                  deadln_stk  = 1.d0                              !Shed one leaf (oldest)
                  ddealla_stk = lfarea(tl,    aint(stk_age(tl,3)))!Dead leaf area (cm2) (oldest leaf)
                  ddeallw_stk = lfweight(tl,  aint(stk_age(tl,3)))!Dead biomass (g) (oldest leaf)                  
                  
                  !Set oldest leaf state to zero
                  lfage(tl,aint(stk_age(tl,3)))   = 0.d0
                  lfarea(tl,aint(stk_age(tl,3)))  = 0.d0
                  lfweight(tl,aint(stk_age(tl,3)))= 0.d0
                  
                  !Integrate to daily rates
                  deadln      = deadln    + deadln_stk
                  ddealla     = ddealla   + ddealla_stk
                  ddeallw     = ddeallw   + ddeallw_stk
                  
              endif    
              
              !Check whether a new leaf will appear
              if(aint(stk_age(tl,3) + dnleaf) .gt. aint(stk_age(tl,3)))
     & then            
                  new_lf      = .true. !New leaf appeared
                  dnoden_stk  = 1.d0   !Considering a new leaf is always attached on underlying new internode
                  
                  !Update leaf profile
                  do lf = aint(stk_age(tl,3)), 1,-1
                      
                          lfage(tl,lf+1)      = lfage(tl,lf)      !Age        (°Cd)
                          lfarea(tl,lf+1)     = lfarea(tl,lf)     !Area       (cm2)
                          lfweight(tl,lf+1)   = lfweight(tl,lf)   !Dry weight (g)
                  enddo
                  
                      !New leaf initiation
                      lfage(tl,1)      = 0.d0
                      lfarea(tl,1)     = 0.d0 !No lf area until check whether it is available carbon
                      lfweight(tl,1)   = 0.d0 !No lf weight either
              endif
              
              endif
          
              !--------------------------------!
              !------ Leaf Sink Strenght ------!
              !--------------------------------!          
              
              !--- Sink Strenght (Constant SLA)              
              dla_ss_stk  = 0.d0
              
              !Sink Strength of appeared leaves on new tillers
              if(new_tl)then     
                  nNewLf  = init_nlf
                  !For each initial Lf
                  do lf = 1, init_nlf
                    dla_ss_stk    = dla_ss_stk + init_la
                  enddo         
              endif
                  
              !Sink Strength of appeared leaves
              if(new_lf)then
                  dla_ss_stk  = dla_ss_stk + init_la
                  nNewLf      = aint(stk_age(tl,3) + dnleaf) - 
     & aint(stk_age(tl,3))
              endif
              
              !Number of Lfs initated
              Glf = nNewLf + 1
              
              !Growth only Lfs older than Glf and skip sheded
              do lf = Glf, (aint(stk_age(tl, 3) + dnleaf) - deadln_stk)
      
                  if(gRatefunc .eq. 1)then                      
                      !Use constant growing rate
                      !Check whether the leaf is still growing
                      if(lfage(tl,lf) .lt. (lfgrp * phyloc))then
                          !Growing Rate (cm2)
                          dla_ss_stk = dla_ss_stk + 
     & (di * mla_rate * swface)  
                      endif                          
                          
                  else                          
                      !Use variable growing rate with respect to Lf age
                      mla_rate = AScurv(2,lfage(tl,lf),0.,mla,lfshp,
     & (lfgrp*phyloc/2.),1.)
                      !Growing Rate (cm2)
                      dla_ss_stk = dla_ss_stk + 
     & (di * mla_rate * swface)
                  endif
     
                if(lfage(tl,lf) .ge. ((maxgl-maxdevgl) * phyloc))then 
                    !Developed Green Lf per stalk counter
                    devgl = devgl + 1.d0 / aint(nstk)             
                endif
              enddo              
              
              !--- Integrate sink strenght for carbon balance
              dla_ss              = dla_ss + dla_ss_stk               ! Total LA SS [cm2 m-2]
              stk_dleafdm_ss(tl)  = dla_ss_stk / sla * (1.e4/1.e6)    ! t ha-1 stk-1
              
              !--- Carbon balance 
		    if(stk_dleafdm(tl) .ge. stk_dleafdm_ss(tl))then
                  !Enough carbon supplied by photosynthesis
                  stk_ssfac(tl)       = 1.d0
                  stk_dnetbiomass(tl) = stk_dleafdm(tl) - 
     & stk_dleafdm_ss(tl)
                  dnetbiomass         = dnetbiomass+stk_dnetbiomass(tl)   !Remaining biomass will be allocated to roots or stalks later on
                  stk_dleafdm(tl) = stk_dleafdm_ss(tl)                    !Reduce incoming Carbon to potential demand         
              else
                  if(stk_dleafdm_ss(tl) .gt. 0.d0)then !Avoid NaN    
                      stk_ssfac(tl)= stk_dleafdm(tl)/stk_dleafdm_ss(tl)  !Sink Strenght factor
                  endif
              endif
              
              !Update Lf growth rate and number (t ha-1)
              dleafdm         = dleafdm + stk_dleafdm(tl)
              
              !-------------------------!
              !------ Leaf Growth ------!
              !-------------------------!
              
              !Reallocated Carbon              
              !Growth of appeared leaves on new tillers
              if(new_tl)then
                  !For each initial Lf
                  do lf = 1, init_nlf
                    dla_lf            = init_la * stk_ssfac(tl)
                    lfarea(tl,lf)     = lfarea(tl,lf)     + dla_lf        !Lf Area increment (cm2)
                    lfweight(tl,lf)   = lfweight(tl,lf)   + dla_lf/sla    !Lf dry weight increment (g)
                    dla               = dla + dla_lf !(cm2)
                  enddo                  
              endif
                  
              !Growth of appeared leaves
              if(new_lf)then
                  dla_lf          = init_la * stk_ssfac(tl)
                  lfarea(tl,1)    = lfarea(tl,1)     + dla_lf        !Lf Area increment (cm2)
                  lfweight(tl,1)  = lfweight(tl,1)   + dla_lf/sla    !Lf dry weight increment (g)
                  dla             = dla + dla_lf !(cm2)
              endif
              
              !Growth only Lfs older than Glf and skip sheded
              do lf = Glf, (aint(stk_age(tl, 3) + dnleaf) - deadln_stk)
                  
                  !Allocate the supplied carbon to leaf sinks
                  dla_lf  = 0.d0
                  if(gRatefunc .eq. 1)then  
                      if(lfage(tl,lf) .lt. (lfgrp * phyloc))then
                          !Actual leaf daily area increment (cm2)
                          dla_lf = (di*mla_rate*swface) * stk_ssfac(tl) 
                      endif
                  else
                      !Use variable growing rate with respect to Lf age
                      mla_rate = AScurv(2,lfage(tl,lf),0.,mla,lfshp,
     & (lfgrp*phyloc/2.),1.)
                      !Actual leaf daily area increment (cm2)
                      dla_lf = (di*mla_rate*swface) * stk_ssfac(tl) 
                  endif                      
                  
                    lfage(tl,lf)    = lfage(tl,lf)      + di          !Lf Age increment (dd)
                    lfarea(tl,lf)   = lfarea(tl,lf)     + dla_lf      !Lf Area increment (cm2)
                    lfweight(tl,lf) = lfweight(tl,lf)   + dla_lf / sla!Lf dry weight increment (g)
                      
                    !Integrate Daily Total Leaf Area Increment
                    dla = dla + dla_lf !(cm2)
                      
              enddo
              
                  stk_age(tl,3) = stk_age(tl,3)+dnleaf_stk-deadln_stk
              
              enddo
              
          !Check whether a tiller senesced              
              if(aint(nstk) .gt. aint(nstk+dnstk))then
                  ddealla = ddealla + sum(lfarea(aint(nstk),1:
     & aint(stk_age(aint(nstk), 3))))                 
              endif
                        
          !Convert units
          dla     = dla       / 1.e4          !(m2 m-2) *Recall that the Leaf is integrated over number of stalk per m2 thus m2Leaf / m2Soil
          ddealla = ddealla   / 1.e4          !(m2 m-2)
          ddeallw = ddeallw   * (1.e4/1.e6)   !(t ha-1)
              
      !--------------------------------------------
      !--------------------------------------------
          case(2)
	
      !--- Limiting the maximum number of leaves to MAXGL
		if(ln .gt. maxgl)then			
			ddeadln = 1.d0 !Shed a leaf  	  
			deadln  = deadln + 1.     
			ddealla = cumla(int(deadln))
              ddeallw = cumlw(int(deadln))
		else			
			ddealla = 0.d0
              ddeallw = 0.d0
              ddeadln = 0.d0
          endif         
		
      !--- Total leaf number production
		lntotal = ln + deadln  	
	
	!--- Calculating the leaf area increment
	! We are not assuming that the PHYLOC would be affected by the Water Stress. We are just including a 
	! restriction for the expasion of leaves out of the expansion algorithm. In other words, 
	! in spite of our believe still being that Water Stress affects area expansion rather than leaf production
	! our algorthim does not allow to include the water stress easily. 
	     
          dla     = dnleaf * (mla / 1.e4) * nstk * swface * stk_agefac!m2[Leaf] m-2[Soil]
	
	!--- Checking the Needed mass to reach the potential leaf area increment (sink strenght)
          dleafdm_ss = dla  *  (1./(sla/1.e4))  !g[DWLeaf] m-2
          dleafdm_ss = dleafdm_ss * 1.e4/1.e6   !t[DWLeaf] ha-1              
      
      !--- How much is been partioned before sink strength demand
          dleafdm    = dw * lgpf
          
      !--- Carbon balance check
          dnetbiomass = 0.d0
		if(dleafdm .ge. dleafdm_ss)then
              !Enough carbon supplied by photosynthesis
              dnetbiomass = dleafdm - dleafdm_ss !Remaining biomass will be allocated to other crop pool
              dleafdm     = dleafdm_ss           !Update allocated biomass to leaves pool           
          else
              dnetbiomass = dleafdm - dleafdm_ss               !Negative sign indicate lack of carbon  
              dla         = dleafdm * (1.e6/1.e4) * (sla/1.e4) !Update leaf area increment assuming constant SLA (m2[Leaf] m-2[Soil])
          endif
	
      !--- Update Array of leaf areas
      !--- Note that this approach one leaf+1 is explicit grown, not all leaves as it really happens (1:maxgl) (MV)
		cumla(int(lntotal+1)) = cumla(int(lntotal+1)) + dla
          cumlw(int(lntotal+1)) = cumlw(int(lntotal+1)) + dleafdm
          
          end select
	
	return
	end subroutine LAIS	
          
      subroutine TempHour(tmaxday,tminday,thour)
        !Calculates the Hourly temperature based on Parton & Logan (1981)
        !Murilo Vianna
    
          Use Variables
	    Implicit None	
	
            integer hour
            integer doy
            real tsunset                               !Temperature related variables !oC        
            real decsol                                ! astronomic variables
            real ahn                                   ! astronomic variables
            real timnight                              ! time related variables
            real timday                                ! time related variables
            real sunset
            real sunrise
            real photop
            real nigthp
            real bb
            real be
            real bbd_iday
            real bbd_inight
            real bbd_inight2
            real bbe
            real ddy
            real tmaxday
            real tminday
            real thour(24)
            
            real d_2_r
            real r_2_d
            real :: pi  = 3.14159265
        
            real :: a   = 1.607 !Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = 2.000)
            real :: b   = 2.762 !Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = 2.200)
            real :: c   = 1.179 !Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = -0.17)
		  
            save
            
            !Linking with SWAP variables            
            doy       = daynr
            d_2_r     = pi/180.
            r_2_d     = 180./pi
            
          		
		    !calculating photoperiod			
              decsol  = 23.45 * sin(((360./365.)*(doy-80.)*d_2_r))
		    photop  = acos((-tan((lat)*d_2_r))*(tan((decsol)*d_2_r))) * 
     & r_2_d * (2./15.)
              
		    nigthp  = 24. - photop
		    sunrise = 12. - photop/2.
		    sunset  = 12. + photop/2.
		
            bb      = 12. - photop / 2. + c
            be      = 12. + photop / 2.
            ddy     = photop - c
        
            !Calculating air temperature follow Parton & Logan (1981)				
            tsunset = (tmaxday-tminday)*sin(((pi*ddy)/(photop+2*a))) + 
     & tminday
        
		    !Initial Conditions
		    do hour = 1,24
			    
                    bbd_iday    = hour - bb
                    bbe         = hour - be
                    bbd_inight2 = (24. + be) + hour
                
                    !Rescaling time
                    if(hour .gt. sunset) then
                        bbd_inight  = hour - sunset
                    else
                        bbd_inight = (24. + hour) - sunset
                    endif
                
                    !Day time temperature
                    if(hour .ge. bb .and. hour .le. sunset) then
                
                        thour(hour) = (tmaxday - tminday) * 
     & sin(((pi * bbd_iday) / (photop + 2*a))) + tminday
                    
                    else
                        !Night time temperature                    
                        thour(hour) = tminday + (tsunset - tminday) * 
     & exp(-b * bbd_inight/(24. - photop))                    
                    
                    endif						
		    enddo
		
	    return
	end subroutine TempHour     
          
                    
          
	subroutine DIAMPERS(thour,Tbi,Topt,dpercoeff,swface,agefactor,nstk, !Input
     & pleng,noden,sucmax,SucAccFac,dws,di,diac,phyloc,dw,sgpf,    !Input
     & age_stkemer,dper,dsuc,dfib,sucexceeded,internode,stk_dnetbiomass,
     & dnetbiomass,dstkdm,stk_itn,stk_age,itage,itpf,itlen,itsuc,itfib,
     & ittdw,ngrnodes,itshp,sucshp,maxitdw,flsink_fbres,dRGP_pg)                        !Crop Parameters
      
      !*     Subroutine DIAMPERS
	!*     Calculates the Hourly dPER and integrates in a daily value PERDAY, 
	!*     Calculates the Mean Diameter of Stalks
	!*     Calculates the Sucrose Mass
      !*     Reviewed and modified in Jan-2018 by Murilo Vianna
	
      implicit none          
      
          integer SucAccFac    !Sucrose Accumulation Factor - Internode number where the sucrose fraction is equal to sucmax/2 
		integer h
          integer noden
		integer daynr
          integer grnodes !Growing internodes
          integer ngrnodes
          integer itpfmethod   !Biomass Partitioning method (1 = for each stk, 2 = considering a single stalk)
          integer tl
          integer it              !Above ground internodes
          integer ibg             !Below ground internodes
          integer indxit
          integer nit_belowground !Number of internodes below ground
          integer nint_stk        !truncated stalks number of internodes
          
		REAL thour(24)	 
		REAL internode(100,6)            !Declaring the internode array to store: 1)internode number,2)diameter,3)lenght,4)sucrose
		REAL stalk_suc(200)
		REAL perday
		REAL dper
		REAL agefactor
		REAL chustk
		REAL diac
		REAL diam_stk
		REAL diam
		REAL pleng
		REAL ileng
		REAL nstk
		REAL swface
		REAL dpercoeff
		REAL dnleaf
		REAL ln
		REAL wsuc
		REAL nodencurrent
		REAL lntotal
		REAL sucpfacum
		REAL fiberpf
		REAL sucmax
		REAL i
		REAL ws
          real Tbf
          real Tbi
          real sucexceeded
          real dsucexceeded
          real dfib
          real dsuc
          real suc_before
          real fib_before
          real suc_after
          real fib_after
          real dper_h
          real dws
          real tfac_per
          real Topt
          real avg_tfac_per
          real phyloc
          real dphyn
          real di
          real sumpfac
          real itshp
          real AScurv
          real dw
          real sgpf
          real dstkdm
          real dintn
          real age_stkemer        !Age in which a Tiller emerges it Stalks
          real dnetbiomass
          real dalloc_stk
          real maxitdw
          real dss_stk
          real ddw_stk
          real ditss_rate
          real nbiostk
          real nbiostk_alloc
          real reduced_per
          real sucshp
          real ssfac
          real dRGP_pg
          real deb
          
          !Stalk Array
          real stk_age(60, 4)     !Stalk age (1); fraction of age in relation to the oldest stalk (2) Number of Green Leaves (3); Canopy Fraction (4)
          real stk_dnetbiomass(60)!Biomass Reallocated from Leaves to Stalks
          real stk_dstkdm(60)     !Stalk Daily Biomass Increment
          real stk_itn(60)        !Internode Number
          real stk_bg_ss          !Below ground internodes sink strength
          
          !Inernode Arrays
          real itage(60,60)       !Internode Age
          real itpf(60,60)        !Internode Partitioning Factor
          real itlen(60,60)       !Internode Length (mm)
          real itsuc(60,60)       !Internode Sucrose Mass
          real itfib(60,60)       !Internode Fiber Mass
          real ittdw(60,60)       !Internode Total Biomass
          real itbg(30,4)         !Below Ground Internodes Age (TT) and Sink Strength (g)
          
          
          logical flsucexceeded
          logical flsink_fbres
			
          save
          	
          !--- Initialization
              dstkdm      = 0.d0
              stk_dstkdm  = 0.d0
              itpf        = 0.d0
              dsuc        = 0.d0
              dfib        = 0.d0
              suc_before  = 0.d0
              fib_before  = 0.d0
              suc_after   = 0.d0
              fib_after   = 0.d0
              dalloc_stk  = 0.d0
              
              
          !--- Internode number increment (Assuming plastochron = phyllochron)
              dintn       = di / phyloc          
          
          !--- Hourly Plant Extension (MV)
              
              dper    = 0.d0 !Daily  (mm d-1)
              dper_h  = 0.d0 !Hourly (mm h-1)
              
              avg_tfac_per = 0.d0 !Average Temperature Reducing Factor
              do h = 1, 24                  
                  tfac_per= min(1.d0,max(0.d0,thour(h)-Tbi)/(Topt-Tbi))
                  dper_h  = dpercoeff * tfac_per * swface * agefactor ! AgeFactor is an age reduction factor for dPER, based on N. G. Inman-Bamber et al. 2008 Australian Journal of Agricultural Research, Fig.3
                  dper    = dper + dper_h                  
                  avg_tfac_per = avg_tfac_per + tfac_per
              enddo
              avg_tfac_per = avg_tfac_per / 24.
              
        !--- Stalk diameter 
        !--- Original from SAMUCA 1st version (Marin)
		if(nstk .LT. 9)then
			diam_stk = -.077 * nstk + 3.0443
		else
			diam_stk = -.0256 * nstk**2 + .4206 *  nstk + .7763
          endif          
		diam = .17409 + .1803853 * log(pleng*100)+.542597*diam_stk		
		diam = (max(0.0, diam))
          
          itpfmethod  = 1
          
          select case(itpfmethod)
              
              case(1)
          
          !--- Carbon partitioned to stalks
          dstkdm      = dw * sgpf                 ! t ha-1
          stk_dstkdm  = dstkdm * stk_age(1:60, 4) ! Partition for each stalk as function of light intercepetion (collum 4 of stk_age is fraction of light intercepted by each stalk)
          stk_dstkdm  = stk_dstkdm+stk_dnetbiomass! Add Up the surplus biomass from Leaves
          dstkdm      = 0.d0                      ! Will be updated after stalk biomass allocation
          
          stk_dnetbiomass = 0.d0                  !Transfered to stalks
          dnetbiomass     = 0.d0                  !Transfered to stalks
          
          !Partitioning Factor among internodes
          sumpfac = 0.d0
          do it = 1, ngrnodes
              !Partitioning factor computed by derivative of an asymptote curve as function of internode thermal age                 
              sumpfac = sumpfac + AScurv(2,it*phyloc,0.,1.,itshp,
     & (ngrnodes/2.*phyloc),1.)                              
          enddo
          
          !--- Carbon Allocation For each Stalk m-2
          do tl = 1, aint(nstk)              
              
              !--- Initialize daily rates
              dss_stk     = 0.d0
              nbiostk     = 0.d0
              
              !--- Check whether the stalk has emerged (based on CHUMAT parameter)
              if(stk_age(tl, 1) .ge. age_stkemer)then
                  !Stalk Emerged!                  
                  !Uptade Internode Number
                  stk_itn(tl) = stk_itn(tl) + dintn
                  
                  !--- Sink Strenght for growth (gDW stk-1)
                  do it = 1, (aint(stk_itn(tl)) + 1)
                      
                      !Update internodes Thermal Age
                      itage(tl,it) = itage(tl,it) + di    !Internode Age  
                      
                      !Potential SS
                      ditss_rate = AScurv(2,itage(tl,it),0.,maxitdw,
     & itshp,(ngrnodes/2.*phyloc),1.)                      
                      dss_stk = dss_stk + ditss_rate * di !(g)                      
                  enddo
                  
                  !--- Carbon Balance
                  ddw_stk = stk_dstkdm(tl) * (1.e6/1.e4)  !Total incoming biomass (g)                  
                  if(ddw_stk .ge. dss_stk)then
                      ssfac   = 1.d0
                      nbiostk = ddw_stk - dss_stk ! surplus biomass (will be reallocated equally among it)
                      ddw_stk = dss_stk           ! reduce incoming dw to ss
                  else
                      if(dss_stk .gt. 0.d0) then
                          ssfac   = ddw_stk / dss_stk ! not enough dw to fully growth
                      else
                          ssfac   = 1.d0 !No sink strenght
                      endif                      
                  endif                  
                  
                  !--- Allocate Carbon among internodes
                  dalloc_stk  = 0.d0
                  
                      do it = 1, (aint(stk_itn(tl)) + 1)               
                          !--- Sink Strength
                          ditss_rate = AScurv(2,itage(tl,it),0.,maxitdw,
     & itshp,(ngrnodes/2.*phyloc),1.)                          
                          
                          !--- Allocate to internode 
                          ittdw(tl,it) = ittdw(tl,it) + ditss_rate * 
     & di * ssfac
                          !--- Internode expansion 
                          !Assume expansion is proportional to biomass increment. 
                          !Therefore, fraction of dper is partitioned as function of relative ss 
                          
                          !--- Create the partitioning factors table of dper throughout growing internodes (ngrnodes)
                          itbg = 0.d0
                          do ibg = 1,ngrnodes
                              
                              !--- Age of growing internode
                              itbg(ibg,1) = (ngrnodes-ibg) * phyloc + 
     & itage(tl,(aint(stk_itn(tl)) + 1))
                              
                              !--- Maximun theoretical biomass of growing internode
                              itbg(ibg,2) = AScurv(1,itbg(ibg,1),0.,
     & maxitdw,
     & itshp,(ngrnodes/2.*phyloc),1.)
                              
                              !--- Fraction of theoretical biomass remaining to reach maxitdw
                              itbg(ibg,3) = 1.d0 - itbg(ibg,2)/maxitdw
                          enddo
                          
                          !--- Normalize Fractions of theoretical biomass remaining to reach maxitdw
                          do ibg = 1,ngrnodes
                              itbg(ibg,4) = itbg(ibg,3) / 
     & sum(itbg(1:ngrnodes,3))                              
                          enddo
                          
                          !--- Arragne with corresponding growing internode (it)
                          nint_stk = (aint(stk_itn(tl))+1) 
                          if(ngrnodes .ge. nint_stk)then                          
                              indxit=it+(ngrnodes-nint_stk)
                          else
                              indxit=it-nint_stk + ngrnodes
                          endif
                          
                      !--- Extend internodes within the growing interval 1<it<ngrnodes
                      if(indxit .ge. 1 .and. indxit .le. ngrnodes) then
                          
                          !--- Extending internodes                  
                          itlen(tl,it) = itlen(tl,it)  + 
     & itbg(indxit,4) * dper
                          endif  
                                                    
                          !--- Total dw allocated due to sink strength
                          dalloc_stk  = dalloc_stk + ditss_rate * 
     & di * ssfac
                          !Compute Actual Sucrose and Fiber Content
                          !This is needed because sucrose and fiber pools are dynamic and can change over time due to enviromental conditions
                          !The new version of samuca run entirely on phytomer and sink strenght based can simulate the amounts of fiber and sucrose partiotioned biomass
                          !For now use it dynamically (Note: Fiber biomass can decrease with this approach, which is not physilogically true)
                          !Sucrose is simulated as an active sink while the fiber might be the sink and sucrose the surplus (storage energy)
                          suc_before  = suc_before + itsuc(tl,it)
                          fib_before  = fib_before + itfib(tl,it)
                          
                      enddo
                      
                  !--- Sink Feedback Response?
                  if(flsink_fbres)then
                      
                      !--- Total allocated dw due to SS
                      !--- This is a hypothesis raised and tested by McCromick that Sink has a feedback response to reduce PG
                      !--- whereas the surplus biomass must be reduced from PG 
                      dalloc_stk  = dalloc_stk *(1.e4/1.e6)        ![t ha-1]
                      nbiostk     = nbiostk    *(1.e4/1.e6)        ![t ha-1]                      
                      dstkdm      = dstkdm  + dalloc_stk           !Total biomass allocated to stalks [t ha-1
                      dRGP_pg     = dRGP_pg + nbiostk              !Total Surplus biomass
                      stk_dstkdm(tl)  = 0.d0
                      
                  else                      
                      !--- Allocate surplus DW equally to every internode (This didnt work)
                      !--- Allocate surplus only to growing internodes
                      nbiostk_alloc = min(1.d0, (aint(stk_itn(tl)) + 1)/
     & ngrnodes) * nbiostk
                      nbiostk = (1.d0 - min(1.d0, (aint(stk_itn(tl))+1)/
     & ngrnodes))* nbiostk
                  
                  
                      !--- Surplus DW from tillers that do not have ngrnodes is re-allocated to roots
                      !--- This was implemented to avoid overestimated biomass of oldest internodes (it < ngrnodes)
                      !--- Note that oldest internodes will always have more biomass using this approach, which may lead to a linear gradient among old-yng it
                      dnetbiomass = dnetbiomass + nbiostk * (1.e4/1.e6)
                      nbiostk     = 0.d0
                      
                      if((aint(stk_itn(tl)) + 1) .gt. ngrnodes)then
                      ittdw(tl,(aint(stk_itn(tl)) + 1)-ngrnodes:
     & (aint(stk_itn(tl)) + 1)) = 
     & ittdw(tl,(aint(stk_itn(tl)) + 1)-ngrnodes:
     & (aint(stk_itn(tl)) + 1)) + nbiostk_alloc / ngrnodes
                      else
                      ittdw(tl,1:(aint(stk_itn(tl)) + 1)) = 
     & ittdw(tl,1:(aint(stk_itn(tl)) + 1)) + nbiostk_alloc / 
     & (aint(stk_itn(tl)) + 1)                          
                      endif
                      
                      !--- Total allocated dw due to SS + Surplus 
                      !--- This is a general assumption of SAMUCA model 
                      !--- whereas the surplus biomass must be allocated to stalks 
                      dalloc_stk  = dalloc_stk + nbiostk_alloc    !Total biomass allocated to this stalk [g m-2]
                      dalloc_stk  = dalloc_stk*(1.e4/1.e6)        ![t ha-1]
                      dstkdm      = dstkdm + dalloc_stk           !Total biomass allocated to stalks [t ha-1]
                      nbiostk_alloc   = 0.d0                      
                      stk_dstkdm(tl)  = 0.d0
                      endif
                      
                      !--- Fractions of Sucrose and Fiber
                      do it = 1, (aint(stk_itn(tl)) + 1) 
                          
                          !Fraction of biomass that is sucrose and fiber
                          stalk_suc(it)  = min(1.d0, ((sucmax/(1.d0+exp(
     & -((aint(stk_itn(tl)) + 1)+1-it)+SucAccFac))))) !Equation from Marin & Jones 2014 (DOI:10.1590/S0103-90162014000100001)
                          
                          !Using AScurv (More versatile)
                          stalk_suc(it)  = AScurv(1,itage(tl,it),0.,
     & sucmax,sucshp,(ngrnodes/2.*phyloc),1.) 
                          
                          itsuc(tl,it) = max(0.d0,ittdw(tl,it) * 
     & stalk_suc(it))
                          itfib(tl,it) = max(0.d0,ittdw(tl,it) -
     & itsuc(tl,it))                 
                          suc_after = suc_after + itsuc(tl,it)
                          fib_after = fib_after + itfib(tl,it)                          
                          
                      enddo
                           
              else
                  !This Tiller do not have become a stalk yet (re-allocate to roots)
                  dnetbiomass     = dnetbiomass + stk_dstkdm(tl)
                  stk_dstkdm(tl)  = 0.d0
              endif 
                  
          enddo
                  
              !Daily amount sucrose increment 
              !Note that it can be negative if the SucAccFac is variable with enviromental conditions, e.g. f(temp,swface)
              dsuc = (suc_after - suc_before) * (1.e4/1.e6)   !t ha-1
              dfib = (fib_after - fib_before) * (1.e4/1.e6)   !t ha-1
          
              case(2)
        !--- Single Internode Array (One stalk represents all field)
        !--- From the original version but including thermal age
        !--- Assume leaf number equal to internode number (phytomer)
      
			! INTERNODE ARRAY:
              ! Note: this is not a single stalk internode array (i. e. internode n°1 represent all internodes n°1 per hectare, and so on...)      
			! 1 - Internode Thermal Age
			! 2 - Internode Biomass Partitioning factor
			! 3 - Internode lenght (mm)
			! 4 - Sucrose Mass (t ha-1)
              ! 5 - Fiber Mass   (t ha-1)
              ! 6 - Total Dry Biomass (t ha-1)
              
              !Update internodes Thermal Age
              internode(1:noden ,1)   = internode(1:noden ,1) + di
              
              !Partitioning factor computed by derivative of an asymptote curve as function of internode thermal age
              sumpfac = 0.d0
              do i = 1, noden                  
                  internode(i,2) = AScurv(2,internode(i,1),0.,1.,itshp,
     & (ngrnodes/2.*phyloc),1.)
                  
                  sumpfac = sumpfac + internode(i,2)                  
              enddo
              
              !Daily rates
              dsuc            = 0.d0
              dfib            = 0.d0
              suc_before      = 0.d0
              fib_before      = 0.d0
              flsucexceeded   = .false.
              dsucexceeded    = 0.d0
              sucexceeded     = 0.d0
              stalk_suc       = 0.d0 !Array
              
              !If thermal age (sumpfac <= 0.d0) is zero skip              
              if(sumpfac .gt. 0.d0)then
              !Normalize as function of sumpfac
              do i = 1, noden                  
                  internode(i,2) = max(0.d0, internode(i,2) / sumpfac)
              enddo
              
              !Sucrose and Fiber amount before allocation
              !This is needed because sucrose and fiber pools are dynamic and can change over time due to enviromental conditions
              !The new version of samuca run entirely on phytomer and sink strenght based can simulate the amounts of fiber and sucrose partiotioned biomass
              !For now use it dynamically (Note: Fiber biomass can decrease with this approach, which might not be physilogically true)
              suc_before      = sum(internode(1:noden,4))
              fib_before      = sum(internode(1:noden,5))
              
              !Internode biomass partitioning and expansion
              do i = 1, noden                  
                  internode(i,3) = internode(i,3) + internode(i,2)* dper !Assume biomass increment is proportional to expansion
                  internode(i,6) = internode(i,6) + internode(i,2)* dws
                  
                  !Fraction of biomass that is sucrose and fiber
                  stalk_suc(i)   = min(1.d0, ((sucmax/(1.d0+exp(
     & -(noden+1-i)+SucAccFac))))) !Equation from Marin & Jones 2014 (DOI:10.1590/S0103-90162014000100001)
                  internode(i,4) = max(0.d0,internode(i,6)*stalk_suc(i))
                  internode(i,5) = max(0.d0,internode(i,6) 
     & - internode(i,4))
                  
                  !Check Maximum Sucrose Content
                  if((internode(i,4)/internode(i,6)) .gt. sucmax)then
                      !This internode exceeded the maximum fraction sucrose content 
                      !What the sugarcane crop would do?
                      ! 1 - Reduce PG? (Reduced Growth Phenomena?)
                      ! 2 - Re-allocate to Fiber?
                      ! 3 - Re-allocate to others internodes?
                      ! 4 - RE-allocate to roots?
                      ! 5 - Combination of all together?                      
                      flsucexceeded   = .true.
                      
                      !Account the exceeded sucrose
                      dsucexceeded    = internode(i,4) - 
     & (internode(i,6) * sucmax)                      
                      
                  endif
                      
                  !Total sucrose exceeded the maximum capacity (sucmax)
                  sucexceeded = sucexceeded + dsucexceeded                  
              enddo
              
              !Daily amount sucrose increment 
              !Note that it can be negative if the SucAccFac is variable with enviromental conditions, e.g. f(temp,swface)
              dsuc = sum(internode(1:noden,4)) - suc_before
              dfib = sum(internode(1:noden,5)) - fib_before
              
              endif 
                   
              end select
                      
	return
      end subroutine DIAMPERS
	
	     
      

      ! ----------------------------------------------------------------------
      subroutine readsamuca (crpfil,pathcrop,tdwi,laiem,rgrlai,slatb,
     &  ssa,span,tbase,kdif,kdir,eff,amaxtb,tmpftb,tmnftb,cvl,cvr,cvs,
     &  q10,rml,rmr,rms,rfsetb,frtb,fltb,fstb,perdl,rdrrtb,
     &  rdrstb,hlim1,hlim2u,hlim2l,hlim3h,hlim3l,hlim4,rsc,adcrh,adcrl,
     &  cofab,rdi,rri,rdc,rdctb,rlwtb,logf,schedule,cumdens,
     &  flsolute,ecmax,ecslop,c2eca,c2ecb,c2ecf,numlay,dateharvest,
     &  swharvest,dmharvest1,dmharvest2,swgrazing,grazingfactor,
     &  daylastharvest,dmlastharvest,wrtmax,                            ! Nwgrassland
     &  nsuptab,dmfac,relnitab,nsupply,                                 ! Nwgrassland
     &  swcf,swetr,cftb,chtb,cfet,alphacrit,
     &  swroottyp,wiltpoint,rootradius,rootcoefa,rsw)                   ! NwRootExtr

! ----------------------------------------------------------------------
!     date               : november 2004   
!     purpose            : read parameters for grass growth routine
! ----------------------------------------------------------------------
      implicit none
      include  'arrays.fi'

      integer crp,i,logf,ifnd,getun2,schedule,numlay,swroottyp
      integer swcf,swetr
      logical flsolute,rdinqr
      real*8 slatb(30),amaxtb(30),tmpftb(30),depth,rootdis(202)
      real*8 tmnftb(30),rfsetb(30),frtb(30),fltb(30),rdrrtb(30)
      real*8 rdrstb(30),kdif,kdir,laiem,cofab,fstb(30)
      real*8 hlim1,hlim2u,hlim2l,hlim3h,hlim3l,hlim4,rdctb(22),rlwtb(22)
      real*8 sum,adcrh,adcrl,afgen,cvl,cvr,cvs,eff,rmr
      real*8 perdl,q10,rdc,rdi,rgrlai,rml,rms,rri,span,ssa,tbase,tdwi
      real*8 cumdens(202)
      real*8 ecmax,ecslop,c2eca,c2ecb,c2ecf(maho)
      real*8 cftb(2*magrs),chtb(2*magrs)
      real*8 albedo,rsc,rsw,cfet,alphacrit
      real*8 wiltpoint,rootradius,rootcoefa                           ! NwRootExtr 
      integer daylastharvest,swharvest,swgrazing                      ! Nwgrassland
      real*8 dmharvest1,dmharvest2, dmlastharvest,dateharvest(999)    ! Nwgrassland
      real*8 nsuptab(magrs),dmfac(magrs),relnitab(2*magrs),nsupply    ! Nwgrassland
      real*8 wrtmax,grazingfactor                                     ! Nwgrassland
      character crpfil*(*),pathcrop*(*)
! locals
      integer   swc2ecf
      real*8    dnrinput(magrs),cfinput(magrs),chinput(magrs)
      character message*200,filnam*200
! ----------------------------------------------------------------------


! --- initialise and start reading
      filnam = trim(pathcrop)//trim(crpfil)//'.crp'
      crp = getun2 (10,90,2)
      call rdinit(crp,logf,filnam)

! --- ET related params  ---------

! --- crop factor or crop height
      call rdsinr ('swcf',1,2,swcf)

! --- check use of crop factors in case of ETref
      if (swetr.eq.1 .and. swcf.eq.2) then
        message = 'If ETref is used (SWETR = 1), always define crop '//
     &           'factors (SWCF = 1)' 
        call fatalerr ('ReadGrass',message)
      endif

      if (swcf.eq.1) then
! ---   crop factor is input
        call rdador ('dnr',0.0d0,366.0d0,dnrinput,(magrs),ifnd)
        call rdfdor ('cf',0.0d0,2.0d0,cfinput,(magrs),ifnd)
! ---   store values in cftb
        do i = 1,ifnd
          cftb(i*2) = cfinput(i) 
          cftb(i*2-1) = dnrinput(i)
        enddo
        chtb = -99.99d0
      else
! ---   crop height is input
        call rdador ('dnr',0.0d0,366.0d0,dnrinput,(magrs),ifnd)
        call rdfdor ('ch',0.0d0,1.0d4,chinput,(magrs),ifnd)
! ---   store values in chtb
        do i = 1,ifnd
          chtb(i*2) = chinput(i) 
          chtb(i*2-1) = dnrinput(i)
        enddo
        cftb = -99.99d0
      endif

! --- reflection coefficient and crop resistance
      if (swcf.eq.1) then
! ---   use standard values for ETref
        albedo = 0.23d0
        rsc = 70.0d0
        rsw = 0.0d0
      else
! ---   use crop specific values
        call rdsdor ('albedo',0.0d0,1.0d0,albedo)
        call rdsdor ('rsc',0.0d0,1.0d6,rsc)
        call rdsdor ('rsw',0.0d0,1.0d6,rsw)
      endif


! --- crop growth related params ---------

! --- initial
      call rdsdor ('tdwi',0.0d0,10000.0d0,tdwi)
      call rdsdor ('laiem',0.0d0,10.0d0,laiem)
      call rdsdor ('rgrlai',0.0d0,1.0d0,rgrlai)

! --- green area
      call rdador ('slatb',0.0d0,366.0d0,slatb,30,ifnd)
      call rdsdor ('ssa',0.0d0,1.0d0,ssa)
      call rdsdor ('span',0.0d0,366.0d0,span)
      call rdsdor ('tbase',-10.0d0,30.0d0,tbase)

! --- assimilation
      call rdsdor ('kdif',0.0d0,2.0d0,kdif)
      call rdsdor ('kdir',0.0d0,2.0d0,kdir)
      call rdsdor ('eff',0.0d0,10.0d0,eff)
      call rdador ('amaxtb',0.0d0,366.0d0,amaxtb,30,ifnd)
      call rdador ('tmpftb',-10.0d0,50.0d0,tmpftb,30,ifnd)
      call rdador ('tmnftb',-10.0d0,50.0d0,tmnftb,30,ifnd)

! --- conversion of assimilates into biomass
      call rdsdor ('cvl',0.0d0,1.0d0,cvl)
      call rdsdor ('cvr',0.0d0,1.0d0,cvr)
      call rdsdor ('cvs',0.0d0,1.0d0,cvs)

! --- maintenance respiration
      call rdsdor ('q10',0.0d0,5.0d0,q10)
      call rdsdor ('rml',0.0d0,1.0d0,rml)
      call rdsdor ('rmr',0.0d0,1.0d0,rmr)
      call rdsdor ('rms',0.0d0,1.0d0,rms)
      call rdador ('rfsetb',0.0d0,366.0d0,rfsetb,30,ifnd)

! --- partitioning
      call rdador ('frtb',0.0d0,366.0d0,frtb,30,ifnd)
      call rdador ('fltb',0.0d0,366.0d0,fltb,30,ifnd)
      call rdador ('fstb',0.0d0,366.0d0,fstb,30,ifnd)

! --- death rates
      call rdsdor ('perdl',0.0d0,3.0d0,perdl)
      call rdador ('rdrrtb',0.0d0,366.0d0,rdrrtb,30,ifnd)
      call rdador ('rdrstb',0.0d0,366.0d0,rdrstb,30,ifnd)

! --- water use
      swroottyp = 1
      if(rdinqr('swroottyp')) then
        call rdsinr ('swroottyp',1,2,swroottyp)                         ! NwRootExtr
      endif
      if (swroottyp.eq.1) then                                          ! NwRootExtr
        call rdsdor ('hlim1' ,-100.0d0,100.0d0,hlim1)                   !
        call rdsdor ('hlim2u',-1000.0d0,100.0d0,hlim2u)                 !
        call rdsdor ('hlim2l',-1000.0d0,100.0d0,hlim2l)                 !
        call rdsdor ('hlim3h',-10000.0d0,100.0d0,hlim3h)                !
        call rdsdor ('hlim3l',-10000.0d0,100.0d0,hlim3l)                !
        call rdsdor ('hlim4' ,-16000.0d0,100.0d0,hlim4)                 !
                                                                        !
        call rdsdor ('adcrh',0.0d0,5.0d0,adcrh)                         !
        call rdsdor ('adcrl',0.0d0,5.0d0,adcrl)                         !
!       Criticial stress index for compensation of root water uptake (-)
        alphacrit = 1.0d0
        if(rdinqr('alphacrit')) then
          call rdsdor ('alphacrit',0.2d0,1.0d0,alphacrit)
        endif

      else                                                              !
        call rdsdor ('wiltpoint',-1.0d6,-1.0d3,wiltpoint)               !
        call rdsdor ('rootradius',0.0001d0,1.0d0,rootradius)            !
        call rdsdor ('rootcoefa',0.0d0,1.0d0,rootcoefa)                 !
      endif                                                             ! NwRootExtr

!     correction factor to relate potential transpiration to reference crop
!     (default = 1.0, range = 0.8 - 1.2)
      cfet = 1.0d0
      if(rdinqr('cfet')) then
        call rdsdor ('cfet',0.5d0,1.5d0,cfet)
      endif


! --- salt stress
      if (flsolute) then
        call rdsdor ('ecmax', 0.0d0,20.0d0,ecmax)
        call rdsdor ('ecslop',0.0d0,40.0d0,ecslop)
        call rdsdor ('c2eca', 0.0d0,1000.0d0,c2eca)
        call rdsdor ('c2ecb', 0.0d0,10.0d0,c2ecb)
        call rdsinr ('swc2ecf',1,2,swc2ecf)
        if (swc2ecf.eq.1) then
          call rdsdor ('c2ecf', 0.d0, 10.d0, c2ecf(1))
          if(numlay.ge.2) then
            do i = 2,numlay
              c2ecf(i) = c2ecf(1)
            enddo
          endif
        else if (swc2ecf.eq.2) then
          call rdfdor ('c2ecf', 0.d0, 10.d0, c2ecf,maho,numlay)
        endif
      endif

! --- interception
      call rdsdor ('cofab',0.0d0,1.0d0,cofab)

! --- rooting
      call rdador ('rdctb',0.0d0,100.0d0,rdctb,22,ifnd)
      call rdador ('rlwtb',0.0d0,5000.0d0,rlwtb,22,ifnd)
      call rdsdor ('rdi',0.0d0,1000.0d0,rdi)
      call rdsdor ('rri',0.0d0,100.0d0,rri)
      call rdsdor ('rdc',0.0d0,1000.0d0,rdc)

! --- nutrient stress
      call rdador ('nsuptab',0.0d0,1000.0d0,nsuptab,magrs,ifnd)
      call rdfdor ('dmfac',0.0d0,1.0d0,dmfac,magrs,ifnd)
!     store values in relni
      do i = 1,ifnd
        relnitab(i*2) = dmfac(i)
        relnitab(i*2-1) = nsuptab(i)
      enddo
      call rdsdor ('Nsupply',0.0d0,1000.0d0,Nsupply)


! --- harvest: dm or dates
      call rdsinr ('swharvest',1,2,swharvest)
      if(swharvest.eq.1) then
        call rdsdor ('dmharvest1',0.0d0,100000.0d0,dmharvest1)
        call rdsinr ('swgrazing',1,2,swgrazing)
        if(swgrazing.eq.2) then
            call rdsdor ('dmharvest2',0.0d0,100000.0d0,dmharvest2)
        else if (swgrazing.eq.1) then
            call rdsdor ('grazingfactor',0.0d0,1.0d0,grazingfactor)
        end if
        call rdsinr ('daylastharvest',1,366,daylastharvest)
        call rdsdor ('dmlastharvest',0.0d0,100000.0d0,dmlastharvest)
      elseif(swharvest.eq.2) then
        call rdatim ('dateharvest',dateharvest,999,ifnd)
      endif

! --- maximum weight of roots (kg/ha dm)
      call rdsdor ('wrtmax',0.0d0,100000.0d0,wrtmax)

! --- determine whether irrigation scheduling is applied
      call rdsinr ('schedule',0,1,schedule)

      close (crp)

! --- CALCULATE NORMALIZED CUMULATIVE ROOT DENSITY FUNCTION
      
      if (swroottyp .eq. 1) then                                        ! NwRootExtr

! ---   specify array ROOTDIS with root density distribution
        do i = 0,100
          depth = 0.01d0 * dble(i)
          rootdis(i*2+1) = depth
          rootdis(i*2+2) = afgen(rdctb,22,depth)
        enddo

! ---   calculate cumulative root density function
        do i = 1,202,2
! ---     relative depths
          cumdens(i) = rootdis(i)
        enddo
        sum = 0.d0
        cumdens(2) = 0.d0
        do i = 4,202,2
! ---     cumulative root density
          sum = sum + (rootdis(i-2)+rootdis(i)) * 0.5d0
     &               * (cumdens(i-1)-cumdens(i-3))
          cumdens(i) = sum
        enddo

! ---   normalize cumulative root density function to one
        do i = 2,202,2
          cumdens(i) = cumdens(i) / sum
        enddo

      endif                                                             ! NwRootExtr


      return
        end
        
        
        SUBROUTINE THOURS(TMAX,TMIN,THOUR,TSTRESS)
      USE Variables
	IMPLICIT NONE
	!*****************************************************************************
	!*     Subroutine THOURS
	!*     Calculates the Hourly temperature and daily integration of dPER 
	!******************************************************************************
	!-----------------------------------------------------------------------  
		
	
		REAL thour(24)	                           !Temperature related variables !oC
		REAL tsunset                               !Temperature related variables !oC
		REAL tmax                                  !Temperature related variables !oC
		REAL tmin                                  !Temperature related variables !oC
		REAL :: a=2.                               !original constants from Parton and Logan paper
		REAL :: b=2.2                              !original constants from Parton and Logan paper
		REAL :: c=-0.17                            !original constants from Parton and Logan paper
		REAL decsol                                ! astronomic variables
		REAL x                                     ! astronomic variables
		REAL ahn                                   ! astronomic variables
		REAL timnight                              ! time related variables
		REAL timday                                ! time related variables
		REAL tim                                   ! time related variables
		REAL tstress                               !Temperature stress indicator
		REAL ts(24)                                ! Temperature stress indicator
		REAL dj(365) 
		REAL sunset(365)
		REAL sunrise(365)
		REAL qo(365)
		REAL photop(365)
		REAL nigthp(365)    
		REAL, Parameter :: pi = 3.14159265         !trigonometric variables
		REAL, Parameter :: d_2_r = 3.14159265/180. !trigonometric variables
		REAL, Parameter :: r_2_d = 180./3.14159265 !trigonometric variables
		INTEGER n
		          
          save
          
		DO tim=1,24
			ts(tim) = 0.
			tstress = 0.
		ENDDO
	
		! initializing arrays
			DO n = 1, 365
				dj(n) = n
				sunset(n) = 0
				sunrise(n) = 0
				qo(n) = 0
				photop(n) = 0
			ENDDO
		
		!calculating photoperiod
			DO n = 1,365 
				decsol = 23.45 * SIN((d_2_r*(dj(n)-81.)*(360./365.)))
				ahn  = ACOS((-TAN(d_2_r*lat)*TAN(d_2_r*decsol)))
				photop (n) = 2 * (r_2_d*ahn) / 15
				nigthp (n) = 24 - photop(n)
				sunrise(n) = 12 - photop(n)/2
				sunset(n) = 12 + photop(n)/2
              ENDDO		
		
              !
		!Initial Conditions
			DO tim=1,24
				
				IF (daynr .GT. 365) THEN
					n = daynr - 365
				ELSE
					n = daynr
				END IF
			
				!Calculating SUNSET temperature follow Parton & Logan (1981)
				
				tsunset = (tmax - tmin) * sin((pi*photop(n)-c)/(photop(n)+2*a))
     &           + tmin
				! Estimating daylight temperatures
				IF (tim .GT. sunrise(n) .AND. tim .LT. sunset(n)) THEN
					timday=tim-sunrise(n)
					thour(tim) = tmin + (tmax - tmin) * SIN((pi*timday)/(photop(n)
     &                 + 2*a))
				
				ELSE ! nocturnal temperatures
					IF (tim .GT. sunset(n) .AND. tim .LT. 24) THEN
						timnight = tim - sunset(n)
					ELSE
						timnight = (24.-sunset(n)) + tim
					ENDIF
					thour(tim) = tmin + (tsunset-tmin) * EXP(-b * timnight)/(nigthp(n)/
     &                (2*a))
				ENDIF
						
			ENDDO
		
		DO tim=1,24
	
		!Computing the Temperature Stress on Photosynthesis
		!########Aqui é necessário computar o numero de horas em as temperaturas estiveram acima desses valores.
			IF (thour(tim) .LT. 15) THEN
				ts(tim) = -0.50 + 0.10*ts(tim)
			ELSEIF (thour(tim) .GT. 35) THEN
				ts(tim)= -0.0666667*ts(tim) + 3.33333
			ELSE
				ts(tim)= 1.
			ENDIF
			tstress = tstress + ts(tim)
		ENDDO
		
		tstress = max(0.,min(1.,tstress / 24.))
	
	RETURN
          END SUBROUTINE THOURS
	!************************************************************************