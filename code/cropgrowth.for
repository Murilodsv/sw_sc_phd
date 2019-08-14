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
        if (croptype(numcrop) .eq. 4) call Samuca(3)
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
      
      use Variables 
	implicit none
      
      integer     n_inte_host                             ! Number of integer crop parameter to be read in 'Samuca.par'   called from ReadFile_samuca.f90
      integer     n_real_host                             ! Number of real crop parameter to be read in 'Samuca.par'      called from ReadFile_samuca.f90
      integer     inte_host(6)                            ! Array with integer crop parameters
      real        real_host(108)                          ! Array with real crop parameters
      
      integer     icrop_ini
      integer     icrop_end
      integer     i
      
      integer		task                                  ! Controler of call ordering from the main program
      integer		method_ws                             ! Water Stress Method:  1 = linear response; 2 = asymptote response
      integer		method_pop                            ! Tillering Method:     1 = Cdays; 2 = Cdays + Light Transmission; 3 = Source-Sink
      integer		nseason                               ! 
      integer		atln                                  ! 
      integer		atln_now                              ! 
      integer		dn_lf_alive_dewlap                    ! 
      integer		ghour                                 ! 
      integer		glai                                  ! 
      integer		maxdgl                                ! 
      integer		maxgl                                 ! 
      integer		n_it                                  ! 
      integer		n_it_ag                               ! 
      integer		n_it_bg                               ! 
      integer		n_lf                                  ! 
      integer		n_lf_ag                               ! 
      integer		n_lf_ag_dewlap                        ! 
      integer		n_lf_alive                            ! 
      integer		n_lf_alive_ag                         ! 
      integer		n_lf_alive_bg                         ! 
      integer		n_lf_alive_dewlap                     ! 
      integer		n_lf_alive_juveni                     ! 
      integer		n_lf_alive_juveni_ag                  ! 
      integer		n_lf_alive_juveni_bg                  ! 
      integer		n_lf_bg                               ! 
      integer		n_lf_dead                             ! 
      integer		n_lf_dead_ag                          ! 
      integer		n_lf_dead_bg                          ! 
      integer		n_lf_it_form                          ! 
      integer		n_lf_when_stk_emerg                   ! 
      integer		n_ph                                  ! 
      integer		n_ph_ag                               ! 
      integer		n_ph_bg                               ! 
      integer		nphy_bground                          ! 
      integer		nsublay_cm                            ! 
      integer		phy                                   ! 
      integer		pos_it_bg                             ! 
      integer		sl                                    !
      integer     sub_sl                                !
      integer     n_sub_sl                              !
      integer		tl                                    ! 
      logical		fl_potential                          ! 
      logical		fl_appear_leaf                        ! 
      logical		fl_hasleaf                            ! 
      logical		fl_it_visible                         ! 
      logical		fl_lf_ag_phy                          ! 
      logical		fl_shed_leaf                          ! 
      logical		fl_stalk_emerged                      ! 
      logical		fl_tiller_decrease                    ! 
      logical		fl_tiller_increase                    ! 
      logical		fl_tiller_peaked                      ! 
      logical		fl_tiller_stop                        ! 
      logical		fl_use_reserves                       ! 
      real        agefactor_fac_amax                    ! 
      real        agefactor_fac_rue                     ! 
      real        agefactor_fac_per                     ! 
      real        a_pl                                  ! 
      real        b_pl                                  ! 
      real        c_pl                                  ! 
      real        max_lf_dw                             ! 
      real        init_stalkfw                          ! 
      real        init_stalkht                          ! 
      real        nstalks_planting                      ! 
      real        ini_nstk                              ! 
      real        tilleragefac                          ! 
      real		initcropdepth                         ! 
      real		init_plantdepth_ratoon                ! 
      real		dw_rt                                 ! 
      real		max_rt_dw                             ! 
      real		dw_it_bg                              ! 
      real		str_it_bg                             ! 
      real		sug_it_bg                             ! 
      real		suc_it_bg                             ! 
      real		hex_it_bg                             ! 
      real		dw_it                                 ! 
      real		ini_dw_rt                             ! 
      real		rootleftfrac                          ! 
      real		dw_total                              !                  
      real		age_it_phy                            ! 
      real		age_lf_phy                            ! 
      real		agefactor_amax                        ! 
      real		agefactor_per                         ! 
      real		agefactor_rue                         ! 
      real		amax_conv                             ! 
      real		amax_mod                              ! 
      real		amax_out                              ! 
      real		amaxfbfac                             ! 
      real		avail_subs_crop                       ! 
      real		c_check_tol                           ! 
      real		c_scattering                          ! 
      real		can_ipar                              ! 
      real		chudec_lt                             ! 
      real		chumat_lt                             ! 
      real		co2_pho_res_end                       ! 
      real		co2_pho_res_ini                       ! 
      real		cr_source_sink_ratio                  ! 
      real		cr_source_sink_ratio_ruse             ! 
      real		dage_it_phy                           ! 
      real		dage_lf_phy                           ! 
      real		ddw_it                                ! 
      real		ddw_it_ag                             ! 
      real		ddw_it_ag_dead                        ! 
      real		ddw_it_bg                             ! 
      real		ddw_it_bg_dead                        ! 
      real		ddw_it_dead                           ! 
      real		ddw_it_phy_growth                     ! 
      real		ddw_it_phy_reserves                   ! 
      real		ddw_lf                                ! 
      real		ddw_lf_ag                             ! 
      real		ddw_lf_appear                         ! 
      real		ddw_lf_bg                             ! 
      real		ddw_lf_dead                           ! 
      real		ddw_lf_shed                           ! 
      real		ddw_rt                                ! 
      real		ddw_rt_dead                           ! 
      real		dead_lai                              ! 
      real		diac_at_emergence                     ! 
      real		diacem                                ! 
      real		diacsoil                              ! 
      real		diacsoilem                            ! 
      real		diair                                 ! 
      real		diam_stk                              ! 
      real		diphy                                 ! 
      real		disoil                                ! 
      real		dla_gain_ref_till                     ! 
      real		dla_phy                               ! 
      real		dlai_dead                             ! 
      real		dlai_gain                             ! 
      real		dlai_gain_appear                      ! 
      real		dlai_shed                             ! 
      real		dnstk                                 ! 
      real		dnstk_dead_rate                       ! 
      real		dphy_stimuli                          ! 
      real		dr_itss                               ! 
      real		dr_lfss                               ! 
      real		dr_rtss                               ! 
      real		drdepth                               ! 
      real		dshootext_bg                          ! 
      real		dshootext_bg_rate                     ! 
      real		dstr_it_ag                            ! 
      real		dstr_it_ag_dead                       ! 
      real		dstr_it_bg                            ! 
      real		dstr_it_bg_dead                       ! 
      real		dstr_it_phy                           ! 
      real		dstr_it_phy_growth                    ! 
      real		dsubsres                              ! 
      real		dsubsres_it                           ! 
      real		dsubsres_lf                           ! 
      real		dsubsres_ratio                        ! 
      real		dsubsres_rt                           ! 
      real		dsug_corr_fac_ag                      ! 
      real		dsug_corr_fac_bg                      ! 
      real		dsug_it_ag                            ! 
      real		dsug_it_ag_dead                       ! 
      real		dsug_it_bg                            ! 
      real		dsug_it_bg_dead                       ! 
      real		dsug_it_phy                           ! 
      real		dsug_it_phy_growth                    ! 
      real		dsug_it_phy_reserves                  ! 
      real		dswat_ddws                            ! 
      real		dswat_dsuc                            ! 
      real		dtcrss                                ! 
      real		dtg                                   ! 
      real		dtg_avail_it                          ! 
      real		dtg_avail_it_ag                       ! 
      real		dtg_avail_it_ag_ref_till              ! 
      real		dtg_avail_it_bg                       ! 
      real		dtg_avail_it_bg_ref_till              ! 
      real		dtg_avail_it_phy                      ! 
      real		dtg_avail_it_ref_till                 ! 
      real		dtg_avail_lf                          ! 
      real		dtg_avail_lf_phy                      ! 
      real		dtg_avail_lf_ref_till                 ! 
      real		dtg_avail_rt                          ! 
      real		dtitss                                ! 
      real		dtitss_ag                             ! 
      real		dtitss_ag_ref_till                    ! 
      real		dtitss_bg                             ! 
      real		dtitss_bg_ref_till                    ! 
      real		dtitss_phy                            ! 
      real		dtitss_ref_till                       ! 
      real		dtlfss                                ! 
      real		dtlfss_phy                            ! 
      real		dtlfss_ref_till                       ! 
      real		dtot_str_dw_ref_till                  ! 
      real		dtrtss                                ! 
      real		dw_aerial                             ! 
      real		dw_it_phy                             ! 
      real		dw_lf                                 ! 
      real		dw_lf_ag                              ! 
      real		dw_lf_bg                              ! 
      real		dw_lf_phy                             ! 
      real		dw_lf_shed_phy                        ! 
      real		dw_ss_it                              ! 
      real		dw_ss_it_phy                          ! 
      real		dw_ss_lf                              ! 
      real		dw_ss_lf_phy                          ! 
      real		dw_ss_rt                              ! 
      real		dwat_it_ag                            ! 
      real		dwat_it_ag_dead                       ! 
      real		eff_conv                              ! 
      real		eff_mod                               ! 
      real		eff_out                               ! 
      real		effective_rd                          ! 
      real		end_tt_it_growth                      ! 
      real		end_tt_lf_growth                      ! 
      real		end_tt_rt_growth                      ! 
      real		exc_dtg_it                            ! 
      real		exc_dtg_lf                            ! 
      real		exc_dtg_rt                            ! 
      real		fdeadlf                               ! 
      real		frac_ag                               ! 
      real		frac_bg                               ! 
      real		frac_hex_bg                           ! 
      real		frac_li                               ! 
      real		frac_suc_bg                           ! 
      real		fw_it_ag                              ! 
      real		gresp                                 ! 
      real		gresp_it                              ! 
      real		gresp_it_phy                          ! 
      real		gresp_lf                              ! 
      real		gresp_lf_phy                          ! 
      real		gresp_rt                              ! 
      real		hex_it_ag                             ! 
      real		hex_it_ag_ref_till                    ! 
      real		hex_it_bg_ref_till                    ! 
      real		hex_it_phy                            ! 
      real		hex_min                               ! 
      real		hour                                  ! 
      real		ini_dw_lf_phy                         ! 
      real		ini_la                                ! 
      real		init_leaf_area                        ! 
      real		it_struc_pfac                         ! 
      real		it_struc_pfac_delta                   ! 
      real		it_struc_pfac_max                     ! 
      real		it_struc_pfac_min                     ! 
      real		it_struc_pfac_rate                    ! 
      real		it_struc_pfac_tb                      ! 
      real		it_struc_pfac_te                      ! 
      real		it_struc_pfac_temp_max_red            ! 
      real		it_struc_pfac_tm                      ! 
      real		it_struc_pfac_wate_max_red            ! 
      real		it_struc_tb_end                       ! 
      real		it_struc_tb_ini                       ! 
      real		it_struc_to1                          ! 
      real		it_struc_to2                          ! 
      real		k_can                                 ! 
      real		kmr_it_phy                            ! 
      real		kmr_leaf                              ! 
      real		kmr_root                              ! 
      real		kmr_stem                              ! 
      real		kmr_stor                              ! 
      real		la_lf_shed_phy                        ! 
      real		lai_ass                               ! 
      real		laimod                                ! 
      real		lf_dpos                               ! 
      real		lgpf                                  ! 
      real		lt                                    ! 
      real		ltthreshold                           ! 
      real		maintenance_factor_crop               ! 
      real		maintenance_factor_it                 ! 
      real		maintenance_factor_it_ag              ! 
      real		maintenance_factor_it_bg              ! 
      real		maintenance_factor_it_phy             ! 
      real		maintenance_factor_lf                 ! 
      real		maintenance_factor_lf_phy             ! 
      real		maintenance_factor_rt                 ! 
      real		max_ini_la                            ! 
      real		max_it_dw                             ! 
      real		max_it_dw_bg                          ! 
      real		max_it_dw_phy                         ! 
      real		max_per_it                            ! 
      real		mid_tt_it_growth                      ! 
      real		mid_tt_lf_growth                      ! 
      real		mid_tt_rt_growth                      ! 
      real		mresp_it                              ! 
      real		mresp_it_phy                          ! 
      real		mresp_lf                              ! 
      real		mresp_lf_phy                          ! 
      real		mresp_rt                              ! 
      real		n_lf_max_ini_la                       ! 
      real		n_lf_tiller                           ! 
      real		nsenesleaf_effect                     ! 
      real		nstk_at_appearance                    ! 
      real		nstk_now                              ! 
      real		par_rad                               ! 
      real		per                                   ! 
      real		per_hour                              ! 
      real		per_it_phy                            ! 
      real		pho_fac_co2                           ! 
      real		phy_stimuli                           ! 
      real		phyllochron                           ! 
      real		plastochron                           ! 
      real		poppeak_lt                            ! 
      real		q10_it_phy                            ! 
      real		q10_leaf                              ! 
      real		q10_root                              ! 
      real		q10_stem                              ! 
      real		q10_stor                              ! 
      real		rdprof                                ! 
      real		reduc_growth_factor_crop              ! 
      real		reduc_growth_factor_it                ! 
      real		reduc_growth_factor_it_ag             ! 
      real		reduc_growth_factor_it_bg             ! 
      real		reduc_growth_factor_it_phy            ! 
      real		reduc_growth_factor_lf                ! 
      real		reduc_growth_factor_lf_phy            ! 
      real		reduc_growth_factor_rt                ! 
      real		rel_ss_it_phy                         ! 
      real		rel_ss_lf_phy                         ! 
      real		res_used_emerg                        ! 
      real		res_used_emerg_fac                    ! 
      real		reserves_used_growth_it               ! 
      real		reserves_used_growth_lf               ! 
      real		reserves_used_growth_rt               ! 
      real		reserves_used_mresp_crop              ! 
      real		reserves_used_mresp_it                ! 
      real		reserves_used_mresp_it_ag             ! 
      real		reserves_used_mresp_it_bg             ! 
      real		reserves_used_mresp_it_phy            ! 
      real		reserves_used_mresp_lf                ! 
      real		reserves_used_mresp_lf_phy            ! 
      real		reserves_used_mresp_rt                ! 
      real		rgpf                                  ! 
      real		root_front_size                       ! 
      real		rootdrate                             ! 
      real		rootshape                             ! 
      real		rpup                                  ! 
      real		rue_mod                               ! 
      real		shared_it_str_bg                      ! 
      real		shared_it_sug_bg                      ! 
      real		shootdepth                            ! 
      real		soiltemperature                       ! 
      real		srlmax                                ! 
      real		srlmin                                ! 
      real		stk_h                                 ! 
      real		str_it_ag                             ! 
      real		str_it_phy                            ! 
      real		subs_avail_growth_crop                ! 
      real		subs_avail_growth_it                  ! 
      real		subs_avail_growth_it_ag               ! 
      real		subs_avail_growth_it_ag_ref_till      ! 
      real		subs_avail_growth_it_bg               ! 
      real		subs_avail_growth_it_bg_ref_till      ! 
      real		subs_avail_growth_it_phy              ! 
      real		subs_avail_growth_it_ref_till         ! 
      real		subs_avail_growth_lf                  ! 
      real		subs_avail_growth_lf_phy              ! 
      real		subs_avail_growth_lf_ref_till         ! 
      real		subs_avail_growth_rt                  ! 
      real		subsres                               ! 
      real		subsres_avail_it                      ! 
      real		subsres_avail_it_ag                   ! 
      real		subsres_avail_it_ag_ref_till          ! 
      real		subsres_avail_it_bg                   ! 
      real		subsres_avail_it_bg_ref_till          ! 
      real		subsres_avail_it_phy                  ! 
      real		subsres_avail_it_ref_till             ! 
      real		subsres_avail_lf                      ! 
      real		subsres_avail_lf_phy                  ! 
      real		subsres_avail_lf_ref_till             ! 
      real		subsres_avail_rt                      ! 
      real		suc_acc_ini                           ! 
      real		suc_frac_rate_ts                      ! 
      real		suc_it_ag                             ! 
      real		suc_it_ag_ref_till                    ! 
      real		suc_it_bg_ref_till                    ! 
      real		suc_it_phy                            ! 
      real		suc_min                               ! 
      real		sug_cont                              ! 
      real		sug_it_ag                             ! 
      real		sug_it_phy                            ! 
      real		sup_ratio_it                          ! 
      real		sup_ratio_it_ag                       ! 
      real		sup_ratio_it_bg                       ! 
      real		sup_ratio_it_phy                      ! 
      real		sup_ratio_lf                          ! 
      real		sup_ratio_lf_phy                      ! 
      real		sup_ratio_rt                          ! 
      real		supply_rate_it                        ! 
      real		supply_rate_it_ag                     ! 
      real		supply_rate_it_bg                     ! 
      real		supply_rate_it_phy                    ! 
      real		supply_rate_lf                        ! 
      real		supply_rate_lf_phy                    ! 
      real		supply_rate_rt                        ! 
      real		supply_used_crop                      ! 
      real		supply_used_dw_crop                   ! 
      real		supply_used_dw_it                     ! 
      real		supply_used_dw_it_ag                  ! 
      real		supply_used_dw_it_bg                  ! 
      real		supply_used_dw_it_phy                 ! 
      real		supply_used_dw_lf                     ! 
      real		supply_used_dw_lf_phy                 ! 
      real		supply_used_dw_rt                     ! 
      real		supply_used_gresp_crop                ! 
      real		supply_used_gresp_it                  ! 
      real		supply_used_gresp_it_ag               ! 
      real		supply_used_gresp_it_bg               ! 
      real		supply_used_gresp_it_phy              ! 
      real		supply_used_gresp_lf                  ! 
      real		supply_used_gresp_lf_phy              ! 
      real		supply_used_gresp_rt                  ! 
      real		supply_used_it                        ! 
      real		supply_used_it_ag                     ! 
      real		supply_used_it_bg                     ! 
      real		supply_used_it_phy                    ! 
      real		supply_used_lf                        ! 
      real		supply_used_lf_phy                    ! 
      real		supply_used_mresp_crop                ! 
      real		supply_used_mresp_it                  ! 
      real		supply_used_mresp_it_ag               ! 
      real		supply_used_mresp_it_bg               ! 
      real		supply_used_mresp_it_phy              ! 
      real		supply_used_mresp_lf                  ! 
      real		supply_used_mresp_lf_phy              ! 
      real		supply_used_mresp_rt                  ! 
      real		supply_used_rt                        ! 
      real		t_mresp                               ! 
      real		tb0pho                                ! 
      real		tb1pho                                ! 
      real		tb2pho                                ! 
      real		tbfpho                                ! 
      real		tbmax_per                             ! 
      real		temperature_factor                    ! 
      real		tempfac_per                           ! 
      real		tempfac_pho                           ! 
      real		tilleragefac_adjust                   ! 
      real		tillochron                            ! 
      real		tot_dw_ss_crop                        ! 
      real		tot_dw_ss_it                          ! 
      real		tot_dw_ss_it_ag                       ! 
      real		tot_dw_ss_it_bg                       ! 
      real		tot_dw_ss_lf                          ! 
      real		tot_dw_ss_rt                          ! 
      real		tot_gresp_crop                        ! 
      real		tot_gresp_it                          ! 
      real		tot_gresp_it_ag                       ! 
      real		tot_gresp_it_bg                       ! 
      real		tot_gresp_lf                          ! 
      real		tot_gresp_rt                          ! 
      real		tot_mresp_crop                        ! 
      real		tot_mresp_it                          ! 
      real		tot_mresp_it_ag                       ! 
      real		tot_mresp_it_bg                       ! 
      real		tot_mresp_lf                          ! 
      real		tot_mresp_rt                          ! 
      real		tref_mr                               ! 
      real		tref_mr_it_phy                        ! 
      real		ts_it_phy                             ! 
      real		tt_chumat_lt                          ! 
      real		wat_con                               ! 
      real		wat_it_ag                             !
      
      !--- Crop parameters
      real        amax
      real        phtmax
      real        parmax
      real        ccmp
      real        ccmax
      real        cceff
      real        rue
      real        tb
      real        tbper
      real        chustk
      real        chupeak
      real        chudec
      real        chumat
      real        popmat
      real		POPPEAK		
      real		SLA			
      real		RDM			
      real		DPERCOEFF	
      real		MLA			
      real		KC_MIN		
      real		EORATIO		
      real		RWUEP1		
      real		RWUEP2		
      real		T_MAX_WS_PHO
      real		T_MID_WS_PHO
      real		T_MIN_WS_PHO
      real		T_MAX_WS_EXP
      real		T_MID_WS_EXP
      real		T_MIN_WS_EXP
      real		MAXLAI_EO	
      real		TBM			
      real		THRESHEWS	
      real		SWCON1		
      real		SWCON2		
      real		SWCON3		
      real		RWUMAX		
      real		PORMIN		
      real		T_MAX_WS_FPF
      real		T_MID_WS_FPF
      real		T_MIN_WS_FPF
      real		T_MAX_WS_TIL
      real		T_MID_WS_TIL
      real	    T_MIN_WS_TIL	
      logical     POTENTIAL_GROWTH
      integer     TILLERMET		
      real 	    ROWSP			
      integer     SEQNOW			
      logical     RATOON			
      
      real        BOTTOM(maho)		
      real        DEP(maho)			
      logical     FLCROPALIVE	
      real        PLANTDEPTH	      
      
      real        SLTHICKNESS(maho)
      real        tsoil_lay(maho)
      real        SRL
      real        UPPER(maho)
      real        RLD(maho)
      logical 	USETSOIL
      real 		TRWUP
      real 		TMIN
      real 		TMAX
      real 		THOUR(24)
      real 		SWFACT
      real 		SWFACP
      real 		SWFACF
      real 		SWFACE
      real 		SRAD*8
      integer 	NDWS
      integer 	NDEWS
      logical 	MULCHEFFECT
      integer 	METPG
      real 		LI
      real 		LAT_SIM
      real 		EOP
      real 		DTGA
      integer 	DOY
      real 		DILEAF
      real 		DI
      real 		CO2
      integer  	YEAR				
      logical  	WRITEACTOUT
      integer  	WARN
      real  		TRASW
      real  		RESP
      real  		POL
      integer  	OUTDPP
      real  		KC
      integer  	DAS
      integer  	DAP
      real*8      dso
      real*8      dsinbe
      real*8      dsinb
      real*8      cosld
      real*8      dayl
      real*8      sinld
      real        tmed
      
      integer     outp        
      integer     outd
      integer     outdph
      integer     outdpa
      integer     outpfac
      integer     outstres                
      logical     writedetphoto 
      logical     writedcrop    
      logical     writehead     
      real        dw_it_ag
      real        nstk
      real        diac
      real        sgpf
      logical     flemerged
      logical     flinit_file
      logical     flclos_file
      
      character 	(len = 6)	pltype          				    !  Planting type (Ratoon or PlCane)    
      character 	(len = 6)	cropstatus          			    !  Dead or Alive
	character 	(len = 6)	cropdstage          			    !  Development Stage - Only Sprout or Emergd

      !--- Arrays Variables
      real		phprof(100,60)                                  ! Phytomer profile and attributes dimensions    
      real		drld_sl(maho)                                 !
      real		dw_rt_sl(maho)                                !
      real		ddw_rt_sl(maho)                               !
      real		srl_prof(1000)                                  !
      real		ddw_rt_prof(1000)                               !
      real		drld_prof(1000)                                 !
      real		geot(maho)                                    !
      real		rootprof(1000)                                  ! Root profile (index = cm comparment)    Up to 10 meters
      real		dw_rt_prof(maho)                              !
      real		tillerageprof(100,2)                            !
      real		tempfac_h_per(24)                               ! 24 hours
      real		Acanopy(3+1,5+1)                                ! Instantaneous CO2 Assimilation Rate at three hours and five canopy depth in kg(CO2) ha-1(leaf) h-1 
      real		Qleaf(3+1,5+1)                                  ! Instantaneous par absorbed by leaves at three hours and five canopy depth in W m-2
      real		incpar(3,4)                                     ! Incoming direct, difuse and total par radiation above canopy in three hours W m-2
      real		photo_layer_act(3)                              ! Actual Total Daily Photosynthesis per canopy Layer  
      real		rgf(maho+1,3)                                 !
      real		lroot(maho)                                   !
      real		dlroot(maho)                                  !
      real		drld(maho)                                    !
      real		drld_dead(maho)                               !
      real        relative_rld(maho)
      logical		fl_it_AG(100)                                   ! Above Ground Internode Flag
      logical		fl_lf_AG(100)                                   ! Above Ground Leaf Flag
      logical		fl_lf_alive(100) 
    
      real        array_deb(MAHO)
      
      !--- Real Functions
      real		afgen                                           ! Interpolation function (The Fortran Simulation Translator, FST version 2.0)
      real		fgrowth                                         ! Flexible growth function
      real		asy_ws                                          ! Flexible function for water stress response
      real		tiller_senes                                    ! Tiller senescence function
      
      save      
             
      goto (1000,2000,3000) task
      
      !--- dev notes:
      !   nsublay changed to      nsublay_cm
      !   k       changed to      k_can
      !   nlay    changed to      numlay
      !   check sequential issues: file initialization, rotooning flags, etc...               []
      !   make sure all control parameters are readable or set in the code                    [OK]
      !   retrieve slthickness, bottom, upper and dep from swap                               [OK]
      !   retrieve environmental variables from swap (temperature, rain, radiation, etc...)   []
      !   check if tsoil is calculated for subcompartments or by soil layers                  [OK]
      !   review water_stress to cope with swap
      
      ! Arrays from control
      ! ratoon
      ! rowsp
      ! plantdepth
      ! potential_growth
      ! tillermet
      ! usetsoil

1000  continue
      
      !-------------------------------!
      !--- Reading crop parameters ---!
      !-------------------------------!

      !--- size of parameters arrays
      n_inte_host = size(inte_host)
      n_real_host = size(real_host)
      
      !--- read from 'Samuca.par'
      call ReadFile_samuca(6,
     &    n_inte_host,
     &    inte_host,          
     &    n_real_host,
     &    real_host)

      !--- Passing read parameters values
      nsenesleaf_effect             = inte_host(  1) ! (I)
      maxgl                         = inte_host(  2) ! (I)
      n_lf_max_ini_la               = inte_host(  3) ! (I)
      n_lf_when_stk_emerg           = inte_host(  4) ! (I)
      n_lf_it_form                  = inte_host(  5) ! (I)
      maxdgl                        = inte_host(  6) ! (I)
      amax                          = real_host(  1) ! (R)
      eff                           = real_host(  2) ! (R)
      phtmax                        = real_host(  3) ! (R)
      parmax                        = real_host(  4) ! (R)
      ccmp                          = real_host(  5) ! (R)
      ccmax                         = real_host(  6) ! (R)
      cceff                         = real_host(  7) ! (R)
      rue                           = real_host(  8) ! (R)
      tb                            = real_host(  9) ! (R)
      tb0pho                        = real_host( 10) ! (R)
      tb1pho                        = real_host( 11) ! (R)
      tb2pho                        = real_host( 12) ! (R)
      tbfpho                        = real_host( 13) ! (R)
      tbper                         = real_host( 14) ! (R)
      tbMax_per                     = real_host( 15) ! (R)
      chustk                        = real_host( 16) ! (R)
      chupeak                       = real_host( 17) ! (R)
      chudec                        = real_host( 18) ! (R)
      chumat                        = real_host( 19) ! (R)
      popmat                        = real_host( 20) ! (R)
      poppeak                       = real_host( 21) ! (R)
      ltthreshold                   = real_host( 22) ! (R)
      tillochron                    = real_host( 23) ! (R)
      fdeadlf                       = real_host( 24) ! (R)
      phyllochron                   = real_host( 25) ! (R)
      sla                           = real_host( 26) ! (R)
      rdm                           = real_host( 27) ! (R)
      srlMax                        = real_host( 28) ! (R)
      srlMin                        = real_host( 29) ! (R)
      rootdrate                     = real_host( 30) ! (R)
      max_rt_dw                     = real_host( 31) ! (R)
      end_tt_rt_growth              = real_host( 32) ! (R)
      rootleftfrac                  = real_host( 33) ! (R)
      kdif                          = real_host( 34) ! (R)
      dpercoeff                     = real_host( 35) ! (R)
      mla                           = real_host( 36) ! (R)
      kc_min                        = real_host( 37) ! (R)
      eoratio                       = real_host( 38) ! (R)
      rwuep1                        = real_host( 39) ! (R)
      rwuep2                        = real_host( 40) ! (R)
      t_max_ws_pho                  = real_host( 41) ! (R)
      t_mid_ws_pho                  = real_host( 42) ! (R)
      t_min_ws_pho                  = real_host( 43) ! (R)
      t_max_ws_exp                  = real_host( 44) ! (R)
      t_mid_ws_exp                  = real_host( 45) ! (R)
      t_min_ws_exp                  = real_host( 46) ! (R)
      plastochron                   = real_host( 47) ! (R)
      frac_suc_BG                   = real_host( 48) ! (R)
      frac_hex_BG                   = real_host( 49) ! (R)
      init_leaf_area                = real_host( 50) ! (R)
      max_ini_la                    = real_host( 51) ! (R)
      cr_source_sink_ratio_ruse     = real_host( 52) ! (R)
      init_plantdepth_ratoon        = real_host( 53) ! (R)
      maxlai_eo                     = real_host( 54) ! (R)
      gresp                         = real_host( 55) ! (R)
      kmr_leaf                      = real_host( 56) ! (R)
      kmr_stem                      = real_host( 57) ! (R)
      kmr_root                      = real_host( 58) ! (R)
      kmr_stor                      = real_host( 59) ! (R)
      q10_leaf                      = real_host( 60) ! (R)
      q10_stem                      = real_host( 61) ! (R)
      q10_root                      = real_host( 62) ! (R)
      q10_stor                      = real_host( 63) ! (R)
      tref_mr                       = real_host( 64) ! (R)
      tbm                           = real_host( 65) ! (R)
      threshews                     = real_host( 66) ! (R)
      dshootext_BG_rate             = real_host( 67) ! (R)
      mid_tt_rt_growth              = real_host( 68) ! (R)
      it_struc_tb_ini               = real_host( 69) ! (R)
      it_struc_to1                  = real_host( 70) ! (R)
      it_struc_to2                  = real_host( 71) ! (R)
      it_struc_tb_end               = real_host( 72) ! (R)
      max_it_dw                     = real_host( 73) ! (R)
      mid_tt_it_growth              = real_host( 74) ! (R)
      end_tt_it_growth              = real_host( 75) ! (R)
      mid_tt_lf_growth              = real_host( 76) ! (R)
      end_tt_lf_growth              = real_host( 77) ! (R)
      it_struc_pfac_max             = real_host( 78) ! (R)
      it_struc_pfac_min             = real_host( 79) ! (R)
      it_struc_pfac_tb              = real_host( 80) ! (R)
      it_struc_pfac_tm              = real_host( 81) ! (R)
      it_struc_pfac_te              = real_host( 82) ! (R)
      it_struc_pfac_delta           = real_host( 83) ! (R)
      it_struc_pfac_temp_max_red    = real_host( 84) ! (R)
      it_struc_pfac_wate_max_red    = real_host( 85) ! (R)
      max_it_dw_BG                  = real_host( 86) ! (R)
      suc_min                       = real_host( 87) ! (R)
      max_per_it                    = real_host( 88) ! (R)
      tilleragefac_adjust           = real_host( 89) ! (R)
      dswat_ddws                    = real_host( 90) ! (R)
      dswat_dsuc                    = real_host( 91) ! (R)
      rootshape                     = real_host( 92) ! (R)
      hex_min                       = real_host( 93) ! (R)
      suc_acc_ini                   = real_host( 94) ! (R)
      suc_frac_rate_ts              = real_host( 95) ! (R)
      swcon1                        = real_host( 96) ! (R)
      swcon2                        = real_host( 97) ! (R)
      swcon3                        = real_host( 98) ! (R)
      rwumax                        = real_host( 99) ! (R)
      pormin                        = real_host(100) ! (R)
      tt_chumat_lt                  = real_host(101) ! (R)
      res_used_emerg_fac            = real_host(102) ! (R)
      agefactor_fac_amax            = real_host(103) ! (R)
      agefactor_fac_rue             = real_host(104) ! (R)
      agefactor_fac_per             = real_host(105) ! (R)
      c_scattering                  = real_host(106) ! (R)
      k_can                         = real_host(107) ! (R)
      root_front_size               = real_host(108) ! (R)

      !--- Convert parameters for i/o purpose
      agefactor_fac_amax           = agefactor_fac_amax / 1.e5
      agefactor_fac_rue            = agefactor_fac_rue  / 1.e5
      agefactor_fac_per            = agefactor_fac_per  / 1.e5

      !--- Assume same response for tillering and partitioning factor
      t_max_ws_fpf    = t_max_ws_exp
      t_mid_ws_fpf    = t_mid_ws_exp
      t_min_ws_fpf    = t_min_ws_exp
      t_max_ws_til    = t_max_ws_pho
      t_mid_ws_til    = t_mid_ws_pho
      t_min_ws_til    = t_min_ws_pho
      
      !--- Species-related response
      co2_pho_res_end =   270.d0
      co2_pho_res_ini =   0.d0
      
      !--- retrive soil vertical discretization from 'Swap.swp'
      slthickness = hsublay(1:numlay)
      do sl = 1, numlay         
        if (sl == 1)then             
             dep(sl) = slthickness(sl)
        else             
             dep(sl) = dep(sl-1) + slthickness(sl)
        endif         
        upper(sl)  = dep(sl) - slthickness(sl)
        bottom(sl) = dep(sl)        
      enddo      
    
      !--- Get initial and final icrop rotation IDs for sugarcane
      i           = 1
      icrop_end   = 0
      do while(cropstart(i) .gt. 0.0001)
          if(cropfil(i) .eq. 'Sugarcane') icrop_end = i
          i = i + 1
      enddo      
      i           = 1
      do while(cropstart(i) .gt. 0.0001 .and. 
     & .not. (cropfil(i) .eq. 'Sugarcane'))
          i = i + 1
      enddo
      icrop_ini = i
      
      !--- Update flags and seq ID
      if(icrop .eq. icrop_ini)then
          seqnow      =   1
          flinit_file =   .true.
          flclos_file =   .false.      
      else
          seqnow      =   seqnow  +   1
          flinit_file =   .false.
          if(icrop .eq. icrop_end) flclos_file =   .true. 
      endif      
      
        !--------------------------!
        !--- Simulation Options ---!
        !--------------------------!
      writeactout         = .true.
      writedetphoto       = .true.
      writedcrop          = .true.
      writehead           = .true.
      potential_growth    = .true.    ! Change it when water stress is reviewed
      tillermet           = 1
      metpg               = 2
      ratoon              = .false.
      plantdepth          = 20.d0
      rowsp               = 140.d0
      usetsoil            = .true.      
      mulcheffect         = .false.   ! Mulch effect turned off for this version      
      co2                 = 380.d0      
      swcf                = 1
      
      !--- SWAP Methods:
      swroottyp           = 1         ! Root water uptake type 1 = Feddes; 2 = Matric Flux 
      idev 			    = 1         ! Switch for length of growth period in case of simple crop: 1 = fixed; 2 = depends on temperature sum
      swgc                = 1         ! Switch for simple crop: 1 = leaf area index is input; 2 = soil cover fraction is input
      swcf                = 1         ! Switch for simple crop: 1 = crop factor is input; 2 = crop height is input
      swinter             = 1         ! Switch for interception method: 0 = no interception; 1 = agricultural crops; 2 = trees and forests
      if (swcf.eq.1) then
          !--- use standard values for ETref (FAO56)
          albedo = 0.23d0
          rsc = 70.0d0
          rsw = 0.0d0
      else
          write(*,*) 
     & 'SWCF = 0. Please provide sugarcane albedo, rsc and rsw in'//
     & ' file.crp'
      endif
      
      ch    =   0.d0      ! Crop Height [cm] for PenMon()
      kc    =   kc_min    ! Crop factor (-)
      cf    =   kc        ! Crop factor for SWAP (-)
      
        !--- Simulate Water Stress
        fl_potential      = potential_growth
    
        !---------------!
        !--- Methods ---!
        !---------------!
    
        !--- Water Stress Response
        method_ws         = 2
    
        !--- Tillering
        method_pop        = tillermet
    
        !--- Hourly Hour temperature
        a_pl        = 1.607 !Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = 2.000)
        b_pl        = 2.762 !Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = 2.200)
        c_pl        = 1.179 !Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = -0.17)
    
        !----------------------------------!
        !--- Crop States Initialization ---!
        !----------------------------------!
    
        !--- Leaf dry weight at end of life-spam of a leaf grown under optimun conditions [g]
        max_lf_dw       = mla / sla ! Use this while the model considers fixed SLA (PIT)
    
        !--- Initial dry mass [t ha-1] in planting date (using flat bed method for planting)
        !--- Assuming stalks with 1.5 kg and 2 meters
        init_stalkfw        = 1.5d0 !kg   
        init_stalkht        = 2.d0  !m
        nstalks_planting    = 2.d0  !#
        ini_nstk            = 5. * 1. / (rowsp / 100.) ! plants m-2 - Assuming 5 emerged stems per 1 linear meter (20 cm between each other)
        tilleragefac        = 1.
    
        if(ratoon)then
        
        !-----------------!
        !--- Ratooning ---!
        !-----------------!
        
        pltype = 'Ratoon'
    
        !--- Initializing phytomer profile    
        phprof    = 0.d0 
    
        initcropdepth = min(init_plantdepth_ratoon, plantdepth)
        if(bottom(1) .ge. initcropdepth)then            
            initcropdepth = bottom(1) + 0.01 !Ensure that the ratoon depth shoot is below first soil layer           
        endif
            
        nseason = seqnow
    
        !--- Check root dry weight left from last season
        if(nseason .eq. 1) then
        
            !--- If ratoonng and 1st season: Assume Laclau & Laclau (2009) Root dryWeight at maturity ~ 120 g m-2 
            dw_rt   = max_rt_dw * (1.e4 / 1.e6) ! [ton ha-1]
        
            !--- Substrates for initial growth - Use same as plant cane when ratoon is the first season        
            dw_it_BG    = (init_stalkfw / init_stalkht*nstalks_planting)
     &/ (rowsp/100.) / 1.e3 * 1.e4 ! [ton ha-1]
            
            !--- Considering 70% of water, 15% structural biomass. Substrates reserves are sucrose (13%) and hexoses (2%)
            str_it_BG   =   dw_it_BG *  0.15d0  ! 15% Fiber
            sug_it_BG   =   dw_it_BG *  0.15d0  ! 15% Sugars
            suc_it_BG   =   dw_it_BG *  0.13d0  ! 13% Sucrose
            hex_it_BG   =   dw_it_BG *  0.02d0  !  2% Hexoses        
            dw_it_BG    =   dw_it_BG *  0.3d0   ! 70% Water  
            dw_it       =   dw_it_BG
        
            !--- Fraction of roots left alive after harvesting
            ini_dw_rt   = dw_rt     *   rootleftfrac        
            dw_rt       = ini_dw_rt
            dw_total    = dw_rt     +   str_it_BG   +   sug_it_BG
        
            !--- Roots reached in maximum depth
            rdm             = min(rdm, bottom(numlay))        
            rd              = rdm
            effective_rd    = initcropdepth
        
            !--- Equalize to soil layers depth
            rdprof = 0.d0
            do sl = 1, numlay
                if(rd .ge. upper(sl)) then
                    rdprof = rdprof + slthickness(sl)
                endif              
            enddo
        
            !--- Geotropism function
            do sl = 1, numlay
                geot(sl) = max(0.d0,(1-dep(sl)/rdprof)) ** rootshape
            enddo
        
            !--- Convert dryweight to root length density
            drld = 0.d0
            rld  = 0.d0
            rgf  = 0.d0
            srl  = (srlmin + srlmax) / 2.d0
            do sl = 1, numlay
                rgf(sl,1)       = geot(sl)/sum(geot)
                dw_rt_prof(sl)  = rgf(sl,1) * dw_rt * (1.e6/1.e8)   ! [g cm-2]
                lroot(sl)       = dw_rt_prof(sl)    * srl * 100.d0  ! [cm cm-2]
                drld(sl)        = lroot(sl) / slthickness(sl)       ! [cm cm-3]
                rld(sl)         = rld(sl) + drld(sl)
            enddo
        
            cropdstage      =  'Sprout'
            cropstatus      = ' Alive'
            flcropalive     = .true.        
            
        else
            
            !--- Sequential Ratooning
            !--- Last season below ground biomass will not be reseted in order to be used as initial conditions
            
            !--- Roots remained alive            
            ini_dw_rt   = dw_rt     *   rootleftfrac        
            dw_rt       = ini_dw_rt
            rld         = rld       *   rootleftfrac
            dw_total    = dw_rt
            
            !--- Maximum Root depth [cm]
            !--- Note that the last season root depth is not modified
            rdm             = min(rdm, bottom(numlay))       
            effective_rd    = initcropdepth
            
            !--- Crop Stage
            if(flcropalive) cropdstage  = 'Sprout'
        
        endif
            
        else
        
        !------------------!
        !--- Plant Cane ---!
        !------------------!
        
        pltype = 'PlCane'
        
        !Initial Partitionioning factors
        rgpf = 1.00
        sgpf = 0.d0
        lgpf = 0.d0
        
        !--- Initializing phytomer profile    
        phprof    = 0.d0  
        
        !--- Initial Crop depth as same as the planting depth [cm]
        initcropdepth = plantdepth     
            
        !--- Substrates reserve for before emergence is considered as sugars content remaining in the chopped stalks
        dw_it_BG    =   (init_stalkfw / init_stalkht * nstalks_planting)
     &   / (rowsp/100.) / 1.e3 * 1.e4 ! [ton ha-1]
        
        !--- Considering 70% of water, 15% structural biomass. Substrates reserves are sucrose (13%) and hexoses (2%)
        str_it_BG   =   dw_it_BG *  0.15d0  ! 15% Fiber
        sug_it_BG   =   dw_it_BG *  0.15d0  ! 15% Sugars
        suc_it_BG   =   dw_it_BG *  0.13d0  ! 13% Sucrose
        hex_it_BG   =   dw_it_BG *  0.02d0  !  2% Hexoses        
        dw_it_BG    =   dw_it_BG *  0.3d0   ! 70% Water  
        dw_it       =   dw_it_BG
        
        !--- Fraction of roots left alive after harvesting
        ini_dw_rt   = 0.d0  ! No Roots in plant cane initial conditions
        rld         = 0.d0
        dw_rt       = ini_dw_rt
        dw_total    = str_it_BG   +   sug_it_BG        
        
        !--- Crop Depth [cm]
        rdm             = min(rdm, bottom(numlay))    
        effective_rd    = initcropdepth
        rd              = initcropdepth    
        
        !--- Crop Stage
        cropdstage      =  'Sprout'
        cropstatus      = ' Alive'
        flcropalive     = .true.
        
        endif
            
        !--- Biomass and Aerial Conditions
        dw_aerial   			=   0.d0
        dw_lf       			=   0.d0
        dw_lf_BG    			=   0.d0
        dw_lf_AG    			=   0.d0
        dw_it_AG    			=   0.d0
        dw_it       			=   dw_it_AG + dw_it_BG
        str_it_AG   			=   0.d0
        sug_it_AG   			=   0.d0
        wat_it_AG   			=   0.d0
        fw_it_AG    			=   0.d0
        suc_it_AG   			=   0.d0
        hex_it_AG   			=   0.d0
        nstk					=	0.d0
        lai         			=   0.d0
        lai_ass     			=   0.d0
        stk_h					=	0.d0
        diac        			=   0.d0
        diacsoil    			=   0.d0
        diacem      			=   0.d0
        diacsoilem  			=   0.d0
        nstk_now    			=   ini_nstk
        n_lf_tiller 			=   0.d0
        sug_cont    			=   0.d0
    
        !--- Counters
        n_lf_dead               = 0			
        n_lf_dead_AG            = 0		
        n_lf_dead_BG            = 0		
        n_lf_alive_juveni	      = 1
        n_lf_alive_dewlap       = 0
        n_lf_AG_dewlap          = 0
        n_ph_AG                 = 0				
        n_lf_alive_AG		      = 0
        n_lf_alive_juveni_AG    = 0
        n_it_AG                 = 0				
        n_ph_BG                 = 1				
        n_lf_BG                 = 1				
        n_lf_alive_BG           = 1		
        n_lf_alive_juveni_BG    = 1
        n_ph		              = 1    
        n_lf_alive              = 1
        n_it                    = 1
        n_it_BG                 = 1
        n_lf                    = 1
        n_lf_AG                 = 0
    
        if(.not. (aint(nstk_now) .eq. nstk_now))then
            !--- Increase one tiller to account for decimals
            atln_now    = aint(nstk_now) + 1
        else
            atln_now    = aint(nstk_now)
        endif
    
        !--- Shared Sugars among below ground internodes
        shared_it_sug_BG =  (sug_it_BG * (1.e6/1.e4) / 
     & (ini_nstk * tilleragefac)) / n_it_BG
        shared_it_str_BG =  (str_it_BG * (1.e6/1.e4) / 
     & (ini_nstk * tilleragefac)) / n_it_BG
    
        !--- Update profile total sugars and biomass [g]    
        phprof(1:n_ph,50) = shared_it_sug_BG + shared_it_str_BG
        phprof(1:n_ph,51) = shared_it_str_BG
        phprof(1:n_ph,52) = shared_it_sug_BG
        phprof(1:n_ph,53) = shared_it_sug_BG * frac_hex_BG
        phprof(1:n_ph,54) = shared_it_sug_BG * frac_suc_BG
        phprof(1:n_ph,14) = max_it_dw_BG
        phprof(1:n_ph,11) = kmr_stor
        phprof(1:n_ph,13) = q10_stor
    
        !--- Flags
        fl_use_reserves     = .true.
        flemerged           = .false.
        fl_stalk_emerged    = .false.    
        fl_shed_leaf        = .false.
        fl_it_visible       = .false.
        fl_hasleaf          = .false.	
        fl_appear_leaf      = .false.
        fl_tiller_increase  = .true.
        fl_tiller_peaked    = .false.
        fl_tiller_decrease  = .false.
        fl_tiller_stop      = .false.
        fl_it_AG            = .false.
        fl_lf_AG            = .false.        
        fl_lf_alive(1:n_ph) = .true.
    
        poppeak_lt          = 0.d0
        chudec_lt           = 0.d0
        dnstk_dead_rate     = 0.d0
        chumat_lt           = 0.d0    
    
        !--- Initial root dry weight in g m-2
        ini_dw_rt   =   ini_dw_rt   *   (1.e6 / 1.e4)
    
        !--- Initial Crop Depth [cm]
        shootdepth          = initcropdepth
        diac_at_emergence   = 0.d0
    
        !--- Resources used for emergence (reset plant memory)
        res_used_emerg      = 0.d0
        
        !--------------------!
        !--- Output files ---!
        !--------------------!

        !--- File i/o units
        outp        =   901
        outd        =   902
        outdph      =   903
        outdpa      =   904
        outpfac     =   905
        outstres    =   906
        
        if(flinit_file)then
        !--- open output files
        call output_samuca(  2,           
     &                    project,        
     &                    outp,           
     &                    outd,           
     &                    outdph,         
     &                    outdpa,         
     &                    outpfac,        
     &                    outstres,       
     &                    writedetphoto,  
     &                    writedcrop,     
     &                    writehead)
        endif
        
        !--- Link with SWAP        
        !--- prepare rld data for cumdens calculation
        
        do sl = 2, numlay*2,2
            !relative_rld(sl-1)
            !relative_rld(sl)
            write(*,*)    sl
        enddo
        
        
!        ! --- CALCULATE NORMALIZED CUMULATIVE ROOT DENSITY FUNCTION      
!
!! ---   specify array ROOTDIS with root density distribution
!        do i = 0,100
!          depth = 0.01d0 * dble(i)
!          rootdis(i*2+1) = depth
!          rootdis(i*2+2) = afgen(rdctb,22,depth)
!        enddo
!
!! ---   calculate cumulative root density function
!        do i = 1,202,2
!! ---     relative depths
!          cumdens(i) = rootdis(i)
!        enddo
!        sum = 0.d0
!        cumdens(2) = 0.d0
!        do i = 4,202,2
!! ---     cumulative root density
!          sum = sum + (rootdis(i-2)+rootdis(i)) * 0.5d0
!     &               * (cumdens(i-1)-cumdens(i-3))
!          cumdens(i) = sum
!        enddo
!
!! ---   normalize cumulative root density function to one
!        do i = 2,202,2
!          cumdens(i) = cumdens(i) / sum
!        enddo
            
	return      
      
2000  continue
	
      !-----------------------!
      !--- Potential rates ---!
      !-----------------------!
    
      !--- Not in use by SAMUCA.
      !--- To run potential conditions, turn-off the Limiting Factors -> potential_growth = .true.
	
      return
      
3000  continue
      
      !-------------------------------------!
      !--- Time-Step Rate Initialization ---!
      !-------------------------------------!
           
      !--- Link with SWAP variables
      srad    =   rad * 1.e-6
      tmax    =   tmx
      tmin    =   tmn      
      tmed    =   (tmax + tmin) / 2.d0
      eop     =   ptra      
      doy     =   daynr
      das     =   daycum
      dap     =   daycrop
      year    =   iyear
      lat_sim =   lat
      
      !--- Crop Step-Rates
      di							= 0.d0
      disoil						= 0.d0
      drdepth						= 0.d0
      dshootext_BG				= 0.d0
      rgf     					= 0.d0
      dw_rt_prof                  = 0.d0
      drld						= 0.d0
      drld_dead					= 0.d0
      dphy_stimuli				= 0.d0
      dla_gain_ref_till           = 0.d0
      dlai_gain					= 0.d0
      dlai_dead					= 0.d0
      dlai_shed					= 0.d0
      dlai_gain_appear            = 0.d0
      ddw_lf_appear               = 0.d0
      per							= 0.d0
      per_hour					= 0.d0
      dnstk						= 0.d0
      ddw_rt         				= 0.d0
      ddw_lf         				= 0.d0
      ddw_lf_BG                   = 0.d0
      ddw_lf_AG                   = 0.d0
      ddw_it         				= 0.d0
      ddw_it_AG      				= 0.d0
      dstr_it_AG     				= 0.d0
      dsug_it_AG     				= 0.d0
      ddw_it_BG      				= 0.d0
      dstr_it_BG     				= 0.d0
      dsug_it_BG     				= 0.d0
      dwat_it_AG     				= 0.d0
      ddw_lf_shed    				= 0.d0
      dsubsres       				= 0.d0
      ddw_rt_dead    				= 0.d0
      ddw_lf_dead    				= 0.d0
      ddw_it_dead    				= 0.d0
      ddw_it_AG_dead 				= 0.d0
      dstr_it_AG_dead				= 0.d0
      dsug_it_AG_dead				= 0.d0
      ddw_it_BG_dead 				= 0.d0
      dstr_it_BG_dead				= 0.d0
      dsug_it_BG_dead				= 0.d0
      dwat_it_AG_dead				= 0.d0

      !--- Respiration Rates and Sink Strenght
      tot_gresp_lf 				= 0.d0
      tot_mresp_lf 				= 0.d0
      tot_dw_ss_lf 				= 0.d0
      tot_gresp_it_AG 			= 0.d0
      tot_mresp_it_AG 			= 0.d0
      tot_dw_ss_it_AG 			= 0.d0
      tot_gresp_it_BG 			= 0.d0
      tot_mresp_it_BG 			= 0.d0
      tot_dw_ss_it_BG 			= 0.d0
      tot_gresp_rt				= 0.d0
      tot_mresp_rt				= 0.d0
      dw_ss_rt					= 0.d0
      dtlfss						= 0.d0
      dtitss						= 0.d0
      dtitss_AG					= 0.d0
      dtitss_BG					= 0.d0
      dtrtss						= 0.d0
      dtcrss						= 0.d0
      tot_gresp_it				= 0.d0
      tot_mresp_it				= 0.d0
      tot_dw_ss_it				= 0.d0
      tot_gresp_crop 				= 0.d0
      tot_mresp_crop 				= 0.d0
      tot_dw_ss_crop 				= 0.d0
      dr_rtss 					= 0.d0
      dr_lfss 					= 0.d0
      dr_itss 					= 0.d0
      frac_AG   					= 0.d0
      frac_BG   					= 0.d0
      rel_ss_lf_phy				= 0.d0
      rel_ss_it_phy				= 0.d0
      dw_ss_lf					= 0.d0
      gresp_lf					= 0.d0
      mresp_lf					= 0.d0
      dw_ss_it					= 0.d0
      gresp_it					= 0.d0
      mresp_it					= 0.d0

      !--- Substrates Assimilated
      dtg							= 0.d0
      dtga                        = 0.d0
      frac_li                     = 0.d0
      li                          = 0.d0
      Acanopy				        = 0.d0
      Qleaf				        = 0.d0
      incpar				        = 0.d0
      photo_layer_act				= 0.d0
      avail_subs_crop				= 0.d0
      dtg_avail_rt    			= 0.d0
      dtg_avail_lf    			= 0.d0
      dtg_avail_it    			= 0.d0
      dtg_avail_it_BG 			= 0.d0
      dtg_avail_it_AG 			= 0.d0
      subsres_avail_rt    		= 0.d0
      subsres_avail_lf    		= 0.d0
      subsres_avail_it    		= 0.d0
      subsres_avail_it_BG 		= 0.d0
      subsres_avail_it_AG 		= 0.d0
      sup_ratio_lf				= 0.d0
      supply_rate_lf				= 0.d0
      supply_used_lf				= 0.d0
      supply_used_mresp_lf		= 0.d0
      supply_used_gresp_lf		= 0.d0
      supply_used_dw_lf			= 0.d0
      reserves_used_mresp_lf		= 0.d0
      maintenance_factor_lf		= 1.d0
      reduc_growth_factor_lf		= 1.d0
      sup_ratio_it				= 0.d0
      supply_rate_it				= 0.d0
      supply_used_it				= 0.d0
      supply_used_mresp_it		= 0.d0
      supply_used_gresp_it		= 0.d0
      supply_used_dw_it			= 0.d0
      reserves_used_mresp_it		= 0.d0
      maintenance_factor_it		= 1.d0
      reduc_growth_factor_it		= 1.d0
      sup_ratio_it_BG				= 0.d0
      supply_rate_it_BG			= 0.d0
      supply_used_it_BG			= 0.d0
      supply_used_mresp_it_BG		= 0.d0
      supply_used_gresp_it_BG		= 0.d0
      supply_used_dw_it_BG		= 0.d0
      reserves_used_mresp_it_BG	= 0.d0
      maintenance_factor_it_BG	= 1.d0
      reduc_growth_factor_it_BG	= 1.d0
      sup_ratio_it_AG				= 0.d0
      supply_rate_it_AG			= 0.d0
      supply_used_it_AG			= 0.d0
      supply_used_mresp_it_AG		= 0.d0
      supply_used_gresp_it_AG		= 0.d0
      supply_used_dw_it_AG		= 0.d0
      reserves_used_mresp_it_AG	= 0.d0
      maintenance_factor_it_AG	= 1.d0
      reduc_growth_factor_it_AG	= 1.d0
      sup_ratio_rt				= 0.d0
      supply_rate_rt				= 0.d0
      supply_used_rt				= 0.d0
      supply_used_mresp_rt		= 0.d0
      supply_used_gresp_rt		= 0.d0
      supply_used_dw_rt			= 0.d0
      reserves_used_mresp_rt		= 0.d0
      maintenance_factor_rt		= 1.d0
      reduc_growth_factor_rt		= 1.d0
      supply_used_crop		    = 0.d0
      supply_used_mresp_crop	    = 0.d0
      supply_used_gresp_crop	    = 0.d0
      supply_used_dw_crop		    = 0.d0
      reserves_used_mresp_crop    = 0.d0
      maintenance_factor_crop	    = 1.d0
      reduc_growth_factor_crop    = 1.d0
      dtg_avail_lf_ref_till       = 0.d0
      dtg_avail_it_ref_till       = 0.d0
      dtg_avail_it_BG_ref_till    = 0.d0
      dtg_avail_it_AG_ref_till    = 0.d0
      subsres_avail_lf_ref_till   = 0.d0
      subsres_avail_it_ref_till   = 0.d0
      subsres_avail_it_BG_ref_till= 0.d0
      subsres_avail_it_AG_ref_till= 0.d0
      dtlfss_ref_till             = 0.d0
      dtitss_ref_till             = 0.d0
      dtitss_BG_ref_till          = 0.d0
      dtitss_AG_ref_till          = 0.d0
      dtg_avail_lf_phy          	= 0.d0
      subsres_avail_lf_phy      	= 0.d0
      dtlfss_phy                	= 0.d0
      mresp_lf_phy              	= 0.d0
      gresp_lf_phy              	= 0.d0
      dw_ss_lf_phy              	= 0.d0
      sup_ratio_lf_phy          	= 0.d0
      supply_rate_lf_phy        	= 0.d0
      supply_used_lf_phy        	= 0.d0
      supply_used_mresp_lf_phy  	= 0.d0
      supply_used_gresp_lf_phy  	= 0.d0
      supply_used_dw_lf_phy     	= 0.d0
      reserves_used_mresp_lf_phy	= 0.d0
      maintenance_factor_lf_phy 	= 1.d0
      reduc_growth_factor_lf_phy	= 1.d0
      dtg_avail_it_phy          	= 0.d0
      subsres_avail_it_phy      	= 0.d0
      dtitss_phy                	= 0.d0
      mresp_it_phy              	= 0.d0
      gresp_it_phy              	= 0.d0
      dw_ss_it_phy              	= 0.d0
      sup_ratio_it_phy          	= 0.d0
      supply_rate_it_phy        	= 0.d0
      supply_used_it_phy        	= 0.d0
      supply_used_mresp_it_phy  	= 0.d0
      supply_used_gresp_it_phy  	= 0.d0
      supply_used_dw_it_phy     	= 0.d0
      reserves_used_mresp_lf_phy	= 0.d0
      maintenance_factor_it_phy 	= 1.d0
      reduc_growth_factor_it_phy	= 1.d0
      it_struc_pfac_rate			= 0.d0
      dstr_it_phy					= 0.d0
      dsug_it_phy					= 0.d0
      exc_dtg_lf      			= 0.d0
      exc_dtg_it      			= 0.d0
      exc_dtg_rt      			= 0.d0
      dtot_str_dw_ref_till		= 0.d0
      suc_it_AG_ref_till	        = 0.d0
      hex_it_AG_ref_till	        = 0.d0
      suc_it_BG_ref_till	        = 0.d0
      hex_it_BG_ref_till	        = 0.d0
      shared_it_sug_BG            = 0.d0

      !--- Crop Stress factors
      swfacp						= 1.d0
      swface						= 1.d0
      swfact						= 1.d0
      swfacf						= 1.d0
      agefactor_per				= 1.d0
      agefactor_rue				= 1.d0
      pho_fac_co2					= 1.d0
      tempfac_h_per				= 1.d0
      tempfac_pho					= 1.d0
      tempfac_per					= 1.d0
      amaxfbfac					= 1.d0
      agefactor_amax              = 1.d0

      !--- Phytomer Rates
      phprof(1: 100, 2) 		= 0.d0  ! Leaf Sink strenght
      phprof(1: 100, 3) 		= 0.d0  ! Allocated Leaf biomass
      phprof(1: 100, 4) 		= 0.d0  ! Leaf area rate
      phprof(1: 100, 7) 		= 0.d0  ! Internode Sink Strength dw rate g d-1
      phprof(1: 100,21) 		= 0.d0 	! Internode Growth Respiration
      phprof(1: 100,22) 		= 1.d0 	! Maintenance Respiration Factor (0-1) 1 =  is maintenance is ok
      phprof(1: 100,23) 		= 0.d0 	! dLength (cm)
      phprof(1: 100,25) 		= 0.d0 	! mresp leaf
      phprof(1: 100,26) 		= 0.d0 	! gresp leaf
      phprof(1: 100,27) 		= 0.d0 	! dw ss leaf
      phprof(1: 100,28) 		= 1.d0 	! sup_ratio_lf_phy
      phprof(1: 100,29) 		= 0.d0 	! supply_rate_lf
      phprof(1: 100,30) 		= 0.d0 	! sup_ratio_lf_phy
      phprof(1: 100,31) 		= 0.d0 	! supply_used_mresp_lf
      phprof(1: 100,32) 		= 0.d0 	! supply_used_gresp_lf
      phprof(1: 100,33) 		= 0.d0 	! supply_used_dw_lf
      phprof(1: 100,34) 		= 1.d0 	! maintenance_factor_lf
      phprof(1: 100,35) 		= 1.d0 	! reduc_growth_factor_lf
      phprof(1: 100,36) 		= 0.d0 	! mresp internode
      phprof(1: 100,37) 		= 0.d0 	! gresp internode
      phprof(1: 100,38) 		= 0.d0 	! dw ss internode
      phprof(1: 100,39) 		= 0.d0 	! supply_rate_it
      phprof(1: 100,40) 		= 1.d0 	! sup_ratio_it_phy
      phprof(1: 100,41) 		= 0.d0 	! supply_used_it
      phprof(1: 100,42) 		= 0.d0 	! supply_used_mresp_it
      phprof(1: 100,43) 		= 0.d0 	! supply_used_gresp_it
      phprof(1: 100,44) 		= 0.d0 	! supply_used_dw_it
      phprof(1: 100,45) 		= 1.d0 	! maintenance_factor_it
      phprof(1: 100,46) 		= 1.d0 	! reduc_growth_factor_it
      phprof(1: 100,47) 		= 0.d0 	! Internode dry weigth rate [g dt-1]
      phprof(1: 100,48) 		= 0.d0 	! Internode structural dry weigth rate [g dt-1]
      phprof(1: 100,49) 		= 0.d0 	! Internode total sugars rate [g dt-1]
      phprof(1: 100,55)       = 0.d0  ! Leaf Age rate [dCdays]
      phprof(1: 100,56)       = 0.d0  ! Phytomer Age rate [dCdays]
      phprof(1: 100,57)       = 0.d0  ! Internode Age rate [dCdays]
    
      !--- Check if crop is alive
      if(.not. flcropalive) return

      !------------------------!
      !--- DEFINING FACTORS ---!
      !------------------------!

      !-----------------------!
      !--- Solar Radiation ---!
      !-----------------------!

      !--- Fraction of Solar Radiation Crop Can Actively Use
      !--- Photosynthetically Active Radiation (PAR) - Assume as 50% of total incoming radiation flux
      par_rad = srad * 0.5d0

      !--- Canopy light interception fraction (Beer Law)
      li      = 1.d0 - exp(-k_can * lai)

      !--- Canopy intercepted PAR [MJ]
      can_ipar = par_rad * li

      !-------------------!
      !--- Temperature ---!
      !-------------------!

      !--- Temperature stress on photosynthesis
      tempfac_pho      = temperature_factor(tmed, tb0pho, tb1pho,tb2pho,
     & tbfpho) ! Photosynthesis
      
      !--- Temperature stress factor on expansioning
      tempfac_per      = min(1.,max(0.,tmed - tbper) / 
     & (tbMax_per - tbper)) 

      !--- Hourly Temperature (PL model)
      call TempHour_samuca(tmax,tmin,doy,lat_sim,a_pl,b_pl,c_pl,thour)

      !--- Hourly Plant Extension Temperature Factor
      do hour = 1, 24
      tempfac_h_per(hour)   = min(1.d0, max(0.d0, thour(hour) - tbper) 
     & / (tbMax_per - tbper))
      enddo

      !-------------------------------------!
      !--- Atmospheric CO2 Concentration ---!
      !-------------------------------------!

      !--- Following Mathew Jones and Abraham Singels (https://doi.org/10.1016/j.eja.2017.12.009)
      !--- No effect on C4 Photosynthesis after 270 ppm - Higher Water Use Efficiency is believed to be the reason of increased biomass gain under increased CO2 (included in ptrans())
      if(co2 .gt. co2_pho_res_end)then
      !--- Optimun CO2 conditions
      pho_fac_co2 =   1.d0
      else if(co2 .lt. co2_pho_res_ini)then
      !--- Extreme Low CO2 conditions (No photosynthesis)
      pho_fac_co2 =   0.d0
      else
      !--- Transient concentration
      pho_fac_co2 = (co2_pho_res_end - co2) / 
     & (co2_pho_res_end - co2_pho_res_ini)
      endif

      !-----------------------!
      !--- Genotype traits ---!
      !-----------------------!

      !--- Aging factor to account for reduced growth due to crop aging
      !--- Necessary to include effects of the Reduced Growth Phenomena (RGP) still not well understood,
      !--- or reduced turgor pressure on top parts in planting extension.
      agefactor_rue = exp(agefactor_fac_rue * diacem)
      agefactor_rue = min(1.d0, agefactor_rue)

      !--- Max assimimilation reduction
      agefactor_amax = exp(agefactor_fac_amax * diacem)
      agefactor_amax = min(1.d0, agefactor_amax)

      !--- Age reduction factor for dper, based on N. G. Inman-Bamber et al. 2008 Australian Journal of Agricultural Research, Fig.3
      agefactor_per = exp(agefactor_fac_per * diacem)
      agefactor_per = min(1.d0, agefactor_per)

      !------------------------!
      !--- LIMITING FACTORS ---!
      !------------------------!

      if(.not. fl_potential)then

      !--- Water Stress ---!
      call waterstress(   2,            
     & ndws,         
     & ndews,        
     & eop,          
     & trwup,        
     & rwuep1,       
     & rwuep2 ,      
     & t_max_ws_pho, 
     & t_mid_ws_pho, 
     & t_min_ws_pho, 
     & t_max_ws_exp, 
     & t_mid_ws_exp, 
     & t_min_ws_exp, 
     & t_max_ws_til, 
     & t_mid_ws_til, 
     & t_min_ws_til, 
     & t_max_ws_fpf, 
     & t_mid_ws_fpf, 
     & t_min_ws_fpf, 
     & threshews,    
     & swfacp,       
     & swface,       
     & swfact,       
     & swfacf)

      if(swfacp .lt. 1.d0 .and. flemerged)then
          fl_use_reserves = .false. ! do not use reserves when crop is stressed-out
      endif


      !--- Soil Nutrients ---!

      !--- (PIT) ---!

      endif
    
      !------------------------!
      !--- REDUCING FACTORS ---!
      !------------------------!

      !--- (PIT) ---!


      !------------------------!
      !--- CROP DEVELOPMENT ---!
      !------------------------!

      !----------------!
      !--- Age Rate ---!
      !----------------!
      if(usetsoil)then          

      !----------------------------!
      !--- Use Soil Temperature ---!
      !----------------------------!
          
          !--- Retrive soil temperature for each layer
          tsoil_lay   = 0.d0
          sl          = 1
          n_sub_sl    = 1        
          do sub_sl = 1, numnod              
              
              tsoil_lay(sl)   =  tsoil_lay(sl) +  tsoil(sub_sl) * 
     & dz(sub_sl) / slthickness(sl)
              
              n_sub_sl = n_sub_sl + 1              
              if(n_sub_sl .gt. ncomp(sl))then
                  n_sub_sl = 1
                  sl       = sl + 1
              endif
          enddo
          
          !######## DEBUG
          if(icrop .eq. 2)then
              write(*,*) 'debug'
          endif
          
          if(isnan(tsoil_lay(1)))then
              write(*,*) 'debug'
          
          endif         
          
          !######## DEBUG
          
      if(mulcheffect)then
          !--- Mulch effect when mulch is present (skip the first layer temperature (mulch))
              !--- average soil temperature in plant depth soil layers (weighted mean)
                  soiltemperature = 0.d0
                  do sl = 1, numlay
                      if(initcropdepth .gt. upper(sl)) then

                          if(initcropdepth .gt. bottom(sl)) then
                              soiltemperature = soiltemperature + 
     & tsoil_lay(sl+1) * slthickness(sl) / initcropdepth
                          else
                              soiltemperature = soiltemperature + 
     & tsoil_lay(sl+1) * (initcropdepth - bottom(sl-1))  / initcropdepth
                          endif
                      endif
                  enddo
              else
              !--- use soil temperature but without mulch effect (bare soil)
              !--- average soil temperature in plant depth soil layers (weighted mean)
                      
                  soiltemperature = 0.d0
                  do sl = 1, numlay
                      if(initcropdepth .gt. upper(sl)) then
                          if(initcropdepth .ge. bottom(sl)) then
                              soiltemperature = soiltemperature + 
     & tsoil_lay(sl) * slthickness(sl) / initcropdepth
                          else
                              soiltemperature = soiltemperature + 
     & tsoil_lay(sl) * (initcropdepth - bottom(sl-1))  / initcropdepth
                          endif
                      endif
                  enddo
              endif

              !--- Degree-days using soil and air temperatures
              disoil  = min(max(0.d0, soiltemperature - tb)   ,tbm - tb)
              diair   = min(max(0.d0, tmed - tb)              ,tbm - tb)

              !--- Use soil temperature to compute crop age until stalks arises (Appical meristems is the sensor)
              if(.not. fl_stalk_emerged) then

              !--- Computing crop age (degree-days) based on soil temperature
              di      = disoil

              if(flemerged)then
                  !--- Leaves emerges prior to stalks
                      dileaf = diair
                  else
                  !--- Before emergence leaves (shoots) are below ground
                      dileaf  = disoil
                  endif
              else

              !--- Computing crop age (degree-days) based on air temperature
              di      = diair
              dileaf  = diair
              endif

      else

        !--------------------------------!
        !--- Not Use Soil Temperature ---!
        !--------------------------------!

        !--- Considering soil temperature equal as air temperature
        soiltemperature = tmed

        !--- Computing crop age (degree-days) based on air temperature
        di      = min(max(0.d0, tmed - tb), tbm - tb)
        diair   = di
        disoil  = di
        dileaf  = di

      endif
    
      !------------------------!
      !--- Phytomer Stimuli ---!
      !------------------------!

      !--- Stimuli rate [phy/dt]
      dphy_stimuli    = (1.d0 / plastochron) * di
    
    
      if(flemerged)then

      !-----------------!
      !--- Tillering ---!
      !-----------------!

      select case(method_pop)

      case(1)

      !------------------------------------!
      !--- Tillering Rate ~ Thermal Age ---!
      !------------------------------------!

      !--- Daily initial tillers numbers rate
      if(diacsoil .lt. (chupeak + diac_at_emergence)) then
          !--- Initial tiller grow
              dnstk = ((poppeak-ini_nstk)/(chupeak)) * disoil

          elseif(diacsoil .ge. (chupeak + diac_at_emergence) .and. 
     & diacsoil .lt. (chudec + diac_at_emergence)) then
          !--- tillering peak
              dnstk = 0.

          elseif(diacsoil .ge. (chudec + diac_at_emergence) .and. 
     & diacsoil .lt. (chumat + diac_at_emergence)) then
          !--- reduction phase to mature (abortion of tillers)
              dnstk = (-(poppeak - popmat) / ((chumat)-(chudec)))*disoil

          elseif(diacsoil .ge. (chumat+diac_at_emergence)) then
          !--- late stable tiller population
              dnstk = 0.
          endif

      case(2)

      !------------------------------------------------------!
      !--- Tillering Rate ~ Thermal Age + Solar Radiation ---!
      !------------------------------------------------------!


      !--- Dead leaf position below the living leaves profile
      lf_dpos     = n_lf_alive_AG + 1

      !--- Dead LAI, where the fdeadlf is the fraction of blades of attached dead leaves
      dead_lai    = sum(phprof(lf_dpos : (lf_dpos + 
     & aint(nsenesleaf_effect)), 5)) * nstk_now * 
     & tilleragefac * 1.e-4 * fdeadlf

      !--- Modified LAI
      laimod = lai + dead_lai

      !--- Transmitted Light Through Canopy
      lt     = exp(-k_can * laimod)

      !--- Check Tillering States
      if(lt .lt. ltthreshold .and. .not. fl_tiller_peaked)then
          !--- Fisrt time reaching tillering peak
              fl_tiller_peaked    = .true.
              fl_tiller_stop      = .true.

              !--- Store the peak of population
              poppeak_lt          = nstk_now
              chudec_lt           = diacsoil
              chumat_lt           = chudec_lt + tt_chumat_lt

              !--- Tillering dead rate [till cdays-1]
              dnstk_dead_rate     = (popmat - poppeak_lt) / 
     & (chumat_lt - chudec_lt)

          else if(lt .lt. ltthreshold .and. fl_tiller_peaked)then
          !--- Decrease tillering
              fl_tiller_decrease  = .true.
              fl_tiller_stop      = .false.

              !--- Update Tillering dead rate [till cdays-1]
              if(diacsoil .gt. chumat_lt)then
                  dnstk_dead_rate = 0.d0
              else
                  dnstk_dead_rate     = (popmat - nstk_now) / 
     & (chumat_lt - diacsoil)
              endif

          else if(lt .gt. ltthreshold .and. fl_tiller_peaked)then
          !--- Don't let increase tillering after peak has reached (Unless lodging happens and resets fl_tiller_peaked)
              fl_tiller_stop      = .true.
          else
          !--- Increase tillering
              fl_tiller_increase  = .true.
              fl_tiller_stop      = .false.
          endif

          !--- No change in tillering
          if(fl_tiller_stop)then
              fl_tiller_increase  = .false.
              fl_tiller_decrease  = .false.
          endif

          !--- Tillering rate
          if(fl_tiller_increase)then
              !--- Increase number of tiller
                  dnstk = disoil / tillochron
              elseif(fl_tiller_decrease)then
              !--- Decrease number of tiller
                  dnstk = dnstk_dead_rate * disoil
              else
              !--- Stabilize plant population
                  dnstk = 0.d0
              endif

          end select

          !--- Dead Tissues rate due to tiller senescence
          if(dnstk .lt. 0.d0)then

              !--- Dead Biomass rates [ton ha-1]
              ddw_lf_dead         =   tiller_senes(dw_lf,     
     & nstk_now, dnstk, tilleragefac, tillerageprof, atln)
              ddw_it_dead         =   tiller_senes(dw_it,     
     & nstk_now, dnstk, tilleragefac, tillerageprof, atln)
              ddw_it_AG_dead      =   tiller_senes(dw_it_AG,  
     & nstk_now, dnstk, tilleragefac, tillerageprof, atln)
              dstr_it_AG_dead     =   tiller_senes(str_it_AG, 
     & nstk_now, dnstk, tilleragefac, tillerageprof, atln)
              dsug_it_AG_dead     =   tiller_senes(sug_it_AG, 
     & nstk_now, dnstk, tilleragefac, tillerageprof, atln)
              ddw_it_BG_dead      =   tiller_senes(dw_it_BG,  
     & nstk_now, dnstk, tilleragefac, tillerageprof, atln)
              dstr_it_BG_dead     =   tiller_senes(str_it_BG, 
     & nstk_now, dnstk, tilleragefac, tillerageprof, atln)
              dsug_it_BG_dead     =   tiller_senes(sug_it_BG, 
     & nstk_now, dnstk, tilleragefac, tillerageprof, atln)
              dwat_it_AG_dead     =   tiller_senes(wat_it_AG, 
     & nstk_now, dnstk, tilleragefac, tillerageprof, atln)

              !--- Dead Leaf Area Index [m2 m-2]
              dlai_dead = ddw_lf_dead * (1.e6/1.e4) * sla / 1.e4

          endif
          
          endif
    
          !--- Leaf shedding rate
          if(fl_shed_leaf)then

          !--- Senesced leaf dry weight and area
          dw_lf_shed_phy      =   phprof(n_lf_alive + 1, 6)
          la_lf_shed_phy      =   phprof(n_lf_alive + 1, 5)
          nstk_at_appearance  =   phprof(n_lf_alive + 1, 10)
          fl_lf_AG_phy        =   fl_lf_AG(n_lf_alive + 1)

          !--- Shed DW Leaf rate [ton ha-1]
          ddw_lf_shed = dw_lf_shed_phy * nstk_now * tilleragefac * 
     & (1.e4/1.e6) * min(1.d0, nstk_at_appearance / nstk_now)

          !--- Shed Leaf Area Index [m2 m-2]
          dlai_shed   = la_lf_shed_phy * nstk_now * tilleragefac / 
     & 1.e4   * min(1.d0, nstk_at_appearance / nstk_now)

          !--- Update dead leaves counter
          n_lf_dead   =   n_lf_dead   + 1

          if(fl_lf_AG_phy)then
              !--- Above ground leaf
                  n_lf_dead_AG    =   n_lf_dead_AG    + 1
              else
              !--- Below ground leaf
                  n_lf_dead_BG    =   n_lf_dead_BG    + 1
              endif

              !--- Leaf is shed, update flag
              fl_shed_leaf    =   .false.

              !--- Update leaf status
              fl_lf_alive(n_lf_alive + 1) = .false.

          endif

            !--- Leaf appearance
            if(fl_appear_leaf)then

            !--- Leaf Area Index Gain [m2 m-2]
            dlai_gain_appear    = phprof(1, 5) * nstk_now * 
     & tilleragefac / 1.e4

              !--- Leaf Dry Weight Gain [ton ha-1]
              ddw_lf_appear       = phprof(1, 6) * nstk_now * 
     & tilleragefac * 1.e4/1.e6

            !--- Leaf appeared, update flag
            fl_appear_leaf = .false.

            endif
        
        !---------------------!
        !--- Sink Strength ---!
        !---------------------!
        
        !--- Phytomer Level
        do phy = 1, n_ph
            
            !--- Check which temperature to use (soil or air) based on phytomer position
            
            if(fl_it_AG(phy))then                
                !--- Above ground
                diphy   = di
                t_mresp = tmed
            else
                !--- Below ground
                diphy   = disoil
                t_mresp = soiltemperature
            endif
            
            !---------------------------!
            !--- Flag of active leaf ---!
            !---------------------------!
            if(phy .le. n_lf_alive)then
                fl_hasleaf  = .true.
            else
                fl_hasleaf  = .false.
            endif
            !---------------------------!
            
            !---------------------------------!
            !--- Flag of visible internode ---!
            !---------------------------------!
            if(phy .gt. n_lf_it_form)then
                fl_it_visible   = .true.
            else
                fl_it_visible   = .false.
            endif
            !---------------------------------!
                        
            if(fl_hasleaf) then
                
                !------------!
                !--- Leaf ---!
                !------------!
                
                !--- Leaf Age [Cdays]
                age_lf_phy      = phprof(phy, 1)
                
                !--- Leaf Dry Weight [g]
                dw_lf_phy       = phprof(phy, 6)
                
                !--- Leaf Initial DW [g]
                ini_dw_lf_phy   = phprof(phy, 9)
            
                !--- Leaf Maintenance respiration [gCH2O]
                mresp_lf    = dw_lf_phy * (kmr_leaf * q10_leaf ** 
     & ((tmed - tref_mr) / 10.))                
                
                !--- Leaf Dry Weight Sink Strength rate [gDW]
                dw_ss_lf    = fgrowth(1, max_lf_dw, ini_dw_lf_phy, 0.,
     & mid_tt_lf_growth, end_tt_lf_growth, age_lf_phy, 1.) * dileaf
            
                !--- Leaf Dry Weight Growth Respiration [gCH2O]
                gresp_lf    = (dw_ss_lf * (1.d0 / (1.d0 - gresp))) -
     & dw_ss_lf
                
                !--- Age rate for each living leaf
                dage_lf_phy    = max(0.d0, dileaf)
                
                !--- Integrate leaf sink strength
                dtlfss = dtlfss + (gresp_lf + mresp_lf + dw_ss_lf)
                
                !--- Update Leaf sink strength rate [gCH2O]
                phprof(phy,2)   = gresp_lf + mresp_lf + dw_ss_lf
                phprof(phy,25)  = mresp_lf                          ! mresp leaf
                phprof(phy,26)  = gresp_lf                          ! gresp leaf
                phprof(phy,27)  = dw_ss_lf                          ! dw ss leaf
                phprof(phy,55)  = dage_lf_phy  
                
                !--- Total Growth/Maintenance Respiration and DW Sink Strength 
                tot_gresp_lf = tot_gresp_lf + gresp_lf  ! [gCH2O]
                tot_mresp_lf = tot_mresp_lf + mresp_lf  ! [gCH2O]
                tot_dw_ss_lf = tot_dw_ss_lf + dw_ss_lf  ! [gDW]
                
            endif
                        
            !-----------------!
            !--- Internode ---!
            !-----------------!
                
            !--- Internode Age [Cdays]
            age_it_phy  = phprof(phy,58)
                
            !--- Internode Dry Weight [g]
            dw_it_phy   = phprof(phy,50)
            
            !--- Maintenance Respiration
            kmr_it_phy      = phprof(phy,11)
            q10_it_phy      = phprof(phy,13)
            tref_mr_it_phy  = tref_mr
            
            !--- Internode Maintenance respiration [gCH2O]
            mresp_it    = dw_it_phy * (kmr_it_phy * q10_it_phy ** 
     & ((t_mresp - tref_mr_it_phy) / 10.))
            dw_ss_it    = 0.d0
            gresp_it    = 0.d0
            dage_it_phy = 0.d0
            
            if(fl_it_visible)then
                
                !--- Actively Growing Internode
                !--- Internode Dry Weight Sink Strength rate [gDW]                
                max_it_dw_phy   = phprof(phy, 14)                
                
                dw_ss_it    = fgrowth(1, max_it_dw_phy, 0., 0., 
     & mid_tt_it_growth, end_tt_it_growth, age_it_phy, 1.) * diphy
            
                !--- Internode Dry Weight Growth Respiration [gCH2O]
                gresp_it    = (dw_ss_it * (1.d0 / (1.d0 - gresp))) - 
     & dw_ss_it
            
                !--- Age rate for each internode
                dage_it_phy = max(0.d0, diphy)
                
            endif
                
            !--- Integrate Internode sink strength                
            if(fl_it_AG(phy))then
                    
                !--- Above Ground
                dtitss_AG = dtitss_AG + (gresp_it + mresp_it + dw_ss_it)
                    
                !--- Total Growth/Maintenance Respiration and DW Sink Strength 
                tot_gresp_it_AG = tot_gresp_it_AG + gresp_it ! [gCH2O]                 
                tot_mresp_it_AG = tot_mresp_it_AG + mresp_it ! [gCH2O]          
                tot_dw_ss_it_AG = tot_dw_ss_it_AG + dw_ss_it ! [gDW]       
                    
            else
                    
                !--- Below Ground
                dtitss_BG = dtitss_BG + (gresp_it + mresp_it + dw_ss_it)
                    
                !--- Total Growth/Maintenance Respiration and DW Sink Strength 
                tot_gresp_it_BG = tot_gresp_it_BG + gresp_it ! [gCH2O]
                tot_mresp_it_BG = tot_mresp_it_BG + mresp_it ! [gCH2O]
                tot_dw_ss_it_BG = tot_dw_ss_it_BG + dw_ss_it ! [gDW]
                    
            endif
            
            !--- Update phytomer profile
            phprof(phy,7)   = gresp_it + mresp_it + dw_ss_it
            phprof(phy,36)  = mresp_it                          ! mresp internode
            phprof(phy,37)  = gresp_it                          ! gresp internode
            phprof(phy,38)  = dw_ss_it                          ! dw ss internode
            phprof(phy,57)  = dage_it_phy                       ! age rate internode            
            phprof(phy,56)  = max(0.d0, diphy)                  ! age rate phytomer
            
        enddo
        
        !------------!
        !--- Root ---!
        !------------!
        
        !--- Roots Dry Weight Sink Strength rate [gDW m-2]
        dw_ss_rt    = fgrowth(1, max_rt_dw, ini_dw_rt, 0., 
     & mid_tt_rt_growth, end_tt_rt_growth, diacsoil, 1.) * disoil
        tot_dw_ss_rt= dw_ss_rt
        
        !--- Roots Dry Weight Growth Respiration [gCH2O m-2]
        gresp_rt     = (dw_ss_rt * (1.d0 / (1.d0 - gresp))) - dw_ss_rt
        tot_gresp_rt = gresp_rt
            
        !--- Roots Maintenance respiration [gCH2O m-2]
        mresp_rt     = (dw_rt * 1.e6/1.e4) * (kmr_root * q10_root ** 
     & ((soiltemperature - tref_mr) / 10.))
        tot_mresp_rt = mresp_rt
        
        !--- Total Roots sink strength [gCH2O m-2]
        dtrtss      = gresp_rt + mresp_rt + dw_ss_rt
                    
        !--- Use the initial plant population to scale
        !--- Upscale total substrates needed for leaf and internodes growth [gCH2O m-2] (Tiller Age factor added to account for different shoot ages in the same area)
        dtlfss      = dtlfss                    * nstk_now* tilleragefac
        dtitss      = (dtitss_BG + dtitss_AG)   * nstk_now* tilleragefac
        dtitss_BG   = dtitss_BG                 * nstk_now* tilleragefac
        dtitss_AG   = dtitss_AG                 * nstk_now* tilleragefac
        
        !--- Upscale total substrates needed for leaf and internodes growth/maintenance respiration and DW [gCH2O m-2, gDW m-2] (Tiller Age factor added to account for different shoot ages in the same area)
        tot_gresp_lf    = tot_gresp_lf          * nstk_now *tilleragefac
        tot_mresp_lf    = tot_mresp_lf          * nstk_now *tilleragefac
        tot_gresp_it_AG = tot_gresp_it_AG       * nstk_now *tilleragefac ! Above Ground
        tot_mresp_it_AG = tot_mresp_it_AG       * nstk_now *tilleragefac ! Above Ground       
        tot_gresp_it_BG = tot_gresp_it_BG       * nstk_now *tilleragefac ! Below Ground
        tot_mresp_it_BG = tot_mresp_it_BG       * nstk_now *tilleragefac ! Below Ground
        tot_dw_ss_lf    = tot_dw_ss_lf          * nstk_now *tilleragefac
        tot_dw_ss_it_AG = tot_dw_ss_it_AG       * nstk_now *tilleragefac
        tot_dw_ss_it_BG = tot_dw_ss_it_BG       * nstk_now *tilleragefac        
        
        !--- Total internodes sink strengths
        tot_gresp_it    = tot_gresp_it_AG + tot_gresp_it_BG
        tot_mresp_it    = tot_mresp_it_AG + tot_mresp_it_BG
        tot_dw_ss_it    = tot_dw_ss_it_AG + tot_dw_ss_it_BG
        
        !--------------------------!
        !--- Crop Sink Strength ---!
        !--------------------------!
        
        !--- Total substrates needed for crop growth [gCH2O m-2]
        dtcrss = dtitss + dtlfss + dtrtss        
        
        !--- Total substrates needed for crop growth/maintenance respiration [gCH2O m-2]
        tot_gresp_crop = (tot_gresp_it_AG + tot_gresp_it_BG) + 
     & tot_gresp_lf + tot_gresp_rt
        tot_mresp_crop = (tot_mresp_it_AG + tot_mresp_it_BG) + 
     & tot_mresp_lf + tot_mresp_rt
        tot_dw_ss_crop = (tot_dw_ss_it_AG + tot_dw_ss_it_BG) + 
     & tot_dw_ss_lf + tot_dw_ss_rt
        
        !------------------------------!
        !--- Relative Sink Strength ---!
        !------------------------------!
        
        !--- Daily Relative Sink Strength by organ pool
        if(dtcrss .gt. 0.000000d0)then !To avoid NaN
            dr_rtss =   dtrtss / dtcrss
            dr_lfss =   dtlfss / dtcrss
            dr_itss =   dtitss / dtcrss
        else
            dr_rtss =   0.d0
            dr_lfss =   0.d0
            dr_itss =   0.d0
        endif
        
        !--- Carbon Balance Check
        if(dtcrss .gt. 0.000000d0)then
            if((dr_rtss + dr_lfss + dr_itss) .lt. 1.d0)then            
                !--- Send to roots
                dr_rtss = dr_rtss + 1.d0 - (dr_rtss + dr_lfss + dr_itss)
            else if((dr_rtss + dr_lfss + dr_itss) .gt. 1.d0)then        
                !--- Reduce roots pf
                dr_rtss = dr_rtss - ((dr_rtss + dr_lfss + dr_itss) - 
     & 1.d0)
            endif
        endif
                                
        !--- Below ground fraction of growing internodes (considering leaf profile as the growing phytomers)
        if(dtitss_AG .gt. 0.d0) frac_AG = max(0.d0, min(1.d0, 
     & dtitss_AG/dtitss))
        if(dtitss_BG .gt. 0.d0) frac_BG = max(0.d0, min(1.d0, 
     & dtitss_BG/dtitss))
        
    !-------------------!
    !--- CROP GROWTH ---!
    !-------------------!    
    
      if(.not. flemerged)then
        
        !----------------------------------------------------------------!
        !--- Available CH2O for Growth & Maintenance Before Emergence ---!
        !----------------------------------------------------------------!
        
        !--- Available substrates for crop growth and maintenance [tonCH2O ha-1]
        subsres             = sug_it_BG             ! Use only below ground sugar pool      [ton ha-1]
        
        !--- CO2 Assimilation rate 
        dtg                 = 0.d0                  ! No CO2 assimilation before emergence  [ton ha-1]
                
      else
    
        !----------------------------------------------------------------!
        !--- Available CH2O for Growth & Maintenance After Emergence ---!
        !----------------------------------------------------------------!
        
        !--- Available substrates for crop growth and maintenance [tonCH2O ha-1]
        subsres             = sug_it_BG    ! Use only below ground sugar pool      [ton ha-1]        
        
        !----------------------!
        !--- Photosynthesis ---!
        !----------------------!
        
        !--- Select among photosynthesis methods
        select case(metpg)
        
        case(1)
            
            !------------------!
            !--- RUE method ---!
            !------------------!
            
            !--- WARNING: This method do not take into account Growth + Maintenance Respiration. The reason is because most (all to date) of RUE values found in literature
            !--- are computed based on dry biomass ~ IPAR. In other words, when dry biomass is weighted part of maintenance respiration is lost in the transition time of sampling-weighting
            !--- AND the most important: the growth respiration has already gone, otherwise we couldnt be weighting anything!
            
            !--- Reduced Radiation Use Efficiency [gDW/MJPAR/m2]
            rue_mod = rue * agefactor_rue * tempfac_pho * 
     & pho_fac_co2 * swfacp
            
            !--- Carbon Gain [gDW/m2]
            dtg     = rue_mod * li * par_rad
            
            !--- Convert to ton ha-1
            dtg     =   dtg * (1.e4/1.e6)
        
        case(2)
            
            !----------------------------------------!
            !--- CO2 Assimilation by Canopy Layer ---!
            !----------------------------------------!
            
            !--- Astrological calculations for difuse and direct PAR interception
            call astro(logf,swscre,doy,lat,dayl,daylp,
     & sinld,cosld,dsinb,dsinbe,dso)
                        
            !--- Convert CO2 Assimilation rate to kgCO2 ha-1 h-1
            amax_conv   = amax / 1.e3 * 1.e4 * 3600 * 44.d0 / 1.e6
            
            !--- Convert Quantum Efficiency to kgCO2 ha-1 h-1 (J m-2 s-1)-1
            eff_conv    = eff / 1.e3 * 1.e4 * 3600 * 44.d0 / 1.e6 * 4.6
            
            !--- Reduced Maximum Assimilation Rate [mmol m-2 s-1]
            !--- Here we assume temperature, water stress and feedback response mainly affect the maximum assimilation rate, rather than quantum eff.
            amax_mod = amax_conv * tempfac_pho * swfacp * 
     & amaxfbfac * pho_fac_co2
            eff_mod  = eff_conv
            
            !--- LAI for assimilation
            lai_ass  = lai
                        
            !--- Total assimilation for three canopy layers on hourly-step (Gaussian Integration) - Groudriaan
            call totass_samuca(doy,                
     & dayl,               
     & amax_mod,           
     & eff_mod,            
     & lai_ass,            
     & kdif,               
     & c_scattering,       
     & srad * 1.e6,         ! Solar Radiation [J/m2/day] - PAR [W/m2] is computed within radiat()
     & sinld,              
     & cosld,              
     & dtg,                 ! Output
     & Acanopy,             ! Output
     & Qleaf,               ! Output
     & incpar,              ! Output
     & photo_layer_act,     ! Output
     & frac_li)             ! Output  
            
            !--- Convert CO2 assimilation to CH2O assimilation rate [kgCH2O ha-1] (stoichiometric conversion)
            dtg             = dtg               * 30.d0/44.d0
            photo_layer_act = photo_layer_act   * 30.d0/44.d0
            
            !--- Convert to ton ha-1
            dtg             = dtg               * 1.e-3
            photo_layer_act = photo_layer_act   * 1.e-3
            
        case(3)
            
            !-----------------------------------!
            !--- Canopy Gross Photosynthesis ---!
            !-----------------------------------!
            
            !--- LAI for assimilation
            lai_ass  = lai
            
            !--- Canopy gross photosysntesis rate
            call PGS(swfacp,1.,1.,1.,chustk,par_rad,lai_ass,dtg,resp,
     & diac,tmed,dw_total,CCEFF,CCMAX,k_can,PHTMAX,CCMP,PARMAX)
            
            !--- Growth and Maintenance respiration (computed on PGS subroutine)
            dtg = max(0.d0,dtg)
           
        end  select    
      endif
    
      !----------------------!
      !--- Carbon Balance ---!
      !----------------------!

      !--- Available CH2O for leaves, internodes and roots [g m-2]
      dtg_avail_rt        = dtg       * dr_rtss * (1.e6/1.e4)
      dtg_avail_lf        = dtg       * dr_lfss * (1.e6/1.e4)
      dtg_avail_it        = dtg       * dr_itss * (1.e6/1.e4)
      dtg_avail_it_BG     = dtg       * dr_itss * (1.e6/1.e4) * frac_BG
      dtg_avail_it_AG     = dtg       * dr_itss * (1.e6/1.e4) * frac_AG

      !--- Available CH2O reserves in case its needed [g m-2]
      subsres_avail_rt    = subsres   * dr_rtss * (1.e6/1.e4)
      subsres_avail_lf    = subsres   * dr_lfss * (1.e6/1.e4)
      subsres_avail_it    = subsres   * dr_itss * (1.e6/1.e4)
      subsres_avail_it_BG = subsres   * dr_itss * (1.e6/1.e4) * frac_BG
      subsres_avail_it_AG = subsres   * dr_itss * (1.e6/1.e4) * frac_AG
        
    !---  Crop will use its reserves to growth before emergence (searching for light!) [ton ha-1]        
      if(fl_use_reserves)then
            
        !------------------------------!
        !--- Use reserves to growth ---!
        !------------------------------!
        
        !--- Total substrates available to growth [g m-2]
        subs_avail_growth_crop  = (dtg + subsres) * (1.e6/1.e4)
        
        !--- This condition will happen before emergence [g m-2]
        subs_avail_growth_rt    = dtg_avail_rt  	+subsres_avail_rt
        subs_avail_growth_lf    = dtg_avail_lf    +subsres_avail_lf
        subs_avail_growth_it    = dtg_avail_it    +subsres_avail_it
        subs_avail_growth_it_BG = dtg_avail_it_BG +subsres_avail_it_BG
        subs_avail_growth_it_AG = dtg_avail_it_AG +subsres_avail_it_AG
            
        !--- Carbon balance
        subsres_avail_rt    = 0.d0
        subsres_avail_lf    = 0.d0
        subsres_avail_it    = 0.d0
        subsres_avail_it_BG = 0.d0
        subsres_avail_it_AG = 0.d0
            
      else
            
        !-------------------------------------------!
        !--- Use only CO2 assimilation to growth ---!
        !-------------------------------------------!
        
        !--- Total substrates available to growth [g m-2]
        subs_avail_growth_crop  = dtg * (1.e6/1.e4)
        
        !--- Still the reserves will be used for maintenance when renecessary [g m-2]
        subs_avail_growth_rt    = dtg_avail_rt  
        subs_avail_growth_lf    = dtg_avail_lf	
        subs_avail_growth_it    = dtg_avail_it   
        subs_avail_growth_it_BG = dtg_avail_it_BG
        subs_avail_growth_it_AG = dtg_avail_it_AG
            
      endif
    
      !------------------------------!
      !--- Crop Source-Sink Ratio ---!
      !------------------------------!

      if(dtcrss .gt. 0.d0) cr_source_sink_ratio = subs_avail_growth_crop
     &  / dtcrss

      !------------!
      !--- Leaf ---!
      !------------!
      call subs_balance(  subs_avail_growth_lf,    ! Input
     & subsres_avail_lf,        ! Input
     & dtlfss,                  ! Input
     & tot_mresp_lf,            ! Input
     & tot_gresp_lf,            ! Input
     & tot_dw_ss_lf,            ! Input
     & sup_ratio_lf,            ! Output
     & supply_rate_lf,          ! Output
     & supply_used_lf,          ! Output
     & supply_used_mresp_lf,    ! Output
     & supply_used_gresp_lf,    ! Output
     & supply_used_dw_lf,       ! Output
     & reserves_used_mresp_lf,  ! Output
     & maintenance_factor_lf,   ! Output
     & reduc_growth_factor_lf)  ! Output
        
        !------------!
        !--- Stem ---!
        !------------!
        call subs_balance(  subs_avail_growth_it,    ! Input
     & subsres_avail_it,        ! Input
     & dtitss,                  ! Input
     & tot_mresp_it,            ! Input
     & tot_gresp_it,            ! Input
     & tot_dw_ss_it,            ! Input
     & sup_ratio_it,            ! Output
     & supply_rate_it,          ! Output
     & supply_used_it,          ! Output
     & supply_used_mresp_it,    ! Output
     & supply_used_gresp_it,    ! Output
     & supply_used_dw_it,       ! Output
     & reserves_used_mresp_it,  ! Output
     & maintenance_factor_it,   ! Output
     & reduc_growth_factor_it)  ! Output
        
        !-------------------------------!
        !--- Below ground Internodes ---!
        !-------------------------------!
        call subs_balance(subs_avail_growth_it_BG,       ! Input
     & subsres_avail_it_BG,         ! Input
     & dtitss_BG,                   ! Input
     & tot_mresp_it_BG,             ! Input
     & tot_gresp_it_BG,             ! Input
     & tot_dw_ss_it_BG,             ! Input
     & sup_ratio_it_BG,             ! Output
     & supply_rate_it_BG,           ! Output
     & supply_used_it_BG,           ! Output
     & supply_used_mresp_it_BG,     ! Output
     & supply_used_gresp_it_BG,     ! Output
     & supply_used_dw_it_BG,        ! Output
     & reserves_used_mresp_it_BG,   ! Output
     & maintenance_factor_it_BG,    ! Output
     & reduc_growth_factor_it_BG)   ! Output
        
        !-------------------------------!
        !--- Above ground Internodes ---!
        !-------------------------------!
        call subs_balance(  subs_avail_growth_it_AG,     ! Input
     & subsres_avail_it_AG,         ! Input
     & dtitss_AG,                   ! Input
     & tot_mresp_it_AG,             ! Input
     & tot_gresp_it_AG,             ! Input
     & tot_dw_ss_it_AG,             ! Input
     & sup_ratio_it_AG,             ! Output
     & supply_rate_it_AG,           ! Output
     & supply_used_it_AG,           ! Output
     & supply_used_mresp_it_AG,     ! Output
     & supply_used_gresp_it_AG,     ! Output
     & supply_used_dw_it_AG,        ! Output
     & reserves_used_mresp_it_AG,   ! Output
     & maintenance_factor_it_AG,    ! Output
     & reduc_growth_factor_it_AG)   ! Output
        
        !-------------!
        !--- Roots ---!
        !-------------!
        call subs_balance(  subs_avail_growth_rt,    ! Input
     & subsres_avail_rt,        ! Input
     & dtrtss,                  ! Input
     & tot_mresp_rt,            ! Input
     & tot_gresp_rt,            ! Input
     & tot_dw_ss_rt,            ! Input
     & sup_ratio_rt,            ! Output
     & supply_rate_rt,          ! Output
     & supply_used_rt,          ! Output
     & supply_used_mresp_rt,    ! Output
     & supply_used_gresp_rt,    ! Output
     & supply_used_dw_rt,       ! Output
     & reserves_used_mresp_rt,  ! Output
     & maintenance_factor_rt,   ! Output
     & reduc_growth_factor_rt)  ! Output
    
        !--- Overall Crop Carbon Balance
        supply_used_crop  = supply_used_rt + supply_used_it + 
     & supply_used_lf
        supply_used_mresp_crop	     = supply_used_mresp_rt	   	+ 	
     & supply_used_mresp_it   	+	 supply_used_mresp_lf	   
        supply_used_gresp_crop	     = supply_used_gresp_rt	   	+ 	
     & supply_used_gresp_it   	+	 supply_used_gresp_lf	   
        supply_used_dw_crop	         = supply_used_dw_rt	   	+ 	
     & supply_used_dw_it	   	+	 supply_used_dw_lf	  
        reserves_used_mresp_crop	 = reserves_used_mresp_rt  	+ 	
     & reserves_used_mresp_it 	+	 reserves_used_mresp_lf
        maintenance_factor_crop	     = (maintenance_factor_rt  * 
     & dw_rt 		+	maintenance_factor_it  * dw_it  + 	 
     & maintenance_factor_lf  * dw_lf) / dw_total
        reduc_growth_factor_crop	 = (reduc_growth_factor_rt * 
     & dw_rt  	+	reduc_growth_factor_it * dw_it  + 	 
     & reduc_growth_factor_lf * dw_lf) / dw_total
        
        !--- Downscale to organ level to find structural and sugar partitioning factors
        dstr_it_BG  = 0.d0
        dsug_it_BG  = 0.d0
        dstr_it_AG  = 0.d0
        dsug_it_AG  = 0.d0
        
        !--- Total substrates available for leaves and internodes at the "reference stalk" level
        !--- Note that at below ground conditions tilleragefac should be 1.d0 (keep it here for debugging purpose)
        subs_avail_growth_lf_ref_till       = subs_avail_growth_lf    
     &   / (nstk_now  * tilleragefac)
        subs_avail_growth_it_ref_till       = subs_avail_growth_it    
     &   / (nstk_now  * tilleragefac)
        subs_avail_growth_it_BG_ref_till    = subs_avail_growth_it_BG 
     &   / (nstk_now  * tilleragefac)
        subs_avail_growth_it_AG_ref_till    = subs_avail_growth_it_AG 
     &   / (nstk_now  * tilleragefac)

        !--- Total substrates reserves in case needed for leaves and internodes at the "reference stalk" level
        !--- Note that all reserves are available for growth and respiration in (avail_subs_crop)
        subsres_avail_lf_ref_till           = subsres_avail_lf       
     &    / (nstk_now  * tilleragefac)
        subsres_avail_it_ref_till           = subsres_avail_it       
     &    / (nstk_now  * tilleragefac)
        subsres_avail_it_BG_ref_till        = subsres_avail_it_BG    
     &    / (nstk_now  * tilleragefac)
        subsres_avail_it_AG_ref_till        = subsres_avail_it_AG    
     &    / (nstk_now  * tilleragefac)

        !--- Total sink strength of all leaves and internodes
        dtlfss_ref_till             = dtlfss        / (nstk_now  * 
     & tilleragefac)
        dtitss_ref_till             = dtitss        / (nstk_now  * 
     & tilleragefac)
        dtitss_BG_ref_till          = dtitss_BG     / (nstk_now  * 
     & tilleragefac)
        dtitss_AG_ref_till          = dtitss_AG     / (nstk_now  * 
     & tilleragefac)
    
      !----------------------!
      !--- Phytomer Level ---!
      !----------------------!
      do phy = 1, n_ph
            
        !--- Check which temperature to use (soil or air) based on phytomer position
        if(fl_it_AG(phy))then                
            !--- Above ground
            diphy   = di
            t_mresp = tmed
        else
            !--- Below ground
            diphy   = disoil
            t_mresp = soiltemperature
        endif
            
        !----------------------!
        !--- Phytomer Flags ---!
        !----------------------!
            
        !--- Has an active leaf        
        if(phy .le. n_lf_alive)then
            fl_hasleaf = .true.
        else
            fl_hasleaf = .false.
        endif
            
        !--- Has a growing internode
        if(phy .gt. n_lf_it_form)then
            fl_it_visible   = .true.
        else
            fl_it_visible   = .false.
        endif
                        
        if(fl_hasleaf)then
                
            !------------!
            !--- Leaf ---!
            !------------!
                
            !--- Initial sink strengths of leaf
            dtlfss_phy      = phprof(phy,2)
            mresp_lf_phy    = phprof(phy,25)
            gresp_lf_phy    = phprof(phy,26)
            dw_ss_lf_phy    = phprof(phy,27)
                
            !--- Relative Sink Strength                
            if(dtlfss_ref_till .gt. 0.d0) rel_ss_lf_phy    = dtlfss_phy
     &  / dtlfss_ref_till
            
            !--- Substrates available for this leaf
            subs_avail_growth_lf_phy    = subs_avail_growth_lf_ref_till
     &  * rel_ss_lf_phy
                
            !--- Reserves available for this leaf in case needed
            subsres_avail_lf_phy        = subsres_avail_lf_ref_till *
     &  rel_ss_lf_phy
                
            !--- Leaf Growth
            call subs_balance(  subs_avail_growth_lf_phy,   ! Input
     & subsres_avail_lf_phy,        ! Input
     & dtlfss_phy,                  ! Input
     & mresp_lf_phy,                ! Input
     & gresp_lf_phy,                ! Input
     & dw_ss_lf_phy,                ! Input
     & sup_ratio_lf_phy,            ! Output
     & supply_rate_lf_phy,          ! Output
     & supply_used_lf_phy,          ! Output
     & supply_used_mresp_lf_phy,    ! Output
     & supply_used_gresp_lf_phy,    ! Output
     & supply_used_dw_lf_phy,       ! Output
     & reserves_used_mresp_lf_phy,  ! Output
     & maintenance_factor_lf_phy,   ! Output
     & reduc_growth_factor_lf_phy)  ! Output          
                
            !--- Update phytomer profile rates
            phprof(phy,28)  = sup_ratio_lf_phy
            phprof(phy,29)  = supply_rate_lf_phy
            phprof(phy,30)  = supply_used_lf_phy
            phprof(phy,31)  = supply_used_mresp_lf_phy
            phprof(phy,32)  = supply_used_gresp_lf_phy
            phprof(phy,33)  = supply_used_dw_lf_phy
            phprof(phy,34)  = maintenance_factor_lf_phy
            phprof(phy,35)  = reduc_growth_factor_lf_phy
                
            !--- Leaf Blade Area Gain (cm2)
            if(flemerged)then
                if(fl_lf_AG(phy))then
                    dla_phy             = supply_used_dw_lf_phy  * sla  
                    phprof(phy, 4)      = dla_phy
                    dla_gain_ref_till   = dla_gain_ref_till +  dla_phy
                endif
            endif                
        endif
                
        !-----------------!
        !--- Internode ---!
        !-----------------!
                
        !--- Initial sink strengths of internode [gCH2O]                
        dtitss_phy      = phprof(phy,7)
        mresp_it_phy    = phprof(phy,36)
        gresp_it_phy    = phprof(phy,37)
        dw_ss_it_phy    = phprof(phy,38)
                
        if(fl_it_AG(phy))then
                
            !--- Above Ground
            
            !--- Relative Sink Strength [0-1]
            if(dtitss_AG_ref_till .gt. 0.d0) rel_ss_it_phy    = 
     & dtitss_phy / dtitss_AG_ref_till
                 
            !--- Substrates available for internode growth [g]
            subs_avail_growth_it_phy = 
     & subs_avail_growth_it_AG_ref_till * rel_ss_it_phy
                
            !--- Reserves available for this leaf in case needed
            subsres_avail_it_phy    = 
     & subsres_avail_it_AG_ref_till * rel_ss_it_phy
                
        else
            
            !--- Below Ground
            
            !--- Relative Sink Strength [0-1]
            if(dtitss_BG_ref_till .gt. 0.d0) rel_ss_it_phy    = 
     & dtitss_phy / dtitss_BG_ref_till
                 
            !--- Substrates available for internode growth [g]
            subs_avail_growth_it_phy = 
     & subs_avail_growth_it_BG_ref_till * rel_ss_it_phy
                
            !--- Reserves available for this leaf in case needed
            subsres_avail_it_phy    = 
     & subsres_avail_it_BG_ref_till * rel_ss_it_phy
                
        endif            
            
        !--- Internode Growth [g]            
        call subs_balance(  subs_avail_growth_it_phy,    ! Input
     & subsres_avail_it_phy,        ! Input
     & dtitss_phy,                  ! Input
     & mresp_it_phy,                ! Input
     & gresp_it_phy,                ! Input
     & dw_ss_it_phy,                ! Input
     & sup_ratio_it_phy,            ! Output
     & supply_rate_it_phy,          ! Output
     & supply_used_it_phy,          ! Output
     & supply_used_mresp_it_phy,    ! Output
     & supply_used_gresp_it_phy,    ! Output
     & supply_used_dw_it_phy,       ! Output
     & reserves_used_mresp_it_phy,  ! Output
     & maintenance_factor_it_phy,   ! Output
     & reduc_growth_factor_it_phy)  ! Output
            
        !--- Internode Age [Cdays]
        age_it_phy      = phprof(phy,58)
                
        !--- Structural partitioning factor [0-1]
        it_struc_pfac_rate = it_struc_pfac( it_struc_tb_ini,            
     & it_struc_to1,               
     & it_struc_to2,               
     & it_struc_tb_end,            
     & it_struc_pfac_temp_max_red, 
     & it_struc_pfac_wate_max_red, 
     & it_struc_pfac_max,          
     & it_struc_pfac_min,          
     & it_struc_pfac_tb,           
     & it_struc_pfac_tm,           
     & it_struc_pfac_te,           
     & it_struc_pfac_delta,        
     & age_it_phy,                 
     & t_mresp,                    
     & swfacf)                     ! From SOPLAT
            
        !--- Total Sugars and Structural rate [g]
        dstr_it_phy  = it_struc_pfac_rate           *   
     & supply_used_dw_it_phy
        dsug_it_phy  = (1.d0 - it_struc_pfac_rate)  *   
     & supply_used_dw_it_phy
            
        !--- Update phytomer profile
        phprof(phy,39)  = sup_ratio_it_phy
        phprof(phy,40)  = supply_rate_it_phy
        phprof(phy,41)  = supply_used_it_phy
        phprof(phy,42)  = supply_used_mresp_it_phy
        phprof(phy,43)  = supply_used_gresp_it_phy
        phprof(phy,44)  = supply_used_dw_it_phy
        phprof(phy,45)  = maintenance_factor_it_phy
        phprof(phy,46)  = reduc_growth_factor_it_phy
        phprof(phy,47)  = supply_used_dw_it_phy
        phprof(phy,48)  = dstr_it_phy
        phprof(phy,49)  = dsug_it_phy
        phprof(phy,15)  = it_struc_pfac_rate
                    
        !--- Integrate structural and sugar parts rates
        if(fl_it_AG(phy))then
                    
            !--- Above Ground
            dstr_it_AG  = dstr_it_AG    +   dstr_it_phy
            dsug_it_AG  = dsug_it_AG    +   dsug_it_phy    
                    
        else
                    
            !--- Below Ground
            dstr_it_BG  = dstr_it_BG    +   dstr_it_phy
            dsug_it_BG  = dsug_it_BG    +   dsug_it_phy
                    
        endif                   
      enddo
        
        !--- Upscale Structural and Sugar Rates to Field Level [g m-2]
        dstr_it_BG  = dstr_it_BG	* nstk_now * tilleragefac
        dsug_it_BG  = dsug_it_BG	* nstk_now * tilleragefac
        dstr_it_AG  = dstr_it_AG	* nstk_now * tilleragefac
        dsug_it_AG  = dsug_it_AG	* nstk_now * tilleragefac
            
        !--- Correction factor to meet carbon balance
        dsug_corr_fac_BG   = 1.d0
        dsug_corr_fac_AG   = 1.d0    
        if((dstr_it_BG + dsug_it_BG) .gt. 0.000000d0 .and. 
     & supply_used_dw_it_BG .gt. 0.d0) dsug_corr_fac_BG   = 
     & supply_used_dw_it_BG / (dstr_it_BG + dsug_it_BG)
        if((dstr_it_AG + dsug_it_AG) .gt. 0.000000d0 .and. 
     & supply_used_dw_it_AG .gt. 0.d0) dsug_corr_fac_AG   = 
     & supply_used_dw_it_AG / (dstr_it_AG + dsug_it_AG)
        
        !--- Check carbon balance discrepancy
        c_check_tol = 0.10
        if(abs(1.d0 - dsug_corr_fac_BG) .gt. c_check_tol)then
            write(warn,*) 'More than 10% Carbon Balance Discrepancy'//
     & ' on Internode Below Ground Sugar/Structural Partitioning'//
     & ' at DAS: ',das,
     & 'Discrepancy of ', abs(1.d0 - dsug_corr_fac_BG) * 100.d0,
     &  '% was corrected.'
        endif
        
        c_check_tol = 0.10
        if(abs(1.d0 - dsug_corr_fac_AG) .gt. c_check_tol)then
            write(warn,*) 'More than 10% Carbon Balance Discrepancy'//
     & ' on Internode Above Ground Sugar/Structural Partitioning'//
     & ' at DAS: ',das,
     & 'Discrepancy of ', abs(1.d0 - dsug_corr_fac_AG) * 100.d0,
     &  '% was corrected.'
        endif  
        
        !--- Correct to meet carbon balance
        dstr_it_BG  = dstr_it_BG  *   dsug_corr_fac_BG
        dsug_it_BG  = dsug_it_BG	*   dsug_corr_fac_BG
        
        !--- Biomass Rates [ton ha-1]
        ddw_rt      = supply_used_dw_rt     * (1.e4/1.e6)
        ddw_lf      = supply_used_dw_lf     * (1.e4/1.e6)
        ddw_it      = supply_used_dw_it     * (1.e4/1.e6)
        ddw_it_BG   = supply_used_dw_it_BG  * (1.e4/1.e6)        
        ddw_it_AG   = supply_used_dw_it_AG  * (1.e4/1.e6)
        dstr_it_BG  = dstr_it_BG            * (1.e4/1.e6)
        dsug_it_BG  = dsug_it_BG	        * (1.e4/1.e6)
        dstr_it_AG  = dstr_it_AG	        * (1.e4/1.e6)
        dsug_it_AG  = dsug_it_AG	        * (1.e4/1.e6)
    
        !--- Leaf Area Index Gain [m2 m-2]
        dlai_gain   = dla_gain_ref_till     * 
     & (nstk_now * tilleragefac) / 1.e4
        
        !--- Exceeded Substrates [tonCH2O ha-1]
        if(dtg_avail_lf .gt. dtlfss) exc_dtg_lf = 
     & (dtg_avail_lf - dtlfss) * (1.e4/1.e6)
        if(dtg_avail_it .gt. dtitss) exc_dtg_it = 
     & (dtg_avail_it - dtitss) * (1.e4/1.e6)
        if(dtg_avail_rt .gt. dtrtss) exc_dtg_rt = 
     & (dtg_avail_rt - dtrtss) * (1.e4/1.e6)
        
        !--- Net Rate of Reserves
        if(fl_use_reserves)then
            
            if(.not. dtg_avail_lf .gt. dtlfss) 
     & reserves_used_growth_lf = supply_used_lf - dtg_avail_lf
            if(.not. dtg_avail_it .gt. dtitss) 
     & reserves_used_growth_it = supply_used_it - dtg_avail_it
            if(.not. dtg_avail_rt .gt. dtrtss) 
     & reserves_used_growth_rt = supply_used_rt - dtg_avail_rt
            
        else
            reserves_used_growth_lf = 0.d0
            reserves_used_growth_it = 0.d0
            reserves_used_growth_rt = 0.d0               
        endif    
        
    !--- Substrate reserves balance
        dsubsres_lf     = exc_dtg_lf - reserves_used_mresp_lf * 
     & (1.e4/1.e6) - reserves_used_growth_lf * (1.e4/1.e6)
        dsubsres_it     = exc_dtg_it - reserves_used_mresp_it * 
     & (1.e4/1.e6) - reserves_used_growth_it * (1.e4/1.e6)
        dsubsres_rt     = exc_dtg_rt - reserves_used_mresp_rt * 
     & (1.e4/1.e6) - reserves_used_growth_rt * (1.e4/1.e6)
        dsubsres        = dsubsres_lf + dsubsres_it + dsubsres_rt
    
        !--- Ratio Reserves Variation    
        dsubsres_ratio  = 1.d0
        if(subsres .gt. 0.d0) dsubsres_ratio  = (subsres + dsubsres) 
     & / subsres
    
        !-------------------!
        !--- Stalk Rates ---!
        !-------------------!
        if(flemerged)then
        
            !--- Above ground conditions
            if(fl_stalk_emerged)then
            
                !-------------------------------------------!
                !--- Stalk has emerged from soil surface ---!
                !-------------------------------------------!
            
                !--- Plant extension rate
                do hour = 1, 24            
                    !--- Hourly plant extension rate [mm h-1]
			        per_hour = dpercoeff * tempfac_h_per(hour) * swface * 
     & agefactor_per
			
                    !--- Integrate to daily rate [mm day-1]
			        per = per + per_hour	        
                enddo
        
                !--- Fraction of stalk extension partitioned among internodes as function of structural gain        
                dtot_str_dw_ref_till = sum(phprof(1:n_ph,15))
            
                if(dtot_str_dw_ref_till .gt. 0.d0)then
                    !--- Only extends when structural gain is positive
                    do phy = 1, n_ph
                        !--- Internode extension rate [mm]
                        per_it_phy     = min(max_per_it, per * 
     & (phprof(phy,15) / dtot_str_dw_ref_till)) ! dLength [mm]      
                        phprof(phy,23) = per_it_phy ! dLength [mm]      
                    enddo
                endif
                        
                !--------------------------------!
                !--- Water Fraction in Stalks ---!
                !--------------------------------!
            
                !--- Computing water fraction rate of Stalks for Stalk Fresh Mass [Fresh Cane Yield: ton ha-1]
                !--- Following Martines SASTA 2001 and Mathew Jones (CANEGRO)
                dwat_it_AG = (dswat_ddws * ddw_it_AG) - 
     & (dswat_dsuc * dsug_it_AG)              
        
            endif
        
        else
        
            !--- Crop is still below the ground surface        
            !--- Shoot extension towards soil surface [cm]
            dshootext_BG    =   dshootext_BG_rate     *   disoil
        
        endif
        
    !-------------------!
    !--- Root Growth ---!
    !-------------------!
        
        !--- Root Front velocity [cm day-1]
        drdepth = rootdrate * disoil 
        
        !--- Number of sublayers for root profile rate
        nsublay = aint(dep(numlay))
    
        !--- Root Profile Rate (RLD and Biomass)
        call root_profile(numlay,   ! Input
     & nsublay,                   ! Input
     & initcropdepth,             ! Input
     & rpup,                      ! Input
     & effective_rd,              ! Input
     & root_front_size,           ! Input
     & srlmax,                    ! Input
     & srlmin,                    ! Input
     & bottom,                    ! Input
     & slthickness,               ! Input
     & ddw_rt,                    ! Input
     & flemerged,                 ! Input
     & drld,                      ! Output
     & ddw_rt_sl)                 ! Output
       
        !--- Root Senescence (PIT)
        ddw_rt_dead = 0.d0
        drld_dead   = 0.d0
    
    !------------------------!
    !--- Step Integration ---!
    !------------------------!
    
        !--- Phytomer Level Integration    
        do phy   = 1, n_ph
    
            !--- Integrate phytomer attributes
            phprof(phy, 1)  = phprof(phy, 1) + phprof(phy,55) ! Leaf Age                    [Cdays]
            phprof(phy,58)  = phprof(phy,58) + phprof(phy,57) ! Internode Age               [Cdays]
            phprof(phy,12)  = phprof(phy,12) + phprof(phy,56) ! Phytomer Age                [Cdays]
            phprof(phy, 6)  = phprof(phy, 6) + phprof(phy,33) ! Leaf Dry Weight             [g]
            phprof(phy, 5)  = phprof(phy, 5) + phprof(phy, 4) ! Leaf Area                   [cm2]
            phprof(phy,16)  = phprof(phy,16) + phprof(phy,23) ! Internode length            [mm]
                
            if(.not. fl_it_AG(phy))then
            
                !--- Carbon balance of growth
                dsug_it_phy_growth      = phprof(phy,49)
                dstr_it_phy_growth      = phprof(phy,48)
                ddw_it_phy_growth       = phprof(phy,47)
            
                !--- Carbon balance of reserves
                dsug_it_phy_reserves    = max(0.d0, (phprof(phy,52)  * 
     &   dsubsres_ratio) - phprof(phy,52))
                ddw_it_phy_reserves     = dsug_it_phy_reserves
            
                !--- Current State
                dw_it_phy               = phprof(phy,50)
                str_it_phy              = phprof(phy,51)  
                sug_it_phy              = phprof(phy,52)
            
                !--- Integrate Total Dry Weight and Sugars [g]
                dw_it_phy      = dw_it_phy  + ddw_it_phy_growth     + 
     &   ddw_it_phy_reserves
                str_it_phy     = str_it_phy + dstr_it_phy_growth 
                sug_it_phy     = sug_it_phy + dsug_it_phy_growth    + 
     &   dsug_it_phy_reserves
            
                !--- Update internode profile
                phprof(phy,50)  = dw_it_phy  ! Internode Dry Weight        [g]
                phprof(phy,51)  = str_it_phy ! Internode Structural Weight [g]
                phprof(phy,52)  = sug_it_phy ! Internode Sugars Weight     [g]
            
                !--- Sucrose and Hexose Contents at below ground internodes [g]
                !--- 50% shared due to unkown
                suc_it_phy  =   frac_suc_BG   *   sug_it_phy   
                hex_it_phy  =   frac_hex_BG   *   sug_it_phy
            
                 !--- Below Ground
                suc_it_BG_ref_till  = suc_it_BG_ref_till    +  
     &  suc_it_phy
                hex_it_BG_ref_till  = hex_it_BG_ref_till    +  
     &  hex_it_phy   
            
            else
            
                !--- Carbon balance of growth
                dsug_it_phy_growth      = phprof(phy,49)
                dstr_it_phy_growth      = phprof(phy,48)
                ddw_it_phy_growth       = phprof(phy,47)
            
                !--- Current State
                dw_it_phy               = phprof(phy,50)
                str_it_phy              = phprof(phy,51)  
                sug_it_phy              = phprof(phy,52)
            
                !--- Integrate Total Dry Weight and Sugars [g]                  
                dw_it_phy   = dw_it_phy  + ddw_it_phy_growth    ! Internode Dry Weight        [g]
                str_it_phy  = str_it_phy + dstr_it_phy_growth   ! Internode Structural Weight [g]
                sug_it_phy  = sug_it_phy + dsug_it_phy_growth   ! Internode Sugars Weight     [g]
            
                !--- Update internode profile
                phprof(phy,50)  = dw_it_phy  ! Internode Dry Weight        [g]
                phprof(phy,51)  = str_it_phy ! Internode Structural Weight [g]
                phprof(phy,52)  = sug_it_phy ! Internode Sugars Weight     [g]
            
                !--- Sucrose and Hexose Contents at above ground internodes
                call sucrose_content(   dw_it_phy,             ! Input Variable
     & sug_it_phy,            ! Input Variable
     & suc_min,               ! Input Parameter
     & hex_min,               ! Input Parameter
     & suc_acc_ini,           ! Input Parameter
     & suc_frac_rate_ts,      ! Input Parameter
     & suc_it_phy,            ! Output Variable
     & hex_it_phy)            ! Output Variable
            
                !--- Above Ground
                suc_it_AG_ref_till  = suc_it_AG_ref_till    +  
     &  suc_it_phy
                hex_it_AG_ref_till  = hex_it_AG_ref_till    +  
     &  hex_it_phy                  
            
            endif
        
            !--- Store in phytomer profile
            phprof(phy,53)  = suc_it_phy ! Internode sucrose weight [g]
            phprof(phy,54)  = hex_it_phy ! Internode hexoses weight [g]
        
            !--- Fractions of Fiber/Sugars/Sucrose/Hexose
            if(phprof(phy,50) .gt. 0.d0)then
                phprof(phy,17)  = phprof(phy,51) / phprof(phy,50)
                phprof(phy,18)  = phprof(phy,52) / phprof(phy,50)
                phprof(phy,19)  = phprof(phy,53) / phprof(phy,50)
                phprof(phy,20)  = phprof(phy,54) / phprof(phy,50)
            endif
        
        enddo
      
        !--- Field Level Biomass Integration [ton ha-1]
        dw_rt       =   dw_rt       +   ddw_rt      -   ddw_rt_dead
        dw_lf       =   dw_lf       +   ddw_lf      -   ddw_lf_dead    
     &  -   ddw_lf_shed     +   ddw_lf_appear
        dw_it       =   dw_it       +   ddw_it      -   ddw_it_dead    
     &  +   dsubsres
        dw_it_AG    =   dw_it_AG    +   ddw_it_AG   -   ddw_it_AG_dead
        str_it_AG   =   str_it_AG   +   dstr_it_AG  -   dstr_it_AG_dead
        sug_it_AG   =   sug_it_AG   +   dsug_it_AG  -   dsug_it_AG_dead    
        dw_it_BG    =   dw_it_BG    +   ddw_it_BG   -   ddw_it_BG_dead 
     &  +   dsubsres
        str_it_BG   =   str_it_BG   +   dstr_it_BG  -   dstr_it_BG_dead
        sug_it_BG   =   sug_it_BG   +   dsug_it_BG  -   dsug_it_BG_dead
     &  +   dsubsres
        wat_it_AG   =   wat_it_AG   +   dwat_it_AG  -   dwat_it_AG_dead    
    
        !--- Upscale Sucrose/Hexose to Field Level [ton ha-1]    
        suc_it_AG   =   suc_it_AG_ref_till * (nstk_now * tilleragefac)
     &  * (1.e4 / 1.e6)
        hex_it_AG   =   hex_it_AG_ref_till * (nstk_now * tilleragefac)
     &  * (1.e4 / 1.e6)
        suc_it_BG   =   suc_it_BG_ref_till * (nstk_now * tilleragefac)
     &  * (1.e4 / 1.e6) 
        hex_it_BG   =   hex_it_BG_ref_till * (nstk_now * tilleragefac)
     &  * (1.e4 / 1.e6)
        
        !--- Correction factor to meet carbon balance
        dsug_corr_fac_BG   = 1.d0
        dsug_corr_fac_AG   = 1.d0    
        if((suc_it_BG + hex_it_BG) .gt. 0.00001) dsug_corr_fac_BG   =
     &  sug_it_BG / (suc_it_BG + hex_it_BG)
        if((suc_it_AG + hex_it_AG) .gt. 0.00001) dsug_corr_fac_AG   =
     &  sug_it_AG / (suc_it_AG + hex_it_AG)     
    
        !--- Check carbon balance discrepancy
        c_check_tol = 0.10d0
        if(abs(1.d0 - dsug_corr_fac_BG) .gt. c_check_tol)then
            write(warn,*) 'More than 10% Carbon Balance Discrepancy'//
     & ' on Internode Below Ground Sugar/Structural Partitioning'//
     & ' at DAS: ',das, 
     & 'Discrepancy of ', abs(1.d0 - dsug_corr_fac_BG) * 100.d0,
     &  '% was corrected.'
        endif
        
        c_check_tol = 0.10d0
        if(abs(1.d0 - dsug_corr_fac_AG) .gt. c_check_tol)then
            write(warn,*) 'More than 10% Carbon Balance Discrepancy'//
     & ' on Internode Above Ground Sugar/Structural Partitioning'//
     & ' at DAS: ',das, 
     & 'Discrepancy of ', abs(1.d0 - dsug_corr_fac_AG) * 100.d0,
     &  '% was corrected.'
        endif        
    
        suc_it_AG   =   suc_it_AG	*	dsug_corr_fac_AG
        hex_it_AG   =   hex_it_AG	*	dsug_corr_fac_AG
        suc_it_BG   =   suc_it_BG	*	dsug_corr_fac_BG 
        hex_it_BG   =   hex_it_BG	*	dsug_corr_fac_BG    
    
        !--- Total Dry Biomass [ton ha-1]
        dw_total    =   dw_it       +   dw_lf   +   dw_rt
          
        !--- Stalk Fresh Mass [ton ha-1]
        fw_it_AG    =   dw_it_AG    +   max(0.d0, wat_it_AG) ! Avoid Negative Mass that can happen depending on parameters setup (dswat_dsuc & dswat_dstr)
    
        !--- Overall sugar content in dry mass basis
        if(dw_it_AG .gt. 0.d0) sug_cont    = sug_it_AG / dw_it_AG
    
        !--- Sucrose content on Fresh Stalk basis (POL%)
        if(sug_cont .gt. suc_acc_ini) then    
            pol          = suc_it_AG / fw_it_AG * 100.d0
            wat_con      = 1.d0 - (dw_it_AG / fw_it_AG)
        else
            pol      = 0.d0 ! To Avoid high pol values at early growth
            wat_con  = 0.85
        endif    
    
        !--- Aerial Dry Biomass [ton ha-1]
        if(flemerged)then        
            !--- After emergence
            dw_aerial   =   dw_it_AG    +   dw_lf        
        else        
            !--- Leaf Dry Weight is at below ground (Shoot)
            dw_aerial   =   dw_it_AG        
        endif    
        
        !--- Plant Population [tillers m-2]
        nstk    =   nstk    +   dnstk
    
        !--- Absolute number of tillers
        atln = aint(max(0.d0, nstk))        
        if(.not. (atln .eq. nstk))then
            !--- Increase one tiller to account for decimals
            atln    =   atln + 1
        endif
    
        if(flemerged) nstk_now = nstk ! nstk_now consider the below ground tillers while crop is not emrged
        if(flemerged) atln_now = atln ! atln_now consider the below ground tillers while crop is not emrged
    
        !--- Relative Age Difference Among Tillers [0-1]
        do tl = 1, atln            
            tillerageprof(tl,1) = tillerageprof(tl,1) + disoil
            tillerageprof(tl,2) = (tillerageprof(tl,1) / 
     & tillerageprof(1,1)) ** tilleragefac_adjust
        enddo
      
        !--- Update Tiller Age Factor
        !--- This factor is necessary to avoid overpredictions when upscaling all tillers to field level
        !--- This factor could be avoided if all tillers were simulated to achieve Field-Plant-Organ Scales (next version). 
        if(atln .lt. ini_nstk) then        
            !--- Only primary tillers
            tilleragefac = 1.d0
        else
            !--- Secondary/tertiary/four... emerged!
            tilleragefac = max(0.01, sum(tillerageprof(1:atln,2)) / 
     & atln) ! Do not let tilleragefac <= Zero (avoid zero division)
        endif
    
        !--- Reallocate below ground sugars among new tillers  
        if(abs(dnstk) .gt. 0.d0)then
            !--- Shared sugars [g] below ground sugars among below ground internodes            
            shared_it_sug_BG =  (sug_it_BG * (1.e6/1.e4) / (nstk_now * 
     & tilleragefac)) / n_it_BG                     
        
            !--- First Below Ground Internode position
            pos_it_BG        = n_ph - n_it_BG + 1        
        
            !--- Carbon balance including the new internode
            do phy = pos_it_BG, n_ph                
                !--- Carbon balance of reserves
                ts_it_phy       =   shared_it_sug_BG
                dw_it_phy       =   phprof(phy,51)  +   ts_it_phy
            
                !--- Update below ground internodes DW and Sugars
                phprof(phy,50)  =   dw_it_phy                           ! All weight are sugars at initial step
                phprof(phy,52)  =   ts_it_phy                           ! Total sugars
                phprof(phy,53)  =   shared_it_sug_BG * frac_suc_BG    ! 50% share sucrose/hexose
                phprof(phy,54)  =   shared_it_sug_BG * frac_hex_BG    ! 50% share sucrose/hexose                
            enddo        
        endif
    
        !--- Stalk Height [m]
        stk_h   =   stk_h   +   per / 1.e3
        
        !--- Stalk diameter 
        !--- Based on stalk population (Fabio Marin)       
        if(nstk_now .lt. 9.d0) then
            diam_stk = -.077 * nstk_now + 3.0443
        else
            diam_stk = -.0256 * nstk_now**2. + .4206 *  nstk_now + .7763
        endif
    
        !--- Leaf Area Index [m2 m-2]
        lai         =   lai     +   dlai_gain   -   dlai_dead   -   
     & dlai_shed   +      dlai_gain_appear
    
        !--- Crop Coefficient (EORATIO)
        kc          = kc_min    + (eoratio - kc_min) * lai / (maxlai_eo)
    
        !--- Average Number of green leaves per tiller
        n_lf_tiller = n_lf_alive_AG * tilleragefac
    
        !--- Root Depth [cm]
        rd = min(rd + drdepth, rdm)
    
        !--- Effective Root Depth [cm]
        effective_rd = min(effective_rd + drdepth, rdm)
        if(.not. flemerged) then
            !--- Upward root growth
            rpup    = rpup + min(drdepth, dshootext_BG)
            rpup    = min(initcropdepth, rpup)
        endif
    
        !--- Root Length Density [cm.root cm-3.soil]
        rld      = rld  +   drld    -   drld_dead ! Arrays algebra    
        
        !--- Phytomer appearance stimulus [0-1]
        phy_stimuli     = phy_stimuli + dphy_stimuli
    
        !--- Crop Age [Cdays]
        diac        =   diac        +   di
        diacsoil    =   diacsoil    +   disoil
    
        !--- Crop Age After Emergence [Cdays]
        if(flemerged) then
            diacem        = diacem        + di
            diacsoilem    = diacsoilem    + disoil
        endif    
    
        !--- Memorize how much substrates is needed for emergence
        if(.not. flemerged)then        
            !--- Learning how much substrates is needed
            res_used_emerg  =   res_used_emerg + supply_used_crop * 
     & (1.e4/1.e6)
        endif
    
    !--------------------!
    !--- Flags Update ---!
    !--------------------!
    
        if(cr_source_sink_ratio .lt. cr_source_sink_ratio_ruse .and. 
     & flemerged)then
        
            !--- Use reserves only for maintenance
            fl_use_reserves     = .false.
        
        else        
            if(sug_it_BG .gt. (res_used_emerg * res_used_emerg_fac))then
            
                !--- Use reserves to growth and maintenance
                fl_use_reserves     = .true.            
            else
                !--- Do not use reserves to growth if its not enough to the crop the emerge again (crop memory)
                fl_use_reserves     = .false.        
            endif
        endif
    
        !--- Check phytomer appearence stimulus 
        if(phy_stimuli .ge. 1.d0) then
        
            !-------------------------------!
            !--- Initiate a new phytomer ---!
            !-------------------------------!
        
            !--- Update Counters (leaf + internode + phytomers)
            n_ph                = n_ph              + 1
            n_it                = n_it              + 1
            n_lf                = n_lf              + 1
            n_lf_alive          = n_lf_alive        + 1
            n_lf_alive_juveni   = n_lf_alive_juveni + 1
                
            if(flemerged)then
            
                !--- Crop emerged
                n_ph_AG                 = n_ph_AG               + 1 ! Note: Here we consider that when leaf reach the surface the phytomer is above ground too. Depite the fact that most of its mass is below ground yet.        
                n_lf_AG                 = n_lf_AG               + 1
                n_lf_alive_AG           = n_lf_alive_AG         + 1
                n_lf_alive_juveni_AG    = n_lf_alive_juveni_AG  + 1
            
                if(fl_stalk_emerged)then            
                    !--- Stalks Emerged
                    n_it_AG =   n_it_AG + 1                
                else
                    !--- Stalks are below the ground
                    n_it_BG =   n_it_BG + 1                
                endif                            
            else
            
                !--- Crop is not emerged yet
                n_ph_BG                 = n_ph_BG               + 1
                n_it_BG                 = n_it_BG               + 1
                n_lf_BG                 = n_lf_BG               + 1
                n_lf_alive_BG           = n_lf_alive_BG         + 1
                n_lf_alive_juveni_BG    = n_lf_alive_juveni_BG  + 1                    
            endif                     
        
            !--- Update phytomer array
            do phy = n_ph, 1, -1
                
                !--- Update profile                
                phprof(phy+1,1)   = phprof(phy,1)  ! Leaf Age
                phprof(phy+1,2)   = phprof(phy,2)  ! Leaf Sink strenght
                phprof(phy+1,3)   = phprof(phy,3)  ! Allocated Leaf biomass
                phprof(phy+1,4)   = phprof(phy,4)  ! Leaf area rate
                phprof(phy+1,5)   = phprof(phy,5)  ! Leaf area
                phprof(phy+1,6)   = phprof(phy,6)  ! Leaf weight
                phprof(phy+1,7)   = phprof(phy,7)  ! Internode Sink Strength dw rate g d-1
                phprof(phy+1,8)   = phprof(phy,8)  ! Initial Leaf Area [cm2]
                phprof(phy+1,9)   = phprof(phy,9)  ! Initial Leaf DW [g]
                phprof(phy+1,10)  = phprof(phy,10) ! Plant population at appearance
                phprof(phy+1,11)  = phprof(phy,11) ! Kmr Internode, 
                phprof(phy+1,12)  = phprof(phy,12) ! Total phytomer Age
                phprof(phy+1,13)  = phprof(phy,13) ! Q10 Internode
                phprof(phy+1,14)  = phprof(phy,14) ! Internode Biomass at end of growth [g]
                phprof(phy+1,15)  = phprof(phy,15) ! Fiber Partitioning factor [0-1]
                phprof(phy+1,16)  = phprof(phy,16) ! Internode Length
                phprof(phy+1,17)  = phprof(phy,17) ! Fraction of Total Sugars
                phprof(phy+1,18)  = phprof(phy,18) ! Fraction of Fiber
                phprof(phy+1,19)  = phprof(phy,19) ! Fraction of Sucrose
                phprof(phy+1,20)  = phprof(phy,20) ! Fraction of Hexose
                phprof(phy+1,21)  = phprof(phy,21) ! Internode Growth Respiration
                phprof(phy+1,22)  = phprof(phy,22) ! Maintenance Respiration Factor (0-1) 1 =  is maintenance is ok
                phprof(phy+1,23)  = phprof(phy,23) ! dLength (cm)
                phprof(phy+1,24)  = phprof(phy,24) ! Lignin
                phprof(phy+1,25)  = phprof(phy,25) ! mresp leaf
                phprof(phy+1,26)  = phprof(phy,26) ! gresp leaf
                phprof(phy+1,27)  = phprof(phy,27) ! dw ss leaf
                phprof(phy+1,28)  = phprof(phy,28) ! sup_ratio_lf_phy
                phprof(phy+1,29)  = phprof(phy,29) ! supply_rate_lf
                phprof(phy+1,30)  = phprof(phy,30) ! supply_used_lf_phy
                phprof(phy+1,31)  = phprof(phy,31) ! supply_used_mresp_lf
                phprof(phy+1,32)  = phprof(phy,32) ! supply_used_gresp_lf
                phprof(phy+1,33)  = phprof(phy,33) ! supply_used_dw_lf
                phprof(phy+1,34)  = phprof(phy,34) ! maintenance_factor_lf
                phprof(phy+1,35)  = phprof(phy,35) ! reduc_growth_factor_lf
                phprof(phy+1,36)  = phprof(phy,36) ! mresp internode
                phprof(phy+1,37)  = phprof(phy,37) ! gresp internode
                phprof(phy+1,38)  = phprof(phy,38) ! dw ss internode
                phprof(phy+1,39)  = phprof(phy,39) ! sup_ratio_it_phy
                phprof(phy+1,40)  = phprof(phy,40) ! supply_rate_it_phy
                phprof(phy+1,41)  = phprof(phy,41) ! supply_used_it_phy
                phprof(phy+1,42)  = phprof(phy,42) ! supply_used_mresp_it_phy
                phprof(phy+1,43)  = phprof(phy,43) ! supply_used_gresp_it_phy
                phprof(phy+1,44)  = phprof(phy,44) ! supply_used_dw_it_phy
                phprof(phy+1,45)  = phprof(phy,45) ! maintenance_factor_it_phy
                phprof(phy+1,46)  = phprof(phy,46) ! reduc_growth_factor_it_phy
                phprof(phy+1,47)  = phprof(phy,47) ! Internode dry weigth rate [g dt-1]
                phprof(phy+1,48)  = phprof(phy,48) ! Internode structural dry weigth rate [g dt-1]
                phprof(phy+1,49)  = phprof(phy,49) ! Internode total sugars rate [g dt-1]
                phprof(phy+1,50)  = phprof(phy,50) ! Internode total dry weigth [g]
                phprof(phy+1,51)  = phprof(phy,51) ! Internode structural dry weigth [g]
                phprof(phy+1,52)  = phprof(phy,52) ! Internode total sugars [g]
                phprof(phy+1,53)  = phprof(phy,53) ! Internode sucrose weight [g]
                phprof(phy+1,54)  = phprof(phy,54) ! Internode hexoses weight [g]
                phprof(phy+1,55)  = phprof(phy,55) ! Leaf Age rate [dCdays]
                phprof(phy+1,56)  = phprof(phy,56) ! Phytomer Age rate [dCdays]
                phprof(phy+1,57)  = phprof(phy,57) ! Internode Age rate [dCdays]
                phprof(phy+1,58)  = phprof(phy,58) ! Internode Age [Cdays]
            
                !--- Flags Arrays
                fl_lf_AG(phy+1)   = fl_lf_AG(phy)
                fl_lf_alive(phy+1)= fl_lf_alive(phy)
                fl_it_AG(phy+1)   = fl_it_AG(phy)                
            enddo
        
            !--- Reset phytomer stimuli
            phy_stimuli    = phy_stimuli  - 1.d0                
        
            !-----------------------------------!
            !--- New phytomer initialization ---!
            !-----------------------------------!
            phprof(1, 1:60) = 0.d0 ! 60 is the total number of phytomers attributes nphy_att
        
            !--- Leaf is alive
            fl_lf_alive(1)  = .true.
        
            !--- Initial leaf area depending on leaf number appearance to deal with leaf sheath size at different stages
            ini_la  = min(1.d0, max(0.d0, (n_lf_AG - 1) / 
     & (n_lf_max_ini_la))) * (max_ini_la - init_leaf_area) + 
     & init_leaf_area
        
            !--- Leaf initial area and dry Weight [cm2 and g]        
            if(flemerged) phprof(1,5)    = ini_la           ! [cm2]
            phprof(1,6)                  = ini_la / sla     ! [g]
        
            !--- Store initial State for SS
            if(flemerged) phprof(1,8)    = ini_la           ! [cm2]         
            phprof(1,9)                  = ini_la / sla     ! [g]
            phprof(1,10)                 = nstk_now         ! [tiller m-2] For upscaling
            
            !--- Initial age of the new phytomer [cDays]
            phprof(1,1)  =   phy_stimuli * phyllochron
            phprof(1,12) =   phy_stimuli * plastochron
            phprof(1,58) =   0.d0   ! Internode will have age zero until reach "Natural Break Point"
        
        
            if(flemerged)then
            
                !--- Crop Emerged
                fl_lf_AG(1) = .true.
            
                if(fl_stalk_emerged)then
                    !--- Stalks emerged
                    fl_it_AG(1) = .true.                
                else
                    !--- Stalks below ground
                    fl_it_AG(1) = .false.
                endif                        
            else
                !--- Crop not emerged
                fl_lf_AG(1) = .false.
                fl_it_AG(1) = .false.
            endif
            
            if(.not. fl_it_AG(1))then
            
                !--- New Below Ground Internode            
                !--- Shared sugars [g] below ground sugars among below ground internodes            
                shared_it_sug_BG =  (sug_it_BG * (1.e6/1.e4) / 
     & (nstk_now * tilleragefac)) / n_it_BG
                        
                !--- Initialize below ground internode 
                phprof(1,50) = shared_it_sug_BG                 ! All weight are sugars at initial step
                phprof(1,51) = 0.d0                             ! No structural
                phprof(1,52) = shared_it_sug_BG                 ! Total sugars
                phprof(1,53) = shared_it_sug_BG * frac_suc_BG   ! 50% share sucrose/hexose
                phprof(1,54) = shared_it_sug_BG * frac_hex_BG   ! 50% share sucrose/hexose
                phprof(1,14) = max_it_dw_BG   
                phprof(1,11) = kmr_stor   
                phprof(1,13) = q10_stor
            
                !--- Carbon balance including the new internode
                do phy = 2, n_it_BG    
                
                    !--- Carbon balance of reserves
                    ts_it_phy       =   shared_it_sug_BG
                    dw_it_phy       =   phprof(phy,51)  +   ts_it_phy
            
                    !--- Update below ground internodes DW and Sugars
                    phprof(phy,50)  =   dw_it_phy                       ! All weight are sugars at initial step
                    phprof(phy,52)  =   ts_it_phy                       ! Total sugars
                    phprof(phy,53)  =   shared_it_sug_BG * frac_suc_BG  ! 50% share sucrose/hexose
                    phprof(phy,54)  =   shared_it_sug_BG * frac_hex_BG  ! 50% share sucrose/hexose                
                enddo
            else
                phprof(1,14) = max_it_dw
                phprof(1,11) = kmr_stem   
                phprof(1,13) = q10_stem
            endif        
            
            !--- Appear a new young leaf
            fl_appear_leaf    = .true.
        
            !--- Leaf Maturity Counters
            dn_lf_alive_dewlap = 0
        
            !--- Amount of juvenile leaves
            if(n_lf_alive_juveni_AG .gt. (maxgl - maxdgl))then            
                n_lf_alive_juveni_AG   = (maxgl - maxdgl)
                dn_lf_alive_dewlap  = 1
            endif
        
            !--- Number of Leaves with formed dewlap 
            n_lf_alive_dewlap  = n_lf_alive_dewlap + dn_lf_alive_dewlap
            n_lf_AG_dewlap     = n_lf_AG_dewlap    + dn_lf_alive_dewlap
        
            !--- Check leaf spam
            if(n_lf_alive_dewlap .gt. maxdgl)then
            
                !--- Shed the oldest leaf                
                fl_shed_leaf    = .true.
            
                !--- Update living leaf number
                n_lf_alive        = maxgl
                n_lf_alive_AG     = maxgl
                n_lf_alive_BG     = maxgl
                n_lf_alive_dewlap = maxdgl
            
            endif
        
            !--- Check if stalk emerged
            if(n_lf_AG .ge. n_lf_when_stk_emerg)then
                fl_stalk_emerged    = .true.        
            endif
        
        endif
    
        !--- Before emergence
        if(.not. flemerged)then
        
            !--- Shoot Depth [cm]
            shootdepth  = shootdepth - dshootext_BG
        
            !--- Update Counter
            nphy_BGround = nphy_BGround + 1
        
            !--- Critical level of Substrates reserves
            !--- Not even maintenace respiration can be sustained under this condition
            if(sug_it_BG .le. 0.d0 .and. shootdepth .gt. 0.d0) then                
                !--- Kill the crop before emergence
                flcropalive = .false.
                cropstatus  = '  Dead'                 
            endif
        
            if(shootdepth .le. 0.d0)then
            
                !------------------------!
                !--- Crop has emerged ---!
                !------------------------!
                    flemerged = .true.
                !------------------------!
            
                !------------------------------------------!
                !--- Initialize above ground conditions ---!
                !------------------------------------------!
		        shootdepth          = 0.d0
                phprof(n_ph,5)      = init_leaf_area                            ! Inital leaf area (cm2)
                nstk                = ini_nstk                                  ! Initial Tillering
                cropdstage          = 'Emergd'                                  ! Update Stage ID     
                diac_at_emergence   = diacsoil                                  ! Cdays at Emergence
                lai                 = init_leaf_area * ini_nstk / 1.e4          ! [m2 m-2]
            endif        
        endif
       
    
    !--------------------------!
    !--- Write Step Outputs ---!
    !--------------------------!
        
        !--- Detailed Photosynthesis output
        if(writedetphoto .and. flemerged)then
                
            !--- Convert to μmol m-2 s-1 for output purpose
            amax_out = amax_mod * 1.e3 / 1.e4 / 3600 / 44.d0 * 1.e6
            eff_out  = eff_mod  * 1.e3 / 1.e4 / 3600 / 44.d0 * 1.e6 / 4.6
        
            do glai = 1 ,5
                do ghour = 1, 3
                    write(outdph,111) seqnow, ',', pltype, ',', year,
     &  ',', doy, ',', das, ',', dap, ',', ghour, ',', glai, ',',
     &  lai_ass, ',', frac_li, ',', amax_out, ',', eff_out, ',',
     &  Acanopy(ghour+1,1), ',', Acanopy(1,glai+1), ',',
     &  Qleaf(ghour+1,glai+1), ',', Acanopy(ghour+1,glai+1), ',',
     &  incpar(ghour,2), ',', incpar(ghour,3), ',', incpar(ghour,4)
                enddo
            enddo
        endif
    
      write(outd,'(1x,i4,1x,i3,1x,10f20.5)') das, dap, frac_li,
     &  li, dtg, dtga
    
    
111   format(     i2,         a1,        ! seqnow
     &            a6,         a1,        ! pltype
     &            i4,         a1,        ! year
     &            i3,         a1,        ! doy
     &            i4,         a1,        ! das
     &            i4,         a1,        ! dap                   
     &            i2,         a1,        ! ghour                 [hour of day]
     &            i2,         a1,        ! glai                  [canopy layer]
     &            f20.5,      a1,        ! lai                   [m2/m2]
     &            f20.5,      a1,        ! frac light absorbed   [0-1]
     &            f20.5,      a1,        ! amax_out              [μmol m-2 s-1]
     &            f20.5,      a1,        ! eff_out               [μmol(CO2) μmol(photon)-1]
     &            f20.5,      a1,        ! Acanopy(ghour)        [hour]
     &            f20.5,      a1,        ! Acanopy(glai)         [m2/m2]
     &            f20.5,      a1,        ! Qleaf(ghour,glai)     [μmol/m2/s]
     &            f20.5,      a1,        ! Acanopy(ghour,glai)   [μmol/m2/s]
     &            f20.5,      a1,        ! incpar(ghour,2)       [direct PAR - W/m2]
     &            f20.5,      a1,        ! incpar(ghour,3)       [difuse PAR - W/m2]
     &            f20.5)                 ! incpar(ghour,4)       [total PAR - W/m2]

        
      !--- Detailed Crop Outputs (Phytomer Profile)
          if(writedcrop)then
              do phy = 1, n_ph           
            
            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy,
     &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',',
     &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',',
     &  'Leaf Age'                   , ',', 'Cdays'   ,
     &  ',', phprof(phy,1)
            
            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy,
     &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',',
     &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',',
     &  'Leaf Total DW'              , ',', 'g'       ,
     &  ',', phprof(phy,6)
                        
            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy,
     &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',',
     &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',',
     &  'Leaf Area'                  , ',', 'cm2'     ,
     &  ',', phprof(phy,5)
            
            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy,
     &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',',
     &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',',
     &  'Internode Age'              , ',', 'Cdays'   ,
     &  ',', phprof(phy,58)
            
            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy,
     &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',',
     &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',',
     &  'Internode Total DW'         , ',', 'g'       ,
     &  ',', phprof(phy,50)
            
            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy,
     &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',',
     &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',',
     &  'Internode Structural DW'    , ',', 'g'       ,
     &  ',', phprof(phy,51)
            
            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy,
     &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',',
     &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',',
     &  'Internode Total Sugars DW'  , ',', 'g'       ,
     &  ',', phprof(phy,52)
            
            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy,
     &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',',
     &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',',
     &  'Internode Sucrose DW'       , ',', 'g'       ,
     &  ',', phprof(phy,53)
            
            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy,
     &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',',
     &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',',
     &  'Internode Hexose DW'        , ',', 'g'       ,
     &  ',', phprof(phy,54)
            
            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy,
     &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',',
     &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',',
     &  'Internode Length'           , ',', 'mm'      ,
     &  ',', phprof(phy,16)
            
            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy,
     &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',',
     &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',',
     &  'Phytomer Age'               , ',', 'Cdays'   ,
     &  ',', phprof(phy,12)
            
            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy,
     &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',',
     &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',',
     &  'Internode Fiber Fraction'   , ',', 'Cdays'   ,
     &  ',', phprof(phy,17)
            
            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy,
     &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy),
     &  ',', fl_lf_AG(phy), ',', fl_lf_alive(phy), ',',
     &  'Internode Sugars Fraction'  , ',', 'Cdays'   ,
     &  ',', phprof(phy,18)
            
            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy,
     &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',',
     &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',',
     &  'Internode Sucrose Fraction' , ',', 'Cdays'   ,
     &  ',', phprof(phy,19)
            
            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy,
     &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy), ',',
     &  fl_lf_AG(phy), ',', fl_lf_alive(phy), ',',
     &  'Internode Hexose Fraction'  , ',', 'Cdays'   ,
     &  ',', phprof(phy,20)
            
            write(outdpa,113) seqnow, ',', pltype, ',', year, ',', doy,
     &  ',', das, ',', dap, ',', phy, ',', fl_it_AG(phy),
     &  ',', fl_lf_AG(phy), ',', fl_lf_alive(phy), ',',
     &  'Pfac Struc'                 , ',', '0-1'     ,
     &  ',', phprof(phy,15)
            
          enddo
113     format(i2,a1,a6,a1,i4,a1,i3,a1,i4,a1,i4,a1,i3,
     & a1,l1,a1,l1,a1,l1,a1,a25,a1,a5,a1,f12.4)
      endif
    
    !--- Partitioning Factors Outputs
      write(outpfac, 144) das, dap, fl_use_reserves, 
     & cr_source_sink_ratio, dtcrss, tot_gresp_crop, 
     & tot_mresp_crop, tot_dw_ss_crop, 
     & dtg*(1.e6/1.e4), sug_it_BG, subs_avail_growth_crop, 
     & supply_used_crop, supply_used_mresp_crop, 
     & supply_used_gresp_crop, supply_used_dw_crop, 
     & reserves_used_mresp_crop, maintenance_factor_crop, 
     & reduc_growth_factor_crop, 
     & dr_rtss, dr_lfss, dr_itss, swfacp, str_it_AG, 
     & sug_it_AG, frac_li, li
144     format(1X,I4,3X,i3,3x,l1,3X,200F12.4)  
        
        
      !--- Stress Factors Outputs
      write(outstres, 145) das, dap, trasw, eop, 
     & trwup*10.d0, max(trwup/(eop/10.),0.d0), swfacp, 
     & swface, swfacf, swfact, tmed, tempfac_pho, tempfac_per, 
     & co2, pho_fac_co2, diacem, agefactor_amax, 
     & agefactor_per, sug_it_BG, amaxfbfac, dtg*(1.e6/1.e4), per

145     format(1X,I4,3X,i4,3x,200F12.4)  
        !--------------------!
        !--- Crop Outputs ---!
        !--------------------!
        if(writeactout)then
            write(outp,109) seqnow,     
     & pltype,
     & year,  
     & doy,   
     & das,   
     & dap,   
     & diac,  
     & dw_total, 
     & dw_it_AG, 
     & dw_lf,    
     & dw_rt,    
     & fw_it_AG, 
     & suc_it_AG,
     & pol,      
     & lai,      
     & nstk,     
     & stk_h,    
     & n_lf_AG_dewlap*1., 
     &  swface,   
     &  swfacp,   
     & cropstatus,
     & cropdstage,
     & project
            
        endif
    
109     format(I2,3X,A6,3X,I4,4X,I3,3X,I4,4X,I3,1F8.1,2f8.2,
     & 3F8.2,8F8.2,3X,A6,2X,A6,2X,A20)     
        
        !--- Link with SWAP variables
        ch    =   stk_h * 1.e2    ! Crop Height [cm] for PenMon()
        cf    =   kc              ! Crop factor (-)
        
        if(flclos_file .and. flCropEnd)then
        !--- close output files
        call output_samuca(  3,           
     &                    project,        
     &                    outp,           
     &                    outd,           
     &                    outdph,         
     &                    outdpa,         
     &                    outpfac,        
     &                    outstres,       
     &                    writedetphoto,  
     &                    writedcrop,     
     &                    writehead)
        endif
        
      return
      
      end subroutine Samuca
          
	!************************************************************************
          

          
          
          subroutine Samuca_phd_version(task)            
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
      integer  :: deb_io		= 999	!I/O for debugging
      
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
      logical		fluseritchie		!use ritchie water balance instead of Feddes or De Jong?
      logical     flusethourgdd       !use hourly temperature on GDD?
      logical     flusethour          !Compute hourly temperature based on Parton&Logan(1981) model?
      logical     flglaiplas          !Use Green Leaf Area Plasticity Related to Water Stress?
      logical		fldebug				!Debug code      
      
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
      real        di                  !daily accumulated temperature above TB used for the crop developmet (degree days)
      real        di_air              !daily accumulated temperature above TB in above ground conditions, i.e. using air temperature (degree days)
      real        di_soil             !daily accumulated temperature above TB in soil considering the root depth range
      real        di_soils            !daily accumulated temperature above TB in soil surface (meant to be used for tillering)
      real        diac                !Cumulative Degree-Days
      real        diaclf              !Cumulative Degree-Days for leaf dev
      real        diam                !average stem diameter (cm)
      real        dileaf              !
      real        dla                 !
      real        dleafdm             !Daily incremental leaf dry mass
      real        dnleaf              !Incremental leaf number
      real        dnstk               !Actual Tillering daily rate # m-2 t
      real        dnstkpot            !Potential Tillering daily rate # m-2 t
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
      real        pgpot               !canopy potential gross photosynthesis rate (t ha-2 day-1) [after defining factors reduction]
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
      real        dshootdepth         !Shoot depth before emergence daily rate 
      real        sla                 !specific leaf area (m2 kg-1) (P)
      real        srad*8              !Daily solar radiation (MJ m-2)
      real        srl                 !(P)
      real        stalkgpf            !
      real        stkdmc              !Dry matter fraction in the stalk fresh mass
      real        suc_stk             !
      real        sucmax              !
      real        agefpg              !Age factor effect on photosynthesis
      real        swface              !soil water stress reduction factor on crop expansion
      real        swfacp              !soil water stress reduction factor on crop photosynthesis
      real        swfact              !soil water stress reduction factor on tillering
      real        swfacp_ini          !Threshold of the actual/potential transpiration to start stress effect on photosynthesis (P)
      real        swfacp_end          !Threshold of the actual/potential transpiration to which photosynthesis ceases (P)
      real        swface_ini          !Threshold of the actual/potential transpiration to start stress effect on crop extension (P)
      real        swface_end          !Threshold of the actual/potential transpiration to which crop extension ceases (P)          
      real        tra_fac             !Daily actual/pontential transpiration ratio (tra_fac =1: no water stress)
      real        rwuep1              !
      real        rwuep2              !
      real        thour(24)           !Hourly temperature
      real        thour_soil(24)      !Hourly Soil Temperature within root depth range
      real        thour_soils(24)     !Hourly Soil Surface Temperature
      real        t_soil              !Daily Average Soil Temperature within root depth range         
      real        t_soils             !Daily Average Soil Surface Temperature        
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
      real        rgpf_be             !Fraction of structural biomass to be allocated to roots before crop emergence [crop will share reserves among: respiration, roots and apical shoot]
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
      real        stk_age(60,4)       !Stalk age (1) fraction of age in relation to the oldest stalk (2) Number of Green Leaves (3) Fraction of LI (4)
      real        lfage(60,20)        !
      real        lfarea(60,20)       !
      real        lfweight(60,20)     !
      real        plas_fac            !Green leaf area plasticity to water stress
      real*8      glai                !Green leaf area
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
      
      !------------------------------------------------
      !----- MODEL DEBUGGING AND CONSTANTS CONTROL ----
      !------------------------------------------------
      
      !Warnings\errors to include:
      
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
      
      !--- control (Try to make as user settings)
      
          flsink_fbres    = .true.!Simulate Feedback response of sink to photosynthesis rate? (T or F)
      !--- Output Options (put this on a control input file)          
          fldetrtsyout    = .true.!Detailed Root System output?
          fldetlfprout    = .true.!Detailed Leaf Profile (architecture) output?
          fldetitprout    = .true.!Detailed Stalk Profile output?
          fldetpgfaout    = .true.
          outstk_rank     = 1     !Rank of detailed stalk (1 <= outstk_rank <= peakpop)
          itoutnumber     = 35    !Up to 35 internodes will be printed out on outstk_rank and averaged stalks
          fluseritchie	= .false.
          flusethourgdd   = .false.
          flusethour      = .true.
          fldebug			= .true.!Debug
          flglaiplas      = .false.
          
      !------------------------------------------------
      !------------------------------------------------
            
      !--- Reading Crop Parameters
          open(parinp_io,FILE='Samuca.par',
     &      STATUS='OLD',READONLY)
          
          nhd  = 4    !Number of lines of file header   
          nf   = 76   !Number of Real parameters (72 previous)
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
     &    swfacp_ini      ,skip, !
     &    swfacp_end      ,skip,
     &    swface_ini      ,skip,
     &    swface_end      ,skip,
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
      glai        = 0.d0
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
      swfact      = 1.d0
      agefpg      = 1.d0
      tra_fac     = 1.d0
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
              initcropdepth   = plantdepth
              shootdepth      = plantdepth
      
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
          ! More info in github repository (Murilo Vianna)
          ! https://github.com/Murilodsv/R-scripts/blob/master/Optimization_SWAP_Sugarcane_v1.R    
          !--------------------------------------------------
          
          open(calibr_io,FILE='Param_optim.opt',
     &      STATUS='OLD',READONLY)          
          
          !call readrea('Calibration',2,real_host,1,.false.,
     &    !.false.,calibr_io,'Param_optim.opt',messag)
          
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
       
          !swcon1 = 1.32E-3 !L/?/?
          !swcon3 = 7.01    !L/?/?          
          
          !--- Original empirical coefficients for Ritchie Potential Root Water Uptake 
          swcon1 = 2.67E-3 !L/?/?
          swcon3 = 6.68    !L/?/?
          
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
          
          !-----------------!
          !--- Debugging ---!
          !-----------------!
          if(fldebug)then
			open(deb_io,FILE='
     &    Debug_'//trim(project)//'.OUT',STATUS='REPLACE',RECL=5180) 
          endif
	
      endif
          
          !--- Writing the Main Output file header            
		write(defout_io,11) '#Simulating for ', project
		write(defout_io,13) 
		write(defout_io,14)
		write(defout_io,15)
		write(defout_io,16)
		write(defout_io,17)
                    
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
              write(detrtout_io,181)
              write(detrtout_io,11)
              write(detrtout_io,182) (i, i=1,numnod)
              
          endif
              
          if(fldetlfprout)then              
              !--- Writing the Main Output file header            
		    write(detlfout_io,11) '#Simulating for ', project      
              write(detlfout_io,19)
              write(detlfout_io,191)
              write(detlfout_io,11)
              write(detlfout_io,192) (i, i=1,maxgl+1),(i, i=1,maxgl+1),
     & (i, i=1,maxgl+1),(i, i=1,maxgl+1)
          endif
              
          if(fldetitprout)then              
              !--- Writing the Main Output file header            
		    write(detitout_io,11) '#Simulating for ', project      
              write(detitout_io,20)
              write(detitout_io,201)
              write(detitout_io,11)
              write(detitout_io,202) (i, i=1,itoutnumber),
     & (i, i=1,itoutnumber), (i, i=1,itoutnumber), (i, i=1,itoutnumber),
     & (i, i=1,itoutnumber), (i, i=1,itoutnumber)
          endif
              
          if(fldetpgfaout)then              
              !--- Writing the Main Output file header            
		    write(detpgout_io,11) '#Simulating for ', project      
              write(detpgout_io,21)
              write(detpgout_io,211)
              write(detpgout_io,11)
              write(detpgout_io,212)
              
          endif
          
          !--- Detailed Root System Outputs
18        format('Result of sugarcane RootSystem:')
181       format(
     &    'wr:      Total Root System Dry Mass (t ha-1)',/,
     &    'rd:      Root System Depth (cm)',/,
     &    'rsene:   Root Senesence Factor (0-1)',/,
     &    'rld:     Root Length Density (cm cm-3)',/,
     &    'qropot:  Potential Total Root Water Uptake (cm)',/,
     &    'ptra:    Potential Transpiration (cm)')          
182       format('Year    DOY    DAS    DAP     GDD      wr      rd  ',
     &    ' rsene  ',<numnod>(' rld',i2.2,2x),'qropot    ptra')         
          
          !--- Detailed Leaf outputs
19        format('Result of sugarcane LeafProfile:')
191       format(
     & 'ngl:   Average Number of Green Leaf per Stalk',/,
     & 'devgl: Average Number of Developed Green Leaf per Stalk ',/,
     & 'p_la:  Green Leaf Area for primary Stalk (cm2)',/,
     & 'a_la:  Average Green Leaf Area (cm2)',/,
     & 'p_lw:  Leaf Dry Weight for primary Stalk (g)',/,
     & 'a_lw:  Average Leaf Dry Weight (g)')
192       format('Year    DOY    DAS    DAP     GDD     ngl   devgl  ',
     & <maxgl+1>('p_la',i2.2,2x), <maxgl+1>('a_la',i2.2,2x),
     & <maxgl+1>('p_lw',i2.2,2x), <maxgl+1>('a_lw',i2.2,2x))       
          
      
          !--- Detailed Internodes Outputs
20        format('Result of sugarcane StalkProfile:')
201       format(
     & 'itpl: Primary Stalk Internode Length (cm)',/,
     & 'ital: Average Internode Length for all Stalks (cm)',/,
     & 'itps: Primary Stalk Internode Sucrose Mass (g)',/,
     & 'itas: Average Internode Sucrose Mass for all Stalks (g)',/,
     & 'itpw: Primary Stalk Internode Total Dry Mass (g)',/,
     & 'itaw: Average Internode Total Dry Mass for all Stalks (g)')
202       format('Year    DOY    DAS    DAP     GDD  m_nint  ',
     & <itoutnumber>('itpl',i2.2,2x), <itoutnumber>('ital',i2.2,2x),
     & <itoutnumber>('itps',i2.2,2x), <itoutnumber>('itas',i2.2,2x),
     & <itoutnumber>('itpw',i2.2,2x), <itoutnumber>('itaw',i2.2,2x))
     
          !--- Detailed Crop Stresses
21        format('Result of sugarcane PG Factors:')
211       format(     
     & 'dpar   !Daily par MJ m-2 d-1',/,
     & 'extc   !k',/,
     & 'dlai   !LAI',/,
     & 'frli   !Fraction Light Intercepted',/,
     & 'aco2   !Atm CO2 concentration',/,
     & 'mrue   !Maximum RUE (gDW MJ-1) - Crop',
     &' Parameter',/,
     & 'co2f   !CO2           Factor',/,
     & 'tmpf   !Temperature   Factor',/,
     & 'agef   !Age           Factor',/,
     & 'swfp   !Avl Water     Factor',/, 
     & 'sfbf   !Sink Feedback Factor',/,
     & 'gpho   !Daily photosynthesis before RGP_fac',
     & 'reduction and resp = 0.d0 (t ha-1)',/,
     & 'prfb   !Daily photosynthesis reduction due',
     & 'to sink feedback (t ha-1)',/,
     & 'dtdw   !Daily total allocated',
     & 'biomass (t ha-1)',/, 
     & 'pcfb   !Cumulative photosynthesis reduction',
     & 'due to sink feedback (t ha-1)',/,
     & 'cipa   !Cumulative PAR intercepted (MJ m-2)',
     & /,
     & 'tldw   !Crop total live dry biomass (g m-2)',
     & /,
     & 'aldw   !Crop total live aerial dry biomass',
     & '(g m-2)',/,
     & 'ctdw   !Crop total dry biomass',
     & 'produced (g m-2)',/,
     & 'cadw   !Crop total aerial dry',
     & 'biomass produced (g m-2)',/,
     & 'drdw   !Daily Actual RUE (g MJ-1)',
     & 'Total  Dw Basis (deadbiomass not accounted)',/,
     & 'drtw   !Total Actual RUE (g MJ-1)',
     & 'Total   w Basis (deadbiomass not accounted)',/,
     & 'drda   !Daily Actual RUE (g MJ-1)',
     & 'Total Dwa Basis (deadbiomass not accounted)',/,
     & 'drta   !Total Actual RUE (g MJ-1)',
     & 'Total  wa Basis (deadbiomass not accounted)',/,
     & 'cchk   !Carbon Balance Check')
212       format('Year    DOY    DAS    DAP     GDD  ',
     & '  dpar    extc    dlai    frli    aco2    mrue',
     & '    co2f    tmpf    agef    swfp    sfbf    gpho    prfb',
     & '    dtdw    pcfb    cipa    tldw    aldw    ctdw    cadw',
     & '    drdw    drtw    drda    drta    cchk')

     
            
	return      
      
2000  continue
	
	!----------------------------
	!--- Daily Rate Potential ---
	!----------------------------
      
      !Reset daily rates      
      di          = 0.d0
      di_air      = 0.d0
      di_soil     = 0.d0
      di_soils    = 0.d0
      dshootdepth = 0.d0
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
      pgpot       = 0.d0
      dRGP_pg     = 0.d0
      dnetbiomass = 0.d0
      dwwater     = 0.d0
      dsuc        = 0.d0
      dfib        = 0.d0
      dper        = 0.d0
      dnstk       = 0.d0
      dnstkpot    = 0.d0
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
      swfact      = 1.d0
      agefpg      = 1.d0
      tra_fac     = 1.d0
      RGP_fac     = 1.d0
      
      !-----------------------------------
      !--- Linking with SWAP Variables ---
      !-----------------------------------
      tmax        = tmx                       !(oC)
      tmin        = tmn                       !(oC)
      t_mean      = 0.5 * (tmax + tmin)       !(oC)
      par         = (rad * 0.5)/0.1E7         !(MJ/m2.d)
      epp         = ptra                      !(cm/d)
      ch          = pleng * 100.              !(cm) Used for Penmam-M. method      
      if(swhea .eq. 1)then
          !--- link to soil heat flux
          t_soil  = sum(tsoil(1:noddrz))/(max(1,size(tsoil(1:noddrz)))) ! Average Soil Temperature within rootsystem depth
          t_soils = tetop                     ! Temperatures (ºC) at top of soil profile (under snow cover) [Implement mulch cover...]
      else
          !--- assume as equal as air temp
          t_soil  = t_mean
          t_soils = t_mean          
      endif
            
      !--- Hourly Temperature in thour array based on Parton & Logan empirical model
      call TempHour(tmax,tmin,thour)
      thour_soil  = thour     ! using Tsoil = T air (How to compute max and min temperature on soil?- MV)
      thour_soils = thour     ! add here first soil node temperature from temperature module
      
      !--- Calculating growing degree-days (GDD) using a unique tB
      if(flusethourgdd)then
	!--- compute on hourly base
          do i = 1, 24
              di_soil  = di_soil  + max(0.d0, thour_soil(i)-tb)  / 24.d0
              di_soils = di_soils + max(0.d0, thour_soils(i)-tb) / 24.d0
              di_air   = di_air   + max(0.d0, thour(i)-tb)       / 24.d0
          enddo
      else
      !--- use daily temp average
          di_soil     = max(0.d0, t_soil-tb)
          di_soils    = max(0.d0, t_soils-tb)
          di_air      = max(0.d0, t_mean-tb)
      endif 
      
      !-------------------------
      !--- Check Cane Stages ---
      !-------------------------
      
      !--- Check if crop emerged
      if(shootdepth .lt. 1.d-8 .and. .not. flemerged)then
          flemerged   = .true. !Emerged
          shootdepth  = 0.d0
          chuem       = diac
          initnstk    = init_pop
          nstk        = initnstk
          la          = init_la / 1.e4 !One leaf area in m2 (dim: 15 x 2 cm)
          ln          = 2.d0           !Assuming two leaves per stem
          lntotal     = ln             !All leaves have dewlap
          lai         = nstk * ln * la !Considering two developed leaves per stalk
          canestage   = 'Emergd'                  
      endif
      
      !--- Check if stalks emerged
      if(diac .gt. chustk .and. .not. flstalkemerged)then
              !if(.not. flemerged) !add a warning message! Stalk growth before crop emergence! Review your crop parameters
              flstalkemerged  = .true.
              canestage       = 'StkGro'
              
              !initiate Stalks
              wstkwat     = wa * (1.d0/0.3d0 - 1.d0) !Water content in stalks (70% water ~ potential condition) - this is a convertion from Dry to Fresh Mass
              noden       = 1.d0
              age_stkemer = stk_age(1,1)
      endif
      
      !------------------------!
      !--- Before Emergence ---!
      !------------------------!
      
      if(.not. flemerged)then         
          
          !--- Crop Development
          di  = di_soil !Assuming crop is below ground
          
          !--- Shoot expansion rate towards soil surface
          dshootdepth  = -di * shootrate_bg !Assuming the primary shoot growth rate of 0.08 cm/DG (Keating et. al. 1999)
          
          !--- Maintenance Respiration from Liu & Bull (2001) [Fig3]
          !--- doi.org/10.1016/S0304-3800(01)00372-6
          !--- Note: q10 curves are more apropriate to follow classical Crop Models (Thornley & Campbel, 1990)
          !--- Note: Maintenance Respiration is only computed for before emergence conditions to simulate whether the crop has enough substrates reserves to reach surface
          mresp   = 3.991 * exp(0.046 * t_mean)                       !g CO2 g-1 w min-1 
          mresp   = mresp *((24.d0*60.d0)/10.d0**6)*(CH2O_M/CO2_M)    !g CH2O g-1 w d-1
          mresp   = mresp * w ! t ha-1 d-1
              
          !--- Reduce Available Sugars (Maintenance is priority)
          availw_bg   = max(0.d0, availw_bg - mresp)
              
          !--- Sink Strength
          !--- Assuming 30% of cost for growth respiration (Growth Respiration - Inman-Bamber, 1991)
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
              
          !There is no Biomass Gain (pg = 0.d0), only re-allocation of reserves and respiration
          dw  = -resp
             
          if(availw_bg .le. 0.d0)then
              !Not enough reserves for shoot emergence                
              flcropalive = .false.   !Crop is dead before emergence!
              canestatus  = 'DeadLC'  ! LC - Lack of Carbon
              !Warning Msg: Plant it shallow or increase chopped stalks biomass  
          endif
              
          !--- For carbon checking purpose
          rgpf_ck   = rgpf
          lgpf_ck   = lgpf
          sgpf_ck   = sgpf
          tgpf_ck   = tppf
                  
          !Update soil Kc until crop is below ground          
          if(swetr .eq. 1)then
			!--- use kc curve in relation to LAI
			cf          = cfbs          
          else
			!--- use PM method
			cf          = 1.d0
          endif
          
      else

          !-----------------------!
          !--- After Emergence ---!
          !-----------------------!
          
          !--- Crop Development
          di = di_air          
              
          !--- Tillers Rate calculated in Stalks/m2
          !--- Computed based on soil surface temperature (process site)
          if(diac .gt. chuem .and. diac .lt. chupeak)then
              dnstkpot = ((poppeak-initnstk)/(chupeak-chuem))*di_soils    !initial tiller grow
          elseif (diac .ge. chupeak .and. diac .lt. chudec)then
              dnstkpot = 0.d0								                ! peak tiller 
          elseif (diac .ge. chudec .and. diac .lt. chumat)then
              dnstkpot = (-(poppeak - popmat)/(chumat-chudec))*di_soils   ! reduction phase to mature
          elseif (diac .ge. chumat) then
              dnstkpot = 0.d0		            					        ! late stable tiller population
          endif
          
          !--- Crop Growth
          select case(pgmethod)                  
          case(1)
              
              !--- Compute Potential Canopy Photosynthesis based on RUE (pg in tDW ha-1)
              !--- Note that age factor is not computed for photosynthesis (No concrete evidences for that)
              swfacp  = 1.d0 !This is Potential Rate
              agefpg  = 1.d0
              if(di .gt. 0.d0)then !No photosynthesis below tb          
              call pgs_rue(par,co2,t_mean,thour,flusethour,swfacp,
     & agefpg,glai,extcoef,tb,to_pg1,to_pg2,tbM,rue,pgpot,li,tstress,
     & co2_fac)                  
              endif
      
              case(2)
              !To be included...
              !Can call totass() -> Layered Canopy Method (J. Goudriaan)
          end select 
                 
          !Dry biomass partitioning to roots, leaves and stalks - With no Water Stress
          select case(pfmethod)
              case(1)
                  
                  !--- From original samuca v1 Marin & Jones (2014)
                  
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
                  write(*,*) "Warning:Stalks start growing before",
     & "emergence"
                  write(*,*) "Check: CHUSTK or Planting Depth",
     & "parameters"
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
                  write(*,*) "Warning:Stalks start growing before ",
     & "emergence"
                  write(*,*) "Check: CHUSTK or Planting Depth",
     & "parameters"
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
                        
          !Crop coefficient as function of leaf area index (EORATIO method)
          if(swetr .eq. 1)then
			!--- use kc curve in relation to LAI
			cf = kc_min + (eoratio - kc_min) * glai / (maxlai)
			cf = max(0.d0,cf)          
          else
			!--- use PM method
			cf = 1.d0
          endif             
      
      endif
      
      !--- End of Potential Rates                    
      return
      
3000  continue    
      
      !--- Actual crop growth rate
      if(.not. flemerged)then
          
          !------------------------------!   
          !-------Before Emergence-------!
          !------------------------------!
          
          !--- No stress is considered before crop emergence
          !--- SG: Experiment with different moisture threshold and emergence rate?
          
      else
          
          !------------------------------!   
          !--------After Emergence-------!
          !------------------------------!
          
          !--- Compute water stress
          
          !--- Use SWAP embedded methods are more appropriated (set fluseritchie = .false.)
	 if(fluseritchie)then
          !--- compute water stress on crop expansion based on Ritchie 1985 model (potential root water uptake is computed as function of RLD)
          prwu    = 0.d0
          tqropot = 0.d0
        
          !---  root senescence
          !--- Depicted as the fraction of effective roots as function of total roots biomass
          rootsene = 1.d0 - ((f_rts * wr) / (wr + 1.d0))
          
          !--- potential RWU 
          !--- Note that rootsene is NOT been used here          
              !--- compute the potential root water uptake based on empirical relation with root length density (RLD) (Richie (1985)
              !--- IMPORTANT:    This will be only used for crop expansion factor calculation (swface)
              !---               The actual root water uptake is compute according to Feddes Method           
           do node = 1, noddrz
           !--- Apply Ritchie Equation
              prwu(node) = max(0.,min(0.07, 
     &		swcon1*EXP(MIN((swcon2(node)*(theta(node)-wpp(node))),40.))/
     &		(swcon3-LOG(rld(node)))))
			
			!--- compute root water uptake for each node
		    prwulay(node) = prwu(node) * laythi(node) * rld(node)
			
              !--- integrate
		    tqropot = tqropot + prwulay(node)
           enddo
           
           !--- potential uptake never lower than actual uptake
           tqropot = max(tqropot,qrosum)
           
          ! --- water stress factors in growth and photosynthesis 
          if (ptra .le. 1.d-5) then
              !No atmospheric demand
            wuf       = 0.d0
            swfacp    = 1.d0
            swface    = 1.d0
        
          else                             
              
              wuf = max(tqropot/ptra,0.d0)        
        
              if (wuf .lt. rwuep1) then          
                  swface = max(0.,min((1./rwuep1) * wuf,1.))      
              else
                  swface = 1.
              endif
              
              ! SWAP assumes that potential root water uptake is never greater than potential evapotranspiration (ptra), 
              ! i. e. potential root water uptake is equal to ptra
              ! the actual root water uptake (qrosum) is computed by the feddes factor multiplication on ptra
              ! Note: Based on above, wuf_swap will never be greater than 1
              wuf_swap = max(qrosum/ptra,0.d0)
      
              if (wuf_swap .lt. rwuep2) then          
                  swfacp = max(0.,min((1./rwuep2) * wuf_swap,1.))      
              else
                  swfacp = 1.
              endif
              
              if(swfacp .lt. swface)then
                  swface = swfacp * rwuep2 / rwuep1
              endif
        
          endif
          
          !--- Water Stress Effect on Tillering (to be included...)    
          swfact = 1.d0
                 
      else
		!--- Use Feddes for both extension and photosynthesis stress
          !--- Note that in this case water stress effect will have the same magnitude in crop extension and photosynthesis,
          !--- while there are evidences that crop extension is signficantly more sensitive to water stress than photosynthesis rates
          ! There is room to explore wet (qredwetsum), salinity and frost effects here
          tra_fac = max(0.d0, min(1.d0, tra / ptra))
          
          !--- Water Stress Effect on Photosynthesis
          if(tra_fac .ge. swfacp_end .and. tra_fac .le. swfacp_ini)then
              !compute water stress effect on photosynthesis
              swfacp = (tra_fac - swfacp_end) * (1.d0 / (swfacp_ini - 
     & swfacp_end))
              
          else if(tra_fac .lt. swfacp_end)then
              !ceases photynthesis due to water stress
              swfacp = 0.d0              
          else
              !No water stress on photosynthesis
              swfacp = 1.d0              
          endif
          
          !--- Water Stress Effect on Crop Extension (Turgor pressure)
          if(tra_fac .ge. swface_end .and. tra_fac .le. swface_ini)then
              !compute water stress effect on crop extension
              swface = (tra_fac - swface_end) * (1.d0 / (swface_ini - 
     & swface_end))
              
          else if(tra_fac .lt. swface_end)then
              !ceases crop extension due to water stress
              swface = 0.d0              
          else
              !No water stress on crop extension
              swface = 1.d0              
          endif
          
          !--- Water Stress Effect on Tillering (to be included...)    
          swfact = 1.d0
              
      endif
      
      !-- code debug    
       write(deb_io,90) iyear,daynr,daycum,daycrop,diac,
     & qrosum,
     & tqropot,ptra,cumdens(1:202)
      
      !--- Crop Development
      !--- Actual Tillering Rate
      dnstk = dnstkpot * swfact
      
      !--- Crop Growth      
      !--- Actual Gross Photosynthesis
      !--- Here the "Limiting factor" is only water stress (swfacp), nutrients effect are not included
      !--- None "Reducing Factors" are simulated due to the high level of uncertainty/unknow effect
      pg = pgpot * swfacp
              
      !--- Crop Respiration
      select case(pgmethod)
          case(1)              
          !--- RUE       
          !--- NOTE: Respiration is set to zero here because we are using RUE method (gDW MJ-1).
          mresp = 0.d0
          gresp = 0.d0
          resp = mresp + gresp
              
          case(2)
          !---  Implementation of murilo vianna phd thesis (2018)
      end select
      
      !--- Net Biomass Gain Due to Photosynthesis
      dw = max(0.d0, pg - resp)
              
      !--- Stalk Canopy Fraction
      !--- Partition of light among stalks based on thermal-age
      !--- The stk_age(1:60,4) is the fraction of light intercepted for each stalk
      !--- Each stalk has a total LA area is integrated from top-down the canopy
      !--- For each canopy step integration the LI is computed with first derivative of Beer's Law
      !--- in each step, the LI is fractioned among stalks that share the same thermal-age range
      call stk_li(stk_age,glai,kdif,nstk)
              
      !--- Canopy Development
      call lais(diac,di,phyloc,nstk,swface,stk_agefac,ln,maxgl,
     & cumla,ddealla,mla,sla,cumlw,ddeallw,dnleaf,dla,dleafdm,dw,lgpf,
     & dnetbiomass,dnoden,ddeadln,devgl,stk_age,lfage,lfarea,lfweight,
     & dnstk,stk_dnetbiomass,init_nlf,lfshp,maxdevgl)
                           
      !Check whether a tiller senesced              
      if(dnstk .lt. 0.d0)then
          !Top parts dead biomass rate (t ha-1)
          ddeadtop = wt * -(stk_age(aint(nstk),2) / 
     & sum(stk_age(1:aint(nstk), 2)))
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
                  !--- 0.9 and 0.5 threshold from 1st version of SAMUCA (Marin) - No documentation found about these values
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
                
		!--- Integration of daily state variables
          shootdepth= shootdepth + dshootdepth
		nstk      = nstk     + dnstk                     !Number of stalks          
		ln        = ln       + dnleaf - ddeadln          !Number of green leaves per Stem                   
		noden     = noden    + dnoden                    !Number of internodes
          lai       = lai      + dla - ddealla             !Leaf Area Index
		w         = w        + dw  - dwdead              !Total Dry Biomass
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
          diac	  = diac	 + di						 !Cumulative Degree-Days
          if(flemerged) diacem = diacem + di
          
          !--- Green leaf area plasticity realted to water stress
          if(flglaiplas)then
              plas_fac = 0.15
              glai = (lai * (1.d0 - plas_fac)) + lai * plas_fac * swface
          else
              glai = lai
          endif         
          
          !--- Update tillers age
          do tl = 1, aint(nstk)
              stk_age(tl, 1) = stk_age(tl, 1) + di
              stk_age(tl, 2) = stk_age(tl, 1) / stk_age(1, 1) 
          enddo
          
          !--- Stalks age factor
          !--- This is implemented to account for the difference in age among stalks (primary, secondary, tertiary...)
          if(nstk .lt. init_pop)then
              stk_agefac  = 1.d0
          else                  
              stk_agefac  = sum(stk_age(1:aint(nstk), 2)) / 
     &    (aint(nstk))
          endif
          
          !Age Factor
          !Empirical factor to reduce crop processes due to crop aging, e.g.: Stalk extension and photosynthesis (RGP: Reduced Growth Phenomena)
          agefactor = exp(-rate_af*(diac/1000.)) 
          agefactor = min(1.,agefactor)
          
          !--- Compute Stalks Sucrose, Fiber (t ha-1), Height (m) and internode number(#stk-1)
          if(ws .gt. 0.d0 .and. flstalkemerged)then
              suc     = 0.d0
              fib     = 0.d0
              pleng   = 0.d0
              do tl = 1, aint(nstk)
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
     & wl,wt,ws,suc,fib,wsfresh,pol,wdead,glai,nstk,pleng,devgl,
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
     & glai                       , !LAI  
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
      
          end subroutine Samuca_phd_version
          
	!************************************************************************
          
          
          
          subroutine stk_li(stk_age,lai,kdif,nstk)              
          !--- Subroutine implemented to partition the Daily Carbon among stalks
          !--- The main assumptions: 
          !--- (i)  RUE is constant for every leaf
          !--- (ii) Vertical Canopy LI is fractioned among tillers based on its thermal-age
          
          !--- Stalk height could alternatively be used on Item (ii), but would need to implement stalk height for every single stalk.
          !--- Another alternative would be to use the diameter of the spherical model
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
          
          dx      = 0.005     !Relative step change in LAI
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
          
          !Normalize to intercepted light 
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
                    soma = soma +(rld_tocumdens(i-2)+rld_tocumdens(i))*
     & 0.5d0
     &               * (cumdens(i-1)-cumdens(i-3))
                    cumdens(i) = soma
                enddo
      
                ! ---   normalize cumulative root density function to one
                do i = 2,202,2
                    cumdens(i) = cumdens(i) / soma
                enddo
            
            else                
                    ! ---   no root system
                do i = 2,202,2
                    cumdens(i) = 0.d0
                enddo
                
            endif
          
      end select
                  
            return    
            
            
20          continue         
          !Daily Rate 
            
          !Calculating root depth as function of the cumulative degree-days
          !PLANTDEPTH = 25.0   ! PLANTING DEPTH
          !Laclau and Laclau (2009) rate of deepening of the root front (0.53 cm day-1 or 0.048 cm oC-1 day-1) over the first 4 months after planting, and an increase thereafter to 1.75 cm day-1 (0.22 cm oC-1 day-1) in the irrigated crop and 1.86 cm day-1 (0.24 cm oC-1 day-1)
            
          if (diac .LT. 1000) then   ! A variation in RDEPTH should be computed dur the water stress - the higher WS, the deeper the root goes
            drdepth = (rootdrate * di) 
            !RDEPTH = 0.048 * DIAC !- Original results from Laclau & Laclau (2009)
          else
            drdepth = (rootdrate * di)
            !RDEPTH = .22 * DIAC !- Original results from Laclau & Laclau (2009)
          endif
          
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
                    soma = soma +(rld_tocumdens(i-2)+rld_tocumdens(i))*
     & 0.5d0
     &               * (cumdens(i-1)-cumdens(i-3))
                    cumdens(i) = soma
                enddo
      
                ! ---   normalize cumulative root density function to one
                do i = 2,202,2
                    cumdens(i) = cumdens(i) / soma
                enddo
            
            else                
                    ! ---   no root system
                do i = 2,202,2
                    cumdens(i) = 0.d0
                enddo
                
            endif
          
      end select  
          
          end subroutine 
              
              
		
	subroutine pgs_rue(par,co2,t_mean,thour,flusethour,swfacp,agefpg,
     & lai,extcoef,tb,to_pg1,to_pg2,tbM,rue,pg,li,tstress,co2_fac)
      
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
		real par
		real*8 lai
		real diac
		real li                      !Canopy Light interception
		real pg
          real rue
		real swfacp
		real agefpg
		real extcoef
		real t_mean
          real thour(24)
          real tb
          real to_pg1
          real to_pg2
          real tbM
		real tstress
          real tfac_h(24)
		real*8 co2
		real co2_fac          
          logical flusethour
          
          save
          
	    !Fraction Canopy light interception
		li    = 1.d0 - exp(-extcoef * lai)
          
          !Effect of CO2	
		co2_fac =  ((0.0001282051  * co2) + 0.95)          
          
          if(flusethour)then
              
              !--- use hourly air temperature
              tstress = 1.d0
              do i = 1, 24
		    !Computing the Temperature Stress on Photosynthesis (definig factor!)
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
          
              !Averaged Daily Tempererature stress
              tstress = sum(tfac_h(1:24)) / 24.d0
              
          else
              !--- use daily mean air temperature
                  
              if(t_mean .lt. tb)then
                  !No photosynthesis below tb
                  tstress = 0.d0                  
              elseif(t_mean .ge. tb .and. t_mean .lt. to_pg1)then
                  !Below optimum temperature range
                  tstress = (t_mean - tb) / (to_pg1 - tb) 
              elseif(t_mean .ge. to_pg1 .and. t_mean .lt. to_pg2)then
                  !Optimun temperature range    
                  tstress = 1.d0
              elseif(t_mean .ge. to_pg2 .and. t_mean .lt. tbM)then
                  !Above optimun temperature range    
                  tstress = 1.d0 - (t_mean - to_pg2) / (tbM - to_pg2)
              elseif(t_mean .gt. tbM)then
                  !No photosynthesis above tbM
                  tstress = 0.d0    
              endif
          endif
          		
          !--- Gross photosynthesis
          !--- Factors imposed to reduce gross photosynthesis:
          !Atm CO2, crop age, temperature and soil water 
		pg = par * li * rue * co2_fac * agefpg * tstress * swfacp  
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
          
          !--- How much Carbon is been partioned before sink demand
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
                          !Expansion Rate (cm2)
                          dla_ss_stk = dla_ss_stk + 
     & (di * mla_rate)  
                      endif                          
                          
                  else                          
                      !Use variable growing rate with respect to Lf age
                      mla_rate = AScurv(2,lfage(tl,lf),0.,mla,lfshp,
     & (lfgrp*phyloc/2.),1.)
                      !Expansion Rate (cm2)
                      dla_ss_stk = dla_ss_stk + 
     & (di * mla_rate)
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
                          dla_lf = (di*mla_rate) * stk_ssfac(tl) 
                      endif
                  else
                      !Use variable growing rate with respect to Lf age
                      mla_rate = AScurv(2,lfage(tl,lf),0.,mla,lfshp,
     & (lfgrp*phyloc/2.),1.)
                      !Actual leaf daily area increment (cm2)
                      dla_lf = (di*mla_rate) * stk_ssfac(tl) 
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
              
              do h = 1, 24                  
                  tfac_per= min(1.d0,max(0.d0,thour(h)-Tbi)/(Topt-Tbi))
                  dper_h  = dpercoeff * tfac_per * swface * agefactor ! AgeFactor is an age reduction factor for dPER, based on N. G. Inman-Bamber et al. 2008 Australian Journal of Agricultural Research, Fig.3
                  dper    = dper + dper_h                  
              enddo
              
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
	
	
         subroutine TempHour_samuca(tmaxday,tminday,doy,lat,a,b,c,thour)
        !Calculates the Hourly temperature based on Parton & Logan (1981)
    
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
        real lat
        real d_2_r
        real r_2_d

        real a          !       = 1.607 Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = 2.000)
        real b          !       = 2.762 Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = 2.200)
        real c          !       = 1.179 Calibrated for Sao Paulo State   (original constants from Parton and Logan paper = -0.17)

        real thour(24)

        real :: pi      =  3.14159265

        save

        d_2_r = pi/180.
        r_2_d = 180./pi

        !calculating photoperiod
        decsol  = 23.45 * sin(((360./365.)*(doy-80.)*d_2_r))

        !ahn     = acos((-tan(d_2_r*lat)*tan(d_2_r*decsol)))
        photop  = acos((-tan((lat)*d_2_r))*(tan((decsol)*d_2_r))) * 
     & r_2_d * (2./15.)
        nigthp  = 24 - photop
        sunrise = 12 - photop/2
        sunset  = 12 + photop/2

        bb      = 12. - photop / 2. + c
        be      = 12. + photop / 2.
        ddy     = photop - c

        !Calculating air temperature follow Parton & Logan (1981)
        tsunset = (tmaxday - tminday) * sin(((pi*ddy)/(photop+2*a))) + 
     &  tminday

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
        end subroutine TempHour_samuca
      

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
      