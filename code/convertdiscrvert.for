! File VersionID:
!   $Id: convertdiscrvert.for 205 2011-11-07 12:05:07Z kroes006 $
! ----------------------------------------------------------------------
      subroutine ConvertDiscrVert(SwDiscrvert,part,Swop,nrlevs,numlay,
     &         botcom,numnod,dz,h,theta,inq,inqrot,inqdra,
     &         IThetaBeg,Tsoil,cofgen,
     &         numnodNew,dzNew,botcomNew,hNew,thetaNew,inqNew,
     &         inqrotNew,inqdraNew,IThetaBegNew,TsoilNew,
     &         DiPoCp,FrArMtrx,IAvFrMpWlWtDm1,IAvFrMpWlWtDm2,
     &         IQExcMtxDm1Cp,IQExcMtxDm2Cp,IQOutDrRapCp,VlMpStDm1,
     &         VlMpStDm2,DiPoCpNew,IAvFrMpWlWtDm1New,IAvFrMpWlWtDm2New,
     &         IQExcMtxDm1CpNew,IQExcMtxDm2CpNew,IQOutDrRapCpNew,
     &         VlMpStDm1New,VlMpStDm2New,swsophy,numtab,sptab)
!***********************************************************************
!*    Routine: ConvertDiscrVert()
!*    Purpose: converts vertical descritization 
!*    Usage:   Call ConvertDiscrVert(......)
!*    Author(s):   
!*    Version:   
!*    Date/history:
!*    See also:
!     Arguments:
!     I/O Data_type Name         Meaning
!      I     I4     SwDiscrVert  
!      I     I4     part         
!      I     I4     Swop  
!      I     I4     logf         Unit number
!      I     I4     nrlevs       
!      I     I4     numlay       
!      I     I4     botcom(maho) 
!      I     I4     numnod       
!      I     R8     dz(macp)     
!      I     R8     theta(macp)  
!      I     R8     inq(macp)    
!      I     R8     inqrot(macp) 
!      I     R8     inqdra(macp) 
!      I     R8     Tsoil(0:macp)  
!      I     I4     numnodNew       
!      I     R8     dzNew(macp) 
!      I     R8     DiPoCp(macp)
!      I     R8     IAvFrMpWlWtDm1(macp)
!      I     R8     IAvFrMpWlWtDm2(macp)
!      I     R8     IQExcMtxDm1Cp(macp)
!      I     R8     IQExcMtxDm2Cp(macp)
!      I     R8     IQOutDrRapCp(macp)
!      I     R8     VlMpStDm1
!      I     R8     VlMpStDm2
!      O     I4     botcomNew(maho) 
!      O     R8     thetaNew(macp)  
!      O     R8     inqNew(macp)    
!      O     R8     inqrotNew(macp) 
!      O     R8     inqdraNew(macp) 
!      O     R8     TsoilNew(0:macp) 
!      O     R8     DiPoCpNew(macp)
!      O     R8     IAvFrMpWlWtDm1New(macp)
!      O     R8     IAvFrMpWlWtDm2New(macp)
!      O     R8     IQExcMtxDm1CpNew(macp)
!      O     R8     IQExcMtxDm2CpNew(macp)
!      O     R8     IQOutDrRapCpNew(macp)
!      O     R8     VlMpStDm1New(macp)
!      O     R8     VlMpStDm2New(macp)
!***********************************************************************
!---- Declarations
      IMPLICIT NONE
      include 'arrays.fi'
!     global
      integer   SwDiscrVert,part,Swop,numnod,numlay,numnodNew,nrlevs
      integer   botcom(maho),botcomNew(maho)
      real*8    cofgen(12,macp)
      real*8    dz(macp),h(macp),hNew(macp),theta(macp),
     &          inqrot(macp),inq(macp+1),inqdra(Madr,macp),Tsoil(0:macp)
      real*8    dzNew(macp),thetaNew(macp),
     &          inqrotNew(macp),inqNew(macp+1),
     &          inqdraNew(Madr,macp),TsoilNew(0:macp)
      real*8    IThetaBeg(MaCp), IThetaBegNew(MaCp)
      real*8    DiPoCp(macp), FrArMtrx(macp),IAvFrMpWlWtDm1(macp)
      real*8    IAvFrMpWlWtDm2(macp),IQExcMtxDm1Cp(macp)
      real*8    IQExcMtxDm2Cp(macp),IQOutDrRapCp(macp), VlMpStDm1(macp)
      real*8    VlMpStDm2(macp),DiPoCpNew(macp), IAvFrMpWlWtDm1New(macp) 
      real*8    IAvFrMpWlWtDm2New(macp), IQExcMtxDm1CpNew(macp)
      real*8    IQExcMtxDm2CpNew(macp), IQOutDrRapCpNew(macp)
      real*8    VlMpStDm1New(macp), VlMpStDm2New(macp)
      integer   swsophy,numtab(macp)
      real*8    sptab(5,macp,matab)

!     local
      integer   lay,node,nodeN,nodeNew(macp,2),i,level
      real*8    disnodNew(macp+1),prhead,total,zNew(macp)
      real*8    CofgenNew(12,macp)
      character Message*80,ModuleName*80

      SAVE

! --- error in call of part
      if (part.lt.1 .or. part.gt.2) then
        write(message,*) 'fatal error in variabel PART '
        call fatalerr(ModuleName,message)
      endif

! --- always convert to maintain identic writing

      if(SwDiscrVert.eq.0) then
! --    Simply copy
        numnodNew = numnod
        do lay = 1,numlay
          botcomNew(lay) = botcom(lay)
        enddo
        do node = 1,numnodNew
          dzNew(node) = dz(node)
          hNew(node) = h(node)
          thetaNew(node) = theta(node)
          IThetaBegNew(node) = IThetaBeg(node)
          inqNew(node) = inq(node)
          inqrotNew(node) = inqrot(node)
          do level=1,nrlevs
            inqdraNew(level,node) = inqdra(level,node)
          enddo
!
          if (swop.eq.2) then
             VlMpStDm1New(node)     = VlMpStDm1(node)
             VlMpStDm2New(node)     = VlMpStDm2(node)
             DiPoCpNew(node)        = DiPoCp(node)
             IQExcMtxDm1CpNew(node) = IQExcMtxDm1Cp(node)
             IQOutDrRapCpNew(node)  = IQOutDrRapCp(node)
             IAvFrMpWlWtDm1New(node)= IAvFrMpWlWtDm1(node)
             IQExcMtxDm2CpNew(node) = IQExcMtxDm2Cp(node)
             IAvFrMpWlWtDm2New(node)= IAvFrMpWlWtDm2(node)
!
             ThetaNew(node)         = FrArMtrx(node)*Theta(node)
             IThetaBegNew(node)     = FrArMtrx(node)*IThetaBeg(node)
          endif
        enddo
        do node = 0,numnodNew
           TsoilNew(node) = Tsoil(node)
        enddo
        inqNew(numnodNew+1) = inq(numnod+1)

      else if(SwDiscrVert.eq.1) then
! ---   initial part (part 1)
        if (part.eq.1) then

!         calculate zNew = depth at bottom of new compartment
          Total = 0.0
          Do node = 1,numnodNew
            Total = Total + dzNew(node)
            zNew(node) = Total
          enddo
!         Fill array NodeNew(Numnod,1) = top old compartment and NodeNew(Numnod,2) = bottom old comp. in new comp. NodeN
          NodeN = 1
          Node = 1
          Total = 0.0
          do while (node.le.numnod) 
            Total = Total + dz(node) 
            If (ABS(zNew(NodeN)-Total).LT.1.0D-6) then
              NodeNew(NodeN,2) = Node
              NodeN = NodeN+1
            endif
            node = node+1
          enddo
          NodeNew(1,1) = 1
          Do node = 2,numnodNew
            NodeNew(Node,1) = NodeNew(Node-1,2)+1
          Enddo
!         botcomNew
          do lay = 1, numlay
            do node = 1,numnodNew
              if (NodeNew(Node,2).eq.botcom(lay)) then
                botcomNew(lay) = node
              else
                continue
              endif
            Enddo
          Enddo

!         for hNew: position of nodal points and distances between them
          zNew(1) = - 0.5*dzNew(1)
          disnodNew(1) = - zNew(1)
          do node = 2,numnodNew
            zNew(node) = zNew(node-1) - 0.5*(dzNew(node-1)+dzNew(node))
            disnodNew(node) = zNew(node-1)-zNew(node)
          enddo
!  
        endif

! ---   In both part 1 and 2: convert theta to thetaNew based on weighed average
        Do node = 1,NumNodNew
          Total = 0.0
          Do i = NodeNew(node,1),NodeNew(node,2)   
            Total = Total+dz(i)
          Enddo
          thetaNew(node) = 0.0
          IThetaBegNew(node) = 0.d0
          Do i = NodeNew(node,1),NodeNew(node,2)
            if (Swop.ne.2) then
               thetaNew(node) = thetaNew(node)+theta(i)*dz(i)/Total
               IThetaBegNew(node) = IThetaBegNew(node)+IThetaBeg(i)*
     &                              dz(i)/Total
            else
               thetaNew(node) = thetaNew(node)+FrArMtrx(i)*theta(i)*
     &                          dz(i)/Total
               IThetaBegNew(node) = IThetaBegNew(node)+FrArMtrx(i)*
     &                              IThetaBeg(i)*dz(i)/Total
            endif
          Enddo
        Enddo
! ---   convert Tsoil to TSoilNew based on weighed average
        Do node = 1,NumNodNew
          Total = 0.0
          Do i = NodeNew(node,1),NodeNew(node,2)   
            Total = Total+dz(i)
          Enddo
          TsoilNew(node) = 0.0
          Do i = NodeNew(node,1),NodeNew(node,2)
            TsoilNew(node) = TsoilNew(node)+Tsoil(i)*dz(i)/Total
          Enddo               
        Enddo

        if (swop.eq.2) then
! ---   convert VlMpStDm to VlMpStDmNew based on integration
           Do node = 1,NumNodNew
             VlMpStDm1New(node) = 0.0
             VlMpStDm2New(node) = 0.0
             Do i = NodeNew(node,1),NodeNew(node,2)
               VlMpStDm1New(node) = VlMpStDm1New(node) + VlMpStDm1(i)
               VlMpStDm2New(node) = VlMpStDm2New(node) + VlMpStDm2(i)
             Enddo               
           Enddo
! ---   convert DiPoCp to DiPoCpNew based on weighed average
           Do node = 1,NumNodNew
             Total = 0.0
             Do i = NodeNew(node,1),NodeNew(node,2)   
               Total = Total+dz(i)
             Enddo
             DiPoCpNew(node) = 0.0
             Do i = NodeNew(node,1),NodeNew(node,2)
               DiPoCpNew(node) = DiPoCpNew(node)+DiPoCp(i)*dz(i)/Total
             Enddo               
           Enddo
        endif

! ---   only in dynamic part (part 2)
        if (part.eq.2) then

! -       Determine hNew
          do node = 1,numnodNew
!          
! - Bug fixed: Rob H 28 okt 2011  !  this output for tabulated soil physics is suppressed
             do i = 1, 9
                cofgenNew(i,node) = cofgen(i,NodeNew(node,2)) ! CofgenNew Newnode = Cofgen bottom old node
             enddo
             
            hNew(node) = prhead (node,disnodNew(node),
     &               cofgenNew,thetaNew(node),hNew,swsophy,numtab,sptab)
          enddo

! ---     convert inqrot to inqrotNew based on integration
          Do node = 1,NumNodNew
            inqrotNew(node) = 0.0
            Do i = NodeNew(node,1),NodeNew(node,2)
              inqrotNew(node) = inqrotNew(node)+inqrot(i)
            Enddo               
          Enddo
! ---     convert inq to inqNew
          Do node = 1,NumNodNew
            inqNew(node) = inq(NodeNew(node,1))
          Enddo
          inqNew(NumNodNew+1) = inq(numnod+1)
! ---     convert inqdra to inqdraNew based on integration
          Do level = 1,nrlevs
            Do node = 1,NumNodNew
              inqdraNew(level,node) = 0.0
              Do i = NodeNew(node,1),NodeNew(node,2)
                inqdraNew(level,node) = inqdraNew(level,node) +
     &                                 inqdra(level,i)
              Enddo               
            Enddo
          Enddo
        endif

! ---   convert macropore fluxes to New fluxes based on integration, and 
!       IAvFrMpWlWtDm1 to IAvFrMpWlWtDm1New, based on weighed average
        if (swop.eq.2) then
          Do node = 1,NumNodNew
            Total = 0.0
            Do i = NodeNew(node,1),NodeNew(node,2)   
              Total = Total+dz(i)
            Enddo
            IQExcMtxDm1CpNew(node) = 0.0
            IQExcMtxDm2CpNew(node) = 0.0
            IQOutDrRapCpNew(node)  = 0.0
            IAvFrMpWlWtDm1New(node) = 0.0
            IAvFrMpWlWtDm2New(node) = 0.0
            Do i = NodeNew(node,1),NodeNew(node,2)
              IQExcMtxDm1CpNew(node) = 
     &           IQExcMtxDm1CpNew(node) + IQExcMtxDm1Cp(i)
              IQExcMtxDm2CpNew(node) = 
     &           IQExcMtxDm2CpNew(node) + IQExcMtxDm2Cp(i)
              IQOutDrRapCpNew(node) = 
     &           IQOutDrRapCpNew(node)  + IQOutDrRapCp(i) 
              IAvFrMpWlWtDm1New(node) = 
     &           IAvFrMpWlWtDm1New(node) + IAvFrMpWlWtDm1(i)*dz(i)/Total
              IAvFrMpWlWtDm2New(node) = 
     &           IAvFrMpWlWtDm2New(node) + IAvFrMpWlWtDm2(i)*dz(i)/Total
            Enddo               
          Enddo
        endif

      else
        write(message,*) 'fatal error in variable SwDiscrVert'
        call fatalerr(ModuleName,message)
      endif

      return
      end

