!Simulation Control File
 
!General Information
*Project Name
<prj.nm>

*Authors
<aut.nm>

*Contact
<email>

*Site
<site.nm>

*Simulation details
<sim.det>

*Simulation Period
!ID   Year    DOY      Year     DOY     Seq    #Seq     Rep    #Rep
!     Start   Start     End     End   (T/F)     (n)    (T/F)     (n)
<sim.id> <s.ini.yr> <s.ini.doy> <s.end.yr> <s.end.doy>	<flseq> <nseq> <flrep> <nrep>

*Fields
!ID        Weather    Latitude  Longitude   Altitude
!         File(MET)  (degrees)  (degrees)        (m)
<sim.id> <met.fn> <met.lat> <met.lon> <met.alt>

*Soil
!ID       DataBase        Soil     Runoff     Albedo    Drainage
!         File(SPD)    Profile     Curv.N                  Frac
<sim.id> <soil.fn> <soil.id> <soil.cn> <soil.alb> <swcon>

*Crops
!ID       Crop      Cv.ID    Cv.Name
<sim.id> <crop.nm> <cv.id> <cv.nm>

*Planting
!ID       Year        DOY   RowSpace PlantDepth  Ratooning       #Seq
<sim.id> <p.yr> <p.doy> <p.rowsp> <p.depth> <flratoon> <p.seq>

*Harvest
!ID       Year        DOY     HIndex       #Seq
<sim.id> <h.yr> <h.doy> <hi> <h.seq>

*Irrigation
!ID     Irrig?    IrriEff               Method
!          T/F      (0-1)                    
<sim.id> <flirrig> <eff.irrig> <irrig.met>

*Soil Surface Residue
!ID	                         Residue   Total DW      DWSAT        SSA       KRES     RESALB      HCDRY      HCWET      MAXEF
!         Year        DOY       Type      kg/ha kgH2O/kgDW      cm2/g          -        0-1		
<sim.id> <r.ini.yr> <r.ini.doy> <r.type> <r.ini.dw> <r.dwsat> <r.sra> <r.k> <r.alb> <r.hcdry> <r.hcwet> <max.es.frac>

*Simulation Options
!ID    SEVM    PETP    SSTM    STBC    PHTY    GPHM    CPAM    TILM    STAG    MUEF    POTG    SALN  
<sim.id> <sevm> <petp> <sstm> <stbc> <phty> <gphm> <cpam> <tilm> <stag> <muef> <potg> <saln>

*Methods Parameters
!ID    SLU1    HRNC    DHRL  SDRYA1  SDRYA2  SDRYB1  SDRYB2   SWETA   SWETB   SEQUA   SEQUB   TBOTM   TBOTA   TBOTI   TBOTD
<sim.id> <slu1> <hrnc> <dhrl> <sdrya1> <sdrya2> <sdryb1> <sdryb2> <sweta> <swetb> <sequa> <sequb> <tbot.mean> <tbot.ampli> <tbot.imref> <tbot.ddamp>

*Outputs Options
!ID	   DSWA    DSTE    DETP    DETA    DPHO    DCRP
<sim.id> <dswa> <dste> <detp> <deta> <dpho> <dcrp>

!OPTIONS DESCRIPTION:
!ID   - Simulation identification 		In case of more than one crop/met/soil/manag combination (1-999)
!DOY  - Day of Year (1-366)
!SEVM - Soil Evaporation method:        1 = SALUS method 2 = Ritchie method                                                               
!PETP - Potential Evapotranspiration:   1 = Pemanm-Monteith (FAO-56)2 = Priestlay-Taylor 3 = Input Eto                                   
!SSTM - Soil Surface Temperature:       1 = Empirical Relation with mulch 2 = Energy Balance Monteith & Unsworth 3 = APSIM surface method
!STBC - Soil Temp Bottom conditions:    1 = No soil heat flux at soil profile bottom 2 = Heat flux at bottom of profile (sinusoidal curve)                    
!PHTY - Photosynthesis type:            1 = C4 species 2 = C3 3 = CAM                                                                    
!GPHM - Gross Photosynthesis method:    1 = RUE 2 = CO2 assimilation curve(layered canopy) 3 = CO2 assimilation curve PHTMAX             
!CPAM - Carbon Partitioning method:     1 = Marin's Thesis 2 = Degree-days Curves 3 = Marin & Jones (2014) (disabled)                               
!TILM - Tillering method:               1 = Degree-days curve 2 = tillochron and light competition                                        
!STAG - Use Soil Temp for crop age:     T OR F (True or False) Warning: Crop degree-days parameters may change
!DSWA - Detailed Soil:                  T OR F (True or False)
!DSTE - Soil Temperature:               T OR F (True or False)
!DETP - Detailed EO POT:                T OR F (True or False)
!DETA - Detailed EO ACT:                T OR F (True or False)
!DPHO - Detailed Photosynthesis:		T OR F (True or False) Warning: Only for Canopy Layered Method (2)
!HCDRY	- Apparent Thermal Conductivity of Mulch in Dry Conditions [W m-1 K-1]
!HCWET  - Mulch Apparent Thermal Conductivity Increase by Water Concentration Increase [(dW m-1 K-1)/(KgH20)]
