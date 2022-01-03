subroutine SurfaceTemp_Mod(srad,Qo,tmax,tmin,alt,r,rh,lai,kdif,pleng,hrnc,dhrlai,stemp)
    
    !Surface temperature estimates based on Monteith & Unsworth equation (Principles of Envirnm. Physics 4th ed, pg 228)
    !Rn is estimated at surface level considering canopy solar radiation interception in both short and long-wave terms
    !Aerodynamic resistence was optimized for bare soil temperature measurements and related to plant height
    !An empirical fitted curve mimic the effect of plant height on aerodynamic resistance throughout crop season
    !This assumption rely on the momentum transport and increased Ra for low wind speed close to soil surface
    !Murilo Vianna - 07/24/2017
    
    implicit none
    
    !--- input variables    
    real*8 srad               !Solar global radiation (MJ m-2 d-1)
    real*8 Qo                 !Extratestrial solar radiation (MJ m-2 d-1)
    real*8 tmn                !Daily mean temperature (�C)
    real*8 tmax
    real*8 tmin               
    real*8 rh                 !Relative humidity (%)
    real*8 alt                !Altitude from sea level (m)    
    real*8 r                  !Surface albedo (0-1)      
    real*8 lai                !Leaf area index
    real*8 kdif               !extinction for difuse light beam
    real*8 pleng              !Crop height (m)
    real*8 hrnc               !Heat Resistance with no crop
    real*8 dhrlai             !Heat Resistance change with LAI
            
    !--- local variables    
    real boc                !Net short-wave income (kj s-1 m-2) 
    real bol                !Net long-wave income (kj s-1 m-2)
    real nr                 !Nt radiation at soil level (kj s-1 m-2)
    real qgt                !Transmited solar radiation (MJ m-2 d-1)
    real esat               !Saturated vapor preassure (kpa)
    real eact               !Actual vapor pressure (kpa)
    real qgc                !Solar radiation at clear-sky condition (MJ m-2 d-1)
    real y                  !Psychrometric constant (kpa �C-1)
    real yl                 !Adjusted psychrometric constant (y*) (kpa �C-1) - See Monteith & Unsworth 
    real s                  !vapor pressure slope with respect to temperature (kpa �C-1)
    real hr                 !Heat diffusion resistance (s m-1)
    real coef_a             !Minimum Aerodynamic resistance (s m-1)
    real coef_b             !Maximum Aerodynamic resistance (s m-1)
    real coef_c             !Shape coefficient for adjusted ra curve
    real*8 stemp              !surface temperature (�C) - OUTPUT
    
    !--- Constants    
    real airD               !Air density at normal conditions (kg m-3) 
    real airC               !Air specific heat (kj kg-1 �C-1)
    real atmp               !Atmospheric pressure
    real e                  !Dry air/ vapour ratio mixture
    real t_abs
    real lambda
    real sigma
    
    parameter (airD = 1.2d0)    
    parameter (airC = 1.01d0)
    parameter (atmp = 101.3d0)
    parameter (e    = 0.622d0)
    parameter (t_abs  = 273.15)
    parameter (lambda = 2.465e6)
    parameter (sigma  = 5.67477e-8)
    
    !--- Implementation
    
    !Psychrometric constant (kpa �C-1)
    y   = (airC * atmp)/(e * lambda/1000.)
    yl  = 0.93 * y !from Monteith & Unsworth
    
    !Trasnmited solar radiation to soil surface
    qgt = exp(-kdif*lai) * srad
    
    !Short-Wave income below canopy (in kj s-1 m-2)
    boc = qgt * (1 - r) * (1000.d0 / 86400.d0)
    
    !Saturated vapor pressure for mean temperature (Tetens)
    esat = 0.6108 * 10**((7.5*((tmax + tmin) / 2.))/(237.3+((tmax + tmin) / 2.)))
    eact = rh * esat / 100.
    
    qgc = (0.75 + 2.0e-5 * alt) * Qo
    
    !Long-Wave income below canopy (in kj s-1 m-2)
    bol = - (sigma * (((tmax + t_abs)**4) + ((tmin + t_abs)**4))/2.) * (0.34 - 0.14*sqrt(eact)) * (1.35 * qgt/qgc - 0.35) / 1000.
  
    !Net radiation (kj s-1 m-2)
    nr  = boc + bol
    
    s = 4098. * (0.6108 * exp((17.27 * ((tmax + tmin) / 2.))/(((tmax + tmin) / 2.) + 237.3)))/(((tmax + tmin) / 2.) + 237.3)**2.
    
    !Compute Heat diffusion resistance as function of Crop LAI   
    hr   = hrnc + dhrlai * lai !(s m-1)
    
    !--- Surface temperature (Monteith & Unsworth)
    stemp = ((tmax + tmin)/2.) + (((yl * hr / (airD * airC)) * nr) / (s + yl)) - (esat - eact)/(s + yl) 
    
    end subroutine