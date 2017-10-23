### 16-day LAI

LAI.fl = list.files('C:/Local Data/F95/Veg/LAI/','250m.flt',full.names=T)

date.list = matrix(data=NA,nrow=length(LAI.fl),ncol=1,dimnames = list(NULL,'Date'))

for (l in 1:length(LAI.fl)) {

	DATE = strptime(substr(LAI.fl[l],27,34),'%Y.%j')
	date.list[l] = format(DATE,'%Y-%m-%d EST')

}

Diff = difftime(Date[k],date.list,units='days')
select = which(Diff <0 & Diff > -16.5)

if(length(select>1)){
			
	select = select[1]

	LAI = readBin(LAI.fl[select],'numeric',size=4,n=286*159)
				
} else {

	LAI = readBin(LAI.fl[select],'numeric',size=4,n=286*159)
		
}

	FC.phi.fl = list.files('C:/Local Data/F95/Veg/FCphi/',pattern=paste(CURRENT.YMD[k],'_FCPhi_250m.flt',sep=''),full.names=T)
	FC.phi = readBin(FC.phi.fl,'numeric',size=4,n=286*159)



### Radiation terms (W/m^2)
#G.fl = list.files('C:/Local Data/F95/Radiation/G/',pattern=paste(CURRENT.YMD[k],'_G_250m.flt',sep=''),full.names=T)
##G.fl = list.files('C:/Local Data/F95/Radiation/G/',pattern=paste(CURRENT.YMD[k],'_disag1G_250m.flt',sep=''),full.names=T)
#G = readBin(G.fl,'numeric',size=4,n=286*159)

#Rns.fl = list.files('C:/Local Data/F95/Radiation/Rns/',pattern=paste(CURRENT.YMD[k],'_Rns_250m.flt',sep=''),full.names=T)
##Rns.fl = list.files('C:/Local Data/F95/Radiation/Rns/',pattern=paste(CURRENT.YMD[k],'_disag1Rns_250m.flt',sep=''),full.names=T)
#Rns = readBin(Rns.fl,'numeric',size=4,n=286*159)

#Rnv.fl = list.files('C:/Local Data/F95/Radiation/Rnc/',pattern=paste(CURRENT.YMD[k],'_Rnc_250m.flt',sep=''),full.names=T)
##Rnv.fl = list.files('C:/Local Data/F95/Radiation/Rnc/',pattern=paste(CURRENT.YMD[k],'_disag1Rnc_250m.flt',sep=''),full.names=T)
#Rnv = readBin(Rnv.fl,'numeric',size=4,n=286*159)


### Resistance terms (s/m)
ra.fl = list.files('C:/Local Data/F95/Resistance/ra/',pattern=paste(CURRENT.YMD[k],'_ra_250m.flt',sep=''),full.names=T)
ra = readBin(ra.fl,'numeric',size=4,n=286*159)

rb.fl = list.files('C:/Local Data/F95/Resistance/rb/',pattern=paste(CURRENT.YMD[k],'_rb_250m.flt',sep=''),full.names=T)
rb = readBin(rb.fl,'numeric',size=4,n=286*159)

rss.fl = list.files('C:/Local Data/F95/Resistance/rss/',pattern=paste(CURRENT.YMD[k],'_rss_250m.flt',sep=''),full.names=T)
rss = readBin(rss.fl,'numeric',size=4,n=286*159)

rv.fl = list.files('C:/Local Data/F95/Resistance/rv/',pattern=paste(CURRENT.YMD[k],'_rv_250m.flt',sep=''),full.names=T)
rv = readBin(rv.fl,'numeric',size=4,n=286*159)

rw.fl = list.files('C:/Local Data/F95/Resistance/rw/',pattern=paste(CURRENT.YMD[k],'_rw_250m.flt',sep=''),full.names=T)
rw = readBin(rw.fl,'numeric',size=4,n=286*159)


### Instantaneous vapour pressure (EA; kPa) at time of overpass (~9:30am)
EA.fl = list.files('C:/Local Data/F95/MetData/VapourPressure/',pattern=paste(CURRENT.YMD[k],'_250m_ea.flt',sep=''),full.names=T)

EA = readBin(EA.fl,'numeric',size=4,n=286*159)

### Instantaneous air temp (Ta, degrees C) at time of overpass (~9:30am)
Ta.fl = list.files('C:/Local Data/F95/MetData/AirTemp/',pattern=paste(CURRENT.YMD[k],'_250m_Ta.flt',sep=''),full.names=T)

Ta = readBin(Ta.fl,'numeric',size=4,n=286*159)

Ta = Ta + 273.15 		# convert from degrees C to Kelvin

### Surface soil moisture (S0; mm)
SMC0_fl = list.files('C:/Local Data/F95/Soil/SMC_surface/',pattern=paste(CURRENT.YMD[k],'_SMC0_250m.flt',sep=''),full.names=T)

SMC0 =  readBin(SMC0_fl,'numeric', size=4, n=286*159)	

### Soil water potential in the top soil layer(SWP)
S0.FC  = 31.61		# volumetric soil moisture at field capacity in the top soil layer (mm)
S0.AV	= 0.15		# average available water (AV = S0.FC - S0.WP; v/v) for the study area - obtained from ASRIR
S0.WP = 0.1			# average wilting point for the study area - obtained from ASRIR

relSMC0 = SMC0 / S0.FC	

volSMC0 = relSMC0 * S0.AV + S0.WP			# in m^3

SWP = matrix(NA,length(relSMC0))

SWP[volSMC0 > 0.1] = 1.
Cond = volSMC0 < 0.1
SWP[Cond] = (volSMC0[Cond] - 0.04)/(0.1 - 0.04)
SWP[volSMC0 < 0.04] = 0.



