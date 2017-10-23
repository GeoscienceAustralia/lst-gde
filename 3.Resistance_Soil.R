rm(list=ls())

setwd('C:/Local Data/')

X = read.csv('C:/Local Data/Code/CB_Date_Range.csv')

Date = strptime(X[,1],'%Y-%m-%d') 

CURRENT = as.POSIXlt(Date)  
CURRENT.YMD = format(CURRENT,'%Y%m%d')


#---------------------------------
### loop function:
for (k in 33:length(CURRENT.YMD)) {
#k=33


#---------------------------------
#-- Import daily data ------------
#---------------------------------
### Zero plane displacement (d, m)
	d.fl = list.files('C:/Local Data/F95/Veg/ZPlaneDisp/',pattern=paste(CURRENT.YMD[k],'_d_250m.flt',sep=''),full.names=T)

	d = readBin(d.fl,'numeric',size=4,n=286*159)

### Roughness length (Z0, m)
	Z0.fl = list.files('C:/Local Data/F95/Veg/RoughLength/',pattern=paste(CURRENT.YMD[k],'_Z0_250m.flt',sep=''),full.names=T)

	Z0 = readBin(Z0.fl,'numeric',size=4,n=286*159)

### Vegetation height (hc, m)
	hc.fl = list.files('C:/Local Data/F95/Veg/VegHeight/',pattern=paste(CURRENT.YMD[k],'_hc_250m.flt',sep=''),full.names=T)

	hc = readBin(hc.fl,'numeric',size=4,n=286*159)

	hc[hc<0.001] = 0.001


### Wind speed (u, m/s at 2m height)
	WindSpeed = list.files('C:/Local Data/F95/MetData/WindSpeed/',pattern=paste('AustWindV2_250m_',CURRENT.YMD[k],'.flt',sep=''),full.names=T)

	u =  readBin(WindSpeed,'numeric', size=4, n=286*159)	

### Surface soil moisture (S0; mm)
	SMC0_fl = list.files('C:/Local Data/F95/Soil/SMC_surface/',pattern=paste(CURRENT.YMD[k],'_SMC0_250m.flt',sep=''),full.names=T)

	SMC0 = readBin(SMC0_fl,'numeric', size=4, n=286*159)	
	
#---------------------------------
#--- Calculate rw ----------------
#---------------------------------
# Resistance to heat exchange between the soil surface and the air in the canopy. Based on Friedl (1995; Eqs 7-8, 10-11)
### Wind speed at canopy height (uH; m/s) 
	#Zr = 35		# reference height (m)  
	Zr = 2.5*hc
				
	uH = (1.5 * u * log((hc - d) / Z0)) / (log ((Zr - d) / Z0))	

### Dampening coefficeint for wind speed and eddy diffusivity within the canopy
	alpha = 1.5 + 0.6 * uH

### Eddy diffusivity for heat and momentum transport at canopy height
	vk  = 0.4 		# von Karman's constant

	KH = (1.5 * vk **2 * u * (hc - d)) / (log((Zr - d) / Z0))	

### Calculate aerodynamic resistance to heat transfer at the soil surface(rw, s/m)
	Z0s = 0.01		# roughness length of the soil surface in m

	rw = ((hc * exp(alpha)) / (alpha * KH)) * (exp((-alpha * Z0s) / hc) - exp ((-alpha * (d + Z0)) / hc))

	rw[rw<=0] = 0.001

#---------------------------------
#--- Calculate rss ---------------
#---------------------------------
### convert mm soil moisture content to volumetric soil moisture content (m^3/m^3)
	S0.WP = 0.10		# wilting point (v/v)
	S0.AV	= 0.15		# available water (AV = S0.FC - S0.WP; v/v)
	S0.fc = 31.61		# soil moisture at field capacity in the top soil layer (mm)
	
	relSMC0 = SMC0 / S0.fc	# relative soil moisture content

	volSMC0 = relSMC0 * S0.AV + S0.WP			# in m^3

	
### Soil resistance to evaporation (rss; s/m)
# from Barrett & Renzullo (2009)
	rss = 5000 * exp (-15. * volSMC0)


#---------------------------------
#--- Write files -----------------
#---------------------------------
### Write rw to file
	ofn = paste('C:/Local Data/F95/Resistance/rw/',substr(CURRENT.YMD[k],1,8),'_rw_250m.flt',sep='')

	writeBin(as.vector(rw),ofn,size=4)


### Write rss to file
	ofn = paste('C:/Local Data/F95/Resistance/rss/',substr(CURRENT.YMD[k],1,8),'_rss_250m.flt',sep='')

	writeBin(as.vector(rss),ofn,size=4)


}	# END TEMPORAL (k) LOOP


#source('C:/Local Data/Code/display.image.R')
#display.image(matrix(rss,286,159,byrow=T),-26.8802,150.7572,-0.00250,0.00250,Zscale='radiance')



