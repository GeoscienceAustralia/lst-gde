rm(list=ls())

setwd('C:/Local Data/')

X = read.csv('C:/Local Data/Code/CB_Date_Range.csv')
Date = strptime(X[,1],'%Y-%m-%d')
CURRENT = as.POSIXlt(Date)  
CURRENT.YMD = format(CURRENT,'%Y%m%d')


#---------------------------------
#-- Import static data -----------
#---------------------------------


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

### Wind speed (u, m/s at 2m height)
	WindSpeed = list.files('C:/Local Data/F95/MetData/WindSpeed/',pattern=paste('AustWindV2_250m_',CURRENT.YMD[k],'.flt',sep=''),full.names=T)

	u =  readBin(WindSpeed,'numeric', size=4, n=286*159)	

#---------------------------------
#--- Calculate ra ----------------
#---------------------------------
### Calculate aerodynamic resistance to heat exchange between the air within the canopy and the air at the reference height(ra, s/m)
# Based on Friedl (1995; Eq 12)
# assume neutral conditions thus ignore stability functions for momentum and heat
	vk  = 0.4 		# von Karman's constant
	#Zr  = 35
	Zr = 2.5*hc			# height (m) is the reference height at which wind speed is measured. 
				# AWAP wind speed intepolated at 2 m above surface (i.e. 2 m above bare ground and 2 m above canopy (hc)

	ra = ((log((Zr - d) / Z0))**2) / (vk**2 * u)



#---------------------------------
#--- Write files -----------------
#---------------------------------
### Write rs to file
	ofn = paste('C:/Local Data/F95/Resistance/ra/',CURRENT.YMD[k],'_ra_250m.flt',sep='')

	writeBin(as.vector(ra),ofn,size=4)


}	# END TEMPORAL (k) LOOP


#source('C:/Local Data/Code/Friedl_code/Resistance_Veg_modBG.R')
#source('C:/Local Data/Code/Friedl_code/Resistance_Soil_modBG.R')


#source('C:/Local Data/Code/display.image.R')
#display.image(matrix(ra,286,159,byrow=T),-26.8802,150.7572,-0.0025,0.0025,Zscale='radiance')

