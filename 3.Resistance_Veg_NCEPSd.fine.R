rm(list=ls())

setwd('C:/Local Data/')

X = read.csv('C:/Local Data/Code/CB_Date_Range.csv')

Date = strptime(X[,1],'%Y-%m-%d') 

CURRENT = as.POSIXlt(Date)  
CURRENT.YMD = format(CURRENT,'%Y%m%d')

### grid coordiantes for aggregation of pixel
ngrid = read.csv('C:/Local Data/Diagnostics/csv_files/AggregatePixels.csv')
ngrid[ngrid==-9999] = NA

#---------------------------------
#--- Define Area of Interest -----
#---------------------------------
### 250 m grid coordinates of interst

ROI_latlongs = read.csv('C:/Local Data/Code/ROI_BAWAP_CentreCoords_250m.csv')

lats = ROI_latlongs$latitude
lons = ROI_latlongs$longitude

### AWAP coords

AWAP.NROWS = 681
AWAP.NCOLS = 841

### boundary latitude and longitudes of the AWAP region

AWAP.LLATS = seq(-9.975,by=-0.05,length.out=AWAP.NROWS+1)
AWAP.LLONS = seq(111.975,by=0.05,length.out=AWAP.NCOLS+1)

### pixels of interest from AWAP dataset 
AWAP.ppp = (findInterval(-lats,-AWAP.LLATS) - 1)*AWAP.NCOLS + findInterval(lons,AWAP.LLONS)


AWAP.Sd = readBin('C:/Local Data/Diagnostics/flt_files/AWAPSd_1km_aggregated.flt','numeric',size=4,1*4617)		# mean aggregation for study area
#AWAP.Sd = AWAP.Sd[1:3518]

NCEP.Sd = read.csv('C:/Local Data/Diagnostics/csv_files/Reanalysis_Sdflux_UTC0000.csv')
NCEP.Sd = NCEP.Sd[33:4649,2]

AWAP1km.Sd = readBin('C:/Local Data/Diagnostics/flt_files/AWAPSd_1km_all.flt','numeric',size=4,3192*4617)
AWAP1km.Sd = matrix(data=AWAP1km.Sd,nrow=3192,ncol=4617)

AWAP250m.Sd = readBin('C:/Local Data/Diagnostics/flt_files/AWAPSd_250m_all.flt','numeric',size=4,45474*4617)
AWAP250m.Sd = matrix(data=AWAP250m.Sd,nrow=45474,ncol=4617)





#---------------------------------
### loop function:
#for (k in 33:length(CURRENT.YMD)) {
for (k in 33:4649) {					# truncated time-series
#k=33


#---------------------------------
#--- Import files ------------
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

### Leaf size (s, m)
	s.fl = list.files('C:/Local Data/F95/Veg/LeafWidth/',pattern=paste(CURRENT.YMD[k],'_s_250m.flt',sep=''),full.names=T)

	s = readBin(s.fl,'numeric',size=4,n=286*159)

### Surface soil moisture (S0; mm/day)
	SMC0_fl = list.files('C:/Local Data/F95/Soil/SMC_surface/',pattern=paste(CURRENT.YMD[k],'_SMC0_250m.flt',sep=''),full.names=T)

	SMC0 =  readBin(SMC0_fl,'numeric', size=4, n=286*159)	

### Shallow root-zone soil moisture (Ss; mm/day)
	SMCs_fl = list.files('C:/Local Data/F95/Soil/SMC_shallow/',pattern=paste(CURRENT.YMD[k],'_SMCs_250m.flt',sep=''),full.names=T)

	SMCs =  readBin(SMCs_fl,'numeric', size=4, n=286*159)


### Incident solar radiation (Sd, W/m^2) at time of overpass 
	Sd = AWAP250m.Sd[,k-32] / AWAP.Sd[k-32] * NCEP.Sd[k-32]


### Instantaneous vapour pressure (EA; kPa) at time of overpass (~9:30am)
	EA.fl = list.files('C:/Local Data/F95/MetData/VapourPressure/',pattern=paste(CURRENT.YMD[k],'_250m_ea.flt',sep=''),full.names=T)

	EA = readBin(EA.fl,'numeric',size=4,n=286*159)

### Instantaneous air temp (Ta, degrees C) at time of overpass (~9:30am)
	Ta.fl = list.files('C:/Local Data/F95/MetData/AirTemp/',pattern=paste(CURRENT.YMD[k],'_250m_Ta.flt',sep=''),full.names=T)

	Ta = readBin(Ta.fl,'numeric',size=4,n=286*159)

	Ta = Ta + 273.15 		# convert from degrees C to Kelvin

### Wind speed (u, m/s at 2m height)
	WindSpeed = list.files('C:/Local Data/F95/MetData/WindSpeed/ModelInput_250m/',pattern=paste('AustWindV2_250m_',CURRENT.YMD[k],'.flt',sep=''),full.names=T)

	u =  readBin(WindSpeed,'numeric', size=4, n=286*159)	


#---------------------------------
#--- Import 16-day data ----------
#---------------------------------
### List files
	LAI.fl = list.files('C:/Local Data/F95/Veg/LAI/','250m.flt',full.names=T)

### create output matrix for 16-day file dates
	date.list = matrix(data=NA,nrow=length(LAI.fl),ncol=1,dimnames = list(NULL,'Date'))


### fill output matrix with file dates

		for (l in 1:length(LAI.fl)) {

			DATE = strptime(substr(LAI.fl[l],27,34),'%Y.%j')
			date.list[l] = format(DATE,'%Y-%m-%d EST')

		}


### Identify 16-day file corrsponding to CURRENT.YMD[k]
	Diff = difftime(Date[k],date.list,units='days')

	select = which(Diff <0 & Diff > -16.5)


### Read relevant LAI and LSE files
		if(length(select>1)){
			
			select = select[1]

			LAI = readBin(LAI.fl[select],'numeric',size=4,n=286*159)

		} else {

			LAI = readBin(LAI.fl[select],'numeric',size=4,n=286*159)
			
		}


#---------------------------------
#--- Calculate vpd at overpass ---
#---------------------------------
### Calculate saturated vapour pressure (EA.star; kPa)	
	c1 = 0.611		# kPA; constants from Barrett & Renzullo (2009)
	c2 = 17.22
	c3 = 35.86		# K	
	c4 = 273.15		# K

	EA.star = c1 * exp((c2 * (Ta - c4)) / (Ta - c3))	 

### Calculate the difference between saturation vapour pressure for given temperature and the measured vapour pressure
	vpd = EA.star - EA


#---------------------------------
#--- Calculate scaling functions -
#---------------------------------
### Relative wetness (dimensionless); linear scaling function of soil water availablity
# input data units not important as BETA is scaled b/w 0-1
	S0.max = 31.61		# field capacity storage of top layer (mm)
	Ss.max = 31.6305		# field capacity storage of shallow root zone layer (mm)
	Sd.max = 1968.295		# field capacity storage of deep root zone layer (mm)

	BETA = (SMC0 + SMCs) / (S0.max + Ss.max)

	BETA[BETA>1] = 1
	BETA[BETA<0] = 0


### Scaling function of stomatal conductance for incoming shortwave radiation (sf.l). (Cox et al., 1998, based on Jarvis, 1976; Eq4)
	p2 = 406 	# light limitation scaling parameter. Derived by least square fitting to obs data at a grassland site. 
	sf.l = (Sd / 1000) * (1000 + p2) / (Sd + p2)

### Scaling function of stomatal conductance for vapour pressure deficit (sf.vpd). (Cox et al., 1998, based on Jarvis, 1976; Eq6)
	p3 = 0.4	# vpd limitation scaling parameter. Derived by least square fitting to obs data at a grassland site.
	sf.vpd = 1 - p3 * vpd

### Scaling function of stomatal conductance for the average profile soil moisture content (sf.sm). (Cox et al., 1998, based on Jarvis, 1976; Eq7)
	p4 = -0.3	# plant available limitation scaling parameter. Derived by least square fitting to obs data at a grassland site.
	sf.sm = (1 - exp(-p4 * BETA)) / (1 - exp(-p4))

#---------------------------------
#--- Calculate stomatal resist. --
#---------------------------------
### Min/max LEAF stomatal conductance 
	gmin = 0.0002		# minimum leaf stomatal conductance (m/s)
	gmax = 0.025		# maximum leaf stomatal conductance possible [OR 0.015???]

### Min/max CANOPY stomatal conductance 
	Gmin = gmin * LAI		# minimum canopy stomatal conductance 
	Gmax = gmax * LAI		# maximum canopy stomatal conductance possible 

### Calculate CANOPY stomatal conductance (G)
	G = Gmax * sf.l * sf.vpd * sf.sm + Gmin

### Calculate CANOPY resistance (rv; s/m) at time of overpass
	rv = 1 / G


#---------------------------------
#--- Calculate boundary resist. --
#---------------------------------
# Based on Friedl (1995)
### Calculate aerodynamic resistance to heat transfer from the leaf surfaces to the air within the canopy (rb; m/s) 
 
	#Zr = 35				# reference height (m) 
	Zr = 2.5*hc

	uH = (1.5 * u * log((hc - d) / Z0)) / (log ((Zr - d) / Z0))		# Wind speed at canopy height (uH; m/s) 

	alpha = 1.5 + 0.6 * uH		# Dampening coeeficeint for wind speed and eddy diffusivity within the canopy

	rb = 50 / (alpha * LAI * (1 - exp(-alpha / 2))) * sqrt(s / uH)	


#---------------------------------
#--- Write files -----------------
#---------------------------------
### Write rv to file
#	ofn = paste('C:/Local Data/F95/Resistance/rv/',substr(CURRENT.YMD[k],1,8),'_rv_250m.flt',sep='')
	ofn = paste('C:/Local Data/F95/Resistance/rvNCEPderived/',substr(CURRENT.YMD[k],1,8),'_rv_250m_NCEP.f.flt',sep='')

	writeBin(as.vector(rv),ofn,size=4)

### Write rb to file
#	ofn = paste('C:/Local Data/F95/Resistance/rb/',substr(CURRENT.YMD[k],1,8),'_rb_250m.flt',sep='')

#	writeBin(as.vector(rb),ofn,size=4)

}	# END TEMPORAL (k) LOOP


#source('C:/Local Data/Code/display.image.R')
#display.image(matrix(rb,286,159,byrow=T),-26.8802,150.7572,-0.00250,0.00250,Zscale='radiance')




