rm(list=ls())			# clear all previous code

setwd('C:/Local Data/')		# set working directory

X = read.csv('C:/Local Data/Code/CB_Date_Range.csv')
Date = strptime(X[,1],'%Y-%m-%d')
CURRENT = as.POSIXlt(Date)  
CURRENT.YMD = format(CURRENT,'%Y%m%d')


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

### grid coordiantes for aggregation of pixel
ngrid = read.csv('C:/Local Data/Diagnostics/csv_files/AggregatePixels.csv')
ngrid[ngrid==-9999] = NA



NCEP_temp = read.csv('C:/Local Data/Diagnostics/csv_files/TimeSeries_Analysis/Reanalysis_TempDifference.csv')

AWAP.Sd = readBin('C:/Local Data/Diagnostics/flt_files/AWAPSd_1km_aggregated.flt','numeric',size=4,1*4617)		# mean aggregation for study area
#AWAP.Sd = AWAP.Sd[1:3518]

NCEP.Sd = read.csv('C:/Local Data/Diagnostics/csv_files/Reanalysis_Sdflux_UTC0000.csv')
NCEP.Sd = NCEP.Sd[33:4649,2]

AWAP1km.Sd = readBin('C:/Local Data/Diagnostics/flt_files/AWAPSd_1km_all.flt','numeric',size=4,3192*4617)
AWAP1km.Sd = matrix(data=AWAP1km.Sd,nrow=3192,ncol=4617)

AWAP250m.Sd = readBin('C:/Local Data/Diagnostics/flt_files/AWAPSd_250m_all.flt','numeric',size=4,45474*4617)
AWAP250m.Sd = matrix(data=AWAP250m.Sd,nrow=45474,ncol=4617)



#kth = sample(33:3518,10,replace=F)

#for (kkk in 1:10) {
for (k in 33:4649) {
#for (k in 3551:4649) {

#	k = kth[kkk]
	

	NCEP.fine = AWAP250m.Sd[,k-32] / AWAP.Sd[k-32] * NCEP.Sd[k-32]

	

#---------------------------------
#--- Import daily data -----------
#---------------------------------
### Instantaneous vapour pressure (EA; kPa) at time of overpass (~9:30am)
	EA.fl = list.files('C:/Local Data/F95/MetData/VapourPressure/',pattern=paste(CURRENT.YMD[k],'_250m_ea.flt',sep=''),full.names=T)

	EA = readBin(EA.fl,'numeric',size=4,n=286*159)

### Instantaneous air temp (Ta, degrees C) at time of overpass (~9:30am)
	Ta.fl = list.files('C:/Local Data/F95/MetData/AirTemp/',pattern=paste(CURRENT.YMD[k],'_250m_Ta.flt',sep=''),full.names=T)

	Ta = readBin(Ta.fl,'numeric',size=4,n=286*159)

	Ta = Ta + 273.15 		# convert from degrees C to Kelvin


### NCEP reanalysis temperature diffference data
	DELTA.T = NCEP_temp[k,2]

#---------------------------------
#--- Import 16-day data ----------
#---------------------------------
### List files
	LSE.fl = list.files('C:/Local Data/F95/Radiation/LSE/','250m.flt',full.names=T)
	LAI.fl = list.files('C:/Local Data/F95/Veg/LAI/','250m.flt',full.names=T)
	A.fl   = list.files('C:/Local Data/F95/Radiation/Albedo/BlackSky/','A_250m.flt',full.names=T)

### create output matrix for 16-day file dates
	date.LAI = matrix(data=NA,nrow=length(LAI.fl),ncol=1,dimnames = list(NULL,'Date'))
	date.A   = matrix(data=NA,nrow=length(A.fl),ncol=1,dimnames = list(NULL,'Date'))


### fill output matrix with file dates

		for (l in 1:length(LAI.fl)) {

			DATE = strptime(substr(LAI.fl[l],27,34),'%Y.%j')
			date.LAI[l] = format(DATE,'%Y-%m-%d EST')

		}

		for (l in 1:length(A.fl)) {

			DATE = strptime(substr(A.fl[l],45,52),'%Y%m%d')
			date.A[l] = format(DATE,'%Y-%m-%d EST')
		}




### Identify 16-day file corrsponding to CURRENT.YMD[k]
	Diff.LAI = difftime(Date[k],date.LAI,units='days')
	Diff.A   = difftime(Date[k],date.A,units='days')

	select.LAI = which(Diff.LAI <0 & Diff.LAI > -16.5)
	select.A   = which(Diff.A <0 & Diff.A > -16.5)

	

### Read relevant LAI and LSE files
		if(length(select.LAI>1)){
			
			select.LAI = select.LAI[1]

			LAI = readBin(LAI.fl[select.LAI],'numeric',size=4,n=286*159)
			LSE = readBin(LSE.fl[select.LAI],'numeric',size=4,n=286*159)
		
		} else {

			LAI = readBin(LAI.fl[select.LAI],'numeric',size=4,n=286*159)
			LSE = readBin(LSE.fl[select.LAI],'numeric',size=4,n=286*159)

		}

### Read relevant albedo files
		if(length(select.A>1)){
			
			select.A = select.A[1]

			A = readBin(A.fl[select.A],'numeric',size=4,n=286*159)
					
		} else {

			A = readBin(A.fl[select.A],'numeric',size=4,n=286*159)
			

		}


#---------------------------------
#--- Define times of Interest ----
#---------------------------------
### Day of year
	yyyy = as.numeric(substr(CURRENT.YMD[k],1,4))
	mm   = as.numeric(substr(CURRENT.YMD[k],5,6))
	dd   = as.numeric(substr(CURRENT.YMD[k],7,8))
	
	get.day.of.year = function(year,month,day) {
		leap.years = seq(1968,2020,4)
		ly.adj = 0 
			if (is.element(year,leap.years)) ly.adj = 1
				          jday = 31 * (month - 1) - (trunc((month - 1) / 2))- 2 + day + ly.adj
			if (month <= 2) jday = 31 * (month - 1) + day
			if (month >  8) jday = 31 * (month - 1) - (trunc((month - 2) / 2))- 2 + day + ly.adj
	return(jday)
	}

	DOY  = get.day.of.year(yyyy,mm,dd)
	
### Revolution angle (THETA, in radians)
# CBM model from Forsythe et al. (1995) reported as most accurate
	THETA = 0.2163108 + (2*atan(0.9671396*tan(0.00860*(DOY-186))))

### Sun declination angle (DELTA, in radians)
# CBM model from Forsythe et al. (1995)reported as most accurate
	DELTA = asin(0.39795*cos(THETA))

### Day length (Day.Length, in hours; *24/2pi to get hours)
# CBM model from Forsythe et al. (1995) reported as most accurate
# Daylenth definition (p) used: sunrise/sunset when top of sun even with horizon. Alternative p values available 
	p = 0.8333
	Day.Length = 24 - (24/pi)*acos((sin((p*pi)/180)+sin((lats*pi)/180)*sin(DELTA))/(cos((lats*pi)/180)*cos(DELTA)))


### Calculate time of day (in hours) of Landsat overpass
	Time = 9.5		# APPROXIMATE TIME OF OVERPASS; SHOULD THIS BE REPLACED WITH ACTUAL TIME LATER?


### Solar hour angle at hour of overpass - How does this equation work?
	th = 2 * pi * (Time - 12) / 24


### Sine of 90 degrees minus the solar zenith (H = 90 - SolZA)
# from Paltridge & Platt (1976, pg 62). How does this equation work?
	sin.H = sin(lats * pi / 180) * sin(DELTA) + cos(lats * pi / 180) * cos(DELTA) * cos(th)

	mu = 90 - (asin(sin.H)*180/pi)	# solar zenith angle (in degrees)




#---------------------------------------------
#--- Upwelling shortwave radiation -----------
#---------------------------------------------
### Upwelling shortwave radiation (Su, W/m^2) based on Mecikalski et al. (1999)
#	A  = 0.15			# currently using an assigned value. Replace with temporal/spatial dataset?
#	A  = 0.20			# Possibly follow Monteith & Upsworth (2013; pg 119-122) and Sellers et al (1996)
					# or use existing dataset e.g. AVHRR or MODIS
	Su = A * NCEP.fine
#	Su = A * AWAP.Sd[k]



#---------------------------------------------
#--- Downwelling longwave radiation ----------
#---------------------------------------------
### Downwelling longwave radiation (Ld; W/M^2) 
# epsilon.a = clear-sky air emissivity; sigma = Stefan-Boltzmann constant 
	sigma = 5.67e-8		# W/m^2/K^4

	eta = 46.5 * EA / Ta							# Prata (1996) physical based model (cited as preferred approach by Bisht et al., 2005; Wu et al., 2012 and Monteith & Upsworth, 2013, pg72)
	epsilon.a = 1 - (1 + eta) * exp(-sqrt(1.2 + 3 * eta))		
	Ld = sigma * epsilon.a * Ta **4


#--------------------------------------
#--- Calculate net radiation ----------
#--------------------------------------
### Net radiation (Rn, W/m^2) 	# Rn = Sd - Su + Ld - Lu; Based on Mecikalski et al. (1999) and rearrangement of Monteith & Unsworth (2013; eq 8.25)
	sigma    = 5.67e-8		# Stefan-Boltzmann constant (W/m^2/K^4)
 
	Rn = NCEP.fine - Su + LSE * Ld - LSE * sigma * (Ta + DELTA.T)**4
#	Rn = AWAP.Sd[k] - Su + LSE * Ld - LSE * sigma * (Ta + DELTA.T)**4
#	Rn[Rn<=10] = NA	

	
### Calculate net radiation absorbed by the canopy surface(Rnc, W/m^2) based on Kustas and Norman (1995)
	K   = 0.45			# extinction coefficient, from Anderson et al., 1997

	Rnc = Rn * (1 - exp(-K * LAI/cos(mu*pi/180)))


### Calculate net radiation absorbed by the soil surface(Rns, W/m^2) based on Kustas and Norman (1995)
	Rns = Rn - Rnc


	
#--------------------------------------
#--- Calculate soil flux (G) ----------
#--------------------------------------	
### Soil heat flux in W/m^2
	KG  = 0.35	# soil constant that Friedl (2002) suggested varies btw 0.2 and 0.5 depending on soil and moisture content
			# Revisit this term
	
	G = KG * Rns * cos(mu*pi/180)


#--------------------------------------
#--- Write to file ( W/m^2) ----------
#--------------------------------------	
	ofn = paste('C:/Local Data/F95/Radiation/NCEPderived/Disaggregated/',substr(CURRENT.YMD[k],1,8),'_NCEPfine.Rn_250m.flt',sep='')
#	ofn = paste('C:/Local Data/F95/Radiation/NCEPderived/',substr(CURRENT.YMD[k],1,8),'_AWAPagg.Rn_250m.flt',sep='')
	writeBin(as.vector(Rn),ofn,size=4)	

	ofn = paste('C:/Local Data/F95/Radiation/NCEPderived/Disaggregated/',substr(CURRENT.YMD[k],1,8),'_NCEPfine.Rnc_250m.flt',sep='')
#	ofn = paste('C:/Local Data/F95/Radiation/NCEPderived/',substr(CURRENT.YMD[k],1,8),'_AWAPagg.Rnc_250m.flt',sep='')
	writeBin(as.vector(Rnc),ofn,size=4)

	ofn = paste('C:/Local Data/F95/Radiation/NCEPderived/Disaggregated/',substr(CURRENT.YMD[k],1,8),'_NCEPfine.Rns_250m.flt',sep='')
#	ofn = paste('C:/Local Data/F95/Radiation/NCEPderived/',substr(CURRENT.YMD[k],1,8),'_AWAPagg.Rns_250m.flt',sep='')
	writeBin(as.vector(Rns),ofn,size=4)
	
	ofn = paste('C:/Local Data/F95/Radiation/NCEPderived/Disaggregated/',substr(CURRENT.YMD[k],1,8),'_NCEPfine.G_250m.flt',sep='')
#	ofn = paste('C:/Local Data/F95/Radiation/NCEPderived/',substr(CURRENT.YMD[k],1,8),'_AWAPagg.G_250m.flt',sep='')
	writeBin(as.vector(G),ofn,size=4)	




}	# END SPATIAL (k) LOOP





