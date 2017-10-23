rm(list=ls())

setwd('C:/Local Data/')

Y = read.csv('C:/Local Data/Code/CB_Date_Range.csv')

Date = strptime(Y[,1],'%Y-%m-%d')

#---------------------------------
#--- Define Area of Interest -----
#---------------------------------
### 0.5 degree grid coordinates of interst
#	ROI_latlongs = read.csv('C:/Local Data/Code/ROI_BAWAP_CentreCoords_120m.csv')
	ROI_latlongs = read.csv('C:/Local Data/Code/ROI_BAWAP_CentreCoords_250m.csv')

	lats = ROI_latlongs$latitude
	lons = ROI_latlongs$longitude

### AWAP coords	
	AWAP.NROWS = 681
	AWAP.NCOLS = 841

### boundary latitude and longitudes of the AWAP region
	AWAP.LLATS = seq(-9.975,by=-0.05,length.out=AWAP.NROWS+1)
	AWAP.LLONS = seq(111.975,by=0.05,length.out=AWAP.NCOLS+1)

### pixels of interest from specified dataset (AWAP)
	AWAP.ppp = (findInterval(-lats,-AWAP.LLATS) - 1)*AWAP.NCOLS + findInterval(lons,AWAP.LLONS)


CURRENT = as.POSIXlt(Date)  
CURRENT.YMD = format(CURRENT,'%Y%m%d')

#---------------------------------
### loop function:
for (k in 1:length(CURRENT.YMD)) {
#k=1

#---------------------------------
#--- Import daily AWAP data ------
#---------------------------------
### Daily maximum temperature (TMX, degrees C)
	tmaxFilename   = list.files(paste('X:/Climate/bawap/flt/tmax/day/',substr(CURRENT.YMD[k],1,4),sep=''),pattern=paste(CURRENT.YMD[k],'.flt',sep=''),full.names=T)
	
	TMX   = readBin(tmaxFilename,'numeric',size=4,n=681*841)[AWAP.ppp]


### Daily minimum temperature (TMN, degrees C)
	tminFilename   = list.files(paste('X:/Climate/bawap/flt/tmin/day/',substr(CURRENT.YMD[k],1,4),sep=''),pattern=paste(CURRENT.YMD[k],'.flt',sep=''),full.names=T)
	
	TMN   = readBin(tminFilename,'numeric',size=4,n=681*841)[AWAP.ppp]

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
	Time = 9.5		# Landsat overpass at approximately 9:30am. SHOULD THIS BE REPLACED WITH ACTUAL TIME LATER?

#	Landsat.fl = list.files(paste('C:/#FILE LOCATION#/',substr(CURRENT.YMD[k],0,8),'_LST.flt',sep=''),full.names=T)
#	Time = as.numeric(format(substr(Landsat.fl,X,X),'%H%M')
	### UPDATE THIS TO REFLECT TIME OF LANDSAT OVERPASS


#---------------------------------
#--- Calculate Air Temperature ---
#---------------------------------
# at time of overpass following Parton & Logan (1981) and McVicar & Jupp (1999)
	A = 1.8
	C = 0.88

### What is BB????	
	BB = 12-(Day.Length/2)+C

### No. of hrs after the min. temperature occurs until sunset 
	BBD = Time-BB
	

### Instantaneous air temperature at time of overpass (in degrees C)
	AirTemp = TMN + (TMX - TMN)*sin((pi*BBD)/(Day.Length + 2*A))



#---------------------------------
#--- Write to file ---------------
#---------------------------------
#	ofn = paste('C:/Local Data/F95/MetData/AirTemp/',substr(CURRENT.YMD[k],0,8),'_bawap_Ta.flt',sep='')
	ofn = paste('C:/Local Data/F95/MetData/AirTemp/',substr(CURRENT.YMD[k],0,8),'_250m_Ta.flt',sep='')

	writeBin(as.vector(AirTemp),ofn,size=4)


}	# END TEMPORAL (k) LOOP
