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

### pixels of interest from AWAP dataset 
	AWAP.ppp = (findInterval(-lats,-AWAP.LLATS) - 1)*AWAP.NCOLS + findInterval(lons,AWAP.LLONS)

CURRENT = as.POSIXlt(Date)
CURRENT.YMD = format(CURRENT,'%Y%m%d')


#---------------------------------
### Import loop function:
for (k in 1:length(CURRENT.YMD)) {
#k=1

#---------------------------------
#--- Define times of Interest ----
#---------------------------------
### Calculate time of day (in hours) of Landsat overpass 
	Time = 9.5		# Landsat overpass at approximately 9:30am. SHOULD THIS BE REPLACED WITH ACTUAL TIME LATER?

#	Landsat.fl = list.files(paste('C:/#FILE LOCATION#/',substr(CURRENT.YMD[k],0,8),'_LST.flt',sep=''),full.names=T)
#	Time = as.numeric(format(substr(Landsat.fl,X,X),'%H%M')
	### UPDATE THIS TO REFLECT TIME OF LANDSAT OVERPASS

#---------------------------------
#-- Import vapour pressure data --
#---------------------------------
### Vapour pressure (e.a09; hPA) at 9am
	VP09 = list.files(paste('X:/Climate/bawap/flt/vph09/day/',substr(CURRENT.YMD[k],1,4),sep=''),pattern=paste(CURRENT.YMD[k],'.flt',sep=''),full.names=T)

	EA09 = readBin(VP09,'numeric',size=4,n=681*841)[AWAP.ppp]
		

### Vapour pressure (VP; hPA) at 3pm
	VP15 = list.files(paste('X:/Climate/bawap/flt/vph15/day/',substr(CURRENT.YMD[k],1,4),sep=''),pattern=paste(CURRENT.YMD[k],'.flt',sep=''),full.names=T)

	EA15 = readBin(VP15,'numeric',size=4,n=681*841)[AWAP.ppp]
		

#---------------------------------------------
#--- Vapour Pressure -------------------------
#---------------------------------------------
### convert from hPA to kPA
	EA09 = EA09 / 10
	EA15 = EA15 / 10

# McVicar & Jupp (1999) discuss a method of VP estimation, however, it requires input of net radiation which is currently unknown.

### calculate vapour pressure at time of overpass using linear interpolation
#	y = y1 + (((x-x1)(y2-y1))/(x2-x1))

	EA = EA09 + (((Time-9)*(EA15-EA09))/(15-9))

	
#---------------------------------
#--- Write to file ---------------
#---------------------------------
### atmospheric transmittance .flt file
#	ofn = paste('C:/Local Data/F95/MetData/VapourPressure/',substr(CURRENT.YMD[k],0,8),'_bawap_ea.flt',sep='')
	ofn = paste('C:/Local Data/F95/MetData/VapourPressure/',substr(CURRENT.YMD[k],0,8),'_250m_ea.flt',sep='')

	writeBin(as.vector(EA),ofn,size=4)	
	

 
}	# END SPATIAL (k) LOOP

