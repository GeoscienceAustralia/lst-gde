rm(list=ls())

setwd('C:/Local Data/')


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

### MODIS 250 m dataset coords
	MODIS.NROWS = 14902
	MODIS.NCOLS = 19160

### boundary latitude and longitudes of the MODIS region
	MODIS.LLATS = seq(-10.,by=-0.002348678,length.out=MODIS.NROWS+1)
	MODIS.LLONS = seq(110., by= 0.002348678,length.out=MODIS.NCOLS+1)

### pixels of interest from MODIS data
	MODIS.ppp = (findInterval(-lats,-MODIS.LLATS) - 1)*MODIS.NCOLS + findInterval(lons,MODIS.LLONS)


fl = 	list.files('G:/NDVI_grd',pattern='.envi',full.names=T)

seq = seq(1,length(fl),by=2)

fl = fl[seq]


#---------------------------------
### loop [temporal/date] function:
for (k in 1:length(fl)) {
#k = 1   


### import NDVI files
	NDVI = readBin(fl[k],'integer',size=2,n=14902*19160)[MODIS.ppp]

	NDVI = NDVI * 0.0001	# adjust for scaling factor (possible NDVI range: -1 to 1; -3000 = null)

### Derive FC from NDVI
	a       = 0.6		# erectophile plant canopy
	NDVImin = 0.		# estimated from a selection of (091/079) atmospherically corrected NDVI landsat scenes
	NDVImax = 0.99		# estimated from a selection of (091/079) atmospherically corrected NDVI landsat scenes 

	FC = 1 - ((NDVImax - NDVI) / (NDVImax - NDVImin))**a		# Fractional cover expressed as a function of NDVI (see Choudhury et al., 1994, eq11)

	FC[FC<=0] = 0.001		# adjust for FC <= 0


### Derive LAI from FC
	G = 0.5 			# assuming homogeneous distribution of leaves. IS G AND a THE SAME THING?? 
	LAI = -(log(1-FC)/G) 	

### Derive sensor view angle FC from LAI
	phi = 35			# sensor view angle (degrees) 
	FC.phi = 1 - exp((-G * LAI) / cos(pi*phi/180.))


### Derive land surface emissivity from FC
	Emiss.s  = 0.97		# soil surface emissivity (Jimenez-Munoz et al., 2009) 	
	Emiss.v  = 0.99		# vegetation surface emissivity (Jimenez-Munoz et al., 2009)

	LSE = Emiss.v * FC.phi + (1 - FC.phi) * Emiss.s			# Land surface emissivity (see Sobrino et al., 2001)


### write files
	ofn.NDVI = paste('C:/Local Data/F95/Veg/NDVI/',substr(fl[k],21,28),'_NDVI_250m.flt',sep='')
	writeBin(as.vector(NDVI),ofn.NDVI,size=4)

	ofn.FC = paste('C:/Local Data/F95/Veg/FractionalCover/',substr(fl[k],21,28),'_FC_250m.flt',sep='')
	writeBin(as.vector(FC),ofn.FC,size=4)

	ofn.LAI = paste('C:/Local Data/F95/Veg/LAI/',substr(fl[k],21,28),'_LAI_250m.flt',sep='')
	writeBin(as.vector(LAI),ofn.LAI,size=4)

	ofn.LSE = paste('C:/Local Data/F95/Radiation/LSE/',substr(fl[k],21,28),'_LSE_250m.flt',sep='')
	writeBin(as.vector(LSE),ofn.LSE,size=4)


}  # END TEMPORAL LOOP (k) 


#source('C:/Local Data/Code/display.image.R')
#display.image(matrix(s,688,365,byrow=T),-26.8802,150.7572,-0.0025,0.0025,Zscale='radiance')


