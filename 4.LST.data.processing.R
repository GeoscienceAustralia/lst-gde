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
	ROI_latlongs = read.csv('C:/Local Data/Code/ROI_BAWAP_CentreCoords_1km_aggregated.csv')

	lats = ROI_latlongs$latitude
	lons = ROI_latlongs$longitude

### MODIS 1 km dataset coords
	MODIS.NROWS = 3726
	MODIS.NCOLS = 4790

### boundary latitude and longitudes of the MODIS region 
# see http://remote-sensing.nci.org.au/u39/public/html/modis/lpdaac-mosaics-cmar/templates/
	MODIS.LLATS = seq(-10.,by=-0.009394,length.out=MODIS.NROWS+1)
	MODIS.LLONS = seq(110.,by= 0.009394,length.out=MODIS.NCOLS+1)


### pixels of interest from MODIS data
	MODIS.ppp = (findInterval(-lats,-MODIS.LLATS) - 1)*MODIS.NCOLS + findInterval(lons,MODIS.LLONS)


fl = 	list.files('G:/LST_grd',pattern='.envi',full.names=T)

seq = seq(1,length(fl),by=2)

fl = fl[seq]


#---------------------------------
### loop [temporal/date] function:
for (k in 1:length(fl)) {
#k = 1   


### import NDVI files
	LST = readBin(fl[k],'integer',size=2,n=3726*4790)[MODIS.ppp]

	LST = LST * 0.02	# adjust for scaling factor (possible NDVI range: -1 to 1; -3000 = null)


### write files
	ofn = paste('C:/Local Data/F95/LST/MODIS_LST_1km/',substr(fl[k],20,27),'_MODIS_LST_1km.flt',sep='')
	writeBin(as.vector(LST),ofn,size=4)


}

#source('C:/Local Data/Code/display.image.R')
#display.image(matrix(LST,286,159,byrow=T),-26.8802,150.7572,-0.00250,0.00250,Zscale='radiance')
