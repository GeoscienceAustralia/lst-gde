rm(list=ls())

setwd('C:/Local Data/')

#---------------------------------
#--- Define Area of Interest -----
#---------------------------------
### 250 m grid coordinates of interst
	ROI_latlongs = read.csv('C:/Local Data/Code/ROI_BAWAP_CentreCoords_250m.csv')

	lats = ROI_latlongs$latitude
	lons = ROI_latlongs$longitude

### MODIS 500 m dataset coords
	MODIS.NROWS = 7451
	MODIS.NCOLS = 9580


### boundary latitude and longitudes of the MODIS region
	MODIS.LLATS = seq(-9.9976515,by=-0.004697,length.out=MODIS.NROWS+1)
	MODIS.LLONS = seq(109.9976515,by=0.004697,length.out=MODIS.NCOLS+1)


### pixels of interest from MODIS data
	MODIS.ppp = (findInterval(-lats,-MODIS.LLATS) - 1)*MODIS.NCOLS + findInterval(lons,MODIS.LLONS)

#fl = list.files('E:/Albedo_grd/WhiteSky',pattern='.envi',full.names=T)
fl = 	list.files('E:/Albedo_grd/BlackSky',pattern='.envi',full.names=T)

seq = seq(1,length(fl),by=2)

fl = fl[seq]


#---------------------------------
### loop [temporal/date] function:
for (k in 1:length(fl)) {
#k = 2   


### import NDVI files
	A = readBin(fl[k],'integer',size=2,n=7451*9580)[MODIS.ppp]
	A = A * 0.001
	A[A<0]  = NA
	A[A>=1] = NA


### write files
#	DATE = format(strptime(substr(fl[k],23,30),'%Y.%j'),'%Y%m%d')
	DATE = format(strptime(substr(fl[k],32,39),'%Y.%j'),'%Y%m%d')
#	ofn = paste('C:/Local Data/F95/Radiation/Albedo/WhiteSky/',DATE,'_WhiteSkyA_250m.flt',sep='')
	ofn = paste('C:/Local Data/F95/Radiation/Albedo/BlackSky/',DATE,'_BlackSkyA_250m.flt',sep='')
	writeBin(as.vector(A),ofn,size=4)

}  # END TEMPORAL LOOP (k) 

#source('C:/Local Data/Code/display.image.R')
#display.image(matrix(A,286,159,byrow=T),-26.8802,150.7572,-0.00250,0.00250,Zscale='radiance')
