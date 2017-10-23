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

### MODIS 1 km dataset coords
	MODIS.NROWS = 3726
	MODIS.NCOLS = 4790

### boundary latitude and longitudes of the MODIS region 
# see http://remote-sensing.nci.org.au/u39/public/html/modis/lpdaac-mosaics-cmar/templates/
	MODIS.LLATS = seq(-10.,by=-0.009394,length.out=MODIS.NROWS+1)
	MODIS.LLONS = seq(110.,by= 0.009394,length.out=MODIS.NCOLS+1)


### pixels of interest from MODIS data
	MODIS.ppp = (findInterval(-lats,-MODIS.LLATS) - 1)*MODIS.NCOLS + findInterval(lons,MODIS.LLONS)


fl = 	list.files('M:/PhD/ViewAngle',pattern='.envi',full.names=T)

seq = seq(1,length(fl),by=2)

fl = fl[seq]


#---------------------------------
### loop [temporal/date] function:
for (k in 1:length(fl)) {
#for (k in 21:31) {
#k = 21   

###open file
	phi = readBin(fl[k],'integer',size=1,n=3726*4790)[MODIS.ppp]
	phi[phi<0] = NA		# convert no data values to nulls
	phi = phi-65		# apply offset

	print(range(phi))

### write files
	ofn = paste('C:/Local Data/F95/LST/MODIS_LST_1km_ViewAngle/',substr(fl[k],26,33),'_MODIS_LST_1km_ViewAngle.flt',sep='')
	writeBin(as.vector(phi),ofn,size=4)

}

