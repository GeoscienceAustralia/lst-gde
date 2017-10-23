rm(list=ls())

setwd('C:/Local Data/')

#---------------------------------
#--- Define Area of Interest -----
#---------------------------------
### 0.5 degree grid coordinates of interst
#	ROI_latlongs = read.csv('C:/Local Data/Code/ROI_BAWAP_CentreCoords_120m.csv')
	ROI_latlongs = read.csv('C:/Local Data/Code/ROI_BAWAP_CentreCoords_250m.csv')

	lats = ROI_latlongs$latitude
	lons = ROI_latlongs$longitude

### WIND coords
	WIND.NROWS = 3501
	WIND.NCOLS = 4501

### boundary latitude and longitudes of the WIND region
	WIND.LLATS = seq(-9.995,by=-0.01,length.out=WIND.NROWS+1)
	WIND.LLONS = seq(109.995,by=0.01,length.out=WIND.NCOLS+1)

### pixels of interest from WIND data
	WIND.ppp = (findInterval(-lats,-WIND.LLATS) - 1)*WIND.NCOLS + findInterval(lons,WIND.LLONS)


#---------------------------------
#--- Import daily wind data ------
#---------------------------------
### Daily wind speeds (data stored as byte (8-bit unsigned intergers from 0-255))
# data source: X:/Climate/wind/mcvicar_etal_grl2008/data/1km

	WindSpeed = list.files('C:/Local Data/Temp/Wind',pattern=paste('.byt',sep=''),full.names=T)

#---------------------------------
### loop function:
for (j in 1:length(WindSpeed)) {
#j=1

	u =  readBin(WindSpeed[j],'integer', size=1, signed = FALSE, n=3501*4501)[WIND.ppp]
		

#---------------------------------
#--- Convert DN to wind speed ----
#---------------------------------
### Convert from digital number to wind speed (u, m/s) 

	u = u/15.875


#---------------------------------
#--- Write files -----------------
#---------------------------------
### Write to file
#	ofn = paste('C:/Local Data/F95/MetData/WindSpeed/',substr(WindSpeed[j],25,49),'.flt',sep='')
	ofn = paste('C:/Local Data/F95/MetData/WindSpeed/','AustWindV2_250m_', substr(WindSpeed[j],39,46),'.flt',sep='')

	writeBin(as.vector(u),ofn,size=4)

}



