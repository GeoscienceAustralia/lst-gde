rm(list=ls())

setwd('C:/Local Data/')

library(raster)

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

### Landsat 30 m persistent veg dataset coords 
	Land.NROWS = 2676
	Land.NCOLS = 4310

### boundary latitude and longitudes of the Landsat region
	Land.LLATS = seq(-26.85446686,by=-0.00028882271,length.out=Land.NROWS+1)
	Land.LLONS = seq(150.3679369,by=0.00028882271,length.out=Land.NCOLS+1)

### pixels of interest from Landsat data
	Land.ppp = (findInterval(-lats,-Land.LLATS) - 1)*Land.NCOLS + findInterval(lons,Land.LLONS)

### Digital surface model (250 m) clipped dataset coords 
	DSM.NROWS = 800
	DSM.NCOLS = 800

### boundary latitude and longitudes of the DSM region
	DSM.LLATS = seq(-25.99888889,by=-0.0025,length.out=DSM.NROWS+1)
	DSM.LLONS = seq(149.9986111,by=0.0025,length.out=DSM.NCOLS+1)

### pixels of interest from DSM data
	DSM.ppp = (findInterval(-lats,-DSM.LLATS) - 1)*DSM.NCOLS + findInterval(lons,DSM.LLONS)

#---------------------------------
#--- Import static data ----------
#---------------------------------
### Landsat persistent green vegetation dataset
	PGV.fl = 'L:/Home/gow03a/Data/Condamine_digitial_data/Condamine_data/pgv21.bil'
	PGV = raster(PGV.fl,navite=T)
	PGV = readBin(PGV.fl,'numeric',size=4, 2676*4310)[Land.ppp]


### SRTM digital surface model (DSM)
	DSM.fl = 'C:/Local Data/DSM_250m.bil'
	DSM = readBin(DSM.fl,'numeric',size=4,800*800)[DSM.ppp]
	DSM[DSM<=0]=0.001


### Define vegetation height output matrix
	hc.grd = matrix(NA,286*159)



#---------------------------------
### loop function:
for (k in 33:length(CURRENT.YMD)) {
#for (k in 4384:4399) {
#k=33


#---------------------------------
#--- Import 16-day data ----------
#---------------------------------
### List files
	LAI.fl = list.files('C:/Local Data/F95/Veg/LAI/','250m.flt',full.names=T)
	NDVI.fl = list.files('C:/Local Data/F95/Veg/NDVI/','250m.flt',full.names=T)

	
### create output matrix for 16-day file dates
	date = matrix(data=NA,nrow=length(LAI.fl),ncol=1,dimnames = list(NULL,'Date'))
	

### fill output matrix with file dates

		for (l in 1:length(LAI.fl)) {

			DATE = strptime(substr(LAI.fl[l],27,34),'%Y.%j')
			date[l] = format(DATE,'%Y-%m-%d EST')

		}

		
### Identify 16-day file corrsponding to CURRENT.YMD[k]
	Diff = difftime(Date[k],date,units='days')
	
	select = which(Diff <=0 & Diff > -16.5)
	
	
### Read relevant LAI and LSE files
		if(length(select>1)){
			
			select = select[1]

			LAI  = readBin(LAI.fl[select],'numeric',size=4,n=286*159)
			NDVI = readBin(NDVI.fl[select],'numeric',size=4,n=286*159)

					
		} else {

			LAI  = readBin(LAI.fl[select.LAI],'numeric',size=4,n=286*159)
			NDVI = readBin(NDVI.fl[select],'numeric',size=4,n=286*159)

		}
	
	LAI[is.na(LAI)] = 0.01

#---------------------------------
### spatial [i] loop function:
	for (i in 1:45474) {

		if (PGV[i]>100){				# woody veg; minimal temporal variablity in hc.

			hc.grd[i] = DSM[i]		

		} else {

			if (DSM[i] >2.0) {		# woody veg; minimal temporal variablity in hc.

				hc.grd[i] = DSM[i]	
		
			} else {

				if (NDVI[i]<=0.2) {
					
					#hc.grd[i] = 0.001		# fixed value for bare ground; temporally varying
					hc.grd[i] = 1.0
 				} else {

					if (LAI[i] <=2.0) {

						hc.grd[i] = 0.2	# fixed value for grasses/low crops; temporally varying

					} else {

						hc.grd[i] = 1.0	# fixed value for tall crops; temporally varying

					}
				}
			}
		}

	}	# END SPATIAL (i) LOOP


#---------------------------------
#-- Assign leaf size to hc -------
#---------------------------------	
### create leaf size grid

	s = matrix(NA,286*159)

### leaf width (m) assigned to different height ranges

	s[hc.grd <= 0.001] = 0.0001		# bare ground/water
	s[hc.grd >  0.001] = 0.0050		# grass/crop
	s[hc.grd >  0.100] = 0.0075		# shrub
	s[hc.grd >= 8.000] = 0.0200		# tree
	


#---------------------------------
#-- Calculate displacement -------
#---------------------------------
### create displacement grid

	d = matrix(NA,286*159)

### Calculate zero plane displacement (d, m)

	d = 0.50*hc.grd	# Friedl (1995)


#---------------------------------
#-- Calculate roughness length ---
#---------------------------------
### create displacement grid

	Z0 = matrix(NA,286*159)


### Calculate roughness length (Zo, m)

	Z0 = 0.05*hc.grd	# Friedl (1995)


#---------------------------------
#--- Write to file (static) ------
#---------------------------------
### Write veg height to file
	ofn = paste('C:/Local Data/F95/Veg/VegHeight/',substr(CURRENT.YMD[k],1,8),'_hc_250m.flt',sep='')
	
	writeBin(as.vector(hc.grd),ofn,size=4)


### Write leaf width to file
	ofn = paste('C:/Local Data/F95/Veg/LeafWidth/',substr(CURRENT.YMD[k],1,8),'_s_250m.flt',sep='')
	
	writeBin(as.vector(s),ofn,size=4)


### Write zero place displacement to file
	ofn = paste('C:/Local Data/F95/Veg/ZPlaneDisp/',substr(CURRENT.YMD[k],1,8),'_d_250m.flt',sep='')

	writeBin(as.vector(d),ofn,size=4)


### Write roughness length to file
	ofn = paste('C:/Local Data/F95/Veg/RoughLength/',substr(CURRENT.YMD[k],1,8),'_Z0_250m.flt',sep='')

	writeBin(as.vector(Z0),ofn,size=4)

}	# END TEMPORAL (k) LOOP


#source('C:/Local Data/Code/display.image.R')
#display.image(matrix(s,286,159,byrow=T),-26.8802,150.7572,-0.0025,0.0025,Zscale='radiance')

