rm(list=ls())

library(ncdf)

setwd('C:/Local Data/')

X = read.csv('C:/Local Data/Code/CB_Sunrise.csv')

LocalTime   = strptime(X[,1],'%Y%m%d%H%M') 

CURRENT = as.POSIXlt(LocalTime)  
CURRENT.YMD = format(CURRENT,'%Y%m%d')

#---------------------------------
#--- Define Area of Interest -----
#---------------------------------
### 120 m grid coordinates of interst

#ROI_latlongs = read.csv('C:/Local Data/Code/ROI_BAWAP_CentreCoords_120m.csv')
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


#---------------------------------
#--- Identify ncd files ----------
#---------------------------------
### Identify top layer soil moisture (S0) files
SMC0.00 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/S0/2000',full.names=T)
SMC0.01 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/S0/2001',full.names=T)
SMC0.02 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/S0/2002',full.names=T)
SMC0.03 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/S0/2003',full.names=T)
SMC0.04 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/S0/2004',full.names=T)
SMC0.05 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/S0/2005',full.names=T)
SMC0.06 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/S0/2006',full.names=T)
SMC0.07 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/S0/2007',full.names=T)
SMC0.08 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/S0/2008',full.names=T)
SMC0.09 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/S0/2009',full.names=T)
SMC0.10 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/S0/2010',full.names=T)
SMC0.11 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/S0/2011',full.names=T)
SMC0.12 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/S0/2012',full.names=T)
SMC0    = c(SMC0.00,SMC0.01,SMC0.02,SMC0.03,SMC0.04,SMC0.05,SMC0.06,SMC0.07,SMC0.08,SMC0.09,SMC0.10,SMC0.11,SMC0.12)

### Identify shallow root zone moisture field (Ss) files
SMCs.00 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Ss/2000',full.names=T)
SMCs.01 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Ss/2001',full.names=T)
SMCs.02 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Ss/2002',full.names=T)
SMCs.03 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Ss/2003',full.names=T)
SMCs.04 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Ss/2004',full.names=T)
SMCs.05 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Ss/2005',full.names=T)
SMCs.06 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Ss/2006',full.names=T)
SMCs.07 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Ss/2007',full.names=T)
SMCs.08 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Ss/2008',full.names=T)
SMCs.09 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Ss/2009',full.names=T)
SMCs.10 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Ss/2010',full.names=T)
SMCs.11 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Ss/2011',full.names=T)
SMCs.12 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Ss/2012',full.names=T)
SMCs    = c(SMCs.00,SMCs.01,SMCs.02,SMCs.03,SMCs.04,SMCs.05,SMCs.06,SMCs.07,SMCs.08,SMCs.09,SMCs.10,SMCs.11,SMCs.12)


### Identify deep root zone moisture field (Sd) files
SMCd.00 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Sd/2000',full.names=T)
SMCd.01 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Sd/2001',full.names=T)
SMCd.02 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Sd/2002',full.names=T)
SMCd.03 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Sd/2003',full.names=T)
SMCd.04 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Sd/2004',full.names=T)
SMCd.05 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Sd/2005',full.names=T)
SMCd.06 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Sd/2006',full.names=T)
SMCd.07 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Sd/2007',full.names=T)
SMCd.08 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Sd/2008',full.names=T)
SMCd.09 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Sd/2009',full.names=T)
SMCd.10 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Sd/2010',full.names=T)
SMCd.11 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Sd/2011',full.names=T)
SMCd.12 = list.files('//wron/Working/Wirada/AWRA_data/runs/AWRA_L_v3_53/Daily/Sd/2012',full.names=T)
SMCd    = c(SMCd.00,SMCd.01,SMCd.02,SMCd.03,SMCd.04,SMCd.05,SMCd.06,SMCd.07,SMCd.08,SMCd.09,SMCd.10,SMCd.11,SMCd.12)


#---------------------------------
### loop function:
for (k in 1:4649) {
#k=1

#---------------------------------
#--- Surface SMC -----------------
#---------------------------------
### Read S0 ncd files	
	ex.nc = open.ncdf(SMC0[k])

	S0 	= as.vector(t(t(get.var.ncdf(ex.nc,'S0'))[681:1,]))[AWAP.ppp]	# soil moisture at surface (mm/day)

	close(ex.nc)


### Write S0 to file
	ofn = paste('C:/Local Data/F95/Soil/SMC_surface/',substr(SMCs[k],53,60),'_SMC0_250m.flt',sep='')

	writeBin(as.vector(S0),ofn,size=4)



#---------------------------------
#--- Shallow root-zone SMC -------
#---------------------------------
### Read Ss ncd files	
	ex.nc = open.ncdf(SMCs[k])

	Ss 	= as.vector(t(t(get.var.ncdf(ex.nc,'Ss'))[681:1,]))[AWAP.ppp]	# shallow root-zone soil moisture (mm/day)

	close(ex.nc)


### Write Ss to file
	ofn = paste('C:/Local Data/F95/Soil/SMC_shallow/',substr(SMCs[k],53,60),'_SMCs_250m.flt',sep='')

	writeBin(as.vector(Ss),ofn,size=4)



#---------------------------------
#--- Deep root-zone SMC ----------
#---------------------------------
### Read Sd ncd files	
	ex.nc = open.ncdf(SMCd[k])

	Sd 	= as.vector(t(t(get.var.ncdf(ex.nc,'Sd'))[681:1,]))[AWAP.ppp]	# deep root-zone soil moisture (mm/day)

	close(ex.nc)


### Write Sd to file
	ofn = paste('C:/Local Data/F95/Soil/SMC_deep/',substr(SMCd[k],53,60),'_SMCd_250m.flt',sep='')

	writeBin(as.vector(Sd),ofn,size=4)


}


