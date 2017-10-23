# This script converts 


rm(list=ls())			# clear all previous code

setwd('C:/Local Data/')		# set working directory


X = read.csv('C:/Local Data/Code/CB_Date_Range.csv')
Date = strptime(X[,1],'%Y-%m-%d') 
CURRENT = as.POSIXlt(Date)  
CURRENT.YMD = format(CURRENT,'%Y%m%d')

### grid coordiantes for aggregation of pixel
ngrid = read.csv('C:/Local Data/Diagnostics/csv_files/AggregatePixels.csv')


OUT = matrix(NA,3192,4617)

for (k in 33:4649) {

	TsMod.fl = list.files('C:/Local Data/F95/Outputs/Temperature/Ts_NCEPderived/TsMod_250m/',
		pattern=paste(CURRENT.YMD[k],'_Ts_250m.flt',sep=''),full.names=T)

	if (length(TsMod.fl) > 0) {

		TsMod = readBin(TsMod.fl,'numeric',size=4,286*159)
		
		TsMod.grd = matrix(NA,3192,1)

		for (i in 1:3192) {
		
			ngrid.ag = which(ngrid[,4]==i)
		
			TsMod.grd[i,] = mean(TsMod[ngrid.ag],na.rm=T)

		}

		OUT[,k-32] = signif(TsMod.grd,5)

	} else {
	
		OUT[,k-32] = NA

	}
	
}

OUT[is.nan(OUT)] = NA

ofn= paste('C:/Local Data/Diagnostics/flt_files/TsMod_1km_NCEPderived.flt',sep='')
writeBin(as.vector(OUT),ofn,size=4)
