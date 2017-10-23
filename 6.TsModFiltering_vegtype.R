# This script removes valuse from the TsMod time-series where
# LAI <=-0.5 and hc <=0.5 m. My research has found the model 
# does not operate well under these conditions.
# The script also removes the (temporally-varying) systematic 
# errror from TsMod. Systematic error was estimated from grass
# grid cells. 

rm(list=ls())			# clear all previous code

setwd('C:/Local Data/')		# set working directory


### Define dates of interest
X = read.csv('C:/Local Data/Code/CB_Date_Range.csv')
Date = strptime(X[,1],'%Y-%m-%d') 
CURRENT = as.POSIXlt(Date)  
CURRENT.YMD = format(CURRENT,'%Y%m%d')

### read in data
TsMod = readBin('C:/Local Data/Diagnostics/flt_files/TsMod_1km_NCEPderived.flt','numeric',size=4,3192*4617)
hc = readBin('C:/Local Data/Diagnostics/flt_files/hc_1km_all.flt','numeric',size=4,3192*4617)
LAI = readBin('C:/Local Data/Diagnostics/flt_files/LAI_1km_all.flt','numeric',size=4,3192*4617)



### format data
TsMod.grd = matrix(data=TsMod,nrow=3192,ncol=4617)
TsMod.grd[TsMod.grd==0 | is.nan(TsMod.grd)] = NA

hc.grd = matrix(data=hc,nrow=3192,ncol=4617)
LAI.grd = matrix(data=LAI,nrow=3192,ncol=4617)


### remove systamtic error
outMod = matrix(TsMod.grd,3192,4617)


### filter data
hc.filt = which(hc.grd <= 0.5)
LAI.filt = which(LAI.grd <=0.5)

outMod[hc.filt] = NA
outMod[LAI.filt] = NA

### write to file
ofn = paste('C:/Local Data/Diagnostics/flt_files/TsMod_1km_NCEPderived_vegfilter.flt')
writeBin(as.vector(outMod),ofn,size=4)


