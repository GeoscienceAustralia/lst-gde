rm(list=ls())


library(ncdf4)


### Ground heat flux data (G, W/m^2) downloaded from NCEP
fl = list.files('C:/Local Data/ReanalysisData/GroundHeat',full.names=T)

for (k in 1:length(fl)){
#for (k in 1:3) {
#k=1
	ex.nc = nc_open(fl[k])

	data  = ncvar_get(ex.nc,'gflux')

	TIME  = ncvar_get(ex.nc,'time')

	TIME  = as.POSIXct((TIME*60*60),origin='1800-01-01',tz='GMT')	# converting hrs since origin (01/01/1800) to date/time format

	GFLUX = data[82,62,]				# selecting for 151.875 longitude; -27.618601 latitude; all time 

	nc_close(ex.nc)

	DATA = list(TIME,GFLUX)

	write.csv(DATA,file=paste('C:/Local Data/ReanalysisData/CB_flux_data/','Reanalysis_G_',substr(fl[k],57,60),'.csv',sep=''))
	
}



### Net latent heat flux data (LE, W/m^2)
# downloaded from NCEP

fl = list.files('C:/Local Data/ReanalysisData/LatentHeat_Net',full.names=T)

for (k in 1:length(fl)){

	ex.nc = nc_open(fl[k])

	data  = ncvar_get(ex.nc,'lhtfl')

	TIME  = ncvar_get(ex.nc,'time')

	TIME  = as.POSIXct((TIME*60*60),origin='1800-01-01',tz='GMT')	# converting hrs since origin (01/01/1800) to date/time format

	Lheat = data[82,62,]				# selecting for 151.875 longitude; -27.618601 latitude; all time 

	nc_close(ex.nc)

	DATA = list(TIME,Lheat)

	write.csv(DATA,file=paste('C:/Local Data/ReanalysisData/CB_flux_data/','Reanalysis_LE_',substr(fl[k],61,64),'.csv',sep=''))

}

### Downwelling longwave radiation data (Ld, W/m^2)
# downloaded from NCEP

fl = list.files('C:/Local Data/ReanalysisData/Radiation_Ld',full.names=T)

for (k in 1:length(fl)){

	ex.nc = nc_open(fl[k])

	data  = ncvar_get(ex.nc,'dlwrf')

	TIME  = ncvar_get(ex.nc,'time')

	TIME  = as.POSIXct((TIME*60*60),origin='1800-01-01',tz='GMT')	# converting hrs since origin (01/01/1800) to date/time format

	Ld = data[82,62,]				# selecting for 151.875 longitude; -27.618601 latitude; all time 

	nc_close(ex.nc)

	DATA = list(TIME,Ld)

	write.csv(DATA,file=paste('C:/Local Data/ReanalysisData/CB_flux_data/','Reanalysis_Ld_',substr(fl[k],59,62),'.csv',sep=''))

}

### Upwelling longwave radiation data (Lu, W/m^2)
# downloaded from NCEP

fl = list.files('C:/Local Data/ReanalysisData/Radiation_Lu',full.names=T)

for (k in 1:length(fl)){

	ex.nc = nc_open(fl[k])

	data  = ncvar_get(ex.nc,'ulwrf')

	TIME  = ncvar_get(ex.nc,'time')

	TIME  = as.POSIXct((TIME*60*60),origin='1800-01-01',tz='GMT')	# converting hrs since origin (01/01/1800) to date/time format

	Lu = data[82,62,]				# selecting for 151.875 longitude; -27.618601 latitude; all time 

	nc_close(ex.nc)

	DATA = list(TIME,Lu)

	write.csv(DATA,file=paste('C:/Local Data/ReanalysisData/CB_flux_data/','Reanalysis_Lu_',substr(fl[k],59,62),'.csv',sep=''))

}

### Downwelling shortwave radiation data (Sd, W/m^2)
# downloaded from NCEP

fl = list.files('C:/Local Data/ReanalysisData/Radiation_Sd',full.names=T)

for (k in 1:length(fl)){

	ex.nc = nc_open(fl[k])

	data  = ncvar_get(ex.nc,'dswrf')

	TIME  = ncvar_get(ex.nc,'time')

	TIME  = as.POSIXct((TIME*60*60),origin='1800-01-01',tz='GMT')	# converting hrs since origin (01/01/1800) to date/time format

	Sd = data[82,62,]				# selecting for 151.875 longitude; -27.618601 latitude; all time 

	nc_close(ex.nc)

	DATA = list(TIME,Sd)

	write.csv(DATA,file=paste('C:/Local Data/ReanalysisData/CB_flux_data/','Reanalysis_Sd_',substr(fl[k],59,62),'.csv',sep=''))

}

### Upwelling shortwave radiation data (Su, W/m^2)
# downloaded from NCEP

fl = list.files('C:/Local Data/ReanalysisData/Radiation_Su',full.names=T)

for (k in 1:length(fl)){

	ex.nc = nc_open(fl[k])

	data  = ncvar_get(ex.nc,'uswrf')

	TIME  = ncvar_get(ex.nc,'time')

	TIME  = as.POSIXct((TIME*60*60),origin='1800-01-01',tz='GMT')	# converting hrs since origin (01/01/1800) to date/time format

	Su = data[82,62,]				# selecting for 151.875 longitude; -27.618601 latitude; all time 

	nc_close(ex.nc)

	DATA = list(TIME,Su)

	write.csv(DATA,file=paste('C:/Local Data/ReanalysisData/CB_flux_data/','Reanalysis_Su_',substr(fl[k],59,62),'.csv',sep=''))

}

### Upwelling shortwave radiation data (Su, W/m^2)
# downloaded from NCEP

fl = list.files('C:/Local Data/ReanalysisData/SensibleHeat_Net',full.names=T)

for (k in 1:length(fl)){

	ex.nc = nc_open(fl[k])

	data  = ncvar_get(ex.nc,'shtfl')

	TIME  = ncvar_get(ex.nc,'time')

	TIME  = as.POSIXct((TIME*60*60),origin='1800-01-01',tz='GMT')	# converting hrs since origin (01/01/1800) to date/time format

	Sheat = data[82,62,]				# selecting for 151.875 longitude; -27.618601 latitude; all time 

	nc_close(ex.nc)

	DATA = list(TIME,Sheat)

	write.csv(DATA,file = paste('C:/Local Data/ReanalysisData/CB_flux_data/','Reanalysis_H_',substr(fl[k],63,66),'.csv',sep=''))
}

