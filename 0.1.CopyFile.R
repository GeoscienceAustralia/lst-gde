

### List all relevant files

## MODIS LST
#fl = list.files('//cmar-04-cdc.it.csiro.au/work/lpdaac-mosaics/c5/v1-hdf4/aust/MOD11A1.005/',pattern ='_day_lst.hdf.gz',full.names=T,recursive=T)

## MODIS LAI
#fl = list.files('//cmar-04-cdc.it.csiro.au/work/lpdaac-mosaics/c5/v1-hdf4/aust/MOD15A2.005/',pattern ='_lai.hdf.gz',full.names=T,recursive=T)

## MODIS LST view angle
#fl = list.files('//cmar-04-cdc.it.csiro.au/work/lpdaac-mosaics/c5/v1-hdf4/aust/MOD11A1.005/',pattern ='_day_view_zenith.hdf.gz',full.names=T,recursive=T)

## MODIS quality data
fl = list.files('//cmar-04-cdc.it.csiro.au/work/lpdaac-mosaics/c5/v1-hdf4/aust/MOD11A1.005/',pattern ='_day_quality.hdf.gz',full.names=T,recursive=T)


### Temporarily copy files to new location
#file.copy(fl,'F:/LST_grd')
#file.copy(fl[132:578],'C:/temp/LAI')
#file.copy(fl,'M:/PhD/MODIS_LST_ViewAngle')
file.copy(fl[GET],'C:/Local Data/F95/LST/MODIS_LST_1km_quality')