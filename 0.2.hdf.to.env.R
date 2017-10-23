### Convert hdf to envi files. Required to read data into R. 
### Fucntion won't work if hd files located in a directory with spaces.

#fl = list.files('C:/temp/LAI', pattern='.hdf',full.names=T)
#fl = list.files('M:/PhD/MODIS_LST_ViewAngle', pattern='.hdf',full.names=T)
fl = list.files('G:/MODIS_LST_1km_quality', pattern='.hdf',full.names=T)

for (i in 1:length(fl)) { 
#for (i in 1:223) {



system(paste("gdal_translate -of ENVI",fl[i],paste(fl[i],'.envi',sep='')))

}