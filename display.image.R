#------------------------------------------------------------------
display.image = function(X,TL.Lat=1,TL.Long=1,dLat=-0.01,dLong=0.01, XD = c(TL.Long, TL.Long + (n.col+1)*dLong), YD = c(TL.Lat + (n.row+1)*dLat, TL.Lat), ZD=range(X.2D2,na.rm=T),Zscale = "radiance") {
 
 #  subroutine to create a very simple graphical display of the satellite
 #    image data. assumes you have installed on your system the "oz" 
 #    package from  http://cran.r-project.org/ <packages: Oz>
 #    to display the Australian coastline with/without state boundaries.
 #    if this in not istalled, then comment out the next line AND 
 #    uncomment the last 5 lines below.
#        library(oz)

 #  Input arguments:
 #   X is an image stored as a 2-d matrix arranged such that: the 
 #      first row is the northern boundary, bottom row the southern
 #      boundary; first column is the western-most boundary, and last
 #      column is the eastern.
 #   TL.Lat,TL.Long are the top-left pixel corner geographic
 #      coordinates in decimal degrees. if supplied not default is (1,1).
 #   dLat,dLong are the latitude and longitude increments (cell 
 #      resolution) in decimal degrees. if not supplied default is 
 #      +/-0.01 degrees

   n.row = dim(X)[1]; n.col=dim(X)[2]
   X.2D2 = t(X)[,seq(n.row,1,-1)]# ; X.2D2[X.2D2 > 100] = 100.
   dev.new(height=6,width=7.8)
	X.2D2[X.2D2 > max(ZD)] = max(ZD) 
	if (Zscale == "rain") {
		ramp=colorRampPalette(c("white","darkred","red","yellow","darkgreen","cyan","blue","navy","purple"))
	}
	if (Zscale == "heat") {
		#ramp=colorRampPalette(rev(c("darkred","red","grey90","blue","navy")))
		ramp=colorRampPalette(rev(c("red","darkred","grey80","navy","blue")))
	}
	if (Zscale == "moisture") {
		ramp=colorRampPalette(c("sienna","gray90","steelblue4"))
	}
	if (Zscale == "radiance") {
		ramp=colorRampPalette(c("grey10","navy","steelblue1","green4","yellow","orange","darkred","grey90"))
	}
        if (Zscale == "elevation") {
                ramp=colorRampPalette(c("sandybrown","saddlebrown","olivedrab","darkgreen","springgreen2","khaki","grey40","grey100"))
        }
        if (Zscale == "vegetation") {
                ramp=colorRampPalette(c("grey97","lightgreen","greenyellow","forestgreen","darkgreen","darkolivegreen"))
        }
        if (Zscale == "ET") {
                ramp=colorRampPalette(rev(c("grey10","navy","steelblue1","green4","yellow","orange","darkred","grey90")))
        }
	if (Zscale=="three"){
		ramp = colorRampPalette(c("red","white","blue"))
	}
        if (Zscale=="onehundred"){
                ramp = colorRampPalette(rep(c("black","red","orange","yellow","green","cyan","blue","violet","white","grey"),10))
        }

     xx = seq(TL.Long,by=dLong,length.out=n.col)
     yy = seq(TL.Lat+n.row*dLat,by=-dLat,length.out=n.row)

        filled.contour(xx,yy,z=X.2D2,color.palette=ramp,nlevels=64,frame.plot=F,asp=1,
                        plot.axes={axis(1); axis(2); oz(states=T,add=T,col="black")},
                        zlim=ZD,
                        ylim=YD,
                        xlim=XD)
       filled.contour(xx,yy,z=X.2D2,color.palette=ramp,nlevels=26,frame.plot=F,asp=1,
                        plot.axes={axis(1); axis(2)},
                        zlim=range(X.2D2,na.rm=T),
                        ylim=c(TL.Lat + (n.row+1)*dLat, TL.Lat),
                        xlim=c(TL.Long, TL.Long + (n.col+1)*dLong))
}
#------------------------------------------------------------------

