rm(list=ls())			# clear all previous code

setwd('C:/Local Data/')		# set working directory


#---------------------------------
#--- Define function -------------
#---------------------------------
### Define function to solve F95 (six simultaneous equations) (see Friedl, 1995; 2002)

funF95 = function(a,ra,rv,rb,rw,rss,Rnv,Rns,G,SWP,ea,Ta) { 

# constants 
	rho	= 1.204	# density of air
	gamma = 0.066	# psychometric constant (kPa/degrees C)
	cp    = 1007	# specific heat capacity of air (J/(kg.K))
	c1	= 0.611	# constant used to scale water vapour pressure function (kPa)
	c2	= 17.22	# constant used to scale water vapour pressure function (-)
	c3	= 35.86	# constant used to scale water vapour pressure function (K)
	c4	= 273.15	# offset used in water vapour pressure function (K); Barrett & Renzullo (2009)

		f = numeric(length(a))

	f[1] = (rho * cp / gamma) * ((a[2] - a[1]) / (rv + rb)) + (rho * cp / rb) * (a[5] - a[4]) - Rnv
	f[2] = (rho * cp / gamma) * ((a[3] - a[1]) / (rw + rss)) + (rho * cp / rw) * (a[6] - a[4]) + G - Rns
	f[3] =  c1 * exp ((c2 * (a[5] - c4)) / (a[5] - c3)) - a[2]
	f[4] = (c1 * exp ((c2 * (a[6] - c4)) / (a[6] - c3)))* SWP - a[3]
#	f[4] = (c1 * exp ((c2 * (a[6] - c4)) / (a[6] - c3)))* exp((9.80665*SWP)/(461*a[6])) - a[3]
	f[5] = (rho * cp / gamma) * ((a[3] - a[1]) / (rw + rss)) + (rho * cp / gamma) * ((a[2] - a[1]) / (rv + rb)) - ((rho * cp) / (gamma * ra)) * (a[1] - ea)
	f[6] = (rho * cp / rw) * (a[6] - a[4]) + (rho * cp / rb) * (a[5] - a[4]) - (rho * cp / ra) * (a[4] - Ta)


#	a[1] = e0; a[2] = ev.star; a[3] = es; a[4] = T0; a[5] = Tv; a[6] = Tss;		# variables being solved for
	
#	return(f)
	return(sum(f^2))
} 


#---------------------------------
#--- Define period of interest ---
#---------------------------------
X = read.csv('C:/Local Data/Code/CB_Date_Range.csv')
Date = strptime(X[,1],'%Y-%m-%d') 
CURRENT = as.POSIXlt(Date)  
CURRENT.YMD = format(CURRENT,'%Y%m%d')


#---------------------------------
#--- Define constants ------------
#---------------------------------
	rho	= 1.204	# density of air
	gamma = 0.066	# psychometric constant (kPa/degrees C)
	cp    = 1007	# specific heat capacity of air (J/(kg.K))
	c1	= 0.611	# constant used to scale water vapour pressure function (kPa); Barrett & Renzullo (2009)
	c2	= 17.22	# constant used to scale water vapour pressure function (-); Barrett & Renzullo (2009)
	c3	= 35.86	# constant used to scale water vapour pressure function (K); Barrett & Renzullo (2009)
	c4	= 273.15	# offset used in water vapour pressure function (K); Barrett & Renzullo (2009)


#---------------------------------
### loop [temporal/date] function:
#for (k in 33:length(CURRENT.YMD)) {
k = 4565   

	source('C:/Local Data/Code/ModelData_code/F95.SolveFunction_Inputs.R')

	print(paste('...........  PROCESSING DATE',k))

	G.fl = list.files('C:/Local Data/F95/Radiation/NCEPderived/',pattern=paste(CURRENT.YMD[k],'_NCEPfine.G_250m.flt',sep=''),full.names=T)
	G = readBin(G.fl,'numeric',size=4,n=286*159)

	Rns.fl = list.files('C:/Local Data/F95/Radiation/NCEPderived/',pattern=paste(CURRENT.YMD[k],'_NCEPfine.Rns_250m.flt',sep=''),full.names=T)
	Rns = readBin(Rns.fl,'numeric',size=4,n=286*159)

	Rnv.fl = list.files('C:/Local Data/F95/Radiation/NCEPderived/',pattern=paste(CURRENT.YMD[k],'_NCEPfine.Rnc_250m.flt',sep=''),full.names=T)
	Rnv = readBin(Rnv.fl,'numeric',size=4,n=286*159)

	rv.fl = list.files('C:/Local Data/F95/Resistance/rv/',pattern=paste(CURRENT.YMD[k],'_rv_250m_NCEP.f.flt',sep=''),full.names=T)
	rv = readBin(rv.fl,'numeric',size=4,n=286*159)

#--------------------------------------
#--- OUTPUT VARIABLE DECLARATION ------
#--------------------------------------
### Create a matrix for which to fill during the spatial [i] loop.
	e0.grd  	= matrix(NA,45474,1)
	ev.star.grd = matrix(NA,45474,1)
      es.grd   	= matrix(NA,45474,1)
      T0.grd   	= matrix(NA,45474,1)
	Tv.grd	= matrix(NA,45474,1)
	Tss.grd  	= matrix(NA,45474,1)

	e0.ObjFn  	  = matrix(NA,45474,1)
	ev.star.ObjFn = matrix(NA,45474,1)
      es.ObjFn   	  = matrix(NA,45474,1)
      T0.ObjFn   	  = matrix(NA,45474,1)
	Tv.ObjFn	  = matrix(NA,45474,1)
	Tss.ObjFn  	  = matrix(NA,45474,1)


#---------------------------------
### for loop [spatial/pixel] function:

	for (i in 1:45474){   
#	for (i in 1:10)	{
#	i = 3061

		if (!is.na(Rns[i])) {

#--------------------------------------
#--- Call function -------------------- 
#--------------------------------------
### Call F95 function
			xF95 = c(1,1,1,300,300,300)

			RESULTS = optim(xF95,fn=funF95,method='BFGS',
				ra=ra[i], rv=rv[i], rb=rb[i], rw=rw[i], rss=rss[i], 
				Rnv=Rnv[i], Rns=Rns[i], G=G[i], SWP=SWP[i], 
				ea=EA[i], Ta=Ta[i])

# output value grids
			e0	  		= RESULTS$par[1]
			ev.star 		= RESULTS$par[2]
			es 	  		= RESULTS$par[3]
			T0 	  		= RESULTS$par[4]
			Tv	  		= RESULTS$par[5]
			Tss 	  		= RESULTS$par[6]

			e0.grd[i] 	   	= e0
			ev.star.grd[i] 	= ev.star
			es.grd[i] 	   	= es
			T0.grd[i] 	   	= T0
			Tv.grd[i] 	   	= Tv
			Tss.grd[i] 	   	= Tss


		} else {

			e0.grd[i] 	   	= NA
			ev.star.grd[i] 	= NA
			es.grd[i] 	   	= NA
			T0.grd[i] 	   	= NA
			Tv.grd[i] 	   	= NA
			Tss.grd[i] 	   	= NA

		}	
			

	}	# END SPATIAL (i) LOOP


#--------------------------------------
#--- Calculate residual terms --------- 
#--------------------------------------
### Calculate latent heat from the canopy (W/m^2)

LEv = rho * cp / gamma * ((ev.star.grd - e0.grd) / (rv + rb))
LEs = rho * cp / gamma * ((es.grd - e0.grd) / (rw + rss))
LE = LEv + LEs


### Calculate sensible heat from the canopy (W/m^2)

Hv = rho * cp / rb * (Tv.grd - T0.grd)
Hs = rho * cp / rw * (Tss.grd - T0.grd)
H = Hv + Hs

### Calculate land surface temperature (Ts;K)

Ts = FC.phi * Tv.grd + (1 - FC.phi) * Tss.grd

#--------------------------------------
#--- Write to file -------------------- 
#--------------------------------------
### Temperature model outputs to file (K)
	ofn = paste('C:/Local Data/F95/Outputs/Temperature/Tv/',substr(CURRENT.YMD[k],0,8),'_Tv_250m.flt',sep='')
	writeBin(as.vector(Tv.grd),ofn,size=4)

	ofn = paste('C:/Local Data/F95/Outputs/Temperature/T0/',substr(CURRENT.YMD[k],0,8),'_T0_250m.flt',sep='')
	writeBin(as.vector(T0.grd),ofn,size=4)

	ofn = paste('C:/Local Data/F95/Outputs/Temperature/Tss/',substr(CURRENT.YMD[k],0,8),'_Tss_250m.flt',sep='')
	writeBin(as.vector(Tss.grd),ofn,size=4)

	ofn = paste('C:/Local Data/F95/Outputs/Temperature/Ts/',substr(CURRENT.YMD[k],0,8),'_Ts_250m.flt',sep='')
	writeBin(as.vector(Ts),ofn,size=4)


### Vapour pressure model outputs to file (kPa)
	ofn = paste('C:/Local Data/F95/Outputs/VapourPressure/evstar/',substr(CURRENT.YMD[k],0,8),'_evstar_250m.flt',sep='')
	writeBin(as.vector(ev.star.grd),ofn,size=4)

	ofn = paste('C:/Local Data/F95/Outputs/VapourPressure/e0/',substr(CURRENT.YMD[k],0,8),'_e0_250m.flt',sep='')
	writeBin(as.vector(e0.grd),ofn,size=4)

	ofn = paste('C:/Local Data/F95/Outputs/VapourPressure/es/',substr(CURRENT.YMD[k],0,8),'_es_250m.flt',sep='')
	writeBin(as.vector(es.grd),ofn,size=4)


### Heat flux model outputs to file(W/m^2)
	ofn = paste('C:/Local Data/F95/Outputs/Heat/LEv/',substr(CURRENT.YMD[k],0,8),'_LEv_250m.flt',sep='')
	writeBin(as.vector(LEv),ofn,size=4)

	ofn = paste('C:/Local Data/F95/Outputs/Heat/LE/',substr(CURRENT.YMD[k],0,8),'_LE_250m.flt',sep='')
	writeBin(as.vector(LE),ofn,size=4)

	ofn = paste('C:/Local Data/F95/Outputs/Heat/H/',substr(CURRENT.YMD[k],0,8),'_H_250m.flt',sep='')
	writeBin(as.vector(H),ofn,size=4)


#}	# END TEMPORAL (k) LOOP


#--------------------------------------
#--- Display data --------------------- 
#--------------------------------------
source('C:/Local Data/Code/display.image.R')

#display.image(matrix(FC.phi,286,159,byrow=T),-26.8802,150.7572,-0.00250,0.00250,Zscale='radiance')
#display.image(matrix(Tv.grd,286,159,byrow=T),-26.8802,150.7572,-0.00250,0.00250,Zscale='radiance')
#display.image(matrix(Tss.grd,286,159,byrow=T),-26.8802,150.7572,-0.00250,0.00250,Zscale='radiance')
#display.image(matrix(Ts,286,159,byrow=T),-26.8802,150.7572,-0.00250,0.00250,Zscale='radiance')
#display.image(matrix(LEv,286,159,byrow=T),-26.8802,150.7572,-0.00250,0.00250,Zscale='radiance')
