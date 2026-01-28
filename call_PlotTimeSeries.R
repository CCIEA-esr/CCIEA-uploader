### column order should be:
# Year 
# Y data
# how the y data are measured.  The will become the Yaxis lable.  Eg "Biomass" or "Number"
# time series name.  eg Lingcod.  This will become the figure title
# showmean: true or false
# showsmo: 0=data only, 1=smooth line only, 2=both
# sample input
#

CallPlotTimeSeries <-function(df,Ylabel,title,timename,varname,PERIOD,datatype,timesp,seastyp,firstyr,lastyr,showmean,showsmo,showreg,showsd,outdir,outid,Ymin,Ymax,query,usepise){

library("png")
source("PlotTimeSeries.R")

data01=data.frame()  # input data
data02=data.frame()  # stores seasonal aves, etc.

	
# read data from erddap
# usepise=="1": given SEup, SElo
if(usepise=="1"){
		data01$SEup <- data01[,3]
		data01$SElo <- data01[,4]
#		colnames(data01) <- c("time",varname,"Seup","Selo")
		}
# usepise=="2": given standard dev, compute SEup,SElo
else if(usepise=="2"){
		data01$SEup <- data01[,2]+data01[,3]
		data01$SElo <- data01[,2]-data01[,3]
	}
else{
	data01 = df
	}
#sometimes csv files have blank lines
data01<-data01[!(data01[,1]==""),]
# these 2 lines worked for reading data from ERDDAP, use for monthly data
if(datatype==0){
	data01$year<-as.POSIXct(data01[,1], origin = "1970-01-01T00:00:00Z", tz = "UTC")
	data01$index<-data01[,2]
	data01$monthyr<-as.numeric(format(data01$year,format = "%Y", tz = "UTC"))+(as.numeric(format(data01$year,format = "%m", tz = "UTC"))-1)/12
	}
#data01<-na.omit(data01)

data01$metric = Ylabel
	timetitle=substr(timename,0,nchar(timename)-1)
# Dissolved Oxygen at NH05 is not monthly averaged, but seasonal and annual are still computed
	if(outid=="cciea_OC_DO4"&&timename=="Monthly_")timetitle=""
	timeseries <- paste(timetitle,title,sep=" ")
data01$timeseries = timeseries
print(data01)

maxyr=lastyr+1
XLIM = c(as.character(firstyr), as.character(maxyr))
#subset
# get data between firstyr and maxyr
# these 2 lines worked for reading data from ERDDAP
#data01 = data01[data01$year > as.POSIXct(paste(firstyr-1,12,31,sep='-')),]
#data01 = data01[data01$year <= as.POSIXct(paste(maxyr,01,01,sep='-')),]
data01 = data01[data01$year > firstyr-1,]
data01 = data01[data01$year <= maxyr,]

print(data01)

#data01<-data01[which(data01$monthyr>=firstyr & data01$monthyr<=maxyr),]
SmoothedMths=12
	sm=1
	sr=1
	sd=1
	ss=showsmo
	if(showmean=="false")sm=0
	if(showreg=="false")sr=0
	if(showsd=="false")sd=0
	graphics.off()

	Ylab <- Ylabel
	timetitle=substr(timename,0,nchar(timename)-1)
# Dissolved Oxygen at NH05 is not monthly averaged, but seasonal and annual are still computed
	if(outid=="cciea_OC_DO4"&&timename=="Monthly_")timetitle=""
	timeseries <- paste(timetitle,title,sep=" ")
	gappy="false"
	w=7.5
	h=1.8
#	if(timesp==1)h=2.8
	plotfile=paste(timename,outid,"_",PERIOD,"_",firstyr,"_",lastyr,"_",sm,sr,sd,ss,".png",sep="")
	if(outid=="cciea_OC_DO4"&&timename=="Monthly_")plotfile=paste(outid,"_",PERIOD,"_",firstyr,"_",lastyr,"_",sm,sr,sd,ss,".png",sep="")
	LWD = 0.5
	YATS = 4
	Ylabs=NA
	yminor=NA
	te=10
	mt=1
	#stats.years = 1981:2010
	Y2 = NA
	Mar = c(0, 3.5, 0.7, 0)
	TitleCex = 0.8
	#par(mfcol=c(1,1))
png(paste(outdir,plotfile,sep=""), units="in", res=300, width=w, height=h)
#			par(pin=c(6.5,1.5))

#annual average of monthly data 
		if(timesp==2){
#			par(mar=c(4,0,0,0), oma=c(2,3,0,2), pin=c(6.5,1.5))
			X <-data01$monthyr
			Y <- data01[,2]
			Ys<-YearlyAverage(Y,X)
			Ys<-Ys[rowSums(is.na(Ys)) != ncol(Ys),] #remove only rows where both data and SD are missing, make sure data and std dev are in sync
			X<-as.numeric(rownames(Ys))
			Y<-Ys[,'mean']
			Ye<-Ys[,'sd']
			SEup<-Y+Ye
			SElo<-Y-Ye
			shade="false"
			data02=data.frame(cbind(X,X,Y,SEup,SElo))
			colnames(data02) <- c("year","monthyr","index","SEup","SElo")
			data02$metric = Ylabel
			data02$timeseries = timeseries
#			PlotTimeSeries(X,Y,Ylab,timeseries,PERIOD,showmean,showreg,showsd,shade,Ye,Ymin,Ymax,firstyr,lastyr)
#			PlotTimeSeries(data02, Y2=Y2, XLIM = XLIM, TitleCex = TitleCex, Y.axis.labels = Ylabs,yminor = yminor, LWD = LWD, Mar=Mar,  PERIOD=PERIOD, TicsEvery = te, MinorTics = mt,showbox = showreg,showmean = showmean,showstd = showsd,showenv = shade)
			}
#seasonal average of monthly data, season set by seastyp
		else if(timesp==1){
#			par(mar=c(4,0,0,0), oma=c(2,3,0,2), pin=c(6.5,1.5))
			X <-data01$monthyr
			Y <- data01[,2]
			Ys<-SeasonalAverage(Y,X)
			Yt<-as.data.frame(cbind(Ys[,seastyp,drop=FALSE],Ys[,seastyp+4,drop=FALSE]))
			Yt<-Yt[rowSums(is.na(Yt)) != ncol(Yt),] #remove only rows where both data and SD are missing, make sure data and std dev are in sync
			X<-as.numeric(rownames(Yt))
			Y<-Yt[,1]
			Ye<-Yt[,2]
			Ye[is.na(Ye)] <- 0  #set std dev missing values to 0 that are missing because only one value in average
			shade="true"
			SEup<-Y+Ye
			SElo<-Y-Ye
			data02=data.frame(cbind(X,X,Y,SEup,SElo))  #year is the same as monthyr for annual data
			colnames(data02) <- c("year","monthyr","index","SEup","SElo")
			data02$metric = Ylabel
			data02$timeseries = timeseries
#			PlotTimeSeries(data02, Y2=Y2, XLIM = XLIM, TitleCex = TitleCex, Y.axis.labels = Ylabs,yminor = yminor, LWD = LWD, Mar=Mar,  PERIOD=PERIOD, TicsEvery = te, MinorTics = mt,showbox = showreg,showmean = showmean,showstd = showsd,showenv = shade)
			}
#monthly (datatype=0) data or yearly (datatype=1) data
		else {
			# showsmo: 0=data only, 1=smooth line only, 2=both
			if (SmoothedMths>0 && showsmo>0 ) {
				X <-data01$monthyr
				Y <- data01[,2]
				Yma<-as.vector(movingAverage(Y,X,SmoothedMths))
				data01$smoothed <- Yma
				}
#			par(mar=c(4,0,0,0), oma=c(2,3,0,2), pin=c(6.5,1.5))
# note this was oddly set to true - was that a mistake? set to false 3/24/2021 because pup count image had stdev shading
			shade="false"
			data02=data01
			}
	PlotTimeSeries(data02, Y2=Y2, XLIM = XLIM, TitleCex = TitleCex, Y.axis.labels = Ylabs,yminor = yminor, LWD = LWD, Mar=Mar,  PERIOD=PERIOD, TicsEvery = te, MinorTics = mt)
	gc()
	garbage <-dev.off()
	

}  # end funtion 

 
SeasonalAverage <- function(y, x) {

	# Track the sum and count of number of non-NA items
	sw     <- rep(0, length(unique(floor(x))))
	cw     <- rep(0, length(unique(floor(x))))
	ssp    <- rep(0, length(unique(floor(x))))
	csp    <- rep(0, length(unique(floor(x))))
	ssu    <- rep(0, length(unique(floor(x))))
	csu    <- rep(0, length(unique(floor(x))))
	sf     <- rep(0, length(unique(floor(x))))
	cf     <- rep(0, length(unique(floor(x))))
	wsd    <- rep(0, length(unique(floor(x))))
	spsd   <- rep(0, length(unique(floor(x))))
	ssd    <- rep(0, length(unique(floor(x))))
	fsd    <- rep(0, length(unique(floor(x))))

	months<-round((x-floor(x))*13,digits=0)
#original	
#	wintermths<-which(months<3)
#	springmths<-which(months>2&months<6)
#	summermths<-which(months>5&months<9)
#	fallmths<-which(months>8&months<12)
#PFMC	winter = jan,feb,mar, summer = jun,jul,aug  (for now spring = apr,may,jun  fall = oct,nov,dec)sum(!is.na(x))
	wintermths<-which(months<4)
	springmths<-which(months>3&months<7)
	summermths<-which(months>6&months<10)
	fallmths<-which(months>9&months<=12)
	
	year=as.integer(floor((x)))
	#    while (year <= max(x)){
	for (i in unique(year)){
		#    	 year=(x[i])
		sw[unique(year)==i]=sum(y[wintermths][year[wintermths]==i],na.rm=TRUE)
		cw[unique(year)==i]=sum(!is.na(y[wintermths][year[wintermths]==i]))
		ssp[unique(year)==i]=sum(y[springmths][year[springmths]==i],na.rm=TRUE)
		csp[unique(year)==i]=sum(!is.na(y[springmths][year[springmths]==i]))
		ssu[unique(year)==i]=sum(y[summermths][year[summermths]==i],na.rm=TRUE)
		csu[unique(year)==i]=sum(!is.na(y[summermths][year[summermths]==i]))
		sf[unique(year)==i]=sum(y[fallmths][year[fallmths]==i],na.rm=TRUE)
		cf[unique(year)==i]=sum(!is.na(y[fallmths][year[fallmths]==i]))
		}

	# add calculation of SD to retdata
	for (i in unique(year)){
		wsd[unique(year)==i]=sd(y[wintermths][year[wintermths]==i],na.rm=TRUE)
		spsd[unique(year)==i]=sd(y[springmths][year[springmths]==i],na.rm=TRUE)
		ssd[unique(year)==i]=sd(y[summermths][year[summermths]==i],na.rm=TRUE)
		fsd[unique(year)==i]=sd(y[fallmths][year[fallmths]==i],na.rm=TRUE)
		}

	# return sum divided by count
	retdata<-as.data.frame(cbind(sw/cw,ssp/csp,ssu/csu,sf/cf,wsd,spsd,ssd,fsd))
	colnames(retdata)<-c("winter","spring","summer","fall","winter_sd","spring_sd","summer_sd","fall_sd")
	rownames(retdata)<-unique(year)

	retdata
}

YearlyAverage <- function(y, x) {

	# Track the sum and count of number of non-NA items
	ymn  <- rep(0, length(unique(floor(x))))
	ysd  <- rep(0, length(unique(floor(x))))

	months<-(x-floor(x))*13
	year=as.integer(floor((x)))
	#    while (year <= max(x)){
	for (i in unique(year)){
		#      year=(x[i])
		ymn[unique(year)==i]=mean(y[year==i],na.rm=TRUE)
		ysd[unique(year)==i]=sd(y[year==i],na.rm=TRUE)

		}

	# return sum divided by count
	retdata<-as.data.frame(cbind(ymn,ysd))
	colnames(retdata)<-c("mean","sd")
	rownames(retdata)<-unique(year)

	retdata
}

movingAverage <- function(y, x, n=12) {

	# Track the sum and count of number of non-NA items
	s     <- rep(0, length(x))
	count <- rep(0, length(x))

	year=floor(min(x))
	#    while (year <= max(x)){
	for (i in 1:length(x)){
		year=(x[i])
		s[i]=sum(y[which(x>(year-n/12)&x<(year+n/12))])
		count[i]=length(y[which(x>(year-n/12)&x<(year+n/12))])
		}

	# return sum divided by count
	s/count
}




