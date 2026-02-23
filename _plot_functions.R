################# run_plot ######################################

run_plot <- function(timesp, dframe, varname, ylab, title, seastyp, PERIOD,
                     firstyr, lastyr, tsreg, tssd, tsmn, tssmo,
                     outdir, outid, table_id, datatype, query, usePiSe,
                     ymin, ymax, subcomponent){
  seastyp <- as.integer(seastyp)
  PERIOD <- as.integer(PERIOD)	# Size of regression window at end of plot
  firstyr <- as.integer(firstyr)
  lastyr <- as.integer(lastyr)
  datatype<- as.integer(datatype)
  usepise=""
  if(usePiSe!="")usepise=usePiSe
  Ymin <- as.double(ymin)
  Ymax <- as.double(ymax)
  
  #outdir="../images/plots/custom/"
  timename=""
  if(table_id==0&&subcomponent=="Monthly"){   #OC
    timsns=list('Monthly','Seasonal','Annual')
    timseas=list('Winter','Spring','Summer','Fall')
    timename=paste(timsns[[as.numeric(timesp)+1]],"_",sep="")
    if(seastyp>0 && timesp==1)timename=paste(timseas[[as.numeric(seastyp)]],"_",sep="")
  }
  file_stats = CallPlotTimeSeries(dframe,ylab,title,timename,varname,PERIOD,datatype,timesp,seastyp,firstyr,lastyr,tsmn,tssmo,tsreg,tssd,outdir,outid,Ymin,Ymax,query,usepise)
  return(file_stats)
}

######################## signif_util ###########################
signif_util = function(x, digits){
  return( floor(x) + signif(x %% 1, digits) )
  }
################# CallPlotTimeSeries ###########################
# column order should be:
# Year 
# Y data
# how the y data are measured.  The will become the Yaxis lable.  Eg "Biomass" or "Number"
# time series name.  eg Lingcod.  This will become the figure title
# showmean: true or false
# showsmo: 0=data only, 1=smooth line only, 2=both
# sample input
#

CallPlotTimeSeries <-function(dframe,Ylabel,title,timename,varname,PERIOD,datatype,timesp,seastyp,firstyr,lastyr,showmean,showsmo,showreg,showsd,outdir,outid,Ymin,Ymax,query,usepise){
  
  library("png")
  
  if(datatype==0){ #monthly
    data01=data.frame(year=dframe$time, index=dframe$index)  # input data
  }else{
    data01=data.frame(year=dframe$year, index=dframe$index)  # input data
  }
  
  data02=data.frame()  # stores seasonal aves, etc.
  # usepise=="2": given standard dev, compute SEup,SElo
  if(usepise=="2"){
    data01$SEup <- dframe$index+dframe$SE
    data01$SElo <- dframe$index-dframe$SE
  }
  #sometimes csv files have blank lines
  #data01<-data01[!(data01[,1]==""),]
  
    #data01<-na.omit(data01)
   data01$metric = Ylabel
  timetitle=substr(timename,0,nchar(timename)-1)
  # Dissolved Oxygen at NH05 is not monthly averaged, but seasonal and annual are still computed
  if(outid=="cciea_OC_DO4"&&timename=="Monthly_")timetitle=""
  timeseries <- paste(timetitle,title,sep=" ")
  data01$timeseries = timeseries

  maxyr=lastyr+1
  XLIM = c(as.character(firstyr), as.character(maxyr))
  #subset
  # get data between firstyr and maxyr
  # these 2 lines worked for reading data from ERDDAP
  #data01 = data01[data01$year > as.POSIXct(paste(firstyr-1,12,31,sep='-')),]
  #data01 = data01[data01$year <= as.POSIXct(paste(maxyr,01,01,sep='-')),]
  if(!is.na(firstyr))data01 = data01[data01$year > firstyr-1,]
  if(!is.na(maxyr))data01 = data01[data01$year <= maxyr,]
  
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
  plotfile=""
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
  file_stats=PlotTimeSeries(data02, Y2=Y2, XLIM = XLIM, TitleCex = TitleCex, Y.axis.labels = Ylabs,yminor = yminor, LWD = LWD, Mar=Mar,  PERIOD=PERIOD, TicsEvery = te, MinorTics = mt)
  
  file_stats=c(file_stats,plotfilename=plotfile)
  gc()
  garbage <-dev.off()
  return(file_stats)
  
}  # end funtion 

###################### SeasonalAverage ####################
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

###################### YearlyAverage ####################
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

###################### movingAverage ####################
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

################# update_metadata_line ##################################
## update a line in the metadata
update_metadata_line <- function(metadata,info){

  metadata$ERDDAP_query_value <- as.character(metadata$ERDDAP_query_value)
  column_types <- sapply(metadata,class) # returns named vector
  col_list=as.list(column_types)
  metadata <- rows_update(metadata,info,by = "CCIEA_timeseries_ID")
  
return(metadata)
  
}

############################ update_metadata_file ##########################
## incorporate edited metadata back into full spreadsheet
## backup present spreadsheet first
## metadata_spreadsheet_folder - name of Drive folder where metadata file is located
## meta_file_search - partial name of metadata file, remainder is date and version
## Case for adding new data needs to be developed, currently uses CCIEA_timeseries_ID to match new metadata to old
update_metadata_file <- function(metadata_spreadsheet_folder,meta_file_search){
  print(paste0("Starting update_metadata ",now()))
  #backup spreadsheet before updating
  meta_folder_id=find_folder_id(metadata_spreadsheet_folder)
  backup_folder_name="Older Metadata Spreadsheets"
  # new_meta_file contains the old metadata, but it is the one we will be updating
  new_meta_file=backup_file(meta_file_search,meta_folder_id,backup_folder_name)
  print(new_meta_file)
  
  ## write to Google Drive
  sheet_write(data = metadata, ss=new_meta_file$id, sheet = 1)
  
  ## also write to GitHub as csv file
  write_csv(x = metadata, file = "data/CCIEA_metadata.csv",na="")
  
}

############################ PlotTimeSeries ###############################
# file made: "2018-10-23"

# This function will plot the time series data.  See the initial portion of the function for parts that you can alter.  

# If you label the column titles correctly, you only need to give the object name : PlotTimeSeries(MyData)
# correct column names are: "year", "index", "timeseries", "metric","SEup", "SElo", "SE"

# NOTE: if you call SEup separately (say to plot SD instead of SE), the first value cannot be an NA. 
# Set it and SElo to the index data point for that value.

# The default plot is the standard IEA time series plot
# If you include a "treshold" value, the plot will shift format to plotting versus a threshold
# If so, threshold.loc, will swap the plotting of grey polygons to indicate periods where the timeseries is above or below the threshold

# There are special plots for Seabirds: For seabird plots you must include both Y2 and Y.outlier in the plot call.
# For seabird mortality, Y is all the data, Y2 is data with outliers removed, Y.outlier are just the outliers.
# Trends are evaluated with the outliers removed.

PlotTimeSeries <-function(
    dframe=NA,
    X = NA,
    Y = NA,
    Y2 = NA, # set to NA to NOT plot points. NULL will replace with Y. Name the column for different value
    Y.outlier = NULL, # for birds to exclude outliers
    threshold = NA, 
    threshold.loc = "below", 
    threshold.correct = FALSE, # corrects for missing values in the threshold plots. Assumes zero is minimum.
    sig.trend = NA, # for plotting stream flow data.   values are c('positive','negative','not_sig')
    sig.mean = NA,
    # to calculate mean and sd for a specific set of years not whole time series.  For climate data.
    # stats.years.equals.plotted=TRUE, # overwritten by stats.years
    # stats.years = STATS.YEARS, # 1992:2021,
    stats.years = NA, 
    LWD = 1,
    LWD.threshold = 1,
    SE = NULL, 
    SEup = NA, 
    SElo = NA,
    XLIM = NA,
    YLIM = NA,
    plot.to.month = 6, # plot to month X of the max year. Just extends the axis a bit. 
    Xlab = NA, 
    Ylab = NULL, 
    Title = NULL,
    Xangle = 0,
    Xaxis.cex = 0.9, # plotted separately from x axis
    Yaxis.cex = 0.9, # plotted separately from y axis
    Ylab.cex = 0.9, # 1
    TitleCex = 0.9,
    YATS = 3, # number of tics on Y axis
    Y.lab.drop = FALSE, # quick trigger to skip every other label on the yaxis.  Set to TRUE to skip
    Y.axis.labels = NA, # eg,  c(0, 0.50, 1.00)  overides other axis plotting
    yminor = NA, # location of minor tics on y axis, eg  c( 0.25, 0.75,)
    Label.cex = 0.9, # 1.0,
    # Axis.cex = 1, 
    Pt.cex = 0.3, # change point size on plots
    TicsEvery = 10, 
    MinorTics = 1,
    MediumTics = TRUE, # adds unlabled major tic between curren tics. Needs even spacing.
    # NULL names the file based on the timeseries.  NA does not plot a title.
    ALPHA = 200, 
    PERIOD = 5,  # set to 0 to not plot diagnostics
    Arrow.cex = 2,
    AltTxt.Period = '5-years',
    AltTxt = NA,
    AltTxt.file.name = NA,  # this output name is necessary if you have weird characters in the time series name (eg, a colon, ":")
    Mar = c(0, 3.5, 0.7, 0),  # set all margins at once
    #Mar = c(5, 4, 4, 2),
    Bmar = NA, ## or one at a time
    Lmar = NA,
    Tmar = NA,
    Rmar = NA, # occassionally necessary to make space for legend
    Ylab_line = 2.5){ # occasionally necessary to make space for the Ylabel if the #'s are large 
  
  ####################################### end input ###############################################
  # set color scheme here ####
  # col2rgb("lightblue")
  
  mn.col = 'black'
  sd.col =  box.col = threshold.col = rgb(63, 173, 213, max = 255) 
  pt.col =  rgb(225, 118, 44, max = 255) 
  line.col = "black" 
  error.col = rgb(211, 211, 211, alpha = ALPHA, max = 255) # transparent grey
  arrow.color = "black"
  # threshold.col = rgb(65, 173, 213, max = 255)
  threshold.fill.col = "lightgrey"
  
  # SET PLOT PARAMETERS HERE ####
  title.font = 2
  
  if(is.na(Bmar == TRUE)){Bmar = Mar[1]}
  if(is.na(Lmar == TRUE)){Lmar = Mar[2]}
  if(is.na(Tmar == TRUE)){Tmar = Mar[3]}
  if(is.na(Rmar == TRUE)){Rmar = Mar[4]}
  
  par(ps = 10, cex = 1, cex.axis = Yaxis.cex, cex.main = TitleCex, cex.lab=Label.cex, mar=c(Bmar,Lmar,Tmar,Rmar))#, bg=NA)
  
  # load data; set title and TS for alt text ####
  if(length(dframe)==1){X = X; Y = Y; TS = Title}
  if(length(dframe)!=1){
    X=dframe$year
    Y=dframe$index
    if(is.null(Y2)){Y2 <- Y} # for when y is raw.data but no fitted data exist. to plot points.
    TS = Title
    if(is.null(Title)){Title=dframe$timeseries[1]; TS=Title}else{
      if(is.na(Title)){Title=NA; TS=dframe$timeseries[1]}}
    if(is.null(Ylab)){Ylab=dframe$metric[1]}else{if(is.na(Ylab)){Ylab=NA}}
    
    #### calculate SE ###
    # find SEup and SElo
    if(is.na(SEup[1] == TRUE)){
      if(length(SEup) > 1){SEup[1] = SElo[1] <- Y[1]}
    }
    
    if(is.na(SEup[1])==TRUE){ # use SEup if specified externally or calculate within
      cn = colnames(dframe)
      se.up = grep('SEup',cn)
      # for SEup and SElo in dframe
      if(length(se.up)>0){SEup=dframe$SEup;SElo=dframe$SElo}
      # if(length(se.up)==1){SEup=dframe$SEup;SElo=dframe$SElo}
      # for just specifying SE
      if(is.null(SE)){
        if(length(se.up)==0){if(!is.null(dframe$SE)){SEup=dframe$index+dframe$SE; SElo=dframe$index-dframe$SE}}
        if(length(se.up)==0){if(!is.null(dframe$se)){SEup=dframe$index+dframe$se; SElo=dframe$index-dframe$se}}
      }}
  }
  
  # convert to time. Data should be 2011-04-28 or just year 2009
  #if(nchar(as.character(X[1]))>10){ x = substring(X,1,10)} # X is correct.  Removes extra characters.
  if(nchar(as.character(X[1]))>=10){ x = as.POSIXct(X)}                 # year-month-day
  if(nchar(as.character(X[1]))== 7){ x = as.POSIXct(paste(X,01,sep='-'))}   # year-month
  if(nchar(as.character(X[1]))== 4){ x = as.POSIXct(paste(X,01,01,sep='-'))}# year
  
  XY = data.frame(cbind(x,Y))
  XY_full = XY
  XYY2 = cbind(XY,Y2)
  XY = na.omit(XY)
  
  XLIM1 = as.numeric(substring(as.POSIXct(strptime(XLIM[1], "%Y")),1,4))
  XLIM2 = as.numeric(substring(as.POSIXct(strptime(XLIM[2], "%Y")),1,4))
  
  # prep for x-axis labels and ranges####
  x1 = as.numeric(substring(as.POSIXct(strptime(x, "%Y")),1,4))
  xmin1 = min(x1)
  xmin2=ifelse(is.na(XLIM1)==TRUE,xmin1,XLIM1)
  # just five a little space
  Xmin = as.POSIXct(paste((xmin2-1),11,01,sep='-'))
  xmax = max(x)
  
  xmax1 = as.POSIXct(paste(max(x1),01,01,sep='-'))
  
  # covert for date within year by extending to next year
  if(xmax>xmax1){xmax2 = max(x1)+1}else{xmax2=max(x1)}
  xmax3 = ceiling(xmin2 + 1.02*(xmax2 -xmin2))
  xmax4 = ifelse(is.na(XLIM2)==TRUE,xmax3,ceiling(min(xmin2) + 1.02*(XLIM2 - xmin2)))
  # set axis length. may get overwritten below.
  # auto-extends to the next year if necessary to plot data
  # next step can limit within the data year 
  Xmax = as.POSIXct(paste(xmax4,01,01,sep='-'))
  XaxisMax = as.POSIXct(paste(xmax4,01,01,sep='-')) 
  #########
  # set Max for X axis to middle of selected year
  # must set XMAx[2] this option 
  if( is.na(plot.to.month) == FALSE){
    if(is.na(XLIM[2])==FALSE){
      ptm = ifelse(length(plot.to.month)==1, paste0(0,plot.to.month),plot.to.month)  
      if(nchar(as.character(XLIM[2]))> 10){ Xmax = substring(XLIM[2],1,10)}              
      if(nchar(as.character(XLIM[2]))==10){ Xmax = as.POSIXct(XLIM[2])}                 
      if(nchar(as.character(XLIM[2]))== 7){ Xmax = as.POSIXct(paste(XLIM[2],15,sep='-'))}   
      if(nchar(as.character(XLIM[2]))== 4){ Xmax = as.POSIXct(paste(XLIM[2],ptm,15,sep='-'))}
      xmax4 = ifelse(is.na(XLIM2)==TRUE,xmax3,ceiling(min(xmin2) + 1.05*(XLIM2 - xmin2)))
      XaxisMax = as.POSIXct(paste(xmax4,01,01,sep='-'))
    }
  }
  # Xmax sets the length of the x-axis line
  # XaxisMax = legnth of the un-plotted axis to keep the 
  # symbols away from stuff
  XaxisMax = as.POSIXct(paste(xmax4,01,01,sep='-')) 
  #########
  
  xL = length(X)   # for pch and lwd
  xmax5 = ifelse(is.na(XLIM2)==TRUE,xmax2,XLIM2)
  
  # range of xlabels
  labrange = c(xmin2:xmax5)
  atx  = as.POSIXct(paste(labrange,01,01,sep='-'))
  xlabs = as.numeric(substring(as.POSIXct(strptime(atx, "%Y")),1,4))
  
  # mean and sd of the full time series OR selected portion of time series####
  Y[Y %in% c(-999, -9999)] <- NA  # replace -999 with NAs
  if(!is.null(Y2)){Y2[Y2 %in% c(-999, -9999)]<- NA} # replace -999 with NAs
  SEup[SEup %in% c(-999, -9999)] <- NA 
  SElo[SElo %in% c(-999, -9999)] <- NA
  
  # remove points a priori identified as outliers. mostly for seabirds
  # set Y.calc ####
  if(!is.null(Y.outlier)){Y.calc = Y2}else{Y.calc = Y}
  
  # calculate statistics ####
  
  # set years for calculating stats, if unspecified, uses the plotted years
  # stats.years.equals.plotted == FALSE uses all the data in the input file
  
  if(is.na(stats.years[1])==TRUE){ # use specified stats.years or...
    #          if(stats.years.equals.plotted == TRUE){stats.years <- XLIM[1]:XLIM[2]}else{
    #               stats.years = min(XY$x):max(xY$x)
    #          }
    stdev = sd(Y.calc, na.rm=TRUE)
    mn = mean(Y.calc, na.rm=TRUE)
    Usd = mn+stdev
    Lsd = mn-stdev
    
    # for seabirds, I think
    stdev.plot = sd(Y, na.rm=TRUE)
    mn.plot = mean(Y, na.rm=TRUE)
    Usd.plot = mn.plot+stdev.plot
    Lsd.plot = mn.plot-stdev.plot
  }else{
    # still contains NAs
    x.yr = substr(x,1,4)
    Y.temp = data.frame(cbind(x.yr,Y.calc))
    # subset to the stats.years for calculating stats
    Y.calc2 = as.numeric(as.character(Y.temp[Y.temp$x %in% stats.years,'Y.calc']))
    stdev = sd(Y.calc2, na.rm=TRUE)
    mn = mean(Y.calc2, na.rm=TRUE)
    Usd = mn+stdev
    Lsd = mn-stdev
  } 
  #set Y range based on data and/or SE
  if(is.na(YLIM[1])==TRUE){ymin = min(Y,Y2,SElo,Lsd,na.rm=TRUE)}else{ymin=YLIM[1]}
  if(length(YLIM==2)){
    if(is.na(YLIM[2])==TRUE){ymax = max(Y,Y2,SEup, Usd, na.rm=TRUE)}else{ymax=YLIM[2]}
  }
  ydatamin=min(Y)
  ydatamax=max(Y)
  if(is.na(Y.axis.labels[1]) == TRUE){yats = pretty(c(ymin,ymax), n=YATS)}else{yats = Y.axis.labels}
  # format maintains decimal places for integers
  maxats = max(yats)
  Ymin1=ifelse(is.na(YLIM[1])==TRUE,ymin,YLIM[1])
  Ymax=ifelse(is.na(YLIM[2])==TRUE,max(ymax,maxats),YLIM[2])
  Ymin = Ymin1 - 1*(Ymax-Ymin1) # makes space below the graph.  Not for axis labeling
  
  # for shifting 45 degree years.
  mdfy=length(xlabs)/100
  xshift = max(mdfy*diff(xlabs)[1],0.3)
  
  if(x[1] > x[length(x)]){print('Warining: sort your data by year!')}
  
  #### PLOT COMMAND ####
  plot(Y~x, xlab=NA, ylab=NA, pch='19', type="n", 
       xlim=c(Xmin, XaxisMax), ylim=c(Ymin,Ymax), 
       bty="n", xaxt="n", yaxt='n')
  xline =  min(min(yats) - 0.05*(par()$usr[4]-par()$usr[3]),ymin)
  modif1 = ifelse(length(atx)>21,0.06,0.05)
  tl =     min(xline) - modif1*(par()$usr[4]-par()$usr[3])
  tl2 =    min(xline) - 0.03*(par()$usr[4]-par()$usr[3])
  modif2 = ifelse(length(atx)>21,0.16,0.14)
  txtloc = min(xline) - modif2*(par()$usr[4]-par()$usr[3]) 
  
  if(x[1] > x[length(x)]){legend('topleft',legend='Warining: sort your data by year first!', 
                                 bty='n', text.font = 2, text.col = 'red')}
  
  # yaxis
  # quick function to drop everyother ylab to make axes easier to see if they are crowded.
  if(Y.lab.drop == TRUE){
    g = seq(1, length(yats),2)
    ymajor = yats[g]
    h = seq(2, length(yats),2)
    yminor = yats[h]
  }else{ymajor <- yats}
  yats.label = format(ymajor)
  
  axis(side=2, at=ymajor, labels=yats.label, las=1, cex=1)
  if(is.na(yminor[1])==FALSE){axis(side=2, at=yminor, labels=NA, tck = -0.05)}
  
  #### location of x lables ##############
  ########### Xangle == 0####
  if(is.na(TicsEvery)==TRUE){ 
    if(Xangle==0){ 
      if(length(atx)<=13){
        lines(c(Xmin,atx[length(atx)]),c(xline,xline))
        for(g in 1:length(atx)){
          arrows(atx[g],xline,atx[g],tl,length=0)
          
        }
        for(g in 1:length(atx)){
          #text(atx[g]-xshift,txtloc,xlabs[g], srt=Xangle, cex=0.7)
          text(atx[g],txtloc,xlabs[g], srt=Xangle, cex = Xaxis.cex)
        }
      }
      if(length(atx)>13){
        seq.num=3
        if((length(atx)/2==floor(length(atx)/2))==TRUE){seq.num=2}
        if((length(atx)/3==floor(length(atx)/3))==TRUE){seq.num=3}
        if((length(atx)/4==floor(length(atx)/4))==TRUE){seq.num=4}
        if(length(atx)>50){seq.num=5}
        if(length(atx)>99){seq.num=10}
        atx2 = as.numeric(substring(as.character(atx),1,4))
        atx3 = rev(abs(seq(-max(atx2), -min(atx2), seq.num)))
        atx4 = as.POSIXct(paste(atx3,01,01,sep='-'))
        lines(c(Xmin,atx[length(atx)]),c(xline,xline))
        for(g in 1:length(atx)){
          arrows(atx4[g],xline,atx4[g],tl,length=0)
        }
        for(g in 1:length(atx3)){
          text(atx4[g],txtloc,atx3[g], srt=Xangle, cex = Xaxis.cex)
        }
      }
    }     
    ############ Xangle == 45 or 90  ####
    if(Xangle!=0){
      if(length(atx)<=13){
        lines(c(Xmin,atx[length(atx)]),c(xline,xline))
        for(g in 1:length(atx)){
          arrows(atx[g],xline,atx[g],tl,length=0)
          
        }
        for(g in 1:length(atx)){
          text(atx[g]-xshift,txtloc,xlabs[g], srt=Xangle, cex= Xaxis.cex)
        }
      }
      if(length(atx)>13){
        seq.num=3=''
        if((length(atx)/2==floor(length(atx)/2))==TRUE){seq.num=2}
        if((length(atx)/3==floor(length(atx)/3))==TRUE){seq.num=3}
        if((length(atx)/4==floor(length(atx)/4))==TRUE){seq.num=4}
        if(length(atx)>50){seq.num=5}
        if(length(atx)>99){seq.num=10}
        atx2 = as.numeric(substring(as.character(atx),1,4))
        atx3 = rev(abs(seq(-max(atx2), -min(atx2), seq.num)))
        atx4 = as.POSIXct(paste(atx3,01,01,sep='-'))
        lines(c(Xmin,atx[length(atx)]),c(xline,xline))
        for(g in 1:length(atx)){
          arrows(atx4[g],xline,atx4[g],tl,length=0)
        }
        for(g in 1:length(atx3)){
          text(atx4[g]-xshift,txtloc,atx3[g], srt=Xangle,  cex= Xaxis.cex)
        }
      }
    }
    ############
  }else{
    atx2 = as.numeric(substring(as.character(atx),1,4))
    atx3 = rev(abs(seq(-max(atx2), -min(atx2), TicsEvery)))
    atx4 = as.POSIXct(paste(atx3,01,01,sep='-'))
    # lines(c(Xmin,atx[length(atx)]),c(xline,xline))
    lines(c(Xmin,Xmax),c(xline,xline))
    for(g in 1:length(atx)){
      arrows(atx4[g],xline,atx4[g],tl,length=0)
    }
    for(g in 1:length(atx3)){
      text(atx4[g],txtloc,atx3[g], srt=Xangle,cex= Xaxis.cex)
    } # end g
    # medium tics
    if(MediumTics == TRUE){
      atx4_year = as.numeric(substring(atx4,1,4))
      diff_x <- diff(atx4_year)
      med_x = atx4_year + diff_x[1]/2
      Med_x = as.POSIXct( paste(med_x, "01","01", sep="-"))
      Med_x = Med_x[-length(Med_x)]
      for(g in 1:length(Med_x)){
        arrows(Med_x[g],xline,Med_x[g],tl,length=0)
      }
    } # end medium tics
  } # end else
  
  # minor tics ####
  if(is.na(MinorTics)==TRUE){atx2 = as.numeric(substring(as.character(atx),1,4))
  if(length(atx)>99){atx5 = rev(abs(seq(-max(atx2),-min(atx2),5)))}else{atx5=atx}
  atx6 = as.POSIXct(paste(atx5,01,01,sep='-'))
  for(g in 1:length(atx5)){
    arrows(atx6[g],xline,atx6[g],tl2,length=0)}
  }else{
    atx2 = as.numeric(substring(as.character(atx),1,4))
    atx5 = rev(abs(seq(-max(atx2),-min(atx2),MinorTics)))
    atx6 = as.POSIXct(paste(atx5,01,01,sep='-'))
    for(g in 1:length(atx5)){
      arrows(atx6[g],xline,atx6[g],tl2,length=0)}
  }
  
  ### get time period fo the analysis - new code select based on date ###
  ### all time periods in years ####
  
  # period = PERIOD-1
  # last.year = max(x)
  # first.year = as.numeric(substring(last.year, 1,4)) - period
  # first.month = substring(last.year, 6,7)
  # # if(nchar(first.month)==2){first.month <- first.month}else(first.month <- paste0('0',first.month))
  # first.date = as.POSIXlt(paste0(first.year,'-', first.month,'-01'))
  
  ###########
  
  # different calculations for yearly vs monthly data
  last.year = max(x)
  last.year.month = substring(last.year, 6,10)
  if(last.year.month == "01-01"){period = PERIOD-1}else(period = PERIOD)
  first.year = as.numeric(substring(last.year, 1,4)) - period
  first.month = substring(last.year, 6,7)
  first.date = as.POSIXlt(paste0(first.year,'-', first.month,'-01'))
  
  ############
  
  
  X5 = XY$x[ XY$x >= first.date ]
  if(is.null(Y.outlier)){Y5 = XY$Y[ XY$x >= first.date ]}else{{Y5 = XYY2$Y2[ XY$x >= first.date ]}}
  
  ################
  
  #### plot longterm mean and sd & replot the data on top ####
  
  # Error Polygon information
  
  error.poly <- data.frame(cbind(x,SEup,SElo))
  error.poly <- na.omit(error.poly)
  error.poly$x = as.POSIXct( error.poly$x , origin =  '1970-01-01') 
  error.poly <- error.poly[ error.poly$x >= as.POSIXct( paste(XLIM[1],"01","01", sep="-")) , ]
  error.x = as.POSIXct( error.poly$x , origin =  '1970-01-01') 
  SEup2 = error.poly$SEup
  SElo2 = error.poly$SElo
  
  ### STANDARD plot features ####
  if(PERIOD!=0){ 
    if(is.na(threshold)){
      # plot se envelope style
      
      if(is.null(Y.outlier)){
        # plot box around evaluation period. upper and lower are set by Usd and Lsd.
        uplo = data.frame(cbind(x=c(min(X5),max(X5),max(X5),min(X5)), y=c(Usd,Usd,Lsd,Lsd)))
        uplo = na.omit(uplo)
        polygon(x=uplo$x, y=uplo$y, border=NA,col=box.col)
        # polygon(x=c(X5[1],max(X5),max(X5),X5[1]), y=c(Usd,Usd,Lsd,Lsd), border=NA,col=box.col)
        # plot mean and sd of full time series ####
        segments(Xmin,mn,max(x),mn, col=mn.col,lty="dotted",lwd=1)               
        segments(Xmin,Usd,max(x),Usd, col=sd.col,lwd=1)
        segments(Xmin,Lsd,max(x),Lsd,col=sd.col,lwd=1)
        
        SEx = na.omit(SEup)
        if(length(SEx) ==0){SEup = NA}
        if(length(SEup) > 1){polygon(c(error.x,rev(error.x)),c(SEup2,rev(SElo2)),border=NA,col=error.col)}
        #if(is.na(SEup[1])==FALSE){polygon(c(error.x,rev(error.x)),c(SEup2,rev(SElo2)),border=NA,col=error.col)}
        
      }
      if(!is.null(Y.outlier)){
        # plot box around evaluation period
        uplo = data.frame(cbind(x=c(min(X5),max(X5),max(X5),min(X5)), y=c(Usd,Usd,Lsd,Lsd)))
        uplo = na.omit(uplo)
        polygon(x=as.numeric(uplo$x), y=as.numeric(uplo$y), border=NA,col=box.col)
        segments(Xmin, Usd, max(x), Usd, lty = 'dotted')
        segments(Xmin, Lsd, max(x), Lsd, lty = 'dotted')
        # segments(Xmin, mn, max(x), mn, lty = 'solid')
      }
      
      #get plot dimensions for scaling
      xx<-par() 
      x1<-xx$usr[1]
      x2<-xx$usr[2]
      
      # run regression on last 5 years
      #check if the change is greater or less than one sd of full time series       
      
      m1 = lm(Y5~X5)
      s1 <- summary(m1)
      b1 <- s1$coefficients[2,1]
      pval <- s1$coefficients[2,4]
      pred = predict(m1)
      delta = pred[length(pred)] - pred[1]
      Z = abs(delta)-(stdev)
      x01 <- (x2-x1)*0.97 + x1     	# center of point on xaxis
      
      y01 = yats[1] + (max(yats) - yats[1])*0.7
      y02 = yats[1] + (max(yats) - yats[1])*0.3
      
      # plot upper arrow or equals sign ####
      LineWT = 3
      
      afont = 2
      arrow.cex= Arrow.cex
      
      # switch to user input symbols for flow data
      if(is.na(sig.trend[1])==FALSE){
        Z = ifelse(sig.trend[1] %in% c('positive','negative'), 1, 0)
        delta = ifelse(sig.trend[1] == 'positive', 1, -1)
      }

      if(Z<=0){text(x01,y01,"\\->", family="HersheySymbol", cex=arrow.cex, col=arrow.color, font=2)} 
      if(Z>0 & delta>0){text(x01,y01,"\\#H2296", family="HersheySymbol", cex=arrow.cex, col=arrow.color, font=2)} 
      if(Z>0 & delta<0){text(x01,y01,"\\#H2299", family="HersheySymbol", cex=arrow.cex, col=arrow.color, font=2)} 
      
      mn5 = mean(Y5, na.rm=TRUE)
      dMean = mn-mn5
      Z2 = abs(dMean)-stdev
      # plot pl or minus sign
      
      # switch to user imput symbols for flow data
      if(is.na(sig.mean[1])==FALSE){
        Z2 = ifelse(sig.mean[1] %in% c('positive','negative'), 1, 0) 
        dMean = ifelse(sig.mean[1] == 'positive', -1, 1)
      }

      if(Z2<=0){points(x01,y02,cex=1, pch=19, col='black')}
      if(Z2>0 & dMean>0){text(x01,y02,"_", cex=1, col=arrow.color, font=afont)}
      if(Z2>0 & dMean<0){text(x01,y02,"+", cex=1, col=arrow.color, font=afont)}
      
    } # end standard plot ###
  } # end standard plot features on/off
  
  # some preparations; files used below.
  
  XY = data.frame(cbind(x,Y, SEup, SElo))
  # to limit plotting on X axis.
  # stats have already been done
  
  date.min = as.POSIXlt (paste0(XLIM[1],"-01-01") )
  
  XY$x.date = as.POSIXlt(XY$x, origin =  '1970-01-01 00:00.00 UTC')
  XY = XY[ XY$x.date >= date.min, ]
  XY_no_na = XY[ !is.na(XY$Y),]
  XY0 = XY
  
  if(threshold.loc == 'above'){XY0[is.na(XY)] <- 0}else{XY0[is.na(XY)] <- threshold}
  
  #XY = na.omit(XY)
  # NA/s converted to zeros for plotting on thresholds
  
  # THRESHOLD line and polygon fill ####
  if(!is.na(threshold)){
    if(threshold.loc=='n'){segments(Xmin,threshold,max(x),
                                    threshold,col=threshold.col,lwd=1)}else{
                                      if(threshold.correct == TRUE){
                                        d1 = c(XY0$x,rev(XY0$x))
                                        d2 = c(XY0$Y, rep(threshold,length(XY0$Y)))}else{
                                          d1 = c(XY$x,rev(XY$x))
                                          d2 = c(XY$Y, rep(threshold,length(XY$Y)))}
                                      polygon(d1,d2, border=NA, col=threshold.fill.col)
                                      d3.l = length(d1)/2
                                      fill.loc = ifelse(threshold.loc=="above",min(yats),max(yats))
                                      d3 = c(rep(threshold,d3.l),rep(fill.loc,d3.l))
                                      polygon(d1,d3, border=NA, col="white")
                                      
                                      
                                      #threshold line #####
                                      if(threshold>xline){segments(Xmin,threshold,max(x),threshold,
                                                                   col=threshold.col,lwd=LWD.threshold)}
                                    }
    
  } ##### 
  
  
  ##############################
  
  #REPLOT time series on top ####
  if(is.na(LWD)==TRUE){LWD = ifelse(100/xL > 2, 0.5, ifelse(100/xL < 1,0.5, 1))}else{LWD = LWD}
  
  if(threshold.correct == TRUE){
    lines(XY$Y~XY$x,lwd=LWD,col=line.col)
    lines(XY0$Y~XY0$x,lwd=LWD,col=line.col, lty="dotted")
  }else{lines(XY$Y~XY$x,lwd=LWD,col=line.col)
    lines(XY_no_na$Y~XY_no_na$x,lwd=LWD,col=line.col, lty="dotted")}
  
  # check if Y2 is NA or not. Then plot if not NA
  Y3 = na.omit(Y2)
  if(!is.na(Y3[1])){
    XYY2 <- XYY2[ XYY2$x >= date.min, ]
    points(XYY2$Y2~XYY2$x, pch=20, col=pt.col, cex=Pt.cex)}
  
  # seabird extra plot - plot outliers as new point symbol to identify ####
  
  if(!is.null(Y.outlier)){
    Y.out = data.frame(cbind(x, Y.outlier))
    Y.out = na.omit(Y.out)
    points(Y.out[,1],Y.out[,2], pch=21, cex=Pt.cex*1.2, bg='white') 
  }
  
  if(!is.na(threshold)){
    if(is.na(SEup[1])==FALSE){
      lines(XY$x,XY$SEup,lty="dotted")
      lines(XY$x,XY$SElo,lty="dotted")
    }}
  
  # AXIS LABELS ####
  if(is.na(Xlab)!=TRUE){mtext(side=1, Xlab, line=-1)}
  loc = par()$usr[3] + 0.7*(par()$usr[4]- par()$usr[3])
  mtext(side=2,Ylab, line=Ylab_line, at=loc, cex=Ylab.cex)
  
  # title ####
  if(!is.na(Title)==TRUE){title(Title, line=0, font=title.font, cex.main=TitleCex)}
  
  # update note on figure ####
  
  if(exists('note.update.status')==TRUE){
    if(note.update.status == TRUE){
      if(dframe$type[1] != 'current.data'){
        legend('topleft',legend='OLD DATA', text.col='red',   text.font = 2, bty='n')}else{
          legend('topleft',legend='NEW DATA', text.col='green', text.font = 2, bty='n')}}
  }
  
  ######## test code ###########
  
  # stats.years.2 = as.POSIXct(paste0(stats.years, "-01-01"))
  # XY_stats = XY0[XY0$x %in% stats.years.2, ]
  # lines(XY_stats$Y~XY_stats$x, col='red', lty="solid", lwd=1)
  # segments(stats.years.2[1] , xline , stats.years.2[30],xline, col='red', lwd=2, lty='solid')
  
  ##############################     
  
  ############## ALT Text doesn't work special bird plots
  
  ##########   ALT Text ########
  if(!is.na(AltTxt)){
    d1 = substring(AltTxt,nchar(AltTxt),nchar(AltTxt))
    if(d1!="/"){AltTxt= paste(AltTxt,"/",sep="")}
    atxmin = min(X)
    atxmax = max(X)
    YMin = round(Ymin,2); YMax = round(Ymax,2)
    atymin = ifelse(Ymin<0,paste("negative",abs(YMin)),YMin)
    atymax = ifelse(Ymax<0,paste("negative",abs(YMax)),YMax)
    xy = data.frame(cbind(X,Y))
    xy$X = as.character(xy$X)
    xy$Y = as.numeric(as.character(xy$Y))
    ymx = xy[match(max(na.omit(xy$Y)),xy$Y),]
    ymn = xy[match(min(na.omit(xy$Y)),xy$Y),]
    
    
    if(is.null(Ylab)){Ylab = dframe$metric[1]}
    if(is.na(Ylab)){Ylab = dframe$metric[1]}
    
    ##### Standard plotting alt text #### 
    if(is.na(threshold)){
      
      OpeningStatement = paste0("This figure shows time-series data for ", TS," (y-axis, ", Ylab,") from ", atxmin," to ",atxmax," (x-axis, Year). ")
      # MeanStatement = paste0("The mean of the full time series was ",round(mn,2),". ") 
      Mean_SD = paste0("The mean of the full time series was ",round(mn,2)," (lower standard deviation = ", round(Lsd,2), ", upper standard deviation = ",round(Usd,2), "). ")
      HighLowYears = paste0( "The index was highest in ", ymx[1,1]," (at ", round(ymx[1,2],2),"), and lowest in ", ymn[1,1]," (at ", round(ymn[1,2],2),"). ")
      # StandardDeviationStatement = paste0("The upper standard deviation was ", round(Usd,2),", and the lower standard deviation was ",round(Lsd,2),".")
      
      # alt text to describe the arrows
      if(Z<=0){PeriodTrend = paste0('The index changed by less than one standard deviation of the full time series over the last ', AltTxt.Period, '. ')}
      if(Z>0 & delta>0){PeriodTrend = paste0('The index increased by more than one standard deviation of the full time series over the last ' ,AltTxt.Period, '. ')}
      if(Z>0 & delta<0){PeriodTrend = paste0('The index decreased by more than one standard deviation of the full time series over the last ', AltTxt.Period, '. ')}
      
      # alt text to describe the plus and minus
      if(Z2<=0){points(x01,y02,cex=1, pch=19)
        PeriodMean = paste0("The mean of the last ", AltTxt.Period," was within one standard deviation of the long-term mean. ")
      }
      if(Z2>0 & dMean>0){PeriodMean = paste0("The mean of the last ", AltTxt.Period,"  was more than one standard deviation below the mean of the full time series. ")}
      if(Z2>0 & dMean<0){PeriodMean = paste0("The mean of the last ",AltTxt.Period," was more than one standard deviation above the mean of the full time series. ")}
      
      ###################### alt text "above years" ###################
      above.sd = na.omit(xy[xy$Y>Usd,])
      if(nrow(above.sd)==0){
        AboveYears = 'The index was never above one standard deviation of the full time series. '
      }
      if(nrow(above.sd)==1){
        above.last = above.sd[1,1]
        AboveYears = paste0("The index was more than one standard deviation above the mean of the full time series in ",above.last,'. ')
      }
      if(nrow(above.sd)==2){
        AboveYears = paste0("The index was  more than one standard deviation above the mean of the full time series in ",above.sd[1,1]," and ",above.sd[2,1],". ")
      }
      if(nrow(above.sd)>2){
        no.yrs = nrow(above.sd)
        above.first = above.sd[1,1]
        above.last = above.sd[no.yrs,1]
        above = above.first
        for(i in 2:(no.yrs-1)){
          above = paste0(above,", ",above.sd[i,1])
        }
        above.years = paste0(above," and ",above.last)
        AboveYears = paste0("The index was more than one standard deviation above the mean of the full time series in ",above.years,". ")
      }
      
      ### below years ####
      below.sd = na.omit(xy[xy$Y<Lsd,])
      if(nrow(below.sd)==0){
        BelowYears = 'The index was never below one standard deviation of the full time series. '
      }
      if(nrow(below.sd)==1){
        below.last = below.sd[1,1]
        BelowYears = paste0("The index was more than one standard deviation below the mean of the full time series in ",below.last,'. ')
      }
      if(nrow(below.sd)==2){
        BelowYears = paste0("The index was more than one standard deviation below the mean of the full time series in ",below.sd[1,1]," and ",below.sd[2,1],". ")
      }
      if(nrow(below.sd)>2){
        no.yrs = nrow(below.sd)
        below.first = below.sd[1,1]
        below.last = below.sd[no.yrs,1]
        below = below.first
        for(i in 2:(no.yrs-1)){
          below = paste0(below,", ",below.sd[i,1])
        }
        below.years = paste0(below," and ",below.last)
        BelowYears = paste0("The index was more than one standard deviation below the mean of the full time series in ",below.years,". ")
      }
      LastYear = paste0("The index was ",round(Y[length(Y)],2), " in ",X[length(X)],", the most recent year of the time series. ")
      
      AltText = paste0(OpeningStatement, Mean_SD, AboveYears, BelowYears, LastYear)
      # AltText
      
      #file.name = paste(AltTxt,TS,"_alt.txt", sep='')
      # file.name = paste0(AltTxt,TS,"_alt.txt")
      # fileConn<-file(file.name)
      # writeLines(AltText, fileConn)
      # close(fileConn)     
    } # end standard alt text
    
    ##################### Alt text for threshold graph ##########################
    if(!is.na(threshold)){  
      OpeningStatement = paste0("This figure shows time-series data for ", TS," from ",atxmin," to ",atxmax," (x-axis, Year). The y-axis (",Ylab, ") varies from ",atymin," to ",atymax,". The index is plotted relative to a threshold value of ",threshold,". The threshold is indicated by a blue line.")
      
      # above ####
      at3 = paste0("The index was highest in ", ymx[1,1]," (at ", round(ymx[1,2],2),"), and lowest in ", ymn[1,1]," (at ", round(ymn[1,2],2),"). ",sep='')
      above.th = na.omit(xy[xy$Y>threshold,])
      if(nrow(above.th)==0){
        AboveYears = paste0('The index was never above the threshold')
      }
      if(nrow(above.th)==1){
        above.last = above.th[1,1]
        AboveYears = paste0("The index was above the threshold value of")
      }
      if(nrow(above.th)==2){
        AboveYears = paste0("The index was above the threshold in ",above.th[1,1]," and ",above.th[2,1],". ")
      }
      if(nrow(above.th)>2){
        no.yrs = nrow(above.th)
        above.first = above.th[1,1]
        above.last = above.th[no.yrs,1]
        above = above.first
        for(i in 2:(no.yrs-1)){
          above = paste(above,", ",above.th[i,1], sep='')
        }
        above.years = paste0(above," and ",above.last)
        AboveYears = paste0("The index was above the threshold in ",above.years,". ")
      }
      # below
      below.th = na.omit(xy[xy$Y>Lsd,])
      if(nrow(below.th)==0){
        BelowYears = 'The index was never below the threshold. '
      }
      if(nrow(below.th)==1){
        below.last = below.th[1,1]
        BelowYears = paste0("The index was below the threshold in ",below.last,'. ')
      }
      if(nrow(below.th)==2){
        BelowYears = paste0("The index was below the threshold in ",below.th[1,1]," and ",below.th[2,1],". ")
      }
      if(nrow(below.th)>2){
        no.yrs = nrow(below.th)
        below.first = below.th[1,1]
        below.last = below.th[no.yrs,1]
        below = below.first
        for(i in 2:(no.yrs-1)){
          below = paste0(below,", ",below.th[i,1])
        }
        below.years = paste0(below," and ",below.last)
        BelowYears = paste0("The index below the threshold in ",below.years,". ")
      }
      LastYear = paste0("The index was ",round(Y[length(Y)],2), " in ",X[length(X)],", the most recent year of the time series. ")  
      
      AltText = paste0(OpeningStatement, AboveYears, BelowYears, LastYear)
      
      # file.name = paste(AltTxt,TS,"_alt.txt", sep='')
      # fileConn<-file(file.name)
      # writeLines(AltText, fileConn)
      # close(fileConn) 
    } # end threshold alt text
    
    prefix= opts_current$get()$label
    if(is.null(prefix)){prefix <- ""}
    
    if(is.na(AltTxt.file.name)==TRUE){alt.name = TS}else{alt.name=AltTxt.file.name}
    
    file.name = paste0(AltTxt, prefix,"_", alt.name, "_alt.txt")
    fileConn <- file(file.name)
    writeLines(AltText, fileConn)
    close(fileConn)     
  }  # end all alt text  
  # return(XXYY)
  tr="";
  st="";
  
  # change is less than standard deviation
  if(Z <= 0){
    tr="n";
  }
  else{
    # change is greater than stdev and positive
    if(delta > 0){
      tr="u";
    }
    else{
      # change is greater than stdev and negative
      tr="d";
    }
  }
  if(Z2 <= 0){
    # difference between mean from last 5 years and total mean is less than stdev
    st="e";
  }
  else{
    # last 5 year mean greater than total mean
    if(dMean > 0){
      st="b";
    }
    else{
      # last 5 year mean less than total mean
      st="a";
    }
  }
  
  file_stats <- list(trend=tr,status=st,min=signif_util(ydatamin,6),max=signif_util(ydatamax,6))
  return(file_stats)
}  # end funtion  



