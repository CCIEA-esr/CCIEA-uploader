## Working version
# !!Add case where data file name is missing in metadata spreadsheet (HMS data)

run_plot <- function(timesp, df, varname, ylab, title, seastyp, PERIOD,
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
  CallPlotTimeSeries(df,ylab,title,timename,varname,PERIOD,datatype,timesp,seastyp,firstyr,lastyr,tsmn,tssmo,tsreg,tssd,outdir,outid,Ymin,Ymax,query,usepise)
  
}


## PERIOD: number of years over which to calculate status and trend (green window in plots)
## timesp: type of plot 0=regular time series, 1=seasonal plot, 2=annual plot of monthly data
## seastyp: season number for seasonal plots 0: not a seasonal plot, 1: winter = jan,feb,mar, 2: spring = apr,may,jun, 3: summer = jun,jul,aug, 4: fall = oct,nov,dec)
library("dplyr")

source ("call_PlotTimeSeries.R")

# Set global variables.

seastyp <- 0
outdir <- "figures/test/"

meta_file <- "data/CCIEA_metadata.csv"
# Read the metadata directly from the URL into a data frame.
metadata <- read.csv(meta_file, stringsAsFactors = FALSE)

# 3. Main Processing Loop
# Loop through each row of the metadata data frame.
# Plot all the timeseries data in the current esr_year folder for each PI
# Update the metadata with the new plot file name
# 
for (i in 7:nrow(metadata)) {

  # Extract the current row's data.
  info <- metadata[i, ]
  
  # Print a status message, similar to the PHP script.
  cat(paste(info$Component_Section, info$ERDDAP_Dataset_ID, info$CCIEA_timeseries_ID, paste0("'", info$Title, "'"), "\n"))
  
  # Check if the dataset should be processed.
  if (!is.na(info$serve_flag) && !is.na(info$Component_Section) && info$Component_Section != "" && !is.na(info$cciea_filename)  && info$cciea_filename != "") {
    
    # --- Set plotting parameters based on metadata ---
    timesp <- 0
    
    table_id <- 1 
    if (info$Component_Section == "Climate and Ocean Drivers") {
      table_id <- 0
    }
    
    PERIOD <- 5
    pltvar <- info$ERDDAP_Dataset_ID
    # In R, grepl() is used to find substrings, similar to PHP's strpos().
    if (grepl("_SM_", pltvar, fixed = TRUE)) {
      PERIOD <- 10 # Compute 10-yr regression for Salmon
    }
    PI <- info$PI_ID
    outid <- info$CCIEA_timeseries_ID
    varname=info$ERDDAP_variable_name
    
    usePiSe <- info$use_pi_se
    if (is.na(usePiSe) || usePiSe == "") {
      usePiSe <- 0
    }
    
# Read data
    infile <- paste0("data/timeseries_data/",info$cciea_filename,sep="")
    indata <- read.csv(infile, stringsAsFactors = FALSE)
#    timeseries <- unique(indata$timeseries)
    timeseriesname <- info$ERDDAP_query_value

    
#for (j in 1:length(timeseries)) { ## not closed yet
    
    timesp <- 0
    
    table_id <- 1 
    if (info$Component_Section == "Climate and Ocean Drivers") {
      table_id <- 0
    }
    
#df=indata[indata$timeseries==timeseries[j],]
df <- indata %>% filter(timeseries==timeseriesname)

    PERIOD <- 5
    pltvar <- info$ERDDAP_Dataset_ID
    # In R, grepl() is used to find substrings, similar to PHP's strpos().
    if (grepl("_SM_", pltvar, fixed = TRUE)) {
      PERIOD <- 10 # Compute 10-yr regression for Salmon
    }
    PI <- info$PI_ID
    outid <- info$CCIEA_timeseries_ID
    varname=info$ERDDAP_variable_name
    ylab <- info$Units
    title <- info$Title;
    firstyr <- info$year_begin;
    lastyr <- info$year_end;
    ymin <- info$min;
    ymax <- info$max;
    subcomponent <- info$Subcomponent;
   
    usePiSe <- info$use_pi_se
    if (is.na(usePiSe) || usePiSe == "") {
      usePiSe <- 0
    }
    
    tsreg <- "true"
    tssd <- "true"
    tsmn <- "true"
    tssmo <- 0
    
    # Determine data type (monthly vs. yearly).
    # `startsWith()` is the R equivalent of checking if strpos() === 0.
    datatype <- 1 # Default to yearly
    if (startsWith(pltvar, "cciea_OC_") && subcomponent != "Annual") {
      datatype <- 0 # Monthly data
    }
    
    if (startsWith(pltvar, "cciea_EI_DOMACID_MON")) {
      datatype <- 0 # Monthly data
      tsreg <- "false"
      tssd <- "false"
      tsmn <- "false"
    }
        
    
    # Always plot the yearly or monthly data
    run_plot(timesp, df, varname, ylab, title, seastyp, PERIOD,
                       firstyr, lastyr, tsreg, tssd, tsmn, tssmo,
                       outdir, outid, table_id, datatype, query, usePiSe,
                       ymin, ymax, subcomponent)
     }   
    # If data is monthly oceanographic, create additional seasonal and annual plots.
    if (datatype == 0 && startsWith(pltvar, "cciea_OC_")) {
      
      # Generate seasonal plots (Winter, Spring, Summer, Fall)
      for (j in 1:4) {
        timesp_s <- 1
        seastyp_s <- j
        run_plot(timesp_s, df, varname, ylab, title, seastyp_s, PERIOD,
                           firstyr, lastyr, tsreg, tssd, tsmn, tssmo,
                           outdir, outid, table_id, datatype, query, usePiSe,
                           ymin, ymax, subcomponent)
      }
      
      # Generate annual summary plot
      timesp_a <- 2
      seastyp_a <- 0
      run_plot(timesp_a, df, varname, ylab, title, seastyp_a, PERIOD,
                         firstyr, lastyr, tsreg, tssd, tsmn, tssmo,
                         outdir, outid, table_id, datatype, query, usePiSe,
                         ymin, ymax, subcomponent)
    }
  }


