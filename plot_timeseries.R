## Working version
# For now run on laptop Rscript plot_timeseries.R
# push new images to GitHub and metadata spreadsheet to Drive
# Open spreadsheet in Google Sheets before running

## Main code ##

## PERIOD: number of years over which to calculate status and trend (green window in plots)
## timesp: type of plot 0=regular time series, 1=seasonal plot, 2=annual plot of monthly data
## seastyp: season number for seasonal plots 0: not a seasonal plot, 1: winter = jan,feb,mar, 2: spring = apr,may,jun, 3: summer = jun,jul,aug, 4: fall = oct,nov,dec)
library("dplyr")

source("_init.R")
source("_gdrive.R")
source("_pi_folders_to_json.R")
source("_plot_functions.R")

# Set global variables.

seastyp <- 0
outdir <- "figures/timeseries_images/"

meta_file <- "data/CCIEA_metadata.csv"
# Read the metadata directly from the URL into a data frame.
metadata <- read.csv(meta_file, stringsAsFactors = FALSE)

# 3. Main Processing Loop
# Loop through each row of the metadata data frame.
# Plot all the timeseries data in the current esr_year folder for each PI
# Update the metadata with the new plot file name
# 
for (i in 1:nrow(metadata)) {

  # Extract the current row's data.
  info <- metadata[i, ]
  
  # Print a status message, similar to the PHP script.
  cat(paste(i,". ",info$cciea_filename,": ", info$ERDDAP_Dataset_ID, info$CCIEA_timeseries_ID, paste0("'", info$Title, "'"), "\n"))
  
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
    if(!file.exists(infile)){
      infile <- paste0("data/timeseries_data/",info$PI_filename,sep="")
      cat(paste(" using PI filename: ",info$PI_filename, "\n"))
      }
#    if(file.exists(infile) && info$cciea_filename != "HWB_CSVI.csv" && info$cciea_filename != "oc_arg_Newport_M.csv"){ ## CVSI has categorical index
    if(file.exists(infile) && info$cciea_filename != "HWB_CSVI.csv"){ ## CVSI has categorical index
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
      suppressMessages(dframe <- indata %>% filter(timeseries==timeseriesname))
      # Seabird Farallon Islands have "ND" text mixed in with data
      if (info$ERDDAP_Dataset_ID == "cciea_B_AS_DIET_ND") {
        dframe$index[dframe$index == "ND"] <- ""
        dframe$index <- as.numeric(dframe$index)
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
      ylab <- info$Units
      title <- info$Title;
      
#      lastyr <- info$year_end;
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
      # For now, just ignore the option to plot part of a timeseries, use max/min year of data

# Monthly      
        if(grepl("_M.csv",info$cciea_filename) || grepl("Monthly",info$cciea_filename) || grepl("copepods",info$cciea_filename) || pltvar == "cciea_EI_KRILLEN") {
        datatype <- 0 # Monthly data
        tmin=min(dframe$time)
        tmax=max(dframe$time)
        info$year_begin = as.numeric(substring(as.POSIXct(strptime(tmin, "%Y")),1,4))
        info$year_end = as.numeric(substring(as.POSIXct(strptime(tmax, "%Y")),1,4))
        firstyr=info$year_begin
        lastyr= info$year_end
      }
      else{
        datatype <- 1 # default to yearly data
        firstyr=min(dframe$year)
        lastyr=max(dframe$year)
        info$year_begin = firstyr
        info$year_end = lastyr
      }
      
      if (startsWith(pltvar, "cciea_EI_DOMACID_MON")) {
        datatype <- 0 # Monthly data
        tsreg <- "false"
        tssd <- "false"
        tsmn <- "false"
        tmin=min(dframe$time)
        tmax=max(dframe$time)
        info$year_begin = as.numeric(substring(as.POSIXct(strptime(tmin, "%Y")),1,4))
        info$year_end = as.numeric(substring(as.POSIXct(strptime(tmax, "%Y")),1,4))
        firstyr=info$year_begin
        lastyr= info$year_end
      }
      
      file_stats=data.frame()
      # Always plot the yearly or monthly data
      file_stats=run_plot(timesp, dframe, varname, ylab, title, seastyp, PERIOD,
               firstyr, lastyr, tsreg, tssd, tsmn, tssmo,
               outdir, outid, table_id, datatype, query, usePiSe,
               ymin, ymax, subcomponent)
      if(file_stats$plotfilename==""){
        next
      }
      else{
        info$default_figure=file_stats$plotfilename
        info$trend=file_stats$trend
        info$status=file_stats$status
        info$min=file_stats$min
        info$max=file_stats$max
        
        metadata = update_metadata_line(metadata,info)
#        write_csv(x = metadata, file = "data/CCIEA_metadata_updated.csv",na="")
        
        }
      
      #      # If data is monthly oceanographic, create additional seasonal and annual plots.
      #      if (datatype == 0 && startsWith(pltvar, "cciea_OC_")) {
        
      #        # Generate seasonal plots (Winter, Spring, Summer, Fall)
      #        for (j in 1:4) {
      #          timesp_s <- 1
      #          seastyp_s <- j
      #          run_plot(timesp_s, df, varname, ylab, title, seastyp_s, PERIOD,
      #                   firstyr, lastyr, tsreg, tssd, tsmn, tssmo,
      #                   outdir, outid, table_id, datatype, query, usePiSe,
      #                   ymin, ymax, subcomponent)
      #        }
        
      #        # Generate annual summary plot
      #        timesp_a <- 2
      #        seastyp_a <- 0
      #        run_plot(timesp_a, df, varname, ylab, title, seastyp_a, PERIOD,
      #                 firstyr, lastyr, tsreg, tssd, tsmn, tssmo,
      #                 outdir, outid, table_id, datatype, query, usePiSe,
      #                 ymin, ymax, subcomponent)
      #      }
    }
    else{
      print(cat(info$cciea_filename," or PI file ",info$PI_filename," not found, going to next line, ",i,", in metadata"))
    }
  }
}

# write updated metadata back out to file
#print(metadata)
update_metadata_file(metadata_spreadsheet_folder,meta_file_search)


