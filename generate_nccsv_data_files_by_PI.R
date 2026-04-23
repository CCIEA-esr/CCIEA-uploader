# generates nccsv files for files that have been uploaded
# uses uploader-status.json to determine what files need generating
# generates an NCCSV file and a dataset XML file for each ERDDAP_Dataset_ID


library(tidyverse)
library(jsonlite)
library(xml2)

source("_init.R")

# --- Configuration & Arguments ---
#args <- commandArgs(trailingOnly = TRUE)
#PI_id <- if (length(args) > 0) args[1] else "Schroeder"

#cat("generating nccsv for PI", PI_id, "\n")

#path <- "/home/ldewitt/Documents/GitHub/CCIEA-uploader/data/"
path <- "data/"
infile <- paste0(path, "CCIEA_metadata.csv")
inpath <- paste0(path, "timeseries_data")
nccsvoutpath <- paste0(path, "erddap_files/nccsv")
xmloutpath <- paste0(path, "erddap_files/datasets_xml")
newfilelist <- paste0(path, "uploader_status_",esr_year,".json")
erdsettype <- "EDDTableFromNccsvFiles"

# --- Helper: Load Filename Fixes ---
#file_fixnames <- "rename_files.csv"
#fix_df <- read_csv(file_fixnames, show_col_types = FALSE) %>%
#  filter(!is.na(.[[1]]))
#fix_map <- setNames(fix_df[[2]], fix_df[[1]])

# --- Helper: Load Uploader Status (JSON) ---
status_data <- fromJSON(newfilelist, simplifyVector = FALSE)
version <- status_data$statusupdate
headervars <- status_data$headervars
headervarsmon <- status_data$headervarsmon

# old code that fixed file names
# newfilearray <- list()
# for (pi_item in status_data$status) {
#   pifiles <- list()
#  for (f in pi_item$files) {
#     if (f$name %in% names(fix_map)) {
#       fixed_name <- fix_map[[f$name]]
#       pifiles[[fixed_name]] <- f
#     } else {
#       cat(f$name, "not found in naming list\n")
#     }
#   }
#   newfilearray[[pi_item$name]] <- list(
#     newmeta = pi_item$newmeta,
#     version = pi_item$newmetaupdate,
#     files = pifiles
#   )
# }

# Process the nested JSON into file list for PI
newfilearray <- list()
pis <- c()
for (pi_item in status_data$status) {
  pis <- c(pis,pi_item$name)
  pifiles <- list()
  for (f in pi_item$files) {
    pifiles[[f$name]] <- f
    }
  newfilearray[[pi_item$name]] <- list(
  newmeta = pi_item$newmeta,
  version = pi_item$newmetaupdate,
  files = pifiles
  )
}

# --- Read Metadata
  meta_df <- read_csv(infile, show_col_types = FALSE)

# Initialize ERDDAP XML
  root <- xml_new_root("erddapDatasets")
  
# Loop through PI's who have uploaded data

for (PI_id in pis){
  cat("generating nccsv for PI", PI_id, "\n")


# --- Main Processing Loop ---
# Filter metadata for the specific PI
pi_meta <- meta_df %>% filter(PI_ID == PI_id, serve_flag == "1")

if (nrow(pi_meta) > 0 && PI_id %in% names(newfilearray)) {  ## process this PI
  
  status_files <- newfilearray[[PI_id]]$files
  if (length(status_files) == 0) {
    print(paste("No new files for", PI_id))
    next
  }
# files uploaded for this PI according to cciea uploader json file:
  infilelist <- names(status_files)
  
  # Group by Dataset ID
  datasets <- split(pi_meta, pi_meta$ERDDAP_Dataset_ID)

  for (eid in names(datasets)) { # loop through erddap dataset id's for this PI - one nccsv file per dataset, but there can be multiple data files in a dataset
    cat(eid, "\n")
    if(str_ends(eid, "_ND")){
      print(paste(eid," is a private dataset, don't serve it in ERDDAP"))
      next
      }
    
    dset_df <- datasets[[eid]]  # list of metadata for this PI and erddap dataset id
    
    outdir <- file.path(nccsvoutpath, PI_id)
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    outfile <- file.path(outdir, paste0(eid, ".csv"))
    
    # Check column requirements across all files in this dataset
    pifilelist <- unique(dset_df$cciea_filename)  # list of data files for this PI and erddap dataset id in metadata spreadsheet

   # loop through files for this dataset, check file existance and naming 
   # note multiple files in a dataset mostly applies to bird data
   # keep track of whether need to read cciea_filename or PI_filename
    pifilestatus <- list()
    for (idx in seq_along(pifilelist)) {
      fname <- pifilelist[idx]
      pname <- fname
      pifilestatus[[idx]] <- list()
      pifilestatus[[idx]]$metafile <- fname
      pifilestatus[[idx]]$infile <- fname   # file uploaded by PI; if different than cciea_filename, this name should be entered into the PI_filename column
      if (!fname %in% infilelist){
        # uploaded file name is not cciea_filename, check if it's the PI_filename in metadata spreadsheet instead
        meta_for_fname <- dset_df %>% filter(cciea_filename == fname, serve_flag == "1") # metadata for rows where cciea_filename == fname
        pifile <- unique(meta_for_fname$PI_filename) # PI filename where cciea_filename == fname
        if(length(pifile>0) && pifile %in% infilelist){
          pname <- pifile
          pifilestatus[[idx]]$infile <- pname
          }
      	}
      fpath <- file.path(inpath, pname)
      if (!file.exists(fpath)) stop(paste("Something is wrong, file not found", fname, "or", pname," ; perhaps you need to remove serve_flag from this dataset?"))
      }
      

    has_metric <- FALSE
    has_se <- FALSE    
   # check consistency of header across all files (usually only one file)
   #   multiple files in a dataset mostly applies just to bird data
    for (fname in pifilestatus) {
      fpath <- file.path(inpath, fname$infile)
      # If one file has SEup/SElo, then need to add a value (compute?) or missing to all files
      # Look at each file to see if it already has SEup/SElo before proceeding
      header <- names(read_csv(fpath, n_max = 0, show_col_types = FALSE))
      if ("metric" %in% header) has_metric <- TRUE
      if (all(c("SEup", "SElo") %in% header)) has_se <- TRUE
      }
    
    # Process and Merge Data
    combined_data <- data.frame() # gather all data for this erddap dataset id
    meta_collect <- list()  # gather all metadata as an array for this erddap dataset id, metadata for each indicator is potentially different
    
    for (pifile in pifilestatus) {  # loop through files for this dataset, collect data
      iname <- pifile$infile  # input file name: read this file
    cat(iname, "\n")
      mname <- pifile$metafile # file name established in loop above for cciea_filename in metadata spreadsheet for this indicator: use this file name to find metadata
      fpath <- file.path(inpath, iname)
      # some PI's duplicate the time column (eg oc_nph_jf_A.csv). tell read_csv not to try to fix this by default
      raw_data <- read_csv(fpath, show_col_types = FALSE, name_repair = "minimal")
      # now rename the duplicate columns
      colnames(raw_data) <- make.unique(colnames(raw_data))
      
      # Determine Time Column
      is_monthly <- str_detect(iname, "_M.csv|Monthly") || "time" %in% names(raw_data)
      t_col <- if (is_monthly) "time" else "year"
      t_units <- if (is_monthly) "yyyy-MM-dd" else "yyyy"
      
      # sometimes character "NA" is mixed in with numeric values of index - check and remove 
      numeric_check <- as.numeric(as.character(raw_data$index))
      indx_type <- "float"
      if (sum(!is.na(numeric_check)) > 0) { # has some numeric values
	tmp <- raw_data %>%
	  mutate(index = as.numeric(ifelse(index == "NA", "NaN", index))) %>%
          select(all_of(c(t_col, "index", "timeseries")), 
               any_of(c("metric", "SEup", "SElo")))
	indx_type <- "float"
         } else {   # all character values
	tmp <- raw_data %>%
          select(all_of(c(t_col, "index", "timeseries")), 
               any_of(c("metric", "SEup", "SElo"))) 
	indx_type <- "String"
         }
         
      # Copepods need the "station" combined with "timeseries" because timeseries values are not unique
      if("station" %in% colnames(raw_data) && "timeseries" %in% colnames(raw_data)){
        tmp$timeseries <- paste(raw_data$station,tmp$timeseries)
        }
      # Freshwater indicators need the "metric" combined with "timeseries" because timeseries values are not unique
      if(eid == "cciea_HB_FLOW" && "metric" %in% colnames(raw_data) && "timeseries" %in% colnames(raw_data)){
        tmp$timeseries <- paste(raw_data$timeseries,tmp$metric)
        }      
        
      # Sometimes date is YYYY-mm, sometimes it's YYYY-mm-ddThh:mm:ss - standardize monthly to YYYY-mm-dd
      if(is_monthly)tmp$time = ifelse(nchar(as.character(tmp$time))>=10,substring(tmp$time,1,10),as.character(tmp$time))
      if(is_monthly)tmp$time = ifelse(nchar(as.character(tmp$time))<10,paste0(substring(tmp$time,1,7),"-01"),as.character(tmp$time))
      # Add metric, SE columns if available
      if (has_metric && !"metric" %in% names(tmp)) tmp$metric <- NA
      if (has_se) {
        if (!"SEup" %in% names(tmp)) tmp$SEup <- NA
        if (!"SElo" %in% names(tmp)) tmp$SElo <- NA
        }
      
      combined_data <- bind_rows(combined_data, tmp)
      
      # Collect metadata. Each erddap dataset id could have multiple PI's, however assume each file is from same institution, PI, and background url
      # metadata for all the cciea timeseries id's for this ERDDAP_Dataset_ID to place in this file
      meta_row_all <- dset_df %>% filter(cciea_filename == mname) 
      # Get the common information from metadata for the first file
      meta_row <- meta_row_all %>% slice(1)  #gets first timeseries in file
      meta_collect$title <- c(meta_collect$title, meta_row$Dataset_Title)
      meta_collect$contact <- c(meta_collect$contact, meta_row$Contact)
      meta_collect$pi <- c(meta_collect$pi, meta_row$PI)
      meta_collect$units <- c(meta_collect$units, meta_row$Units)
      meta_collect$title <- c(meta_collect$title, meta_row$Dataset_Title)
      meta_collect$iname <- c(meta_collect$iname, meta_row$Long_variable_name)
      meta_collect$ename <- c(meta_collect$ename, meta_row$ERDDAP_variable_name)
      if(isTRUE(!is.na(meta_row$Background_Info)&& meta_row$Background_Info != ""))meta_collect$globalinfourl <- c(meta_collect$globalinfourl, meta_row$Background_Info)
      # tsname is array used for subset variables list
      meta_collect$tsname <- c(meta_collect$tsname, meta_row$ERDDAP_query_parameter)
   
      ids <- dset_df$CCIEA_timeseries_ID # cciea_timeseries_id's in this file
      # each indicator (cciea timeseries id) could have different summary and infoURL
      for (indid in ids){
      	meta_collect$inst <- c(meta_collect$inst, meta_row_all$Institution)
    	sumry = "";
    	thismeta <- meta_row_all %>% filter(CCIEA_timeseries_ID == indid)
    	if(nrow(thismeta)>1){
    	  print(paste(indid," defined ",nrow(thismeta)," times in the metadata spreadsheet: using first one, but need to fix this!"))
    	  thismeta <- thismeta %>% slice(1)  #gets first timeseries in file
    	  }
        sumry <- iconv(thismeta$Source_Data, from = "UTF-8", to = "ISO-8859-1", sub = "")
      	if(isTRUE(!is.na(thismeta$Additional_Calculations)&& thismeta$Additional_Calculations != ""))sumry <- paste0(sumry,"; Additional Calculations: ",iconv(thismeta$Additional_Calculations, from = "UTF-8", to = "ISO-8859-1", sub = ""))
        meta_collect$sumar <- paste0(meta_collect$sumar, sumry)
        }  # loop through indicators for this file
    } # loop through files for this dataset
    
        if(is.null(meta_collect$globalinfourl)||isTRUE(is.na(meta_collect$globalinfourl)&& meta_collect$globalinfourl != ""))meta_collect$globalinfourl <- global_info_url
   
    # Write NCCSV File
    con <- file(outfile, "w")
    writeLines(paste0('*GLOBAL*,Conventions,"COARDS, CF-1.6, ACDD-1.3, NCCSV-1.1"'), con)
    writeLines(paste0('*GLOBAL*,history,"last updated: ', version, '"'), con)
    writeLines(paste0('*GLOBAL*,title,"', paste(unique(meta_collect$title), collapse=";"), '"'), con)
    writeLines(paste0('*GLOBAL*,institution,"', paste(unique(meta_collect$inst), collapse=";"), '"'), con)
    writeLines(paste0('*GLOBAL*,summary,"', paste(unique(meta_collect$sumar), collapse=";"), '"'), con)
    writeLines(paste0('*GLOBAL*,infoUrl,"', paste(unique(meta_collect$globalinfourl), collapse=";"), '"'), con)
    writeLines(paste0('*GLOBAL*,contributor_name,"', paste(unique(meta_collect$pi), collapse=";"), '"'), con)
    writeLines(paste0('*GLOBAL*,contributor_email,"', paste(unique(meta_collect$contact), collapse=";"), '"'), con)
    
    # NCCSV Data Type Headers
    writeLines(paste0(t_col, ',*DATA_TYPE*,String'), con)
    writeLines(paste0(t_col, ',units,"', t_units, '"'), con)
    writeLines(paste0('index,*DATA_TYPE*,',indx_type), con)
    writeLines(paste0('index,units,"', paste(unique(meta_collect$units), collapse=";"), '"'), con)
    writeLines(paste0('index,long_name,"', paste(unique(meta_collect$iname), collapse=";"), '"'), con) # get long name from metadata
    writeLines('timeseries,*DATA_TYPE*,String', con)
    if (has_metric) writeLines('metric,*DATA_TYPE*,String', con)
    if (has_se) {
      writeLines('SEup,*DATA_TYPE*,float', con)
      writeLines('SElo,*DATA_TYPE*,float', con)
      }
    
    writeLines('*END_METADATA*', con)
    write.csv(combined_data, con, row.names = FALSE, quote = TRUE, na = "NaN")
    writeLines('*END_DATA*', con)
    close(con)
    
    # --- Add ERDDAP Dataset XML Node ---
    dnode <- xml_add_child(root, "dataset", type = erdsettype, datasetID = eid, active = "true")
    xml_add_child(dnode, "reloadEveryNMinutes", "2628000")
    xml_add_child(dnode, "fileDir", outdir)
    xml_add_child(dnode, "fileNameRegex", paste0(eid, ".csv"))
    xml_add_child(dnode, "recursive", "false")
    xml_add_child(dnode, "columnNamesRow", "1")
    xml_add_child(dnode, "firstDataRow", "2")
    xml_add_child(dnode, "defaultGraphQuery", 'time,',meta_row$ERDDAP_variable_name,'&',meta_row$ERDDAP_query_parameter,'="',meta_row$ERDDAP_query_value,'"&.draw=linesAndMarkers')
    v_att <- xml_add_child(dnode, "addAttributes")
      att <- xml_add_child(v_att, "att","Other")
  	xml_set_attr(att, "name", "cdm_data_type")
      att <- xml_add_child(v_att, "att","[standard]")
  	xml_set_attr(att, "name", "license")
      att <- xml_add_child(v_att, "att","(local files)")
  	xml_set_attr(att, "name", "sourceUrl")
      att <- xml_add_child(v_att, "att","CF Standard Name Table v70")
  	xml_set_attr(att, "name", "standard_name_vocabulary")
      att <- xml_add_child(v_att, "att",meta_row$ERDDAP_query_parameter)
  	xml_set_attr(att, "name", "subsetVariables")

    v_time <- xml_add_child(dnode, "dataVariable") #time
    xml_add_child(v_time, "sourceName", t_col)
    xml_add_child(v_time, "destinationName", "time")
    xml_add_child(v_time, "dataType", "String")
    t_att <- xml_add_child(v_time, "addAttributes")
      att <- xml_add_child(t_att, "att","Time")
  	xml_set_attr(att, "name", "ioos_category")
    
    v_indx <- xml_add_child(dnode, "dataVariable") #index
    xml_add_child(v_indx, "sourceName", 'index')
    xml_add_child(v_indx, "destinationName", meta_row$ERDDAP_variable_name)
    xml_add_child(v_indx, "dataType", indx_type)
    t_att <- xml_add_child(v_indx, "addAttributes")
      att <- xml_add_child(t_att, "att","Other")
  	xml_set_attr(att, "name", "ioos_category")
    
    v_indx <- xml_add_child(dnode, "dataVariable") #timeseries
    xml_add_child(v_indx, "sourceName", 'timeseries')
    xml_add_child(v_indx, "destinationName", meta_row$ERDDAP_query_parameter)
    xml_add_child(v_indx, "dataType", 'String')
    t_att <- xml_add_child(v_indx, "addAttributes")
      att <- xml_add_child(t_att, "att","Other")
  	xml_set_attr(att, "name", "ioos_category")
    
    if(has_se){
    
      v_indx <- xml_add_child(dnode, "dataVariable") #seup
      xml_add_child(v_indx, "sourceName", 'SEup')
      xml_add_child(v_indx, "destinationName", 'SEup')
      xml_add_child(v_indx, "dataType", 'float')
      t_att <- xml_add_child(v_indx, "addAttributes")
        att <- xml_add_child(t_att, "att","Other")
      	  xml_set_attr(att, "name", "ioos_category")

      v_indx <- xml_add_child(dnode, "dataVariable") #selo
      xml_add_child(v_indx, "sourceName", 'SElo')
      xml_add_child(v_indx, "destinationName", 'SElo')
      xml_add_child(v_indx, "dataType", 'float')
      t_att <- xml_add_child(v_indx, "addAttributes")
        att <- xml_add_child(t_att, "att","Other")
  	  xml_set_attr(att, "name", "ioos_category")
      }
  } ## end of erddap dataset id loop
  
  # Save XML
  pi_file <- PI_id
  combined_file <- "cciea_datasets"
  # Make individual or combined files
  ## xmlfile <-  pi_file
  xmlfile <-  combined_file
  xmloutdir <- file.path(xmloutpath, paste0(xmlfile,".xml"))
  write_xml(root, xmloutdir)
  }  ## end of this PI loop
}
