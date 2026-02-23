library("jsonlite")
library("tidyverse")
library("dplyr")
library("data.table")
library("readxl")

##---------get_PI_folders----------------------------------
#' Get a dribble of PI folder names and ID's
#'
#' @param folder Upper level Google Drive folder name containing data provider folders.
#' @returns name, id, and drive resource object for all PI folders in the Google Drive folder named folder
#' A dribble: 19 × 3
#'   name              id        drive_resource   
#'   <chr>             <drv_id>  <list>           
#' 1 Andrews           13hq... <named list [35]>
#' 2 Bjorkstedt        1Fuh... <named list [35]>
#' 3 Burke             1ek7... <named list [35]>
#' 4 Cope              1ymd... <named list [35]>
#' 5 .....
#`
#' examples
#' get_PI_folders("CCIEA Data Upload")
#' 
get_PI_folders <- function(folder){
# find all PI folders in the Google Drive folder named folder
  folder_id=find_folder_id(folder)
  pifolders=find_folders_in_folder(folder_id)
  return(pifolders)
}

##---------get_pi_year_folders----------------------------------
## Get a json list of yearly folders and ID's for a PI given the ID of the upper PI folder
## example: {"name":"Andrews","files":[{"name":"2021-2022","id":"1cNg.."},{"name":"2022-2023","id":"1evr..."},{"name":"2023-2024","id":"12fI..."},{"name":"2024-2025","id":"1plt..."}]}
get_pi_year_folders <- function(PI,PIid){ 
  # find the id's of all the years in this PI folder
  PIyears=find_folders_in_folder(PIid)
  piyearfolder <- list()
  piyearfolder$name=PI
  yearobj <- list()
  for(y in 1:nrow(PIyears)){
    yearobj <- append(yearobj,list(list(name=PIyears$name[y],id=PIyears$id[y])))
    }
    piyearfolder$files = yearobj
  return(toJSON(piyearfolder, auto_unbox=TRUE))
  }   

##---------generate_file_status----------------------------------
## create json file of files uploaded to Google Drive and check file headers
generate_file_status <- function(esr_year,headervars,headervarsmon){
  print(paste0("Starting generate_file_status ",now()))
  file_naming <- fromJSON("data/cciea_naming_conventions.json")
  pifolders = get_PI_folders(cciea_folders[3])
  folderarray <- list()
#loop through PI folders
  for (p in 1:length(pifolders$name)){
    PI=pifolders$name[p]
    print(PI)
    PIid=pifolders$id[p]
    
    allowednames<-list()
    filenames <- file_naming %>%
      filter(id == PI)
    if(length(filenames$files)>0){
    	temp <- filenames$files[[1]]
    	newfiles <- temp %>%
      	    filter(!is.null(newname) & !is.null(title))
    	allowednames <- newfiles$newname
    	}
    pis <- list(name = PI,newmeta = 0,newmetaupdate="")
   
    piobj<-list()
    pifiles=find_PI_files_in_esr_year(PIid,esr_year)
## Loop through all the files in the esr_year folder
    if (length(pifiles$name) > 0) {
      for(f in 1:length(pifiles$name)){
        fileobj <- list()
    print(pifiles$name[f])
        ## if this is a metadata file
        if(grepl("metadata",pifiles$name[f])){
          pis$newmeta=1
          pis$newmetaupdate=localfromgmt(pifiles$drive_resource[[f]]$modifiedTime)
          }
        ## if this is an indicator data csv file
        else{
          datares="Annual"
          fileobj <- list(name=pifiles$name[f],updated=pifiles$drive_resource[[f]]$modifiedTime,typechk=0,namechk=9,datares=datares)
          if(grepl(".csv",pifiles$name[f]))fileobj$typechk=1
          # hack for copepods because Zoop file name doesn't contain monthly designation and sampling_frequency column in metadata is inconsistent
          if(grepl("_M.csv",pifiles$name[f]) || grepl("Monthly",pifiles$name[f]) || grepl("copepods",pifiles$name[f]))datares="Monthly"
          fileobj$datares=datares
          if(length(allowednames) > 0)(if(pifiles$name[f] %in% allowednames)fileobj$namechk=1 else fileobj$namechk=0)
          headercols <- list()
          if(fileobj$typechk==1){
            content=drive_read_string(as_id(pifiles$drive_resource[[f]]$id),encoding="UTF-8")
            if(!is.na(content))content=read_csv(content,show_col_types = FALSE)
            columns=names(content)
            headerchk=headervars
            if(datares=="Monthly")headerchk=headervarsmon
            check_cols <- function(x,columns){x %in% columns}
            headercols <- map(headerchk,check_cols,columns)
          }
# do this in check_upload_status instead
#          write_csv(content,paste0("data/timeseries_data/",pifiles$name[f],sep=""))
          fileobj$headerchk=headercols
          piobj=append(piobj,list(fileobj))
          }
        }
##    file[1,]
##    print(paste(files$name[f],files$drive_resource[[f]]$modifiedTime,files$drive_resource[[f]]$fileExtension,sep=" "))
      }
      pis$files=piobj
      folderarray <- append(folderarray,list(pis))
    }
  statusobj <- list()
  statusobj$statusupdate<-Sys.time()
  statusobj$esr_year <- esr_year
  statusobj$headervars <- headervars
  statusobj$headervarsmon <- headervarsmon
  statusobj$status <- folderarray
  output<-toJSON(statusobj, auto_unbox=TRUE)
  write(output,file=paste("data/uploader_status_",esr_year,".json",sep=""))
  return(statusobj$statusupdate)
  }

## Get just the last_updated date for display
read_updated <- function(esr_year){
  json_data <- fromJSON(paste("data/uploader_status_",esr_year,".json",sep=""), simplifyVector = FALSE)
  last_updated <- json_data$statusupdate
  return(last_updated)
  }

##---------get_indices----------------------------------
## read metadata file from Drive, get just a list of data providers and indicators
## output as json file
## metadata_spreadsheet_folder - name of Drive folder where metadata file is located
## meta_file_search - partial name of metadata file, remainder is date and version
##    there can only be one metadata file in the folder !!CHECK FOR THIS SOMEWHERE!!
## 
get_indices <- function(esr_year,last_year,metadata_spreadsheet_folder,meta_file_search){
  print(paste0("Starting get_indices ",now()))
  pifolders = get_PI_folders(cciea_folders[3])
  metadata_spreadsheet_folder_id = find_folder_id(metadata_spreadsheet_folder)
  # search for any file with file_search in the name
  file=search_file_in_folder(meta_file_search,metadata_spreadsheet_folder_id)
  print(file$name)
  df <- read_sheet(file$id)
  df1<- apply(df,2,as.character)
  fwrite(df1, file="data/CCIEA_metadata.csv",sep=",",quote="auto",na="")
  df_trimmed<- df %>% select(c('PI_ID','PI', 'Contact', 'Title','Component_Section','serve_flag'))
  meta <- df_trimmed %>% filter(serve_flag==1)
  pis <- distinct(meta,PI,PI_ID,Contact)
  pis <- arrange(pis,PI)
  piarray <- list()
  for (p in 1:length(pis$PI)){
    piobj <- list()
    pi=pis[p,]$PI
    piid=pis[p,]$PI_ID
    print(piid)
    piobj$name=pi
    piobj$id=piid
    pifolderid=pifolders %>% filter(name==piid)
    if(length(pifolderid$name)>0){
      pifolderid = pifolderid$id
      PIyears=find_folders_in_folder(pifolderid)
      piobj$contact=pis[p,]$Contact
      this_yearfolder <- PIyears %>% filter(name==esr_year)
      last_yearfolder <- PIyears %>% filter(name==last_year)
      uploadfolder <- PIyears %>% filter(name=="Uploaded_files")
      piobj$this_year=this_yearfolder$id
      piobj$last_year=last_yearfolder$id
      piobj$upload=uploadfolder$id
      indices=meta %>% filter(PI==pi)
      components <- distinct(indices,Component_Section)
      cobjarray <- list()
      for(c in 1:length(components$Component_Section)){
        cobj <- list()
        cobj$name = components$Component_Section[c]
        cindices <- indices %>% filter(Component_Section==components$Component_Section[c])
        cindices <- arrange(cindices,Title)
        cobj$indices <- cindices$Title
        cobjarray <- append(cobjarray,list(cobj))
       }
      piobj$components <- cobjarray
      piarray <- append(piarray,list(piobj))
    }
    else{
      print(paste0(piid," does not have a folder"))
      }
  }
  pi_indices <- list()
  pi_indices$esr_year = esr_year
  pi_indices$last_year = last_year
  pi_indices$pis <- piarray
  output <- toJSON(pi_indices, auto_unbox=TRUE)
  write(output,file="data/items_meta.json")
  }

##---------get_file_conventions----------------------------------
## get file naming conventions from file 'file_name' in folder 'file_folder'
## and located in Google Drive folder named folder
## current file name defined in _init.R, current folder cciea_folders[2] which is "CCIEA ESR data"
get_file_conventions <- function(file_folder,file_name){
  print(paste0("Starting get_file_conventions ",now()))
  folder_id = find_folder_id(file_folder)
  file=find_file_in_folder(file_name,folder_id)
  content <- read_sheet(file$id)
  df<- content %>% select(c('PI ID','Dataset Title', 'Name (CCIEA standardized)')) 
  pis <- unique(df[['PI ID']])
  piarray <- list()
  for (p in 1:length(pis)){
    piobj <- list()
    piobj$id <- pis[p]
    files <- df %>% filter(`PI ID`==pis[p] & !is.na(`Dataset Title`))
    filearray <- list()
    for (f in 1:nrow(files)){
      fobj <- list()
      fobj$title <- files[f,][["Dataset Title"]]
      fobj$newname <- files[f,][["Name (CCIEA standardized)"]]
      filearray <- append(filearray,list(fobj))
    }
    piobj$files <- filearray
    piarray <- append(piarray,list(piobj))
  }
  output <- toJSON(piarray, auto_unbox=TRUE)
  write(output,file="data/cciea_naming_conventions.json")
}

##---------check_upload_status----------------------------------
## look for files in "Uploaded_files", cleans them up, and moves them to esr_year folder
## back up original file data files before cleaning in "PI_original" folder
## 
check_upload_status <- function(esr_year,metadata_spreadsheet_folder,meta_file_search,meta_param_file_search){
  print(paste0("Starting check_upload ",now()))
  pifolders = get_PI_folders(cciea_folders[3])
  #loop through PI folders
  for (p in 1:length(pifolders$name)){
    PI=pifolders$name[p]
    print(PI)
    pi_folder_id=pifolders$id[p]
    PIyears=find_folders_in_folder(pi_folder_id)
    this_yearfolder <- PIyears %>% filter(name==esr_year)
    upload_folder_id <- PIyears %>% filter(name=="Uploaded_files")
    backup_folder_id <- PIyears %>% filter(name=="PI_original")
    
    if(nrow(PIyears)>0 && nrow(this_yearfolder)==1 && nrow(upload_folder_id)==1 && nrow(backup_folder_id)==1){
      ## look for new files in upload folder
      pifiles=find_PI_files_in_esr_year(pi_folder_id,"Uploaded_files")
    
      ## Loop through all the files in the upload folder
      if (length(pifiles$name) > 0) {
        for(f in 1:length(pifiles$name)){
          # check file format. If excel file download it, convert to csv, re-upload it as csv
          # move excel file to PI_original folder
          # change file extension to .csv and rename pifiles$name[f]
          # pifiles$id[f] will also need changing
          # maybe do in a separate loop then run find_PI_files.. again?
          
          
          
          
          
          fileobj <- list()
          print(pifiles$name[f])
          ## if this is a metadata file, incorporate it back into the full spreadsheet
          ## to-do !! NEED TO CHECK IF THEY UPLOADED MORE THAN ONE METADATA FILE!!
          ## back up their metadata file to backup_folder_id
          if(grepl("metadata",pifiles$name[f])){
            update_metadata(PI,pifiles$id[f],metadata_spreadsheet_folder,meta_file_search,meta_param_file_search)
            drive_mv(file = pifiles$id[f], path = backup_folder_id)     	
            }
          ## if this is an indicator csv data file or spreadsheet - clean it, move it to esr_year folder, and plot it
          else if(grepl(".csv",pifiles$name[f])){
           datares="Annual"
           # hack for copepods because Zoop file name doesn't contain monthly designation and sampling_frequency column in metadata is inconsistent
           if(grepl("_M.csv",pifiles$name[f]) || grepl("Monthly",pifiles$name[f]) || grep("copepods",pifiles$name[f]))datares="Monthly"
           mime_type <- pifiles$drive_resource[[f]]$mimeType
           created_time <- pifiles$drive_resource[[f]]$createdTime
           if(mime_type=="text/csv"){
             content <- drive_read_string(pifiles$id[f])
              df <- read_csv(content)
             }
            else if(mime_type=="application/vnd.google-apps.spreadsheet"){
              df <- read_sheet(pifiles$id[f])
              }
            ## clean up the file if needed
            df_cleaned <- clean_file(df,datares)
           outfile <- paste0("data/timeseries_data/",pifiles$name[f],sep="")
           write_csv(df_cleaned,outfile, na="")
#          write.csv(df_cleaned, file = "temp.csv",row.names = FALSE)
           # use drive_put instead of drive_upload, drive_upload fails when file already exists, even if overwrite=TRUE
           temp_file <- tempfile(fileext = ".csv")
           write_csv(df_cleaned,temp_file, na="")
           drive_put(media=temp_file,name=pifiles$name[f],path=this_yearfolder,type="text/csv")
           unlink(temp_file)
           ## to-do could check for backup folder and create it if not already there -
           ##		backup=drive_mkdir("PI_original",path=pifolderid,overwrite=FALSE)
            drive_mv(file = pifiles$id[f], path = backup_folder_id)     	
          
         }
          ## If this is some other type of upload, just move it to esr_year folder
          else{
            drive_mv(file = pifiles$id[f], path = this_yearfolder)
          }
       }
     }
    }
    else{
      print("PI folder structure missing required folder")
    }
  }
}

##---------clean_file----------------------------------
## apply Nick's file cleaning code to df
clean_file <- function(df,datares){
  # fix columns
  #if(df[1,1]=="UTC"){df = data.frame(read.table(Data.File, header = TRUE, skip=1, sep=","))}
  cn = colnames(df)
  if(!("index" %in% cn))cn[cn%in%c("data","Data","fitted.data","Fitted.data","mean","count","kg.day","anomaly", "kg", "km","Annual.Anomaly","ln.catch.1.","ONI","PDO","NPGO")]<-"index"
  if(!("timeseries" %in% cn))cn[cn%in%c("time.series","TimeSeries","Time.Series")]<-"timeseries"
  cn[cn%in%c("Metric")]<-"metric"
  cn[cn%in%c("Month")]<-"month"
  cn[cn%in%c('Day','day')]<- 'day'
  cn[cn%in%c("se","standard.error","error")]<-"SE"
  cn[cn%in%c("sd","standard.deviation", "stdev")]<-"SD"
  cn[cn%in%c("raw.data","Raw.Data")]<-"Y2"
  if(datares=="Annual")cn[cn%in%c("Year","date","Date","time","UTC","time..UTC.")]<-"year"
  colnames(df) <- cn

  ## don't change year column to time if there is already a time column
  if (datares == "Monthly" & !("time" %in% cn)) {
    # if there's not a time column, but there are year, month, day columns -> make year and month into time
      yr=grep("year",cn)
      mth = grep("month",cn)
      day = grep("day",cn)
      if(length(day==1)){DAY = df$day}else{DAY=15}
      if(length(mth)==1){
        df$month = ifelse(nchar(df$month)==1,paste(0,df$month,sep=""),df$month)
        df$time = paste(df$year,df$month,DAY,sep='-')
        }
      # fix time to 10 places
        df$time = as.character(df$time)
        df$time = ifelse(nchar(df$time)>10,substring(df$time,1,10),df$time)
       #cn[cn %in% c("Year", "date", "Date", "year", "UTC", "time..UTC.")] <- "time"
    }

  return(df)
}

##---------update_metadata----------------------------------
## incorporate edited metadata back into full spreadsheet
## backup present spreadsheet first
## metadata_spreadsheet_folder - name of Drive folder where metadata file is located
## meta_file_search - partial name of metadata file, remainder is date and version
## meta_param_file_search - partial name of parameter table file ie full name is "CCIEA_parameter_table_YYYYMMDD.csv" in metadata_spreadsheet_folder to identify metadata columns
## Case for adding new data needs to be developed, currently uses CCIEA_timeseries_ID to match new metadata to old
update_metadata <- function(PIid,meta_uploaded_fileid,metadata_spreadsheet_folder,meta_file_search,meta_param_file_search){
  print(paste0("Starting update_metadata ",now()))
  #backup spreadsheet before updating
  meta_folder_id=find_folder_id(metadata_spreadsheet_folder)
  backup_folder_name="Older Metadata Spreadsheets"
  # new_meta_file contains the old metadata, but it is the one we will be updating
  new_meta_file=backup_file(meta_file_search,meta_folder_id,backup_folder_name)
  print(new_meta_file)

## Original code had stuff here about nccsv header - future just add header from oceanview side?
# Read the parameter table to create a mapping for metadata column names
# This assumes the second column indicates if a parameter is metadata (1) or not (0)
  # Create a named vector to map ERDDAP names to the names used in the final CSV
  # This will be used to rename columns from new metadata files.
  # `setNames(value, name)`
  param_file=search_file_in_folder(meta_param_file_search,meta_folder_id)
  param_table <- read_sheet(param_file$id)
  col_name_map <- param_table %>%
    filter(.[[2]] == 1) %>% # Filter rows where the second column is 1
    { setNames(.$`name in csv file`, .$`ERDDAP name`) }

# read large metadata file on drive
  old_meta =read_sheet(new_meta_file$id)
  old_meta$ERDDAP_query_value <- as.character(old_meta$ERDDAP_query_value)
  column_types <- sapply(old_meta,class) # returns named vector
  col_list=as.list(column_types)

csv=drive_read_string(meta_uploaded_fileid)
new_meta_header <- read_csv(csv,n_max=1, show_col_types = FALSE,na.strings = c("NA", "NULL", ""))  # (lat/lon are "logical")

temp=old_meta[0,]
common_cols <- intersect(names(new_meta_header),names(temp))
col_types <- col_list[common_cols]

new_meta_chunk <- read_csv(csv,col_types =col_types, show_col_types = FALSE)

# Check that the metadata contains the required ID column
if (!"CCIEA_timeseries_ID" %in% colnames(new_meta_chunk)) {
          warning(paste("  -> WARNING: No 'CCIEA_timeseries_ID' column in", PIid, " metadata. Skipping file."))
          next
        }
# Filter out any rows that are missing the timeseries ID
# Figure out what to do for adding NEW data - flag it here -> notification?
new_meta_chunk <- new_meta_chunk %>% filter(!is.na(CCIEA_timeseries_ID))

old_meta <- rows_update(old_meta,new_meta_chunk,by = "CCIEA_timeseries_ID")
old_meta[is.na(old_meta)] <- ""

## write to Google Drive
sheet_write(data = old_meta, ss=new_meta_file$id, sheet = 1)

## also write to GitHub as csv file
write_csv(x = old_meta, file = "data/CCIEA_metadata.csv",na="")

## also write to nccsv file for ERDDAP
# Rename columns from ERDDAP names to the final CSV names using our map
# needs to be reversed - this named it the other way I think
#new_meta_renamed <- new_meta_chunk %>% rename_with(~ col_name_map[.], .cols = any_of(names(col_name_map)))


}
