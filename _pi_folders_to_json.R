library("jsonlite")
library("tidyverse")
library("dplyr")
library("data.table")

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
  print(paste("Starting generate_file_status ",now(),sep=""))
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
    if (length(pifiles$name) > 0) {
      for(f in 1:length(pifiles$name)){
        fileobj <- list()
    print(pifiles$name[f])
        if(grepl("metadata",pifiles$name[f])){
          pis$newmeta=1
          pis$newmetaupdate=localfromgmt(pifiles$drive_resource[[f]]$modifiedTime)
          }
        else{
          datares="Annual"
          fileobj <- list(name=pifiles$name[f],updated=pifiles$drive_resource[[f]]$modifiedTime,typechk=0,namechk=9,datares=datares)
          if(grepl(".csv",pifiles$name[f]))fileobj$typechk=1
          if(grepl("_M.csv",pifiles$name[f]) || grepl("Monthly",pifiles$name[f]))datares="Monthly"
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
  }

## Get just the last_updated date for display
read_updated <- function(esr_year){
  json_data <- fromJSON(paste("data/uploader_status_",esr_year,".json",sep=""), simplifyVector = FALSE)
  last_updated <- json_data$statusupdate
  return(last_updated)
  }

##---------get_indices----------------------------------
## read metadata file from Drive and output as json file
## file_folder - name of Drive folder where metadata file is located
## meta_file_search - partial name of metadata file, remainder is date and version
##    there can only be one metadata file in the folder
get_indices <- function(esr_year,last_year,metadata_spreadsheet_folder,meta_file_search){
  print(paste("Starting get_indices ",now(),sep=""))
  pifolders = get_PI_folders(cciea_folders[3])
  folder_id = find_folder_id(metadata_spreadsheet_folder)
  # search for any file with file_search in the name
  file=search_file_in_folder(meta_file_search,folder_id)
  print(file$name)
  df <- read_sheet(file$id)
  df1<- apply(df,2,as.character)
  fwrite(df1, file="data/metadata.csv",sep=",",quote="auto",na="")
  write.csv(df,"data/metadata.csv")
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
      piobj$this_year=this_yearfolder$id
      piobj$last_year=last_yearfolder$id
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
      print(paste0(piid," does not have a folder",sep=""))
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
  print(paste("Starting get_file_conventions ",now(),sep=""))
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

