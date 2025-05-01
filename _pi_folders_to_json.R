library("jsonlite")
library(tidyverse)
library(dplyr)

## Get a dribble of PI folder names and ID's
# # A dribble: 19 × 3
##   name              id        drive_resource   
##   <chr>             <drv_id>  <list>           
## 1 Andrews           13hq... <named list [35]>
## 2 Bjorkstedt        1Fuh... <named list [35]>
## 3 Burke             1ek7... <named list [35]>
## 4 Cope              1ymd... <named list [35]>
## 5 .....
get_PI_folders <- function(){
# Data upload folder path on Google Drive.
  folder=c("_CCIEA","CCIEA ESR data","CCIEA Data Upload")
# CCIEA required/optional file column headers
  headervars=c("year","index","timeseries","metric","SEup","SElo")
  headervarsmon=c("time","index","timeseries","metric","SEup","SElo")
# Could follow the path above, but for now just find the id of last element that contains the yearly folders
  folder_id=find_folder_id(folder[3])
  pifolders=find_folders_in_folder(folder_id)
  return(pifolders)
  }

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

## create json file of files uploaded to Google Drive and check file headers
generate_file_status <- function(esr_year){
  pifolders = get_PI_folders()
  folderarray <- list()
#loop through PI folders
  for (p in 1:length(pifolders$name)){
    PI=pifolders$name[p]
    print(PI)
    PIid=pifolders$id[p]
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
          headercols <- list()
          if(fileobj$typechk==1){
            content=drive_read_string(as_id(pifiles$drive_resource[[f]]$id))
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

get_indices <- function(esr_year,last_year){
  pifolders = get_PI_folders()
  df = read.csv('data/metadata.csv')
  df_trimmed<- df %>% select(c('PI_ID','PI', 'Contact', 'Title','Component_Section','serve_flag'))
  meta <- df_trimmed %>% filter(serve_flag==1)
  pis <- distinct(meta,PI,PI_ID,Contact)
  pis <- arrange(pis,PI)
  piarray <- list()
  for (p in 1:length(pis$PI)){
    piobj <- list()
    pi=pis[p,]$PI
    piid=pis[p,]$PI_ID
    piobj$name=pi
    piobj$id=piid
    pifolderid=pifolders %>% filter(name==piid)
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
  pi_indices <- list()
  pi_indices$pis <- piarray
  output <- toJSON(pi_indices, auto_unbox=TRUE)
  write(output,file="data/items_meta_test.json")
  }










  }
