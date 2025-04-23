library("jsonlite")

generate_file_status <- function(esr_year){
  
# Location of data upload folder. Each element of the vector is a folder below
# the previous elements. The last element is the Folder with the ESR years folders
  folder=c("_CCIEA","CCIEA ESR data","CCIEA Data Upload")
# CCIEA required/optional file column headers
  headervars=c("year","index","timeseries","metric","SEup","SElo")
  headervarsmon=c("time","index","timeseries","metric","SEup","SElo")
# The yearly folders are in the folder represented by the last element in the folder vector above
  folder_id=find_folder_id(folder[3])
  pifolders=find_folders_in_folder(folder_id)
  folderarray <- list()
  yearfolders <- list()
  for (p in 1:length(pifolders$name)){
    PI=pifolders$name[p]
    PIid=pifolders$id[p]
## find the id's of all the years in the folder
    PIyears=find_folders_in_folder(PIid)
    print(PI)
    pis <- list(name = PI,newmeta = 0,newmetaupdate="")
    piyearfolders <- list(name = PI)
    yearobj <- list()
    for(y in 1:nrow(PIyears)){
      yearobj <- append(yearobj,list(name=PIyears$name[y],id=PIyears$id[y]))
    }
    piobj<-list()
    pifiles=find_PI_files_in_esr_year(PIid,esr_year)
##    print(pifiles)
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
  statusobj$status<-folderarray
  output<-toJSON(statusobj, auto_unbox=TRUE)
  write(output,file=paste("data/uploader_status_",esr_year,".json",sep=""))

  }
  
read_updated <- function(esr_year){
  json_data <- fromJSON(paste("data/uploader_status_",esr_year,".json",sep=""), simplifyVector = FALSE)
  last_updated <- json_data$statusupdate
  return(last_updated)
  }


