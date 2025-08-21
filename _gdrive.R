library("googledrive")
library("googlesheets4")
library("tidyverse")

#gargle is used from RStudio to authenticate Google Drive
#library("gargle")
#options(gargle_oauth_cache = "~/.cache/gargle",gargle_oauth_email="lynn.dewitt@noaa.gov")
#google_client <- gargle::gargle_oauth_client_from_json("/home/ldewitt/projects/IEA/docs/uploader/client_secret.json")
#drive_auth_configure(client = google_client)

## Nice recipe for accessing Google Drive/Sheets from GitHub Action
## https://www.obrien.page/blog/2023/03_10_google_and_github_actions/

## find the id of the folder with name folder_name
## will need to be more rigorous to account for duplicate named folders - assume no duplicate names for now
find_folder_id <- function(folder_name){
  query=paste("mimeType = 'application/vnd.google-apps.folder' and trashed=false and name = '",folder_name,"'",sep="")
  topfolder=drive_ls(q=query)
  id=topfolder$id[1]
  return(id)
}
## Return tibble of all the folders in a folder with id folder_id
find_folders_in_folder <- function(folder_id){
  query=paste("'",folder_id,"' in parents and mimeType = 'application/vnd.google-apps.folder' and trashed=false",sep="")
  folders=drive_find(orderBy="name",q=query)
  return(folders)
}
find_PI_files_in_esr_year <- function(PIid,esr_year){
  query=paste("'",PIid,"' in parents and mimeType = 'application/vnd.google-apps.folder' and name='",esr_year,"'",sep="") 
  yearfolder=drive_find(q=query) 
  query=paste("'",yearfolder$id,"' in parents and fileExtension = 'csv' and mimeType != 'application/vnd.google-apps.folder' and trashed=false",sep="")
 files = drive_find(orderBy="name",q=query)
 return(files)
}

## Return id of file named file_name in a folder with id folder_id
find_file_in_folder <- function(file_name,folder_id){
  query=paste("'",folder_id,"' in parents and mimeType != 'application/vnd.google-apps.folder' and name='",file_name,"'",sep="") 
  file = drive_find(q=query)
  return(file)
}

localfromgmt <- function(utc){
  local=format(as.POSIXct(utc, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S"),tz="America/Los_Angeles",usetz=TRUE)
  return(local)
}

## Return tibble of file whose name contains the string file_search in a folder with id folder_id
search_file_in_folder <- function(file_search,folder_id){
  query=paste("'",folder_id,"' in parents and mimeType != 'application/vnd.google-apps.folder' and name contains '",file_search,"'",sep="")
  file = drive_find(q=query)
  return(file)
}

## Copy a file to a backup folder, rename file in current folder with current date
## Rename first sheet in new file with new file name
## file_name_search: string to search for to find a file in the folder named folder
## folder: name of folder where file to back up is located
## backup_folder_name: name of folder to copy for backup
## backup file will have the same name with today's date appended
## returns a list containing name and id of new file
backup_file <- function(file_name_search, folder_id, backup_folder_name){
 file=search_file_in_folder(file_name_search,folder_id)
  new_file_name = paste0(file_name_search,"_",today())
  backup_folder_id=find_folder_id(backup_folder_name)
  ## Specify file name when copying or else Drive prepends "Copy of"
  drive_cp(file$id,path = backup_folder_id,name = file$name)
  drive_mv(file$id,name = new_file_name)
  sheet_rename(file$id, sheet = 1, new_name = new_file_name)
  return(list(name = new_file_name, id = file$id))
}

