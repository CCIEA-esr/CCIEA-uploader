## Runs GitHub actions to:
## 1. look at Google Drive and update uploader status
## 2. get most recent metadata from metadata spreadsheet
## 3. read and get most recent file conventions frpm spreadsheet
## Runs daily
## values of default function parameters (esr_year,last_year,headervars,headervarsmon)
##   are in _init.R (sourced in _pi_folders_to_json.R)
## init.R must be updated at the beginning of each ESR season
source("_init.R")
source("_gdrive.R")
source("_pi_folders_to_json.R")
check_upload_status(esr_year,metadata_spreadsheet_folder,meta_file_search,meta_param_file_search)
get_file_conventions(cciea_folders[2],file_name_conventions)
get_indices(esr_year,last_year,metadata_spreadsheet_folder,meta_file_search)
generate_file_status(esr_year,headervars,headervarsmon)
last_updated <- read_updated(esr_year)
print(last_updated)
