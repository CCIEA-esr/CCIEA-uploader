## GitHub action to look at Google Drive and update uploader status, runs daily
## valuess of function parameters are in _init.R (sourced in _pi_folders_to_json.R )
source("_gdrive.R")
source("_pi_folders_to_json.R")
generate_file_status(esr_year,headervars,headervarsmon)
get_indices(esr_year,last_year)
last_updated <- read_updated(esr_year)
get_file_conventions(cciea_folders[2],file_name_conventions)
print(last_updated)


