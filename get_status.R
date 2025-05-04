## GitHub action to look at Google Drive and update uploader status, runs daily
source("_init.R")
source("_gdrive.R")
source("_pi_folders_to_json.R")
generate_file_status(esr_year,headervars,headervarsmon)
get_indices(esr_year,last_year)
last_updated <- read_updated(esr_year)
print(last_updated)


