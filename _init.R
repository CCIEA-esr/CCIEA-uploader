## Default parameters for GitHub Actions
## This file must be updated at the beginning of each ESR Season
## Google Drive folders for each data provider must exist for esr_year and be named consistently
esr_year="2025-2026"
last_year="2024-2025"
# CCIEA required/optional file column headers
headervars=c("year","index","timeseries","metric","SEup","SElo")
headervarsmon=c("time","index","timeseries","metric","SEup","SElo")
# CCIEA folder names on Google Drive so we don't have to remember them
cciea_folders=c("_CCIEA","CCIEA ESR data","CCIEA Data Upload")
# name of Greg's file naming conventions spreadsheet
file_name_conventions="Data File Naming Conventions"
# name of folder where metadata spreadsheet is located
metadata_spreadsheet_folder="ERDDAP metadata spreadsheet"
# partial name of metadata file, the rest of the name can be the date and version, but there can only be one metadata file in the folder
meta_file_search="CCIEA_metadata"
# partial name of parameter file that defines columns in metadata, located in metadata_spreadsheet_folder
meta_param_file_search="CCIEA_parameter_table"
