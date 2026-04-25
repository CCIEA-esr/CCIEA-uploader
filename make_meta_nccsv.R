# generates nccsv file and ERDDAP xml for CCIEA_metadata.csv

library(tidyverse)
library(jsonlite)
library(xml2)

source("_init.R")

# --- Configuration & Arguments ---

#path <- "/home/ldewitt/Documents/GitHub/CCIEA-uploader/data/"
path <- "data/"
infile <- paste0(path, "CCIEA_metadata.csv")
nccsvoutpath <- paste0(path, "erddap_files/nccsv/") # where to write the nccsv files on GitHub
outfile <- "CCIEA_metadata.csv"
outfilepath <- paste0(nccsvoutpath, outfile)
xmloutfile <- "cciea_metadata_datasets.xml"
xmloutpath <- paste0(path, "erddap_files/datasets_xml/") # where to write the output xml files on GitHub
erdsettype <- "EDDTableFromNccsvFiles"
serverdatapath <- paste0("/data/cciea_uploader_",esr_year,"/nccsv/") # data path on server for data in datasets.xml
eid <- "CCIEA_metadata"
title <- "CCIEA Indicator List and Metadata"
datasetID <- "CCIEA_metadata"
contibutor_name <- "Greg Williams"
contibutor_email <- "greg.williams@noaa.gov"
creator_name <- "Lynn deWitt"
creator_email <- "lynn.dewitt@noaa.gov"
infourl <- "https://www.integratedecosystemassessment.noaa.gov/regions/california-current/about-california-current-integrated-ecosystem-assessment"
institution <- "NOAA/NMFS SWFSC,NWFSC"
summary <- "The CCIEA is an interdisciplinary research effort led by U.S. West Coast NOAA scientists. Our goal is to provide science support for ecosystem-based management of the California Current - a complex ecosystem in which natural and human systems are inextricably linked. We evaluate the status of the California Current Ecosystem by interpreting a variety of environmental, biological, economic, and social indicators. Current and historical indicator data are listed in this ERDDAP dataset."
defaultdataquery <- "principal_investigator%2Cinstitution%2Cdataset_title%2Cindicator_title%2Ccomponent%2Csubcomponent%2Cerddap_dataset_id&currently_served=1"
defaultgraphquery <- 'longitude%2Clatitude%2C&amp;.draw=markers'
calculated_cols  <- c("trend", "status", "year_begin", "year_end", "default_figure", "min", "max")
subsetvariables <- "principal_investigator_id,principal_investigator,contact,indicator_title,dataset_title,institution,erddap_dataset_id,region,sampling_frequency,component,subcomponent,filename,cciea_filename,currently_served,pfmc_flag,sanctuary_flag,whale_entanglement_flag,no_download_flag,salmon_web_flag,trend,status"
     
#ind_in_meta <- read_csv("https://oceanview.pfeg.noaa.gov/erddap/tabledap/CCIEA_metadata.csv0?cciea_timeseries_id&distinct()")

#version <- status_data$statusupdate

parameter_table  <- "CCIEA_parameter_table_20230114.csv"
outxml           <- "CCIEA_metadata.xml"

today_date       <- Sys.Date()


# Load Parameter Metadata
meta_params <- read_csv(paste0(path, parameter_table),show_col_types = FALSE, na = character()) %>%
  filter(`Serve in ERDDAP` == 1)

# Google Drive Interaction

metadata_df <- read_csv(infile, show_col_types = FALSE)

# Ensure calculated columns exist
for (col in calculated_cols) {
  if (!(col %in% names(metadata_df))) metadata_df[[col]] <- NA
}

# Write NCCSV File 
con <- file(outfilepath, "w")
# Write Global Attributes
writeLines('*GLOBAL*,Conventions,"COARDS, CF-1.6, ACDD-1.3, NCCSV-1.1"', con)
writeLines(paste0('*GLOBAL*,history,"last updated: ', today_date, '"'), con)
writeLines('*GLOBAL*,acknowledgement,', con)
writeLines(paste0('*GLOBAL*,contributor_name,"', contibutor_name,'"'), con)
writeLines(paste0('*GLOBAL*,contributor_email,"', contibutor_email,'"'), con)
writeLines(paste0('*GLOBAL*,creator_name,"', creator_name,'"'), con)
writeLines(paste0('*GLOBAL*,creator_email,"', creator_email,'"'), con)
writeLines(paste0('*GLOBAL*,infoUrl,"', infourl,'"'), con)
writeLines(paste0('*GLOBAL*,institution,"', institution,'"'), con)
writeLines(paste0('*GLOBAL*,summary,"', summary,'"'), con)
writeLines(paste0('*GLOBAL*,title,"', title,'"'), con)

# Write Variable Attributes
for (i in 1:nrow(meta_params)) {
  p <- meta_params[i, ]
  writeLines(paste0(p$`name in csv file`, ",*DATA_TYPE*,", p$type), con)
  if (!is.na(p$units)) writeLines(paste0(p$`name in csv file`, ",units,\"", p$units, "\""), con)
}

writeLines("*END_METADATA*", con)
close(con)
con <- file(outfilepath, open="ab")
write_csv(metadata_df, con, append = TRUE, col_names = TRUE, na = "")
writeLines("*END_DATA*", con)
close(con)

# Generate ERDDAP XML
doc <- xml_new_root("erddapDatasets")
    dnode <- xml_add_child(doc, "dataset", type = erdsettype, datasetID = eid, active = "true")
    xml_add_child(dnode, "reloadEveryNMinutes", "2628000")
    xml_add_child(dnode, "fileDir", serverdatapath)
    xml_add_child(dnode, "fileNameRegex", outfile)
    xml_add_child(dnode, "recursive", "false")
    xml_add_child(dnode, "defaultGraphQuery", defaultgraphquery)
    xml_add_child(dnode, "defaultDataQuery", defaultdataquery)
    v_att <- xml_add_child(dnode, "addAttributes")
      att <- xml_add_child(v_att, "att","Other")
  	xml_set_attr(att, "name", "cdm_data_type")
      att <- xml_add_child(v_att, "att","[standard]")
  	xml_set_attr(att, "name", "license")
      att <- xml_add_child(v_att, "att","(local files)")
  	xml_set_attr(att, "name", "sourceUrl")
      att <- xml_add_child(v_att, "att","CF Standard Name Table v70")
  	xml_set_attr(att, "name", "standard_name_vocabulary")
      att <- xml_add_child(v_att, "att",subsetvariables)
  	xml_set_attr(att, "name", "subsetVariables")

# Add Data Variables from meta_params
for (i in 1:nrow(meta_params)) {
  p <- meta_params[i, ]
  dv <- xml_add_child(dnode, "dataVariable")
  xml_add_child(dv, "sourceName", p$`name in csv file`)
  xml_add_child(dv, "destinationName", p$`ERDDAP name`)
  xml_add_child(dv, "dataType", p$type)
      t_att <- xml_add_child(dv, "addAttributes")
        att <- xml_add_child(t_att, "att","Other")
  	  xml_set_attr(att, "name", "ioos_category")
}

write_xml(doc, paste0(xmloutpath, xmloutfile))

