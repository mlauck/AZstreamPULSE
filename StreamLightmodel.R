#Initial package install--------------------------------------------
#devtools::install_github("psavoy/StreamLightUtils")
#devtools::install_github("psavoy/StreamLight")

##Load packages-----------------------------------------------------
library(StreamLight)
library(StreamLightUtils)

#1. Download NLDAS data (NC_NHC example) ----------------------------------
NLDAS_DL(
  save_dir = working_dir,
  Site_ID = "NC_NHC",
  Lat = 35.9925, 
  Lon = -79.0460, 
  startDate = "2017-01-01"
)

#Process the downloaded data
NLDAS_processed <- NLDAS_proc(
  read_dir = working_dir, 
  Site_IDs = "NC_NHC"
)

#Read in a table with initial site information
sites <- data(NC_site_basic)

#Download NLDAS data at NC_NHC
NLDAS_DL_bulk(
  save_dir = working_dir,
  site_locs = sites
)

#List of successfully downloaded sites
NLDAS_list <- stringr::str_sub(list.files(working_dir), 1, -11)

#Processing the downloaded NLDAS data
NLDAS_processed <- StreamLightUtils::NLDAS_proc(read_dir = working_dir, NLDAS_list)



#2. Download and process MODIS LAI data ----------------------------

#Make a table for the MODIS request 
request_sites <- sites[, c("Site_ID", "Lat", "Lon")] 

#Export your sites as a .csv for the AppEEARS request  
write.table(
  request_sites, 
  paste0(working_dir, "/NC_sites.csv"), 
  sep = ",", 
  row.names = FALSE,
  quote = FALSE, 
  col.names = FALSE
)



#3. Using stream_light---------------------------------------------

#Read in a driver file
data(NC_NHC_driver)
head(NC_NHC_driver)

#Extract tree height
extract_height(
  Site_ID = NC_params[, "Site_ID"], 
  Lat = NC_params[, "Lat"],
  Lon = NC_params[, "Lon"],
  site_crs = NC_params[, "epsg_crs"]
)


#Load the example driver file for NC_NHC
data(NC_NHC_driver)

#Run the model
NC_NHC_modeled <- stream_light(
  NC_NHC_driver, 
  Lat = 35.9925, 
  Lon = -79.0460, 
  channel_azimuth = 330, 
  bottom_width = 18.9, 
  BH = 0.1, 
  BS = 100, 
  WL = 0.1, 
  TH = 23, 
  overhang = 2.3, 
  overhang_height = NA, 
  x_LAD = 1
)

#Function for batching over multiple sites
batch_model <- function(Site, read_dir, save_dir){
  #Get the model driver
  driver_file <- readRDS(paste(read_dir, "/", Site, "_driver.rds", sep = ""))
  
  #Get model parameters for the site
  site_p <- params[params[, "Site_ID"] == Site, ]
  
  #Run the model
  modeled <- stream_light(
    driver_file, 
    Lat = site_p[, "Lat"], 
    Lon = site_p[, "Lon"],
    channel_azimuth = site_p[, "Azimuth"], 
    bottom_width = site_p[, "Width"], 
    BH = site_p[, "BH"],
    BS = site_p[, "BS"], 
    WL = site_p[, "WL"], 
    TH = site_p[, "TH"], 
    overhang = site_p[, "overhang"],
    overhang_height = site_p[, "overhang_height"], 
    x_LAD = site_p[, "x"]
  )
  
  #Save the output
  saveRDS(modeled, paste(save_dir, "/", Site, "_predicted.rds", sep = ""))
  
} #End batch_model 

#Applying the model to all sites
model_rd <- working_dir
model_sd <- working_dir

#Running the model
lapply(
  params[, "Site_ID"], 
  FUN = batch_model, 
  read_dir = model_rd,
  save_dir = model_sd
) 

#Take a look at the output
data(NC_NHC_predicted)
NC_NHC_predicted[1:2, ]


#4. Using aqualight-------------------------------
batch_model <- function(Site_ID, model_parameters, model_drivers){
  #Print a message to keep track of progress
  message(paste0("Generating modeled estimates for ", Site_ID))
  
  #Get the model driver
  driver_file <- model_drivers[[Site_ID]]
  
  #The input_output file contains the model inputs and outputs, let's select just the necessary
  #input columns to reduce confusion
  model_driver <- driver_file[, c("local_time", "offset", "jday", "Year", "DOY", "Hour", "SW_inc",
                                  "LAI", "kd_pred", "depth", "width")]
  
  #Get model parameters for the site
  site_p <- model_parameters[model_parameters[, "Site_ID"] == Site_ID, ]
  
  #Run the model
  modeled <- aqua_light(
    driver_file = model_driver, 
    Lat = site_p[, "Lat"], 
    Lon = site_p[, "Lon"],
    channel_azimuth = site_p[, "Azimuth"], 
    bankfull_width = site_p[, "width_harvey"], 
    BH = site_p[, "bank_height"],
    TH = site_p[, "TH"], 
    overhang = site_p[, "overhang"],
    overhang_height = site_p[, "overhang_height"],
    x_LAD = site_p[, "x"]
  )
  
  return(modeled)
  
} #End batch_model

modeled_estimates <- lapply(
  names(inputs_outputs),
  FUN = batch_model,
  model_parameters = site_parameters,
  model_drivers = inputs_outputs
)


