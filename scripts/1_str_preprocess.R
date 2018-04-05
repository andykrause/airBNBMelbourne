##########################################################################################
#                                                                                        #  
#   Data Preprocessing for the Short-Term Rental data of the ABB project                 #
#                                                                                        #  
##########################################################################################

 ## Load libraries

  library(plyr)
  library(stringr)
  library(lubridate)
  library(dplyr)
  library(multidplyr)

  ## Set locations

  # Assign path based on computer name
  data_path <- file.path('c:', 'dropbox', 'research', 'airBNB', 'data')
  
  ## Load sources

  source(file.path(getwd(), 'functions', "abb_Functions.R"))

 ## Load Data  
   
  # Property Information

  str_raw <- read.csv(file.path(data_path, 'raw', 'melbproperties.csv'), header=T)
  str2_raw <- read.csv(file.path(data_path, '/raw/melbproperties_to_mar2017.csv'), 
                        header=T)
  
  # Daily rental information

  daily_raw  <- read.csv(file.path(data_path, '/raw/melbDaily.csv'), header=T)
  daily2_raw  <- read.csv(file.path(data_path, '/raw/melbDaily_to_mar2017.csv'), 
                           header=T)
  
 ## Fix Names
  
  names(str_raw) <- tolower(names(str_raw))
  names(str2_raw) <- tolower(names(str2_raw))
  names(daily_raw) <- tolower(names(daily_raw))
  names(daily2_raw) <- tolower(names(daily2_raw))
  
 ## Determine unique observations
  
  # Structure Data
  str_df <- dplyr::bind_rows(str2_raw, str_raw)
  str_df <- str_df %>% dplyr::filter(!duplicated(property.id))  
  
  # Daily Data
  daily_df <- dplyr::bind_rows(daily2_raw, daily_raw) %>%
    dplyr::mutate(uniq_id = paste0(property.id, '_', date)) %>%
    dplyr::filter(!duplicated(uniq_id)) %>%
    dplyr::select(property.id, date, status, price, booked.date, reservation.id)
  
 ## Fix Dates  
  
  # Daily data
  daily_df$date <- lubridate::as_date(daily_df$date)
  
  # Property level data
  str_df$created.date <- lubridate::as_date(str_df$created.date)
  str_df$created.year <- lubridate::year(str_df$created.date)

 ## Add Summary Information

  # Create new cluster
  abb_cl <- get_default_cluster() 
  
  # Register function and variable
  cluster_copy(abb_cl, abbCalcBookStr)

  # Summarize daily by property
  daily_summ <- daily_df %>% 
    multidplyr::partition(property.id, cluster=abb_cl) %>% 
    dplyr::do(abbCalcBookStr(.)) %>% 
    dplyr::collect()
  
 # Add summary data to property data
  
  str_df <- str_df %>%
    dplyr::inner_join(daily_summ, 
                      by='property.id')
  
 ## Write out data  

  saveRDS(str_df, file=file.path(data_path, 'prepared', 'st_prop.RDS'))
  saveRDS(daily_df, file=file.path(data_path, 'prepared', 'st_daily.RDS'))
  
#*****************************************************************************************
#*****************************************************************************************