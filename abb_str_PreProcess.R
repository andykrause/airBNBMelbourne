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
  data_path <- 'c:/dropbox/research/airBNB/data/'
  
  # Code Path
  code_path <- getwd()
  
 ## Load sources

  source(file.path(code_path, "abb_Functions.R"))

 ## Load Data  
   
  # Property Information

  str.data <- read.csv(file.path(data.path, '/raw/melbproperties.csv'), header=T)
  str.data2 <- read.csv(file.path(data.path, '/raw/melbproperties_to_mar2017.csv'), 
                        header=T)
  
  # Daily rental information

  daily.data  <- read.csv(file.path(data.path, '/raw/melbDaily.csv'), header=T)
  daily.data2  <- read.csv(file.path(data.path, '/raw/melbDaily_to_mar2017.csv'), header=T)
  
 ## Fix Names
  
  names(str.data) <- tolower(names(str.data))
  names(str.data2) <- tolower(names(str.data2))
  names(daily.data) <- tolower(names(daily.data))
  names(daily.data2) <- tolower(names(daily.data2))
  
 ## Determine unique observations
  
  # Structure Data
  str_df <- dplyr::bind_rows(str.data2, str.data)
  str_df <- str_df %>% dplyr::filter(!duplicated(property.id))  
  
  # Daily Data
  daily_df <- dplyr::bind_rows(daily.data2, daily.data) %>%
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

  save(str_df, file=paste0(data.path, 'prepared/stpropdata.RData'))
  save(daily_df, file=paste0(data.path, 'prepared/stdailydata.RData'))
  