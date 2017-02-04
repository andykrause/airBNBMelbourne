##########################################################################################
#                                                                                        #  
#   Data Preprocessing for the Short-Term Rental data of the ABB project                 #
#                                                                                        #  
##########################################################################################

 ## Load libraries

  library(chron)
  library(plyr)
  library(stringr)

  ## Set location

  # Get computer names
  comp.name <- Sys.info()['nodename']

  # Assign path based on computer name
  if(comp.name == '7020D-121777-W' | 
     comp.name == 'DESKTOP-1D7JO4J'){
  
    data.path <- 'c:/dropbox/research/airBNB/data/'
  
  } else {
  
    data.path <- 'other path'
  
  }

 ## Load sources

  source("c:/code/research/airbnbMelbourne/abb_Functions.R")

 ## Load Data  
   
  # Property Information

  str.data <- read.csv(paste0(data.path, '/raw/melbproperties.csv'), header=T)

  # Daily rental information

  daily.data  <- read.csv(paste0(data.path, '/raw/melbDaily.csv'), header=T)

 ## Fix Names
  
  names(str.data) <- tolower(names(str.data))
  names(daily.data) <- tolower(names(daily.data))

 ## Fix Dates  
  
  # Daily data
  daily.data$date <- as.Date(daily.data$date)

  # Property level data
  str.data$created.date <- as.Date(str.data$created.date)
  str.data$created.year <- as.numeric(substr(str.data$created.date, 1, 4))

 ## Add Summary Information

  # Summarize daily by property
  daily.summ.list <- lapply(as.list(str.data$property.id), abbCalcBookStr,
                            book.data=daily.data)

  # Extract to a df
  daily.summ <- rbind.fill(daily.summ.list)
  
  # Add summary data to property data
  
  str.data <- merge(str.data, daily.summ, 
                    by.x='property.id',
                    by.y='id')

 ## Write out data  

  save(str.data, file=paste0(data.path, 'prepared/stpropdata.RData'))
  save(daily.data, file=paste0(data.path, 'prepared/stdailydata.RData'))
  