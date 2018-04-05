##########################################################################################
#                                                                                        #  
#   Data Preprocessing for the Long-Term Rental data of the ABB project                  #
#                                                                                        #  
##########################################################################################


 ## Load Libraries

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
  
  # Load the raw rent data
  rent_df <- read.csv(file.path(data_path, 'raw', 'rentData.csv'), header=T)

 ## Remove observations without sufficient keys
  
  r_df <- rent_df[!is.na(rent_df$EventID), ]
  r_df <- r_df[!is.na(r_df$AddressID), ]
  r_df <- r_df[!is.na(r_df$ActivityID), ]
  
 ## Create Unique Key
  
  r_df$id.key <- paste0(as.numeric(as.factor(r_df$ActivityID)), '..',
                       as.numeric(as.factor(r_df$AddressID)))
  r_df$id.key <- as.numeric(as.factor(r_df$id.key))
  
 ## Fix Dates  
  
  r_df$last.adv.date <- apmFixDates(r_df$LastAdvertisedEventDate)
  r_df$first.adv.date <- apmFixDates(r_df$FirstAdvertisedEventDate)
  r_df$event.date <- apmFixDates(r_df$EventDate)
  r_df$event.year <- lubridate::year(r_df$event.date)
  
 ## Remove those not occuring in 2015 or later
  
  r_df <- r_df %>% dplyr::filter(event.year >= 2014)
  
 ## Add an identifier that signifies last record in a set of advertisements (of record)  
    
  r_df <- r_df[order(r_df$event.date, decreasing=T), ]
  r_df <- r_df[order(r_df$id.key), ]
  r_df$AID <- paste0(r_df$id.key, '..', r_df$last.adv.date)
  r_df$of.record <- ifelse(duplicated(r_df$AID), 0 ,1)
  
 ## Split into property data and listing data
  
  ltr_df <- r_df[r_df$of.record == 1,]
  listing_df <- r_df[, c('id.key', 'EventID', 'ActivityID', 'AddressID', 'of.record',
                          'event.date', 'EventPrice', 'first.adv.date',
                          'FirstAdvertisedEventPrice', 'last.adv.date',
                          'LastAdvertisedEventPrice', 'EventTypeCode')]
  
  ## Remove extra fields from property data
  
  ltr_df <- ltr_df[, c('id.key', 'GeographicalID', 'EventID', 'AddressID', 
                             'ActivityID', 'FlatNumber', 'StreetNumber', 'StreetName',
                             'StreetType', 'Suburb', 'Postcode', 'Property_Latitude', 
                             'Property_Longitude', 'Street_Centroid_Latitude',
                             'Street_Centroid_Longitude', 'event.date', 'EventPrice',
                             'PropertyType', 'AreaSize', 'Bedrooms', 'Baths',
                             'Parking', 'HasStudy', 'HasCourtyard', 'HasBalcony',
                             'HasAirConditioning', 'HasGarage')]
   
 ## Add days on Market
  
  # Fix id.key
  listing_df$id.key <- as.character(listing_df$id.key)
  
  # Calculate all maximums
  dom_df <- listing_df %>%
    dplyr::group_by(id.key) %>%
    dplyr::summarize(min=min(event.date),
                     max=max(event.date),
                     dom=as.numeric(max-min))
  
  # Add to property data
  ltr_df$DOM <- dom.data$dom[match(ltr_df$id.key, dom.data$id.key)]
  
  # Convert all names to lower case
  names(ltr_df) <- tolower(names(ltr_df))
  names(listing_df) <- tolower(names(listing_df))
  
 ## Write out data
  
  export_path <- file.path(data_path, 'prepared')
  saveRDS(ltr_df, file=paste0(export_path, 'ltpropdata.RDS'))
  saveRDS(listing_df, file=paste0(export_path, 'ltlistdata.RDS'))
  
 