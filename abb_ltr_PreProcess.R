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
  data_path <- 'c:/dropbox/research/airBNB/data/'

  # Code Path
  code_path <- getwd()

 ## Load sources

  source(file.path(code_path, "abb_Functions.R"))
  
  # Load the raw rent data
  rent16 <- read.csv(file.path(data_path, 'raw', 'rentData.csv'), header=T)

 ## Remove observations without sufficient keys
  
  r16 <- rent16[!is.na(rent16$EventID), ]
  r16 <- r16[!is.na(r16$AddressID), ]
  r16 <- r16[!is.na(r16$ActivityID), ]
  
 ## Create Unique Key
  
  r16$id.key <- paste0(as.numeric(as.factor(r16$ActivityID)), '..',
                       as.numeric(as.factor(r16$AddressID)))
  r16$id.key <- as.numeric(as.factor(r16$id.key))
  
 ## Fix Dates  
  
  r16$last.adv.date <- apmFixDates(r16$LastAdvertisedEventDate)
  r16$first.adv.date <- apmFixDates(r16$FirstAdvertisedEventDate)
  r16$event.date <- apmFixDates(r16$EventDate)
  r16$event.year <- lubridate::year(r16$event.date)
  
 ## Remove those not occuring in 2015 or later
  
  r16 <- r16 %>% dplyr::filter(event.year >= 2014)
  
 ## Add an identifier that signifies last record in a set of advertisements (of record)  
    
  r16 <- r16[order(r16$event.date, decreasing=T), ]
  r16 <- r16[order(r16$id.key), ]
  r16$AID <- paste0(r16$id.key, '..', r16$last.adv.date)
  r16$of.record <- ifelse(duplicated(r16$AID), 0 ,1)
  
 ## Split into property data and listing data
  
  ltr.data <- r16[r16$of.record == 1,]
  listing.data <- r16[, c('id.key', 'EventID', 'ActivityID', 'AddressID', 'of.record',
                          'event.date', 'EventPrice', 'first.adv.date',
                          'FirstAdvertisedEventPrice', 'last.adv.date',
                          'LastAdvertisedEventPrice', 'EventTypeCode')]
  
  ## Remove extra fields from property data
  
  ltr.data <- ltr.data[, c('id.key', 'GeographicalID', 'EventID', 'AddressID', 
                             'ActivityID', 'FlatNumber', 'StreetNumber', 'StreetName',
                             'StreetType', 'Suburb', 'Postcode', 'Property_Latitude', 
                             'Property_Longitude', 'Street_Centroid_Latitude',
                             'Street_Centroid_Longitude', 'event.date', 'EventPrice',
                             'PropertyType', 'AreaSize', 'Bedrooms', 'Baths',
                             'Parking', 'HasStudy', 'HasCourtyard', 'HasBalcony',
                             'HasAirConditioning', 'HasGarage')]
   
 ## Add days on Market
  
  # Fix id.key
  listing.data$id.key <- as.character(listing.data$id.key)
  
  # Calculate all maximums
  dom.data <- listing.data %>%
    dplyr::group_by(id.key) %>%
    dplyr::summarize(min=min(event.date),
                     max=max(event.date),
                     dom=as.numeric(max-min))
  
  # Add to property data
  ltr.data$DOM <- dom.data$dom[match(ltr.data$id.key, dom.data$id.key)]
  
  # Convert all names to lower case
  names(ltr.data) <- tolower(names(ltr.data))
  names(listing.data) <- tolower(names(listing.data))
  
 ## Write out data
  
  export.path <- paste0(data_path, '/prepared/')
  save(ltr.data, file=paste0(export.path, 'ltpropdata.RData'))
  save(listing.data, file=paste0(export.path, 'ltlistdata.RData'))
  
 