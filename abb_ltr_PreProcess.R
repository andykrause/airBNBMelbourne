##########################################################################################
#                                                                                        #  
#   Data Preprocessing for the Long-Term Rental data of the ABB project                  #
#                                                                                        #  
##########################################################################################

 ## Load Libraries

  library(stringr)
  
 ## Load custom functions  

  source('c:/code/research/airbnbmelbourne/abb_Functions.R')
  
 ## Load Data  
  
  # Get computer names
  comp.name <- Sys.info()['nodename']
  
  # Assign path based on computer name
  if(comp.name == '7020D-121777-W' | 
     comp.name == 'DESKTOP-1D7JO4J'){
    
    data.path <- 'c:/dropbox/research/airBNB/data/'
    
  } else {
    
    data.path <- 'gideon path'
    
  }
  
  # Set Raw path
  raw.path <- paste0(data.path, '/raw/')
  
  # Load the raw rent data
  rent16 <- read.csv(paste0(raw.path, 'rentData.csv'), header=T)

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
  r16$event.year <- as.numeric(substr(r16$event.date, 1, 4))
  
 ## Remove those not occuring in 2015 or later
  
  r16 <- r16[r16$event.year >= 2014, ]
  
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
  list.max <- as.data.frame(tapply(listing.data$event.date, 
                                   listing.data$id.key, 
                                   max))
  
  # Calculate all minimums
  list.min <- as.data.frame(tapply(listing.data$event.date, 
                                   listing.data$id.key, 
                                   min))
  
  # Calc the DOM and convert to a data.frame
  dom <- list.max - list.min
  dom.df <- data.frame(id.key = rownames(list.max),
                       dom = dom)
  names(dom.df)[2] <- 'dom'
  
  # Add to property data
  ltr.data$DOM <- dom.df$dom[match(ltr.data$id.key, dom.df$id.key)]
  
  # Convert all names to lower case
  names(ltr.data) <- tolower(names(ltr.data))
  names(listing.data) <- tolower(names(listing.data))
  
 ## Write out data
  
  export.path <- paste0(data.path, '/prepared/')
  save(ltr.data, file=paste0(export.path, 'ltpropdata.RData'))
  save(listing.data, file=paste0(export.path, 'ltlistdata.RData'))
  
 