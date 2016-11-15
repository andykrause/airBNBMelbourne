
 ## Load Libraries

  library(stringr)
  
 ## Load custom functions  

  source('c:/code/research/airbnbmelbourne/dataPrep_Functions.R')
  
 ## Load Data  
  
  raw.path <- 'c:/dropbox/research/airBNB/data/raw/'
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
  
  r16$lastdate <- apmFixDates(r16$LastAdvertisedEventDate)
  r16$eventdate <- apmFixDates(r16$EventDate)
  r16$eventyear <- as.numeric(substr(r16$eventdate, 1, 4))
  
 ## Remove those not occuring in 2015 or later
  
  r16 <- r16[r16$eventyear >= 2014, ]
  
 ## Add an identifier that signifies last record in a set of advertisements (of record)  
    
  r16 <- r16[order(r16$eventdate, decreasing=T), ]
  r16 <- r16[order(r16$id.key), ]
  r16$AID <- paste0(r16$id.key, '..', r16$lastdate)
  r16$of.record <- ifelse(duplicated(r16$AID), 0 ,1)
  
 ## Split into property data and listing data
  
  prop.data <- r16[r16$of.record == 1,]
  list.data <- r16[, c('id.key', 'EventID', 'ActivityID', 'AddressID', 'of.record',
                       'eventdate', 'EventPrice', 'FirstAdvertisedEventDate',
                       'FirstAdvertisedEventPrice', 'LastAdvertisedEventDate',
                       'LastAdvertisedEventPrice', 'EventTypeCode')]
  
  ## Remove extra fields from property data
  
  prop.data <- prop.data[, c('id.key', 'GeographicalID', 'EventID', 'AddressID', 
                             'ActivityID', 'FlatNumber', 'StreetNumber', 'StreetName',
                             'StreetType', 'Suburb', 'Postcode', 'Property_Latitude', 
                             'Property_Longitude', 'Street_Centroid_Latitude',
                             'Street_Centroid_Longitude', 'EventDate', 'EventPrice',
                             'PropertyType', 'AreaSize', 'Bedrooms', 'Baths',
                             'Parking', 'HasStudy', 'HasCourtyard', 'HasBalcony',
                             'HasAirConditioning', 'HasGarage')]
   
 ## Add days on Market
  
  # Fix id.key
  list.data$id.key <- as.character(list.data$id.key)
  
  # Calculate all maximums
  list.max <- as.data.frame(tapply(list.data$eventdate, 
                                   list.data$id.key, 
                                   max))
  
  # Calculate all minimums
  list.min <- as.data.frame(tapply(list.data$eventdate, 
                                   list.data$id.key, 
                                   min))
  
  # Calc the DOM and convert to a data.frame
  dom <- list.max - list.min
  dom.df <- data.frame(id.key = rownames(list.max),
                       dom = dom)
  names(dom.df)[2] <- 'dom'
  
  # Add to property data
  prop.data$DOM <- dom.df$dom[match(prop.data$id.key, dom.df$id.key)]
  
 ## Write out data
  
  data.path <- 'c:/dropbox/research/airBNB/data/prepared/'
  write.csv(prop.data, paste0(data.path, 'ltpropdata.csv'), row.names=FALSE)
  write.csv(list.data, paste0(data.path, 'ltlistdata.csv'), row.names=FALSE)
  
 