
### Preliminary Commands -----------------------------------------------------------------

 ## Set global options

  options(xtable.comment = FALSE)

 ## Load libraries

  library(spdep)
  library(maptools)
  library(gstat)
  library(ggplot2)
  library(geosphere)
  library(ggmap)
  library(xtable)
  library(chron)
  library(plyr)
  library(sp)
  library(rgeos)
  library(reshape2)
  library(stringr)
  library(RColorBrewer)
  library(Hmisc)

 ## Set path location

  # Get computer names
  comp.name <- Sys.info()['nodename']

  # Assign path based on computer name
  if(comp.name == '7020D-121777-W' | 
      comp.name == 'DESKTOP-1D7JO4J'){
  
    data.path <- 'c:/dropbox/research/airBNB/data/'
  
  } else {
  
    data.path <- 'gideon path'
  
  }

 ## Load working workspace   
  
  load("C:/Dropbox/Research/airBNB/data/analyzed/abb_working.RData")
  
 ## Load sources (custom function files)

  source("c:/code/dataviztools/ggplottools.R")
  source("c:/code/datamgmttools/dataMungetools.R")
  source("c:/code/research/AirBNBMelbourne/analysis_Functions.R")
  source("c:/code/research/AirBNBMelbourne/dataPrep_Functions.R")

  ltr.dataf <- rent.dataf
  
### Working analysis ---------------------------------------------------------------------  

  exch.rate <- 1.32

 ## Calculate actual revenue for study period for AirBnb
  
  # Determine # of booking for each airbnb
  abb.dataf$bookings <- (abb.dataf$total.days - 
                          abb.dataf$blocked.days - 
                           abb.dataf$available.days)
  
  # Determine property specific occupancy rate
  abb.dataf$occ.rate <- (abb.dataf$bookings / 
                           (abb.dataf$total.days - abb.dataf$blocked.days))
  
  # Set the blocked rate
  abb.dataf$blocked.rate <- abb.dataf$blocked.days / abb.dataf$total.days
  
  # Set the extrapolation parameter for those on the site for less than whole period
  abb.dataf$extr.par <- 366 / abb.dataf$total.days
  
  # Get all of the reservations
  resv.daily <- daily.dataf[daily.dataf$status == 'R' &
                             daily.dataf$booked.date != 'imputed' &
                              daily.dataf$price < 600, ]
  
  # Calculate the property specific median nightly rate
  med.daily <- tapply2DF(resv.daily$price, resv.daily$property.id, median)
  
  # Add median rate to the property  
  abb.dataf$med.rate <- med.daily$Var[match(abb.dataf$property.id, med.daily$ID)] 
  
  # Calculate the total revenue  
  abb.dataf$revenue <- (abb.dataf$med.rate * exch.rate * 
                          abb.dataf$extr.par * abb.dataf$bookings)
  
  # Remove those with no revenue
  abb.dataf <- abb.dataf[!is.na(abb.dataf$revenue), ]
  
 ## Calculate ltral revenues  
  
  # Calculate the actual revenue
  ltr.dataf$revenue <- ltr.dataf$event.price * (52 - ltr.dataf$dom/7)
  
  # Remove those with very low revenues (due to very long DOMs)
  ltr.dataf <- ltr.dataf[ltr.dataf$revenue > 5000, ]
  
 ## Save for future costs
  
### Comparison
  
 ## Data prep
    
  # Re-order factors
  abb.dataf$bedbath <- factor(abb.dataf$bedbath, 
                              levels=c('2..2', '1..1', '2..1', '3..1', '3..2', '4..2'))
  ltr.dataf$bedbath <- factor(ltr.dataf$bedbath, 
                               levels=c('2..2', '1..1', '2..1', '3..1', '3..2', '4..2'))

  # Add product as a variable
  abb.dataf$product <- paste0(substr(abb.dataf$type, 1, 1), abb.dataf$bedbath)
  ltr.dataf$product <- paste0(substr(ltr.dataf$type, 1, 1), ltr.dataf$bedbath)

  # Convert suburbs from factor to character
  abb.dataf$suburb <- as.character(abb.dataf$suburb)
  ltr.dataf$suburb <- as.character(ltr.dataf$suburb)

 ## Global model

  # We begin by imputing rents and rates for all properties using a global model with 
  # suburb fixed effects.  
 
  # Set model specifications
  ltr.mod.spec <- formula(log(event.price) ~ as.factor(bedbath) + as.factor(suburb))
  abb.mod.spec <- formula(log(nightly.rate) ~ as.factor(bedbath) + as.factor(suburb))
 
  glob <- fullMarketAnalysis(ltr.df=ltr.dataf,
                             abb.df=abb.dataf,
                             ltr.mod.spec=ltr.mod.spec,
                             abb.mod.spec=abb.mod.spec,
                             exch.rate=exch.rate,
                             clip.field='suburb')
  
  aa <- which(ltr.dataf$sub.mrkt=='city-core')
  bb <- which(abb.dataf$sub.mrkt=='city-core')

  beach <- fullMarketAnalysis(ltr.df=ltr.dataf[aa,],
                             abb.df=abb.dataf[bb, ],
                             ltr.mod.spec=ltr.mod.spec,
                             abb.mod.spec=abb.mod.spec,
                             exch.rate=exch.rate,
                             clip.field='suburb')
  
  
  