
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

### Working analysis ---------------------------------------------------------------------  

### Comparison

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
  
  
  