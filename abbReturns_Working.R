
### Preliminary Commands -----------------------------------------------------------------

 ## Set global options

  options(xtable.comment = FALSE)

 ## Load libraries

  library(spdep)
  library(maptools)
  library(gstat)
  library(kernlab)
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
  library(ROCR)

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
  
  load(paste0(data.path, '/analyzed/abb_working.RData'))
  load(paste0(data.path, '/analyzed/abb_objs.RData'))
  
 ## Load sources (custom function files)

  source("c:/code/dataviztools/ggplottools.R")
  source("c:/code/datamgmttools/dataMungetools.R")
  source("c:/code/research/AirBNBMelbourne/analysis_Functions.R")
  source("c:/code/research/AirBNBMelbourne/dataPrep_Functions.R")
  source("c:/code/spatialtools/point2surface_Tools.R")
  source("c:/code/spatialtools/spatiallogitmodels.R")
  
  sm.col <- abb.col[c(2, 1, 3, 5, 6)]
  
### Working analysis ---------------------------------------------------------------------  
  
  
  calcBookStr <- function(id, book.data){
    
    id.data <- book.data[book.data$property.id == id, 'status']
    
    # Divide by status and collapse
    status.string <- id.data[1]
    
    for(ss in 2:length(b)){
      
      if(b[jj] == b[[jj-1]]){
        x <- paste0(x,b[[jj]])
      } else {
        x <- paste0(x,'.',b[[jj]])
      }
    }
    
    c <- as.list(strsplit(x,'[.]')[[1]])
    
    
    
    
  }
  
  
  b <- daily.data[daily.data$property.id=='75924','status']
  
  x <- b[1]
  
  aa <- substr(c[[1]],1,1)
  for(ii in 1:length(c)){
    aa[ii] <- substr(c[[ii]],1,1)  
  }
  Q <- which(unlist(aa)=='B')
  cc <- c[which(unlist(aa)=='B')]
  longest.block <- max(unlist(lapply(cc, nchar)))
  
  
  
  
  
  

  