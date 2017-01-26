
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
  
  
  
  
  
calcBookStr <- function(id, 
                        book.data){
    
  id.data <- book.data[book.data$property.id == id, 'status']
    
  if(length(id.data) > 1){
    
    # Divide by status and collapse
    status.string <- id.data[1]
    
    for(ss in 2:length(id.data)){
      
      if(id.data[ss] == id.data[[ss - 1]]){
        status.string <- paste0(status.string, id.data[[ss]])
      } else {
        status.string <- paste0(status.string, '.' ,id.data[[ss]])
      }
    }
    
    # Collapse into list objects
    ss.list <- as.list(strsplit(status.string, '[.]')[[1]])
    
    # Grab the first status of each
    sg.obj <- substr(ss.list[[1]], 1, 1)
    for(sg in 1:length(ss.list)){
      sg.obj[sg] <- substr(ss.list[[sg]], 1, 1)  
    }
    
    # Find location of three types
    id.B <- which(unlist(sg.obj) == 'B')
    id.R <- which(unlist(sg.obj) == 'R')
    id.A <- which(unlist(sg.obj) == 'A')
    
    # Extract
    if(length(id.R) > 0){
      r.list <- ss.list[id.R]
    }
    if(length(id.A) > 0){
      a.list <- ss.list[id.A]
      avails <- unlist(lapply(a.list, nchar))
      avail.rate <- sum(avails) / length(id.data)
      
    } else {
      
      avail.rate <- 0
    
    }
    
    if(length(id.B) > 0){
      b.list <- ss.list[id.B] 
    
      # Count longest and blocked times
      blocks <- unlist(lapply(b.list, nchar))
      
      block.rate <- sum(blocks) / length(id.data)
      longest.block <- max(blocks)
      med.block <- median(blocks)
      nbr.block <- length(id.B)
      
    } else {
      
      block.rate <- 0
      longest.block <- 0
      med.block <- 0
      nbr.block <- 0
      
    }
    
    total.days <- length(id.data)  
    
  } else {
    
    block.rate <- NA
    longest.block <- NA
    med.block <- NA
    nbr.block <- NA
    total.days <- NA
    
  }  
      
  ## Return Values
  
  return(data.frame(id=id,
                    longest.block=longest.block,
                    nbr.block=nbr.block,
                    med.block=med.block,
                    block.rate=block.rate,
                    avail.rate=avail.rate,
                    total.days=total.days))
}


  
  q <- lapply(as.list(abb.revs$property.id), calcBookStr, book.data=daily.data)
  
  ww <- rbind.fill(q)
  
  ww$nbrperdays <- ww$nbr.block / (ww$total.days/366)
  
  ww$zbr <- (ww$block.rate-mean(ww$block.rate))/sd(ww$block.rate)
  ww$znb <- (ww$nbrperdays-mean(ww$nbrperdays))/sd(ww$nbrperdays)
  
  www <- ww[ww$total.days > 180, ]
  rr <- kmeans(www[,c('znb', 'zbr')],3)
  www$type <- rr$cluster
  
  

  