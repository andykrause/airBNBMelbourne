################################################################################
#                                                                              #
#   Server for Reproducible Real Estate Analysis                               #
#                                                                              #  
################################################################################

# load libraries

library(shiny)
library(xtable)
library(sp)
library(spdep)
library(maptools)
library(ggplot2)
library(hexbin)
library(Hmisc)

load("C:/Dropbox/Research/airBNB/data/analyzed/abb_working.RData")

######################################################################################
### Shiny Server ---------------------------------------------------------------------

shinyServer(function(input, output) {

## Extract the county data ---------------------------------------------------------------  
    
  getCountyData <- eventReactive(input$county, {
    if(input$county != 'none'){
      res.obj <- ww[[which(names(ww) == input$county)]]
    } else {
      res.obj <- 'none'
    }
    return(res.obj)
  })
  
## Build the data for the plots ----------------------------------------------------------  
  
  buildData <- eventReactive(input$plot, {
    
  ## Get the county data
    
    if(input$county != 'none'){
      res.obj <- getCountyData()
    }
    
  ## Fix the input variables
    
    # Combine input variables
    all.vars <- list(input$xVar, input$yVar, input$f1Var, input$col, input$shp,
                     input$abserr, input$predType, input$hrb, input$timeRange,
                     input$useMTB, input$useAVR, input$useRME, input$useSRS,
                     input$useHED, input$useBAS, input$useROB, input$useWGT)
    
    # Provide Names
    names(all.vars) <- c('xVar', 'yVar', 'f1Var', 'col', 'shp', 'abserr', 
                         'predType', 'hrb', 'timeRange', 'useMTB', 'useAVR',
                         'useRME', 'useSRS', 'useHED', 'useBAS', 'useROB', 'useWGT')
    
  ## Extract from existing objects into a plottable DF
    
    plot.data <- extractData(res.obj=res.obj,
                              vars=all.vars)
    
  ## Return data
    
    return(list(plot.data=plot.data,
                vars=all.vars))
  })
  
## Basic comparison plots --------------------------------------  
  
  output$compPlot <- renderPlot({

  ## Get data  
    
    # Run reactive function
    plot.list <- buildData()
    
    # Extract data
    plot.data <- plot.list$plot.data
    all.vars <- plot.list$vars
    
  ## Build basic plot object   
    
    if('col' %in% names(plot.data)){
      rPlot <- ggplot(plot.data, aes(x=xVar, y=yVar, color=as.factor(col), group=col))
    } else {
      rPlot <- ggplot(plot.data, aes(x=xVar, y=yVar))
    }  
  
 ## Set y limits    
    
    # Calculate limits
    # if(all.vars$xVar == 'dme' | all.vars$xVar == 'pred.time'){
      rp.table <- tapply(plot.data$yVar, as.character(plot.data$xVar),
                         mean, na.rm=T)
    # }
    # if(all.vars$xVar == 'time.dif'){
    #   ## To DO    
    # }
    
    # Place into vector
    ylim <- c(min(rp.table) - sd(rp.table), 
              max(rp.table) + sd(rp.table))
    
    # Scale the Y axis  
    rPlot <- rPlot + 
      coord_cartesian(ylim=ylim) 
    
 ## Add the geometries
    
    if(all.vars$xVar == 'dme'){
      rPlot <- rPlot + stat_summary(fun.data = "mean_cl_boot", size = 1, shape="_")
    }
    if(all.vars$xVar == 'pred.time' | all.vars$xVar == "time.diff"){
      if(all.vars$f1Var == 'none' & all.vars$col == 'none'){
        rPlot <- rPlot + stat_smooth(se=TRUE) 
      } else {
        rPlot <- rPlot + stat_smooth(se=FALSE)         
      }

    } 
    
 ## Add a facet wrap  
    
    if(all.vars$f1Var != 'none'){
      rPlot <- rPlot + facet_wrap(~f1Var)
    }
      
 ## Add a title and axis labels
    
    # Specific title and x label
    if(all.vars$xVar == 'dme'){
      x.lab <- 'Data/Model/Estimator'
      title <- paste0(x.lab, ': ', toupper(input$county) , ' County')
      if(all.vars$col == 'hrb'){
        rPlot <- rPlot + theme(legend.position='bottom', 
                               legend.title=element_blank())
      } else {
        rPlot <- rPlot + theme(legend.position='none')
      }
    } 
    if(all.vars$xVar == 'pred.time'){
      x.lab <- 'Time of Prediction'
      title <- paste0(x.lab, ': ', toupper(input$county) , ' County')
      rPlot <- rPlot + scale_x_continuous(breaks=seq(1,121, 12),
                                          labels=2006:2016) +
        theme(legend.position='bottom',
              legend.title=element_blank())
    }
    if(all.vars$xVar == "time.diff"){
      x.lab <- 'Forecast Distance\n(in months)'
      title <- paste0(x.lab, ': ', toupper(input$county) , ' County')
      rPlot <- rPlot + scale_x_continuous(breaks=seq(1,121, 12),
                                          labels=seq(1,121, 12)) +
        theme(legend.position='bottom',
              legend.title=element_blank())
    }
    
    
    # Add to plot
    rPlot <- rPlot + ggtitle(title) +
      xlab(paste0('\n', x.lab)) +
      ylab('Mean Prediction Error\n') +
      theme(axis.text.x = element_text(angle = 90, vjust=.4, hjust=.4))
  
  ## Make the plot  
      
    rPlot
    
  }, height = 800, width = 900 ) # Ends Renderplot({  
 })# Ends Shiny Server

##########################################################################################
### Helper Functions

### Merge various data fields together for building data ---------------------------------

extractData <- function(res.obj,
                       vars){
  
  ## Get the raw error data
  
    error.obj <- res.obj$error
    
  ## Remove those outside of the specified time range  
    
    error.obj <- subset(error.obj, pred.time >= vars$timeRange[1] &
                          pred.time <= vars$timeRange[2])
  
  ## Convert to absolute value of the errors  
    
    if(vars$abserr) error.obj$raw.error <- abs(error.obj$raw.error)
    
  ## Add the DME information  
    
    error.obj$dme <- as.character(res.obj$index.id$dme[match(error.obj$uiid, 
                                                res.obj$index.id$uiid)])
  
  ## Trim data based on model or estimator  
    
    # Model Blended Models
    if(!vars$useMTB){
      error.obj <- error.obj[-grep('mtb', error.obj$dme), ]
    }
    # Assessed Value Models
    if(!vars$useAVR){
      error.obj <- error.obj[-grep('ta', error.obj$dme), ]
    }
    # Relative Match Models
    if(!vars$useRME){
      error.obj <- error.obj[-grep('rs', error.obj$dme), ]
    }
    # Sale-Resale Models
    if(!vars$useSRS){
      error.obj <- error.obj[-grep('ss', error.obj$dme), ]
    }
    # Hedonic Models
    if(!vars$useHED){
      error.obj <- error.obj[-grep('th', error.obj$dme), ]
    }
    # Base estimators
    if(!vars$useBAS){
      x.cut <- which(substr(error.obj$dme, 3, 3) == 'b')
      if(length(x.cut) > 0 ) error.obj <- error.obj[-x.cut, ]
    }
    # Robust estimators
    if(!vars$useROB){
      x.cut <- which(substr(error.obj$dme, 3, 3) == 'r')
      if(length(x.cut) > 0 ) error.obj <- error.obj[-x.cut, ]
    }
    # Weighted estimators
    if(!vars$useWGT){
      x.cut <- which(substr(error.obj$dme, 3, 3) == 'w')
      if(length(x.cut) > 0 ) error.obj <- error.obj[-x.cut, ]
    }
    
  ## Trim data based on other variables
    
    # Filter based on hrb
    if(vars$hrb == FALSE){
      error.obj <- error.obj[nchar(error.obj$dme) == 3, ]
    }
    
    # Filter based on predtype Forecast
    if(vars$predType == 'forecast'){
      error.obj <- error.obj[grep('forecast', error.obj$uiid),]
    }
    # Filter based on predtype Holdout
    if(vars$predType == 'holdout'){
      error.obj <- error.obj[grep('holdout', error.obj$uiid),]
    }
    
 ## Add basic plotting variabes    
    
    # Y variable
    error.obj$yVar <- error.obj$raw.error

    # X variable
    if(vars$xVar == 'dme'){
      error.obj$xVar <- error.obj$dme
    }
    if(vars$xVar == 'pred.time'){
      error.obj$xVar <- error.obj$pred.time
    }
    if(vars$xVar == 'time.diff'){
      error.obj$xVar <- res.obj$error.id$time.dif[match(error.obj$ueid, 
                                                   res.obj$error.id$ueid)]
    }
    

    # Put basic plot data together
    plot.data <- data.frame(yVar=error.obj$yVar,
                            xVar=error.obj$xVar,
                            dme=error.obj$dme)

 ## Add additional plotting variables
      
    # Color Variable
    if(vars$col == 'none'){
      col <- "All"
    } else {
      col <- res.obj$index.id[match(error.obj$uiid, res.obj$index.id$uiid),
                              names(res.obj$index.id) == vars$col]
      if(vars$col == 'dme'){
        col <- fixDMECols(col)
      }
      if(vars$col == 'hrb'){
        col[col==1] <- 'Hierarchical Blend'
        col[col==0] <- 'No Blend'
      }
    }
    plot.data$col <- col

    # Facet 1 variable
    if(vars$f1Var == 'none'){
      f1Var <- 0
    } else {
      if(vars$f1Var == 'market'){
          f1Var <- res.obj$index.id[match(error.obj$uiid, res.obj$index.id$uiid),
                                   'market']
          f1Var[f1Var == 'county'] <- 'Countywide'
          f1Var[f1Var == 'Rand.10'] <- 'Random 10 Areas'
          f1Var[f1Var == 'ZipRegionID'] <- 'ZIP Codes'
       }
       if(vars$f1Var == 'est'){
         dest <- plot.data$dme
         est <- substr(dest, 3, 3)
         est[est=='b'] <- 'Base'
         est[est=='r'] <- 'Robust'
         est[est=='w'] <- 'Weighted'
         f1Var <- est
       }
       if(vars$f1Var == 'model'){
         dmodel <- plot.data$dme
         mod <- substr(dmodel, 1, 2)
         mod[mod == 'mt'] <- 'Model Blend'
         mod[mod == 'rs'] <- 'Relative Match'
         mod[mod == 'ss'] <- 'Repeat Sale'
         mod[mod == 'th'] <- 'Hedonic'
         mod[mod == 'ta'] <- 'Assessed Value'
         f1Var <- mod
       }
     }  
     plot.data$f1Var <- f1Var
    
 ## Fix names of X variable
     
     if(vars$xVar == 'dme'){
       xVar <- as.character(plot.data$xVar)
       xVar[xVar == 'mtb'] <- 'Model Blend'
       xVar[xVar == 'mtb.h'] <- 'Model Blend (HRB)'
       xVar[xVar == 'rsb'] <- 'Rel Match - Base'
       xVar[xVar == 'rsb.h'] <- 'Rel Match - Base (HRB)'
       xVar[xVar == 'rsr'] <- 'Rel Match - Robust'
       xVar[xVar == 'rsr.h'] <- 'Rel Match - Robust (HRB)'
       xVar[xVar == 'rsw'] <- 'Rel Match - Wgtd'
       xVar[xVar == 'rsw.h'] <- 'Rel Match - Wgtd (HRB)'
       xVar[xVar == 'ssb'] <- 'Rep Sale - Base'
       xVar[xVar == 'ssb.h'] <- 'Rep Sale - Base (HRB)'
       xVar[xVar == 'ssr'] <- 'Rep Sale - Robust'
       xVar[xVar == 'ssr.h'] <- 'Rep Sale - Robust (HRB)'
       xVar[xVar == 'ssw'] <- 'Rep Sale - Wgtd'
       xVar[xVar == 'ssw.h'] <- 'Rep Sale - Wgtd (HRB)'
       xVar[xVar == 'tab'] <- 'Assd Value - Base'
       xVar[xVar == 'tab.h'] <- 'Assd Value - Base (HRB)'
       xVar[xVar == 'tar'] <- 'Assd Value - Robust'
       xVar[xVar == 'tar.h'] <- 'Assd Value - Robust (HRB)'
       xVar[xVar == 'thb'] <- 'Hedonic - Base'
       xVar[xVar == 'thb.h'] <- 'Hedonic - Base (HRB)'
       xVar[xVar == 'thr'] <- 'Hedonic - Robust'
       xVar[xVar == 'thr.h'] <- 'Hedonic - Robust (HRB)'
       
      plot.data$xVar <- xVar 
     }

 ## Return Data

  return(plot.data)

}

### Function to fix DME Colors ---------------------------------------------------

fixDMECols <- function(col){
  
  col[grep('rs', col)] <- 'Relative Match'
  col[grep('ss', col)] <- 'Repeat Sales'
  col[grep('ta', col)] <- 'Assessed Value'
  col[grep('th', col)] <- 'Hedonic'
  col[grep('mt', col)] <- 'Model Blending'
  return(col)
}

### Convert a tapply to a df -------------------------------------------------------------

tapply2DF <- function(xData,          # Vector being tapply'd 
                      byField,        # Field to split vector by
                      xFunc,          # Function to apply
                      newName='Var',  # Name of new variable 
                      idName='ID',
                      na.rm=FALSE)    # Name of identification field 
{
  
  ## Execute tapply()
  
  xTable <- as.data.frame(tapply(xData, byField, xFunc, na.rm=na.rm))
  
  ## Add names and new fields
  
  # Give calculated field a name
  names(xTable) <- newName
  
  # Add id field and give it a name
  xTable[ ,2] <- rownames(xTable)
  names(xTable)[2] <- idName
  
  # Reorder columns
  xTable <- xTable[ ,c(2, 1)]
  
  # Remove existing field names
  rownames(xTable) <- 1:nrow(xTable)
  
  ## Return values
  
  return(xTable)
}
