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

load("C:/Dropbox/Research/airBNB/data/analyzed/shinyinput.RData")
source('c:/code/research/AirBNBMelbourne/abb_Functions.R')

######################################################################################
### Shiny Server ---------------------------------------------------------------------

shinyServer(function(input, output) {

## Filter the desired data  --------------------------------------------------------------  
  
  filterData <- eventReactive(input$plot, {
    
    x.data <- str.data.ic
    
    ## Type
    
    if(!input$apt){
      x.data <- x.data[x.data$type != 'Apartment',]
    }
    if(!input$house){
      x.data <- x.data[x.data$type != 'House',]
    }
    
    ## Geo Market
    
    if(!input$core){
      x.data <- x.data[x.data$geo.mrkt != 'city-core',]
    }
    if(!input$city){
      x.data <- x.data[x.data$geo.mrkt != 'city',]
    }
    if(!input$suburban){
      x.data <- x.data[x.data$geo.mrkt != 'suburban',]
    }
    if(!input$rural){
      x.data <- x.data[x.data$geo.mrkt != 'rural',]
    }
    if(!input$beach){
      x.data <- x.data[x.data$geo.mrkt != 'beach',]
    }
    
    ## Product Type
    if(!input$bb11){
      x.data <- x.data[x.data$bedbath != '1..1',]
    }
    if(!input$bb21){
      x.data <- x.data[x.data$bedbath != '2..1',]
    }
    if(!input$bb22){
      x.data <- x.data[x.data$bedbath != '2..2',]
    }
    if(!input$bb31){
      x.data <- x.data[x.data$bedbath != '3..1',]
    }
    if(!input$bb32){
      x.data <- x.data[x.data$bedbath != '3..2',]
    }
    if(!input$bb42){
      x.data <- x.data[x.data$bedbath != '4..2',]
    }
    
    ## Host Type
    if(!input$hostps){
      x.data <- x.data[x.data$host.type != 'Profit Seeker', ]
    }
    if(!input$hostos){
      x.data <- x.data[x.data$host.type != 'Opportunistics Sharer', ]
    }
    if(!input$hostmu){
      x.data <- x.data[x.data$host.type != 'Multi-Platform User', ]
    }
    if(!input$hostun){
      x.data <- x.data[x.data$host.type != 'Unknown', ]
    }
    
    return(x.data)
        
  })
 
### Make the occupancy plot --------------------------------------------------------------  
   
  buildOccPlot <- eventReactive(input$plot, {
    
    ## Obtain data
    
    occ.data <- filterData()
    
    ## If no data is returned
    if(nrow(occ.data) == 0){
      xx <- data.frame(x=c(0, 1),
                       y=c(0, 1))
      null.plot <- list(plot=ggplot(xx, aes(x=x, y=y)) + 
                        annotate("text", x = .5, y = .5, 
                                 label = "You have selected\n no properites",
                                 size=9))
      return(null.plot)
    }  
      
    ## Get correct occupancy rate and preference field
    
    if(input$rev.type=='pot'){
      
      occ.data$occ <- occ.data$pot.occ.rate
      occ.data$pref <- occ.data$str.pot.pref
      
    } else {
      
      occ.data$occ <- occ.data$occ.rate
      occ.data$pref <- occ.data$str.act.pref
    
    }
    
    ## Tranform in pcntl is needed
    
    if(input$transform=='Pcntl'){

      occ.data$occ <- makeWtdQtl(occ.data$occ, 
                                 return.type='rank')
    } else {
      
      occ.data$occ <- round(100 * occ.data$occ, 0)
      
    }
    
    ## If facet then transform by facet variable
    
    ## Calculate the values
    
    occ.list <- list()
     
    ## Loop through   
     
    for(i.pl in 1:100){
       
     # Extract the ith data
     occ.df <- occ.data[occ.data$occ == i.pl, ]
     
     if(nrow(occ.df) > 0){
        
       if(input$facet.var == 'none'){
          occ.table <- data.frame(ID='all',
                               Var=mean(occ.df$pref, na.rm=T))
       } else {
          occ.table <- tapply2DF(occ.df$pref,
                                 occ.df[ ,input$facet.var],
                                 mean)
       } 
          
      occ.table$x.var <- i.pl
       
     } else {
       
       occ.table <- NULL
       
     }   
       
     # Add to the capture list
     occ.list[[i.pl]] <- occ.table
     
    }
    
    occ.full <- rbind.fill(occ.list)
    
    if(input$facet.var == 'geo.mrkt'){
      occ.full$ID <- factor(occ.full$ID, 
                            levels=c('city-core', 'city', 'suburban', 'rural', 'beach'))
    }
    
    if(input$facet.var == 'host.type'){
      occ.full$ID <- factor(occ.full$ID,
                            levels=c('Profit Seeker', 'Opportunistic Sharer',
                                     'Multi-Platform User', 'Unknown'))
    }
    
    if(input$facet.var == 'host.type'){
      occ.full$ID <- as.character(occ.full$ID)
      occ.full$ID[occ.full$ID == '1..1'] <- '1 Bed/1 Bath'
      occ.full$ID[occ.full$ID == '2..1'] <- '2 Bed/1 Bath'
      occ.full$ID[occ.full$ID == '2..2'] <- '2 Bed/2 Bath'
      occ.full$ID[occ.full$ID == '3..1'] <- '3 Bed/1 Bath'
      occ.full$ID[occ.full$ID == '3..2'] <- '3 Bed/2 Bath'
      occ.full$ID[occ.full$ID == '4..2'] <- '4 Bed/2 Bath'
      occ.full$ID <- as.factor(occ.full$ID)
    }
    
    
    
    occ.plot <- ggplot(occ.full,
                        aes(x=x.var, y=Var, group=ID, color=ID))
    
    occ.plot <- occ.plot + stat_smooth(se=FALSE, size=2, span=.5)
    
    occ.plot <- occ.plot +
      ylab('\n% of Properties where STR is Preferable') +
      scale_y_continuous(breaks=seq(0, 1, by=.25),
                         labels=c('0%', '25%', '50%', '75%', '100%')) +
      theme(legend.position='none')
      
    ## Add x scale
    
    if(input$transform == 'Raw'){
      occ.plot <- occ.plot +
        xlab('\nOccupancy Rate') +
        scale_x_continuous(breaks=seq(0, 100, by=25),
                           labels=c('0%', '25%', '50%', '75%', '100%'))
    } else {
      occ.plot <- occ.plot + 
           xlab('\nOccupancy Rate (Percentile)') +
           scale_x_continuous(breaks=seq(0, 100, by=25),
                              labels=c('0th', '25th', '50th', '75th', '100th')) 
    }
    
   ## Add colors
    if(input$facet.var == 'geo.mrkt'){
      
      occ.plot <- occ.plot + 
        scale_color_manual(values=abb.col[c(1, 3, 6, 5, 2)],
                           name='') +
        theme(legend.position='bottom')
      
    }
    
    if(input$facet.var == 'type'){
      
      occ.plot <- occ.plot + 
        scale_color_manual(values=c('grey50', 'black'),
                           name='') +
        theme(legend.position='bottom')
      
    }
    
    if(input$facet.var == 'host.type'){
      
      occ.plot <- occ.plot + 
        scale_color_manual(values=c(abb.col[2], abb.col[4], abb.col[7], abb.col[9]),
                           name='') +
        theme(legend.position='bottom')
      
    }
    
    if(input$facet.var == 'bedbath'){
      
      occ.plot <- occ.plot + 
        scale_color_manual(values=c('black', 'salmon', 'red', 'green', 
                                    'darkgreen', 'blue'),
                           name='') +
        theme(legend.position='bottom')
      
    }
    
    return(occ.plot)

  }) 

### Make the Heatmaps --------------------------------------------------------------------
  
  buildHeatMap <- eventReactive(input$plot, {
  
    ## Obtain data
    
    occ.data <- filterData()
    
    ## If no data is returned
    if(nrow(occ.data) == 0){
      xx <- data.frame(x=c(0, 1),
                       y=c(0, 1))
      null.plot <- list(plot=ggplot(xx, aes(x=x, y=y)) + 
                          annotate("text", x = .5, y = .5, 
                                   label = "You have selected\n no properties",
                                   size=9))
      return(null.plot)
    } 
    
    
    
      
  })
  
  unction(hm.data,
                         x.field,
                         y.field,
                         pref.field,
                         bins=NULL,
                         fill.colors=c('red', 'forestgreen'),
                         alpha.count=TRUE,
                         alpha.fill=1,
                         add.points=FALSE,
                         hexmap=FALSE,
                         point.data=NULL,
                         svm=FALSE,
                         quantile=FALSE,
                         return.svm=FALSE,
                         svm.opts=list(type='C-svc',
                                       kernel='polydot',
                                       poly.degree=2,
                                       expand.factor=100)){
    
    ## Set bins
    
    if(is.null(bins)){
      bins <- c(0, 0)
      if(x.field == 'occ' | x.field == 'occ.rate') bins[1] <- .05
      if(x.field == 'occ.qtl') bins[1] <- 5
      if(y.field == 'med.rate') bins[2] <- 25
      if(y.field == 'rate.qtl') bins[2] <- 5
    }
    
    ## Prepare the plotting data  
    
    # If SVM
    if(svm){
      
      # create svm analysis
      svm.obj <- makeSVM(hm.data,
                         x.field=x.field,
                         y.field=y.field,
                         z.field=pref.field,
                         svm.type=svm.opts$type,
                         svm.kernel=svm.opts$kernel,
                         poly.degree=svm.opts$poly.degree,
                         expand.factor=svm.opts$expand.factor,
                         quantile=quantile,
                         bins=bins)
      
      # Convert initial data to point data
      point.data <- hm.data
      point.data$x <- point.data[,x.field]
      point.data$y <- point.data[,y.field]
      
      # Add predicted values to data
      hm.data <- svm.obj$pred
      names(hm.data) <- c('x.var', 'y.var', 'fill.var')
      
      # if not SVM  
    } else {
      
      # Set up X, Y and fill variables
      hm.data$x.var <- hm.data[ ,x.field]
      hm.data$y.var <- hm.data[ ,y.field]
      hm.data$fill.var <- hm.data[ ,pref.field] 
      
    }
    
    ## Make the plot  
    
    # Set up the basics
    hm.plot <- ggplot(data=hm.data,
                      aes(x=x.var, y=y.var))
    
    
    # If adding by count
    if(alpha.count){
      
      # If Hex
      if(hexmap){
        hm.plot <- hm.plot + 
          stat_binhex(data=hm.data,
                      aes(alpha=..count.., fill=as.factor(fill.var)),
                      binwidth=bins, 
                      na.rm=T) +
          guides(alpha=FALSE)
        
        # If not hex
      } else {
        hm.plot <- hm.plot + 
          stat_bin2d(data=hm.data,
                     aes(alpha=..count.., fill=as.factor(fill.var)),
                     binwidth=bins) +
          guides(alpha=FALSE)
      }
      
      # if not adding by count  
    } else {
      
      # if hex
      if(hexmap){
        hm.plot <- hm.plot + 
          stat_binhex(data=hm.data,
                      aes(fill=as.factor(fill.var)),
                      binwidth=bins) +
          guides(alpha=FALSE)
        
        # if not hex
      } else {
        hm.plot <- hm.plot + 
          stat_bin2d(data=hm.data,
                     aes(fill=as.factor(fill.var)),
                     binwidth=bins)  +
          guides(alpha=FALSE)
      }
    }
    
    # Adding points
    
    if(add.points){
      if(is.null(point.data)){
        hm.plot <- hm.plot + geom_point(size=.1, color='gray50', alpha=.35, 
                                        show.legend=FALSE)
      } else {
        hm.plot <- hm.plot + geom_point(data=point.data,
                                        aes(x=x, y=y),
                                        size=.1, color='gray50', alpha=.35, 
                                        show.legend=FALSE)
      }
    }
    
    ## Tidy up plot
    
    if(x.field == 'occ' | x.field == 'occ.rate'){
      hm.plot <- hm.plot +
        xlab('\n Occupancy Rate') +
        scale_x_continuous(breaks=seq(0, 1, by=.25),
                           labels=c('0%', '25%', '50%', '75%', '100%'))  
    }
    if(x.field == 'occ.qtl'){
      hm.plot <- hm.plot +
        xlab('\n Occupancy Rate (Quantile)') +
        scale_x_continuous(breaks=seq(0, 100, by=25),
                           labels=c('0th', '25th', '50th', '75th', '100th'))  
    }
    if(y.field == 'nightly.rate'){
      hm.plot <- hm.plot +
        ylab('\n Nightly Rate') 
    }
    if(y.field == 'rate.qtl'){
      hm.plot <- hm.plot +
        ylab('\n Nightly Rate (Quantile)') +
        scale_y_continuous(breaks=seq(0, 100, by=25),
                           labels=c('0th', '25th', '50th', '75th', '100th'))  
    }
    
    # Add fill legend
    hm.plot <- hm.plot + 
      scale_fill_manual(values=fill.colors,
                        name='',
                        labels=c('Long Term Preferred     ',
                                 'Short Term Preferred     ')) +
      theme(legend.position='bottom')
    
    ## Return Values  
    
    # If return SVM data
    if(return.svm){
      return(list(svm=hm.data,
                  map=hm.plot))
      
      # if not returnign SVM data  
    } else {
      return(hm.plot)
    }
    
  }
  
  
  
### Make the Revenue Density Plot  
  
  buildRevDensPlot <- eventReactive(input$plot, {
    
    rd.data <- filterData()
    
    ## If no data is returned
    if(nrow(rd.data) == 0){
      xx <- data.frame(x=c(0, 1),
                       y=c(0, 1))
      null.plot <- list(plot=ggplot(xx, aes(x=x, y=y)) + 
                          annotate("text", x = .5, y = .5, 
                                   label = "You have selected\n no properties",
                                   size=9))
      return(null.plot)
    } 
    
    # Long-Term
    ltr.rev <- rd.data[,c('property.id', 'ltr.imp.revenue')]
    ltr.rev$tenure <- 'Long-Term   '
    names(ltr.rev)[2] <- 'revenue'
    
    if(input$rev.type=='pot'){
      str.rev <- rd.data[,c('property.id', 'str.pot.revenue')]
      str.rev$tenure <- 'Short-Term (Potential)   '
      names(str.rev)[2] <- 'revenue'
    } else {
      str.rev <- rd.data[,c('property.id', 'str.act.revenue')]
      str.rev$tenure <- 'Short-Term (Actual)   '
      names(str.rev)[2] <- 'revenue'
    }
    
    # Combine
    revdens.data <- rbind(str.rev, ltr.rev)
    
    # Make the plot
    rd.plot <- ggplot(revdens.data, 
                      aes(x=revenue, fill=tenure, color=tenure)) +
      geom_density(alpha=.5) +
      scale_fill_manual(values=c('red', 'green')) +
      scale_color_manual(values=c('red', 'green')) +
      xlab('\nAnnual Revenue') +
      scale_x_continuous(breaks=c(seq(0, 75000, by=25000)),
                         labels=c('$0', '$25k', '$50k', '$75k'))+
      theme(legend.position='none',
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y=element_blank()) +
      coord_cartesian(xlim=c(0, 85000)) +
      facet_wrap(~tenure)
    
  ## Return 
    
    return(rd.plot)
    
  })
 
  
  
  
### Output the occupancy plot ------------------------------------------------------------  
  
  output$occplot <- renderPlot({
    
    buildOccPlot()
    
  }, height = 500, width = 500 )
  
### Output the revenue density plot ------------------------------------------------------------  
  
  output$rdplot <- renderPlot({
    
    buildRevDensPlot()
    
  }, height = 500, width = 500 )
  
  
# --- #  
  
}) # Close Shiny Server





  #   
  # getCountyData <- eventReactive(input$county, {
  #   if(input$county != 'none'){
  #     res.obj <- ww[[which(names(ww) == input$county)]]
  #   } else {
  #     res.obj <- 'none'
  #   }
  #   return(res.obj)
  # })
  # 
## Build the data for the plots ----------------------------------------------------------  
#   
#   buildData <- eventReactive(input$plot, {
#     
#   ## Get the county data
#     
#     if(input$county != 'none'){
#       res.obj <- getCountyData()
#     }
#     
#   ## Fix the input variables
#     
#     # Combine input variables
#     all.vars <- list(input$xVar, input$yVar, input$f1Var, input$col, input$shp,
#                      input$abserr, input$predType, input$hrb, input$timeRange,
#                      input$useMTB, input$useAVR, input$useRME, input$useSRS,
#                      input$useHED, input$useBAS, input$useROB, input$useWGT)
#     
#     # Provide Names
#     names(all.vars) <- c('xVar', 'yVar', 'f1Var', 'col', 'shp', 'abserr', 
#                          'predType', 'hrb', 'timeRange', 'useMTB', 'useAVR',
#                          'useRME', 'useSRS', 'useHED', 'useBAS', 'useROB', 'useWGT')
#     
#   ## Extract from existing objects into a plottable DF
#     
#     plot.data <- extractData(res.obj=res.obj,
#                               vars=all.vars)
#     
#   ## Return data
#     
#     return(list(plot.data=plot.data,
#                 vars=all.vars))
#   })
#   
# ## Basic comparison plots --------------------------------------  
#   
#   output$compPlot <- renderPlot({
# 
#   ## Get data  
#     
#     # Run reactive function
#     plot.list <- buildData()
#     
#     # Extract data
#     plot.data <- plot.list$plot.data
#     all.vars <- plot.list$vars
#     
#   ## Build basic plot object   
#     
#     if('col' %in% names(plot.data)){
#       rPlot <- ggplot(plot.data, aes(x=xVar, y=yVar, color=as.factor(col), group=col))
#     } else {
#       rPlot <- ggplot(plot.data, aes(x=xVar, y=yVar))
#     }  
#   
#  ## Set y limits    
#     
#     # Calculate limits
#     # if(all.vars$xVar == 'dme' | all.vars$xVar == 'pred.time'){
#       rp.table <- tapply(plot.data$yVar, as.character(plot.data$xVar),
#                          mean, na.rm=T)
#     # }
#     # if(all.vars$xVar == 'time.dif'){
#     #   ## To DO    
#     # }
#     
#     # Place into vector
#     ylim <- c(min(rp.table) - sd(rp.table), 
#               max(rp.table) + sd(rp.table))
#     
#     # Scale the Y axis  
#     rPlot <- rPlot + 
#       coord_cartesian(ylim=ylim) 
#     
#  ## Add the geometries
#     
#     if(all.vars$xVar == 'dme'){
#       rPlot <- rPlot + stat_summary(fun.data = "mean_cl_boot", size = 1, shape="_")
#     }
#     if(all.vars$xVar == 'pred.time' | all.vars$xVar == "time.diff"){
#       if(all.vars$f1Var == 'none' & all.vars$col == 'none'){
#         rPlot <- rPlot + stat_smooth(se=TRUE) 
#       } else {
#         rPlot <- rPlot + stat_smooth(se=FALSE)         
#       }
# 
#     } 
#     
#  ## Add a facet wrap  
#     
#     if(all.vars$f1Var != 'none'){
#       rPlot <- rPlot + facet_wrap(~f1Var)
#     }
#       
#  ## Add a title and axis labels
#     
#     # Specific title and x label
#     if(all.vars$xVar == 'dme'){
#       x.lab <- 'Data/Model/Estimator'
#       title <- paste0(x.lab, ': ', toupper(input$county) , ' County')
#       if(all.vars$col == 'hrb'){
#         rPlot <- rPlot + theme(legend.position='bottom', 
#                                legend.title=element_blank())
#       } else {
#         rPlot <- rPlot + theme(legend.position='none')
#       }
#     } 
#     if(all.vars$xVar == 'pred.time'){
#       x.lab <- 'Time of Prediction'
#       title <- paste0(x.lab, ': ', toupper(input$county) , ' County')
#       rPlot <- rPlot + scale_x_continuous(breaks=seq(1,121, 12),
#                                           labels=2006:2016) +
#         theme(legend.position='bottom',
#               legend.title=element_blank())
#     }
#     if(all.vars$xVar == "time.diff"){
#       x.lab <- 'Forecast Distance\n(in months)'
#       title <- paste0(x.lab, ': ', toupper(input$county) , ' County')
#       rPlot <- rPlot + scale_x_continuous(breaks=seq(1,121, 12),
#                                           labels=seq(1,121, 12)) +
#         theme(legend.position='bottom',
#               legend.title=element_blank())
#     }
#     
#     
#     # Add to plot
#     rPlot <- rPlot + ggtitle(title) +
#       xlab(paste0('\n', x.lab)) +
#       ylab('Mean Prediction Error\n') +
#       theme(axis.text.x = element_text(angle = 90, vjust=.4, hjust=.4))
#   
#   ## Make the plot  
#       
#     rPlot
#     
#   }, height = 800, width = 900 ) # Ends Renderplot({  
#  })# Ends Shiny Server
# 
# ##########################################################################################
# ### Helper Functions
# 
# ### Merge various data fields together for building data ---------------------------------
# 
# extractData <- function(res.obj,
#                        vars){
#   
#   ## Get the raw error data
#   
#     error.obj <- res.obj$error
#     
#   ## Remove those outside of the specified time range  
#     
#     error.obj <- subset(error.obj, pred.time >= vars$timeRange[1] &
#                           pred.time <= vars$timeRange[2])
#   
#   ## Convert to absolute value of the errors  
#     
#     if(vars$abserr) error.obj$raw.error <- abs(error.obj$raw.error)
#     
#   ## Add the DME information  
#     
#     error.obj$dme <- as.character(res.obj$index.id$dme[match(error.obj$uiid, 
#                                                 res.obj$index.id$uiid)])
#   
#   ## Trim data based on model or estimator  
#     
#     # Model Blended Models
#     if(!vars$useMTB){
#       error.obj <- error.obj[-grep('mtb', error.obj$dme), ]
#     }
#     # Assessed Value Models
#     if(!vars$useAVR){
#       error.obj <- error.obj[-grep('ta', error.obj$dme), ]
#     }
#     # Relative Match Models
#     if(!vars$useRME){
#       error.obj <- error.obj[-grep('rs', error.obj$dme), ]
#     }
#     # Sale-Resale Models
#     if(!vars$useSRS){
#       error.obj <- error.obj[-grep('ss', error.obj$dme), ]
#     }
#     # Hedonic Models
#     if(!vars$useHED){
#       error.obj <- error.obj[-grep('th', error.obj$dme), ]
#     }
#     # Base estimators
#     if(!vars$useBAS){
#       x.cut <- which(substr(error.obj$dme, 3, 3) == 'b')
#       if(length(x.cut) > 0 ) error.obj <- error.obj[-x.cut, ]
#     }
#     # Robust estimators
#     if(!vars$useROB){
#       x.cut <- which(substr(error.obj$dme, 3, 3) == 'r')
#       if(length(x.cut) > 0 ) error.obj <- error.obj[-x.cut, ]
#     }
#     # Weighted estimators
#     if(!vars$useWGT){
#       x.cut <- which(substr(error.obj$dme, 3, 3) == 'w')
#       if(length(x.cut) > 0 ) error.obj <- error.obj[-x.cut, ]
#     }
#     
#   ## Trim data based on other variables
#     
#     # Filter based on hrb
#     if(vars$hrb == FALSE){
#       error.obj <- error.obj[nchar(error.obj$dme) == 3, ]
#     }
#     
#     # Filter based on predtype Forecast
#     if(vars$predType == 'forecast'){
#       error.obj <- error.obj[grep('forecast', error.obj$uiid),]
#     }
#     # Filter based on predtype Holdout
#     if(vars$predType == 'holdout'){
#       error.obj <- error.obj[grep('holdout', error.obj$uiid),]
#     }
#     
#  ## Add basic plotting variabes    
#     
#     # Y variable
#     error.obj$yVar <- error.obj$raw.error
# 
#     # X variable
#     if(vars$xVar == 'dme'){
#       error.obj$xVar <- error.obj$dme
#     }
#     if(vars$xVar == 'pred.time'){
#       error.obj$xVar <- error.obj$pred.time
#     }
#     if(vars$xVar == 'time.diff'){
#       error.obj$xVar <- res.obj$error.id$time.dif[match(error.obj$ueid, 
#                                                    res.obj$error.id$ueid)]
#     }
#     
# 
#     # Put basic plot data together
#     plot.data <- data.frame(yVar=error.obj$yVar,
#                             xVar=error.obj$xVar,
#                             dme=error.obj$dme)
# 
#  ## Add additional plotting variables
#       
#     # Color Variable
#     if(vars$col == 'none'){
#       col <- "All"
#     } else {
#       col <- res.obj$index.id[match(error.obj$uiid, res.obj$index.id$uiid),
#                               names(res.obj$index.id) == vars$col]
#       if(vars$col == 'dme'){
#         col <- fixDMECols(col)
#       }
#       if(vars$col == 'hrb'){
#         col[col==1] <- 'Hierarchical Blend'
#         col[col==0] <- 'No Blend'
#       }
#     }
#     plot.data$col <- col
# 
#     # Facet 1 variable
#     if(vars$f1Var == 'none'){
#       f1Var <- 0
#     } else {
#       if(vars$f1Var == 'market'){
#           f1Var <- res.obj$index.id[match(error.obj$uiid, res.obj$index.id$uiid),
#                                    'market']
#           f1Var[f1Var == 'county'] <- 'Countywide'
#           f1Var[f1Var == 'Rand.10'] <- 'Random 10 Areas'
#           f1Var[f1Var == 'ZipRegionID'] <- 'ZIP Codes'
#        }
#        if(vars$f1Var == 'est'){
#          dest <- plot.data$dme
#          est <- substr(dest, 3, 3)
#          est[est=='b'] <- 'Base'
#          est[est=='r'] <- 'Robust'
#          est[est=='w'] <- 'Weighted'
#          f1Var <- est
#        }
#        if(vars$f1Var == 'model'){
#          dmodel <- plot.data$dme
#          mod <- substr(dmodel, 1, 2)
#          mod[mod == 'mt'] <- 'Model Blend'
#          mod[mod == 'rs'] <- 'Relative Match'
#          mod[mod == 'ss'] <- 'Repeat Sale'
#          mod[mod == 'th'] <- 'Hedonic'
#          mod[mod == 'ta'] <- 'Assessed Value'
#          f1Var <- mod
#        }
#      }  
#      plot.data$f1Var <- f1Var
#     
#  ## Fix names of X variable
#      
#      if(vars$xVar == 'dme'){
#        xVar <- as.character(plot.data$xVar)
#        xVar[xVar == 'mtb'] <- 'Model Blend'
#        xVar[xVar == 'mtb.h'] <- 'Model Blend (HRB)'
#        xVar[xVar == 'rsb'] <- 'Rel Match - Base'
#        xVar[xVar == 'rsb.h'] <- 'Rel Match - Base (HRB)'
#        xVar[xVar == 'rsr'] <- 'Rel Match - Robust'
#        xVar[xVar == 'rsr.h'] <- 'Rel Match - Robust (HRB)'
#        xVar[xVar == 'rsw'] <- 'Rel Match - Wgtd'
#        xVar[xVar == 'rsw.h'] <- 'Rel Match - Wgtd (HRB)'
#        xVar[xVar == 'ssb'] <- 'Rep Sale - Base'
#        xVar[xVar == 'ssb.h'] <- 'Rep Sale - Base (HRB)'
#        xVar[xVar == 'ssr'] <- 'Rep Sale - Robust'
#        xVar[xVar == 'ssr.h'] <- 'Rep Sale - Robust (HRB)'
#        xVar[xVar == 'ssw'] <- 'Rep Sale - Wgtd'
#        xVar[xVar == 'ssw.h'] <- 'Rep Sale - Wgtd (HRB)'
#        xVar[xVar == 'tab'] <- 'Assd Value - Base'
#        xVar[xVar == 'tab.h'] <- 'Assd Value - Base (HRB)'
#        xVar[xVar == 'tar'] <- 'Assd Value - Robust'
#        xVar[xVar == 'tar.h'] <- 'Assd Value - Robust (HRB)'
#        xVar[xVar == 'thb'] <- 'Hedonic - Base'
#        xVar[xVar == 'thb.h'] <- 'Hedonic - Base (HRB)'
#        xVar[xVar == 'thr'] <- 'Hedonic - Robust'
#        xVar[xVar == 'thr.h'] <- 'Hedonic - Robust (HRB)'
#        
#       plot.data$xVar <- xVar 
#      }
# 
#  ## Return Data
# 
#   return(plot.data)
# 
# }
# 
# ### Function to fix DME Colors ---------------------------------------------------
# 
# fixDMECols <- function(col){
#   
#   col[grep('rs', col)] <- 'Relative Match'
#   col[grep('ss', col)] <- 'Repeat Sales'
#   col[grep('ta', col)] <- 'Assessed Value'
#   col[grep('th', col)] <- 'Hedonic'
#   col[grep('mt', col)] <- 'Model Blending'
#   return(col)
# }
# 
# ### Convert a tapply to a df -------------------------------------------------------------
# 
# tapply2DF <- function(xData,          # Vector being tapply'd 
#                       byField,        # Field to split vector by
#                       xFunc,          # Function to apply
#                       newName='Var',  # Name of new variable 
#                       idName='ID',
#                       na.rm=FALSE)    # Name of identification field 
# {
#   
#   ## Execute tapply()
#   
#   xTable <- as.data.frame(tapply(xData, byField, xFunc, na.rm=na.rm))
#   
#   ## Add names and new fields
#   
#   # Give calculated field a name
#   names(xTable) <- newName
#   
#   # Add id field and give it a name
#   xTable[ ,2] <- rownames(xTable)
#   names(xTable)[2] <- idName
#   
#   # Reorder columns
#   xTable <- xTable[ ,c(2, 1)]
#   
#   # Remove existing field names
#   rownames(xTable) <- 1:nrow(xTable)
#   
#   ## Return values
#   
#   return(xTable)
# }
