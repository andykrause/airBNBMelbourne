################################################################################
#                                                                              #
#   Server for Reproducible Real Estate Analysis                               #
#                                                                              #  
################################################################################

# load libraries

library(shiny)
library(xtable)
library(sp)
library(plyr)
library(spdep)
library(maptools)
library(ggplot2)
library(hexbin)
library(Hmisc)

load("C:/Dropbox/Research/airBNB/data/analyzed/shinyinput.RData")
#load("shinyinput.RData")

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
    
    ## Get correct occupancy rate and preference field
    
    if(input$rev.type=='pot'){
      
      x.data$occ <- x.data$pot.occ.rate
      x.data$pref <- x.data$str.pot.pref
      
    } else {
      
      x.data$occ <- x.data$occ.rate
      x.data$pref <- x.data$str.act.pref
      
    }
    
    ## Transform in pcntl is needed
    
    if(input$transform=='Pcntl'){
      
      x.data$occ <- makeWtdQtl(x.data$occ, 
                                 return.type='rank')
      x.data$rate <- makeWtdQtl(x.data$med.rate, 
                                return.type='rank')
    } else {
      
      x.data$occ <- round(100 * x.data$occ, 0)
      x.data$rate <- x.data$med.rate
      
    }
    
    ## If doing comparison, calc percentiles separately
    
    if(input$facet.var != 'none'){
      
      x.list <- split(x.data, x.data[,input$facet.var])
      for(ix in 1:length(x.list)){
        x.list[[ix]] <- makeWtdQtl(x.list[[ix]]$occ, return.type='rank')
      }
      x.data <- rbind.fill(x.data)
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
    
    if(input$facet.var == 'bedbath'){
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
    
    hm.data <- filterData()
    
    ## If no data is returned
    if(nrow(hm.data) == 0){
      xx <- data.frame(x=c(0, 1),
                       y=c(0, 1))
      null.plot <- list(plot=ggplot(xx, aes(x=x, y=y)) + 
                          annotate("text", x = .5, y = .5, 
                                   label = "You have selected\n no properties",
                                   size=9))
      return(null.plot)
    } 
    
    ## Set bins
    
    if(input$transform == 'Pcntl'){
      
      bins <- c(5, 5)
      quantile=TRUE
      
    } else {
      
      bins <- c(5, 25)
      quantile <- FALSE
      
    }
    
  ## Build SVM data
    
    if(input$svm){
      svm.obj <- makeSVM(hm.data,
                         x.field='occ',
                         y.field='rate',
                         z.field='pref',
                         svm.type='C-svc',
                         svm.kernel='polydot',
                         poly.degree=2,
                         expand.factor=100,
                         quantile=quantile,
                         bins=bins)
    
      point.data <- hm.data
      point.data$x <- point.data$occ
      point.data$y <- point.data$rate
      
      hm.data <- svm.obj$pred
      names(hm.data) <- c('x.var', 'y.var', 'fill.var')
      
    } else {
    
      # Set up X, Y and fill variables
      hm.data$x.var <- hm.data$occ
      hm.data$y.var <- hm.data$rate
      hm.data$fill.var <- hm.data$pref 
      
      point.data <- hm.data
      point.data$x <- point.data$occ
      point.data$y <- point.data$rate
      
    }
    
  ## Make Plot  
    # Set up the basics
    
     hm.plot <- ggplot(data=hm.data,
                       aes(x=x.var, y=y.var))
    
     
    # Add the colors/counts
     hm.plot <- hm.plot +
         stat_bin2d(data=hm.data,
                    aes(alpha=..count.., fill=as.factor(fill.var)),
                    binwidth=bins) +
         guides(alpha=FALSE)

     # Add points
     hm.plot <- hm.plot + geom_point(data=point.data,
                                     aes(x=x, y=y),
                                     size=.1, color='gray50', alpha=.35,
                                     show.legend=FALSE)
    ## Tidy up plot

    if(input$transform == 'Raw'){
         hm.plot <- hm.plot +
           xlab('\n Occupancy Rate') +
           ylab('\n Nightly Rate') + 
           scale_x_continuous(breaks=seq(0, 100, by=25),
                              labels=c('0%', '25%', '50%', '75%', '100%'))
    } else {
      hm.plot <- hm.plot +
        xlab('\n Occupancy Rate (Pcntl)') +
        scale_x_continuous(breaks=seq(0, 100, by=25),
                           labels=c('0th', '25th', '50th', '75th', '100th')) +
        ylab('\n Nightly Rate (Pcntl)') +
        scale_y_continuous(breaks=seq(0, 100, by=25),
                           labels=c('0th', '25th', '50th', '75th', '100th'))
      
    }

  ## Add legend   
     
    hm.plot <- hm.plot +
      scale_fill_manual(values=c('red', 'forestgreen'),
                        name='',
                        labels=c('Long Term Preferred     ',
                                 'Short Term Preferred     ')) +
     theme(legend.position='bottom')

  return(hm.plot)  
       
  })
 
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
 
### Make a map of the data points --------------------------------------------------------  
  
  buildLocMap <- eventReactive(input$plot, {
    
    loc.data <- filterData()
    
    loc.data$pref.x <- ifelse(loc.data$pref == 1, 'Short-Term', 'Long-Term')
    loc.data$pref.x <- as.factor(loc.data$pref.x)
    
    xlims <- c(min(loc.data$longitude), max(loc.data$longitude))
    ylims <- c(min(loc.data$latitude), max(loc.data$latitude))
    
    ggplot() +
      geom_polygon(data=subs, aes(x=long, y=lat, group=group), 
                   fill='white', color='grey70') +
      geom_point(data=loc.data, aes(x=longitude, y=latitude, color=pref.x),
                 size=.4, alpha=.5) +
      scale_color_manual(values=c('red', 'forestgreen'),
                         name='Preference    ') +
      xlab('') + ylab('')+
      theme(legend.position = 'bottom',
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white"),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      guides(colour = guide_legend(override.aes = list(size=3,
                                                       alpha=1))) + 
      ggtitle('Short-Term Rental Locations')+
      theme(plot.title = element_text(hjust = 0.5)) +
      coord_cartesian(xlim=xlims, ylim=ylims) 

  })

### Create Preference Table --------------------------------------------------------------  
  
  createPrefTable <- eventReactive(input$plot, {
    
    pt.data <- filterData() 
    
    if(input$facet.var == 'none'){
      
      rate.table <- data.frame(ID='All Metro',
                               count=nrow(pt.data),
                               var=mean(pt.data$pref),
                               occ=mean(pt.data$occ))
      
    } else {
      
      # Calculate cross-tab values
      counts <- as.numeric(table(pt.data[ ,input$facet.var]))
      prefs <- tapply2DF(pt.data$pref, pt.data[ ,input$facet.var], mean)
      occs <- tapply2DF(pt.data$occ, pt.data[ ,input$facet.var], mean)
      rate.table <- data.frame('Market'=prefs[,1],
                               'Observations'=counts,
                               'STP'=prefs[,2],
                               'occs'=occs[,2])
    } 
    
    rate.table[,2] <- sprintf('%.0f', round(rate.table[,2], 0))
    rate.table[,3] <- paste0(sprintf('%.2f', round(rate.table[,3] * 100, 2)), '%')
    rate.table[,4] <- paste0(sprintf('%.2f', round(rate.table[,4], 2)), '%')
    
    names(rate.table) <- c('Market', 'Observations', '% Short-Term Preference',
                           'Mean Occupancy Rate')
    rate.table <- xtable(rate.table)
    align(rate.table) <- 'llrrr'
    
  ## Return Values
  
  return(rate.table)
    
 })
 
### Estimate Model -----------------------------------------------------------------------  
  
  estimateModel <- eventReactive(input$plot, {
    
    mod.data <- filterData()
    
    base.spec <- formula(pref ~ type + 
                           as.factor(bedbath) +
                           geo.mrkt +
                           I(max.guests/bedrooms) + 
                           min.stay +
                           I(cancel.policy=='Flexible') + 
                           I(cancel.policy=='Strict'))
                      
    if(length(table(as.character(mod.data$type))) < 2){
      base.spec <- update(base.spec, . ~ . -type)
    }
    if(length(table(as.character(mod.data$bedbath))) < 2){
      base.spec <- update(base.spec, . ~ . -as.factor(bedbath))
    }
    if(length(table(as.character(mod.data$geo.mrkt))) < 2){
      base.spec <- update(base.spec, . ~ . -geo.mkrt)
    }
    
    mod.result <- glm(base.spec, 
                      family=binomial(link='logit'),
                      data=mod.data)
    
    coefs <- summary(mod.result)$coefficient
    results.table <- data.frame("Variable"=as.character(rownames(coefs)),
                                'Control' = rep('', nrow(coefs)),
                                "Coefficient"=coefs[,1],
                                "St.Err" = coefs[,2],
                                "p-val" = coefs[,4],
                                stringsAsFactors=FALSE)
    
    # Fix Intercept
    results.table$Variable <- as.character(results.table$Variable)
    results.table$Variable[results.table$Variable == '(Intercept)'] <- 'Intercept'
    
    # Fix type
    type.control <- 'Apartment'
    if(!input$apt | !input$house) type.control <- ''
    results.table$Control[results.table$Variable == 'typeHouse'] <- type.control
    results.table$Variable[results.table$Variable == 'typeHouse'] <- 'House'

    # Fix Bedbath
    bbnames <- names(table(as.character(mod.data$bedbath)))
    bb.control <- '1 Bed & 1 Bath'
    if(bbnames[1] == '2..1' & length(bbnames) > 1) bb.control <- '2 Bed & 1 Bath'
    if(bbnames[1] == '2..2' & length(bbnames) > 1) bb.control <- '2 Bed & 2 Bath'
    if(bbnames[1] == '3..1' & length(bbnames) > 1) bb.control <- '3 Bed & 1 Bath'
    if(bbnames[1] == '3..2' & length(bbnames) > 1) bb.control <- '3 Bed & 2 Bath'
    if(bbnames[1] == '4..2' & length(bbnames) > 1) bb.control <- ''
    
    results.table$Control[grep('bedbath', results.table$Variable)] <- bb.control
    results.table$Variable[results.table$Variable == 'as.factor(bedbath)2..1'] <- 
      '2 Bed & 1 Bath'
    results.table$Variable[results.table$Variable == 'as.factor(bedbath)2..2'] <- 
      '2 Bed & 2 Bath'
    results.table$Variable[results.table$Variable == 'as.factor(bedbath)3..1'] <- 
      '3 Bed & 1 Bath'
    results.table$Variable[results.table$Variable == 'as.factor(bedbath)3..2'] <- 
      '3 Bed & 2 Bath'
    results.table$Variable[results.table$Variable == 'as.factor(bedbath)4..2'] <- 
      '4 Bed & 2 Bath'

    # Fix geomarket
    geonames <- levels(mod.data$geo.mrkt)
    geo.control <- 'City-Core'
    if(geonames[1] == 'city' & length(geonames) > 1) geo.control <- 'City'
    if(geonames[1] == 'suburban' & length(geonames) > 1) geo.control <- 'Suburban'
    if(geonames[1] == 'rural' & length(geonames) > 1) geo.control <- 'Rural'
    if(geonames[1] == 'beach' & length(geonames) > 1) geo.control <- ''
    
    results.table$Control[grep('geo', results.table$Variable)] <- geo.control
    results.table$Variable[results.table$Variable == 'geo.mrktcity'] <- 
      'City'
    results.table$Variable[results.table$Variable == 'geo.mrktsuburban'] <- 
      'Suburban'
    results.table$Variable[results.table$Variable == 'geo.mrktrural'] <- 
      'Rural'
    results.table$Variable[results.table$Variable == 'geo.mrktbeach'] <- 
      'Beach'
    
    results.table$Variable[results.table$Variable == 'I(max.guests/bedrooms)'] <- 
      'Max Guests per Bedrooms'
    results.table$Variable[results.table$Variable == 'min.stay'] <- 
      'Minimum Stay'
    
    results.table$Control[results.table$Variable == 'I(cancel.policy == "Flexible")TRUE'] <- 
      'Moderate Cancellation'
    results.table$Control[results.table$Variable == 'I(cancel.policy == "Strict")TRUE'] <- 
      'Moderate Cancellation'
    results.table$Variable[results.table$Variable == 'I(cancel.policy == "Flexible")TRUE'] <- 
      'Flexible Cancellation'
    results.table$Variable[results.table$Variable == 'I(cancel.policy == "Strict")TRUE'] <- 
      'Strict Cancellation'
    
    
    xtable(results.table)
    
  })  
  
### Output the occupancy plot ------------------------------------------------------------  
  
  output$occplot <- renderPlot({
    
    buildOccPlot()
    
  }, height = 500, width = 500 )
  
### Output the revenue density plot ------------------------------------------------------------  
  
  output$rdplot <- renderPlot({
    
    buildRevDensPlot()
    
  }, height = 500, width = 500 )
  
### Output the revenue density plot ------------------------------------------------------------  
  
  output$hmplot <- renderPlot({
    
    buildHeatMap()
    
  }, height = 500, width = 500 )
  
### Output Location Map ------------------------------------------------------------------  
  
  output$locplot <- renderPlot({
    
    buildLocMap()
    
  }, height = 500, width = 500 )
  
### Output preference table  ------------------------------------------------------------- 
  
  output$prefTable <- renderTable({    
    createPrefTable()
  })
  
### Output regression coefficients table -------------------------------------------------  
  
  output$coefTable <- renderTable({    
    estimateModel()
  })  

}) # Close Shiny Server

makeSVM <- function(svm.data,
                    x.field,
                    y.field,
                    z.field,
                    svm.type='C-svc',
                    svm.kernel='polydot',
                    poly.degree=4,
                    expand.factor=100,
                    quantile=FALSE,
                    bins=c(1, 1)){
  
  
  # Create XY data
  xy.data <- cbind(svm.data[,x.field], svm.data[,y.field])
  
  # Hack if all 0s
  if(sum(svm.data[,z.field]) == 0) svm.data[1,z.field] <- 1
  
  # Specificy SVM
  svm.obj <- ksvm(x=xy.data,
                  y=svm.data[,z.field],
                  data=xy.data,
                  type=svm.type,
                  kernel=svm.kernel,
                  kpar=list(degree=poly.degree))
  
  # Add the fitted to the data
  svm.data$fitted <- svm.obj@fitted
  
  ## Make predictions over grid
  
  # Set up grid
  if(quantile){
    
    pred.grid <- expand.grid(seq(.5, 99.5, by=bins[1]),
                             seq(.5, 99.5, by=bins[2]))
    
  } else {
    
    x.range <- max(svm.data[ ,x.field]) - min(svm.data[ ,x.field])
    x.inc <- x.range / expand.factor
    xx.min <- min(svm.data[, x.field]) 
    xx.max <- max(svm.data[, x.field]) 
    
    y.range <- max(svm.data[ ,y.field]) - min(svm.data[ ,y.field])
    y.inc <- y.range / expand.factor
    yy.min <- min(svm.data[, y.field]) 
    yy.max <- max(svm.data[, y.field]) 
    
    pred.grid <- expand.grid(seq(.5, 99.5, by=bins[1]),
                             seq(yy.min, yy.max, by=bins[2])) 
    
  }
  
  # Make the predictions
  svm.pred <- predict(svm.obj, pred.grid)
  
  # Add predictions to the 
  pred.grid$pred <- svm.pred
  
  ## Return values
  
  return(list(orig=svm.data,
              pred=pred.grid))
  
}

makeWtdQtl <- function(data.vec, 
                       wgts=rep(1,length(data.vec)),
                       return.type='rank')
{
  
  ## Load required library  
  
  require(Hmisc)
  
  ## Set the adjustment jitter to prevent identical breaks  
  
  adj.jit <- abs(mean(data.vec) / 100000)
  
  ## Calculate the weighted quantiles 0  to 1000  
  
  wtd.qtl <- Hmisc::wtd.quantile(data.vec + runif(length(data.vec), 0, adj.jit), 
                                 weights=wgts, 
                                 probs=seq(0, 1, .01))
  
  ## Fix the ends
  
  # Minimum
  if(wtd.qtl[1] > min(data.vec)){
    wtd.qtl[1] <- min(data.vec) - adj.jit
  }
  
  # Maximum
  if(wtd.qtl[length(wtd.qtl)] < max(data.vec)){
    wtd.qtl[length(wtd.qtl)] <- max(data.vec) + adj.jit
  }
  
  ##  Convert to a vector of quantile indicators 
  
  qtl.vec <- as.numeric(as.factor(cut(data.vec, 
                                      breaks=(wtd.qtl + seq(0, 1, .01) * adj.jit))))
  
  ## Return value
  
  if(return.type == 'rank'){
    return(qtl.vec)
  } else {
    return(wtd.qtl)
  }
  
}



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
