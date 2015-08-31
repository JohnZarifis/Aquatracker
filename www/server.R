### Version Sea-8 (Sol)
# This is the server logic for a Shiny web application.


# load helpers.R file 
#source("helpers.R")  # no need UI is loaded first.

# Call function to create the dataset for analysis
#pathname = paste(getwd(), "aquaData.xlsx", sep="/")
#data <- read_excel(pathname, sheet = 1 ,col_names = TRUE, na='na')
data <- create_dataset(Dataset)
View(data) # for debugging reasons
#str(data)
#summary(data)


#---------------------------------------------------------------------------------- shinyServer.......
#
shinyServer(function(input, output, session){
 
  #---------------------------------------------------------------------------------------------------
  #     Subset of Dataset 
  #---------------------------------------------------------------------------------------------------
  passData <- reactive({
    
   
    
    if (input$groupUnit != "All"){ 
      data <- subset(data, Unit %in% c(input$groupUnit))
    }
    if (input$groupHatchery != "All"){ 
      data <- subset(data, Hatchery %in% c(input$groupHatchery))
    }
    
    if (input$groupOriginYear != "All"){ 
      data <- subset(data, Origin.Year %in% c(input$groupOriginYear)) 
    }
    if (input$groupFood != "All"){ 
      data <- subset(data, Actual.Feed %in% c(input$groupFood))
    }
    if (input$groupMonth.Sampling != "All"){ 
      data <- subset(data, Month.Sampling %in% c(input$groupMonth.Sampling))
    }
    
     data <- data[ data$End.Av.Weight >= as.numeric(input$rangeAvWeight[1]) & data$End.Av.Weight <= as.numeric(input$rangeAvWeight[2])
                   & data$Start.Av.Weight >= as.numeric(input$rangeStAvWeight[1]) & data$Start.Av.Weight <= as.numeric(input$rangeStAvWeight[2])
                   & data$Bio.FCR >= as.numeric(input$rangeBioFCR[1])
                   & data$Bio.FCR <= as.numeric(input$rangeBioFCR[2]) 
                   & data$Econ.FCR.Period >= as.numeric(input$rangePeriod.FCR[1]) & data$Econ.FCR.Period <= as.numeric(input$rangePeriod.FCR[2]) 
                   & data$GPD >= as.numeric(input$rangeGPD[1]) & data$GPD <= as.numeric(input$rangeGPD[2])  
                   & data$SGR.Period >= as.numeric(input$rangePeriod.SGR[1]) & data$SGR.Period <= as.numeric(input$rangePeriod.SGR[2]) 
                   & data$SFR.Period >= as.numeric(input$rangePeriod.SFR[1]) & data$SFR.Period <= as.numeric(input$rangePeriod.SFR[2]) 
                   & data$Mortality >= as.numeric(input$rangeMortality[1]) & data$Mortality <= as.numeric(input$rangeMortality[2])
                   & data$Avg.Temperature >= as.numeric(input$rangeAvgTemp[1]) 
                   & data$Avg.Temperature <= as.numeric(input$rangeAvgTemp[2])
                   & (data$From >= ymd(input$dateRangeFrom[1]) & data$From <= ymd(input$dateRangeFrom[2])) 
                   & (data$To >= ymd(input$dateRangeTo[1]) & data$To <= ymd(input$dateRangeTo[2]))
                   & data$SGR.Period >= as.numeric(input$rangePeriod.SGR[1])
                   & data$SGR.Period <= as.numeric(input$rangePeriod.SGR[2])
                   & data$Period.Feed.Qty >= as.numeric(input$rangePeriod.Feed.Qty[1]) 
                   & data$Period.Feed.Qty <= as.numeric(input$rangePeriod.Feed.Qty[2])
                , ]
     
                   #& (data$From >= input$dateRangeFrom[1] & data$From <= input$dateRangeFrom[2]) 
                   #& (data$To >= input$dateRangeTo[1] & data$To <= input$dateRangeTo[2])          
#                   & data$FastingsPerc >= as.numeric(input$rangeFastingsPerc[1]) 
#                   & data$FastingsPerc <= as.numeric(input$rangeFastingsPerc[2])
#                   & data$Fastings.No >= as.numeric(input$rangeFastings.No[1]) 
#                   & data$Fastings.No <= as.numeric(input$rangeFastings.No[2])
#                   & data$LTD.Day.Degrees >= as.numeric(input$rangeLTD.Day.Degrees[1]) 
#                   & data$LTD.Day.Degrees <= as.numeric(input$rangeLTD.Day.Degrees[2])
                   #& data$Period.Mortality >= as.numeric(input$rangePeriod.Mortality[1]) 
                   #& data$Period.Mortality <= as.numeric(input$rangePeriod.Mortality[2])
#                   & data$Age >= as.numeric(input$rangeAge[1]) 
#                   & data$Age <= as.numeric(input$rangeAge[2])
                   
    
    
    # For debugging 
    # class(data$ProductionTimeDays)
    # View(data)
    # str(data)
    # print(nrow(data))
    return(data)
  })  
  
  
#---------------------------------------------------------------------------------------------------
#     Histograms
#---------------------------------------------------------------------------------------------------
#
#...................................................... H1
output$histPlotAvWeight <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="End.Av.Weight", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H2
output$histPlotAvWeightDeviation <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Period.Feed.Qty", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})

#...................................................... H3
output$histPlotPeriod.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Econ.FCR.Period", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H4
output$histPlotEcon.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Bio.FCR", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H5
output$histPlotPeriod.SFR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="SFR.Period", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H6
output$histPlotPeriod.SGR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="SGR.Period", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H7
output$histPlotMortality <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Mortality", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H8
output$histPlotPeriod.Day.Degrees <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Average.Fish.Density", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... H9
output$histPlotAvg.Temperature <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Avg.Temperature", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})

#...................................................... H10
output$histPlotPh <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- histPlot(graphData, x="GPD", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})

#...................................................... H11

#...................................................... H12


#...................................................... H13


#...................................................... H14



#---------------------------------------------------------------------------------------------------
#     Density Plots
#---------------------------------------------------------------------------------------------------
#
#...................................................... D1
output$densPlotAvWeight <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- densityPlot( graphData, x="End.Av.Weight", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D1
output$densPlotAvWeightDeviation <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- densityPlot( graphData, x="Period.Feed.Qty", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D3
output$densPlotPeriod.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData() 
      theGraph <- densityPlot(graphData, x="Econ.FCR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D4
output$densPlotEcon.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData() 
      theGraph <- densityPlot(graphData, x="Bio.FCR", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D5
output$densPlotPeriod.SFR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="SFR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D6
output$densPlotPeriod.SGR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="SGR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D7
output$densPlotMortality <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="Mortality", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D8
output$densPlotPeriod.Day.Degrees <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="Average.Fish.Density", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D9
output$densPlotAvg.Temperature <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="Avg.Temperature", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D10
output$densPlotPh <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="GPD", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... D11

#...................................................... D12

#...................................................... D13

#...................................................... D14


#---------------------------------------------------------------------------------------------------
#     BoxPlots
#---------------------------------------------------------------------------------------------------
#
#...................................................... B1
output$boxPlotAvWeight <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="End.Av.Weight", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B2
output$boxPlotAvWeightDeviation <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Period.Feed.Qty", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B3
output$boxPlotPeriod.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Econ.FCR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B4
output$boxPlotEcon.FCR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Bio.FCR", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B5
output$boxPlotPeriod.SFR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="SFR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B6
output$boxPlotPeriod.SGR <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="SGR.Period", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B7
output$boxPlotMortality <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Mortality", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B8
output$boxPlotPeriod.Day.Degrees <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Average.Fish.Density", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})
#...................................................... B9
output$boxPlotAvg.Temperature <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Avg.Temperature", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})

#...................................................... B10
output$boxPlotPh <- renderPlot({ 
  # Re-run when button is clicked
  if (input$goUniPlot == 0){
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="GPD", group_var=input$radioDimUni )
      print(theGraph)
    })
  }
})

#...................................................... B11

#...................................................... B12


#...................................................... B13

#})

#...................................................... B14


#---------------------------------------------------------------------------------------------------
#     Summary Univariate Statistics
#---------------------------------------------------------------------------------------------------
output$summary_stats_EndAvWeight <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="End.Av.Weight", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
})  
output$summary_stats_AvWeightDeviation <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Period.Feed.Qty", groupvars=input$radioDimUni,
                              na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
})  
output$summary_stats_PeriodFCR <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Econ.FCR.Period", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 
output$summary_stats_EconFCR <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Bio.FCR", groupvars=input$radioDimUni,
                              na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 
output$summary_stats_PeriodSFR <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="SFR.Period", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 
output$summary_stats_PeriodSGR <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="SGR.Period", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 
output$summary_stats_Mortality <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Mortality", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
})  
output$summary_stats_Period.Day.Degrees <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Average.Fish.Density", groupvars=input$radioDimUni,
                              na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 
output$summary_stats_Avg.Temp <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Avg.Temperature", groupvars=input$radioDimUni,
                              na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 

output$summary_stats_Ph <- renderTable({
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="GPD", groupvars=input$radioDimUni,
                              na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    })
    return(data_stats)
  }
}) 




#---------------------------------------------------------------------------------------------------
#     Dislpay dataset (Data)
#---------------------------------------------------------------------------------------------------
# Dislpay dataset
output$dataset <- DT::renderDataTable({
  
  if (input$goUniPlot == 0) { 
    return() }
  else{ 
    isolate({  
      data <- passData() 
      DT::datatable(data, class='compact', rowname = TRUE, caption="Dataset for processing...",
                filter = 'top', options=list(autoWidth=TRUE) ) 
    })
  } 
})


#---------------------------------------------------------------------------------------------------
#     Scatter Matrix Plots & Scatter Plots
#---------------------------------------------------------------------------------------------------
output$scatterMatrixPlot <- renderPlot({
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      
      dim_vars = c("End.Av.Weight", "Econ.FCR.Period", "SFR.Period", "SGR.Period",  
                   "Mortality", "Avg.Temperature", 
                   "GPD", "Bio.FCR", "Period.Feed.Qty")
      group_by_var = input$radioDimMulti
      theGraph <- scatterMatrixPlot(graphData, dim_vars, group_by_var)
      print(theGraph)
   })
  }
})
 
#...................................................... S1
output$scatterPlot.EndAvWeight.PeriodFCR <- renderPlot({ 
#Re-run when button is clicked
if (input$goMultiPlot == 0){ 
  return() }
else{ 
  isolate({    
    graphData <- passData()
    p <- scatterPlot(graphData, x="End.Av.Weight", y="Econ.FCR.Period", colour=input$radioDimMulti,
                     size = "Closing.Fish.No", regr.method="loess") 
   print(p)
  })
 }
})
output$cor.stats.EndAvWeight.PeriodFCR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
          data <- passData()
          if ( input$radioDimMulti != "None"){
            d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=End.Av.Weight, y=Econ.FCR.Period))
          }else{
            d <- data.frame("Pearson Correlation" = cor(x=data$End.Av.Weight, y=data$Econ.FCR.Period))
          }
          return( d ) 
    })  
  }
})  
#.......................................................... S2
output$scatterPlot.EndAvWeight.PeriodSFR <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="End.Av.Weight", y="SFR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EndAvWeight.PeriodSFR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=End.Av.Weight, y=SFR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$End.Av.Weight, y=data$SFR.Period))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S3
output$scatterPlot.EndAvWeight.PeriodSGR <- renderPlot({ 
#Re-run when button is clicked
if (input$goMultiPlot == 0){ 
    return() }
else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="End.Av.Weight", y="SGR.Period", colour=input$radioDimMulti,
                  size = "Closing.Biomass", regr.method="loess") 
      print(p)
      })
    }
})
output$cor.stats.EndAvWeight.PeriodSGR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){
          d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=End.Av.Weight, y=SGR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$End.Av.Weight, y=data$SGR.Period))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S4
output$scatterPlot.EndAvWeight.AvgTemp <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Avg.Temperature", y="End.Av.Weight", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EndAvWeight.AvgTemp <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
          d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Avg.Temperature, y=End.Av.Weight))
      }else{
      d <- data.frame("Pearson Correlation" = cor(x=data$Avg.Temperature, y=data$End.Av.Weight))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S5
output$scatterPlot.PeriodEcon.FCR.PeriodSFR <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Econ.FCR.Period", y="SFR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodEcon.FCR.PeriodSFR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
          d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Econ.FCR.Period, y=SFR.Period))
      }else{
          d <- data.frame("Pearson Correlation" = cor(x=data$Econ.FCR.Period, y=data$SFR.Period))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S6
output$scatterPlot.PeriodEcon.FCR.PeriodSGR <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Econ.FCR.Period", y="SGR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodEcon.FCR.PeriodSGR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
          d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Econ.FCR.Period, y=SGR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$Econ.FCR.Period, y=data$SGR.Period))
      }
      return( d )
    })  
  }
})
#.......................................................... S7
output$scatterPlot.PeriodFCR.AvgTemp <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Avg.Temperature", y="Econ.FCR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodFCR.AvgTemp <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
          d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Avg.Temperature, y=Econ.FCR.Period))
      }else{
          d <- data.frame("Pearson Correlation" = cor(x=data$Avg.Temperature, y=data$Econ.FCR.Period))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S8
output$scatterPlot.PeriodSFR.PeriodSGR <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="SFR.Period", y="SGR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodSFR.PeriodSGR <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=SFR.Period, y=SGR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$SFR.Period, y=data$SGR.Period))
      }
      return( d )      
    })  
  }
})
#.......................................................... S9
output$scatterPlot.PeriodSFR.AvgTemp <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Avg.Temperature", y="SFR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodSFR.AvgTemp <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Avg.Temperature, y=SFR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$Avg.Temperature, y=data$SFR.Period))
      }
      return( d )      
    })  
  }
})
#.......................................................... S10
output$scatterPlot.PeriodSGR.AvgTemp <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Avg.Temperature", y="SGR.Period", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.PeriodSGR.AvgTemp <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Avg.Temperature, y=SGR.Period))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$Avg.Temperature, y=data$SGR.Period))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S11
output$scatterPlot.EconFCR.EndAvWeight <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="End.Av.Weight", y="Average.Fish.Density", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EconFCR.EndAvWeight <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=End.Av.Weight, y=LTD.Econ.FCR))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$End.Av.Weight, y=data$LTD.Econ.FCR))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S12
output$scatterPlot.EconFCR.FCRPeriod <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Econ.FCR.Period", y="Average.Fish.Density", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EconFCR.FCRPeriod <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Econ.FCR.Period, y=LTD.Econ.FCR))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$Econ.FCR.Period, y=data$LTD.Econ.FCR))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S13
output$scatterPlot.EconFCR.SFRPeriod <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="SFR.Period", y="Average.Fish.Density", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EconFCR.SFRPeriod <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=SFR.Period, y=LTD.Econ.FCR))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$SFR.Period, y=data$LTD.Econ.FCR))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S14
output$scatterPlot.EconFCR.SGRPeriod <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="SGR.Period", y="Average.Fish.Density", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EconFCR.SGRPeriod <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=SGR.Period, y=LTD.Econ.FCR))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$SGR.Period, y=data$LTD.Econ.FCR))
      }
      return( d ) 
    })  
  }
})
#.......................................................... S15
output$scatterPlot.EconFCR.AvgTemp <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="Avg.Temperature", y="Average.Fish.Density", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EconFCR.AvgTemp <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Avg.Temperature, y=LTD.Econ.FCR))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$Avg.Temperature, y=data$LTD.Econ.FCR))
      }
      return( d ) 
    })  
  }
})


#.......................................................... S16
output$scatterPlot.EconFCR.Ph <- renderPlot({ 
  #Re-run when button is clicked
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      graphData <- passData()
      p <- scatterPlot(graphData, x="GPD", y="Average.Fish.Density", colour=input$radioDimMulti,
                       size = "Closing.Biomass", regr.method="loess") 
      print(p)
    })
  }
})
output$cor.stats.EconFCR.Ph <- renderPrint({
  if (input$goMultiPlot == 0){ 
    return() }
  else{ 
    isolate({    
      data <- passData()
      if ( input$radioDimMulti != "None"){   
        d <- ddply(data, input$radioDimMulti, summarise, "Pearson Correlation" = cor(x=Ph, y=LTD.Econ.FCR))
      }else{
        d <- data.frame("Pearson Correlation" = cor(x=data$Ph, y=data$LTD.Econ.FCR))
      }
      return( d ) 
    })  
  }
})


#---------------------------------------------------------------------------------------------------
#     Multidimensional Dashboard
#---------------------------------------------------------------------------------------------------

datasetMD <- reactive({
  data <- passData()
#  data <- data[sample(nrow(data), input$sampleSize),]
  data <- data[  (data$From >= ymd(input$MD.dateRangeFrom[1]) & data$From <= ymd(input$MD.dateRangeFrom[2])) 
               & (data$To >= ymd(input$MD.dateRangeTo[1]) & data$To <= ymd(input$MD.dateRangeTo[2])) , ]
  
})

output$plotDashboard <- renderPlot({
 
  dsMD <- datasetMD()
  p <- ggplot(dsMD, aes_string(x=input$x, y=input$y)) + geom_point(size=2.5) 
  
  if (input$color != 'None')
    p <- p + aes_string(color=input$color)
  
  facets <- paste(input$facet_row, '~', input$facet_col)
  if (facets != '. ~ .')
    p <- p + facet_grid(facets)

  if (input$xmeans)
      p <- p + geom_line( stat = "vline", xintercept="mean")
  
  if (input$ymeans)
      p <- p + geom_line( stat = "hline", yintercept="mean")
  
  if (input$total.xmeans)
  { 
    avgx = mean(as.numeric(dsMD[,input$x]))
    p <- p + geom_vline( xintercept=avgx, color="darkred", linetype="dashed", size=1.5)
  }
  if (input$total.ymeans)
  { 
    avgy = mean(as.numeric(dsMD[,input$y]))
    p <- p + geom_hline( yintercept=avgy, color="darkred", linetype="dashed", size=1.5)
  }
  if (input$smooth)
      p <- p + geom_smooth()

  if ( input$comp.ranges)
  {   
    smtr <- stat_summary(fun.data ="mean_cl_boot", geom="crossbar", conf.int=0.95, width=0.3, B=1000, na.rm=T, reps=F) 
    p <- p + smtr
  }    
  
  if ( input$benchmarker)  
  {
    p <- p + geom_abline(intercept=0, slope=1)
  }
  print(p)
  
})

#---------------------------------------------------------------------------------------------------
#     Dislpay Pivot Table (Data)
#---------------------------------------------------------------------------------------------------
                  

# #---------------------------------------------------------------------------------------------------
# #     Dislpay HeatMap (Data)
# #---------------------------------------------------------------------------------------------------
# 




#---------------------------------------------------------------------------------------------------
#     Multidimensional Interactive Dashboard
#---------------------------------------------------------------------------------------------------








  
}) # end shinyServer