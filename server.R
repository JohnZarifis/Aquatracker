### Version Sea-8 (Sol)
# This is the server logic for a Shiny web application.


# load helpers.R file 
#source("helpers.R")  # no need UI is loaded first.

# Call function to create the dataset for analysis
#pathname = paste(getwd(), "aquaData.xlsx", sep="/")
#data <- read_excel(pathname, sheet = 1 ,col_names = TRUE, na='na')
data <- create_dataset(Dataset)
#View(data) # for debugging reasons
#str(data)
#summary(data)


#---------------------------------------------------------------------------------- shinyServer.......
#
shinyServer(function(input, output, session){
 
  #---------------------------------------------------------------------------------------------------
  #     Subset of Dataset 
  #---------------------------------------------------------------------------------------------------
  volumes <- c('Project Files' = getwd())  #getVolumes() #c('R Installation'=R.home())
  fileChoose<- shinyFileChoose(input, 'file', roots=volumes, session=session, restrictions=system.file(package='base'))
  
  passData <- reactive({
    
    if (isTRUE(fileChoose)) 
    #if (is.null(fileChoose)){data <- data}
     {
      fileSelected <- parseFilePaths(volumes, input$file)
      data <- read_excel(as.character(fileSelected$datapath), sheet = 1 ,col_names = TRUE, na='na')
      colnames( data ) <- str_replace_all(colnames( data ), c(" " = "", "-" = ".","%"=".perc"))
      i <- sapply(data, is.character)
      data[i] <- lapply(data[i], as.factor)
    }else {data <- data}
   
    
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
# output$histPlotAvWeightDeviation <- renderPlot({ 
#   # Re-run when button is clicked
#   if (input$goUniPlot == 0){
#     return() }
#   else{ 
#     isolate({    
#       graphData <- passData()
#       theGraph <- histPlot(graphData, x="Period.Feed.Qty", nbins = input$numbins, group_var=input$radioDimUni )
#       print(theGraph)
#     })
#   }
# })

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
#     Scatter Matrix Plots & Scatter Plots
#---------------------------------------------------------------------------------------------------

# output$pD3 <- renderPairsD3({
#   dim_vars = c("End.Av.Weight", "SFR.Period", "SGR.Period",  
#                "Mortality", "Avg.Temperature", "Bio.FCR","Average.Fish.Density" )
#   pairsD3(passData()[,dim_vars], group = group(),  labels = NULL)
# })
# 
# group = reactive({
#   if(input$radioDimMulti=="None"){
#     return(NULL)
#     
#   } else return(passData()[,input$radioDimMulti])
# })
# 
# 
# output$pairsplot = renderUI({
#   pairsD3Output("pD3",width = "1200px", height = "1200px")
# })


output$cor <- renderPrint({
  
      data <- passData()
#       if ( input$radioDimUni != 'None'){   
#         d <- ddply(data, input$radioDimUni, summarise, "Pearson Correlation" = cor(x=data$input$x, y=data$input$y))
#       }else{
#         d <- data.frame("Pearson Correlation" = cor(x=data$input$x, y=data$input$y))
#       }
      return( input$x ) 
     
  
})


#---------------------------------------------------------------------------------------------------
#     Multidimensional Dashboard
#---------------------------------------------------------------------------------------------------

datasetMD <- reactive({
  
  data <- passData()
  
})

output$plotDashboard <- renderPlot({
 
  dsMD <- passData()
  p <- ggplot(dsMD, aes_string(x=input$x, y=input$y)) + geom_point(size=2.5) 
  
  if (input$radioDimUni != 'None')
    p <- p + aes_string(color=input$radioDimUni)
  
  facets <- paste(input$facet_row, '~', input$facet_col)
  if (facets != '. ~ .')
    p <- p + facet_grid(facets)

  if (input$xmeans)
      p <- p + geom_line( stat = "vline", xintercept="mean")
  
  if (input$ymeans)
      p <- p + geom_line( stat = "hline", yintercept="mean")
  
 
  if (input$smooth)
      p <- p + geom_smooth()

  print(p)
  
})









  
}) # end shinyServer