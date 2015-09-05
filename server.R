### Version Aquasmart (Sol)
# This is the server logic for a Shiny web application.

data <- df
#View(data) # for debugging reasons
#str(data)
#summary(data)


#---------------------------------------------------------------------------------- shinyServer.......
#
shinyServer(function(input, output, session){
 
  #---------------------------------------------------------------------------------------------------
  #     Subset of Dataset 
  #---------------------------------------------------------------------------------------------------

  Filedata <- reactive({
    inFile <- input$file1
    if (is.null(inFile)){
      Filedata <- data
      return(Filedata)
      }
    Filedata<-read.csv2(inFile$datapath)
    #View(Filedata)
    names(Filedata) <- names(Dataset)
    Filedata$Start.Date <- parse_date_time(as.character(Filedata$Start.Date), c("%y%m%d", "%m%d%y","%d%m%y"), quiet = TRUE)
    Filedata$End.Date <- parse_date_time(as.character(Filedata$End.Date), c("%y%m%d", "%m%d%y","%d%m%y"), quiet = TRUE)
  
    #View(Filedata)
    #str(Filedata)
    Filedata <- create_dataset(Filedata)
    
    return(Filedata)
    
  })
  
 passData <- reactive({
   
 data <- Filedata()
 
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
                   & data$Bio.FCR.Period >= as.numeric(input$rangeBioFCR[1])
                   & data$Bio.FCR.Period <= as.numeric(input$rangeBioFCR[2]) 
                   & data$Econ.FCR.Period >= as.numeric(input$rangePeriod.FCR[1]) & data$Econ.FCR.Period <= as.numeric(input$rangePeriod.FCR[2]) 
                   & data$Days >= as.numeric(input$rangeDays[1]) & data$Days <= as.numeric(input$rangeDays[2])  
                   & data$SGR.Period >= as.numeric(input$rangePeriod.SGR[1]) & data$SGR.Period <= as.numeric(input$rangePeriod.SGR[2]) 
                   & data$SFR.Period >= as.numeric(input$rangePeriod.SFR[1]) & data$SFR.Period <= as.numeric(input$rangePeriod.SFR[2]) 
                   & data$Mortality.Percentage >= as.numeric(input$rangeMortality[1]) & data$Mortality.Percentage <= as.numeric(input$rangeMortality[2])
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
     #View(data)
    # str(data)
    # print(nrow(data))
    #return(data)
  })  
  
 
  
#---------------------------------------------------------------------------------------------------
#     Histograms
#---------------------------------------------------------------------------------------------------
 
#...................................................... H1
output$histPlotAvWeight <- renderPlot({ 
  # Re-run when button is clicked
      graphData <- passData()
      theGraph <- histPlot(graphData, x="End.Av.Weight", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
  
})


#...................................................... H3
output$histPlotEcon.FCR.Period <- renderPlot({ 
  # Re-run when button is clicked
  
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Econ.FCR.Period", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... H4
output$histPlotBio.FCR <- renderPlot({ 
  # Re-run when button is clicked
      
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Bio.FCR.Period", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... H5
output$histPlotPeriod.SFR <- renderPlot({ 
  # Re-run when button is clicked
  
      graphData <- passData()
      theGraph <- histPlot(graphData, x="SFR.Period", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... H6
output$histPlotPeriod.SGR <- renderPlot({ 
  # Re-run when button is clicked
  
      graphData <- passData()
      theGraph <- histPlot(graphData, x="SGR.Period", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... H7
output$histPlotMortality <- renderPlot({ 
  # Re-run when button is clicked
 
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Mortality.Percentage", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)
   
})
#...................................................... H8
output$histPlotAverage.Fish.Density <- renderPlot({ 
  # Re-run when button is clicked
     
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Average.Fish.Density", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... H9
output$histPlotAvg.Temperature <- renderPlot({ 
  # Re-run when button is clicked
     
      graphData <- passData()
      theGraph <- histPlot(graphData, x="Avg.Temperature", nbins = input$numbins, group_var=input$radioDimUni )
      print(theGraph)

})

#...................................................... H10
# output$histPlotGPD <- renderPlot({ 
#   # Re-run when button is clicked
#    
#       graphData <- passData()
#       theGraph <- histPlot(graphData, x="GPD", nbins = input$numbins, group_var=input$radioDimUni )
#       print(theGraph)
# 
# })


#---------------------------------------------------------------------------------------------------
#     Density Plots
#---------------------------------------------------------------------------------------------------
#
#...................................................... D1
output$densPlotAvWeight <- renderPlot({ 
  # Re-run when button is clicked
    
      graphData <- passData()
      theGraph <- densityPlot( graphData, x="End.Av.Weight", group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... D1

#...................................................... D3
output$densPlotEcon.FCR.Period <- renderPlot({ 
  # Re-run when button is clicked
    
      graphData <- passData() 
      theGraph <- densityPlot(graphData, x="Econ.FCR.Period", group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... D4
output$densPlotBio.FCR <- renderPlot({ 
  # Re-run when button is clicked
   
      graphData <- passData() 
      theGraph <- densityPlot(graphData, x="Bio.FCR.Period", group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... D5
output$densPlotPeriod.SFR <- renderPlot({ 
  # Re-run when button is clicked
   
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="SFR.Period", group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... D6
output$densPlotPeriod.SGR <- renderPlot({ 
  # Re-run when button is clicked
   
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="SGR.Period", group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... D7
output$densPlotMortality <- renderPlot({ 
  # Re-run when button is clicked
 
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="Mortality.Percentage", group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... D8
output$densPlotAverage.Fish.Density <- renderPlot({ 
  # Re-run when button is clicked
 
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="Average.Fish.Density", group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... D9
output$densPlotAvg.Temperature <- renderPlot({ 
  # Re-run when button is clicked
    
      graphData <- passData()   
      theGraph <- densityPlot(graphData, x="Avg.Temperature", group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... D10
# output$densPlotGPD <- renderPlot({ 
#   # Re-run when button is clicked
#    
#       graphData <- passData()   
#       theGraph <- densityPlot(graphData, x="GPD", group_var=input$radioDimUni )
#       print(theGraph)
# 
# })

#---------------------------------------------------------------------------------------------------
#     BoxPlots
#---------------------------------------------------------------------------------------------------
#
#...................................................... B1
output$boxPlotAvWeight <- renderPlot({ 
  # Re-run when button is clicked
 
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="End.Av.Weight", group_var=input$radioDimUni )
      print(theGraph)

})

#...................................................... B3
output$boxPlotEcon.FCR.Period <- renderPlot({ 
  # Re-run when button is clicked
    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Econ.FCR.Period", group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... B4
output$boxPlotBio.FCR <- renderPlot({ 
  # Re-run when button is clicked
   
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Bio.FCR.Period", group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... B5
output$boxPlotSFR.Period <- renderPlot({ 
  # Re-run when button is clicked
    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="SFR.Period", group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... B6
output$boxPlotPeriod.SGR <- renderPlot({ 
  # Re-run when button is clicked
    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="SGR.Period", group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... B7
output$boxPlotMortality <- renderPlot({ 
  # Re-run when button is clicked
 
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Mortality.Percentage", group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... B8
output$boxPlotAverage.Fish.Density <- renderPlot({ 
  # Re-run when button is clicked
    
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Average.Fish.Density", group_var=input$radioDimUni )
      print(theGraph)

})
#...................................................... B9
output$boxPlotAvg.Temperature <- renderPlot({ 
  # Re-run when button is clicked
     
      graphData <- passData()
      theGraph <- boxPlots( graphData, x="Avg.Temperature", group_var=input$radioDimUni )
      print(theGraph)

})

#...................................................... B10
# output$boxPlotGPD <- renderPlot({ 
#   # Re-run when button is clicked
#  
#       graphData <- passData()
#       theGraph <- boxPlots( graphData, x="GPD", group_var=input$radioDimUni )
#       print(theGraph)
#  
# })




#---------------------------------------------------------------------------------------------------
#     Summary Univariate Statistics
#---------------------------------------------------------------------------------------------------
output$summary_stats_EndAvWeight <- renderTable({

      data <- passData()
      data_stats <- sum_stats(data, measurevar="End.Av.Weight", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)

    return(data_stats)
  
})  

output$summary_stats_Econ.FCR.Period <- renderTable({
  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Econ.FCR.Period", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    
    return(data_stats)
  
}) 
output$summary_stats_Bio.FCR <- renderTable({
  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Bio.FCR.Period", groupvars=input$radioDimUni,
                              na.rm=FALSE, conf.interval=.95, .drop=TRUE)
  
    return(data_stats)
  
}) 
output$summary_stats_PeriodSFR <- renderTable({
 
      data <- passData()
      data_stats <- sum_stats(data, measurevar="SFR.Period", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
   
    return(data_stats)
 
}) 
output$summary_stats_PeriodSGR <- renderTable({
  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="SGR.Period", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
   
    return(data_stats)
  
}) 
output$summary_stats_Mortality <- renderTable({
  
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Mortality.Percentage", groupvars=input$radioDimUni,
                                 na.rm=FALSE, conf.interval=.95, .drop=TRUE)
    
    return(data_stats)
  
})  
output$summary_stats_Average.Fish.Density <- renderTable({
   
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Average.Fish.Density", groupvars=input$radioDimUni,
                              na.rm=FALSE, conf.interval=.95, .drop=TRUE)
   
    return(data_stats)
  
}) 
output$summary_stats_Avg.Temp <- renderTable({
    
      data <- passData()
      data_stats <- sum_stats(data, measurevar="Avg.Temperature", groupvars=input$radioDimUni,
                              na.rm=FALSE, conf.interval=.95, .drop=TRUE)
   
    return(data_stats)
  
}) 

# output$summary_stats_GPD <- renderTable({
#   
#       data <- passData()
#       data_stats <- sum_stats(data, measurevar="GPD", groupvars=input$radioDimUni,
#                               na.rm=FALSE, conf.interval=.95, .drop=TRUE)
#    
#     return(data_stats)
#   
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

# datasetMD <- reactive({
#   
#   data <- passData()
#   
# })

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




####----Allow user to download template
output$downloadBtn <- downloadHandler(
  
  filename = function() { 
    paste0('Template.xlsx')
  },
  content = function(file) {
  
    file.copy("Template.xlsx", file, overwrite = TRUE)
    #write.xlsx(passData()[0,] , file) 
    #write.csv(passData()[0,] , file, row.names = FALSE)
    #write.csv(passData() , file, row.names = FALSE)
  }
)
#####-----------Dynamic Sidebar----------------
output$groupUnit <- renderUI({
  
  df<- Filedata()
  selectInput(inputId='groupUnit', label='Cage', choices=c("All", unique(as.character(df$Unit))), selected="All", multiple=TRUE)
  
})
output$groupBatch <- renderUI({
  
  df<- Filedata()
  selectInput(inputId='groupBatch', label='Batch', choices=c("All", unique(as.character(df$Batch))), selected="All", multiple=TRUE)
  
})

output$groupHatchery <- renderUI({
  
  df<- Filedata()
  selectInput(inputId='groupHatchery', label='Hatchery', choices=c("All", unique(as.character(df$Hatchery))), selected="All", multiple=TRUE)
  
})



output$groupOriginYear <- renderUI({
  
  df<- Filedata()
  selectInput(inputId='groupOriginYear', label='Year Class', choices=c("All", unique(as.character(df$Origin.Year))), selected="All", multiple=TRUE)
  
})
output$groupMonth.Sampling <- renderUI({
  
  df<- Filedata()
  selectInput(inputId='groupMonth.Sampling', label='Month.Sampling', choices=c("All", unique(as.character(df$Month.Sampling))), selected="All", multiple=TRUE)
  
})

output$groupFood <- renderUI({
  
  df<- Filedata()
  selectInput(inputId='groupFood', label='Feed', choices=c("All", unique(as.character(df$Actual.Feed))), selected="All", multiple=TRUE)
  
})




output$dateRangeFrom <- renderUI({
  
  df<- Filedata()
  dateRangeInput('dateRangeFrom',
                 label = paste(' End Date: '),
                 start = min( ymd(df$From)-days(0) ), 
                 end = max( ymd(df$From)+days(1) ),
                 min = min( ymd(df$From)-days(0) ),
                 max = max( ymd(df$From)+days(1)),
                 separator = " to ", format = "dd/mm/yyyy",
                 startview = 'year', language = 'en', weekstart = 0
  )
  
})


output$dateRangeTo <- renderUI({
  
  df<- Filedata()
  dateRangeInput('dateRangeTo',
                 label = paste(' Start Date: '),
                 start = min( ymd(df$To)-days(1) ), 
                 end = max( ymd(df$To)+days(1) ),
                 min = min( ymd(df$To)-days(1) ),
                 max = max( ymd(df$To)+days(1) ),
                 separator = " to ", format = "dd/mm/yyyy",
                 startview = 'year', language = 'en', weekstart = 0
                 )
  
  
})


output$rangeStAvWeight <- renderUI({
  
  df<- Filedata()
  sliderInput("rangeStAvWeight", "Start.Av.Weight:", min = min(as.double(df$Start.Av.Weight)), 
              max = max(as.double(df$Start.Av.Weight)), 
              value = c(min(as.double(df$Start.Av.Weight)), max(as.double(df$Start.Av.Weight))),
              step=1.0, round=TRUE, sep=".")
  
  
})

output$rangeBioFCR <- renderUI({
  
  df<- Filedata()
  sliderInput("rangeBioFCR", "Bio FCR Period:", 
              min = min(as.double(df$'Bio.FCR.Period')), 
              max = max(as.double(df$'Bio.FCR.Period')), 
              value = c(min(as.double(df$'Bio.FCR.Period')), 
                        max(as.double(df$'Bio.FCR.Period'))),
              step=0.1, round=-2, sep=".")
  
  
})

output$rangePeriod.FCR <- renderUI({
  
  df<- Filedata()
  sliderInput("rangePeriod.FCR", "Econ.FCR.Period:", min = min(as.double(df$Econ.FCR.Period)), 
              max = max(as.double(df$Econ.FCR.Period)), 
              value = c(min(as.double(df$Econ.FCR.Period)), max(as.double(df$Econ.FCR.Period))), 
              step=0.1, round=-2, sep=".")
  
  
})


output$rangeDays <- renderUI({
  
  df<- Filedata()
  sliderInput("rangeDays", "Days Between Samplings:", min = min(as.double(df$'Days')), 
              max = max(as.double(df$'Days')), 
              value = c(min(as.double(df$'Days')), max(as.double(df$'Days'))), 
              step=1, round=0, sep=".")
  
  
})

output$rangePeriod.Feed.Qty <- renderUI({
  
  df<- Filedata()
  sliderInput("rangePeriod.Feed.Qty", "Period.Feed.Qty:", 
              min = min(as.double(df$Period.Feed.Qty), na.rm=TRUE), 
              max = max(as.double(df$Period.Feed.Qty), na.rm=TRUE), 
              value = c(min(as.double(df$Period.Feed.Qty)), 
                        max(as.double(df$Period.Feed.Qty))), 
              step=10, round=TRUE, sep=".")
  
  
})


output$rangeAvWeight <- renderUI({
  
  df<- Filedata()
  sliderInput("rangeAvWeight", "End.Av.Weight:", min = min(as.double(df$End.Av.Weight)), 
              max = max(as.double(df$End.Av.Weight)), 
              value = c(min(as.double(df$End.Av.Weight)), max(as.double(df$End.Av.Weight))),
              step=1.0, round=TRUE, sep=".")
  
  
})

output$rangePeriod.SFR <- renderUI({
  
  df<- Filedata()
  sliderInput("rangePeriod.SFR", "Period.SFR:", min = min(as.double(df$SFR.Period)), 
              max = max(as.double(df$SFR.Period)), 
              value = c(min(as.double(df$SFR.Period)), max(as.double(df$SFR.Period))), step=0.1, 
              round=-2, sep=".")
  
  
})

output$rangePeriod.SGR <- renderUI({
  
  df<- Filedata()
  sliderInput("rangePeriod.SGR", "Period.SGR:", min = min(as.double(df$SGR.Period)), 
              max = max(as.double(df$SGR.Period)), 
              value = c(min(as.double(df$SGR.Period)), max(as.double(df$SGR.Period))), step=0.1, 
              round=-2, sep=".")
  
  
})


output$rangeAvgTemp <- renderUI({
  
  df<- Filedata()
  sliderInput("rangeAvgTemp", "Avg.Temperature:", min = min(as.double(df$Avg.Temperature)), 
              max = max(as.double(df$Avg.Temperature)), 
              value = c(min(as.double(df$Avg.Temperature)), max(as.double(df$Avg.Temperature))), 
              step=0.1, round=-2, sep=".")
  
  
})


output$rangeMortality <- renderUI({
  
  df<- Filedata()
  sliderInput("rangeMortality", "Mortality (%):", 
              min = min(as.double(df$Mortality.Percentage), na.rm=TRUE), 
              max = max(as.double(df$Mortality.Percentage), na.rm=TRUE), 
              value = c(min(as.double(df$Mortality.Percentage)), 
                        max(as.double(df$Mortality.Percentage))), 
              step=0.1, round=-1, sep=".")
  
  
})


output$rangePeriod.SGR <- renderUI({
  
  df<- Filedata()
  sliderInput("rangePeriod.SGR", "Period.SGR:", min = min(as.double(df$SGR.Period)), 
              max = max(as.double(df$SGR.Period)), 
              value = c(min(as.double(df$SGR.Period)), max(as.double(df$SGR.Period))), step=0.1, 
              round=-2, sep=".")
  
  
})



  
}) # end shinyServer