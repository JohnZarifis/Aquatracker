### Version Aquasmart (data)

source("SidebarUi.R")

shinyUI( 
  navbarPage( theme = "bootstrap.css",
              "Aqua Tracker", 
              
              #---------------------------------------------------------- First MenuPage
              tabPanel(" Univariate Statistics ", id="MenuPage_1", 
                       fluidPage( theme = "bootstrap.css", 
                                 # titlePanel("Exploratory Data Analysis"),
                                  sidebarLayout(
                                    sidebarUni,
                              
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Histograms"
                                                    ,fluidRow(column(3, sliderInput("numbins", "Number of bins:", 
                                                                                   min = 5, max = 100, 
                                                                                   value = 50, step=1))) 
                                                     ,bsCollapse(id = "collapseHistPlot" , # open = "Av. Weight", 
                                                             bsCollapsePanel("Av. Weight", style = "primary" ,
                                                    fluidRow(plotOutput("histPlotAvWeight"))#,
                                                    # fluidRow(plotOutput("histPlotAvWeightDeviation"))
                                                    ),
                                                             bsCollapsePanel("KPI's", style = "primary" ,
                                                    fluidRow( plotOutput("histPlotPeriod.FCR")),
                                                    fluidRow( plotOutput("histPlotEcon.FCR")),
                                                    fluidRow( plotOutput("histPlotPeriod.SFR")),
                                                    fluidRow( plotOutput("histPlotPeriod.SGR")),
                                                    fluidRow( plotOutput("histPlotMortality"))),
                                                             bsCollapsePanel("Envirnomental", style = "primary" ,
                                                    fluidRow( plotOutput("histPlotPeriod.Day.Degrees")),
                                                    fluidRow( plotOutput("histPlotAvg.Temperature")),
                                                    fluidRow( plotOutput("histPlotPh")),
                                                    fluidRow( plotOutput("histPlotCAUDAL.O3"))
                                                    )
                                        )
), # end tabPanel Histograms 
                                        tabPanel("Density Plots",
                                                   fluidRow(plotOutput("densPlotAvWeight")),
                                                   fluidRow(plotOutput("densPlotAvWeightDeviation")),
                                                   fluidRow(plotOutput("densPlotPeriod.FCR")),
                                                   fluidRow(plotOutput("densPlotEcon.FCR")),
                                                   fluidRow(plotOutput("densPlotPeriod.SFR")),
                                                   fluidRow(plotOutput("densPlotPeriod.SGR")),
                                                   fluidRow(plotOutput("densPlotMortality")),
                                                   fluidRow(plotOutput("densPlotPeriod.Day.Degrees")),
                                                   fluidRow(plotOutput("densPlotAvg.Temperature")),
                                                   fluidRow( plotOutput("densPlotPh")),
                                                   fluidRow( plotOutput("densPlotCAUDAL.O3")),
                                                   fluidRow( plotOutput("densPlotWATER.RENEWAL")),
                                                   fluidRow( plotOutput("densPlotNH3")),
                                                   fluidRow( plotOutput("densPlotNO2"))
                                        ), # end tabPanel Density Plots
                                        tabPanel("Boxplots",
                                                   fluidRow(plotOutput("boxPlotAvWeight")),
                                                   fluidRow(plotOutput("boxPlotAvWeightDeviation")),  
                                                   fluidRow(plotOutput("boxPlotPeriod.FCR")),
                                                   fluidRow(plotOutput("boxPlotEcon.FCR")),
                                                   fluidRow(plotOutput("boxPlotPeriod.SFR")),
                                                   fluidRow(plotOutput("boxPlotPeriod.SGR")),
                                                   fluidRow(plotOutput("boxPlotMortality")),
                                                   fluidRow(plotOutput("boxPlotPeriod.Day.Degrees")),
                                                   fluidRow(plotOutput("boxPlotAvg.Temperature")),
                                                   fluidRow( plotOutput("boxPlotPh")),
                                                   fluidRow( plotOutput("boxPlotCAUDAL.O3")),
                                                   fluidRow( plotOutput("boxPlotWATER.RENEWAL")),
                                                   fluidRow( plotOutput("boxPlotNH3")),
                                                   fluidRow( plotOutput("boxPlotNO2"))
                                        ), # end tabPanel BoxPlots
                                        tabPanel("Summary Statistics", 
                                                    h4("End Average Weight:"),
                                                    tableOutput("summary_stats_EndAvWeight"),
                                                    hr(),
                                                    h4("Average Weight Deviation:"),
                                                    tableOutput("summary_stats_AvWeightDeviation"),
                                                    hr(),
                                                    h4("Period FCR:"),
                                                    tableOutput("summary_stats_PeriodFCR"),
                                                    hr(),
                                                    h4("LTD Econ FCR:"),
                                                    tableOutput("summary_stats_EconFCR"),
                                                    hr(),
                                                    h4("Period SFR:"),
                                                    tableOutput("summary_stats_PeriodSFR"),
                                                    hr(),
                                                    h4("Period SGR:"),
                                                    tableOutput("summary_stats_PeriodSGR"),
                                                    hr(),
                                                    h4("LTD Mortality:"),
                                                    tableOutput("summary_stats_Mortality"),
                                                    hr(),
                                                    h4("Period Thermal Age:"),
                                                    tableOutput("summary_stats_Period.Day.Degrees"),
                                                    hr(),
                                                    h4("Avg. Temperature:"),
                                                    tableOutput("summary_stats_Avg.Temp"),
                                                    hr(),
                                                    h4("Ph:"),
                                                    tableOutput("summary_stats_Ph"),
                                                    hr(),
                                                    h4("CAUDAL O3 (Nm3/H):"),
                                                    tableOutput("summary_stats_CAUDAL.O3"),
                                                    hr(),
                                                    h4("WATER RENEWAL (l./min.):"),
                                                    tableOutput("summary_stats_WATER.RENEWAL"),
                                                    hr(),
                                                    h4("NH3 (ppm.):"),
                                                    tableOutput("summary_stats_NH3"),
                                                    hr(),
                                                    h4("NO2 (ppm.):"),
                                                    tableOutput("summary_stats_NO2")
                                                 
                                        )#, # end tabPanel Summary Statistics
                                     ,tabPanel("ScatterPlots",
                                               
                                               column(3, 
                                                      selectInput('x', 'X Axis', choices=names(df), selected="To"),
                                                      selectInput('y', 'Y Axis', choices=names(df), selected="End.Av.Weight")
                                                     
                                               ),
                                               column(3,
                                                      selectInput('facet_row', 'Multiple Rows',
                                                                  c(None='.', names(df[sapply(df, is.factor)]))),
                                                      selectInput('facet_col', 'Multiple Columns',
                                                                  c(None='.', names(df[sapply(df, is.factor)])))
                                               ),
                                               column(3,
                                                      checkboxInput('xmeans', 'X-axis mean'),
                                                      checkboxInput('ymeans', 'Y-axis mean'),
                                                      checkboxInput('smooth', 'Smooth')
                                                      
                                               ),
                                               column(3, verbatimTextOutput("cor"))
                                               
                                               ,plotOutput('plotDashboard',height="800px"))
                                     ) # end tabsetPanel
                                    )# end mainPanel
                                  ) # end sidebarLayout
                       )  # end fluidPage
              ), # end tabPanel "Univariate Statistics"

# ---------------------------------------------------------- Second MenuPage
            tabPanel("Scatter Matrix Plots", id="MenuPage_2", 
                fluidPage( 
                          fluidRow( column(9, radioButtons("radioDimMulti", label = h3("Separate The Dataset By:"), 
                                                      choices = list("None",  "Batch", "Unit", "Hatchery",
                                                       "Origin.Year","Feed Type"="Actual.Feed"), selected = "None", inline = TRUE)),
                                    column(3, actionButton(inputId = 'goMultiPlot',  label = 'Refresh Multivariate plots'))
                          ), # end fluidRow
                          hr(),
                          fluidRow(
                            #uiOutput("pairsplot")
                            )
                          )
                ),# end tabPanel "Scatter Matrix Plots"
                                
              tabPanel("Scatter Plots",id="MenuPage_3",
                                         wellPanel(
                                         fluidRow(column(9, plotOutput("scatterPlot.EndAvWeight.PeriodFCR")),
                                                  column(3, verbatimTextOutput("cor.stats.EndAvWeight.PeriodFCR")) 
                                                 )
                                         ),  
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EndAvWeight.PeriodSFR")),
                                                    column(3, verbatimTextOutput("cor.stats.EndAvWeight.PeriodSFR")) 
                                           )
                                         ),  
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EndAvWeight.PeriodSGR")),
                                                    column(3, verbatimTextOutput("cor.stats.EndAvWeight.PeriodSGR")) 
                                                   )
                                         ),  
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EndAvWeight.AvgTemp")),
                                                    column(3, verbatimTextOutput("cor.stats.EndAvWeight.AvgTemp")) 
                                           )
                                         ),
                                         #..........................................................................
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.PeriodEcon.FCR.PeriodSFR")),
                                                    column(3, verbatimTextOutput("cor.stats.PeriodEcon.FCR.PeriodSFR")) 
                                           )
                                         ),  
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.PeriodEcon.FCR.PeriodSGR")),
                                                    column(3, verbatimTextOutput("cor.stats.PeriodEcon.FCR.PeriodSGR")) 
                                           )
                                         ),  
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.PeriodFCR.AvgTemp")),
                                                    column(3, verbatimTextOutput("cor.stats.PeriodFCR.AvgTemp")) 
                                           )
                                         ),  
                                         #..........................................................................
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.PeriodSFR.PeriodSGR")),
                                                    column(3, verbatimTextOutput("cor.stats.PeriodSFR.PeriodSGR")) 
                                           )
                                         ),  
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.PeriodSFR.AvgTemp")),
                                                    column(3, verbatimTextOutput("cor.stats.PeriodSFR.AvgTemp")) 
                                           )
                                         ),  
                                         #..........................................................................
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.PeriodSGR.AvgTemp")),
                                                    column(3, verbatimTextOutput("cor.stats.PeriodSGR.AvgTemp")) 
                                           )
                                         ),
                                         #..........................................................................
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EconFCR.EndAvWeight")),
                                                    column(3, verbatimTextOutput("cor.stats.EconFCR.EndAvWeight")) 
                                           )
                                         ),
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EconFCR.FCRPeriod")),
                                                    column(3, verbatimTextOutput("cor.stats.EconFCR.FCRPeriod")) 
                                           )
                                         ),
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EconFCR.SFRPeriod")),
                                                    column(3, verbatimTextOutput("cor.stats.EconFCR.SFRPeriod")) 
                                           )
                                         ),
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EconFCR.SGRPeriod")),
                                                    column(3, verbatimTextOutput("cor.stats.EconFCR.SGRPeriod")) 
                                           )
                                         ),
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EconFCR.AvgTemp")),
                                                    column(3, verbatimTextOutput("cor.stats.EconFCR.AvgTemp")) 
                                           )
                                         ),
                                         wellPanel(
                                           fluidRow(column(9, plotOutput("scatterPlot.EconFCR.Ph")),
                                                    column(3, verbatimTextOutput("cor.stats.EconFCR.Ph")) 
                                           )
                                         )
                                       ) # end tabPanel "Scatter Plots"
                              #) # end tabsetPanel
                       #   ) # end fluidRow
                   # ) # end fluidPage
          #  ),  # end tabPanel " Multivariate Statistics "  

#---------------------------------------------------------- Third MenuPage
                       
                       
                      
                   
                                 

  ) # end navbarPage
) # end shinyUI                                               
                                                   

                                       
                                                   