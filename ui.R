### Version Aquasmart (data)

source("SidebarUi.R")

shinyUI( 
  navbarPage( theme = "bootstrap.css",
              "Aqua Tracker", 
              
              #---------------------------------------------------------- First MenuPage
              tabPanel("Statistics ", id="MenuPage_1", 
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
                                                     ,bsCollapse(id = "collapseHistPlot" ,  open = "Av. Weight", 
                                                             bsCollapsePanel("Av. Weight", style = "primary" ,
                                                    fluidRow(plotOutput("histPlotAvWeight"))#,
                                                    # fluidRow(plotOutput("histPlotAvWeightDeviation"))
                                                    ),
                                                             bsCollapsePanel("KPI's", style = "primary" ,
                                                    fluidRow( plotOutput("histPlotEcon.FCR.Period")),
                                                    fluidRow( plotOutput("histPlotBio.FCR")),
                                                    fluidRow( plotOutput("histPlotPeriod.SFR")),
                                                    fluidRow( plotOutput("histPlotPeriod.SGR")),
                                                    fluidRow( plotOutput("histPlotGPD")),
                                                    fluidRow( plotOutput("histPlotMortality"))),
                                                             bsCollapsePanel("Envirnomental", style = "primary" ,
                                                    fluidRow( plotOutput("histPlotAverage.Fish.Density")),
                                                    fluidRow( plotOutput("histPlotAvg.Temperature"))
                                                    
                                                    
                                                    )
                                        )
), # end tabPanel Histograms 
                                        tabPanel("Density Plots"
                                                 ,bsCollapse(id = "collapseDensPlot" ,  open = "Av. Weight", 
                                                             bsCollapsePanel("Av. Weight", style = "primary" 
                                                 
                                                  ,fluidRow(plotOutput("densPlotAvWeight"))
                                                  ),
                                                            bsCollapsePanel("KPI's", style = "primary" 
                                                   ,fluidRow(plotOutput("densPlotAvWeightDeviation")),
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
            tabPanel("Help", id="MenuPage_2", 
                fluidPage( 
                  tabsetPanel(
                    tabPanel("Help"
                          ,fluidRow( 
                          ), # end fluidRow
                          hr(),
                          fluidRow(
                            #uiOutput("pairsplot")
                            )
                          ) # end tabpanel help
                    ,tabPanel("Test",
                              fluidRow(
                                
                              ) )
                  )
                  )
                )# end tabPanel "Help
  ) # end navbarPage
) # end shinyUI                                               
                                                   

                                       
                                                   