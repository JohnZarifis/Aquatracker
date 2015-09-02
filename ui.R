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
                                                  ,fluidRow(plotOutput("densPlotEcon.FCR.Period")),
                                                   fluidRow(plotOutput("densPlotBio.FCR")),
                                                   fluidRow(plotOutput("densPlotPeriod.SFR")),
                                                   fluidRow(plotOutput("densPlotPeriod.SGR")),
                                                   fluidRow( plotOutput("densPlotGPD")),
                                                   fluidRow(plotOutput("densPlotMortality"))),
                                                            bsCollapsePanel("Envirnomental", style = "primary",
                                                   fluidRow(plotOutput("densPlotAverage.Fish.Density")),
                                                   fluidRow(plotOutput("densPlotAvg.Temperature"))
                                                                            )
                                                            ) 
                                                 ), # end tabPanel Density Plots
                                        tabPanel("Boxplots",
                                                 bsCollapse(id = "collapseBoxPlot" ,  open = "Av. Weight", 
                                                             bsCollapsePanel("Av. Weight", style = "primary"
                                                   ,fluidRow(plotOutput("boxPlotAvWeight"))
                                                                            )
                                                            ,bsCollapsePanel("KPI's", style = "primary"
                                                   ,fluidRow(plotOutput("boxPlotEcon.FCR.Period")),
                                                   fluidRow(plotOutput("boxPlotBio.FCR")),
                                                   fluidRow(plotOutput("boxPlotSFR.Period")),
                                                   fluidRow(plotOutput("boxPlotPeriod.SGR")),
                                                   fluidRow( plotOutput("boxPlotGPD")),
                                                   fluidRow(plotOutput("boxPlotMortality"))),
                                                            bsCollapsePanel("Envirnomental", style = "primary",
                                                   fluidRow(plotOutput("boxPlotAverage.Fish.Density")),
                                                   fluidRow(plotOutput("boxPlotAvg.Temperature"))

                                        ))), # end tabPanel BoxPlots
                                        tabPanel("Summary Statistics", 
                                                    h4("End Average Weight:"),
                                                    tableOutput("summary_stats_EndAvWeight"),
                                                    hr(),
                                                    h4("Econ Period FCR:"),
                                                    tableOutput("summary_stats_Econ.FCR.Period"),
                                                    hr(),
                                                    h4("Bio Period FCR:"),
                                                    tableOutput("summary_stats_Bio.FCR"),
                                                    hr(),
                                                    h4("Period SFR:"),
                                                    tableOutput("summary_stats_PeriodSFR"),
                                                    hr(),
                                                    h4("Period SGR:"),
                                                    tableOutput("summary_stats_PeriodSGR"),
                                                    hr(),
                                                    h4("GDP:"),
                                                    tableOutput("summary_stats_GPD"),
                                                    hr(),
                                                    h4("LTD Mortality:"),
                                                    tableOutput("summary_stats_Mortality"),
                                                    hr(),
                                                    h4("Average Fish Density:"),
                                                    tableOutput("summary_stats_Average.Fish.Density"),
                                                    hr(),
                                                    h4("Avg. Temperature:"),
                                                    tableOutput("summary_stats_Avg.Temp"),
                                                    hr()
                                                    
                                                    
                                                    
                                                 
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
            tabPanel("Help", id="MenuPage_3", 
                fluidRow( 
                 wellPanel(
                   h4("how to upload data")
                  ,h4("how to download template")
                  ,h4("how to make analysis")
                  ,h4("how to interpret the results")
                 )
                  ))
            ,tabPanel("About",id="MenuPage_4",
                          fluidRow(
                            wellPanel(
                              
                              img(src="Aquasmart.png", class = "img-responsive"),
                    
                              h2("The European Big Data project for the Aquaculture sector:")

                              , p(strong("AquaSmart"),a("www.aquasmartdata.eu",href = "http://www.aquasmartdata.eu") 
                              , "is the European Big Data project for the Aquaculture sector
                                  , and is very much based in the real world, addressing the actual 
                                    problems that aquaculture producers face.  Its aim is to help fish 
                                    farming businesses to transform raw data into knowledge via accurate business-driven analytical models 
                                    in a seamless and efficient process. Then subsequently, use this knowledge to dramatically improve production performance. 
                                    By offering aquaculture producers throughout Europe software tools to access, share and harness global open data, together with strong data analytics features, 
                                    it is anticipated that AquaSmart will significantly strengthen their competitiveness and enhance growth potential across Europe over the next ten years.") 
                                
                                ,code("URL:www.Aquasmartdata.com,  .org,  .eu")
                                ,br()
                                ,code("Twitter:@AquaSmartData")
                                ,br()
                                ,code("LinkedIn:AquaSmartData")
                                ,br()
                                ,code("FB Page: www.facebook.com/Aquasmartdata")
                                
                                ,p("Link to AquaSmart Horizon 2020 Project Introduction via @SlideShare now at http://bit.ly/1L468Rm.
                                
                                Interested parties can register on with web site.")
                            )
                            
                           
                            
                            )     
                  
                 
                  
                )# end tabPanel "Help
  ) # end navbarPage
) # end shinyUI                                               
                                                   

                                       
                                                   