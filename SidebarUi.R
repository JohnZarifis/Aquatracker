# load helpers.R file
source("helpers.R")

# Call function to create the dataset for analysis
df <- create_dataset(Dataset)
#View(df)

sidebarUni <- sidebarPanel(
  #fixed responsive img #added class img
  #img(src="Aquamanager-logo.png", class = "img-responsive"),
  
#   bsCollapse(id = "collapseSidebar" ,  #open = "Dimensions", 
#              bsCollapsePanel("Dimensions", style = "primary" ,
  #h2("Dimensions"),
  fluidRow(column(6,
                  selectInput(inputId='groupUnit', label='Cage', choices=c("All", unique(as.character(df$Unit))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupBatch', label='Batch', choices=c("All", unique(as.character(df$Batch))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupHatchery', label='Hatchery', choices=c("All", unique(as.character(df$Hatchery))), selected="All", multiple=TRUE)),
           column(6,
                  
                  selectInput(inputId='groupOriginYear', label='Origin.Year', choices=c("All", unique(as.character(df$Origin.Year))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupMonth.Sampling', label='Month.Sampling', choices=c("All", unique(as.character(df$Month.Sampling))), selected="All", multiple=TRUE),
                  selectInput(inputId='groupFood', label='Actual.Feed', choices=c("All", unique(as.character(df$Actual.Feed))), selected="All", multiple=TRUE))),
                  
  
  dateRangeInput('dateRangeFrom',
                 label = paste(' From: '),
                 start = min( ymd(df$From)-days(0) ), 
                 end = max( ymd(df$From)+days(1) ),
                 min = min( ymd(df$From)-days(0) ),
                 max = max( ymd(df$From)+days(1)),
                 separator = " to ", format = "dd/mm/yyyy",
                 startview = 'year', language = 'en', weekstart = 0
  ),
  dateRangeInput('dateRangeTo',
                 label = paste(' To: '),
                 start = min( ymd(df$To)-days(1) ), 
                 end = max( ymd(df$To)+days(1) ),
                 min = min( ymd(df$To)-days(1) ),
                 max = max( ymd(df$To)+days(1) ),
                 separator = " to ", format = "dd/mm/yyyy",
                 startview = 'year', language = 'en', weekstart = 0
  #)
  ),
  
  hr(),
  #bsCollapsePanel('Measures', style = "primary" ,
  #h2('Measures'),
  fluidRow(column(6,
                  sliderInput("rangeStAvWeight", "Start.Av.Weight:", min = min(as.double(df$Start.Av.Weight)), 
                              max = max(as.double(df$Start.Av.Weight)), 
                              value = c(min(as.double(df$Start.Av.Weight)), max(as.double(df$Start.Av.Weight))),
                              step=1.0, round=TRUE, sep="."),
                  sliderInput("rangeBioFCR", "Bio FCR:", 
                              min = min(as.double(df$'Bio.FCR')), 
                              max = max(as.double(df$'Bio.FCR')), 
                              value = c(min(as.double(df$'Bio.FCR')), 
                                        max(as.double(df$'Bio.FCR'))),
                              step=0.1, round=-2, sep="."),
                  sliderInput("rangePeriod.FCR", "Econ.FCR.Period:", min = min(as.double(df$Econ.FCR.Period)), 
                              max = max(as.double(df$Econ.FCR.Period)), 
                              value = c(min(as.double(df$Econ.FCR.Period)), max(as.double(df$Econ.FCR.Period))), 
                              step=0.1, round=-2, sep="."),
                  sliderInput("rangeGPD", "GPD (%):", min = min(as.double(df$'GPD')), 
                              max = max(as.double(df$'GPD')), 
                              value = c(min(as.double(df$'GPD')), max(as.double(df$'GPD'))), 
                              step=0.1, round=-2, sep="."),
                  sliderInput("rangePeriod.Feed.Qty", "Period.Feed.Qty:", 
                              min = min(as.double(df$Period.Feed.Qty), na.rm=TRUE), 
                              max = max(as.double(df$Period.Feed.Qty), na.rm=TRUE), 
                              value = c(min(as.double(df$Period.Feed.Qty)), 
                                        max(as.double(df$Period.Feed.Qty))), 
                              step=10, round=TRUE, sep=".")
  ),
  column(6,
         sliderInput("rangeAvWeight", "End.Av.Weight:", min = min(as.double(df$End.Av.Weight)), 
                     max = max(as.double(df$End.Av.Weight)), 
                     value = c(min(as.double(df$End.Av.Weight)), max(as.double(df$End.Av.Weight))),
                     step=1.0, round=TRUE, sep="."),
         sliderInput("rangePeriod.SFR", "Period.SFR:", min = min(as.double(df$SFR.Period)), 
                     max = max(as.double(df$SFR.Period)), 
                     value = c(min(as.double(df$SFR.Period)), max(as.double(df$SFR.Period))), step=0.1, 
                     round=-2, sep="."),
         sliderInput("rangePeriod.SGR", "Period.SGR:", min = min(as.double(df$SGR.Period)), 
                     max = max(as.double(df$SGR.Period)), 
                     value = c(min(as.double(df$SGR.Period)), max(as.double(df$SGR.Period))), step=0.1, 
                     round=-2, sep="."),
         sliderInput("rangeAvgTemp", "Avg.Temperature:", min = min(as.double(df$Avg.Temperature)), 
                     max = max(as.double(df$Avg.Temperature)), 
                     value = c(min(as.double(df$Avg.Temperature)), max(as.double(df$Avg.Temperature))), 
                     step=0.5, round=-2, sep="."), 
                     
         sliderInput("rangeMortality", "Mortality (%):", 
                     min = min(as.double(df$Mortality), na.rm=TRUE), 
                     max = max(as.double(df$Mortality), na.rm=TRUE), 
                     value = c(min(as.double(df$Mortality)), 
                               max(as.double(df$Mortality))), 
                     step=10, round=TRUE, sep=".")
  ) # end column
  #) # end fluid row
  ), # end of colapsePanel
  

  
  hr(),
  #bsCollapsePanel("Separate By:", style = "primary" ,
  radioButtons("radioDimUni", label = h3("Separate The Dataset By:"), 
               choices = list("None", "Batch", "Hatchery","Origin.Year",  "Actual.Feed",'Month.Sampling'), selected = "None"), #)
  
  shinyFilesButton('file', 'Upload Your Data', 'Please select an Excel file', FALSE)
  ,actionButton(inputId = 'goUniPlot',  label = 'Refresh Univariate plots')
  
#)
) # end sidebarUni function