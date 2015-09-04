# load helpers.R file
source("helpers.R")

# Call function to create the dataset for analysis

#View(df)

sidebarUni <- sidebarPanel(
  #fixed responsive img #added class img
  img(src="logoAq.jpg", class = "img-responsive"),
  
#   bsCollapse(id = "collapseSidebar" ,  #open = "Dimensions", 
#              bsCollapsePanel("Dimensions", style = "primary" ,
  #h2("Dimensions"),
  fluidRow(column(6,
    radioButtons("radioDimUni", label = h3("Separate The Dataset By:"), 
                 choices = list("None", "Batch", "Hatchery","Origin.Year",  "Actual.Feed",'Month.Sampling'), selected = "Hatchery") #)
    
    #,shinyFilesButton('file', 'Upload Your Data', 'Please select an Excel file', FALSE)
  ),
  column(6,img(src="logoAq.jpg", class = "img-responsive")
          ))
#,hr()
#bsCollapsePanel('Measures', style = "primary" ,
#h2('Measures'),
,fluidRow(column(6,
                 uiOutput("rangeStAvWeight")
                 ,uiOutput("rangeBioFCR")
                 ,uiOutput("rangePeriod.FCR")
                 ,uiOutput("rangeDays")
                 ,uiOutput("rangePeriod.Feed.Qty")
                ),
column(6,
       uiOutput("rangeAvWeight")
       ,uiOutput("rangePeriod.SFR")
       ,uiOutput("rangePeriod.SGR")
       ,uiOutput("rangeAvgTemp")
       ,uiOutput("rangeMortality")
       ) # end column

) # end of fluidRow
  ,fluidRow(
    column(6,
                    uiOutput("groupUnit")
                   ,uiOutput("groupBatch")
                   ,uiOutput("groupHatchery")
           ),
           column(6,
                   uiOutput("groupOriginYear")
                  ,uiOutput("groupMonth.Sampling")
                  ,uiOutput("groupFood")
                  ),
                  
  uiOutput("dateRangeFrom")
 ,uiOutput("dateRangeTo"))
,hr()
,fileInput('file1', 'Choose CSV File To upload Your Data'
           , accept=c('text/csv', 
                    'text/comma-separated-values,text/plain', 
                     '.csv'
                     )
           )



) # end sidebarUni function