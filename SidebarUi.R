# load helpers.R file
source("helpers.R")

# Call function to create the dataset for analysis

#View(df)

sidebarUni <- sidebarPanel(
  
  fluidRow(column(6,img(src="logo.jpg", class = "img-responsive")
                  )
    
    
    ,column(6,
    radioButtons("radioDimUni", label = h3("Separate The Dataset By:"), 
                 choices = list("None", "Batch", "Hatchery","Year Class"="Origin.Year" , "Feed"="Actual.Feed",'Month.Sampling'), selected = "Hatchery") #)
    
    
           )
    )

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

,radioButtons('sep', ' Column Separator',
              c('Comma(,)'=',',
                'Semicolon(;)'=';',
                Tab='\t'),
                ';',inline = TRUE)
,radioButtons('dec','Decimal',
              c('Comma (,)'=',','period (.)' ='.'
              ),',',inline = TRUE)



) # end sidebarUni function