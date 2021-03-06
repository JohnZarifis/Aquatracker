### Version 30.05.2015
#
# load packages
#
library("shiny")
library("shinyFiles")
library("shinyBS")
library("lubridate")
library("htmltools")
library("htmlwidgets")
library("RColorBrewer") 
library("readxl") 
library("graphics")
library("ggplot2")
library("lattice")
library("mgcv")
library("plotrix")
library("psych")
library("plyr")
library("dplyr")
library("GGally")
library("e1071")
library("effects")
library("car")
library("ggthemr") # devtools::install_github('ggthemr', 'cttobin')
#
#-----------------------------------------
# load dataset
#
pathname = paste(getwd(), "aquaData.xlsx", sep="/")
Dataset <- read_excel(pathname, sheet = 1 ,col_names = TRUE, na='na')

#View(Dataset)
#str(Dataset)

# ------------------------
# Create the dataset
# ------------------------
create_dataset <- function(dataset){
  
  data <- data.frame( 'Unit' = dataset$Cage
                     ,'Batch' = dataset$Batch 
                     ,"Hatchery" = dataset$Hatchery
                     ,"Origin.Year" = as.character(dataset$"Year Class")
                     ,"From" = ymd(as.Date(dataset$Start.Date, origin="1899-12-30")) 
                     ,"To" = ymd(as.Date(dataset$End.Date, origin="1899-12-30"))                    
                     ,"Month.Sampling" = month(as.Date(dataset$End.Date, origin="1899-12-30"),label = TRUE)
                     ,"Start.Av.Weight" = dataset$'Start.Av. Weight (gr)'
                     ,"End.Av.Weight" = dataset$'End.Av. Weight (gr)'
                     ,"Actual.Feed" = dataset$'Feed Type' 
                     ,"Period.Feed.Qty" = dataset$'Feed Qty (Kg)'
                     ,"Opening.Fish.No" = dataset$'Start.FishNo' 
                     ,"Closing.Fish.No" = dataset$'End.FishNo' 
                     ,"Econ.FCR.Period" = round(as.numeric(dataset$'Econ FCR'),digits=2)
                     ,"Bio.FCR.Period" =  round(as.numeric(dataset$"Bio FCR") , digits = 2)
                     ,"Mortality.No" = dataset$'Mortality (No)' 
                     ,"Mortality.Percentage" = round(as.numeric(dataset$"Mortality (%)"), digits = 2)
                     ,"SFR.Period" = round(as.numeric(dataset$'SFR') , digits = 2)
                     ,"SGR.Period" = round(as.numeric(dataset$'SGR') , digits = 2)
                     ,"GPD" = round(as.numeric(dataset$"GPD (%)"), digits = 2)
                     ,"Protein" = dataset$"Protein (%)"
                     ,"Avg.Temperature" = round(as.numeric(dataset$'Av. Temp'), digits = 2)
                     ,"Days" = dataset$Days
                     ,"Net.Growth" =round(as.numeric(dataset$"Net Growth (Kg)"), digits = 0)
                     ,"Average.Fish.Density" = round(as.numeric(dataset$"Average Fish Density"),digits = 2)
                     ,"Digestible.Protein" = dataset$"Digestible Protein (%)"
                     ,"Fat" = dataset$"Fat (%)"
                     ,"Animal.Protein" = dataset$"Animal Protein (%)"
                     ,"Digestible.Energy" = dataset$"Digestible Energy (MJ/Kg)"
                    )
                    
  

  #  For debugging  
  #  View(data)
  #  str(data)
  # print(nrow(data))
  
  return(data)
  
  #"ProductionTimeDays" = paste(01,dataset$Origin.Month, dataset$Origin.Year, sep="-" )
}
df <- create_dataset(Dataset)
#View(df) # for debugging reasons
#str(df)
#source("SidebarUi.R")

#-----------------------------------------------------------------------------------------------------
##   Function that Summarizes data.
##   Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE,w,quant) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=TRUE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col, w) {
                   c(N         = length2(xx[[col]], na.rm=na.rm),
                     mean      = mean   (xx[[col]], na.rm=na.rm),
                     sd        = sd     (xx[[col]], na.rm=na.rm),
                     w.mean    = weighted.mean(xx[[col]],xx[[w]]),
                     sum.w     = sum(xx[[w]]),
                     min       = min(xx[[col]]),
                     max       = max(xx[[col]]),
                     quant     = quantile(xx[[col]],quant)
                   )
                 },
                 measurevar,w
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval  ?? t or z for us??: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  # Todo create function for percentiles...
  return(datac)
}

######-------end of function-----------####


###--------- Set ggplot2 theme and enrich the pallete --------###

ggthemr('flat dark')

set_swatch(c(   "#ecf0f1" # original Light grayish cyan.
                , "#3498db" # original Bright blue
                , "#2ecc71" # original Strong cyan - lime green.
                , "#f1c40f" # original Vivid yellow
                , "#e74c3c" # original Bright red.
                , "#9b59b6" # original Moderate violet.
                , "#1abc9c" # original Strong cyan.
                , "#f39c12" # original Vivid orange
                , "#2a0189" # Dark violet
                , "#221abc" # Strong blue
                , "#555555" # sgi darkgray 
                , "#3d3208" # Very dark yellow [Olive tone]
                , "#8E388E" #sgi beet
                , "#8470FF" #lightslateblue
                , "#8B8989" #Dark grayish red
                , "#1c8901" #Dark lime green
                , "#D1EEEE" #lightcyan 2
                , "#4b1abc" #Strong violet
                , "#B0171F" #indian red
                , "#8bbc1a" #Strong green
                , "#7D9EC0" #sgi lightblue
                , "#CAE1FF" #lightsteelblue 1
                , "#7171C6" #sgi slateblue
                , "#9c1abc" #Strong magenta
                , "#d35400" #original Strong orange
))

# in case we need to reset the theme.
# set_swatch(c("#ecf0f1", "#3498db", "#2ecc71", "#f1c40f" ,"#e74c3c", "#9b59b6", "#1abc9c", "#f39c12", "#d35400"))
# ggthemr_reset()



#----------------------------------------------------------------------------------
# the histogram plot function
#  
histPlot <- function( ds, x, nbins, group_var ){  
  range_var = diff(range(as.numeric(ds[,x])))/nbins 
  
  if (group_var!="None")
  {
    cdf <- ddply( ds, group_var, function(df) mean(df[,x]) )
    colnames(cdf)<-c("gv", "x.means")
    
    h <- ggplot(ds, aes_string(x=x, fill=group_var)) +
      geom_histogram( aes( y=..density.. ), binwidth=range_var, alpha=.5, position="identity" ) +  
      scale_x_continuous(limits=c(min(as.numeric(ds[,x])),max(as.numeric(ds[,x])))) +
      geom_vline(data=cdf, aes_string(xintercept="x.means", colour="gv"), linetype="dashed", size=1)
    
    
  }else{
    h <- ggplot(ds, aes_string(x=x)) + geom_histogram( aes( y=..density..,fill=..count.. ), binwidth=range_var ) +
      scale_x_continuous(limits=c(min(as.numeric(ds[,x])),max(as.numeric(ds[,x])))) +
      geom_vline(aes_string(xintercept=mean(ds[,x], na.rm=T)), color="red", linetype="dashed", size=1) 
   
  }
  
}
#----------------------------------------------------------------------------------
# the density plot function
# 
densityPlot <- function( ds, x, group_var ){  
  
  if (group_var!="None")
  {
    cdf <- ddply( ds, group_var, function(df)mean(df[,x]) )
    colnames(cdf)<-c("gv", "x.means")
    
    d <- ggplot(ds, aes_string(x=x, color=group_var)) + geom_density(size=1,alpha=0.8) + 
      geom_vline(data=cdf, aes_string(xintercept="x.means", colour="gv"), linetype="dashed", size=0.5)
  }else{
    
    d <- ggplot(ds, aes_string(x=x)) + geom_density(size=1,color="blue") +
      geom_vline( aes_string(xintercept=mean(ds[,x], na.rm=T)), linetype="dashed", size=0.5 )
  }
}

#----------------------------------------------------------------------------------
# the Boxplot function
# 
boxPlots <- function( ds, x, group_var ){  
  if (group_var!="None")
  {
    d <- ggplot(ds, aes_string(x=group_var, y=x, fill=group_var)) + 
      geom_boxplot(notch=TRUE, width=0.5, outlier.size=1.5) +
      stat_summary(fun.y=mean, geom="point", shape=5, size=4)
  }else{
    d <- ggplot(ds, aes_string(x=1, y=x)) + 
      geom_boxplot(notch=TRUE, width=0.5, outlier.size=1.5) +
      stat_summary(fun.y=mean, geom="point", shape=5, size=4)
  }
  
  r <- max(ds[,x]) - min(ds[,x])
  d <- d # + scale_y_continuous(breaks=seq(min(ds[,x]), max(ds[,x]), round(r/5)))  # todo not working in some cases
  
}

#----------------------------------------------------------------------------------
# the scatter plot function
#
scatterPlot <- function(ds, x, y, colour, size, regr.method)
{
  if (colour!="None")
  {
    p <- ggplot(ds, aes_string(x=x, y=y)) 
    p <- p + aes_string(color=colour, size=size) + geom_point()
    p <- p + geom_smooth(method = regr.method, size = 1)  
    p <- p + scale_color_brewer(type="qual",palette='Set1') + scale_fill_brewer()
  }else{
    p <- ggplot(ds, aes_string(x=x, y=y)) 
    p <- p + geom_point()
    p <- p + geom_smooth(method = regr.method, size = 1)  
    p <- p + scale_color_brewer(type="qual",palette='Set1') + scale_fill_brewer() 
  }
}

#----------------------------------------------------------------------------------
# the scatter matrix plot function
# 
scatterMatrixPlot <- function(ds, dim_vars, group_by_var)
{
  if ( group_by_var != "None"){
    ds <- ds[,c(dim_vars,group_by_var)]
    p <- ggpairs(ds, columns=1:length(dim_vars), 
                 upper = list(continuous='cor'),
                 lower = list(continuous = "smooth"), color = group_by_var,params=c(size=3),
                 axisLabels='internal', title = "Matrix Scatter Plot")
  }else{
    ds <- ds[,dim_vars]
    p <- ggpairs(ds, columns=1:length(dim_vars), 
                 upper = list(continuous='cor'),
                 lower = list(continuous = "smooth"),params=c(size=3),
                 axisLabels='internal', title = "Matrix Scatter Plot")
  }
  
}  
##----------------------------------------------------------------------------------
##                 Summary Mutlivariate Statistics function
##
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
sum_stats <- function(data, measurevar, groupvars, na.rm=FALSE, conf.interval=.95, .drop=TRUE)
{
  
  if ( groupvars != "None" ){
    
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
      if (na.rm) sum(!is.na(x))
      else       length(x)
    }
    
    # This does the summary. For each group's data frame, return a vector with
    # N, min, max, mean, median, sd, CV, kurtosis, skewness, Q1, Q3, IR, se and ci
    data_stats <- ddply(data, groupvars, .drop=.drop,
                        .fun = function(xx, col) {
                          c(N      = length2(xx[[col]], na.rm=na.rm),
                            min    = min(xx[[col]], na.rm=na.rm),
                            max    = max(xx[[col]], na.rm=na.rm),
                            mean   = mean(xx[[col]], na.rm=na.rm),
                            median = median(xx[[col]], na.rm=na.rm),
                            sd     = sd(xx[[col]], na.rm=na.rm),
                            CV     = ( sd(xx[[col]], na.rm=na.rm)/mean(xx[[col]], na.rm=na.rm) )*100,
                            kurtosis = kurtosis(xx[[col]], na.rm=na.rm),
                            skewness = skewness(xx[[col]], na.rm=na.rm),
                            Q1       = quantile(xx[[col]], 1/4, na.rm=na.rm, names=FALSE),
                            Q3       = quantile(xx[[col]], 3/4, na.rm=na.rm, names=FALSE),
                            IR       = IQR(xx[[col]], na.rm=na.rm)
                          )
                        },
                        measurevar
    )
    
    data_stats$se <- data_stats$sd / sqrt(data_stats$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, data_stats$N-1)
    data_stats$ci <- data_stats$se * ciMult
    
  }else{
    
    # This does the summary for all records of the data set at the variable measurevar 
    # N, min, max, mean, median, sd, CV, kurtosis, skewness, Q1, Q3, IR, se and ci
    
    ds <- data[,measurevar]
    data_stats <- data.frame(N     = length(ds),
                             min   = min(ds, na.rm=na.rm),
                             max    = max(ds, na.rm=na.rm),
                             mean   = mean(ds, na.rm=na.rm),
                             median = median(ds, na.rm=na.rm),
                             sd     = sd(ds, na.rm=na.rm),
                             CV     = ( sd(ds, na.rm=na.rm)/mean(ds, na.rm=na.rm) )*100,
                             kurtosis = kurtosis(ds, na.rm=na.rm),
                             skewness = skewness(ds, na.rm=na.rm),
                             Q1       = quantile(ds, 1/4, na.rm=na.rm, names=FALSE),
                             Q3       = quantile(ds, 3/4, na.rm=na.rm, names=FALSE),
                             IR       = IQR(ds, na.rm=na.rm)
    )
    
    
    data_stats$se <- data_stats$sd / sqrt(data_stats$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, data_stats$N-1)
    data_stats$ci <- data_stats$se * ciMult
    
    rownames(data_stats)<-"Total"
    
  }
  
  return(data_stats)
  
}
##----------------------------------------------------------------------------------

