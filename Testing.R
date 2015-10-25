# load helpers.R file
source("helpers.R")
data <- df

  z <- "End.Av.Weight"
  x <- gsub("\"", "", "End.Av.Weight")
  y <-  gsub("\"", "", "Mortality.Percentage")
  group.var <-  gsub("\"", "", "Hatchery")
  
  
    d <- ddply(data, .(group.var), summarise, "Pearson Correlation" = cor(x = x , y = y)) 

    #d <- data.frame("Pearson Correlation" = cor(x=x, y=y))

    #d <- ddply(data,.variables = c(group.var), "Pearson Correlation" = sum(x))
  
    View(d)
    sum(x)
  
    
    rbi <- ddply(data, group.var, summarise,
                 #mean_rbi = cor(noquote(x), noquote(y))) 
                  mean(, na.rm = TRUE)
    )
    
    View(rbi)
    
    
    rbib <- ddply(baseball, .(year), summarise,
                 mean_rbi = mean(rbi, na.rm = TRUE))
    View(rbib)
    
    baseball
    
    
    