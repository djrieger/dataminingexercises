myFunction <- function(data, class = NULL) 
{ 
  attribute_names <- names(data)
  if (is.null(class)) {
    B = matrix( 
      #c("col1", "col2", "cor"), 
      nrow=1, 
      ncol=3) 
    for(i in 1:length(attribute_names)) {
      x <- attribute_names[i]
      #print(x)
      column <- data[[x]]
      
      if (is.numeric(column)) {
        png(filename=paste("Histograms/", x, ".png", sep = ""))
        hist(column, main = x)
        dev.off()
        
        for(j in (i+1):length(attribute_names)) {
          y <- attribute_names[j]
          column2 <- data[[y]]
          if (!(x == y) && is.numeric(column2) && !(x == "points") && !(y == "points")) {
            B = rbind(B, c(x, y, abs(cor(column, column2))))
            #print(paste("--", y, ": ", cor(column, column2)))
          }
        }
      }
    }
    
    # sort B and plot
    B = B[order(as.numeric(B[,3])),]
    for (i in 1:3) {
      print(B[i,])
      plot(data[[B[i,1]]], data[[B[i,2]]], xlab=B[i,1], ylab=B[i,2])
    }
    for (i in 1:3) {
      row <- nrow(B) - 4 + i
      print(B[row,])
      plot(data[[B[row,1]]], data[[B[row,2]]], xlab=B[row,1], ylab=B[row,2])
    }
    
  } else {
    if (class != "customernumber") {
      
      for(i in 1:4 ){#length(attribute_names)) {
        x <- attribute_names[i]
        column <- data[[x]]
        
        histograms <- vector()
        
        i <- 0
        if (x != class && x != "customernumber" && is.numeric(column)) {
          for (uniqueVal in unique(data[[class]])) {
            #print(uniqueVal)
            part <- subset(data, data[[class]] == uniqueVal)
            
            #histograms <- c(histograms, hist(part[[x]], main = x, plot=  FALSE))
            if (i > 0)
              hist(part[[x]], main = x, add = T)
            else
              hist(part[[x]], main = x)
            i <- i + 1
          }
          
          print(paste(class, x, length(histograms)))
        }
        
        
      }
    }
  }
}

mydata <- read.csv("dmc2010_train.txt",header = TRUE, sep = ";", quote = "\"")
myFunction(mydata, "target90")
