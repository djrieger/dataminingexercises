myFunction <- function(data) 
{ 
  attribute_names <- names(mydata)
  B = matrix( 
       #c("col1", "col2", "cor"), 
       nrow=1, 
       ncol=3) 
  for(i in 1:length(attribute_names)) {
    x <- attribute_names[i]
    #print(x)
    column <- mydata[[x]]
    
    if (is.numeric(column)) {
      png(filename=paste("Histograms/", x, ".png", sep = ""))
      hist(column, main = x)
      dev.off()
      
      for(j in (i+1):length(attribute_names)) {
        y <- attribute_names[j]
        column2 <- mydata[[y]]
        if (!(x == y) && is.numeric(column2) && !(x == "points") && !(y == "points")) {
          B = rbind(B, c(x, y, abs(cor(column, column2))))
          #print(paste("--", y, ": ", cor(column, column2)))
        }
      }
    }
  }
  B = B[order(as.numeric(B[,3])),]
  for (i in 1:3) {
    print(B[i,])
    plot(mydata[[B[i,1]]], mydata[[B[i,2]]], xlab=B[i,1], ylab=B[i,2])
    row <- nrow(B) - 4 + i
    print(B[row,])
    plot(mydata[[B[row,1]]], mydata[[B[row,2]]], xlab=B[row,1], ylab=B[row,2])
  }
}

mydata <- read.csv("dmc2010_train.txt",header = TRUE, sep = ";", quote = "\"")
myFunction(mydata)