myFunction <- function(data) 
{ 
  attribute_names <- names(mydata)
  for(i in 1:length(attribute_names)) {
    x <- attribute_names[i]
    print(x)
    column <- mydata[,attribute_names[i]]
    if (is.numeric(column)) {
      hist(column)
    }
  }
}

mydata <- read.csv("dmc2010_train.txt",header = TRUE, sep = ";", quote = "\"")
myFunction(mydata)
