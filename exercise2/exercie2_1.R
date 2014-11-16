myFunction <- function(a) 
{ return(a) }

myFunction(4)





mydata <- read.csv("dmc2010_train.txt",header = TRUE, sep = ";", quote = "\"")
attribut_names <- names(mydata)
athist(mydata$attribut_names[1])at

for(i in 1:length(attribut_names)) {
  x <- attribut_names[i]
  hist(mydata$x)
}

?hist


