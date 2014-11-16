myFunction <- function(data) 
{ 
  attribute_names <- names(mydata)
  cor_mat = matrix(nrow=1,ncol=3)
  #c("at1","at2","cor")
  for(i in 1:length(attribute_names)) {
    x <- attribute_names[i]
    column <- mydata[[x]]
    if (is.numeric(column)) {
      png(filename = paste("Histograms/",x,".png",sep =""))
      hist(column, main = x)
      dev.off()
      
      for(j in (i+1):length(attribute_names)) {
        y <- attribute_names[j]
        column_y <- mydata[[y]]

        if (is.numeric(column_y) & x != y & x != "points" & y!="points") {
          cor_ = abs(cor(column,column_y))
          cor_mat = rbind(cor_mat, c(x,y,cor_))         
         # print(cor_)
           
          
        }
      }
      
    }
    
  }
  or_c_mat = cor_mat[order(as.numeric(cor_mat[,3])),]
 # print(ordered_cor_mat)
  print(or_c_mat[1,])
 N = nrow(or_c_mat)
  print(or_c_mat[nrow(or_c_mat)-1,])
 #scatterplot of highest correlation
 plot(mydata[[or_c_mat[1,1]]],mydata[[or_c_mat[1,2]]],xlab = or_c_mat[1,1], ylab = or_c_mat[1,2])
 plot(mydata[[or_c_mat[N-1,1]]],mydata[[or_c_mat[N-1,2]]],xlab = or_c_mat[N-1,1], ylab = or_c_mat[N-1,2])
}

mydata <- read.csv("dmc2010_train.txt",header = TRUE, sep = ";", quote = "\"")
myFunction(mydata)
