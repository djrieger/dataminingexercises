a <- function(index, data, kmeans) {
  norm <- 1 / kmeans$size[kmeans$cluster[index]]
  
  # for debugging:
  #print(kmeans$cluster)
  #data <- head(data)
  #data[1,] <- c(1, 1, 1, 1)
  #data[2,] <- c(2, 1, 1, 1)
  
  result = matrix(nrow=nrow(data), ncol=nrow(data))
  distance <- 0
  for (i in 1:(nrow(data))) {
      if (kmeans$cluster[i] == kmeans$cluster[index]) {
        distance <- distance + sqrt(sum( (data[index,] - data[i,])^2 ))
      }
  }
  return(distance * norm)
}


a(1, newiris, kc)


newiris <- iris
newiris$Species <- NULL
kc <- kmeans(newiris, 3)