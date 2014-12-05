a <- function(index, data, kmeans) {
  norm <- 1 / kmeans$size[kmeans$cluster[index]]
  
  # distance between all points/rows i,j
  data <- head(data)
  #data[1,] <- c(1, 1, 1, 1)
  #data[2,] <- c(2, 1, 1, -2)
  result = matrix(nrow=nrow(data), ncol=nrow(data))
  for (i in 1:(nrow(data)-1))
    for (j in (i+1):nrow(data)) {
      result[i,j] = sqrt(sum( (data[i,] - data[j,])^2 ))
    }
  print(data)
  print(result)
}


a(1, newiris, kc)


newiris <- iris
newiris$Species <- NULL
kc <- kmeans(newiris, 3)