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

b <- function(index_o,data,kmeans){
  
  min_ = 9999999
  for (i in 1:kmeans$iter+1){
    o_cluster = kmeans$cluster[index_o]
    current_cluster_size = kmeans$size[i] 
    dist_sum = 0
    # print("I")
    #  print(i)
    if(i != o_cluster){
      for(j in 1:nrow(data)){
        #print
        #print(i)
        #print(kmeans$cluster[j])
        if(i == kmeans$cluster[j]){
          
          dist_sum = dist_sum + sqrt(sum((data[index_o,] - data[j,])^2))
          
        }
        
      }
      #print(dist_sum)
      #print(current_cluster_size)
      normalized_dist_sum = dist_sum /current_cluster_size
      #print(normalized_dist_sum)
      # print(normalized_dist_sum)
      if(normalized_dist_sum < min_){
        min_ = normalized_dist_sum
      }
    }
    
    
  }
  
  return(min_) 
}

s <- function(index, data, kmeans) {
  if (a(index, data, kmeans) == 0)
    return(0)
  else {
    a_o <- a(index, data, kmeans)
    b_o <- b(index, data, kmeans)
    return( (b_o - a_o) / (max(a_o, b_o) ) )
  }
}

calcSilhouetteCoef <- function(data,kmeans) {
  sum_s = 0
  for (i in 1:kmeans$iter+1){
    current_cluster_size = kmeans$size[i] 
    for(j in 1:nrow(data)){
      if(i == kmeans$cluster[j]){
        sum_s = sum_s +  s(j,data,kmeans)
        
      }
    }
    sum_s = sum_s/current_cluster_size
  }
  sum_s = sum_s/kmeans$iter
  
  
  return(sum_s) 
}