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



b <- function(index_o,data,cv){
  
  min_ = 9999999
  for (i in 1:cv$iter+1){
    o_cluster = cv$cluster[index_o]
    current_cluster_size = cv$size[i] 
    dist_sum = 0
   # print("I")
  #  print(i)
    if(i != o_cluster){
      for(j in 1:nrow(data)){
        #print
        #print(i)
        #print(cv$cluster[j])
        if(i == cv$cluster[j]){
          
           dist_sum = dist_sum + sqrt(sum((data[index_o,] - data[j,])^2))

        }
        
      }
      print(dist_sum)
      print(current_cluster_size)
      normalized_dist_sum = dist_sum /current_cluster_size
      print(normalized_dist_sum)
     # print(normalized_dist_sum)
      if(normalized_dist_sum < min_){
        min_ = normalized_dist_sum
      }
    }

    
  }
  
  return(min_) 
}

x = b(1,newiris,kc)
print(x)
newiris <- iris
newiris$Species <- NULL
kc <- kmeans(newiris,3)




