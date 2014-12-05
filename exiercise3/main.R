source("silhouette.R")

newiris <- iris
newiris$Species <- NULL
kc <- kmeans(newiris, 3)

print(calcSilhouetteCoef(newiris, kc))