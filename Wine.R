# This script implements the k means clustering algorithm 
# and visualization of the resulting clusters using certain libraries.
# The algorithm is used on a dataset with 3 types of wine('Type':A,B,C) with 13 features total.
# Before clustering, the 'Type' column is removed completely and the data is adjusted. '
# Note: The txt file is contained in the zip file. Please change the PATH for the
# read.csv function accordingly. 

library(ggplot2) # library to use the ggplot function 
library(factoextra)# library to extract and visualize multivariate data results
library(VIM)  # library to use the aggr function
Wine <- read.csv("C:/Users/Johnman97/Desktop/Wine.txt")

#Wine_N <- select(Wine,-Type)

# Normalizing the data without the 'Type' column
Wine_std <- scale(Wine[,-1])

# Checking for missing variables.No missing variables!
aggr(Wine_std)

# Implementing the elbow method to determine the optimal number of clusters
# wss: total within class sum of squares method
fviz_nbclust(Wine_std, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2, color = "red")

# Implementing the k-means clustering algorithm for 3 clusters
Wine_clusters <- kmeans(Wine_std,3)
Wine_clusters

# Creating the confusion matrix to check if there are misclassified points
table(Wine$Type,Wine_clusters$cluster)

# Visualizing the k-means clusters 
fviz_cluster(object = Wine_clusters, data = Wine_std, geom = "point",
             stand = FALSE,show.clust.cent = TRUE, ellipse.type = "norm",
             alpha=0.5,ellipse = TRUE)

# fviz_cluster arguments
############################################################################
# object: The output of the algorithm used for clustering(kmeans)
# data: The input data to be clustered. The argument exists solely due to the usage 
# of the kmeans algorithm
# ellipse: TRUE corresponds to the existance of an elliptic outline for each cluster
# ellipse.type : determines the outline type of each cluster
# ('Norm' corresponds to a normal elliptic outline)
# geom: the shape of the data points in the plot. 
# show.clust.cent: used to show the cluster center
# stand: FALSE -> The data is already standarized, 
# so there is no need for extra normalization before plotting
# alpha: [0,1]. Determines the level of trasparency of the data points
############################################################################

