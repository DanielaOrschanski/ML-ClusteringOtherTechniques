# Set up -------------------------------------------------------------------------------------------
# Working Directory
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Libraries
library(data.table)
library(plotly)
library(cluster) # additional clustering algorithms
library(factoextra) # A lot of nice visualizations and eval for clustering
#library(dendextend)
#library(fpc)
library(dbscan)

# SYNTHETIC dataset -------------------------------------------------------------------------------------
dt <- fread("~/IMC Subjects/R/Machine Learning/drilling.csv")
setDT(dt)
dt

#visualize (we don't know the labels beforehand):
plot_ly(data    = dt,
        x       = ~x,
        y       = ~y,
        type    = "scatter",
        mode    = "markers")

#I can distinguish 4 groups 

##1. Kmeans ----
#we will see that it doesnt work properly in this kind of problems
#iterate with various options of K

#Elbow method
cls<- data.table(k = 1:10, WSS = 0)
for (i in cls[, k]) {
  cl <- kmeans(dt, centers = i, nstart = 25)
  cls[ k == i, WSS := cl$tot.withinss]
}
plot_ly(data= cls, x=~k, y=~WSS, type= "scatter", mode= "lines")

  #We can chose 2,3 or 4 values of K

#K=2:
cl_km <- kmeans(dt, centers = 2, nstart = 20)
plot_ly(data    = dt,
        x       = ~x,
        y       = dt$y,
        color   = cl_km$cluster,
        symbol  = cl_km$cluster,
        type    = "scatter",
        mode    = "markers")

#k means split the dots into 2 clusters: the more distant cluster is one color and the other 3 are grouped together

#K=3:
cl_km <- kmeans(dt, centers = 3, nstart = 20)
plot_ly(data    = dt,
        x       = ~x,
        y       = dt$y,
        color   = cl_km$cluster,
        symbol  = cl_km$cluster,
        type    = "scatter",
        mode    = "markers")

#The 2 clusters that are closer to each other are grouped together and the other ones are disguished

#K=4:
cl_km <- kmeans(dt, centers = 4, nstart = 20)
plot_ly(data    = dt,
        x       = ~x,
        y       = dt$y,
        color   = cl_km$cluster,
        symbol  = cl_km$cluster,
        type    = "scatter",
        mode    = "markers")
#The 4 clusters are disguished




##2. Hierarchical clustering ----
  #Build a tree-based hierarchical taxonomy from a set of observation
  
  #2.1. Linkage:
cl_hcl <- agnes(dt, method = "ward")
  #The way to measure the distance between 2 clusters is the Ward linkage: if you merge two clusters, how does it change the total distance from centroids. 

  #2.2. Dendrogram:
plot(cl_hcl, which.plots = 2) 
  #The dendrogram indicates that we could split the data into 4 clusters.
  #horizontal lines represent clusters that are joined together
  #The position of the line on the vertical axis indicates distances where clusters were joined
  #I should cut the tree on the height of 2 when there are 4 clusters
  
  #2.3. Cut the dendrogram:
cl_hcl <- cutree(cl_hcl, k = 4) 
?cutree

plot_ly(data    = dt,
        x       = ~x,
        y       = ~y,
        color   = cl_hcl,
        symbol  = cl_hcl,
        type    = "scatter",
        mode    = "markers")

# gives 4 clusters

#The same with 3 clusters:
cl_hcl <- agnes(dt, method = "ward")
cl_hcl <- cutree(cl_hcl, k = 3)

plot_ly(data    = dt,
        x       = ~x,
        y       = ~y,
        color   = cl_hcl,
        symbol  = cl_hcl,
        type    = "scatter",
        mode    = "markers")

#The 2 clusters from above are grouped together


#------------------------------------------------------------------------------------------------------------------------

## DBScan ----
  #Density-BaSedClustering for Application with Noise

# The two most popular packages for dbscan are "fpc" and "dbscan".
# fpc::dbscan is much slower than dbscan::dbscan. However, for our
# small dataset it does not make a difference.
# The library dbscan also contains more recent evolutions of dbscan (optics, hdbscan)

#1. KNN distance plot for the MinPTS rule of thumb (dim*2)
dbscan::kNNdistplot(dt, 4)
  #minpts= 4 = minimum desired cluster size
  #we have 2 features so we willl use 4 as minpts because of the rule
  
  #Eps: calculates the average distance of each point to the 4 closest neighbours
  #0.05 is a suitable eps because is the point when the curve goes vertical

abline(h = 0.05, lty = 2)
?abline

cl_dbscan <- dbscan::dbscan(dt, eps = 0.05, minPts = 4)
cl_fpc <- fpc::dbscan(dt, eps = 0.05, MinPts = 4)

plot(cl_dbscan, data = dt) #plot provided by the dbscan. 
  #dbscan was able to identify the clusters of the non linear shape
  #the noise is identified by non color
  #it includes the detection of outliers because it classify them as noise: not density reachable to core objects 

plot(cl_fpc, data = dt)
  #just changed the symbols used

plot_ly(data    = dt,
        x       = ~x,
        y       = ~y,
        color   = as.character(cl_dbscan$cluster),
        symbol  = as.character(cl_fpc$cluster),
        type    = "scatter",
        mode    = "markers")
  #another format of graphic



## Hdbscan ----
  #hierarchy in the density based
  #we dont have the y dimension, just the x dimension

cl_hdbscan <- hdbscan(dt, minPts = 4, gen_simplified_tree = T)
cl_dbscan
#4 clusters and 32 noise points

cl_hdbscan 
#4 clusters and 16 noise points

plot(cl_hdbscan, scale = 10, show_flat = T) #with an eps of 0.2 we will have 1 cluster, with an eps of 0.1 we will have 2 clusters. if we take eps of 0,06 we will have the same as in another technique (4 clusters)

View(cl_hdbscan)
plot_ly(x = dt$x,
        y = dt$y,
        color = as.character(cl_hdbscan$cluster),
        symbol = cl_hdbscan$cluster,
        type = "scatter",
        mode = "markers")


## OPTICS ----
  
cl_optics <- dbscan::optics(dt, minPts = 4)
plot(cl_optics)
#Reachability plot: the deeper the valley the better delimited is the cluster
#We plot the reachability distance between core point and eps neighborhood in order (with the point that are closer to each other). The reachability propagates with each point so the picks represent the border between clusters because the distance increase.
#identify the number of clusters: there are 2 methods: 
  #a. Static cut or threshold
  #b. Dynamic threshold: Xi aproach: Change in relative cluster density. 


plot(dt, col = "grey")
polygon(dt[cl_optics$order, ])

# Alternative 1: extract cluster by using a single threshold - static
cl_optics_static <- extractDBSCAN(cl_optics, eps_cl = 0.06)
plot(cl_optics_static)
#color= represent the clusters
#the black lines are noise
#there are 4 picks which represents the 4 clusters

# Alternative 2: extract cluster with varying density
# The current reachability plot is too unstable

cl_optics <- dbscan::optics(dt, minPts = 4)
plot(cl_optics)
plot(dt, col = "grey")
polygon(dt[cl_optics$order, ])

cl_optics_dyn <- extractXi(cl_optics, xi = 0.06) #xi starts in the same of eps
plot(cl_optics_dyn)

# We can increase minPts to smoothen a little the reachability
cl_optics <- dbscan::optics(dt, minPts = 10)
plot(cl_optics)
plot(dt, col = "grey")
polygon(dt[cl_optics$order, ])

cl_optics_dyn <- extractXi(cl_optics, xi = 0.06) 
plot(cl_optics_dyn)

# Clustering with eps-threshold
plot_ly(data    = dt,
        x       = ~x,
        y       = ~y,
        color   = cl_optics_static$cluster,
        symbol  = cl_optics_static$cluster,
        type    = "scatter",
        mode    = "markers")

# Clustering with Xi
plot_ly(data    = dt,
        x       = ~x,
        y       = ~y,
        color   = cl_optics_dyn$cluster,
        symbol  = cl_optics_dyn$cluster,
        type    = "scatter",
        mode    = "markers")
#relaxing the reachability condition on the propagation of the threshold we decrease the quantity of dots taken as noise
#This means that we have more than 4 clusters
