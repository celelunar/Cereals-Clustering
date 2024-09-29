#### LIBRARIES ####
library(dplyr)
library(stats)
library(factoextra)
library(cluster)
library(purrr)
library(clValid)
library(readxl)

#### DATA PREPROCESSING ####

##### 1. Read Data #####
data = read_excel("C:/Users/User/Documents/Multivariate Statistics/06_19_2024/Table11_9.xlsx")
head(data)

##### 2. Take Data From Specific Columns #####
dcal = data %>% select(3:10)
head(dcal)

##### 3. Data Scaling #####
dscale = as.matrix(scale(dcal))
head(dscale)

##### 4. Distance Matrix #####
dist_matrix = dist(dscale, method = "euclidean", diag = TRUE, upper = TRUE)
dist_matrix

#### SINGLE LINKAGE ####

##### 1. Cluster Dendogram ##### 
single_link = hclust(dist_matrix, method = "single")
par(mar = c(1, 1, 1, 1))
plot(single_link, cex = 0.6, hang = -1)

##### 2. k Groups ##### 
# The number of k can be seen from the dendogram results and can be adjusted.

# Seen from the cluster plot below, with 3 clusters a clear data division is obtained. Where the data cluster 1 (red) is close together, the two data cluster 3 (blue) are close together, and cluster 2 (green) is alone.

single_subgroup = cutree(single_link, k = 3)
fviz_cluster(list(data = dscale, cluster = single_subgroup))

plot(single_link, cex = 0.6, hang = -1)
rect.hclust(single_link, k = 3, border = 2:4)

#### COMPLETE LINKAGE ####

##### 1. Cluster Dendogram #####
complete_link = hclust(dist_matrix, method = "complete")
par(mar = c(1, 1, 1, 1))
plot(complete_link, cex = 0.6, hang = -1)

##### 2. k Groups ##### 
# The number of k can be seen from the dendogram results and can be adjusted.

# Seen from the cluster plot below, with 3 clusters, a fairly clear data division is obtained except for data 26. Where the data has a close distance to cluster 1, but the cluster results state that it is included in cluster 3.

complete_subgroup = cutree(complete_link, k = 3)
fviz_cluster(list(data = dscale, cluster = complete_subgroup))

plot(complete_link, cex = 0.6, hang = -1)
rect.hclust(complete_link, k = 3, border = 2:4)

#### AVERAGE LINKAGE ####

##### 1. Cluster Dendogram ##### 
average_link = hclust(dist_matrix, method = "average")
par(mar = c(1, 1, 1, 1))
plot(average_link, cex = 0.6, hang = -1)

##### 2. k Groups ##### 
# The number of k can be seen from the dendogram results and can be adjusted.

# It can be seen from the cluster plot below, with 3 clusters a clear data division is obtained where the data in cluster 1 is adjacent, and clusters 2 and 3 stand alone.

average_subgroup = cutree(average_link, k = 3)
fviz_cluster(list(data = dscale, cluster = average_subgroup))

plot(average_link, cex = 0.6, hang = -1)
rect.hclust(average_link, k = 3, border = 2:4)

#### ASW VALUES ####
# ASW or Average Silhouette Width is a performance metric of clustering and is used to find out which method is the best (the clearer the cluster division). The ASW value is in the interval -1 to 1 where the closer to 1, the better.

# The output below shows that the complete method produces the best cluster for the data.

m = c("average", "single", "complete")
names(m) = c("average", "single", "complete")

ac = function(x){
  agnes(dscale, method = x)$ac
}
map_dbl(m, ac)

#### K-MEANS ####

##### 1. Optimal Number of Clusters ##### 
# To find out the optimal number of clusters, look at the highest one or the one with the dotted line. 

fviz_nbclust(dscale, kmeans, method = "silhouette")

##### 2. Modelling ##### 
# After getting the results, a profile analysis can be made from each cluster. It is known by reading the results of the cluster means. Find the highest and lowest values. If there are still clusters that do not have a profile, then compare the mean with the mean of other clusters OR if you want it easy, minus is quite low and plus is quite high: DDDD Applicable if there is a plus minus, if there is not, you have to check 1 by 1.

# Profile analysis results:
# Cluster 1 Protein, fiber, carbohydrates, and potassium are quite high; Calories, fat, sodium, and sugar are quite low
# Cluster 2 High sodium and carbohydrates; Low fiber
# Cluster 3 High fat
# Cluster 4 High calories and sugar
# Cluster 5 High protein
# Cluster 6 Calories, protein, fat, sodium, fiber, carbohydrates, sugar, and potassium are quite high.
# Cluster 7 Low calories, protein, fat, sodium, fiber, sugar, potassium
# Cluster 8 High fiber and potassium; Low carbohydrates

kmeans_clust = kmeans(dscale, centers = 8, nstart = 25)
kmeans_clust

##### 3. Cluster Plot ##### 
# It can be seen in the cluster plot below that there are several clusters that overlap, such as cluster 5 overlapping with clusters 3 and 1 or cluster 2 with cluster 4.

# If this happens, it is better to try a different number of clusters.

fviz_cluster(kmeans_clust, data = dscale)

##### 4. Number of Data in Each Cluster ##### 
data_cluster = kmeans_clust$cluster
cluster_count = data.frame(Count = table(data_cluster))
print(cluster_count)

##### 5. Number of Cluster Validation ##### 
# The cluster plot results above show that we should try other number of clusters, so the code below can be used to select clusters with connectivity and dunn metrics.
rownames(dscale) = 1:nrow(dscale)
test = clValid(dscale, nClust = 2:9, clMethods = "kmeans", validation = "internal")
summary(test)