## This is the Data Analysis for Objective 8 
## for Stat 143 Survey Operations Project

#install.packages("rstudioapi")

library(rstudioapi)  
library(readxl)
library(cluster)
library(tidyr)
library(dplyr)
library(NbClust)

# Setting the working directory
setwd(dirname(getSourceEditorContext()$path))

# import the file with socio demographic variables and factor scores
df <- read_xlsx("socio_demog_and_factor_scores.xlsx")
str(df)

df_cluster <- df %>% select(attitude_f1, attitude_f2, attitude_f3, 
                            opinion_f1, opinion_f2, opinion_f3, opinion_f4)

##################################
##### Hierarchical Clustering ####
##################################

# Euclidean Single Linkage
euc_dist <- cluster::daisy(x = df_cluster, metric = "euclidean") 
agnes_single_euc <- cluster::agnes(x = euc_dist, diss = T, method = "single")
plot(agnes_single_euc, which.plots = 2)

# Euclidean Complete Linkage
agnes_comp_euc <- cluster::agnes(x = df_cluster, metric = "euclidean", diss = F, method = "complete") 
plot(agnes_comp_euc, which.plots = 2)

# Euclidean Average Linkage
agnes_avg_euc <- cluster::agnes(x = df_cluster, metric = "euclidean", diss = F, method = "average") 
plot(agnes_avg_euc, which.plots = 2)

# Ward's Linkage
agnes_ward <- cluster::agnes(x = df_cluster, diss = F, method = "ward") 
plot(agnes_ward, which.plots = 2)

ward_clust7 <- cutree(tree = agnes_ward, k = 7) 
table(ward_clust7)


# Best Number of Clusters
best_ch <- NbClust(data = scale(df_cluster),
                   distance = "euclidean",
                   min.nc = 2,
                   max.nc = 10,
                   method = "ward.D",
                   index = "ch")
best_ch$All.index
# max ch = 2
best_sil <- NbClust(data = scale(df_cluster),
                    distance = "euclidean",
                    min.nc = 2,
                    max.nc = 10,
                    method = "ward.D",
                    index = "silhouette")
best_sil$All.index
# max sil = 2
best_db <- NbClust(data = scale(df_cluster),
                   distance = "euclidean",
                   min.nc = 2,
                   max.nc = 10,
                   method = "ward.D",
                   index = "db")
best_db$All.index
# min db = 10
best_rat <- NbClust(data = scale(df_cluster),
                    distance = "euclidean",
                    min.nc = 2,
                    max.nc = 10,
                    method = "ward.D",
                    index = "ratkowsky")
best_rat$All.index
# max rat = 4
best_ccc <- NbClust(data = scale(df_cluster),
                    distance = "euclidean",
                    min.nc = 2,
                    max.nc = 10,
                    method = "ward.D",
                    index = "ccc")
best_ccc$All.index
# max ccc = 2
best_fm <- NbClust(data = scale(df_cluster),
                   distance = "euclidean",
                   min.nc = 1,
                   max.nc = 10,
                   method = "ward.D",
                   index = "friedman")
best_fm$All.index[2:10] - best_fm$All.index[1:9]
#max fm = 2
best_tr <- NbClust(data = scale(df_cluster),
                   distance = "euclidean",
                   min.nc = 1,
                   max.nc = 10,
                   method = "ward.D",
                   index = "trcovw")
best_tr$All.index[1:9] - best_tr$All.index[2:10]
# max tr = 2
