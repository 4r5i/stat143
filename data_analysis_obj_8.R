## This is the Data Analysis for Objective 8 
## for Stat 143 Survey Operations Project

#hi hehe 

#install.packages("rstudioapi")
library(rstudioapi)  
library(readxl)
library(cluster)
library(tidyr)
library(dplyr)
library(NbClust)
library(ggplot2)
library(factoextra)

# Setting the working directory
setwd(dirname(getSourceEditorContext()$path))

# import the file with socio demographic variables and factor scores
df <- read_xlsx("socio_demog_and_factor_scores.xlsx")
str(df)

df_cluster <- df %>% select(attitude_f1, attitude_f2, attitude_f3, opinion_f1, opinion_f2, opinion_f3, opinion_f4)

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

fviz_silhouette(silhouette(cutree(tree = agnes_comp_euc, k = 7), 
                           daisy(x = df_cluster, 
                                 metric = "euclidean", 
                                 stand = T)))


# Euclidean Average Linkage
agnes_avg_euc <- cluster::agnes(x = df_cluster, metric = "euclidean", diss = F, method = "average") 
plot(agnes_avg_euc, which.plots = 2)

# Ward's Linkage
agnes_ward <- cluster::agnes(x = df_cluster, diss = F, method = "ward") 
plot(agnes_ward, which.plots = 2)


ward_clust7 <- cutree(tree = agnes_ward, k = 7) 
table(ward_clust7)

ward_clust3 <- cutree(tree = agnes_ward, k = 3) 
table(ward_clust3)

fviz_silhouette(silhouette(cutree(tree = agnes_ward, k = 7), 
                           daisy(x = df_cluster, 
                                 metric = "euclidean", 
                                 stand = T)))
fviz_silhouette(silhouette(cutree(tree = agnes_ward, k = 3), 
                           daisy(x = df_cluster, 
                                 metric = "euclidean", 
                                 stand = T)))

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


# Silhouette Widths of AGNES solution (Euclidean, Ward's Linkage) k = 7
example_sil1 <- silhouette(ward_clust7, daisy(x = df_cluster, metric = "euclidean", stand = T))
cluster_sil1 <- df_cluster %>%
  mutate(
    clust = ward_clust7,
    silhouette = example_sil1[,3]
  ) %>% group_by(clust) %>% 
  summarise_at(vars(silhouette), list(name = mean))

# Visualizing Average Silhouette Widths per Cluster
fviz_silhouette(example_sil1)

# Silhouette Widths of AGNES solution (Euclidean, Ward's Linkage) k = 3
example_sil2 <- silhouette(ward_clust3, daisy(x = df_cluster, metric = "euclidean", stand = T))
cluster_sil2 <- df_cluster %>%
  mutate(
    clust = ward_clust3,
    silhouette = example_sil2[,3]
  ) %>% group_by(clust) %>% 
  summarise_at(vars(silhouette), list(name = mean))

# Visualizing Average Silhouette Widths per Cluster
fviz_silhouette(example_sil2)

########################################
##### Non - Hierarchical Clustering ####
########################################



# Initial centers from AGNES (Euclidean, Ward's Linkage) k = 7
init_centers1 <- df_cluster %>%
  mutate_all(.funs = scale) %>%
  mutate(clust = ward_clust7) %>%
  group_by(clust) %>%
  summarise_all(.funs = mean) %>%
  select(-clust)

kmeans1 <- kmeans(x = scale(df_cluster), centers = init_centers1)
table(kmeans1$cluster)

# Silhouette Widths of K-Means solution, k = 7
example_sil3 <- silhouette(kmeans1$cluster, daisy(x = df_cluster, metric = "euclidean", stand = T))
cluster_sil3 <- df_cluster %>%
  mutate(cluster = ward_clust7) %>% 
  mutate(
    clust = kmeans1$cluster,
    silhouette = example_sil3[,3]
  ) %>% group_by(clust) %>% 
  summarise_at(vars(silhouette), list(name = mean))

# Visualizing Average Silhouette Widths per Cluster
fviz_silhouette(example_sil3)

# Initial centers from AGNES (Euclidean, Ward's Linkage) k = 3
init_centers2 <- df_cluster %>%
  mutate_all(.funs = scale) %>%
  mutate(clust = ward_clust3) %>%
  group_by(clust) %>%
  summarise_all(.funs = mean) %>%
  select(-clust)

kmeans2 <- kmeans(x = scale(df_cluster), centers = init_centers2)
table(kmeans2$cluster)

# Silhouette Widths of K-Means solution, k = 3
example_sil4 <- silhouette(kmeans2$cluster, daisy(x = df_cluster, metric = "euclidean", stand = T))
cluster_sil4 <- df_cluster %>%
  mutate(cluster = ward_clust3) %>% 
  mutate(
    clust = kmeans1$cluster,
    silhouette = example_sil4[,3]
  ) %>% group_by(clust) %>% 
  summarise_at(vars(silhouette), list(name = mean))

# Visualizing Average Silhouette Widths per Cluster
fviz_silhouette(example_sil4)


########################################
##### PAM and CLARA ####################
########################################

pam_results <- pam(df_cluster, k = 2, metric = "euclidean", stand = TRUE)
fviz_silhouette(pam_results, palette = "jco", ggtheme = theme_classic())

clara_results <- clara(df_cluster, k = 6, metric = "euclidean", stand = TRUE)
fviz_silhouette(clara_results, palette = "jco", ggtheme = theme_classic())


###################################################################
##### Using PCA on Factor Scores before Clustering ################
###################################################################
prcomp <- prcomp(x = df_cluster, scale. = T)
summary(prcomp)
princomp <- princomp(x = df_cluster, cor = T)
summary(princomp)

# Kaiser Rule
summary(prcomp)$sdev^2

# Scree Plot
tibble(eigenvalues = (prcomp$sdev)^2, PC = 1:7) %>%
  ggplot(aes(y = eigenvalues, x = PC)) +
  geom_point() +
  geom_bar(stat = "identity", fill = "#FFE392") +
  geom_line() +
  scale_x_continuous(breaks = 1:7) +
  ggthemes::theme_gdocs() 


# PC Loading
prcomp$rotation
princomp$loadings

# Clustering of PC of Attitude and Opinion Factors
df_cluster_pc <- bind_cols(df_cluster, prcomp$x) %>% select(PC1, PC2, PC3, PC4)

agnes_ward_pc <- cluster::agnes(x = df_cluster_pc, diss = F, method = "ward") 
plot(agnes_ward_pc, which.plots = 2)

fviz_silhouette(silhouette(cutree(tree = agnes_ward_pc, k = 6), 
                           daisy(x = df_cluster_pc, 
                                 metric = "euclidean", 
                                 stand = T)))
ward_clust_pc <- cutree(tree = agnes_ward_pc, k = 6)
table(ward_clust_pc)

init_centers_pc <- df_cluster_pc %>%
  mutate_all(.funs = scale) %>%
  mutate(clust = ward_clust_pc) %>%
  group_by(clust) %>%
  summarise_all(.funs = mean) %>%
  select(-clust)

kmeans_pc <- kmeans(x = scale(df_cluster_pc), centers = init_centers_pc)
table(kmeans_pc$cluster)

# Silhouette Widths of K-Means solution, k = 4
example_sil_pc <- silhouette(kmeans_pc$cluster, daisy(x = df_cluster_pc, metric = "euclidean", stand = T))
cluster_sil_pc <- df_cluster_pc %>%
  mutate(cluster = ward_clust_pc) %>% 
  mutate(
    clust = kmeans_pc$cluster,
    silhouette = example_sil_pc[,3]
  ) %>% group_by(clust) %>% 
  summarise_at(vars(silhouette), list(name = mean))

# Visualizing Average Silhouette Widths per Cluster
fviz_silhouette(example_sil_pc)


# Outlier Detection
dist_from_center  <- mahalanobis(x = (df_cluster_pc %>% select(PC1, PC2)), 
                                 center = c(0,0), 
                                 cov = var((df_cluster_pc %>% select(PC1, PC2))))  
df_cluster_pc %>% 
  mutate(id = df$q_code,         
        dist = dist_from_center, 
        tag = if_else(dist > (mean(dist_from_center)+2*sqrt(var(dist_from_center))), "might be outlier", "ok")) %>%   
  ggplot(aes(x = PC1, y = PC2)) +   
  geom_text(aes(label = id, col = tag)) +   
  ggthemes::theme_gdocs() 


options(ggrepel.max.overlaps = Inf)
df_cluster %>%
  mutate(cluster = kmeans_pc$cluster,
         q_code = df$q_code,
         tag = ifelse(cluster %in% c(5), q_code, NA)
         ) %>%
  bind_cols(as_tibble(prcomp$x)) %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(col = "firebrick") +
  ggrepel::geom_label_repel(aes(label = tag))

df_cluster_pc1 <- df_cluster_pc %>% 
  bind_cols(cluster = kmeans_pc$cluster) %>% 
  filter(cluster != 5)

agnes_ward_pc1 <- cluster::agnes(x = df_cluster_pc1, diss = F, method = "ward") 
plot(agnes_ward_pc1, which.plots = 2)

ward_clust_pc1 <- cutree(tree = agnes_ward_pc1, k = 5) 
table(ward_clust_pc1)

fviz_silhouette(silhouette(cutree(tree = agnes_ward_pc1, k = 5), 
                           daisy(x = df_cluster_pc1, 
                                 metric = "euclidean", 
                                 stand = T)))

init_centers_pc1 <- df_cluster_pc1 %>%
  mutate_all(.funs = scale) %>%
  mutate(clust = ward_clust_pc1) %>%
  group_by(clust) %>%
  summarise_all(.funs = mean) %>%
  select(-clust)

kmeans_pc1 <- kmeans(x = scale(df_cluster_pc1), centers = init_centers_pc1)
table(kmeans_pc1$cluster)

# Silhouette Widths of K-Means solution, k = 4
example_sil_pc1 <- silhouette(kmeans_pc1$cluster, daisy(x = df_cluster_pc1, metric = "euclidean", stand = T))
cluster_sil_pc1 <- df_cluster_pc1 %>%
  mutate(cluster = ward_clust_pc1) %>% 
  mutate(
    clust = kmeans_pc1$cluster,
    silhouette = example_sil_pc1[,3]
  ) %>% group_by(clust) %>% 
  summarise_at(vars(silhouette), list(name = mean))

# Visualizing Average Silhouette Widths per Cluster
fviz_silhouette(example_sil_pc1)

# Centers of K-Means Algorithm
df_cluster_pc1 %>%
  mutate_all(.funs = scale) %>%
  mutate(kmeans = kmeans_pc1$cluster) %>%
  group_by(kmeans) %>%
  summarise_all(.funs = mean) %>%
  pivot_longer(c(PC1, PC2, PC3, PC4), names_to = "vars", values_to = "mean") %>%
  mutate(tag = factor(mean < 0, labels = c("average and up", "below average"))) %>%
  ggplot(aes(x = vars, y = mean)) +
  geom_bar(aes(fill = tag), stat = "identity", position = "dodge") +
  facet_wrap(. ~ kmeans) +
  coord_flip() +
  ylim(-2,2) +
  ggthemes::theme_gdocs()

################################################################################
####################### Individual Cluster Analyses ############################
################################################################################

####################### Attitude Factors ############################
df_cluster_att <- df %>% select(attitude_f1, attitude_f2, attitude_f3)

##################################
##### Hierarchical Clustering ####
##################################

# Euclidean Single Linkage
euc_dist <- cluster::daisy(x = df_cluster_att, metric = "euclidean") 
agnes_single_euc <- cluster::agnes(x = euc_dist, diss = T, method = "single")
plot(agnes_single_euc, which.plots = 2)

# Euclidean Complete Linkage
agnes_comp_euc <- cluster::agnes(x = df_cluster_att, metric = "euclidean", diss = F, method = "complete") 
plot(agnes_comp_euc, which.plots = 2)

# Euclidean Average Linkage
agnes_avg_euc <- cluster::agnes(x = df_cluster_att, metric = "euclidean", diss = F, method = "average") 
plot(agnes_avg_euc, which.plots = 2)

# Ward's Linkage
agnes_ward <- cluster::agnes(x = df_cluster_att, diss = F, method = "ward") 
plot(agnes_ward, which.plots = 2)

fviz_silhouette(silhouette(cutree(tree = agnes_ward, k = 5), 
                           daisy(x = df_cluster_att, 
                                 metric = "euclidean", 
                                 stand = T)))

# Best Number of Clusters
best_ch <- NbClust(data = scale(df_cluster_att),
                   distance = "euclidean",
                   min.nc = 2,
                   max.nc = 10,
                   method = "ward.D",
                   index = "ch")
best_ch$All.index
# max ch = 10
best_sil <- NbClust(data = scale(df_cluster_att),
                    distance = "euclidean",
                    min.nc = 2,
                    max.nc = 10,
                    method = "ward.D",
                    index = "silhouette")
best_sil$All.index
# max sil = 4
best_db <- NbClust(data = scale(df_cluster_att),
                   distance = "euclidean",
                   min.nc = 2,
                   max.nc = 10,
                   method = "ward.D",
                   index = "db")
best_db$All.index
# min db = 10
best_rat <- NbClust(data = scale(df_cluster_att),
                    distance = "euclidean",
                    min.nc = 2,
                    max.nc = 10,
                    method = "ward.D",
                    index = "ratkowsky")
best_rat$All.index
# max rat = 4
best_ccc <- NbClust(data = scale(df_cluster_att),
                    distance = "euclidean",
                    min.nc = 2,
                    max.nc = 10,
                    method = "ward.D",
                    index = "ccc")
best_ccc$All.index
# max ccc = 2
best_fm <- NbClust(data = scale(df_cluster_att),
                   distance = "euclidean",
                   min.nc = 1,
                   max.nc = 10,
                   method = "ward.D",
                   index = "friedman")
best_fm$All.index[2:10] - best_fm$All.index[1:9]
#max fm = 10
best_tr <- NbClust(data = scale(df_cluster_att),
                   distance = "euclidean",
                   min.nc = 1,
                   max.nc = 10,
                   method = "ward.D",
                   index = "trcovw")
best_tr$All.index[1:9] - best_tr$All.index[2:10]
# max tr = 3

ward_clust5 <- cutree(tree = agnes_ward, k = 5) 
table(ward_clust5)

# Initial centers from AGNES (Euclidean, Ward's Linkage) k = 5
init_centers <- df_cluster_att %>%
  mutate_all(.funs = scale) %>%
  mutate(clust = ward_clust5) %>%
  group_by(clust) %>%
  summarise_all(.funs = mean) %>%
  select(-clust)

kmeans <- kmeans(x = scale(df_cluster_att), centers = init_centers)
table(kmeans$cluster)
kmeans_att <- kmeans

# Silhouette Widths of K-Means solution, k = 5
example_sil <- silhouette(kmeans$cluster, daisy(x = df_cluster_att, metric = "euclidean", stand = T))
cluster_sil <- df_cluster %>%
  mutate(cluster = ward_clust5) %>% 
  mutate(
    clust = kmeans1$cluster,
    silhouette = example_sil[,3]
  ) %>% group_by(clust) %>% 
  summarise_at(vars(silhouette), list(name = mean))

# Visualizing Average Silhouette Widths per Cluster
fviz_silhouette(example_sil) # ave sil width = 0.28

# Centers of K-Means Algorithm
df_cluster_att %>%
  mutate_all(.funs = scale) %>%
  mutate(kmeans = kmeans$cluster) %>%
  group_by(kmeans) %>%
  summarise_all(.funs = mean) %>%
  pivot_longer(c(attitude_f1, attitude_f2, attitude_f3), names_to = "vars", values_to = "mean") %>%
  mutate(tag = factor(mean < 0, labels = c("average and up", "below average"))) %>%
  ggplot(aes(x = vars, y = mean)) +
  geom_bar(aes(fill = tag), stat = "identity", position = "dodge") +
  facet_wrap(. ~ kmeans) +
  coord_flip() +
  ylim(-2,2) +
  ggthemes::theme_gdocs()

####################### Opinion Factors ############################
df_cluster_opp <- df %>% select(opinion_f1, opinion_f2, opinion_f3, opinion_f4)

##################################
##### Hierarchical Clustering ####
##################################

# Euclidean Single Linkage
euc_dist <- cluster::daisy(x = df_cluster_opp, metric = "euclidean") 
agnes_single_euc <- cluster::agnes(x = euc_dist, diss = T, method = "single")
plot(agnes_single_euc, which.plots = 2)

# Euclidean Complete Linkage
agnes_comp_euc <- cluster::agnes(x = df_cluster_opp, metric = "euclidean", diss = F, method = "complete") 
plot(agnes_comp_euc, which.plots = 2)

# Euclidean Average Linkage
agnes_avg_euc <- cluster::agnes(x = df_cluster_opp, metric = "euclidean", diss = F, method = "average") 
plot(agnes_avg_euc, which.plots = 2)

# Ward's Linkage
agnes_ward <- cluster::agnes(x = df_cluster_opp, diss = F, method = "ward") 
plot(agnes_ward, which.plots = 2)

# Best Number of Clusters
best_ch <- NbClust(data = scale(df_cluster_opp),
                   distance = "euclidean",
                   min.nc = 2,
                   max.nc = 10,
                   method = "ward.D",
                   index = "ch")
best_ch$All.index
# max ch = 3
best_sil <- NbClust(data = scale(df_cluster_opp),
                    distance = "euclidean",
                    min.nc = 2,
                    max.nc = 10,
                    method = "ward.D",
                    index = "silhouette")
best_sil$All.index
# max sil = 4
best_db <- NbClust(data = scale(df_cluster_opp),
                   distance = "euclidean",
                   min.nc = 2,
                   max.nc = 10,
                   method = "ward.D",
                   index = "db")
best_db$All.index
# min db = 4
best_rat <- NbClust(data = scale(df_cluster_opp),
                    distance = "euclidean",
                    min.nc = 2,
                    max.nc = 10,
                    method = "ward.D",
                    index = "ratkowsky")
best_rat$All.index
# max rat = 4
best_ccc <- NbClust(data = scale(df_cluster_opp),
                    distance = "euclidean",
                    min.nc = 2,
                    max.nc = 10,
                    method = "ward.D",
                    index = "ccc")
best_ccc$All.index
# max ccc = 2
best_fm <- NbClust(data = scale(df_cluster_opp),
                   distance = "euclidean",
                   min.nc = 1,
                   max.nc = 10,
                   method = "ward.D",
                   index = "friedman")
best_fm$All.index[2:10] - best_fm$All.index[1:9]
#max fm = 3
best_tr <- NbClust(data = scale(df_cluster_opp),
                   distance = "euclidean",
                   min.nc = 1,
                   max.nc = 10,
                   method = "ward.D",
                   index = "trcovw")
best_tr$All.index[1:9] - best_tr$All.index[2:10]
# max tr = 2

ward_clust4 <- cutree(tree = agnes_ward, k = 4) 
table(ward_clust4)

# Initial centers from AGNES (Euclidean, Ward's Linkage) k = 4
init_centers <- df_cluster_opp %>%
  mutate_all(.funs = scale) %>%
  mutate(clust = ward_clust4) %>%
  group_by(clust) %>%
  summarise_all(.funs = mean) %>%
  select(-clust)

kmeans <- kmeans(x = scale(df_cluster_opp), centers = init_centers)
table(kmeans$cluster)
kmeans_opp <- kmeans

# Silhouette Widths of K-Means solution, k = 4
example_sil <- silhouette(kmeans$cluster, daisy(x = df_cluster_opp, metric = "euclidean", stand = T))
cluster_sil <- df_cluster %>%
  mutate(cluster = ward_clust4) %>% 
  mutate(
    clust = kmeans1$cluster,
    silhouette = example_sil[,3]
  ) %>% group_by(clust) %>% 
  summarise_at(vars(silhouette), list(name = mean))

# Visualizing Average Silhouette Widths per Cluster
fviz_silhouette(example_sil) # ave sil width = 0.22

# Centers of K-Means Algorithm
df_cluster_opp %>%
  mutate_all(.funs = scale) %>%
  mutate(kmeans = kmeans$cluster) %>%
  group_by(kmeans) %>%
  summarise_all(.funs = mean) %>%
  pivot_longer(c(opinion_f1, opinion_f2, opinion_f3, opinion_f4), names_to = "vars", values_to = "mean") %>%
  mutate(tag = factor(mean < 0, labels = c("average and up", "below average"))) %>%
  ggplot(aes(x = vars, y = mean)) +
  geom_bar(aes(fill = tag), stat = "identity", position = "dodge") +
  facet_wrap(. ~ kmeans) +
  coord_flip() +
  ylim(-2,2) +
  ggthemes::theme_gdocs()

##################### Cluster Analysis of Attitude and Opinion Clusters ########

df_att_opp <- data.frame(att_cluster = as.factor(kmeans_att$cluster), 
              opp_cluster = as.factor(kmeans_opp$cluster))

gower.dist <- daisy(df_att_opp, metric = c("gower"))

divisive.clust <- diana(as.matrix(gower.dist), diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")

agnes_ward_att_opp <- cluster::agnes(x = gower.dist, diss = T, method = "ward")
plot(agnes_ward_att_opp, which.plots = 2)
example_sil_att_opp <- silhouette(cutree(tree = agnes_ward_att_opp, k = 5), gower.dist)
fviz_silhouette(example_sil_att_opp)

cluster <- cutree(tree = agnes_ward_att_opp, k = 5)

df_att_opp %>%
  mutate_all(.funs = scale) %>%
  mutate(cluster = cluster) %>%
  group_by(cluster) %>%
  summarise_all(.funs = mode) %>%
  pivot_longer(c(1,2,3,4,5), names_to = "vars", values_to = "mode") %>%
  mutate(tag = factor(mean < 0, labels = c("average and up", "below average"))) %>%
  ggplot(aes(x = vars, y = mean)) +
  geom_bar(aes(fill = tag), stat = "identity", position = "dodge") +
  facet_wrap(. ~ kmeans) +
  coord_flip() +
  ylim(-2,2) +
  ggthemes::theme_gdocs()


library(klaR)
kmodes <- kmodes(df_att_opp, 5)
fviz_silhouette(silhouette(kmodes$cluster, gower.dist))

