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

mahal <- function(x, cx = NULL){
  x <- as.data.frame(x)
  if(is.null(cx)) cx <- cov(x)
  out <- lapply(1:nrow(x), function(i){
    mahalanobis(x = x, center = do.call("c", x[i,]),
                cov = cx,
                tol = 1e-20)
  }
  )
  return(as.dist(do.call("rbind", out)))
}
##################################
##### Hierarchical Clustering ####
##################################

# Ward's Linkage Euclidean
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

# Ward's Linkage Mahalanobis
mahal_dist_all <- mahal(df_cluster)
agnes_ward_mahal_all <- cluster::agnes(x = mahal_dist_all, diss = T, method = "ward") 
plot(agnes_ward_mahal_all, which.plots = 2)


ward_clust_mahal_all <- cutree(tree = agnes_ward_mahal_all, k = 5) 
fviz_silhouette(silhouette(ward_clust_mahal_all, mahal_dist_all))


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

# Ward's Linkage Euclidean
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

# AGNES Ward Linkage Mahalanobis
mahal_dist_att <- mahal(df_cluster_att)
agnes_ward_mahal_att <- cluster::agnes(mahal_dist_att, diss = T, 
                                       stand = T, method = "ward") 
plot(agnes_ward_mahal_att, which.plots = 2)

mahal_clust_att <- cutree(tree = agnes_ward_mahal_att, k = 5)
fviz_silhouette(silhouette(mahal_clust_att, mahal_dist_att))

# Visualizing Cluster 5
# View(df %>% mutate(cluster = mahal_clust_att) %>% filter(cluster == 5))

prcomp_att <- prcomp(df_cluster_att)
options(ggrepel.max.overlaps = Inf)
df_cluster %>%
  mutate(cluster = mahal_clust_att,
         q_code = df$q_code,
         tag = ifelse(cluster %in% c(5), q_code, NA)
  ) %>%
  bind_cols(as_tibble(prcomp_att$x)) %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(col = "firebrick") +
  ggrepel::geom_label_repel(aes(label = tag))

# Remove Cluster 5
df_cluster_att <- df_cluster_att %>% 
  mutate(clust = mahal_clust_att) %>% 
  filter(clust != 5) %>% 
  select(-clust)

mahal_dist_att <- mahal(df_cluster_att)
agnes_ward_mahal_att <- cluster::agnes(mahal_dist_att, diss = T, 
                                       stand = T, method = "ward") 
plot(agnes_ward_mahal_att, which.plots = 2)

mahal_clust_att <- cutree(tree = agnes_ward_mahal_att, k = 3)
fviz_silhouette(silhouette(mahal_clust_att, mahal_dist_att))

# Initial centers from AGNES (Mahalanobis, Ward's Linkage) k = 3
init_centers_att <- df_cluster_att %>%
  mutate_all(.funs = scale) %>%
  mutate(clust = mahal_clust_att) %>%
  group_by(clust) %>%
  summarise_all(.funs = mean) %>% select(-clust)

kmeans_att <- kmeans(x = scale(df_cluster_att), centers = init_centers_att)
table(kmeans_att$cluster)
att_cluster <- kmeans_att$cluster
fviz_silhouette(silhouette(kmeans_att$cluster, mahal_dist_att))

# Centers of K-Means Algorithm
df_cluster_att %>%
  mutate_all(.funs = scale) %>%
  mutate(kmeans = kmeans_att$cluster) %>%
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

df_cluster_att %>%
  mutate_all(.funs = scale) %>%
  mutate(kmeans = kmeans_att$cluster) %>%
  filter(kmeans == 1) %>% 
  boxplot()

####################### Opinion Factors ############################
df_cluster_opp <- df %>% select(opinion_f1, opinion_f2, opinion_f3, opinion_f4)

##################################
##### Hierarchical Clustering ####
##################################

mahal_dist_opp <- mahal(df_cluster_opp)
agnes_ward_mahal_opp <- cluster::agnes(mahal_dist_opp, diss = T, stand = T, method = "ward") 
plot(agnes_ward_mahal_opp, which.plots = 2)

mahal_clust_opp <- cutree(tree = agnes_ward_mahal_opp, k = 5)
fviz_silhouette(silhouette(mahal_clust_opp, mahal_dist_opp))

# Initial centers from AGNES (Mahalanobis, Ward's Linkage) k = 5
init_centers_opp <- df_cluster_opp %>%
  mutate_all(.funs = scale) %>%
  mutate(clust = mahal_clust_opp) %>%
  group_by(clust) %>%
  summarise_all(.funs = mean) %>% select(-clust) 

kmeans_opp <- kmeans(x = scale(df_cluster_opp), centers = init_centers_opp)
table(kmeans_opp$cluster)

# Silhouette Widths of K-Means solution, k = 5
example_sil_opp <- silhouette(kmeans_opp$cluster, mahal_dist_opp)
cluster_sil_opp <- df_cluster %>%
  mutate(cluster = mahal_clust_opp) %>% 
  mutate(
    clust = kmeans_opp$cluster,
    silhouette = example_sil_opp[,3]
  ) %>% group_by(clust) %>% 
  summarise_at(vars(silhouette), list(name = mean))

# Visualizing Average Silhouette Widths per Cluster
fviz_silhouette(example_sil_opp) # ave sil width = 0.33

opp_cluster <- df_cluster_opp %>% 
  mutate(clust = kmeans_opp$cluster,
         q_code = df$q_code) %>% 
  filter(!q_code %in% c(102, 104, 105)) %>% select(clust)

# Centers of K-Means Algorithm
df_cluster_opp %>%
  mutate_all(.funs = scale) %>%
  mutate(kmeans = kmeans_opp$cluster) %>%
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

# Using AGNES Euclidean Distance
euc_dist_opp <- daisy(df_cluster_opp, metric = c("euclidean"), stand = T)
agnes_ward_euc_opp <- cluster::agnes(euc_dist_opp, diss = T, method = "ward") 
plot(agnes_ward_euc_opp, which.plots = 2)

euc_clust_opp <- cutree(tree = agnes_ward_euc_opp, k = 3)
fviz_silhouette(silhouette(euc_clust_opp, euc_dist_opp))


# Initial centers from AGNES (Euclidean, Ward's Linkage) k = 3
init_centers_opp <- df_cluster_opp %>%
  mutate_all(.funs = scale) %>%
  mutate(clust = euc_clust_opp) %>%
  group_by(clust) %>%
  summarise_all(.funs = mean) %>% select(-clust) 

kmeans_opp <- kmeans(x = scale(df_cluster_opp), centers = init_centers_opp)
table(kmeans_opp$cluster)

# Silhouette Widths of K-Means solution, k = 2
example_sil_opp <- silhouette(kmeans_opp$cluster, euc_dist_opp)
cluster_sil_opp <- df_cluster %>%
  mutate(cluster = euc_clust_opp) %>% 
  mutate(
    clust = kmeans_att$cluster,
    silhouette = example_sil_att[,3]
  ) %>% group_by(clust) %>% 
  summarise_at(vars(silhouette), list(name = mean))

# Visualizing Average Silhouette Widths per Cluster
fviz_silhouette(example_sil_opp) # ave sil width = 0.38


# Initial centers from AGNES (Gower, Ward's Linkage) k = 3
init_centers_opp <- df_cluster_opp %>%
  mutate_all(.funs = scale) %>%
  mutate(clust = gower_clust_opp) %>%
  group_by(clust) %>%
  summarise_all(.funs = mean) %>% select(-clust) 

kmeans_opp <- kmeans(x = scale(df_cluster_opp), centers = init_centers_opp)
table(kmeans_opp$cluster)

# Silhouette Widths of K-Means solution, k = 2
example_sil_opp <- silhouette(kmeans_opp$cluster, gower_dist_opp)
cluster_sil_att <- df_cluster %>%
  mutate(cluster = mahal_clust_att) %>% 
  mutate(
    clust = kmeans_att$cluster,
    silhouette = example_sil_att[,3]
  ) %>% group_by(clust) %>% 
  summarise_at(vars(silhouette), list(name = mean))

# Visualizing Average Silhouette Widths per Cluster
fviz_silhouette(example_sil_opp) # ave sil width = 0.38

##################### Cluster Analysis of Attitude and Opinion Clusters ########

df_att_opp <- data.frame(opinion = opp_cluster, attitude = att_cluster)
str(df_att_opp)

gower.dist <- daisy(df_att_opp, metric = c("gower"))
agnes_ward_att_opp <- cluster::agnes(x = gower.dist, diss = T, method = "ward")
plot(agnes_ward_att_opp, which.plots = 2)

cluster <- cutree(tree = agnes_ward_att_opp, k = 5)
example_sil_att_opp <- silhouette(cluster, gower.dist)
fviz_silhouette(example_sil_att_opp)

final_cluster_solution <- df_cluster %>% mutate(q_code = df$q_code) %>% 
                              filter(!q_code %in% c(102, 104, 105)) %>% 
                              mutate(attitude = df_att_opp$attitude, 
                                     opinion = df_att_opp$clust, 
                                     cluster = cluster)

table(cluster)
write.csv(final_cluster_solution, "final_cluster_solution.csv")


##################### Demographic Profile ######################################
demo <- df %>% select(q_code, age, sex, occ_status, income, 
                      education, fam_w_jeep, w_driver_rel) %>% 
  filter(!q_code %in% c(102, 104, 105)) %>% 
  mutate(cluster = cluster) %>% select(-q_code)
