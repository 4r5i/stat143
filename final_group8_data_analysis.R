## This is the Data Analysis for Objective 8 
## for Stat 143 Survey Operations Project

#install.packages("rstudioapi")
library(rstudioapi)  
library(readxl)
library(cluster)
library(tidyr)
library(dplyr)
library(NbClust)
library(ggplot2)
library(factoextra)
library(ggplot2)
library(data.table)


################################################################################
####################### Preliminaries ##########################################
################################################################################

# setting the working directory
setwd(dirname(getSourceEditorContext()$path))

# importing the file with socio demographic variables and factor scores
df <- read_xlsx("socio_demog_and_factor_scores.xlsx")
str(df)

# creating the final data frame for clustering
df_cluster <- df %>% select(attitude_f1, attitude_f2, attitude_f3, opinion_f1, 
                            opinion_f2, opinion_f3, opinion_f4)

mahal <- function(x, cx = NULL){
  
  # Description
  # Calculates the Mahalabonis distance 
  # given data frame, x
  
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


################################################################################
####################### Individual Cluster Analyses ############################
################################################################################

####################### Attitude Factors #######################################
df_cluster_att <- df %>% select(attitude_f1, attitude_f2, attitude_f3)

# Hierarchical Clustering

# AGNES using Ward Linkage and Mahalanobis Distance
mahal_dist_att <- mahal(df_cluster_att)
agnes_ward_mahal_att <- cluster::agnes(mahal_dist_att, diss = T, 
                                       stand = T, method = "ward")

# Dendogram Plot
plot(agnes_ward_mahal_att, which.plots = 2)

# Determining Number of Clusters
NbClust(data = df_cluster_att, 
        diss = mahal_dist_att,
        distance = NULL,
        min.nc = 2, 
        max.nc = 15, 
        method = "ward.D", 
        index = "all")

#* Among all indices:                                                
#* 4 proposed 2 as the best number of clusters 
#* 3 proposed 3 as the best number of clusters 
#* 5 proposed 4 as the best number of clusters 
#* 1 proposed 6 as the best number of clusters 
#* 3 proposed 7 as the best number of clusters 
#* 4 proposed 8 as the best number of clusters 
#* 1 proposed 9 as the best number of clusters 
#* 2 proposed 15 as the best number of clusters 
#* According to the majority rule, the best number of clusters is  4 

# Using 4 clusters
mahal_clust_att <- cutree(tree = agnes_ward_mahal_att, k = 4)

# Silhouette Plot
fviz_silhouette(silhouette(mahal_clust_att, mahal_dist_att))

# Visualizing Cluster 4
df %>% mutate(cluster = mahal_clust_att) %>% filter(cluster == 4)

# Determining if Cluster 5 respondents are outliers using PCA
prcomp_att <- prcomp(df_cluster_att)
options(ggrepel.max.overlaps = Inf)

# PC1 vs PC2 plot
df_cluster %>%
  mutate(cluster = mahal_clust_att,
         q_code = df$q_code,
         tag = ifelse(cluster %in% c(4), q_code, NA)
  ) %>%
  bind_cols(as_tibble(prcomp_att$x)) %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(col = "firebrick") +
  ggrepel::geom_label_repel(aes(label = tag))

# Removing Cluster 4
df_cluster_att <- df_cluster_att %>% 
  mutate(clust = mahal_clust_att) %>% 
  filter(clust != 4) %>% 
  select(-clust)

# AGNES using Ward Linkage and Mahalanobis Distance without the outliers
mahal_dist_att <- mahal(df_cluster_att)
agnes_ward_mahal_att <- cluster::agnes(mahal_dist_att, diss = T, 
                                       stand = T, method = "ward") 
plot(agnes_ward_mahal_att, which.plots = 2)

NbClust(data = df_cluster_att, 
        diss = mahal_dist_att,
        distance = NULL,
        min.nc = 2, 
        max.nc = 15, 
        method = "ward.D", 
        index = "all")

#* Among all indices:                                                
#* 5 proposed 2 as the best number of clusters 
#* 4 proposed 3 as the best number of clusters 
#* 2 proposed 4 as the best number of clusters 
#* 2 proposed 6 as the best number of clusters 
#* 2 proposed 7 as the best number of clusters 
#* 3 proposed 8 as the best number of clusters 
#* 1 proposed 9 as the best number of clusters 
#* 1 proposed 10 as the best number of clusters 
#* 1 proposed 11 as the best number of clusters 
#* 1 proposed 14 as the best number of clusters 
#* 1 proposed 15 as the best number of clusters 
#* According to the majority rule, the best number of clusters is  2 

# Using 3 clusters
mahal_clust_att <- cutree(tree = agnes_ward_mahal_att, k = 3)
fviz_silhouette(silhouette(mahal_clust_att, mahal_dist_att))

# Number of negative silhoutte width mahal_clust solution
data_no_outliers1 <-  data_sil %>% filter(silhouette < 0)
(table(data_no_outliers1$clust))
sum((table(data_no_outliers1$clust)))

# Initial centers from AGNES (Mahalanobis, Ward's Linkage) k = 3
init_centers_att <- df_cluster_att %>%
  mutate_all(.funs = scale) %>%
  mutate(clust = mahal_clust_att) %>%
  group_by(clust) %>%
  summarise_all(.funs = mean) %>% select(-clust)

# KMeans from the initial centers
kmeans_att <- kmeans(x = scale(df_cluster_att), centers = init_centers_att)
table(kmeans_att$cluster)
att_cluster <- kmeans_att$cluster

# Silhouette Widths of K-Means solution, k = 3
example_sil_att <- silhouette(kmeans_att$cluster, mahal_dist_att)

# Number of negative silhoutte width K-Means solution
sil2 <- silhouette(kmeans_att$cluster, mahal_dist_att)
head(sil2) # first 6 rows
data_sil2 <- df_cluster_att %>%
  mutate(
    clust = kmeans_att$cluster, silhouette = sil2[,3]
  )
data_no_outliers2 <-  data_sil2 %>% filter(silhouette < 0)
(table(data_no_outliers2$clust))
sum((table(data_no_outliers2$clust)))

# Observations per cluster with negative silhouette
df_cluster_att %>%
  mutate(
    clust = kmeans_att$cluster,
    silhouette = example_sil_att[,3]
  ) %>% filter(silhouette < 0) %>% 
  group_by(clust) %>% 
  summarise(n = n())

# Silhouette Plot
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

# Comparison og Factor Score Distribution
att_comparison <- df_cluster_att %>% mutate(grouping = kmeans_att$cluster) %>% 
                          rbind(df_cluster_att %>% 
                                  mutate(grouping = 4)) %>% 
                          gather(key = "factor", value = "factor_score", 1:3) 
att_comparison$grouping <- factor(att_comparison$grouping, labels = c("1", "2", "3", "Overall"))
ggplot(att_comparison, aes(x = grouping, y = factor_score))+
  geom_boxplot()+
  facet_wrap(~factor)

#Comparison of 2 grouping solutions for attitude using CH and avg_sil width
kmeans_stats <- fpc::cluster.stats(d = mahal_dist_att, clustering =kmeans_att$cluster )
hclust_stats <- fpc::cluster.stats(d = mahal_dist_att, clustering = mahal_clust_att)
c("kmeans" = kmeans_stats$avg.silwidth, "hclust" = hclust_stats$avg.silwidth)
c("kmeans" = kmeans_stats$ch, "hclust" = hclust_stats$ch)

####################### Opinion Factors ############################
df_cluster_opp <- df %>% select(opinion_f1, opinion_f2, opinion_f3, opinion_f4)

# Hierarchical Clustering

# AGNES using Ward Linkage and Mahalanobis Distance
mahal_dist_opp <- mahal(df_cluster_opp)
agnes_ward_mahal_opp <- cluster::agnes(mahal_dist_opp, diss = T, stand = T, method = "ward")

# Dendogram Plot
plot(agnes_ward_mahal_opp, which.plots = 2)

# Determining Number of Clusters
NbClust(data = df_cluster_opp, 
        diss = mahal_dist_opp,
        distance = NULL,
        min.nc = 2, 
        max.nc = 15, 
        method = "ward.D", 
        index = "all")

#* Among all indices:                                                
#* 5 proposed 2 as the best number of clusters 
#* 2 proposed 3 as the best number of clusters 
#* 3 proposed 4 as the best number of clusters 
#* 8 proposed 5 as the best number of clusters 
#* 1 proposed 8 as the best number of clusters 
#* 1 proposed 13 as the best number of clusters 
#* 2 proposed 14 as the best number of clusters 
#* 1 proposed 15 as the best number of clusters 
#* According to the majority rule, the best number of clusters is  5 

# Using 5 clusters
mahal_clust_opp <- cutree(tree = agnes_ward_mahal_opp, k = 5)

# Silhouette Width
fviz_silhouette(silhouette(mahal_clust_opp, mahal_dist_opp))
# Number of silhouette width mahal-ward solution for opinion
sil3 <- silhouette(mahal_clust_opp, mahal_dist_opp)
head(sil3) # first 6 rows
data_sil3 <- df_cluster_opp %>%
  mutate(
    clust = mahal_clust_opp, silhouette = sil[,3]
  )
data_no_outliers3 <-  data_sil3 %>% filter(silhouette < 0)
(table(data_no_outliers3$clust))
sum((table(data_no_outliers3$clust)))


# Initial centers from AGNES (Mahalanobis, Ward's Linkage) k = 5
init_centers_opp <- df_cluster_opp %>%
  mutate_all(.funs = scale) %>%
  mutate(clust = mahal_clust_opp) %>%
  group_by(clust) %>%
  summarise_all(.funs = mean) %>% select(-clust) 

# KMeans from the initial centers
kmeans_opp <- kmeans(x = scale(df_cluster_opp), centers = init_centers_opp)
table(kmeans_opp$cluster)

# Silhouette Widths of K-Means solution, k = 5
example_sil_opp <- silhouette(kmeans_opp$cluster, mahal_dist_opp)

# Observations per cluster with negative silhouette
df_cluster_opp %>%
  mutate(
    clust = kmeans_opp$cluster,
    silhouette = example_sil_opp[,3]
  ) %>% filter(silhouette < 0) %>% 
  group_by(clust) %>% 
  summarise(n = n())

# Visualizing Average Silhouette Widths per Cluster
fviz_silhouette(example_sil_opp)


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

# Removing the respondents that are outliers wrt attitude factors
opp_cluster <- df_cluster_opp %>% 
  mutate(clust = kmeans_opp$cluster,
         q_code = df$q_code) %>% 
  filter(!q_code %in% c(102, 104, 105)) %>% select(clust) %>% 
  unlist() %>% as.numeric()

op_comparison <- df_cluster_opp %>% mutate(grouping = kmeans_opp$cluster) %>% 
  rbind(df_cluster_opp %>% 
          mutate(grouping = 6)) %>% 
  gather(key = "factor", value = "factor_score", 1:4) 
op_comparison$grouping <- factor(op_comparison$grouping, labels = c("1", "2", "3", "4", "5", "Overall"))
ggplot(op_comparison, aes(x = grouping, y = factor_score))+
  geom_boxplot()+
  facet_wrap(~factor)

#number of negative silhouette width for kmeans solution of opinion
sil4 <- silhouette(kmeans_opp$cluster, mahal_dist_att)
head(sil4) # first 6 rows
data_sil4 <- df_cluster_att %>%
  mutate(
    clust = kmeans_opp$cluster, silhouette = sil2[,3]
  )
data_no_outliers4 <-  data_sil4 %>% filter(silhouette < 0)
(table(data_no_outliers4$clust))
sum((table(data_no_outliers4$clust)))

#Comparison of the two solutions for opinion solution using CH and avergage silhouette width
kmeans_opp_stats <- fpc::cluster.stats(d = mahal_dist_opp, clustering =kmeans_opp$cluster )
hclust_opp_stats <- fpc::cluster.stats(d = mahal_dist_opp, clustering = mahal_clust_opp)
c("kmeans" = kmeans_opp_stats$avg.silwidth, "hclust" = hclust_opp_stats$avg.silwidth)
c("kmeans" = kmeans_opp_stats$ch, "hclust" = hclust_opp_stats$ch)


##################### Cluster Analysis of Attitude and Opinion Clusters ########
df_att_opp <- data.frame(opinion = factor(opp_cluster), attitude = factor(att_cluster))
str(df_att_opp)

# Hierarchical Clustering

# AGNES using Ward Linkage and Gower's Distance
gower.dist <- daisy(df_att_opp, metric = "gower")
agnes_ward_att_opp <- cluster::agnes(x = gower.dist, diss = T, method = "ward")
plot(agnes_ward_att_opp, which.plots = 2)

# Determining Number of Clusters
NbClust(diss = gower.dist,
        distance = NULL,
        min.nc = 2, 
        max.nc = 10, 
        method = "ward.D", 
        index = "silhouette") #10

NbClust(diss = gower.dist,
        distance = NULL,
        min.nc = 2, 
        max.nc = 10, 
        method = "ward.D", 
        index = "frey") #1

NbClust(diss = gower.dist,
        distance = NULL,
        min.nc = 2, 
        max.nc = 10, 
        method = "ward.D", 
        index = "mcclain") #2

NbClust(diss = gower.dist,
        distance = NULL,
        min.nc = 2, 
        max.nc = 10, 
        method = "ward.D", 
        index = "cindex") #10

NbClust(diss = gower.dist,
        distance = NULL,
        min.nc = 2, 
        max.nc = 10, 
        method = "ward.D", 
        index = "dunn") #3

cluster <- cutree(tree = agnes_ward_att_opp, k = 3)
example_sil_att_opp <- silhouette(cluster, gower.dist)
fviz_silhouette(example_sil_att_opp)

# Final data set containing sociodemographic variables and final cluster
final <- df %>% select(q_code, age, sex, occ_status, income, 
                       education, fam_w_jeep, w_driver_rel) %>% 
  filter(!q_code %in% c(102, 104, 105)) %>% 
  mutate(attitude = df_att_opp$attitude, 
         opinion = df_att_opp$opinion, 
         cluster = cluster)

table(final$cluster, final$attitude)
table(final$cluster, final$opinion)

# write.csv(final, "final_demog_cluster.csv")

##################### Demographic Profile ######################################
# Recoding of the Sociodemographic Variables
final <- final %>% mutate(age_group = ifelse(age>60, 4, 
                                    ifelse(age>43, 3, 
                                           ifelse(age>27, 2, 
                                                  ifelse(age>12, 1, 0)))),
                          educ_group = ifelse(education %in% c(1,2,3,4,5), 1, 
                                                ifelse(education %in% c(6,7), 2, 
                                                       ifelse(education %in% c(10), 3,
                                                              ifelse(education %in% c(11,12,13,14,15), 4, 5)))),
                          income_group = ifelse(income %in% c(-97, -98), 0,
                                                ifelse(income %in% c(-99), 1,
                                                       ifelse(income %in% c(1), 2, 
                                                              ifelse(income %in% c(2), 3, 
                                                                     ifelse(income %in% c(3), 4,
                                                                            ifelse(income %in% c(4), 5, 6)))))),
                          occ_group = ifelse(occ_status %in% c(4,5), 4, 
                                              ifelse(occ_status %in% c(6), 5, occ_status)))

# Distribution of Age Group
table(final$cluster, final$age_group)

# Stacked Bar Chart
final$age_group <- factor(final$age_group, levels = c("4","3","2","1","0"))
colors <- c("#800080","#DC143C","#FFD700","#1F497D","green","#FF4500")
ggplot((final %>% rename(category = age_group)), aes(x = cluster, fill = category, group = category)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Plot of Clusters by Age Group",
       x = "Cluster", y = "Count") +
  scale_fill_manual(values = colors,labels = c("Silent Generation/Baby Boomers", "Gen X", "Millennials","Gen Z"))


# Distribution of Sex
table(final$cluster, final$sex)
final$sex <- factor(final$sex)
tally <- final %>% select(cluster, sex) %>% 
  group_by(cluster, sex) %>% 
  tally() %>% ungroup()
tally <- data.table(
  cluster = tally$cluster,
  category = tally$sex,
  n = tally$n
)

# Stacked Bar Chart
tally[, percentage := n / sum(n) * 100, by = cluster] %>% group_by(cluster) %>%
  ggplot(aes(x = cluster, y = percentage, fill = category)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Stacked Bar Plot of Clusters by Sex",
       x = "Cluster", y = "Percentage") +
  scale_fill_manual(values = colors,labels = c("Female", 
                                               "Male")) +
  geom_text(aes(label = paste0(round(percentage,2),"%")), 
            position = position_stack(vjust = 0.5), size = 4)

# Distribution of Respondents Familiar with Modern Jeepney
table(final$cluster, final$fam_w_jeep)

final$fam_w_jeep <- factor(final$fam_w_jeep)
tally <- final %>% select(cluster, fam_w_jeep) %>% 
  group_by(cluster, fam_w_jeep) %>% 
  tally() %>% ungroup()

tally <- data.table(
  cluster = tally$cluster,
  category = tally$fam_w_jeep,
  n = tally$n
)

# Stacked Bar Chart
tally[, percentage := n / sum(n) * 100, by = cluster] %>% group_by(cluster) %>%
  ggplot(aes(x = cluster, y = percentage, fill = category)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Stacked Bar Plot of Clusters by Familiarity with Modern Jeep",
       x = "Cluster", y = "Percentage") +
  scale_fill_manual(values = colors,labels = c("Not Familiar", 
                                               "Familiar")) +
  geom_text(aes(label = paste0(round(percentage,2),"%")), 
            position = position_stack(vjust = 0.5), size = 4)

# Distribution of Respondents with Driver/Operator Relative
table(final$cluster, final$w_driver_rel)

final$w_driver_rel <- factor(final$w_driver_rel)
tally <- final %>% select(cluster, w_driver_rel) %>% 
  group_by(cluster, w_driver_rel) %>% 
  tally() %>% ungroup()

tally <- data.table(
  cluster = tally$cluster,
  category = tally$w_driver_rel,
  n = tally$n
)

# Stacked Bar Chart
tally[, percentage := n / sum(n) * 100, by = cluster] %>% group_by(cluster) %>%
  ggplot(aes(x = cluster, y = percentage, fill = category)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Stacked Bar Plot of Clusters by Having Jeepney Driver/Operator Relatives",
       x = "Cluster", y = "Percentage") +
  scale_fill_manual(values = colors,labels = c("Without Relative", 
                                               "With Relative")) +
  geom_text(aes(label = paste0(round(percentage,2),"%")), 
            position = position_stack(vjust = 0.5), size = 4)


# Distribution of Occupational Status
table(final$cluster, final$occ_group)

# Stacked Bar Chart
final$occ_group <- factor(final$occ_group, levels = c("1", "2", "3", "4","5"))
colors <- c("#800080","#DC143C","#FFD700","#1F497D","green","#FF4500")
ggplot((final %>% rename(category = occ_group)), aes(x = cluster, fill = category, group = category)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Plot of Clusters by Occupational Status",
       x = "Cluster", y = "Count") +
  scale_fill_manual(values = colors,labels = c("Employed", 
                                               "Self-Employed", 
                                               "Unemployed", 
                                               "Student/Working Student",
                                               "Retired"))

# Distribution of Educational Attainment
table(final$cluster, final$educ_group)

# Stacked Bar Chart
final$educ_group <- factor(final$educ_group, levels = c("1","2","3","4","5"))
colors <- c("#800080","#DC143C","#FFD700","#1F497D","green","#FF4500")
ggplot((final %>% rename(category = educ_group)), aes(x = cluster, fill = category, group = category)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Plot of Clusters by Highest Educational Attainment",
       x = "Cluster", y = "Count") +
  scale_fill_manual(values = colors,labels = c("Not A high school graduate",
                                               "High School Graduate",
                                               "College Level",
                                               "College Graduate",
                                               "Others"))

# Distribution of Income Group
table(final$cluster, final$income_group)

# Stacked Bar Chart
final$income_group <- factor(final$income_group, levels = c("0", "1", "2", "3", "4","5","6"))
colors <- c("#800080","#DC143C","#FFD700","#1F497D","green","#FF4500","#008080")
ggplot((final %>% rename(category = income_group)), aes(x = cluster, fill = category, group = category)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Plot of Clusters by Income Group",
       x = "Cluster", y = "Count") +
  scale_fill_manual(values = colors,labels = c("Don't Know/Refused to Answer",
                                               "Not Applicable",
                                               "Less than Php 10,000", 
                                               "Php 10,000 - Php 19,999", 
                                               "Php 20,000 - Php 39,999", 
                                               "Php 40,000 - Php 69,999",
                                               "Greater than Php 70,000"))


#Occupational Status
table(final$cluster, final$occ_group)

final$occ_group <- factor(final$occ_group)
tally <- final %>% select(cluster, occ_group) %>% 
  group_by(cluster, occ_group) %>% 
  tally() %>% ungroup()

tally <- data.table(
  cluster = tally$cluster,
  category = tally$occ_group,
  n = tally$n
)

# Stacked Bar Charts with Percentage Values

# Educational Attainment
final$educ_group <- factor(final$educ_group)
tally <- final %>% select(cluster, educ_group) %>% 
  group_by(cluster, educ_group) %>% 
  tally() %>% ungroup()

tally <- data.table(
  cluster = tally$cluster,
  category = tally$educ_group,
  n = tally$n
)

# Stacked
tally[, percentage := n / sum(n) * 100, by = cluster] %>% group_by(cluster) %>%
  ggplot(aes(x = cluster, y = percentage, fill = category)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Stacked Bar Plot of Clusters by Education Group",
       x = "Cluster", y = "Percentage") +
  scale_fill_manual(values = colors,labels = c("Not a High School Graduate", 
                                               "High School Graduate","College Level",
                                               "College Graduate","Others")) +
  geom_text(aes(label = paste0(round(percentage,2),"%")), 
            position = position_stack(vjust = 0.5), size = 4)

# Age Group
final$age_group <- factor(final$age_group)
tally <- final %>% select(cluster, age_group) %>% 
  group_by(cluster, age_group) %>% 
  tally() %>% ungroup()

tally <- data.table(
  cluster = tally$cluster,
  category = tally$age_group,
  n = tally$n
)

# Stacked
tally[, percentage := n / sum(n) * 100, by = cluster] %>% group_by(cluster) %>%
  ggplot(aes(x = cluster, y = percentage, fill = category)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Stacked Bar Plot of Clusters by Age Group",
       x = "Cluster", y = "Percentage") +
  scale_fill_manual(values = colors,labels = c("Silent Generation/Baby Boomers", 
                                               "Gen X","Millennials",
                                               "Gen Z")) +
  geom_text(aes(label = paste0(round(percentage,2),"%")), 
            position = position_stack(vjust = 0.5), size = 4)

#Income Group
final$income_group <- factor(final$income_group)
tally <- final %>% select(cluster, income_group) %>% 
  group_by(cluster, income_group) %>% 
  tally() %>% ungroup()

tally <- data.table(
  cluster = tally$cluster,
  category = tally$income_group,
  n = tally$n
)

# Stacked
tally[, percentage := n / sum(n) * 100, by = cluster] %>% group_by(cluster) %>%
  ggplot(aes(x = cluster, y = percentage, fill = category)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Stacked Bar Plot of Clusters by Income Group",
       x = "Cluster", y = "Percentage") +
  scale_fill_manual(values = colors,labels = c("Don't Know/Refused to Answer","Not Applicable", 
                                               "Less than Php 10,000","Php 10,000-Php 19,999",
                                               "Php 20,000 - Php 39,999","Php 40,000 - Php 69,999",
                                               "Greater than Php 70,000")) +
  geom_text(aes(label = paste0(round(percentage,2),"%")), 
            position = position_stack(vjust = 0.5), size = 4)

######################################################################################
