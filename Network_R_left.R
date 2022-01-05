#####FIRST STEP: IMPORT DATASET

#clean the environment
rm(list=ls()) 

#installing libraries
#install.packages('dplyr')
#install.packages('tidyr')
#install.packages('network')
#install.packages("igraph")
#install.packages('ergm')
#install.packages('integraph')
#install.packages('tidyverse')
#install.packages('dplyr')
#install.packages('RColorBrewer')

#importing libraries
library(tidyr)
library(RColorBrewer)
library(tidyverse)
library(dplyr)
library(igraph)
library(network)
library(ergm)
#library(intergraph)

#get directory
getwd()

#set the director where the file is
setwd("\\Users\\julia\\OneDrive - Högskolan Dalarna\\Thesis\\CowDistance-Project-main\\data\\2020-10-16_area all_not both in same area_left\\merged data")

#Create list of text files
list_files <- list.files(path = ".", recursive = TRUE,pattern = "\\.txt$",
                            full.names = TRUE)

#Read the files in, assuming comma separator
txt_files_df <- lapply(list_files, function(x) {read.table(file = x, header = T, sep =",")})

## Combine everything in one dataframe
df <- do.call("rbind", lapply(txt_files_df, as.data.frame)) 

##OPTIONAL: SAVE THE IMPORTED DATASET
#save the combined dataframe into a txt file
#write.table(df, file = "2020-10-16_all_merged data_combined.txt", sep = "\t",row.names = FALSE)

###################################
###SECOND STEP: READ THE DATAFRAME, APPLY FILTERS, SUM TOTAL TIME

#Read in the combined file
#df = read.table(file="2020-10-16_all_both_mergeddata_0.5m.txt" , header=TRUE, sep=",")

#check if the dataframe was created correctly
view(df)

head(df)

summary(df)

#check length (should be = 638561 rows)
nrow(df)

#check how many unique tag id are in the df
length(unique(df$tag_id_x))
length(unique(df$tag_id_y))

#create a dataframe to check the tags
#check_tags <- df[, c('tag_id_x','tag_string_x')]
#save this dataframe
#write.table(check_tags, file = "check_tags_FA file.txt", sep = "\t",row.names = FALSE)

#check unique tags
#check_tags <- unique(df[, c('tag_id_x','tag_string_x')])

#verify those tag_strings that are missing
#na <- check_tags[rowSums(is.na(check_tags)) > 0, ]  
#nrow(na)

#include parity attribute in the dataframe (lactation number)
setwd("\\Users\\julia\\OneDrive - Högskolan Dalarna\\Thesis\\CowDistance-Project-main\\data")
lactation <- read.csv(file = 'CowInfo20201016_lactation number.csv')

#check the information on the file
head(lactation)

nrow(lactation)

#create a dataframe with only parity and tag_string
parity <- lactation[,c('tag', 'parity')]
head(parity)
nrow(parity)

#check which is the most commom parity (run df_with_parity first)
freq_parity <- table(as.numeric(df2$parity))
freq_parity

barplot(freq_parity,
        main = "Parity Frequency - Left Side",
        xlab = "Parity Number",
        ylab = "Count of Cows")

#change parity bigger than 4 and group into one group
#parity["parity"][parity['parity'] == 4] <- '3+'
#parity["parity"][parity['parity'] == 5] <- '5'
#parity["parity"][parity['parity'] == 6] <- '5'
#parity["parity"][parity['parity'] == 7] <- '5'

#rename column tag to match with nodes main dataframe (df)
names(parity)[1] <- 'cow_string'
head(parity)

#create a dataframe with unique tags from main df to match lactation
#create list with unique tags
from <- df %>%
  distinct(tag_string_x) %>%
  rename(cow_string = tag_string_x)


to <- df %>%
  distinct(tag_string_y) %>%
  rename(cow_string = tag_string_y)

unique_tags <- full_join(from, to, by = "cow_string")

#drop any missing tag_strings
unique_tags <- drop_na(unique_tags)

head(unique_tags)
nrow(unique_tags)

#join lactation number
df2 <- parity %>%
  select(cow_string, parity) %>%
  distinct() %>%
  right_join(unique_tags, by = 'cow_string')

#check if parity was added
head(df2)
nrow(df2)

#check those who are missing parity
na_parity <- df2[rowSums(is.na(df2)) > 0, ]  
nrow(na_parity)
na_parity

#drop NA
df2 <- drop_na(df2)
#check how many individuals have the lactation number
nrow(df2)
#see those individuals with lactation number
View(df2)

#create a list of all tags that have parity
#list_with_parity <- list(df2$cow_string)
#list_with_parity

list_with_parity <- c("00250C8C", "00250DE6", "00250CA4", "0024FA49", 
               "0024F420", "00250F6A", "00250FDD","00250ACF", 
               "00251EA2", "00250D13", "00250D83", "00250C98",
               "00250B51", "00250ADE", "00250F65", "00250AE0", 
               "00250BEB", "00250B59", "00251C77", "00250F68", 
               "00251BC0", "00250FDE", "0025206C", "00250AAA",
               "00250D96", "00250F28", "00251C98", "00251EA8",
               "0024FA3C", "002505B2", "00250F2A", "0024FA4F", 
               "00250A35", "00250F58", "00251C8B", "00250C9E",
               "00250AAD", "00250CB0", "0025205F", "0024E22B", 
               "00250A42", "00250FBF", "00250F3E", "0024FA54", 
               "00250C01", "00250FDB", "0024FA52", "00250D10",
               "0024FA44", "0024F405", "00250DEE", "00250F7C", 
               "0024FA61", "00250F25", "00250FCD", "00251BD2", 
               "0024FA30", "00250DBC", "00250CBB", "00250D9B",
               "00250DE2", "00250FE6", "00250A49", "00251C30", 
               "00250CBE", "00250DA3", "00251C76", "0024F437", 
               "00250AD8", "00251C59", "0024EE68", "00250BE6",
               "00250F27", "00251C17", "00250F43", "00250FC3", 
               "0024EDC3", "00251C62", "00250B4E", "00250D76", 
               "00250FE8", "00250F77", "00250D95", "00250F3C",
               "00250CDD", "00250B50", "0024F40D")
  
individuals_to_exclude <- c("0024F420", "0024FA3C", "00250CDD", 
                            "00250FE6", "00251C30", "00251C76",
                            "00251C8B", "00251EA8")

#exclude those individuals separated from the group
final_list <- list_with_parity[!(list_with_parity %in% individuals_to_exclude)]

#create a subset of main df with only the nodes that have parity
df_with_parity <- subset(df,tag_string_x %in% final_list & tag_string_y %in% final_list)

df_with_parity <- drop_na(df_with_parity)
head(df_with_parity)
nrow(df_with_parity)
View(df_with_parity)

#check how many unique tags, should be 78
length(unique(df_with_parity$tag_id_y))
length(unique(df_with_parity$tag_id_x))

#create a list of those tag_string that does not have info on parity
#list = c("0021F512","0021F57E","0021F5B4","0021F5F0","00220D4F","0024F3C9","0024F48C")

#list_no_parity = list(na_parity$cow_string)

#create subset of main df excluding these tag_strings
#new_df <- df_unique[!(df_unique$tag_string_x== list | df_unique$tag_string_y== list),]
#new_df  <- drop_na(new_df)
#nrow(new_df)

#check if the tags that does not have parity are i the dataframe
#list %in% new_df
#list %in% df_with_parity

#filter the dataframe with columns we will use
df4 <- df_with_parity[, c('tag_string_x','tag_string_y', 'total_time', 'aggregated_distance')]
head(df4)
nrow(df4)
#df4 <- drop_na(df4)
#nrow(df4)
#View(df4)

#check NAs in parity in the new dataframe, after merging
#is.na(df2)
#na <- df2[rowSums(is.na(df2)) > 0, ]  
#nrow(na)

#Pick observations less than 250 cm apart
#less_250 <- df2$aggregated < 250
#df3 <- df2[less_250,]

#Pick observations less than 1.25 cm apart
#df3 <- df2[, c('tag_string_x','tag_string_y', 'total_time', 'aggregated_distance')]
less_125 <- df4$aggregated < 125
df5 <- df4[less_125,]

#check the aggregated_distance columns
head(df5)

#check length 
print(nrow(df5))

length(unique(df5$tag_string_x))
length(unique(df5$tag_string_y))

#sum up the total time they spend together
#is.na(df3$tag_string_y)
df_aggregated_time_distance <- df5 %>%  
    group_by(tag_string_x, tag_string_y) %>%
    summarise_each(funs = (sum)) %>% 
    ungroup()

#df_aggregated_time_distance <- df3 %>%  
#group_by(tag_id_x, tag_id_y) %>%
 # across(funs = (sum)) %>% 
  #ungroup()

head(df_aggregated_time_distance)
nrow(df_aggregated_time_distance)

#check the variable '2225426'
#variable <- '2225426'
#subset(df3, tag_id_x %in% variable)

#drop NAs form parity
#df_aggregated_time_distance <- drop_na(df_aggregated_time_distance)
#nrow(df_aggregated_time_distance)
#head(df_aggregated_time_distance)

#Subset : by time
#10min
#sub_10 <- subset(df_aggregated_time_distance, 
#              total_time > 600)
#head(sub_10)
#nrow(sub_10)

#30min

sub_30 <- subset(df_aggregated_time_distance, 
              total_time > 1800)

head(sub_30)
nrow(sub_30)
#View(sub_30)

length(unique(sub_30$tag_string_x))
length(unique(sub_30$tag_string_y))

#check if there are na values in this dataset
#is.na(sub_30)
#x <- sub_30[rowSums(is.na(sub_30)) > 0, ]  
#nrow(x)

### THIRD STEP: BUILD AND VISUALIZE THE NETWORK

#create a dataframe with only the edges with weights
#edges_10 <- data.frame(sub_10$tag_id_x, sub_10$tag_id_y, sub_10$total_time)

edges_30 <- data.frame(sub_30$tag_string_x, sub_30$tag_string_y, sub_30$total_time)
head(edges_30)
nrow(edges_30)

#consider edge weight: time they spend together
#consider normalize???
#min(sub_10$total_time)
#max(sub_10$total_time)

#create node list

from <- sub_30 %>%
    distinct(tag_string_x) %>%
    rename(tag_string = tag_string_x)

to <- sub_30 %>%
    distinct(tag_string_y) %>%
    rename(tag_string = tag_string_y)

nodes <- full_join(from, to, by = "tag_string")
#nodes <- nodes %>% rowid_to_column("id")
head(nodes)
nrow(nodes)

#check if there are still nodes without parity
is.na(nodes)
#View(nodes)


#include tag_string in nodes dataframe
#nodes$tag_string <- with(df,tag_string_y[match(nodes$cow_id,tag_string_y)])
#head(nodes)

#try to add those parities that are missing
#rename column tag to match with nodes main dataframe (df)
names(parity)[1] <- 'tag_string'
head(parity)

#include a column for parity for tag_id_x
nodes_with_parity <- parity %>%
  select(tag_string, parity) %>%
  distinct() %>%
  right_join(nodes, by = 'tag_string')

#check the most commom parity
#check which is the most commom parity (run df_with_parity first)
freq_parity <- table(as.numeric(nodes_with_parity$parity))
freq_parity

barplot(freq_parity, 
        main = NULL,
        xlab = "Parity Number",
        ylab = "Number of Cows")

#change parity bigger than 3 and group into one group
#parity["parity"][parity['parity'] == 4] <- '4'
parity["parity"][parity['parity'] == 5] <- '4'
parity["parity"][parity['parity'] == 6] <- '4'
parity["parity"][parity['parity'] == 7] <- '4'


#rerun parity for the nodes (parity >3 grouped into number 4)
nodes_with_parity <- parity %>%
  select(tag_string, parity) %>%
  distinct() %>%
  right_join(nodes, by = 'tag_string')

freq_parity2 <- table(as.numeric(nodes_with_parity$parity))
freq_parity2

barplot(freq_parity2, 
        main = "Frequency of Parity - Left Side",
        xlab = "Parity Number",
        ylab = "Number of Cows")

#Change the parity for all those greater than 3, indentify as one group
#nodes_with_parity["parity"][nodes_with_parity['parity'] == 4] <- '>3'
#nodes_with_parity["parity"][nodes_with_parity['parity'] == 5] <- '>3'
#nodes_with_parity["parity"][nodes_with_parity['parity'] == 6] <- '>3'
#nodes_with_parity["parity"][nodes_with_parity['parity'] == 7] <- '>3'

#add index number
nodes_with_parity$ID <- seq.int(nrow(nodes_with_parity))

#check
View(nodes_with_parity)

#check if it has found all values
#is.na(nodes$parity)

#verify who are those tag_string couldn't be found
#na <- nodes[rowSums(is.na(nodes)) > 0, ]  
#View(na)

#omit NAs from node list
#nodes <- na.omit(nodes)
#nrow(nodes)



##### NETWORK #######

##CREATE A NETWORK OBJECT##
#net_10 <- graph.data.frame(edges_10[,1:2], directed = F, vertices = nodes)
#summary(net_10)

net_30 <- graph.data.frame(edges_30[,1:2], directed = F, vertices = nodes_with_parity)
summary(net_30)


#graph_with_parity <- graph.data.frame(new_edges[,1:2], directed = F, vertices = nodes_with_parity)
#summary(net_30)


#edges_with_weights 
#edge.attributes(net_10)$weight <- edges_10[,3]
edge.attributes(net_30)$weight <- edges_30[,3]

#check the edges weights
#edge.attributes(net_10)$weight 
edge.attributes(net_30)$weight

#3. Diversity
diversity(net_30, weights = NULL, vids = V(net_30))


#set node attributes
#set_vertex_attr(net_30, "parity", index = V(net_30), parity)



#check how many unique nodes are there (104)
#use V(net) will show the total number in the beggining
#gorder(net_10)
gorder(net_30)

#check how many edges (2904 for 250cm)
#gsize(net_10)
gsize(net_30)

#number of degreees for each node
#V(net_10)$degree <- degree(net_10)
V(net_30)$degree <- degree(net_30)


#set the name of each node to be their ID
#V(net_10)$label <- V(net_10)$name
V(net_30)$name <- V(net_30)$ID


#create a dataframe to visualize the degrees per node
#degree_frame_10 <- data.frame(V(net_10)$label,V(net_10)$degree)
#head(degree_frame_10)
degree_frame_30 <- data.frame(V(net_30)$ID,V(net_30)$degree)
head(degree_frame_30)


#histogram of node degree
par(mfrow = c(1,2))

fig.dim = c(5, 3)
hist(V(net_30)$degree,
     col = 'blue',
     main = NULL,
     ylab = 'N_Cows',
     xlab = 'N_Connections',
     cex.lab = 0.8,
     cex.main=0.8,col.main="black"
     )

boxplot(V(net_30)$degree,
        main = NULL,
        cex.main=1,col.main="black")


#histogram of the time each pair spend together
hist(sub_30$total_time,
     col = 'blue',
     main = NULL,
     ylab = 'N_Connections',
     xlab = 'Total number of seconds',
     cex.lab = 0.8,
     cex.main=0.8,col.main="black"
)

#before making the cut of 30min
hist(df_aggregated_time_distance$total_time,
     col = 'blue',
     main = 'Figure 4: Distribution of total time - Left',
     ylab = 'N_Connections',
     xlab = 'Total number of seconds',
     cex.lab = 0.8,
     cex.main=0.8,col.main="black"
)


boxplot(sub_30$total_time,
        main = 'Figure 4: Total Time (n_seconds)',
        cex.main=1,col.main="black")



###########CENTRALITY MEASURES###################

#1. degree centrality
#degree_centrality_10 <- degree(net_10, mode = c('All'))
#V(net_10)$degree <- degree_centrality_10
#V(net_10)$degree

degree_centrality_30 <- degree(net_30, mode = c('All'))
V(net_30)$degree <- degree_centrality_30



#get the which node have the highest degree centrality
#which.max(degree_centrality_10) #it will show the node and the index
which.max(degree_centrality_30)

#2. eigenvector centrality
#the scores here should be between 0-1
#eigen_centrality_10 <- evcent(net_10)$vector
#V(net_10)$Eigen <- eigen_centrality_10

eigen_centrality_30 <- evcent(net_30)$vector
V(net_30)$Eigen <- eigen_centrality_30

#get the which node have the highest degree centrality
#which.max(eigen_centrality_10) #it will show the node and the index
which.max(eigen_centrality_30)


#3. betweeness centrality
#betweeness_centrality_10 <- betweenness(net_10, directed = F)
#V(net_10)$betweeness <- betweeness_centrality_10
#V(net_10)$betweeness
#which.max(betweeness_centrality_10)

betweeness_centrality_30 <- betweenness(net_30, directed = F)
V(net_30)$betweeness <- betweeness_centrality_30
V(net_30)$betweeness
which.max(betweeness_centrality_30)

#4. Other measures
#average distance everone could be reached in terms of path lines
#mean_distance(net_10, directed = FALSE)
mean_distance(net_30, directed = FALSE)

#largest distance, in this case is weighted by the total number
#of seconds they spend together
#diameter(net_10, directed = FALSE, weights = edge.attributes(net_10)$weight )
diameter(net_30, directed = FALSE, weights = edge.attributes(net_30)$weight )

#The diameter of a graph is the length of the longest geodesic.
#geodesic:a path with the minimal number of vertices.
diameter(net_30, directed = F)

#returns a path with the actual diameter. 
#If there are many shortest paths of the length of the diameter, 
#then it returns the first one found.
get.diameter(net_30)$ID

#centralization, how much is centralized around one node
#centr_degree(net_10, mode = 'in', normalized = T)
centr_degree(net_30, mode = 'in', normalized = T)

#edge density: of all possible ties how many are present
#number of actualy ties / number of all possible ties
#edge_density(net_10, loops = F)
edge_density(net_30, loops = F)

#mean degree
#mean(degree(net_10))
mean(degree(net_30))

#clique - set of nodes where they are all tied to each other
cliques(net_30)
sapply(cliques(net_30),length)
list_largest_cliques <- largest.cliques(net_30)
list_largest_cliques

clique_number <- sapply(list_largest_cliques, length)
clique_number

clique_members <- sapply(list_largest_cliques, function(x) paste(attributes(x)$name, collapse = ",", sep ="/" ))
str(clique_members)

cliques_df <- data.frame(cliqueNums = clique_number, cliqueMembs = as.factor(clique_members))
cliques_df



#vcol <- rep('grey80',vcount(net_30))
#vcol[unlist(largest.cliques(net_30))] <- 'gold'
#plot(net_10,
 #    vertex.label = NA,
  #   vertex.col = vcol)

#closeness
closeness(net_30)

#finds the node that connects all others, if this one is removed
#then the network can be broken in different groups
articulation_points(net_30)

#new <- delete_vertices(net_30, v = '2421705')

#plot(new,
 #    vertex.color = rainbow(104),
  #   vertex.label = NA,
   #  vertex.size = V(net_10)$degree/3,
    # edge.width = sqrt(E(net_10)$weight/1000),
    # edge.arrow.size = 0.1,
     #layout = layout.fruchterman.reingold,
     #margin = -0.1)

#components(net_30)
#component_list <- decompose.graph(net_30, mode= 'weak')
#component_list

#convert the igraph object into a dataframe
#igraph_df_10 <- as_long_data_frame(net_10)
#view(igraph_df_10)

igraph_df_30 <- as_long_data_frame(net_30)
view(igraph_df_30)
nrow(igraph_df_30)

#length(unique(igraph_df_30$from_name))
#length(unique(igraph_df_30$to_name))

#length(igraph_df_30$from_name)
#length(igraph_df_30$to_name)

##OPTIONAL: SAVE DATAFRAME
save(igraph_df_30,file="network_1.25m_30min_left.Rda")
#load
#load("network_1.25m_30min.Rda")


###############NETWORK STRUCTURE MEASURES##################

#1. Network Density
#global density (score between 0 and 1)
#edge_density(net_10) 
edge_density(net_30) 

#2. Assortativity (if there are nodes attributes)
#possible to see if an individual with same characteristics tends to 
#group together
#(The assortativity coefficient quantifies the extent to which 
#connected nodes share similar properties. 
#It is analogous to the Pearson correlation coefficient but 
#measures the correlation between every pair of nodes that are 
#connected.)

#will indicate the level of affiliation among the cows with same parity
#assortativity_nominal(net_30, V(net_30)$parity, directed = F)
assortativity.degree(net_30, directed = F)
assortativity(net_30, V(net_30)$parity, directed = F)

#continuous varialbes
#assortativity(net_30, V(net_30)$parity)

######################NETWORK VISUALIZATION####################

#set seed for replicability purposes
set.seed(2021)

###1. Plot with degree centrality
par(mfrow = c(1,2))

#laygout: Fruchterman.reingold 
plot(net_10,
     #edge.color = 'black',
     vertex.color = rainbow(104),
     vertex.label = NA,
     vertex.size = V(net_10)$degree/1.8,
     edge.width = sqrt(E(net_10)$weight/1000),
     edge.arrow.size = 0.1,
     layout = layout.fruchterman.reingold,
     margin = -0.3) 
title("Social Network for 1.25 distance 
      and at least 10min interaction",
      cex.main=0.8,col.main="blue")


plot(net_30,
     edge.color = 'gray',
     vertex.color = rainbow(7),
     vertex.label = NA,
     vertex.size = V(net_30)$degree/1.8,
     edge.width = 0.1, #sqrt(E(net_30)$weight/500),
     edge.arrow.size = 0.5,
     layout = layout.fruchterman.reingold,
     margin = -0.1) 
title("Social Network for 1.25 distance and 
      at least 30min interaction",
      cex.main=0.5,col.main="black")

#layout : graphopt
plot(net_10,
     #edge.color = 'black',
     vertex.color = rainbow(104),
     vertex.label = NA,
     vertex.size = V(net_10)$degree/3,
     edge.width = sqrt(E(net_10)$weight/1000),
     edge.arrow.size = 0.1,
     layout = layout.graphopt)  
title("Social Network for 1.25 distance and at least 10min interaction",cex.main=0.8,col.main="blue")


plot(net_30,
     #edge.color = 'black',
     vertex.color = rainbow(104),
     vertex.label = NA,
     vertex.size = V(net_30)$degree/1.5,
     edge.width = sqrt(E(net_30)$weight/1000),
     edge.arrow.size = 0.1,
     layout = layout.graphopt)  
title("Social Network for 1.25 distance and at least 30min interaction",cex.main=0.8,col.main="blue")


#layout : kamada.kawai
plot(net_30,
     edge.color = 'gray',
     vertex.color = V(net_30)$parity,
     vertex.label = V(net_30)$parity,
     vertex.label.cex = 0.9,
     vertex.size = V(net_30)$degree/0.8,
     edge.width = 0.1, #sqrt(E(net_30)$weight/500),
     edge.arrow.size = 0.5,
     layout = layout.kamada.kawai,
     margin = -0.3)
#title("Social Network for 1.25 distance and 
 #     at least 30min interaction",cex.main=0.5,col.main="black",
  #    line = -18)

###2. Plot with eigenvector centrality

#layout : kamada.kawai
plot(net_10,
     #edge.color = 'black',
     vertex.color = rainbow(104),
     vertex.label = NA,
     vertex.size = V(net_10)$Eigen*10,
     edge.width = sqrt(E(net_10)$weight/1000),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai,
     margin = -0.3)
title("Social Network for 1.25 distance and 
      at least 10min interaction",
      cex.main=0.6,col.main="blue")

plot(net_30,
     #edge.color = 'black',
     vertex.color = V(net_30)$parity,
     vertex.label = V(net_30)$parity,
     vertex.size = V(net_30)$Eigen*10,
     edge.width = sqrt(E(net_30)$weight/1000),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai,
     margin = -0.2)
title("Social Network for 1.25 distance and 
      at least 30min interaction",
      cex.main=0.6,col.main="blue")


###2. Plot with betweeness centrality

#layout : kamada.kawai
plot(net_10,
     #edge.color = 'black',
     vertex.color = rainbow(104),
     vertex.label = NA,
     vertex.size = V(net_10)$betweeness/10,
     edge.width = sqrt(E(net_10)$weight/1000),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai,
     margin = -0.3)
title("Social Network for 1.25 distance and 
      at least 10min interaction",
      cex.main=0.6,col.main="blue")

plot(net_30,
     edge.color = 'gray',
     vertex.color = rainbow(104),
     vertex.label = V(net_30)$parity,
     vertex.size = V(net_30)$betweeness,
     edge.width = sqrt(E(net_30)$weight/1000),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai,
     margin = -0.2)
title("Social Network for 1.25 distance and 
      at least 30min interaction",
      cex.main=0.6,col.main="blue")

#layout : layout_with_mds
l <- layout_with_mds(net_30)
plot(net_30,
     #edge.color = 'black',
     vertex.color = rainbow(104),
     vertex.label = NA,
     vertex.size = V(net_30)$degree,
     edge.width = sqrt(E(net_30)$weight/1000),
     edge.arrow.size = 0.1,
     layout = l,
     margin = -0.2)
title("Social Network for 1.25 distance and 
      at least 30min interaction",
      cex.main=0.6,col.main="blue")

#############COMMUNITY DETECTION##################################

#1. Louvain clustering
#louvain_10 <- cluster_louvain(net_10)

louvain_30 <- cluster_louvain(net_30, weights = net_30$weight)
louvain_30 <- cluster_louvain(net_30)

#get the number of communities
length(communities(louvain_30))

#modularity
modularity(louvain_30)

#plot(louvain_10,
 #    net_10,
     #vertex.color = rainbow(104),
  #   vertex.label = NA,
   #  vertex.size = V(net_10)$degree/4,
    # edge.color = 'black',
     #edge.width = sqrt(E(net_10)$weight/1000),
     #edge.arrow.size = 0.1,
     #layout = layout.kamada.kawai,
     #margin = -0.2)
#title("Communities for 1.25 distance and 
 #     at least 10min interaction",
  #    cex.main=0.6,col.main="black")

#plot(louvain_30,
 #    net_30,
     #vertex.color = net_30$community,
     #vertex.frame = 'white',
     #vertex.label = NA,
     #vertex.size = V(net_30)$degree/2,
     #edge.color = 'black',
     #edge.width = sqrt(E(net_30)$weight/1000),
     #edge.arrow.size = 0.01,
  #   layout = layout.kamada.kawai,
   #  margin = -0.2)
#title("Communities for 1.25 distance and 
 #     at least 30min interaction",
  #    cex.main=0.6,col.main="black")


#assign the communities to graph
net_30$community <- louvain_30$membership
net_30$community
V(net_30)$community <- louvain_30$membership
V(net_30)$community


#check number of communities
unique(net_30$community)

#create a dataframe with all cows and the respective communities
V(net_30)$community <- net_30$community
V(net_30)$label <- V(net_30)$ID

cows_community <- data.frame(V(net_30)$ID,V(net_30)$community)

View(cows_community)


communities <- data.frame()

for (i in unique(net_30$community)) {
  # create subgraphs for each community
  subgraph <- induced_subgraph(net_30, v = which(net_30$community == i))
  # get size of each subgraph
  size <- igraph::gorder(subgraph)
  # get betweenness centrality
  btwn <-  igraph::betweenness(subgraph, directed = F)
  communities <- communities %>% 
    dplyr::bind_rows(
      data.frame(community = i,
                 n_cows = size,
                 most_important = names(which.max(btwn)))
    )
}

#summarize number of cows by community and the most important 
summary_communities <- communities %>%
  group_by(community) %>%
  summarise(n_cows = n_cows, most_important = most_important)

summary_communities
plot(summary_communities)

View(communities)

#knitr::kable(communities %>% 
#               dplyr::select(community, n_cows, most_important))


#include lactation number and ID for the most important nodes 
#first rename column tag to match 
names(nodes_with_parity)[1] <- 'most_important'
head(nodes_with_parity)

#include a column for parity for tag_id_x
most_important <- c("00250C8C","00250FBF","00250B59","0024FA54",
                         "0024F437","00250ACF","0024FA52","0024F40D")

#create a dataframe with the most important individuals by tag
df_most_important <- data.frame(most_important)
df_most_important

#join parity to the list of most important individuals by tag
most_important_with_parity <- nodes_with_parity %>%
  select(most_important, parity, ID) %>%
  distinct() %>%
  right_join(summary_communities, by = 'most_important')

#print result of most important with parity, Id and community
most_important_with_parity


# also color edges according to their starting node
#edge.start <- ends(net_30, es = E(net_30), names = F)[,1]
#E(net_30)$color <- V(net_30)$color[edge.start]
#E(net_30)$arrow.mode <- 0

# only label central characters
#v_labels <- which(V(net_30)$name %in% communities$most_important)

#for (i in 1:length(V(net_30))) {
 # if (!(i %in% v_labels)) {
  #  V(net_30)$label[i] <- ""
#  }
#}

#plot communities

V(net_30)$size <- V(net_30)$degree
vertex.size = V(net_30)$degree*0.5
V(net_30)$frame.color <- "white"
V(net_30)$color <- net_30$community
#V(net_30)$label <- net_30$community
#V(net_30)$label.cex <- 0.8

l2 <- layout_with_mds(net_30)
plot(net_30, rescale = T, layout = l2, main = NULL)
legend("topright",legend=net_30$community,pch=21)


#l2 <- layout_with_sugiyama(net_30)
#plot(net_30, rescale = T, layout = l2, main = "'testing")


##2. Infomap 
infomap_10 <- cluster_infomap(net_10)
infomap_30 <- cluster_infomap(net_30)

length(communities(infomap_10))

length(communities(infomap_30))

plot(infomap_10,
     net_10,
     vertex.color = rainbow(104),
     vertex.label = NA,
     vertex.size = V(net_10)$degree/4,
     edge.arrow.size = 0.1,
     margin = -0.1
     )


plot(infomap_30,
     net_30,
     vertex.color = rainbow(11),
     vertex.label = NA,
     vertex.size = 8,
     edge.color = 'gray',
     #edge.width = sqrt(E(net_30)$weight/1000),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai,
     margin = -0.2)
title("Communities for 1.25 distance and 
      at least 30min interaction",
      cex.main=0.6,col.main="black")

#calculating modularity
modularity(louvain_30, membership, weights = net_30$weight)


########continue here

#hub and authorities
hs <- hub_score(net_30)$vector
as <- authority.score(net_30)$vector

par(mfrow = c(1,2))

set.seed(0910)

plot(net_30,
     vertex.size = hs*30,
     main = 'Hubs',
     vertex.color = hs,
     vertex.label = NA,
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)

plot(net_30,
     vertex.size = as*30,
     main = 'authorities',
     vertex.color = rainbow(104),
     vertex.label = NA,
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)


###RANDOM NETWORKS#####
#Generate random graphs according to the Erdos-Renyi model
#using G(n,m) graphs
#the graph has 'n' vertices and 'm' edges
#'m' edges are chosen uniformly randomly from the set of all possible edges. 
#This set includes loop edges as well if the loops parameter is TRUE.

#simulate 1,000 random networks
#calculate the diameter and the clustering coefficient of each of them
#save the results into two vectors c and r.

c=numeric(1000)
r=numeric(1000)
degree = numeric(1000)
density = numeric(1000)
centralization = numeric(1000)
distance_path = numeric(1000)
diameter = numeric(1000)
communities = numeric(1000)
assortativity = numeric(1000)
cliques = numeric(1000)
transitivity = numeric(1000)
for (i in 1:1000) {
  random_network=erdos.renyi.game(vcount(net_30), ecount(net_30), type="gnm")
  #r[i]=diameter(random_network)
  #c[i]=transitivity(random_network, type="global")
  degree[i] = degree(random_network)
  density[i] = edge_density(random_network)
  centralization[i]= centr_degree(random_network, mode = 'in', normalized = T)$centralization
  distance_path[i]= mean_distance(random_network, directed = FALSE)
  diameter[i] = diameter(random_network, directed = F)
  communities[i] = length(cluster_louvain(random_network))
  assortativity[i] = assortativity_degree(random_network)
  cliques[i] = length(largest.cliques(random_network))
  transitivity[i] = transitivity(random_network)
}
mean(degree)
mean(density)
mean(centralization)
mean(distance_path)
mean(diameter)
mean(as.numeric(communities))
mean(assortativity)
mean(cliques)
mean(transitivity)


#random <- erdos.renyi.game(104, 438, type= "gnm", directed = FALSE)
#degree_distribution(random)


##### ERGM #######

#transforming the igraph object into network object
install.packages('intergraph')
library(intergraph)
install.packages('lpSolve')

# Creating the new network
new_net <- intergraph::asNetwork(net_30)

# Running a simple ergm (only fitting edge count)
ergm(new_net ~ edges)

model1 <- ergm(new_net ~ edges)
summary(model1)

model2 <- ergm(new_net ~ edges + nodematch("parity"))
summary(model2)

model3 <- ergm(new_net ~ edges + nodefactor("parity"))
summary(model3)

model4 <- ergm(new_net ~ edges + sociality('parity', base = 1, nodes = NULL))
summary(model4)
?sociality 

model5 <- ergm(new_net ~ edges + nodemix('parity', base = NULL))
summary(model5)



#make the analysis for cliques adding parity 


blocks <- cohesive.blocks(net_30)
blocks(blocks)
cohesion(blocks)
plotHierarchy(blocks)


##OTHER COMMUNITY DETECTION ALGORITHMS
communityMulti <- multilevel.community(net_30) 
length(communityMulti)

optimal <- optimal.community(net_30)
length(optimal)


Infomap <- infomap.community(net_30)
length(Infomap)

walktrap <- walktrap.community(net_30)
length(walktrap)

edge_betweeness <- edge.betweenness.community(net_30)
length(edge_betweeness)

label_propgation <- label.propagation.community(net_30)
length(label_propgation)

leading_eigenvector <- leading.eigenvector.community(net_30)
length(leading_eigenvector)

#make a graph correlation between degree centrality and parity

transitivity(net_30, type = "global")
#take the transitivity of the nodes that are first infected
#get the neighbours of Cow 31
net_30 %>% neighbors('31', mode = "out")

#subgroup cohesion
cohesion(net_30)
graph.cohesion(net_30)

#ERGM

#Extra material

#plot parity frequency (both left and right)
#first create a dataframe with both info, then make a plot