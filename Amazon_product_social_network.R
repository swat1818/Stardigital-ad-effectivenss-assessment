# Create product social network graphs & visualize major sub components in the network . 
# Identify key products that influence purchase behvior the most
# In this project we use 2 data sets - products, copurchase - to plot the network and calculate key metrics
# calculate key metrics like - Degree measures, centrality measures, authority scores, neighbors mean rating, neighbors sales rank etc

library(igraph)
library(dplyr)
library(sqldf)

products <- read.csv("products.csv", header=T)
copurchase <- read.csv("copurchase.csv", header=T)

###delete products that are not "books" Set a boundary
products1 <- subset(products, products$group =="Book" & products$salesrank < 150000 & products$salesrank != -1)
copurchase1 <- subset(copurchase, copurchase$Source %in% products1$id & copurchase$Target %in% products1$id)

g <- graph.data.frame(copurchase1, directed = T)

###2,3
in_degree <- degree(g, mode = 'in')
out_degree <- degree(g, mode='out')

###4
all_degree <- degree(g, mode='all')
max(all_degree) 
#The highest degree is 53


all_degree[all_degree==53] 
#Node 33 and 4429 have 53 degrees.


#4.	Pick up one of the products (in case there are multiple) with highest degree 
#(in-degree + out-degree), and find its subcomponent, i.e., all the products that are 
# connected to this focal product.From this point on, you will work only on this subcomponent. 
# we pick product ID 4429.
sc_4429 <- subcomponent(g, "4429", "all")
sc_4429

###5.	Visualize the subcomponent using iGraph, trying out different colors, node and edge sizes and layouts, so that the result is most appealing. Find the diameter,
# and color the nodes along the diameter. Provide your insights from the visualizations.
graph_4429 <- induced_subgraph(g, sc_4429)
graph_4429

V(graph_4429)
E(graph_4429)
V(graph_4429)$label <- V(graph_4429)$name
V(graph_4429)$degree <- degree(graph_4429)
plot(graph_4429,
     vertex.color='blue',
     vertex.size= V(graph_4429)$degree*0.2,
     edge.arrow.size=0.1,
     vertex.label.cex=0.1,
     layout=layout.kamada.kawai)

#diameter
diameter(graph_4429, directed = T, weights = NA)
diam_4429 <- get_diameter(graph_4429, weights = NULL)
diam_4429

# color thee nodes along the diameter
V(graph_4429)$color<-"green"
V(graph_4429)$color[diam_4429]<-"red"
plot(graph_4429,
     vertex.color=V(graph_4429)$color,
     vertex.size= V(graph_4429)$degree*0.2,
     edge.arrow.size=0.01,
     vertex.label.cex=0.01,
     layout=layout.kamada.kawai)


### 6.	Compute various statistics about this network (i.e., subcomponent), including degree 
# distribution, density, and centrality (degree centrality, closeness, centrality 
# and between centrality), hub/authority scores, etc. Interpret your results 

# a) degree_distribution
degree_4429 <- degree_distribution(graph_4429, cumulative=T, mode="all")
plot( x=0:max(all_degree), y=1-degree_4429, pch=19, cex=1.2, col="red", 
      xlab="Degree", ylab="Cumulative Frequency")

# b) density
edge_density(graph_4429, loops=F)

# c) degree centrality
centr_degree(graph_4429)

# d) closeness centrality
closeness <- closeness(graph_4429, mode='all', weights=NA)
closeness

# e) betweenness centrality
betweenness <- betweenness(graph_4429, directed='T', weights=NA)
betweenness

# f) hub/authority scores
hub_score <- hub.score(graph_4429)$vector
hub_score

#g) authority_score
authority_score<- authority.score(graph_4429)$vector
authority_score

### 7.	Create a group of variables containing the information of neighbors that 
# “point to” focal products. The variables include:
# retrieve product information of subcomponent 4429 from products file
products1$id<-as.vector(products1$id)
sc_id<-as_ids(sc_4429)
products_sc<-products1[products1$id %in% sc_id,]
products_sc

# a.	Neighbors’ mean rating (nghb_mn_rating), 
# b.	Neighbors’ mean salesrank (nghb_mn_salesrank), 
# c.	Neighbors’ mean number of reviews (nghb_mn_review_cnt), 
library(dplyr)
copurchase1_new <- inner_join(copurchase1, products_sc, by=c('Source'='id'))
n_mean<-copurchase1_new %>%
  group_by(copurchase1_new$Target) %>%
  summarise(nghb_mn_rating=mean(rating),
            nghb_mn_salesrank=mean(salesrank), 
            nghb_mn_review_cnt=mean(review_cnt))
n_mean
colnames(n_mean)<-c("Target","nghb_mn_rating","nghb_mn_salesrank","nghb_mn_review_cnt")

### 8.8.	Include the variables (taking logs where necessary) created in 
# Parts 2-6 above into the “products” information and fit a Poisson regression 
# to predict salesrank of all the books in this subcomponent using products’ own 
# information and their neighbor’s information. Provide an interpretation of your results. 
# convert all igraph lists to data frames
in_degree
in_degree1 <- as.data.frame(in_degree)
in_degree1 <- cbind(newColName = rownames(in_degree1), in_degree1)
rownames(in_degree1) <- 1:nrow(in_degree1)
#colnames(in_degree1)[1] = "Nodes"

out_degree1 <- as.data.frame(out_degree)
out_degree1 <- cbind(newColName = rownames(out_degree1), out_degree1)
rownames(out_degree1) <- 1:nrow(out_degree1)
colnames(out_degree1) <- c("Nodes", "out_degree")

closeness1 <- as.data.frame(closeness)
closeness1 <- cbind(newColName = rownames(closeness1), closeness1)
rownames(closeness1) <- 1:nrow(closeness1)
colnames(closeness1) <- c("Nodes", "closeness")

betweenness1 <- as.data.frame(betweenness)
betweenness1 <- cbind(newColName = rownames(betweenness1), betweenness1)
rownames(betweenness1) <- 1:nrow(betweenness1)
colnames(betweenness1) <- c("Nodes", "betweenness")

hub_score1 <- as.data.frame(hub_score)
hub_score1 <- cbind(newColName = rownames(hub_score1), hub_score1)
rownames(hub_score1) <- 1:nrow(hub_score1)
colnames(hub_score1) <- c("Nodes", "hub_score")

authority_score1 <- as.data.frame(authority_score)
authority_score1 <- cbind(newColName = rownames(authority_score1), authority_score1)
rownames(authority_score1) <- 1:nrow(authority_score1)
colnames(authority_score1) <- c("Nodes", "authority_score")

library("sqldf")
data_poission <- sqldf("SELECT n_mean.Target, hub_score, betweenness, authority_score, 
closeness, in_degree, out_degree, nghb_mn_rating, nghb_mn_salesrank, nghb_mn_review_cnt,
products_sc.review_cnt, products_sc.downloads, products_sc.rating, products_sc.salesrank
                      FROM n_mean, products_sc, hub_score1, betweenness1, authority_score1, closeness1, in_degree1, out_degree1
                      WHERE n_mean.Target = betweenness1.Nodes 
                      and n_mean.Target = authority_score1.Nodes
                      and n_mean.Target = closeness1.Nodes
                      and n_mean.Target = in_degree1.Nodes
                      and n_mean.Target = out_degree1.Nodes
                      and n_mean.Target = hub_score1.Nodes
                      and n_mean.Target = products_sc.id")


summary(salesrating_prediction<- glm(salesrank ~ review_cnt + downloads + rating + hub_score + betweenness + 
                                       authority_score + closeness + in_degree + out_degree + 
                                       nghb_mn_rating + nghb_mn_salesrank + nghb_mn_review_cnt,family="poisson",
                                     data=data_poission))


