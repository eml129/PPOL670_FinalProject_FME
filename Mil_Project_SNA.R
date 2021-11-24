##Military Leaders - SNA

install.packages(c("statnet", "intergraph", "igraph", "dils"), dependencies = T)
install.packages("network", contriburl="http://bojan.3e.pl/R/hacks")
library(statnet)
library(intergraph)
library(igraph)
library(dils)
library(network)
install.packages("stats", dependencies=T)
install.packages("cluster", dependencies=T)
library(stats)
library(cluster)
install.packages("snowboot", dependencies=T)
library(snowboot)

#Preprocessing the data: converted the CSV into an edgelist and an adjacency matrix using Gephi.

#Data notes: Nodes are the leaders (Name) associated with a particular country. The presence of a 1 or 0 indicates whether or not that leader received military education in that country. 
#The edges in this dataset are "ties" to a country. Node relationship is undirected. It is an undirected weighted network. 

#Coerce Military to igraph Object, convert igraph to network object

Military_edgelist = read.csv("/Users/aubreythibaut/Desktop/GOVT_772/data_assignment/Military dataset network/edgelist.csv")
Military_igraph <- IgraphFromEdgelist(Military_edgelist, directed = FALSE)

print_all(Military_igraph)

Military_network <- asNetwork(Military_igraph)

#Check if edgelists of the objects are identical
el.g2 <- get.edgelist(Military_igraph)
el.n2 <- as.matrix(Military_network, "edgelist")
identical( as.numeric(el.g2), as.numeric(el.n2))
#TRUE

#facebook<-asNetwork(Facebook)
##Community detection


#Clique Overview

##Total nodes in the largest clique/Defaults to Strong Ties
clique.number(Military_igraph) #6 cliques 

##Cliques of a Certain Size
cliques(Military_igraph, min=3)
largest_cliques(Military_igraph)
#[[7]] 6/46 vertices, named, from a68e172: [1] 42 7  41 17 21 45

##Maximal Cliques: Counts all the maximal cliques 
count_max_cliques(Military_igraph) #51

#K Cores
Mil.kcore <- coreness(Military_igraph)    # Extract k-cores as a data object.
V(Military_igraph)$core <- Mil.kcore     # Add the cores as a vertex attribute
plot.igraph(Military_igraph, vertex.color=V(Military_igraph)$core, vertex.label=NA)
#See plot 

##Clique Co-membership
Military_net <-asNetwork(Military_igraph)
clique.comember <- clique.census(Military_net, mode="graph", clique.comembership = "sum")

military.dist<-dist(clique.comember$clique.comemb)
hc.mil <-hclust(military.dist)
plot(hc.mil, main="Hierarchical Clustering of Mlilitary Education Clique Co-Membership")
groups<-cutree(hc.mil, h=35)
table(groups)
plot(silhouette(cutree(hc.mil, h=35),military.dist))

##Average silhouette width >=0.8 is good!

#Getting the Largest Component
Military.CompW<-decompose.graph(Military_igraph, mode="weak")
Military.CompS<-decompose.graph(Military_igraph, mode="strong")
Max.Military <- which.max(sapply(Military.CompW, vcount))
Mil.Largest <- Military.CompW[[Max.Military]]
plot.igraph(Mil.Largest, vertex.label=NA) #largest component

#Community detection
par(mfrow=c(1,3))
##Spinglass Algorithm 
Military.Spin<-spinglass.community(Mil.Largest)
table(Military.Spin$membership)
Military.Spin$membership
Military.Spin
plot(Military.Spin, Mil.Largest, vertex.label=NA, main="Spinglass")
#Communities - looks like four key communities 

##Cluster GN 
Mil.Edge <-cluster_edge_betweenness(Mil.Largest)
V(Mil.Largest)$group<-membership(Mil.Edge)
plot(Mil.Edge, Mil.Largest, vertex.label=NA, main="GN")

##Fast and Greedy
Mil.greedy<-fastgreedy.community(Mil.Largest)
V(Mil.Largest)$group2<-membership(Mil.greedy)
plot(Mil.greedy, Mil.Largest, vertex.label=NA, main="Greedy")

##Modularity 

table(V(Mil.Largest)$group2)
modularity(Mil.Largest, as.numeric(factor(V(Mil.Largest)$group2))) #0.316
modularity(Mil.greedy) #0.319

summary(Military_igraph, print.adj = T)
V(Military_igraph)$name

#############################
############################3
###Adjacency 
## Read in adjacency matrix (helpful b/c our graph is weighted)
Mil_adj = read.csv("/Users/aubreythibaut/Desktop/GOVT_772/data_assignment/Military dataset network/adjecencyMatrixFormat.csv")
#Convert adjacency matrix to network object
mil_adj_net <-as.network(Mil_adj, matrix.type = "adjacency", directed = FALSE, loops = FALSE)

#convert network object to igraph
class(mil_adj_net)
mil_adj_igraph <- asIgraph(mil_adj_net)
class(mil_adj_igraph) #our igraph object from the adjacency matrix 

##Basic network features

#Size
network.size(mil_adj_net) #48

#Density 
gden(mil_adj_net) #Not dense at 0.42. Indicates presence of "hubs"

#Components
components(mil_adj_igraph) #Two components. One network connected via hubs and spokes and one isolated network. 


###Plotting the data - understanding the underlying structure of relationships

# 1) Fruchterman, Reingold - (AKA) Distance Scaling: Pulls nodes with more ties closer together and nodes with fewer ties further apart
par(mfrow=c(1,1), mar=c(2.5,0,2.5,0)) #Controls graph window. mfrow customizes the number of graphs. mar customizes the margins
gplot(mil_adj_net,
      vertex.cex=1.5, #Node size
      usearrows=T, #undirected
      mode="fruchtermanreingold", #Layout
      main="Military Education Network\n(Fruchterman Reingold)") #Title

#Classical = Multidimensional scaling technique - nodes overlap
gplot(mil_adj_net, vertex.cex=1.5, gmode="graph", mode="mds", main="Military Education Network\n(Classical)")

#Eigen = Node placement is based on the eigen structure of the adjacency matrix
#NOTE: Eigenvector centrality: a person with few connections could have a very high eigenvector centrality if those few connections were to very well-connected others. 
#Eigenvector centrality allows for connections to have a variable value, so that connecting to some vertices has more benefit than connecting to others. 
gplot(mil_adj_net, vertex.cex=1.5, gmode="graph", mode="eigen", main="Military Education Network\n(Eigen Centrality)")

#Spring = a spring embedder algorithm 
gplot(mil_adj_net, vertex.cex=1.5, gmode="graph", mode="spring", main="Military Education Network\n(Spring)")


###Centrality and Centralization

#Centrality
degree(mil_adj_igraph)
betweenness(mil_adj_igraph)
evcent(mil_adj_igraph)

centralization(mil_adj_net, degree)

