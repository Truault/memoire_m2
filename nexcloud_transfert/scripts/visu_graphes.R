library(igraph)

######
VISU Graphes
#######

setwd("C:/Users/Tanguy RUAULT/Downloads/data_isc/hue")

#### Données : réseau de conseil entre les juges du TCP

## importer les données : réseau vague 2 (2002)
# cette fois-ci, elles sont au format edgelist
W2 <- as.data.frame(read.csv("edgelist_graphGT.csv", header=TRUE, sep=","))

# la "force" représente la durée de la relation : 
### 1 si lien observé seulement en 2002
### 2 si lien observé en 2000 et en 2002

# mettons les deux premières variables au format "factor"
W2$V1 <- as.factor(W2$source)
W2$V2 <- as.factor(W2$target)


## Charger igraph
library(igraph)

###############
#### OBJET GRAPHE
###############

# créons objet graphe
W2g <- graph.data.frame(W2, directed=TRUE)
# voir résultats
summary(W2g)

# voir le graphique
plot(W2g,
     vertex.size=9,
     vertex.label=NA,
     edge.arrow.size=.1)

# pour visualiser un peu plus grand
par(mfrow=c(1,1),mar=c(0,0,0,0))
plot(W2g,
     vertex.size=9,
     vertex.label=NA,
     edge.label= NA,
     edge.arrow.size=.1)

# rajoutons la force des liens
plot(W2g,
     vertex.size=9,
     vertex.label=NA,
     edge.arrow.size=.1,
     edge.width=E(W2g)$weight)

# pour accentuer les différences
plot(W2g,
     vertex.size=.9,
     vertex.label=NA,
     edge.arrow.size=.1,
     edge.width=E(W2g)$force^2)


plot(W2g,
     edge.arrow.size=.5,
     vertex.size =.1,
     vertex.label=NA,
     layout=layout_with_fr)
par(mfrow=c(1,1),mar=c(0,0,0,0))
plot(W2g,
     edge.arrow.size=.1,
     vertex.size =.1,
     vertex.label=NA,
     layout=layout_with_kk)

MygOpt = layout_with_graphopt(W2g, charge = 0.000002)
plot(W2g,
     edge.arrow.size=.1,
     vertex.size =.1,
     layout=MygOpt)

clusters(W2g)
cluster_edge_betweenness(W2g)
###on enlève greta pour avoir qq chose d'homogène

W2gC = delete.vertices(W2g,"1006419421244678144")


MygOpt = layout_with_graphopt(W2gC, charge = 0.000002)
plot(W2gC,
     edge.arrow.size=.1,
     vertex.size =.1,
     vertex.label=NA,
     layout=MygOpt)


plot(W2gC,
     edge.arrow.size=.1,
     vertex.size =.1,
     vertex.label=NA,
     layout=layout_with_kk)

Mykk = layout_with_kk(W2gC,kkconst = 15000)

plot(W2gC,
     edge.arrow.size=.1,
     vertex.size =.1,
     vertex.label=NA,
     layout=Mykk)
plot(W2gC,
     edge.arrow.size=.1,
     vertex.size =.1,
     vertex.label=NA,
     layout=layout_with_fr)






