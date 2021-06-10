#### Sociologie des réseaux sociaux ####
#### Validation finale ####



# Chargement des données 


setwd("~/data")

raw_g = as.data.frame(read.csv("higgs-activity_time.txt", header=TRUE, sep=" "))
raw_f = as.data.frame(read.csv("higgs-social_network.edgelist", header=TRUE, sep = " "))


#On enlève les noeuds qui ne font que se citer eux-mêmes pour alléger le graphe.

higgs_g_0 = raw_g[raw_g$U1 != raw_g$U2, ]
higgs_f_0 = raw_f[raw_f$U1 != raw_f$U2, ]
dim(higgs_g_0)




#On trie par période et on découpe en trois vagues
higgs_g_0[order(higgs_g_0$T),]
#au debut 
h_gw_1 = head(higgs_g_0, 10000)#
#au milieu
h_gw_2 = higgs_g_0[225000:235000,]
# a la fin
h_gw_3 = tail(higgs_g_0, 10000)




#Chargement d'igraph

library(igraph)

##### créer graphe
# créons objet graphe
g_gw1 <- graph.data.frame(h_gw_1, directed=TRUE)
g_gw2 <- graph.data.frame(h_gw_2, directed=TRUE)
g_gw3 <- graph.data.frame(h_gw_3, directed=TRUE)



#############################
#####VISUALISATIONS########



#On applique le freshman Reinglod layout avec des valeurs plus faibles pour 
#mettre moins de temps 


#1ere tranche 

l <- layout_with_fr(g_gw1, niter=20)
l <- norm_coords(l, ymin=-.5, ymax=1, xmin=-.5, xmax=1)

par(mfrow=c(1,1), mar=c(0,0,0,0))
plot(g_gw1, rescale=F, layout=l*1.6, vertex.size = 4, vertex.label = NA)

par(mfrow=c(1,2))




###On ne garde que les liens par nature
m = as.matrix(h_gw_1[1:2])


## 1Ere vague ####


l1 <- layout_with_graphopt(g_gw1, start = m, mass =10, charge=0.01, spring.length = 1, spring.constant = 1)

##Plus la charge est élevée, plus les noeuds sont resserrés


###On ne egarde que les liens de RETWEETS pour y voir plus clair
par(mfrow=c(1,3))
g_gw1.rt <- delete_edges(g_gw1, E(g_gw1)[N != "RT"])
plot(g_gw1.sp, l1, vertex.label = NA, vertex.size =2, vertex.color = "yellow", edge.arrow.size= .1)

###Même chose avec MENTIONS 
g_gw1.mt <- delete_edges(g_gw1, E(g_gw1)[N != "MT"])


plot(g_gw1.sp, l1, vertex.label = NA, vertex.size =2, vertex.color = "blue", edge.arrow.size= .1)

###Même chose avec REPLY
g_gw1.re <- delete_edges(g_gw1, E(g_gw1)[N != "RE"])


plot(g_gw1.sp, l1, vertex.label = NA, vertex.size =2, vertex.color = "green", edge.arrow.size= .1)



####2 vague ####


l1 <- layout_with_graphopt(g_gw2, start = m, mass =10, charge=0.01, spring.length = 1, spring.constant = 1)

# On divise le tableau en 3
par(mfrow=c(1,3))

###RETWEETS
g_gw2.rt <- delete_edges(g_gw2, E(g_gw2)[N != "RT"])
plot(g_gw2.sp, l1, vertex.label = NA, vertex.size =2, vertex.color = "yellow", edge.arrow.size= .1)

###MENTIONS 
g_gw2.mt <- delete_edges(g_gw2, E(g_gw2)[N != "MT"])
plot(g_gw2.sp, l1, vertex.label = NA, vertex.size =2, vertex.color = "blue", edge.arrow.size= .1)

###REPLY
g_gw2.re <- delete_edges(g_gw2, E(g_gw2)[N != "RE"])
plot(g_gw2.sp, l1, vertex.label = NA, vertex.size =2, vertex.color = "green", edge.arrow.size= .1)



####VAGUE 3####
l1 <- layout_with_graphopt(g_gw3, start = m, mass =10, charge=0.01, spring.length = 1, spring.constant = 1)
##Plus la charge est élevée, plus les noeuds sont resserrés

# On divise le tableau en 3
par(mfrow=c(1,3))

###RETWEETS
g_gw3.rt <- delete_edges(g_gw3, E(g_gw3)[N != "RT"])
plot(g_gw3.sp, l1, vertex.label = NA, vertex.size =2, vertex.color = "yellow", edge.arrow.size= .1)

###MENTIONS 
g_gw3.mt <- delete_edges(g_gw3, E(g_gw3)[N != "MT"])
plot(g_gw3.sp, l1, vertex.label = NA, vertex.size =2, vertex.color = "blue", edge.arrow.size= .1)

###REPLY
g_gw3.re <- delete_edges(g_gw3, E(g_gw3)[N != "RE"])
plot(g_gw3.sp, l1, vertex.label = NA, vertex.size =2, vertex.color = "green", edge.arrow.size= .1)





###VISUALISATION GLOBALE PAR VAGUES###



par(mfrow=c(1,1),mar=c(0,0,0,0))
l2 <- layout_with_graphopt(g_gw1, start = m, mass =10, charge=0.0000001, spring.length = 1, spring.constant = 1)


#On utilise le pkg GGGRAPH qui permet d'avoir une visualisation en couleurs. 
library("ggraph")

ggraph(g_gw1, layout = l2) + 
        geom_edge_link(aes(colour = N)) + 
        geom_node_point()

ggraph(g_gw1.m, layout = l2) + 
        geom_edge_link(aes(colour = N)) + 
        geom_node_point()

ggraph(g_gw2, layout = l2) + 
        geom_edge_link(aes(colour = N)) + 
        geom_node_point()

ggraph(g_gw3, layout = l2) + 
        geom_edge_link(aes(colour = N)) + 
        geom_node_point()






####CALCUL

# Distance moyenne entre les noeuds
average.path.length(g_gw1)
average.path.length(g_gw2)
average.path.length(g_gw3)


# Mesure locale correspond à l'entourage de chaque noeud
trans_local <- transitivity(g_gw1,type="local")

# Regardons la moyenne
mean(trans_local, na.rm=TRUE)

#Regardons la taille totale du graphe
length(V(g_gw1))
length(V(g_gw2))
length(V(g_gw3))
cliques(g_gw1, min=4)


# Densité (Nombre de liens existants / nombre de liens possibles)
graph.density(g_gw1)
graph.density(g_gw2)
graph.density(g_gw3)



#Regardons la centralité de degrés de chaque graphe
centr_degree(g_gw1, mode = "all")$centralization
centr_degree(g_gw2, mode = "all")$centralization
centr_degree(g_gw3, mode = "all")$centralization


# Nombre d'îles
(clusters(g_gw1)
clusters(g_gw2)
clusters(g_gw3)

# Nombre d'îles et taille du plus grand cluster
c = clusters(g_gw1)
max(c$csize)
c2 = clusters(g_gw2)
max(c2$csize)
c3 = clusters(g_gw3)
max(c3$csize)


##############
# Mesures sur des liens spécifiques


#Retweets

####Répartition de la centralité  au cours des 3 tranches 


#1ere phase 
V(g_gw1.rt)$degall = degree(g_gw1.rt, mode="all", normalized=TRUE)
sd(V(g_gw1.rt)$degall)
par(mfrow=c(1,3))
degré = (V(g_gw1.rt)$degall)^(1/3.5)
hist(degré, xlim = c(0,0.30), ylim = c(0,6250))

#2 eme phase
V(g_gw2.rt)$degall = degree(g_gw2.rt, mode="all", normalized=TRUE)
sd(V(g_gw2.rt)$degall)

degré = (V(g_gw2.rt)$degall)^(1/3.5)
hist(degré, xlim = c(0,0.30), ylim= c(0,6250))


#3 eme tranche 
V(g_gw3.rt)$degall = degree(g_gw3.rt, mode="all", normalized=TRUE)
sd(V(g_gw3.rt)$degall)

degré = (V(g_gw3.rt)$degall)^(1/3.5)
hist(degré, xlim = c(0,0.30), ylim = c(0,6250))



#Mentions#### 

####Répartition de la centralité  au cours des 3 tranches 


#1ere phase 
V(g_gw1.mt)$degall = degree(g_gw1.mt, mode="all", normalized=TRUE)
sd(V(g_gw1.mt)$degall)
par(mfrow=c(1,3))
degré = (V(g_gw1.mt)$degall)^(1/3.5)
hist(degré, xlim = c(0,0.30), ylim = c(0,6250))

#2 eme phase
V(g_gw2.mt)$degall = degree(g_gw2.mt, mode="all", normalized=TRUE)
sd(V(g_gw2.mt)$degall)

degré = (V(g_gw2.mt)$degall)^(1/3.5)
hist(degré, xlim = c(0,0.30), ylim= c(0,6250))


#3 eme tranche 
V(g_gw3.mt)$degall = degree(g_gw3.mt, mode="all", normalized=TRUE)
sd(V(g_gw3.mt)$degall)

degré = (V(g_gw3.mt)$degall)^(1/3.5)
hist(degré, xlim = c(0,0.30), ylim = c(0,6250))



#replies

####Répartition de la centralité  au cours des 3 tranches 


#1ere phase 
V(g_gw1.re)$degall = degree(g_gw1.re, mode="all", normalized=TRUE)
sd(V(g_gw1.re)$degall)
par(mfrow=c(1,3))
degré = (V(g_gw1.re)$degall)^(1/8)
hist(degré, xlim = c(0.2,0.60), ylim = c(0,1000))

#2 eme phase
V(g_gw2.re)$degall = degree(g_gw2.re, mode="all", normalized=TRUE)
sd(V(g_gw2.re)$degall)

degré = (V(g_gw2.re)$degall)^(1/8)
hist(degré, xlim = c(0.2,0.60), ylim= c(0,1000))


#3 eme tranche 
V(g_gw3.re)$degall = degree(g_gw3.re, mode="all", normalized=TRUE)
sd(V(g_gw3.re)$degall)

degré = (V(g_gw3.re)$degall)^(1/8)
hist(degré, xlim = c(0.2,0.60), ylim = c(0,1000))



