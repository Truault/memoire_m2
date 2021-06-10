###Graphe main communities



#####On cherche à retrouver toutes les interactions entre les comptes des deux 
#####principales communautés. 



##en entrée une edgelist avec le graphe des communautés pc et cs mélangées. . 
setwd("C:/Users/Tanguy RUAULT/Downloads/data_isc")

raw_g = as.data.frame(read.csv("multilayer_mc_complete.csv", header=TRUE))


###on enlève la date
raw_g = raw_g[,-1]

###on enlève les noeuds qui se citent eux_mêmes
raw_g = raw_g[raw_g$target != raw_g$source, ]


raw_g$source = lapply(raw_g$source, format, scientific = FALSE )
raw_g$target = lapply(raw_g$target, format, scientific = FALSE )
#On enlève les noeuds qui ne font que se citer eux-mêmes pour alléger le graphe.

library(igraph)

##### créer graphe
# créons objet graphe

#on suppose que le graphe est dirigé 
mc_g <- graph.data.frame(raw_g, directed=TRUE)
summary(mc_g)


#on charge les attributs qui nous permettent de savoir à quelle com chacun appartient

Attrs <- as.data.frame(read.csv("2main_communities100.csv", header=TRUE, sep = ","))

#On change l'affichage des identifiants 


###### NETTOYAGE #####


Attrs$Id = lapply(Attrs$Id, format, scientific = FALSE )


V(mc_g)$com1 = Attrs$COM1
V(mc_g)$com2 <- Attrs$COM2

#############################
#####VISUALISATIONS########

plot(mc_g, 
     edge.arrow.size = .01,
     vertex.size =1, 
     vertex.label=NA)


###on colorie les noeuds climato-sceptiques en jaune. 

plot(mc_g,
     vertex.color=V(mc_g)$sc,
     edge.arrow.size=.01,
     vertex.size =.03,
     vertex.label=NA)


####On remarque qu'il existe des interactions a priori assez
#### importantes 


plot(mc_g,
     edge.arrow.size=.01,
     vertex.color=V(mc_g)$sc,
     vertex.size =.03,
     vertex.label=NA,
     layout=layout_with_fr)

##greeedy
MygOpt = layout_with_graphopt(mc_g, charge = 0.000002)
plot(mc_g,
     edge.arrow.size=.01,
     vertex.color=V(mc_g)$sc,
     vertex.label=NA,
     layout=MygOpt)

summary(mc_g)



####Traitement sur les interactions entre comptes ####

###on garde uniquement une edgelist avec en source les comptes
###com1 et en target les comptes com2 

####liste des comptes com1
com1 = Attrs[Attrs$COM1==1,]
com1 = com1$Id


###des comptes com2
com2= Attrs[Attrs$COM2==1,]
com2 = com2$Id


###el dont la source ne contient que des comptes com1
interac1 = raw_g[raw_g$source %in% com1,]

###el "...." ET dont la target ne contient que des comptes com2
interac1 = interac1[interac1$target %in% com2,]






###el dont la source ne contient que des comptes climato_sceptiques
raw_pc = raw_g[raw_g$source %in% cs,]

###el "...." ET dont la target ne contient que des comptes pro-climat
raw_pc  = raw_pc[raw_pc$target %in% pc,]











