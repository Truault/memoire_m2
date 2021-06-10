##### ANALYSE DE LA STRUCTURE DE CHACUNE DES COMMUNAUTES #####




##en entrée une edgelist avec la communauté pro climat
setwd("C:/Users/Tanguy RUAULT/Downloads/data_isc")

raw_g = as.data.frame(read.csv("multilayer_pc.csv", header=TRUE))



raw_g$source = lapply(raw_g$source, format, scientific = FALSE )
raw_g$target = lapply(raw_g$target, format, scientific = FALSE )

##on met la première col en dernier 
date = raw_g[1]
target = raw_g[2]
source = raw_g[3]
new_g = data.frame(source, target,date)
head(new_g)








## on ne travaille pour l'instant que sur les deux premiers mois
g_janv_fev = new_g[new_g$date %in% c(01,02),]
#g_oct= new_g
#g_oct$date = regexec("[(2020-11-02)([+-[--A-Za-z1-9-:]*)00,]",g_oct$date)






library(igraph)



##### créer graphe
# créons objet graphe


#on suppose que le graphe est dirigé 
g <- graph.data.frame(g_janv_fev, directed=TRUE)

summary(g)
##on ajoute l'attribut date

#on charge les attributs

Attrs <- as.data.frame(read.csv("pro_climat.csv", header=TRUE, sep = ","))


E(g)$jan = Attrs$jan
E(g)$fev = Attrs$fev

E(g)$acc = Attrs$target==260041830

##visualisation
plot(g,
     edge.arrow.size=.02,
     vertex.size =.03,
     vertex.label=NA)


### quels sont les relations nouvelles ?

diff = difference(E(g), E(g)$fev)
diff[1]

###ajouter la caractéristique au df 


###on colorie en jaune les nouvelles
###relations

plot(g,
     edge.arrow.size=.02,
     edge.color=E(g)$diff,
     vertex.size =.03,
     vertex.label=NA)

