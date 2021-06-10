###Créer un nuage de mots pour les communautés climatiques ###



Import des données

```{r}
install.packages("tm")
install.packages("ggwordcloud")
```






CHANGER LES FICHIERS

```{r}

library(tm)

setwd("C:/Users/Tanguy RUAULT/Downloads/data_isc")



com_0 = readLines("corpus/tx_climato_sc.csv", skipNul = TRUE)


```






















###TOKENISATION###

```{r}

com_0_v = as.character(c(com_0))
library("textreuse")
tc_0=tokenize_ngrams(paste(com_0_v, collapse = "/n"), n = 2)
head(tc_0)

```


```{r}
c("a","b","c","d")



```
















NETTOYAGE#en cours 

```{r}
df_pro_clim$new = gsub(" ", "", df_pro_clim$tc_0)
df_pro_clim = drop(df_pro_clim$tc_0)
head(df_pro_clim)



tc_0 = gsub("[\r\n\t\f\v ]", "", tc_0)
tc_0[1]



grep("tx",tc_0[1], value = TRUE)

```{r}

drop_pat = function(x, pat){
#as a character vector
for (element in x){
   drop(grep(pattern = pat,  x[element], value = TRUE))
}
}

test = (drop_pat(tc_0, "tx"))

test
```


#NETTOYAGE
```{r}
df_pro_clim = as.data.frame(tc_0)
doc <-VCorpus(VectorSource(df_pro_clim))
doc1clean <- tm_map(doc, removeWords,stopwords("english"))
doc2clean <- tm_map(doc1clean, removePunctuation, preserve_intra_word_dashes = FALSE)
doc3clean = tm_map(doc2clean, removePunctuation, preserve_intra_word_dashes = TRUE)
doc4clean <- tm_map(doc3clean, removeNumbers)
#doc5clean <- tm_map(doc4clean, removeWords, c("today","make","just","will","global","like","amp","climate","change","warming","https","Ã","tx","Å","s","t", "rt","tco","us", "can"))
````


#NETTOYAGE

````{r}

library("tm")
count = unlist(lapply(doc4clean, unique))


#NETTOYAGE

#on enlève tous les mots commençant par un @
count = gsub("@$", "", count)
#on enleve tous les charactères non alphanumériques ( donc les emojis)
count = gsub("[^a-zA-Z0-9_]", " ", count)
#on enlève tous les espaces supérieurs à 1
count = gsub("[\r\n\t\f\v ]{2,}", "", count)
count = gsub("(https)|(\bn\b)|(rt)", "", count)
#On enlève tous les espaces hors des lignes
new = gsub("([\r\n\t\f\v ]$|^[\r\n\t\f\v ])", "", count)

head(new)

````


````{r}


library("tm")
count = unlist(doc4clean)
new = gsub("[\r\n\t\f\v ]", "", count)

head(new)


```

new = gsub(" ", "", count)
head(new)

````


##NETTOYAGE (en cours )

````{r}

library("tm")
count = unlist(docdu16onallclean, use.names = FALSE)

new = gsub(" ", "", count)
new[200]

````

clean_space = function(liste){
 for (element in liste){
  new = gsub(" ", "", count)
 }
  return(new)
}

```
NETTOYAGE (en cours)
````{r}

clean_space(count[1:10])

````
df = as.data.frame(count)
a = df[order(df$Freq, decreasing = TRUE),]
com = head(a, 200)
head(com)



calcul des fréquences

````{r}

library("tm")
count = table(new)
df = as.data.frame(count)
a = df[order(df$Freq, decreasing = TRUE),]
com = head(a, 200)
head(com)

```


#ALTERNATIVE ( en cours )
```{r}

library("tm")

count<- unlist(docdu16onallclean)
count[1]
```


df = as.data.frame(count)
a = df[order(df$Freq, decreasing = TRUE),]
com = head(a, 200)
com


Prévisualisation du nuage de mots 


```{r}
library("ggwordcloud")

#prend en entrée la taille et les mots correspondants.

ggplot(com, aes(label = new, size = Freq)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 60) +
  theme_minimal()
```



















