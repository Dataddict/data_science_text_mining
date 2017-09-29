
#install.packages("XML")
#install.packages("tm")
#install.packages("SnowballC")
library(tm)
#chargement des donnees
AdresseNewsReuters <- system.file("texts", "crude", package = "tm")
NewsReuters <- VCorpus(DirSource(AdresseNewsReuters), readerControl = list(reader = readReut21578XMLasPlain))

str(NewsReuters)


#Récupérer le nombre de documents dans le corpus
NewsReuters

#Récupérer le nombre de caractères par document
inspect(NewsReuters)

#visionner le contenu du 5ème texte
writeLines(as.character(NewsReuters[[5]]))

#creer une liste contenant l'ensemble des textes des news Reuters
#listeNews <- lapply(NewsReuters, as.character)
#listeNews[5]

#recuperation des titres
(listeTitre <- meta(NewsReuters, tag = "heading"))

#Récupération des pays concernés par les nouvelles : on utilise le tag "places"
(listePays <- meta(NewsReuters, tag = "places"))
(listePaysUniques <- unique(listePays))

taille <- max(sapply(listePays , length))
res <- sapply(listePays , function(u) c(u,rep(NA,taille -length(u))))
mListePays <- t(res)
tbl_pays <- table(mListePays) #on calcule le nombre d'occurences pour chaque pays
pie(tbl_pays,cex = 0.8,main = "Pays dans les nouvelles Reuters sur le pétrole", col = rainbow(length(tbl_pays)))


#
?DocumentTermMatrix
(tDocMatrix <- DocumentTermMatrix(NewsReuters))
inspect(tDocMatrix) #on obtient le détail : quel mot, présent combien de fois dans tel document

findFreqTerms(x = tDocMatrix , lowfreq = 50)
#les mots les plus souvent utilisés ne sont pas très utiles pour l'étude

#retraitement des données
#lettres miniscules
NewsReutersRetraitee<- tm_map(NewsReuters,FUN = content_transformer(tolower))
#suppression des chiffres
NewsReutersRetraitee<- tm_map(NewsReutersRetraitee,FUN = removeNumbers)
#supression de la ponctuation
NewsReutersRetraitee<- tm_map(NewsReutersRetraitee,FUN = removePunctuation)
#suppression des espaces blancs multiples
NewsReutersRetraitee<- tm_map(NewsReutersRetraitee,FUN = stripWhitespace)

#comparaison des textes
writeLines(as.character(NewsReuters[[5]]))
writeLines(as.character(NewsReutersRetraitee[[5]]))

#afficher les mots qui apparaissent au moins 5 fois
(tDocMatrix <- DocumentTermMatrix(NewsReutersRetraitee))
findFreqTerms(x = tDocMatrix , lowfreq = 5)
#beaucoup de termes n'apportent pas vraiment d'information

#la fonction stopwords retourne un ensemble de mots récurrents et souvent inutiles 
#pour l'analyse de texte pour un langage donné (prépositions, articles,...)
?stopwords
NewsReutersRetraitee<- tm_map(NewsReutersRetraitee,FUN = removeWords, stopwords("english"))
(tDocMatrix <- DocumentTermMatrix(NewsReutersRetraitee))
motsfrequents <- findFreqTerms(x = tDocMatrix , lowfreq = 10)
motsfrequents
#suppression de mots inutiles
motsAsupprimer <- c("dlrs","last", "one", "pct", "said", "will", "reuter","also","say")
NewsReutersRetraitee<- tm_map(NewsReutersRetraitee,FUN = removeWords, motsAsupprimer)



#mots les plus fréquemment utilisés dans l'ensemble des documents 
(tDocMatrix <- DocumentTermMatrix(NewsReutersRetraitee))
(motsFrequemmentU <- removeSparseTerms(tDocMatrix,sparse = 0.75))

inspect(motsFrequemmentU)
#mots les plus souvents associés au mot OPEC
findAssocs(tDocMatrix, "opec", 0.80)

#prices et price / barrel et barrels : des mots ayant le même sens ne sont pas pris en compte pour un seul mot

#la fonction stemDocument permet de résoudre ce problème (mais attention elle peut parfois se tromper en simplifiant)
NewsReutersRetraitee<- tm_map(NewsReutersRetraitee,FUN = stemDocument)


writeLines(as.character(NewsReuters[[5]]))
writeLines(as.character(NewsReutersRetraitee[[5]]))


#mots les plus fréquemment utilisés dans l'ensemble des documents après le stemming
(tDocMatrix <- DocumentTermMatrix(NewsReutersRetraitee))
(motsFrequemmentU <- removeSparseTerms(tDocMatrix,sparse = 0.6))
inspect(motsFrequemmentU)

#mots les plus souvents associés au mot OPEC après stemming
findAssocs(tDocMatrix, "opec", 0.80)


#clustering
#install.packages("cluster")
library(cluster)
#on calcule la "distance" entre les mots (1 mot = 1 ligne) en utilisant  sqrt(sum((x_i - y_i)^2))
dist.mots<- dist(x = t(as.matrix(motsFrequemmentU)), method = "euclidean")
dist.mots

?agnes
#http://www.unesco.org/webworld/idams/advguide/Chapt7_1_4.htm
#Classification hiérarchique : Agglomerative Nesting
motsFrequemmentU.clustering <- agnes(x = dist.mots, diss = T, metric = "euclidian", stand = F, method = "ward")
plot(motsFrequemmentU.clustering)

#on voit que la construction commence par regrouper les deux mots les plus proches et ainsi de suite (cf doc écart de Ward)
#si on coupe l'arbre à 15 on a deux clusters