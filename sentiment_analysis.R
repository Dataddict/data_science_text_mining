#TD 7
#source des données 
# maas-EtAl:2011:ACL-HLT2011,
# author    = {Maas, Andrew L.  and  Daly, Raymond E.  and  Pham, Peter T.  and  Huang, Dan  and  Ng, Andrew Y.  and  Potts, Christopher},
# title     = {Learning Word Vectors for Sentiment Analysis},
# booktitle = {Proceedings of the 49th Annual Meeting of the Association for Computational Linguistics: Human Language Technologies},
# month     = {June},
# year      = {2011},
# address   = {Portland, Oregon, USA},
# publisher = {Association for Computational Linguistics},
# pages     = {142--150},
# url       = {http://www.aclweb.org/anthology/P11-1015}


#install.packages("XML")
#install.packages("tm")
#install.packages("SnowballC")
library(tm)
#chargement des 25002 critiques de film
#chargement des donnees du training set
#critiques positives
# AdresseCritiques_pos <- "D:/Documents/cours R/TD_R/aclImdb/train/pos"
# Critiques_pos<- VCorpus(DirSource(AdresseCritiques_pos, encoding = "UTF-8"),readerControl = list(language ="en_US"))
# Critiques_pos
# 
# #critiques negatives
# AdresseCritiques_neg <- "D:/Documents/cours R/TD_R/aclImdb/train/neg"
# Critiques_neg<- VCorpus(DirSource(AdresseCritiques_neg, encoding = "UTF-8"),readerControl = list(language ="en_US"))
# Critiques_neg
# 
# Critiques <- c(Critiques_pos,Critiques_neg)

#######################################################################"
#chargement des 1002 critiques de film
AdresseCritiques <- "D:/Documents/cours R/TD_R/train_movies_light"
Critiques<- VCorpus(DirSource(AdresseCritiques, encoding = "UTF-8"),readerControl = list(language ="en_US"))



#retraitement des données

#lettres miniscules
Critiques<- tm_map(Critiques,FUN = content_transformer(tolower))
#suppression des chiffres
Critiques<- tm_map(Critiques,FUN = removeNumbers)
#supression de la ponctuation
Critiques<- tm_map(Critiques,FUN = removePunctuation)
#suppression des espaces blancs multiples
Critiques<- tm_map(Critiques,FUN = stripWhitespace)
#stopwords
Critiques<- tm_map(Critiques,FUN = removeWords, stopwords("english"))
#Analyse supplementaire pour supprimer d'autres stopwords
initialTDM <- DocumentTermMatrix(Critiques)
analyseTDM <- removeSparseTerms(initialTDM, sparse = 0.8)
(topWords <- Terms(analyseTDM))

otherStopwords <-c("also","can","dont","just","get","many","the", "see", "seen","will","this")
Critiques<- tm_map(Critiques,FUN = removeWords, otherStopwords)

#on dispose de dictionnaires qui vont nous aider à identifier des mots positifs et d'autres négatifs
#lecture des mots positifs de Hu Liu
HL.pos <- read.table(file = "D:/Documents/cours R/TD_R/Hu_Liu_positive_word_list.txt",header = F, colClasses = c("character"), row.names = NULL, col.names = "positive words")
#lecture des mots négatifs de Hu Liu
HL.neg <- read.table(file = "D:/Documents/cours R/TD_R/Hu_Liu_negative_word_list.txt",header = F, colClasses = c("character"), row.names = NULL, col.names = "negative words")

#transformation en dictionnaire :
HL.dico.pos <-c(as.character(HL.pos$positive.words))
HL.dico.neg <-c(as.character(HL.neg$negative.words))

#Analyse des termes les plus fréquents rencontrés dans les critiques
#termes positifs les plus fréquents
critiquesTDM_pos <- DocumentTermMatrix(Critiques,control = list(dictionary = HL.dico.pos))
analyseTDM <- removeSparseTerms(critiquesTDM_pos, sparse = 0.9)
(topWords <- Terms(analyseTDM))
#termes négatifs les plus fréquents
critiquesTDM_neg <- DocumentTermMatrix(Critiques,control = list(dictionary = HL.dico.neg))
analyseTDM <- removeSparseTerms(critiquesTDM_neg, sparse = 0.95)
(topWords <- Terms(analyseTDM))




#pour chaque document on récupère le nombre total de mots, le nombre de mots positifs, 
#le nombre de mots négatifs et les autres mots
nb_doc <- length(names(Critiques))
mots_positifs <- c()
mots_negatifs <- c()
total_mots <- c()
autres_mots <- c()

for (i in 1:nb_doc)
{
  mots_positifs <- c(mots_positifs, sum(termFreq(Critiques[[i]], control = list(dictionary = HL.dico.pos))))
  mots_negatifs <- c(mots_negatifs, sum(termFreq(Critiques[[i]], control = list(dictionary = HL.dico.neg))))
  total_mots <- c(total_mots, length(strsplit(Critiques[[i]]$content,split = " ")[[1]]))
}
autres_mots <- total_mots - mots_negatifs - mots_positifs

nomDocs <- names(Critiques)
#construction d'un data.frame contenant l'ensemble
EchantillonCritiques <- data.frame(nomDocs, total_mots, mots_positifs, mots_negatifs, autres_mots, stringsAsFactors = F )
#calcul des % de mots positifs
EchantillonCritiques$pos <- EchantillonCritiques$mots_positifs / EchantillonCritiques$total_mots
#calcul des % de mots négatifs
EchantillonCritiques$neg <- EchantillonCritiques$mots_negatifs / EchantillonCritiques$total_mots

#récupération de la note contenue dans le nom du document
for (i in 1:nb_doc)
{
  EchantillonCritiques$note[i] <- as.integer(strsplit(strsplit(EchantillonCritiques$nomDocs[i],split = '[.]')[[1]][1],split = "[_]")[[1]][2])
  EchantillonCritiques$critique[i] <- ifelse(EchantillonCritiques$note[i]>5, yes = 1, no = 0)
}
#application des labels aux facteurs
factor(EchantillonCritiques$critique, levels = c(0,1), labels = c("critiqueNegative","critiquePositive" ))


#Test and train regimen
set.seed(1)
train_test <- c(rep(1,length=trunc((2/3)*nb_doc)),
                rep(2,length=(nb_doc - trunc((2/3)*nb_doc))))

EchantillonCritiques$training <- sample(train_test)
