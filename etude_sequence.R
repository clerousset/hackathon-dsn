library(TraMineR)
inslibrary(questionr)
library(ggplot2)

# ETUDE POPULATION TOTALE
#ANALYSE SEQUENCE
# visiblement les données sont pas oufs avant 2021 et après mai 2023
base_finale_reduit <- base_finale[annee >= 2021 & !(annee == 2023 & mois > 5)]

# on ne garde que les gens qui ont au moins un chgt de statut observé
id_assure_ayant_chgt <- base_finale_reduit[, .(modif=max(rleid(statut))), by = "id_assure"][modif >1]$id_assure
base_finale_reduit <- base_finale_reduit[id_assure %in% id_assure_ayant_chgt]

# on vire les trop jeunes et trop vieux
base_id[,annee_naissance := as.integer(substring(date_naissance, 5))]
base_id[, categorie := fcase(annee_naissance >1996, "jeune", annee_naissance < 1960, "vieux", default = "normal")]
base_finale_reduit <- base_id[,.(id_assure, categorie)][base_finale_reduit, on ="id_assure"]

# c'est parti pour l'analyse de séquence
suite <- seqdef(
  data = dcast(base_finale_reduit[categorie =="normal", !c("categorie")], id_assure ~ annee + mois )[,!"id_assure"])
suite.om = seqdist(suite, method ="LCS")

suite.dist <- hclust(as.dist(suite.om), method = "ward.D2")
plot(as.dendrogram(suite.dist), leaflab = "none")
## graphe pour voir le nombre de classes qu'on veut
plot(sort(suite.dist$height, decreasing = TRUE)[1:20], type = "s", xlab = "nb de classes", ylab = "inertie")

## on regarde les ruptures (= marches qui deviennent d'un coup plus petites) et on choisit le nombre de classes associées
nbcl = 4
suite.part = cutree(suite.dist, nbcl)

# analyse graphique
seqdplot(suite, group = suite.part, border = NA)
#seqIplot(suite, group = suite.part)
seqfplot(suite, group = suite.part) # les suites les plus fréquentes
seqmsplot(suite, group = suite.part)
seqrplot(suite, group =suite.part, dist.matrix = suite.om, criterion = "dist")

freq(suite.part)
cprop(table(suite.part, base_id[id_assure %in% unique(base_finale_reduit[categorie=="normal"]$id_assure)]$sexe))


##Agriculteurs
# visiblement les données sont pas oufs avant 2021 et après mai 2023
base_finale_reduit <- base_finale_agr[annee >= 2021 & !(annee == 2023 & mois > 5)]

# on ne garde que les gens qui ont au moins un chgt de statut observé
id_assure_ayant_chgt <- base_finale_reduit[, .(modif=max(rleid(statut))), by = "id_assure"][modif >1]$id_assure
base_finale_reduit <- base_finale_reduit[id_assure %in% id_assure_ayant_chgt]

# on vire les trop jeunes et trop vieux
base_id[,annee_naissance := as.integer(substring(date_naissance, 5))]
base_id[, categorie := fcase(annee_naissance >1996, "jeune", annee_naissance < 1960, "vieux", default = "normal")]
base_finale_reduit <- base_id[,.(id_assure, categorie)][base_finale_reduit, on ="id_assure"]

# c'est parti pour l'analyse de séquence
suite <- seqdef(
  data = dcast(base_finale_reduit[categorie =="normal", !c("categorie")], id_assure ~ annee + mois )[,!"id_assure"])
suite.om = seqdist(suite, method ="LCS")

suite.dist <- hclust(as.dist(suite.om), method = "ward.D2")
plot(as.dendrogram(suite.dist), leaflab = "none")
## graphe pour voir le nombre de classes qu'on veut
plot(sort(suite.dist$height, decreasing = TRUE)[1:20], type = "s", xlab = "nb de classes", ylab = "inertie")

## on regarde les ruptures (= marches qui deviennent d'un coup plus petites) et on choisit le nombre de classes associées
nbcl = 4
suite.part = cutree(suite.dist, nbcl)

# analyse graphique
seqdplot(suite, group = suite.part, border = NA)
#seqIplot(suite, group = suite.part)
seqfplot(suite, group = suite.part) # les suites les plus fréquentes
seqmsplot(suite, group = suite.part)
seqrplot(suite, group =suite.part, dist.matrix = suite.om, criterion = "dist")

freq(suite.part)
cprop(table(suite.part, base_id[id_assure %in% unique(base_finale_reduit[categorie=="normal"]$id_assure)]$sexe))

