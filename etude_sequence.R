library(TraMineR)
library(questionr)

# visiblement les données sont pas oufs avant 2021 et après mai 2023
base_finale_reduit <- base_finale[annee >= 2021 & !(annee == 2023 & mois > 5)]

suite <- seqdef(
  data = dcast(base_finale_reduit, id_assure ~ annee+mois )[,!"id_assure"])
suite.om = seqdist(suite, method ="LCS")

suite.dist <- hclust(as.dist(suite.om), method = "ward.D2")
plot(as.dendrogram(suite.dist), leaflab = "none")
## graphe pour voir le nombre de classes qu'on veut
plot(sort(suite.dist$height, decreasing = TRUE)[1:20], type = "s", xlab = "nb de classes", ylab = "inertie")

## on regarde les ruptures (= marches qui deviennent d'un coup plus petites) et on choisit le nombre de classes associées
nbcl = 5
suite.part = cutree(suite.dist, nbcl)

# analyse graphique
seqdplot(suite, group = suite.part, border = NA)
#seqIplot(suite, group = suite.part)
seqfplot(suite, group = suite.part) # les suites les plus fréquentes
seqmsplot(suite, group = suite.part)
seqrplot(suite, group =suite.part, dist.matrix = suite.om, criterion = "dist")

freq(suite.part)
cprop(table(suite.part, base_id$sexe))
