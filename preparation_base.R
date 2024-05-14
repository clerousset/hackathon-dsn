install.packages("RPostgres")
install.packages("data.table")
install.packages("DBI")

library("RPostgres")
library("DBI")
library("data.table")

# connexion avec la base de données
con <- dbConnect(RPostgres::Postgres(),
                  host='10.0.0.1',
                  dbname="dsn",
                  user="u262d",
                  password = "063fdd4e96")


# les informations individuelles
base_id <- dbGetQuery(con,"SELECT id as id_assure, sexe, date_naissance FROM ddadtassure
                      ORDER BY id
                      LIMIT 10000")

# on se limte aux identifiant < à une certaine valeur
tail(sort(base_id$id))# 86357


## grosse requête de la base versement
base_versement <- dbGetQuery(con,"SELECT date_versement, montant_net_verse, id_assure, nature_contrat, quotite, quotite_categorie, statut_conventionnel, modalite_temps, sexe, date_naissance, motif_rupture, date_fin_contrat, pcs_ese FROM ddadtversement
                          LEFT JOIN ddadtemployeur_assure ON ddadtversement.id_employeur_assure = ddadtemployeur_assure.id
                          LEFT JOIN ddadtcontrat ON ddadtemployeur_assure.id = ddadtcontrat.id_employeur_assure
                          LEFT JOIN ddadtassure ON ddadtemployeur_assure.id_assure = ddadtassure.id
                           WHERE id_assure < 86357")
setDT(base_versement)  
base_versement[, mois := month(date_versement)][, annee := year(date_versement)]

## création de la vraie base id*mois (un individu peut avoir plusieurs versements le même mois)
base_id_mois <- base_versement[, 
                             .(montant_net_verse = sum(montant_net_verse),
                               quotite = sum(quotite)),                             
                             by=.(id_assure, annee, mois)]

## création de la base individuelle
base_id <- unique(base_id_mois0[, .(id_assure, date_naissance, sexe)])

## préapration de la base cylindrée (pour remplir tous les mois, même qd y a pas de versement pour l'id)
base_cylindree=CJ(id_assure = unique(base_id_mois$id_assure), annee=2019:2023, mois=1:12)

base_cylindree <- base_id_mois[base_cylindree, on = .(id_assure, annee,mois)]

## création de la variable de statut mensuel
base_finale <- base_cylindree[, .(id_assure, annee, mois, statut = fcase(
  is.na(montant_net_verse), "absent",
  quotite >= 150, "temps plein",
  default = "temps partiel"))]scr
