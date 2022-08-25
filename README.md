# Microsimulation de l'accumulation de patrimoine: scripts principaux

Lino Galiana

# Architecture générale du projet

Ce dépôt centralise l'ensemble des scripts finaux nécessaires pour produire le papier. Il utilise un certain nombre de :package: qui contiennent les fonctions nécessaires pour pouvoir traiter et représenter les données, estimer les modèles, etc. 
La prolifération des packages répond aux besoins multiples nécessaires dans ce projet et vise à séparer les tâches spécifiques des tâches générales. 

## Coeur du projet

Le coeur du projet contient trois :package:

1. `REtage` ([code source](https://git.stable.innovation.insee.eu/microsimulation/retage)): un module pour structurer des données d'héritage et construire le modèle. Ce package utilise, pour l'estimation, le package [oglm](https://github.com/linogaliana/oglm) (cf. Section *Packages annexes*)
2. `wealthyR` ([code source](https://git.stable.innovation.insee.eu/microsimulation/wealthyr)): un module pour traiter les données de l'enquête patrimoine (calculer des moments dessus notamment) et estimer un modèle par GMM/MD (cette partie du :package: sera bientôt isolée dans un :package: spécifique)
3. `capitulation` ([code source](https://git.stable.innovation.insee.eu/microsimulation/capitulation)): un package pour construire un modèle de microsimulation de patrimoine (*capit(al) (sim)ulation*) avec une approche de cycle de vie


## Packages annexes

Les packages suivants sont utilisés:

* [oglm](https://github.com/linogaliana/oglm.git): une adaptation personnelle du package [oglmx](https://cran.r-project.org/web/packages/oglmx/index.html) pour lequel on trouve de la documentation [ici](https://cran.r-project.org/web/packages/oglmx/vignettes/oglmxVignette.pdf). Le package `oglmx` étant incomplet (il manque notamment une fonction `predict`), j'ai commencé à ajouter des fonctionnalités à celui-ci
* [tablelight](https://github.com/linogaliana/tablelight): un package pour avoir des tableaux de régression. `stargazer`, le package de référence dans le domaine, fonctionne mal avec les modèles de type `oglm` ; j'ai donc eu besoin d'un autre package pour faire des tableaux de résultats
* [mindist](https://github.com/linogaliana/mindist) package pour estimation GMM/MD en `R`

## Exécution

- Le `pipeline` principal utilise le package `targets` pour orchestrer
le workflow

