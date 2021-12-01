# microsimulationci: image docker pour reproduire les codes du WP microsimulation

<!-- badges: start -->
[![pipeline status](https://git.stable.innovation.insee.eu/microsimulation/microsimulationci/badges/master/pipeline.svg)](https://git.stable.innovation.insee.eu/microsimulation/microsimulationci/commits/master)
<!-- badges: end -->

Image `docker` pour le projet d'extension du modèle `Destinie` en introduisant l'accumulation de patrimoine et l'héritage. 

:warning: Tout ceci est fondé sur l'ancien onyxia où on pouvait utiliser des
images *custom*

Les packages nécessaires sont pré-installés:

* `REtage`
* `capitulation`
* `wealthyR`
* `mindist`

Ainsi que le package `tablelight` pour les sorties de régression


## Créer un service `Rstudio` avec cette image

L'adresse à renseigner pour utiliser cette image est

```shell
git-registry.stable.innovation.insee.eu/microsimulation/microsimulationci
```

L'image par défaut est l'image de référence. Des versions `dev` (pour les développements en cours pas encore stables) et `seminaireD2E` (image datée à n'utiliser que pour répliquer de vieux résultats) sont disponibles