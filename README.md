# Scripts pour reproduire les résultats du DT

<!-- badges: start -->
<!-- badges: end -->

# *Executive summary*

1. Lancer un service Rstudio avec [ce lien](https://onyxia.beta.innovation.insee.eu/my-lab/catalogue/inno/rstudio/deploiement?git.enable=true&rstudio.shiny=false&advanced.sudo=false&onyxia.friendly_name=microsimulation&service.mem=12096&rstudio.image_docker=git-registry.stable.innovation.insee.eu/microsimulation/microsimulationci). Le service s'affiche dans le tableau des services disponibles [ici](https://onyxia.beta.innovation.insee.eu/my-lab/mes-services). 

2. Une fois le service démarré, il faut cloner ce dépôt en SSH. La démarche est expliquée *** avec l'adresse suivante <[git@git.stable.innovation.insee.eu:22222]:microsimulation/misc/estimation.git>

3. Le script

# Architecture générale du projet

Ce dépôt centralise l'ensemble des scripts finaux nécessaires pour produire le papier. Il utilise un certain nombre de :package: qui contiennent les fonctions nécessaires pour pouvoir traiter et représenter les données, estimer les modèles, etc. 

Le coeur du projet de microsimulation contient, pour le moment, 3 packages ainsi que 2 packages satellites (bientôt 3). La prolifération des packages répond aux besoins multiples nécessaires dans ce projet et vise à séparer les tâches spécifiques des tâches générales. 

Pour faciliter le traitement, une image `docker` prête à l'emploi a été créée pour permettre une prise en main immédiate du projet. L'ensemble des dépendances nécessaires au projet sont pré-installées et les bases de données sont automatiquement mises à disposition. Le code source de l'image est disponible [ici](https://git.stable.innovation.insee.eu/microsimulation/microsimulationci). Pour lancer un containeur utilisant cette image, il suffit de cliquer sur [ce lien](https://onyxia.beta.innovation.insee.eu/my-lab/catalogue/inno/rstudio/deploiement?git.enable=true&rstudio.shiny=false&advanced.sudo=false&onyxia.friendly_name=microsimulation&service.mem=12096&rstudio.image_docker=git-registry.stable.innovation.insee.eu/microsimulation/microsimulationci) qui lance un service Rstudio avec 12Go de RAM et tous les éléments nécessaires pour pouvoir exécuter les scripts. 

## Coeur du projet

Le coeur du projet contient trois :package:

1. `REtage` ([code source]([git@git.stable.innovation.insee.eu:22222]:microsimulation/retage.git)): un module pour structurer des données d'héritage et construire le modèle. Ce package utilise, pour l'estimation, le package [oglm](https://github.com/linogaliana/oglm) (cf. Section *Packages annexes*)
2. `wealthyR` ([code source]([git@git.stable.innovation.insee.eu:22222]:microsimulation/wealthyr.git)): un module pour traiter les données de l'enquête patrimoine (calculer des moments dessus notamment) et estimer un modèle par GMM/MD (cette partie du :package: sera bientôt isolée dans un :package: spécifique)
3. `capitulation` ([code source]([git@git.stable.innovation.insee.eu:22222]:microsimulation/capitulation.git)): un package pour construire un modèle de microsimulation avec une approche de cycle de vie

## Packages annexes


