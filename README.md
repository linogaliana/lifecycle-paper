# Galiana L., Wilner L. (2023), "Private Wealth over the Life-Cycle: A Meeting between Microsimulation and Structural Approaches"

<div id="badges">
  <a href="https://www.insee.fr/fr/statistiques/6793990">
    <img src="https://img.shields.io/badge/See the Working Paper-red?style=for-the-badge&logo=firefox&logoColor=white" alt="Twitter Badge"/>
  </a>
</div>

This <img height="18" width="18" src="https://cdn.simpleicons.org/r/00ccff99" /> project,
available on [`Github`](https://github.com/linogaliana/lifecycle-paper) collects the main scripts used
for the following working paper: 

> Galiana L., Wilner L. (2023), "Private Wealth over the Life-Cycle: A Meeting between Microsimulation and Structural Approaches"

<details>
<summary>
See abstract
</summary>
This paper embeds a structural model of private wealth accumulation over the life-cycle within a
dynamic microsimulation model (Destinie 2) designed for long-run projections of pensions. In such an
environment, the optimal savings path results from consumption smoothing and bequests motives, on
top of the mortality risk. Preferences are estimated based on a longitudinal wealth survey through a
method of simulated moments. Simulations issued from these estimations replicate quite well a private
wealth that is more concentrated than labor income. They enable us to compute “augmented”
standards of living including capital income, hence to quantify both the countervailing role played by
private wealth to earnings dropout after retirement and the impact of the mortality risk in this regard.
</details>

```
@book{galiana2023private,
  title={Private Wealth over the Life-Cycle: A Meeting between Microsimulation and Structural Approaches},
  author={Galiana, Lino and Wilner, Lionel},
  year={2023},
  publisher={Insee, Institut national de la statistique et des {\'e}tudes {\'e}conomiques}
}
```

## General Project Architecture

This repository centralizes all the final scripts necessary to produce the paper. It utilizes some custom <img height="18" width="18" src="https://cdn.simpleicons.org/r/00ccff99" /> 📦️ containing the functions required for processing and representing data, estimating models, etc. The proliferation of packages addresses the multiple needs of this project and aims to separate specific tasks from general tasks.

See below sections on data access and librairies installation instructions for more details on replication.

## Core of the Project

Paper output have been produced using [`targets`](https://books.ropensci.org/targets/) framework to orchestrate the workflow.

The core of the project consists of five packages

1. `REtage` ([source code](https://github.com/linogaliana/retage)): a module for structuring inheritance data and building the model. This package utilizes the [oglm](https://github.com/linogaliana/oglm) package for estimation (see below).
2. `wealthyR` ([source code](https://github.com/linogaliana/wealthyr)): a module for processing wealth survey data (calculating moments, etc.) and estimating a model through GMM/MD (see `mindist` package below).
3. `capitulation` ([source code](https://github.com/linogaliana/capitulation)): a package for building a wealth microsimulation model (*capit(al) (sim)ulation*) with a lifecycle approach.
4. `oglm` ([source code](https://github.com/linogaliana/oglm.git)): a personal adaptation of the [oglmx](https://cran.r-project.org/web/packages/oglmx/index.html) package, documentation for which can be found [here](https://cran.r-project.org/web/packages/oglmx/vignettes/oglmxVignette.pdf). Since the `oglmx` package is incomplete (missing functions such as `predict`), I began adding functionalities to it.
5. `mindist` ([source code](https://github.com/linogaliana/mindist)): a package for GMM/MD estimation in `R`.

An additional custom package used : 

* [`tablelight`](https://github.com/linogaliana/tablelight): a package for regression tables. `stargazer`, the reference package in the field, does not work well with `oglm` type models; thus, I needed another package for result tables.

All these packages can be installed by the means of `remotes`:

```r
# Order of dependencies matter
remotes::install_github("linogaliana/oglm")
remotes::install_github("linogaliana/mindist")
remotes::install_github("linogaliana/retage")
remotes::install_github("linogaliana/wealthyR")
remotes::install_github("linogaliana/capitulation")
```

## Data used

This paper is based on "Enquête Histoire de vie et patrimoine", the French part of the HCFS.
More information for researchers that would need access is [available here](https://data.progedo.fr/studies/doi/10.13144/lil-1418). 

Another input required to replicate the paper is based on a special
sample of that survey called "Enquête Patrimoine - Version Destinie". It 
is available upon request for research purposes [here](https://data.progedo.fr/studies/doi/10.13144/lil-1265).