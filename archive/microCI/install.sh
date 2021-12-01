#!/bin/bash

R -e "install.packages('oglmx');"
R -e "devtools::install_github('linogaliana/oglm', upgrade = 'never');"
R -e "devtools::install_github('linogaliana/tablelight', upgrade = 'never');"
R -e "devtools::install_github('linogaliana/mindist', upgrade = 'never');"
R -e " \
    devtools::install_git('https://git.stable.innovation.insee.eu/microsimulation/retage', upgrade = 'never')  ; \
    devtools::install_git('https://git.stable.innovation.insee.eu/microsimulation/capitulation', upgrade = 'never')  ; \
    devtools::install_git('https://git.stable.innovation.insee.eu/microsimulation/wealthyr', upgrade = 'never')  ; \
    "