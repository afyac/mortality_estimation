# List of required packages
list_packages <- c("shiny", "rio", "dplyr", "tidyr", "lubridate", "ggplot2", "shinycssloaders", "shinydashboard",
                   "signal", "geomtextpath", 'MASS', 'zscorer', 'tidyverse', 'readxl', 'nipnTK', 'litteR', 'stringr', 
                   'survey', 'stringi', 'anytime', "flextable", "ggplot2", "ggpubr", "gtools", "lhs", 
                     "paletteer", "parameters", "readxl", "reshape2", "scales", 'glmmTMB',
                   "tidyverse", "ggthemes", 'broom', 'reshape', 'zoo', 'mice', 'sf', 'skimr', 
                   'flextable', 'ggalt', 'ggcorrplot', 'insight', 'pbmcapply', 'janitor', 'remotes')

# Install any packages not yet installed
x2 <- list_packages %in% row.names(installed.packages())
if (any(x2 == FALSE)) { install.packages(list_packages[! x2]) }

# Load all packages    
lapply(list_packages, library, character.only = TRUE)

## Install MAST from github
if(!('mast' %in% list_packages %in% row.names(installed.packages()))){
  remotes::install_github("afyac/mast", upgrade = FALSE)
}

## Please check if you have the right version of the Matrix Package (1.6-4)
## Else uncomment the following line and run it
# install.packages("Matrix", dependencies = TRUE, input='source')
