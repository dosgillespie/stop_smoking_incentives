
# The aim of this code is to install the required packages

installed_devtools <- "devtools" %in% rownames(installed.packages())
if (installed_devtools == FALSE) {
  install.packages("devtools", lib = project_lib)
}

###########################
# CRAN packages installation

packages <- c("rmarkdown", "data.table", "ggplot2", "devtools", "cowplot", "readxl", "knitr", "stringr", "here", "magrittr", "RColorBrewer", "testthat", 
              "fitdistrplus", "fastDummies", "flextable", "bookdown", "viridis", "rmarkdown", "TTR", "boot", "VGAM", "ggeffects", "srvyr",
              "readr", "bit64", "writexl", "Rfast", "dvmisc", "fastmatch", "dplyr", "plyr", "openxlsx", "raster", "mice", "Hmisc",
              "nnet", "quantmod", "Matrix", "codetools", "nlme", "tibble", "ggthemes", "foreign", "splines", "broom", "survey", "ggsci", 
              "shiny", "tidyverse", "gt", "extrafont", "forcats", "snakecase", "paletteer", "scales", "ggtext", "car", "plotly", "StatMatch",
              "ggpubr", "seecolor", "gganimate", "nnet", "colorspace", "mice", "reformulas")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], lib = project_lib)
}


####################
# Make a separate library for the packages that quarto needs 
# as it doesnt like UNC paths

# Create directories if they don't already exist 
project_lib_quarto <- "C:/Users/cm1dog/Documents/_R_packages"

sapply(project_lib_quarto, function(fpath) {
  outputDir <- fpath
  if(!dir.exists(here::here(outputDir))) {dir.create(here::here(outputDir))}
})

packages <- c("rmarkdown", "data.table", "ggplot2", "devtools", "cowplot", "readxl", "knitr", "stringr", "here", "magrittr", "RColorBrewer", "testthat", 
              "fitdistrplus", "fastDummies", "flextable", "bookdown", "viridis", "rmarkdown", "TTR", "boot", "VGAM", "ggeffects", "srvyr",
              "readr", "bit64", "writexl", "Rfast", "dvmisc", "fastmatch", "dplyr", "plyr", "openxlsx", "raster", "mice", "Hmisc",
              "nnet", "quantmod", "Matrix", "codetools", "nlme", "tibble", "ggthemes", "foreign", "splines", "broom", "survey", "ggsci", 
              "shiny", "tidyverse", "gt", "extrafont", "forcats", "snakecase", "paletteer", "scales", "ggtext", "car", "plotly", "StatMatch",
              "ggpubr", "seecolor", "gganimate", "nnet", "colorspace", "mice", "reformulas")

install.packages(packages, lib = project_lib_quarto)

.libPaths("C:/Users/cm1dog/Documents/_R_packages")

