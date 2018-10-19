# *------------------------------------------------------------------
# | PROGRAM NAME: 00_prepare_file_system
# | FILE NAME: 00_prepare_file_system.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  This code installs all packages needed to run the 
# | reproducibility analysis.
# | 
# | 
# |
# *------------------------------------------------------------------

###########################################################################
###  Install packages 
###########################################################################
### Packages from CRAN
install.packages("assertthat")
install.packages("tidyverse")
#install.packages("colorblindr")

install.packages("stringr")
install.packages("ggridges")
install.packages("ggthemes")
install.packages("riverplot")
install.packages("tspmeta")
install.packages("svglite")
install.packages("MultinomialCI")
install.packages("ggsci")
### Packages from GitHub
install.packages("devtools")
require(devtools)

#install_github("jalvesaq/colorout")
install_github("jstagge/staggefuncs")

