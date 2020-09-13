#--------------------------------------------------------------------------------------------------- #
#  ____    _                    _           ____                    __   _   _        
# / ___|  | |_    __ _   _ __  | |_   _    |  _ \   _ __    ___    / _| (_) | |   ___ 
# \___ \  | __|  / _` | | '__| | __| (_)   | |_) | | '__|  / _ \  | |_  | | | |  / _ \
#  ___) | | |_  | (_| | | |    | |_   _    |  __/  | |    | (_) | |  _| | | | | |  __/
# |____/   \__|  \__,_| |_|     \__| (_)   |_|     |_|     \___/  |_|   |_| |_|  \___|
#--------------------------------------------------------------------------------------------------- #



#if (!require("Rcpp")){install.packages('Rcpp',repos='http://cran.us.r-project.org');library("Rcpp")}
if (!require("Rcpp")){install.packages('Rcpp',quiet=T);library("Rcpp")}
if (!require("e1071")){install.packages('e1071');library("e1071")}
if (!require("grid")){install.packages("grid",quiet=T) ; library("grid")}
if (!require("stringi")){install.packages("stringi",quiet=T) ; library("stringi")}
if (!require("dplyr")){install.packages("dplyr",quiet=T) ; library("dplyr")}
if (!require("recipes")){install.packages("recipes",quiet=T) ; library("recipes")}
if (!require("caret")){install.packages("caret",quiet = T) ; library("caret")}
if (!require("RSNNS")){install.packages("RSNNS",quiet=T) ; library("RSNNS")}
if (!require("imager")){install.packages("imager",quiet=T) ; library("imager")}


#----- Corra estas lineas si no tiene instalada la librer?a rhdf5 -----#
#source("http://bioconductor.org/biocLite.R")
#biocLite("rhdf5")
library(rhdf5)
#---------- Fin: Corra estas lineas en una sola ejecucion -------------#





print("All packages and functions have been installed or loaded...")



#----------------------------------------------------------------------------------- #
#  _____               _         ____                    __   _   _        
# | ____|  _ __     __| |  _    |  _ \   _ __    ___    / _| (_) | |   ___ 
# |  _|   | '_ \   / _` | (_)   | |_) | | '__|  / _ \  | |_  | | | |  / _ \
# | |___  | | | | | (_| |  _    |  __/  | |    | (_) | |  _| | | | | |  __/
# |_____| |_| |_|  \__,_| (_)   |_|     |_|     \___/  |_|   |_| |_|  \___|
#----------------------------------------------------------------------------------- #