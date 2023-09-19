##########################################################
# Prediccion precios de vivienda
# Autores: Juan Pablo Bermudez. Lina Bautista. Esteban Meza. Pharad Sebastian Escobar
##########################################################

# Limpiar environment -----------------------------------------------------
rm(list = ls())
cat('\014')

# Librerias ---------------------------------------------------------------
library(pacman)
p_load(tidyverse)

# Directorios -------------------------------------------------------------
stores <- paste0(getwd(),'/stores/') # Directorio de base de datos
views  <- paste0(getwd(),'/views/')  # Directorio para guardar imagenes

# Lectura de datos --------------------------------------------------------
train_data <- read_csv(paste0(stores,'train.csv'))
test_data  <- read_csv(paste0(stores,'test.csv'))
