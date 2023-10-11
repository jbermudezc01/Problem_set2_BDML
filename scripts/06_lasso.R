### modelo de predicción: lasso  ##### 

## limpieza y transformación de datos ##

#Limpieza area de trabajo 
rm(list=ls())
cat('\014')

# cargar paquetes 
install.packages("pacman")
library(pacman)
# cargar librerias 
p_load(tidyverse, # Manipular dataframes
       rio, # Importar datos fácilmente
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un polígono
       units, # unidades
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Obtener datos de OpenStreetMap (OSM)
       tidymodels, # Modelado de datos limpios y ordenados
       randomForest, # Modelos de bosque aleatorio
       rattle, # Interfaz gráfica para el modelado de datos
       spatialsample, # Muestreo espacial para modelos de aprendizaje automático
       xgboost,
       scals,
       purr,
       glmnet) 

# cargar base de datos 

bd <- read.csv('https://raw.githubusercontent.com/jbermudezc01/Problem_set2_BDML/main/stores/base_datos_tratada.csv')

# crear subset de entrenamiento
train <-  bd %>%
  subset(type_data == 1)

# crear subset de testeo
test <- bd %>%
  subset(type_data == 2)

# modelo lasso 

# especificación del modelo 

lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_mode("regression") %>%
  set_engine("glmnet") 

# definir intervalo de parametros

penalty_grid <- grid_regular(penalty(range = c(-2, 2)), levels = 50)

# receta de preprocesamiento 

receta <- recipe(formula = price ~ bathrooms+
                   bedrooms+rooms+property_type+parqueadero+
                   terraza+ascensor+deposito+vigilancia+cocina_integral+distancia_parques+
                   distancia_estaciones_tp+distancia_centro_servicios, data = bd) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# crear flujo de trabajo 

lasso_workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(lasso_spec)


# realizar busqueda de hiperparametros utilizando validación cruzada 

# definir las particiones 

df_fold <- vfold_cv(train, v = 5)

# realizar bsuqeuda de hiperparametro 

tune_lasso <- tune_grid(
  lasso_workflow,
  resamples = df_fold, 
  grid = penalty_grid,
  metrics = metric_set(rmse)
)

# graficar resultados de busqueda de hiperparametros

### modelo de predicción: lasso  ##### 

## limpieza y transformación de datos ##

#Limpieza area de trabajo 
rm(list=ls())
cat('\014')

# cargar paquetes 
install.packages("pacman")
library(pacman)
# cargar librerias 
p_load(tidyverse, # Manipular dataframes
       rio, # Importar datos fácilmente
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un polígono
       units, # unidades
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Obtener datos de OpenStreetMap (OSM)
       tidymodels, # Modelado de datos limpios y ordenados
       randomForest, # Modelos de bosque aleatorio
       rattle, # Interfaz gráfica para el modelado de datos
       spatialsample,
       xgboost,
       scals,
       purr,
       glmnet) # Muestreo espacial para modelos de aprendizaje automático

# cargar base de datos 

bd <- read.csv('https://raw.githubusercontent.com/jbermudezc01/Problem_set2_BDML/main/stores/base_datos_tratada.csv')

# crear subset de entrenamiento
train <-  bd %>%
  subset(type_data == 1)

# crear subset de testeo
test <- bd %>%
  subset(type_data == 2)

# modelo lasso 

# especificación del modelo 

lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_mode("regression") %>%
  set_engine("glmnet") 

# definir intervalo de parametros

penalty_grid <- grid_regular(penalty(range = c(-2, 2)), levels= 100)

# receta de preprocesamiento 

receta <- recipe(formula = price ~ bathrooms+
                   bedrooms+rooms+property_type+parqueadero+
                   terraza+ascensor+deposito+vigilancia+cocina_integral+distancia_parques+
                   distancia_estaciones_tp+distancia_centro_servicios, data = bd) %>%
  
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 

# crear flujo de trabajo 

lasso_workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(lasso_spec)


# realizar busqueda de hiperparametros utilizando validación cruzada 

# definir las particiones 

df_fold <- vfold_cv(train, v = 5)

# busqueda de hiperparametros

tune_lasso <- tune_grid(
lasso_workflow,
resamples = df_fold, 
grid = penalty_grid,
 metrics = metric_set(rmse)
)

# realizar bsuqeuda de hiperparametro 

autoplot(tune_lasso)


lasso_fit <- fit(lasso_workflow, data = train)
lasso_fit
