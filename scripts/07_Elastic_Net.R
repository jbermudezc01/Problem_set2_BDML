##########################################################
# Prediccion precios de vivienda
# Autores: Juan Pablo Bermudez. Lina Bautista. Esteban Meza. Pharad Sebastian Escobar
##########################################################


# Cargar pacman (contiene la función p_load)
library(pacman) 

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un poligono
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       osmdata, # Get OSM's data 
       tidymodels) #para modelos de ML

# cargar base de datos 

bd <- read.csv('https://raw.githubusercontent.com/jbermudezc01/Problem_set2_BDML/main/stores/base_datos_tratada.csv')

# crear subset de entrenamiento
train <-  bd %>%
  subset(type_data == 1)

# definimos el data set train como  sf
train_sf <- st_as_sf( 
  train,
  coords = c("lon", "lat"), 
  crs = 4326
)

# crear subset de prueba
test <- bd %>%
  subset(type_data == 2)

# Especificación del modelo
# Mixture puede tomar valores entre 0 y 1, sin embargo tenemos que hacer CV para saber cuál usar,
#Provemos con 0.5
elastic_net_spec <- linear_reg(penalty = lambda, mixture = .5) %>%
  set_engine("glmnet")


# Receta 1
receta <- recipe(formula = price ~ bathrooms+
                   bedrooms+rooms+property_type+parqueadero+
                   terraza+ascensor+deposito+vigilancia+cocina_integral+piso_laminado+distancia_restaurante+
                   distancia_parques+ distancia_estaciones_tp+ distancia_mall+distancia_ciclovias, data = bd) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# Crear un flujo de trabajo 
EN_workflow <- workflow() %>%
  add_recipe(receta) %>%
  add_model(elastic_net_spec)

#____________________________________________________-
# realizar busqueda de hiperparametros utilizando validación cruzada 
# realizar una validación cruzada de bloques espaciales

set.seed(123)
block_folds <- spatial_block_cv(train_sf, v = 5) 

# visualización de plieges 
autoplot(block_folds) # visualización de los plieges 

#NPI excluimos una zona (a), entrenamos las demás ( b,c,d,e) y validamos con (a) 

#NPI walk(block_folds$splits, function(x) print(autoplot(x)))

# Crear pliegues para la validación cruzada

df_fold <- vfold_cv(train, v = 5)



#_______________________________________
# busqueda de hiperparametros 

tune_EN <- tune_grid(
  EN_workflow,
  resamples = df_fold, 
  grid = penalty_grid,
  metrics = "MAE"
)

# seleccionar las mejores estimaciones de parametros

best_parms_EN <- select_best(tune_EN, metric = "MAE")
best_parms_EN

# actulizar parametros con finalize_workflow() 

EN_final <- finalize_workflow(EN_workflow, best_parms_EN)

#ajustar el modelo con los datos de test

EN_final_fit <- fit(EN_final, data = train)

# predecimos el precio para los datos de test 

test <- test %>%
  mutate(price = predict(EN_final_fit, new_data = test)$.pred)

# Exportar a CSV
test %>% 
  select(property_id, price) %>% 
  write.csv(file = "07_EN.csv", row.names = F)










#######################################
#Método de estimación sin recipe y workflow

#Leave one out Cross Validation (LOOCV) espacial con los datos de entrenamiento:
location_folds_train<-spatial_leave_location_out_cv(
  train,
  group = Neighborhood
)
autoplot(location_folds_train)

folds_train<-list()
for(i in 1:length(location_folds_train$splits)){
  folds_train[[i]]<- location_folds_train$splits[[i]]$in_id
}

p_load("caret")
fitControl<-trainControl(method ="cv",
                         index=folds)


#Entrenamos nuestro modelo con "glmnet" controlando por la métrica del Mean Absolute
#Error, mientras miramos una grilla de los hiperparámetros alpha y lambda
EN<-train(log(Sale_Price) ~ Gr_Liv_Area  +  Bldg_Type ,
          data=ames_sf,
          method = 'glmnet',
          trControl = fitControl,
          metric="MAE",
          tuneGrid = expand.grid(alpha =seq(0,1,length.out = 20),
                                 lambda = seq(0.001,0.2,length.out = 50))
)

EN$bestTune
round(EN$results$RMSE[which.min(EN$results$lambda)],4)

test$log_price_hat<-predict(EN_tp,newdata = test)
test<- test  %>% mutate(price_hat=exp(log_price_hat))
head(test  %>% select(Sale_Price,log_price_hat,price_hat)  %>% st_drop_geometry())



#Métricas de desempeño de prueba (MAE=observada-estimada) 
mean(abs(test$Sale_Price-test$price_hat))
mean(abs(test$Sale_Price-round(test$price_hat)))