# Distribucion variables de interes ---------------------------------------
# Tipo de propiedad
bd %>% count(property_type)

# Convertir variable <price> a log para escalar graficos 
bd <- bd%>%
  mutate(log_price=log(price))

# Resumen de la distibución de la variable escalado a $
summary(bd$price) %>%
  as.matrix() %>%
  as.data.frame()%>%
  mutate(V1 = scales::dollar(V1))

# Histograma estandarizado en $ 
dist_precio <- ggplot(bd, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.5) +
  labs(x = "price", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar)+
  theme_bw()
ggplotly(dist_precio)

# Relación entre precios y el tipo de propiedad 
ggplot(bd, aes(x = price))+
  geom_freqpoly(aes(color = property_type), binwidth = 5000, linewidth = 0.75) +
  scale_x_continuous(labels = scales::dollar_format()) 

# Relación del precio y el número de habitaciones 
ggplot(bd, aes(x = rooms, y = price)) + 
  geom_boxplot(aes(group = cut_width(rooms, 0.1)))

# Relación del precio y el número de baños 
ggplot(bd, aes(x = bathrooms, y = price)) + 
  geom_boxplot(aes(group = cut_width(bathrooms, 0.1)))

# Visualizar localizacion variables OSM -----------------------------------


# Encontrammos el queremos que sea el centro del mapa  . bd es la base principal
latitud_central  <- mean(bd$lat)
longitud_central <- mean(bd$lon)

# GRAFICAR CORRECTAMENTE , iterando a traves de geometria.osm que tiene los poligonos, y coordenadas.x.centroides
# con coordenadas.y.centroides que tienen las coordenadas de centroides
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = geometria.osm[[3]], col = "red",weight = 10,
              opacity = 0.8, popup = geometria.osm[[1]]$name) %>%
  addCircles(lng = coordenadas.x.centroides[[3]], 
             lat = coordenadas.y.centroides[[3]], 
             col = "darkblue", opacity = 0.5, radius = 1)


# Grafica variables distancias --------------------------------------------
# Distribución de la variable 

#restaurante
distancia_restaurantes <- ggplot(bd, aes(x = distancia_restaurante)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un restaurante en metros", y = "Cantidad",
       title = "Distribución de la distancia a restaurantes") +
  theme_bw()
ggplotly(distancia_restaurantes)
#parques
distancia_parques <- ggplot(bd, aes(x = distancia_parques)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros", y = "Cantidad",
       title = "Distribución de la distancia a parques") +
  theme_bw()
ggplotly(distancia_parques)
#estaciones
distancia_estaciones <- ggplot(bd, aes(x = distancia_estaciones_tp)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una estación de transporte público en metros", y = "Cantidad",
       title = "Distribución de la distancia a una estación de transporte público") +
  theme_bw()
ggplotly(distancia_estaciones)
#mall
distancia_mall <- ggplot(bd, aes(x = distancia_mall)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un centro comercial en metros", y = "Cantidad",
       title = "Distribución de la distancia a un centro comercial") +
  theme_bw()
ggplotly(distancia_mall)
#ciclovias
distancia_ciclovia <- ggplot(bd, aes(x = distancia_ciclovias)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una ciclovia en metros", y = "Cantidad",
       title = "Distribución de la distancia a una ciclovia") +
  theme_bw()
ggplotly(distancia_ciclovia)
# centro servicios
distancia_servicios<- ggplot(bd, aes(x = distancia_centro_servicios)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a centros de servicio en metros", y = "Cantidad",
       title = "Distribución de la distancia a centro de servicios") +
  theme_bw()
ggplotly(distancia_servicios)


