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

# Graficas espaciales -----------------------------------------------------

leaflet() %>%
  addTiles() %>%
  addCircles(lng = as.numeric(unlist(purrr::map(restaurantes$osm_points$geometry, ~.x[1]))), 
             lat = as.numeric(unlist(purrr::map(restaurantes$osm_points$geometry, ~.x[2]))))

# visulizar su localización 

# crear puntos

#restaurantes
puntos_restaurantes <- restaurantes$osm_points
head(puntos_restaurantes)
#parques
puntos_parques <- parques$osm_points
head(puntos_parques)
#estaciones
puntos_estaciones <-estaciones$osm_points
head(puntos_estaciones)
#mall
puntos_mall <- mall$osm_points
head(puntos_mall)
#ciclovia
puntos_ciclovia <- ciclovias$osm_points
head(puntos_ciclovia)
# centros_servicios
puntos_servicios <- centro_servicios$osm_points
head(puntos_servicios)

# crear grafico de localización 

#restuarantes
ggplot()+
  geom_sf(data=puntos_restaurantes)+
  theme_bw()
#parques
ggplot()+
  geom_sf(data=puntos_parques)+
  theme_bw()
#estaciones
ggplot()+
  geom_sf(data=puntos_estaciones)+
  theme_bw()
#mall
ggplot()+
  geom_sf(data=puntos_mall)+
  theme_bw()
#ciclovias
ggplot()+
  geom_sf(data=puntos_ciclovia)+
  theme_bw()
#servicios
ggplot()+
  geom_sf(data=puntos_servicios)+
  theme_bw()
