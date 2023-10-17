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
