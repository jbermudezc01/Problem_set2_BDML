# Problem Set 2

Autores: Juan Pablo Bermudez, Lina Bautista, Esteban Meza, Pharad Sebastian Escobar.

El siguiente repositorio contiene los codigos, imagenes, bases de datos y el documento final del segundo taller de Big Data y Machine Learning para Economia Aplicada. Se intenta predecir precios de viviendas de Chapinero usando varios modelos de Machine Learning, como Ridge, Lasso, Elastic Net, y modelos basados en arboles.

El repositorio contiene cinco carpetas clave:

- `document`: contiene el documento final en formato `latex` y `pdf`, y todos los subproductos necesarios para renderizar el archivo `pdf`. El mismo archivo latex es capaz de obtener las graficas directamente de la carpeta `views`.
- `scripts`: contiene todos los scripts utilizados para el taller, completamente reproducibles. La mayoria de los codigos son en lenguaje `R`, a excepcion de los codigos que realizan KNN para intentar imputar los valores faltantes, los cuales estan en lenguaje `python`.
- `stores`: contiene todas las bases necesarias para la reproduccion del repositorio. Dos de ellos fueron descargados directamente de kaggle: test.csv y train.csv. Por otro lado, tambien hay datos en formato geojson de las estaciones de transmilenio y SITP. El resto de bases o son productos intermedios de los codigos, o fueron descargados de la pagina de datos abiertos de bogota, para lo cual tambien agregamos una carpeta llamada `Bases_Bogota`. La base de datos final que utilizamos en las regresiones esta dentro del archivo `Datos_limpios.RData`.
- `views`: contiene todas las figuras realizadas para el taller. Contiene tres carpetas: `graficas_descriptivas`, la cual contiene principalmente histogramas de la variable de precios, y de las variables de distancia utilizadas para la regresión; `mapas`, la cual contiene todos los mapas de las variables geograficas que agregamos en las regresiones; y, por ultimo, `html_temp`, la cual contiene informacion necesaria para la produccion de los mapas. Es importante resaltar que todos los archivos de `html_temp` fueron producidos por una funcion de R, la cual convierte los mapas en formato `html` para poder transformarlos en `png`.

