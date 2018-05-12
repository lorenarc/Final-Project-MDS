# Para todo el trabajo en R, obtenemos la ruta  y fijamos nuestro directorio de trabajo.
getwd()
setwd("C:/Users/satellite/Final-Project-MDS/R Studio")


# En primer lugar cargamos el dataset, lo cargamos desde la web directamente de la siguiente manera:
who1 <- read.csv('https://query.data.world/s/8Pb1O_ASzuDGfaFfGu6w6kLT6t6IwL', header=TRUE,stringsAsFactors = FALSE)

# Podemos cargarlo también desde la ruta donde hemos guardado el fichero.
who1 <- read.csv('WHO.csv', dec=".",sep=",", header=T, stringsAsFactors = FALSE)

# Visualizamos las columnas que tenemos y hacemos un summary para tener una idea general de los datos.
colnames(who1)
summary(who1)
