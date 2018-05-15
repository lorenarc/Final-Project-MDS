# Para todo el trabajo en R, obtenemos la ruta  y fijamos nuestro directorio de trabajo.
getwd()
setwd("C:/Users/satellite/Final-Project-MDS/R Studio")

# Instalamos y cargamos algunos paquetes que van a ser de utilidad en el tratamiento de data tables
install.packages("dplyr", dependencies = TRUE)
install.packages("data.table", dependencies = TRUE)

library("dplyr","data.table")

# Ahora cargamos el dataset, lo hacemos desde la web directamente de la siguiente manera:
who1 <- read.csv('https://query.data.world/s/8Pb1O_ASzuDGfaFfGu6w6kLT6t6IwL', header=TRUE,stringsAsFactors = FALSE)

# Podemos cargarlo también desde la ruta donde hemos guardado el fichero.
who1 <- read.csv('WHO.csv', dec=".",sep=",", header=T, stringsAsFactors = FALSE)

# Visualizamos las columnas que tenemos y hacemos un summary para tener una idea general de los datos.
colnames(who1)
summary(who1)
summary(who1$Gross.national.income.per.capita..PPP.international...)
head(who1)

# La columna Country la ponemos como nombre de las filas para poder trabajar mejor con el 
# fichero y una vez hecho borramos la columna.
# Renombramos "GNI per capita" para que tenga un nombre más accesible.

row.names(who1) <- who1$Country
who1 <- subset(who1,select=-Country)

names(who1)[5] <- "GNIPPP"

# Las características que tengan un 75% de datos perdidos (NA's más de 151) las borramos.


#who1 %>% 
  #group_by(Continent) %>% 
  #summarise_if(is.numeric,median,na.rm = TRUE)












  