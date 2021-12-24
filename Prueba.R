# NICOLAS SILVA #
# PRUEBA N°2 #
# BIG DATA #

# Librerias para leer textos html.
# es solo "rvest", "xml2" esta solo para complementar porciacaso.

library(xml2)
library(rvest)

# Creacion de funciones para que itere la accion de obtener los datos por paguina.
# En otras palabras Generaliza la obtencion de informacion.

# REPARTO DE VOCES #

obteniendoRepartos <- function(linkSubPagina1){
  
  resultadoRepartos <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina1)
  
  boxrepartoAnimes <- html_nodes(subPaginaOtakusTV, css=".carusel_voces")
  repartoAnimes <- html_nodes(boxrepartoAnimes, css= ".mb-1")
  resultadoRepartos <- html_text(repartoAnimes)
  print(resultadoRepartos)
  
  return(resultadoRepartos)
  
}

# TEMPORADAS #

obteniendoNtemporadas <- function(linkSubPagina2){
  
  resultadoNtemporadas <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina2)
  
  box1 <- html_nodes(subPaginaOtakusTV, xpath="//*[@id=\"back_data_perfil\"]/div")
  Ntemporadas <- html_nodes(box1,css="h2")
  resultadoNtemporadas <- html_text(Ntemporadas)
  
  return(resultadoNtemporadas)
  
}

# ESTADO DEL ANIME# 

obteniendoEanimes <- function(linkSubPagina3){
  
  resultadoEanimes <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina3)
  estadoAnimes <- html_nodes(subPaginaOtakusTV,xpath="//*[@id=\"back_data_perfil\"]/div/div/div[3]/div/span")
  
  resultadoEanimes <- html_text(estadoAnimes)
  
  return(resultadoEanimes)
  
}

# FECHA ESTRENO #

obteniendoFestrenos <- function(linkSubPagina4){
  
  resultadoFestrenos <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina4)
  
  fechaAnimes <- html_nodes(subPaginaOtakusTV,xpath="//*[@id=\"back_data_perfil\"]/div/div/div[3]/div/div/span")
  resultadoFestrenos <- html_text(fechaAnimes)
  
  return(resultadoFestrenos)
  
}

# DIRECTOR # 

obteniendoDirectores <- function(linkSubPagina5){
  
  resultadoDirectores <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina5)
  
  Directores <- html_nodes(subPaginaOtakusTV,xpath="//*[@id=\"nav-informacion\"]/div[1]/div/div/p[1]/span[2]")
  resultadoDirectores <- html_text(Directores)
  return(resultadoDirectores)
  
}

# CREADOR #

obteniendoCreadores <- function(linkSubPagina6){
  
  resultadoCreadores <- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina6)
  Creadores <- html_nodes(subPaginaOtakusTV,xpath="//*[@id=\"nav-informacion\"]/div[1]/div/div/p[2]/span[2]")
  resultadoCreadores <- html_text(Creadores)
  return(resultadoCreadores)
  
}

# TITULO ALTERNATIVO #

obteniendoTalternativos <- function(linkSubPagina7){
  
  resultadoTalternativos<- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina7)
  
  TituloAlternativos <- html_nodes(subPaginaOtakusTV,xpath="//*[@id=\"nav-informacion\"]/div[1]/div/div/p[3]/span[2]")
  resultadoTalternativos <- html_text(TituloAlternativos)
  return(resultadoTalternativos)
  
}

# PALABRAS CLAVES #

obteniendoPclaves<- function(linkSubPagina8){
  
  resultadoPclaves<- NA
  
  subPaginaOtakusTV <- read_html(linkSubPagina8)
  box5 <- html_nodes(subPaginaOtakusTV,xpath="/html/body/div[3]/div")
  palabrasClaves <- html_nodes(box5,css=".col-6")
  resultadoPclaves <- html_text(palabrasClaves)
  
  return(resultadoPclaves)
  
}

# Creacion de conjuntos vacios para que la informacion recopilada se almacene aqui.

reparto_Animes <-c()
N_temporadas <-c()
estado_Animes <-c()
fecha_Animes <-c()
Directores_ <-c()
Creadores_ <-c()
Titulo_Alternativos <-c()
palabras_Claves <-c()
Descripciones_ <- c()
Titulos_ <- c()
Ncapitulos <- c() 

 # Obtencion para cada N° de paginas del catalogo.

for (pagina in 1:3){
  
  OtakusTV <- read_html(paste("https://www.otakustv.com/animes?page=",pagina,sep = ""))
  
  # Obteniendo la clase lista de animes #
  
  Animes <- html_nodes(OtakusTV, css=".animes_lista")
  
  # Obteniendo el Link #
  
  seccionImagenAnimes <- html_nodes(Animes,css=".col-6")
  linkAnimes <- html_nodes(seccionImagenAnimes,css="a")
  hrefAnimes <- html_attr(linkAnimes,"href")

  # Obteniendo la Descripcion #

  descripcionAnime <- html_attr(linkAnimes,"data-original-title")
  Descripciones_ <- c(Descripciones_,descripcionAnime)
  
  # Obteniendo Titulos #

  titulosAnime <- html_nodes(seccionImagenAnimes, css =".mb-1")
  titulosAnime <- html_text(titulosAnime)
  Titulos_ <- c(Titulos_,titulosAnime)

  # Obteniendo Capitulos #

  capitulosAnime <- html_nodes(seccionImagenAnimes, css=".bog")
  capitulosAnime <- html_text(capitulosAnime)
  Ncapitulos <- c(Ncapitulos,capitulosAnime)
  Ncapitulos <- gsub("Película .","",Ncapitulos)
  
# Para cada elemento de hrefAnime se vera expuesto y se guardara en la variable concadenada.

for  ( hrefAnime in hrefAnimes) {
  
  # obteniendo Repartos #
  
  repartos <- obteniendoRepartos(hrefAnime)
  reparto_Animes <-c (reparto_Animes,repartos)
  
  
  # obteniendo Ntemporada #
  
  ntemporadas <- obteniendoNtemporadas(hrefAnime)
  N_temporadas <- c(N_temporadas,ntemporadas)
 
  # obteniendo EstadoAnime #
  
  Eanimes <- obteniendoEanimes(hrefAnime)
  estado_Animes <-c(estado_Animes,Eanimes)
  estado_Animes <- gsub("\n","",estado_Animes)
  
  # obteniendo Fecha #
  
  fechasE <- obteniendoFestrenos(hrefAnime)
  fecha_Animes <-c(fecha_Animes,fechasE)
  fecha_Animes  <- gsub("Estreno:","",fecha_Animes)
 
  
  # obteniendo Director #
  
  Danimes <- obteniendoDirectores(hrefAnime)
  Directores_ <-c(Directores_,Danimes)
  
  # obteniendo Creador #
  
  Canimes <- obteniendoCreadores(hrefAnime)
  Creadores_ <-c(Creadores_,Canimes)
  
  # obteniendo Titulo Alternativo #
  
  Tituloalt <- obteniendoTalternativos(hrefAnime)
  Titulo_Alternativos <-c(Titulo_Alternativos,Tituloalt)
  
  # obteniendo Palabras Claves #
  
  Pclaves <- obteniendoPclaves(hrefAnime)
  palabras_Claves <-c(palabras_Claves,Pclaves)

   }
}

#Se utiliza gsub para eliminar partes inecesarias dentro de la lectura html,
# hubieron partes que fue dificil quitar como Episodios o TEMPORADA(S),
# aunque despues se reconociera como numerico.

# Para verificar por separado #

print(Ncapitulos)
print(Titulos_)
print(Descripciones_)
print(palabras_Claves)
print(Titulo_Alternativos)
print(Creadores_)
print(Directores_)
print(fecha_Animes)
print(estado_Animes)
print(N_temporadas2)
print(reparto_Animes)

# Arreglos #

# este no debe hacerse pero para objeto de estudio se utilizara #

Elementofaltante <- c("0 TEMPORADA(S) DISPONIBLE(S)")
print(Elementofaltante)

N_temporadas2 <- c(N_temporadas, Elementofaltante)

# Creacion del dataframe con todos la informacion obtenida.

# No se agrego el reparto_Animes ni Palabras_Claves debido a su diferencia de filas,
# e incluso al realizar el for que ayudo a contruir usted,
# este repetia los datos para cada uno de los actores de voz.

DataOtakusTV <- data.frame(Nombre=Titulos_,Episodios=Ncapitulos,
                           Resumen=Descripciones_,Talternativo=Titulo_Alternativos,
                           Creadores=Creadores_,Directores=Directores_,Estreno=fecha_Animes,
                           Estado=estado_Animes,Temporadas=N_temporadas2)

# Guardado en formato csv2 del dataframe creado.

write.csv2(DataOtakusTV,"DataOtakusTV.csv")

# Lectura del dataframe guardado.

Datos <- read.csv2("DataOtakusTV.csv")
Datos <- select(Datos,-X)

# librearias para el analisis de los datos.

#"lubridate" ayuda a que las fechas tales como las que tenemos en el dataframe
# puedan interpretarse con mayor facilidad, ya que Rstudio tiene un orden para ello.

library(lubridate)

# "dplyr" ayuda a manipular los dataframes y realizar acciones 
# para ajustarlos a nuestras necesidades.

library(dplyr)

# "ggplot2" ayuda a visualizar los datos creando
# graficos de forma declarativa, o sea, uno le proporciona los datos,
# se le dice como asignar variables y se encarga de los detalles.

library(ggplot2)


# Grafico de animes por año #
# Aqui podemos apreciar cuantos animes se han estrenados por año y con los datos obtenidos
# se aprecia que se han estrenados mas animes con el paso de los años.

Datos <- Datos %>%
  mutate(Estreno = mdy(Estreno))

 Grafico_1 <- Datos %>%
  mutate(anio = year(Estreno),cuenta = 1) %>%
  filter(anio > 1993) %>%
  group_by(anio) %>%
  summarise(total= sum(cuenta)) %>%
  ggplot(aes(x = anio, y= total)) + geom_line() + labs(title ="animes por año", x= "", y= "")
  

# Grafico de animes por dia #
# Este grafico es parecido al de arriba, pero en la industria del anime la mayoria emite
# un episodio semanalmente, por lo que podemos conocer que dia de la semana se emitian.

 Grafico_2 <-Datos %>%
  mutate(dia = wday(Estreno),cuento = 1) %>%
  group_by(dia) %>%
  summarise(totales= sum(cuento)) %>%
  ggplot(aes(x = dia, y= totales)) + geom_point() + labs(title ="animes por dia", x= "", y= "")


# Grafico de Adaptaciones por creador #
# Para este grafico se puede apreciar las adaptaciones de cada creador dentro
# de la informacion recopilada, destacando que existen muchos N/N ya que la pagina asi 
# muestra su falta de informacion. 

Grafico_3 <- ggplot(Datos, aes(x = Creadores , y = Temporadas, colour = Estado)) + 
  geom_point() + labs(title ="Adaptaciones por creador", x= "", y= "")
Grafico_3 <- Grafico_3 + facet_grid(. ~ Estado)

# Grafico de conteo para los episodios #
# Un grafico  mas simple pero que explica cuantos animes tienen cierta cantidad de episodios,
# por lo que se destacan los que tienen 12.

Grafico_4 <- ggplot(Datos, aes(x=Episodios)) + geom_bar() + coord_flip() + ggtitle("Conteo de episodios")


# Grafico torta para los estados #
# Grafico que antes de ser realizado se extrajo informacion sobre la variable "Estado",
# obteniendo el porcentaje de cada uno de esos datos dentro del total,
# pra luego realizar este grafico explicativo. Se resalta que Finalizado sale en 2 ocaciones,
# debido a que un Finalizado contiene un espacio en la escritura y el programa lo reconoce como diferente.

porcentaje <- Datos %>%
  group_by(Estado) %>%
  count() %>%
  ungroup() %>%
  mutate(percentage=`n`/sum(`n`) * 100)

Grafico_5 <- ggplot(porcentaje, aes(x=1, y=percentage, fill=Estado)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = paste0(round(percentage,1),"%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") + 
  theme_void() + scale_fill_brewer(palette="RdGy")

# Grafico en barra para los estados #
# Misma utilizacion de los porcentajes de los estados del anime, pero en barra.

Grafico_6 <- ggplot(porcentaje, aes(x=Estado, y=n, fill=percentage)) + 
  geom_bar(stat="identity", position="dodge")

# Grafico de participacion en las obras
# Aqui se puede realizar lo mismo que con "Estado" transformandolo a porcentaje y
# revisar su participacion dentro del total de las obras.

Grafico_7 <- ggplot(Datos, aes(x=Directores)) +
  geom_bar() +
  coord_flip()

porcentaje2 <- Datos %>%
  group_by(Directores) %>%
  count() %>%
  ungroup() %>%
  mutate(percentage=`n`/sum(`n`) * 100)

# Aqui la demostracion, pero debe quitarse el ultimo paquete de colores
# debido a que el maximo que contenia era 11.

Grafico_8 <- ggplot(porcentaje2, aes(x=1, y=percentage, fill=Directores)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = paste0(round(percentage,1),"%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") + 
  theme_void() 

# Analisis de texto #
# Pequeño analisis de texto que por tiempo no se complemento con funciones 
# Ya que, tiene un conjunto dentro de un conjunto por lo que es mas dificil.
# Pero despues de realizarlo se pueden obtener palabras claves, como por ejemplo 
# la palabra "JOVEN" o "ESCUELA" o "ESPADACHIN" interpretando genero o tematica.

Descripciones_2 <- c()

for (pagina in 1:3){
  
  OtakusTV <- read_html(paste("https://www.otakustv.com/animes?page=",pagina,sep = ""))
  
  # Obteniendo la clase lista de animes
  
  Animes <- html_nodes(OtakusTV, css=".animes_lista")
  
  # Obteniendo el Link
  seccionImagenAnimes <- html_nodes(Animes,css=".col-6")
  linkAnimes <- html_nodes(seccionImagenAnimes,css="a")
  hrefAnimes <- html_attr(linkAnimes,"href")
  
  # Obteniendo la Descripcion
  
  descripcionAnimes <- html_attr(linkAnimes,"data-original-title")
  Descripciones_2 <- c(Descripciones_2,descripcionAnimes)
  Descripciones_2<- tolower(Descripciones_2)
  Descripciones_2 <- gsub("\n"," ",Descripciones_2)
  Descripciones_2 <- gsub("[.]"," ",Descripciones_2)
  Descripciones_2 <- gsub(","," ",Descripciones_2)
}        


print(Descripciones_2)

parrafo <- strsplit(Descripciones_2," ")[[1]]
parrafo <- parrafo[parrafo != 'de']
parrafo <- parrafo[parrafo != 'los']
parrafo <- parrafo[parrafo != 'un']
parrafo <- parrafo[parrafo != 'y']
parrafo <- parrafo[parrafo != 'la']
parrafo <- parrafo[parrafo != 'es']
parrafo <- parrafo[parrafo != 'a']
print(parrafo)

table(Descripciones_2)

Dataparrafo <- data.frame(palabras=parrafo)


# haciendo un grafico

Grafico_9 <- ggplot(Dataparrafo,aes(x=palabras)) +
  geom_bar() +
  coord_flip()

