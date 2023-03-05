#### Configuraciones iniciales ####

rm(list = ls())
setwd("D:/Cursos22/Analitica2023/Clase7_analitica")
library(tidyverse)
library(grid)
library(gridExtra)
library(sf)
library(readxl)
library(ggrepel)


# Definamos un directorio para guardar los output de mi analisis:
output <- paste0(getwd() , "/output/")

# https://www.datosabiertos.gob.pe/dataset/informaci%C3%B3n-de-fallecidos-del-sistema-inform%C3%A1tico-nacional-de-defunciones-sinadef-ministerio
# Carguemos los datos de sinadef
df <- read.csv("fallecidos_sinadef.csv", sep = "|")


# Carguemos tambien el diccionario del dataset
dict <- read_xlsx("Diccionario_Datos_SINADEF.xlsx")


# Nombres de las columnas 
colnames(df)

#### Variable FECHA ####
str(df)
# 
# Transformemos la columna FECHA a un Date 
df$FECHA <- as.Date(df$FECHA, format = "%Y-%m-%d")
head(df$FECHA)
# 

# Posibles valores de la columna Año
sort(unique(df$AÑO))

# Posibles valores de la columna SEXO
unique(df$SEXO)


# Filtremos la informacion para el año 2020 considerando solo los 
# sexos : M y F 
df <- df %>% 
  filter(FECHA >= as.Date("2020-01-01") & FECHA <= as.Date("2020-12-31"),
         SEXO %in% c("FEMENINO", "MASCULINO") )

# Carguemos el mapa 
departamental <- read_sf("mapasDpto/DEPARTAMENTOS_inei_geogpsperu_suyopomalia.shp")


# NUmero de valores faltantes por columna
colSums(is.na(df))

#### Generacion de graficos ####

# Grafico 1 
graf1 <- df %>% 
  # Transformemos la columna EDAD
  mutate(EDAD = as.numeric(EDAD)) %>% 
  ggplot(aes(x = EDAD, fill = SEXO))+
  geom_histogram(stat = "count")+
  # Personalicemos un poquito 
  scale_fill_manual(values = c("#456F4B", "#D59D1D"))+
  labs(y = "Cantidad de Fallecidos",
       x = "Edad del Fallecido",
       title = "Histograma de la edad de los Fallecidos",
       caption = "Fuente : Portal de Datos Abiertos")+
  facet_grid(SEXO~.)

graf1 <- grid.arrange(graf1,
                      bottom =textGrob(
                        "SINADEF",
                        x = 0.9,
                        hjust = 1,gp = gpar(fontface = "italic",
                                            fontsize = 14)
                      ))  

# Guardemos las salidas en nuestro directorio output
ggsave(filename = paste0(output, "HistogramaEdad_Sexo.png"),
       width = 6,
       height = 10)



# Grafico 2
graf2 <- df %>% 
  count(FECHA, SEXO) %>% 
  ggplot(aes(x = FECHA, y = n))+
  geom_line(aes(col = SEXO))+
  scale_x_date(date_labels = "%b")+
  labs(y = "Numero de Fallecidos", x = "")+
  # Agreguemos una capa theme para modificar la posicion
  # de la leyenda 
  theme(legend.position = "bottom",
        legend.title = element_blank())

graf2

# Grafico 3
graf3 <- df %>% 
  mutate(EDAD = as.numeric(EDAD)) %>% 
  ggplot(aes(x = SEXO, y = EDAD))+
  geom_violin(scale = "count", aes(fill = SEXO))
graf3

# Actualicemos df considerando solo los dptos del peru 
df  <- df %>% filter(DEPARTAMENTO.DOMICILIO %in% departamental$NOMBDEP)
unique(df$DEPARTAMENTO.DOMICILIO)

# Grafico 4
graf4 <- departamental %>% 
  left_join(df %>% count(DEPARTAMENTO.DOMICILIO, name = "DECESOS"),
            by =c("NOMBDEP"= "DEPARTAMENTO.DOMICILIO")) %>% 
  mutate(DECESOS = as.numeric(DECESOS)) %>% 
  ggplot()+
  geom_sf(aes(fill = DECESOS), show.legend = T, colour = "white")+
  geom_label_repel(aes(label = NOMBDEP,
                       geometry = geometry), 
                   size = 2,
                   stat = "sf_coordinates",
                   min.segment.length = 0,
                   label.size = 1,
                   max.overlaps = Inf) +
  scale_fill_viridis_c(trans = "sqrt" ,  alpha = 0.4)+
  theme_void()
ggsave(filename = paste0(output, "NumFallecidos.png"),
       width = 6,
       height = 10)



graf4

unique(df$DEPARTAMENTO.DOMICILIO)