#### Configuraciones iniciales ####

rm(list = ls())
setwd("D:/Cursos22/Analitica2023/Clase7_analitica")
library(tidyverse)
library(grid)
library(gridExtra)
library(sf)
library(readxl)


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



