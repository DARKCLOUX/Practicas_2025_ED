
library(tidyr)


# Librerías ---------------------------------------------------------------

library(dplyr)
library(data.table)
library(readxl)
library(ggplot2)

options(scipen = 999)

# Carga de datos ----------------------------------------------------------

datos <- fread(input = "Data/CNA2014_ENCABEZADO_15.csv",
               sep = ",") %>% 
  select(COD_VEREDA,TIPO_UC,S05_TENENCIA,P_S5PAUTOS,P_S7P82,P_S7P84F,P_S7P85B) %>% 
  filter(TIPO_UC == 1) %>% 
  mutate(S05_TENENCIA = as.character(S05_TENENCIA))
str(datos)
glimpse(datos)

# Limpieza ----------------------------------------------------------------

t_homologacion <- read_excel(path = 
            "Data/Tablasdehomologacion.xlsx",
            sheet = "Hoja2") %>% 
            mutate(S05_TENENCIA = as.character(S05_TENENCIA))
t_homologacion

##como se cargo la tabla de homologación
str(t_homologacion)

# tenemos que realizar un ajuste a los datos al inició ya que al cargar la tabla
## de homologación la variable es de tipo "carácter" y en datos S05_TENENCIA es de
## tipo entero (INT) por ende debemos cambiarlo como carácter.


### "NOTA" : DEBEMOS ASEGURARNOS DE QUE QUEDEN AMBAS EN STRING ya sea (int,caract,etc), 
### ESTO SE REALIZO CON "mutate(S05_TENENCIA = as.character(S05_TENENCIA))"


## Unión de tablas e indicación de llaves (se pueden colocar como un vector)

datos_dep <- datos %>%
  left_join(t_homologacion, by = c("S05_TENENCIA"="S05_TENENCIA")) %>% 
  select(Predominancia,P_S7P85B) %>% 
  na.omit()
  
datos_dep

## sea hace con "ctrl+shift+r"

# TDF V. Cualitativa ------------------------------------------------------

## "cumsum" suma acumulada

tdf_S05_TENENCIA <- datos_dep %>% 
  group_by(Predominancia) %>% 
  summarise(n_i = n()) %>% 
  arrange(desc(n_i)) %>% 
  mutate(N_i = cumsum(n_i), f_i = n_i/sum(n_i), F_1 = cumsum(f_i))
  
tdf_S05_TENENCIA

# gráfico 

barplot(table(datos_dep$Predominancia))

# Paquete esquisser

library(esquisse)
library(plotly)

esquisse::esquisser(viewer = "browser")

# TDF - V. Cuantitativa  --------------------------------------------------

library(DT)

# Número de clases

k=round(1+3.3*log10(nrow(datos_dep)))
k

# Construcción del rango (limites)

rango =max(datos_dep$P_S7P85B, na.rm = T) - min(datos_dep$P_S7P85B, na.rm = T)
rango

# Longitud o ancho de cada intervalo

longitud =rango/k
longitud

# Numero de cortes
cortes <- min(datos_dep$P_S7P85B, na.rm = T)+c(seq(0,k,1))*longitud
cortes


## se filtra en datos_deep al inicio la leche "select(predominancia$P_S7P85B)" donde afectara a la 
## variable cualitativa

# TDF - leche

tdf_lech <- datos_dep %>% 
  mutate(P_S7P85B_c = as.factor(cut(P_S7P85B,
                          breaks = cortes,
                          levels = cortes,
                          include.lowest = T, # limites cerrado#
                          dig.lab = 6)
                            )
         ) %>% 
  group_by(P_S7P85B_c, .drop = F, .add = F) %>% # este comando ".drop = F, .add = F" sirve para que tomen los valores ausentes 
  summarise(n_i = n()) %>% 
  mutate(N_i = cumsum(n_i),
         f_i = n_i/sum(n_i),
         F_1 = cumsum(f_i),
         x_i = cortes[1:k]+longitud/2,
         c_i = abs(cortes[1:k] - cortes[2  : (k+1)]),
         d_i = n_i/c_i)

tdf_lech

# Histograma y medidas de tendencia central

hist(datos_dep$P_S7P85B)

mean(datos_dep$P_S7P85B)
median(datos_dep$P_S7P85B)

