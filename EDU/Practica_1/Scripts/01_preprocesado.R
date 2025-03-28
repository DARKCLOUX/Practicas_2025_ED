

# Librerías ---------------------------------------------------------------

library(dplyr)
library(data.table)
library(readxl)
library(ggplot2)

options(scipen = 999)

# Carga de datos ----------------------------------------------------------

datos <- fread(input = "Data/S01_15_Unidad_productora_.csv",
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

# esquisse::esquisser(viewer = "browser") 
# Grafico de disperción


Den_lech <- ggplot(datos_dep) +
  aes(x = P_S7P85B) +
  geom_density(adjust = 6L, fill = "#C31010") +
  labs(x = "Producción de leche", y = "Densidad ") +
  theme_light() +
  theme(
    axis.title.y = element_text(size = 16L,
                                face = "bold"),
    axis.title.x = element_text(size = 17L,
                                face = "bold"),
    axis.text.y = element_text(size = 13L),
    axis.text.x = element_text(size = 15L)
  )

Den_lech


Gf_dis_caj <- ggplot(datos_dep) +
  aes(x = "", y = P_S7P85B) +
  geom_boxplot(fill = "#D8D214") +
  labs(
    y = "Precio de la leche (Litros)",
    title = "Diagráma de caja - Distribución de la leche"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16L,
                              face = "italic"),
    axis.title.y = element_text(size = 15L,
                                face = "bold"),
    axis.title.x = element_text(size = 15L),
    axis.text.y = element_text(size = 14L),
    axis.text.x = element_text(size = 14L)
  )

Gf_dis_caj

Gr_P <- ggplot(datos_dep) +
  aes(x = Predominancia) +
  geom_bar(fill = "#54FF9F") +
  labs(
    x = "Caracterización de tenencia",
    y = "Número de UPA(registros)",
    title = "Gráfico de distribución de Predominancia"
  ) +
  coord_flip() +
  theme_light() +
  theme(
    plot.title = element_text(size = 13L,
                              face = "bold.italic"),
    axis.title.y = element_text(size = 12L,
                                face = "bold.italic"),
    axis.title.x = element_text(size = 15L,
                                face = "bold.italic"),
    axis.text.y = element_text(size = 15L),
    axis.text.x = element_text(face = "bold",
                               size = 11L)
  )

Gr_P


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


# medidas de tendencia central --------------------------------------------


media <- mean(datos_dep$P_S7P85B, na.rm = T)
mediana <- median(datos_dep$P_S7P85B, na.rm = T)  

# moda: datos discretos o valores agrupados en intervalos  

# medidas de posición (Quantiles)
# seq(0,1,0,1) - sirve para ver deciles

Q1 <- quantile(datos$P_S7P85B, na.rm = T,
               probs = 0.25, type = 6)
Q2 <- quantile(datos$P_S7P85B, na.rm = T,
               probs = 0.505, type = 6)
Q3 <- quantile(datos$P_S7P85B, na.rm = T,
               probs = 0.75, type = 6)

deciles <- quantile(datos_dep$P_S7P85B, na.rm = T,
                   probs = seq(0,1,0.1),
                   type = 6)
deciles

percentiles <- quantile(datos_dep$P_S7P85B, na.rm = T,
                        probs = seq(0,1,0.01),
                        type = 6)

percentiles
# "por de bajo de 41 litros esta el 90% de las upas" 

# medidas de variabilidad 
## rango intercuantil
riq <- Q3 - Q1

## Desviación absoluta media
des_abs <- mean(abs(datos_dep$P_S7P85B-media))
des_abs
## varianza
varianza <- mean((datos_dep$P_S7P85B-media)^2)
### var(datos_dep$P_S7P85B) esta no se usa por que tenemos una población

## desviación standar 
desv_st <-sqrt(varianza)
desv_st
###el valor promedio de el litro

## recorrido relativo
recorrido_r <- rango/media

## recorrido semiintercuartilico
rrecorrido_semi <- (Q3-Q1)/(Q1+Q3)

# Coeficiente de variación de pearson
CV <- 100*desv_st/abs(media) # si es mayor del 30%, alta variabilidad 

# Tabla de resumen de los cinco números 
tb_resumen <- data.frame(
  minimo = min(datos_dep$P_S7P85B, na.rm = T),
  Q1 = Q1,
  media = media,
  mediana = mediana,
  Q2 = Q2,
  Q3=Q3,
  maximo =max(datos_dep$P_S7P85B, na.rm = T) 
)

# diagrama de caja o bigotes 

boxplot(datos_dep$P_S7P85B)

#a partir de que valor son datos a típicos
bp <- boxplot(datos_dep$P_S7P85B)
bp
### "a partir de 42 litros los datos son a típicos" 

# Tabla de resumen de medidas de variabilidad

tb_resum_varia <- data.frame(
  Varianza = varianza,
  Des.Estandar = desv_st,
  Coef.Var = CV,
  Rango = rango,
  Rec.relativo = recorrido_r,
  Rec.semi.intercuartilico = rrecorrido_semi
)

# Tabla resumen medidas de asimetría y curtosis
tb_asimetria_curtosis <- data.frame(
  Coef.asimetria_YB = (Q1+Q3-2*Q2)/(Q3-Q1),
  Coef.curtosis = (mean((datos_dep$P_S7P85B-media)^4)/(varianza)^2)-3,
  Coef_fisher = (mean((datos_dep$P_S7P85B-media)^3))/(varianza)^(3/2)
)
row.names(tb_asimetria_curtosis) <- NULL

tb_asimetria_curtosis

# Exportar resultados -----------------------------------------------------

## Comparación de espacio y exportación de tablas data.frames

write.csv(datos_dep, file = "datos_dep.csv", row.names = FALSE)

saveRDS(datos_dep, file = "resultados/datos_dep.rds")
saveRDS(tb_resumen, file = "resultados/tb_resumen")
saveRDS(t_homologacion, file = "resultados/t_homologacion.rds")
saveRDS(tb_resum_varia, file = "resultados/tb_resum_varia.rds")
saveRDS(tb_asimetria_curtosis, file = "resultados/tb_asimetria_curtosis.rds")
saveRDS(tdf_S05_TENENCIA, file = "resultados/tdf_S05_TENENCIA.rds")
saveRDS(tdf_lech, file = "resultados/tdf_lech.rds")
saveRDS(Gr_P, file = "resultados/Gr_p.rds")
saveRDS(Den_lech, file = "resultados/Den_lech.rds")
saveRDS(Gf_dis_caj, file = "resultados/Gf_dis_caj.rds")
