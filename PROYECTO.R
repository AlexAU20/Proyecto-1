library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(gridExtra)
library(grid)
library(tidyr)

data_clima <- read.csv("liberia_datos_climaticos.csv",
                       sep = ",",
                       na.strings = "", 
                       dec = ",")

data_clima <- na.omit(data_clima)

data_clima <-
  data_clima %>%
  rename(humedad = "HumedadRelativa....",
         vel_viento = "VelocidadViento..m.s.",
         lluvia = "Lluvia..mm.",
         irradiacion = "Irradiacion..W.m2.",
         evaporacion = "EvapoTranspiracion..mm.",
         t_celcius = "Temperatura..Celsius.",
         fecha = "Date"
         )
data_clima <-
  data_clima %>%
  mutate(fecha = as.Date(fecha, format = "%d/%m/%Y"))

#GRAFICO DE HISTOGRAMAS

k<- ggplot(data_clima,aes(y=lluvia,group = 1))+geom_histogram(
  col = "#CCFFFF",
  fill = "#6699CC"
) + 
  ggtitle(" Cantidad de lluvia")+
  xlab("Frecuencia")+
  ylab("Lluvia (mm)")+
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
l <- ggplot(data_clima, aes(y = humedad, group = 1)) +
  geom_histogram(
    col = "#CCFFFF",
                 fill = "#6699CC") +
  ggtitle("Cantidad de humedad") +
  xlab("Frecuencia") +
  ylab("Humedad (%)") +
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
m <-
  ggplot(data_clima, aes(y = t_celcius, group = 1)) + 
  geom_histogram(col = "#CCFFFF",
                 fill = "#FF9900") +
  ggtitle("Cantidad de temperatura") +
  xlab("Frecuencia") +
  ylab("Temperatura(C°)") +
  theme_ipsum(
    axis_text_size = 7,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
n <- ggplot(data_clima, aes(y= vel_viento, group = 1))+
  geom_histogram(col = "#CCFFFF",
  fill = "#CC9966") +
  ggtitle("Velocidad del viento") +
  xlab("Frecuencia") +
  ylab("Velocidad del viento (m/s)") +
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
o <- ggplot(data_clima, aes(y= evaporacion, group = 1))+geom_histogram(
  col = "#CCFFFF",
  fill = "#669966") +
  ggtitle("Cantidad de evaporacion") +
  xlab("Frecuencia") +
  ylab("Evaporacion (ml)") +
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
p <- ggplot(data_clima, aes(y= irradiacion, group = 1))+
  geom_histogram(col = "#CCFFFF",
  fill = "#FFFF33") +
  ggtitle("Cantidad de irradiacion") +
  xlab("Frecuencia") +
  ylab("Irradiacion(W/m2)") +
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )

grid.arrange(k,l,m,n,o,p,nrow = 6, ncol = 1)

## LINEAS PROMEDIADADAS

datos <-
  data_clima %>%
  select(fecha, t_celcius, humedad, vel_viento, lluvia, irradiacion, evaporacion)%>%
  mutate(fecha = as.Date(fecha, format = "%d/%m/%Y")) %>%
  group_by (fecha = format(fecha,"%Y/%m"))%>%
  summarise(lluvia = sum(lluvia),evaporacion = sum(evaporacion),
            t_celcius = mean(t_celcius),vel_viento = mean(vel_viento),
            irradiacion = mean(irradiacion), humedad = mean(humedad)
            )


q <- ggplot(datos,aes(x=fecha,y=lluvia,group = 1))+
  geom_line(col= "#3399FF")+
  ggtitle("Lluvia por mes") +
  xlab("") +
  ylab("Lluvia(mm)")+
  geom_point()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme_ipsum(
    axis_text_size = 7,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
w <- ggplot(datos,aes(x=fecha,y=humedad,group = 1))+
  geom_line(col= "#003366")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggtitle("Humedad por mes") +
  xlab("") +
  ylab("Humedad(%)")+
  theme_ipsum(
  axis_text_size = 7,
  ticks = TRUE,
  axis = "y",
  grid = "Y,y")+
  geom_point()
r <- ggplot(datos,aes(x=fecha,y=t_celcius,group = 1))+
  geom_line(col= "#FF3300")+
  ggtitle("Cantidad de lluvia en relacion a la fecha") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  ggtitle("Temperatura al mes") +
  xlab("") +
  ylab("Temperatura(C°)")+
  geom_point()+
  theme_ipsum(
    axis_text_size = 7,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
  
t <- ggplot(datos,aes(x=fecha,y=evaporacion,group = 1))+
  geom_line(col= "#9999FF")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  ggtitle("Evaporacion por mes")+
  xlab("") +
  ylab("Evaporacion(ml)")+
  theme_ipsum(
    axis_text_size = 7,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )+
  geom_point()
y <- ggplot(datos,aes(x=fecha,y=vel_viento,group = 1))+
  geom_line(col= "#33FF99")+
  ggtitle("Velocidad del viento en relacion al mes")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
xlab("") +
  ylab("Velocidad del viento(m/s)")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
         geom_point()+
theme_ipsum(
  axis_text_size = 7,
  ticks = TRUE,
  axis = "y",
  grid = "Y,y"
)
u <- ggplot(datos,aes(x=fecha,y=irradiacion,group = 1))+
  geom_line(col= "#FFFF00")+
  ggtitle("Irradiacion por mes")
xlab("Meses a partir de 2016 hasta 2019") +
  ylab("Irradiacion((W/m2))")+
  geom_point()+
theme_ipsum(
  axis_text_size = 7,
  ticks = TRUE,
  axis = "y",
  grid = "Y,y"
)
grid.arrange(q,w,r,t,y,u,nrow = 6, ncol = 1)

## Grafico de puntos

as <- ggplot(data_clima,aes(x=fecha,y=t_celcius))+geom_point(
  col = "#FF6600"
  )+
  ggtitle("Grafico de puntos temperatura")+
  xlab("") +
  ylab("Temperatura(???)")+
  geom_point()+
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
ad <- ggplot(data_clima,aes(x=fecha,y=lluvia))+geom_point(
  col = "#33CCFF"
)+
  ggtitle("Grafico de puntos lluvia")+
  xlab("") +
  ylab("Lluvia (mm)")+
  geom_point()+
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
af <- ggplot(data_clima,aes(x=fecha,y=humedad))+geom_point(
  col = "#99FFCC"
)+
  ggtitle("Grafico de puntos humedad")+
  xlab("") +
  ylab("Humedad (%)")+
  geom_point()+
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
ag <- ggplot(data_clima,aes(x=fecha,y=vel_viento))+geom_point(
  col = "#FF99FF"
)+
  ggtitle("Grafico de puntos velocidad del viento")+
  xlab("") +
  ylab("Viento (m/s)")+
  geom_point()+
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
ah <- ggplot(data_clima,aes(x=fecha,y=irradiacion))+geom_point(
  col = "#FFFF33"
)+
  ggtitle("Grafico de puntos irradiacion")+
  xlab("") +
  ylab("Irradiacion (W/m2)")+
  geom_point()+
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
aj <- ggplot(data_clima,aes(x=fecha,y=evaporacion))+geom_point(
  col = "#663333"
)+
  ggtitle("Grafico de puntos velocidad del viento")+
  xlab("Fecha") +
  ylab("Evaporacion (ml)")+
  geom_point()+
  theme_ipsum(
    axis_text_size = 8,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )

grid.arrange(as,ad,af,ag,ah,aj,nrow = 6, ncol = 1)








