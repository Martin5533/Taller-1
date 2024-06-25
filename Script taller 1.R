#Datos Taller de Titul: Evaluación PAE 

setwd("C:/Users/marti/OneDrive/Taller de titulo")

#Cargar posibles paquetes necesarios 
library(stargazer)
library(texreg) 
library(ggplot2)
library(RColorBrewer)
library(broom)
library(xtable)
library(kableExtra)
library(dplyr)
library(car)
library(haven)
library(here)
library(tibble)
library(scales)
library(sf)
library(flextable)
library(ggmosaic)

#Abrir base de datos 
library(readxl)
Respuestas_PAE_ <- read_excel("~/Taller de titulo/Respuestas PAE .xlsx")
View(Respuestas_PAE_)

#Verificación de hipótesis 
na.omit(Respuestas_PAE_)

#Hipótesis 1: 

#Recodificar variables 



# Crear una función para recodificar la columna "1a"
recodificar_1a <- function(x) {
  ifelse(x >= 1 & x <= 2, "No Hubo mejora",
         ifelse(x >= 3 & x <= 5, "Mediana mejora", 
                ifelse(x >= 6 & x <= 7, "Alto nivel de mejora", NA)))
}

# Crear una función para recodificar la columna "2c"
recodificar_2c <- function(x) {
  ifelse(x >= 1 & x <= 2, "Insatisfecho",
         ifelse(x >= 3 & x <= 5, "Medianamente satisfecho", 
                ifelse(x >= 6 & x <= 7, "Muy satisfecho", NA)))
}

# Aplicar las funciones a las columnas correspondientes en el dataframe Respuestas_PAE_
Respuestas_PAE_ <- Respuestas_PAE_ %>%
  mutate(
    `1a` = recodificar_1a(`1a`),
    `2c` = recodificar_2c(`2c`)
  )

# Crear una tabla bivariada entre las variables recodificadas "1a" y "2c"
tabla_bivariada <- table(Respuestas_PAE_$`2c`, Respuestas_PAE_$`1a`)

# Mostrar la tabla bivariada
print(tabla_bivariada)

# Calcular la tabla bivariada
tabla_bivariada <- table(Respuestas_PAE_$`2c`, Respuestas_PAE_$`1a`)

# Convertir la tabla a porcentajes relativos por filas
tabla_porcentual_filas <- prop.table(tabla_bivariada, margin = 1) * 100

# Mostrar la tabla de porcentajes relativos por filas
print(tabla_porcentual_filas)
print(tabla_bivariada)

#Cerar gráficos

# Convertir las variables a factores para asegurar que se grafican correctamente
Respuestas_PAE_$`2c` <- factor(Respuestas_PAE_$`2c`)
Respuestas_PAE_$`1a` <- factor(Respuestas_PAE_$`1a`)

# Crear el gráfico de barras agrupadas
ggplot(Respuestas_PAE_, aes(x = `2c`, fill = `1a`)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Gráfico de Barras Agrupadas de '2c' y '1a'",
    x = "2c",
    y = "Frecuencia",
    fill = "1a"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )






# Crear el gráfico de barras
# Crear el gráfico de barras con barras naranja oscuro y fondo blanco
grafico_barras <- ggplot(Respuestas_PAE_, aes(x = factor(`1b`, levels = c("Muy en desacuerdo", "En desacuerdo", "Ni de acuerdo ni en desacuerdo", "De acuerdo", "Muy de acuerdo")))) +
  geom_bar(fill = "#FFA500", color = "black") +  # Color naranja oscuro (#FFA500)
  labs(x = "Percepción de Cambios Positivos", y = "Conteo") +
  theme_minimal() +  # Estilo del tema minimalista
  theme(panel.background = element_rect(fill = "white")) +  # Fondo blanco
  ggtitle("Percepción de Cambios Positivos en la Vida desde que Comenzaron a Ir a Terapia")  # Título del gráfico

# Mostrar el gráfico
print(grafico_barras)


#Exportar gráfico en word 
library(ggplot2)
library(officer)

# Guardar el gráfico como un archivo PNG temporal
tmp <- tempfile(fileext = ".png")
ggsave(tmp, plot = grafico_barras, width = 8, height = 6, units = "in", dpi = 300)

# Crear un nuevo documento de Word
doc <- read_docx()

# Agregar la imagen al documento de Word
doc <- doc %>%
  body_add_img(src = tmp, width = 7, height = 5)

# Guardar el documento de Word
output_file <- "grafico_percepcion_cambios_positivos.docx"
print(output_file)
print(doc, target = output_file)

# Limpiar el archivo temporal
unlink(tmp)






#Hipótesis 2: 

#Tabla entre adherencia y mejora del motivo de consulta 
colnames(Respuestas_PAE_) 
colnames(Respuestas_PAE_)[colnames(Respuestas_PAE_) == "2b\r\n"] <- "2b"
Respuestas_PAE_$satisfaccion2a <- cut(Respuestas_PAE_$`2a`,
                                    breaks = c(-Inf, 2, 5, 7),
                                    labels = c("Insatisfecho", "Medianamente satisfecho", "Muy satisfecho"),
                                    right = TRUE)


table(Respuestas_PAE_$satisfaccion2a, Respuestas_PAE_$`2b`) 

#2a

grafico_barras2a <- ggplot(Respuestas_PAE_, aes(x = factor(satisfaccion2a, levels = c("Insatisfecho", "Medianamente satisfecho", "Muy satisfecho")))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#FFA500", color = "black") +  # Color naranja oscuro (#FFA500)
  scale_y_continuous(labels = scales::percent_format()) +  # Mostrar el eje y como porcentaje
  labs(x = "Percepción de Cambios Positivos", y = "Porcentaje") +
  theme_minimal() +  # Estilo del tema minimalista
  theme(panel.background = element_rect(fill = "white")) +  # Fondo blanco
  ggtitle("Satisfacción con atención de Primera Línea")  # Título del gráfico

# Mostrar el gráfico
print(grafico_barras2a)

#2b

grafico_barras2b <- ggplot(Respuestas_PAE_, aes(x = factor(`2b`, levels = c("Muy en desacuerdo", "En desacuerdo", "Ni de acuerdo/desacuerdo", "De acuerdo", "Muy de acuerdo")))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#FFA500", color = "black") +  # Color naranja oscuro (#FFA500)
  scale_y_continuous(labels = scales::percent_format()) +  # Mostrar el eje y como porcentaje
  labs(x = "Percepción de Cambios Positivos", y = "Porcentaje") +
  theme_minimal() +  # Estilo del tema minimalista
  theme(panel.background = element_rect(fill = "white")) +  # Fondo blanco
  ggtitle("Satisfacción con atención de Primera Línea")  # Título del gráfico

# Mostrar el gráfico
print(grafico_barras2b)



#Hipótesis 3: 

table(Respuestas_PAE_$`2h`, Respuestas_PAE_$`1d`)

Respuestas_PAE_ <- Respuestas_PAE_ %>%
  mutate(motivacion2h = case_when(
    `2h` %in% c("Muy en desacuerdo", "En desacuerdo") ~ "Poco motivado",
    `2h` == "Ni de acuerdo ni en desacuerdo" ~ "Medianamente motivado",
    `2h` %in% c("De acuerdo", "Muy de acuerdo") ~ "Muy motivado",
    TRUE ~ as.character(`2h`)  # Manejo de valores no esperados
  ))

table(Respuestas_PAE_$motivacion2h,Respuestas_PAE_$`1d` )


Respuestas_PAE_ <- Respuestas_PAE_ %>%
  mutate(cambios1d = case_when(
    `1d` %in% c("Muy en desacuerdo", "En desacuerdo") ~ "No mejora MC",
    `1d` == "Ni de acuerdo ni en desacuerdo" ~ "Mediana Mejora MC",
    `1d` %in% c("De acuerdo", "Muy de acuerdo") ~ "Alto nivel de Mejora MC",
    TRUE ~ as.character(`1d`)  # Manejo de valores no esperados
))

table(Respuestas_PAE_$motivacion2h, Respuestas_PAE_$cambios1d)

# Calcular la tabla de contingencia
tabla_contingencia3 <- table(Respuestas_PAE_$motivacion2h, Respuestas_PAE_$cambios1d)

# Convertir la tabla en porcentajes por fila
tabla_porcentaje3 <- prop.table(tabla_contingencia3, margin = 2) * 100

# Mostrar la tabla de porcentajes por fila
print(tabla_porcentaje3)

# Calcular la tabla de contingencia
tabla_contingencia3 <- table(Respuestas_PAE_$cambios1d, Respuestas_PAE_$motivacion2h)

# Realizar la prueba de chi-cuadrado para calcular la correlación
resultado_chi2 <- chisq.test(tabla_contingencia3)

# Mostrar el resultado de la prueba
print(resultado_chi2)
#Principales resultados 


#Hipótesis 4: 

table(Respuestas_PAE_$`2c`)
table(Respuestas_PAE_$`2e`)

# Recodificar la variable 2c
Respuestas_PAE_$satisfaccion_terapeuta <- cut(Respuestas_PAE_$`2c`,
                                              breaks = c(-Inf, 2, 6, 7),
                                              labels = c("Insatisfecho con terapeuta", "Medianamente satisfecho con terapeuta", "Muy satisfecho con terapeuta"),
                                              right = TRUE)

# Recodificar la variable 2e
Respuestas_PAE_$satisfaccion2e <- cut(Respuestas_PAE_$`2e`,
                                                         breaks = c(-Inf, 2, 6, 7),
                                                         labels = c("Baja satisfacción compromiso terapeuta", "Mediana satisfacción compromiso terapeuta", "Alta satisfacción compromiso terapeuta"),
                                                         right = TRUE)

tabla_satisfaccion <- table(Respuestas_PAE_$satisfaccion2e, Respuestas_PAE_$satisfaccion_terapeuta)                                                        
tabla_porcentaje_satisfaccion <- prop.table(tabla_satisfaccion, margin = 1) * 100                                                         
print(tabla_porcentaje_satisfaccion)


#1a

Respuestas_PAE_ <- Respuestas_PAE_[!is.na(Respuestas_PAE_$`1a`), ]

# Calcular la proporción de cada valor
Respuestas_PAE_$`1a` <- factor(Respuestas_PAE_$`1a`)
proporcion_datos1a <- as.data.frame(prop.table(table(Respuestas_PAE_$`1a`)))
colnames(proporcion_datos1a) <- c("Valor", "Proporción")


ggplot(proporcion_datos1a, aes(x = Valor, y = Proporción)) +
  geom_bar(stat = "identity", fill = "#FFA500") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Percepción de mejora desde el comienzo de la terapia",
       x = "Valoración",
       y = "Porcentaje") +
  theme_minimal()

#1b
Respuestas_PAE_ <- Respuestas_PAE_[!is.na(Respuestas_PAE_$`1b`), ]

# Calcular la proporción de cada valor
Respuestas_PAE_$`1b` <- factor(Respuestas_PAE_$`1b`)
proporcion_datos1b <- as.data.frame(prop.table(table(Respuestas_PAE_$`1b`)))
colnames(proporcion_datos1b) <- c("Valor", "Proporción")

# Reordenar los niveles del factor en base a la frecuencia (de mayor a menor)
proporcion_datos1b$Valor <- reorder(proporcion_datos1b$Valor, proporcion_datos1b$Proporción)

# Crear el gráfico de barras horizontal con orden invertido
ggplot(proporcion_datos1b, aes(x = Valor, y = Proporción)) +
  geom_bar(stat = "identity", fill = "#FFA500") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Desde que fuí o desde que mi  hijo fue a terapia percibí 
cambios positivos en mi/su vida",
       x = "Valoración",
       y = "Porcentaje") +
  coord_flip() +
  theme_minimal()

#1d

# Eliminar respuestas perdidas (NA) en la variable 1c
Respuestas_PAE_ <- Respuestas_PAE_[!is.na(Respuestas_PAE_$`1d`), ]

# Calcular la proporción de cada valor
Respuestas_PAE_$`1d` <- factor(Respuestas_PAE_$`1d`)
proporcion_datos1d <- as.data.frame(prop.table(table(Respuestas_PAE_$`1d`)))
colnames(proporcion_datos1d) <- c("Valor", "Proporción")

# Reordenar los niveles del factor en base a la frecuencia (de mayor a menor)
proporcion_datos1d$Valor <- reorder(proporcion_datos1d$Valor, proporcion_datos1d$Proporción)

# Crear el gráfico de barras horizontal con orden invertido
ggplot(proporcion_datos1d, aes(x = Valor, y = Proporción)) +
  geom_bar(stat = "identity", fill = "#FFA500") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Ha habido una mejora en el motivo por el cuál comencé/ mi hijo comenzó 
a ir a terapia.",
       x = "Valoración",
       y = "Porcentaje") +
  coord_flip() +
  theme_minimal()


#2a
# Eliminar respuestas perdidas (NA) en la variable 2a
Respuestas_PAE_ <- Respuestas_PAE_[!is.na(Respuestas_PAE_$`2a`), ]

# Convertir la variable 2a a factor si no lo está
Respuestas_PAE_$`2a` <- factor(Respuestas_PAE_$`2a`, ordered = TRUE)

# Contar frecuencias de cada respuesta
frecuencias <- table(Respuestas_PAE_$`2a`)

# Crear dataframe con las respuestas y frecuencias
df_frecuencia2a <- data.frame(Respuesta = as.factor(names(frecuencias)),
                             Frecuencia = as.numeric(frecuencias))

# Ordenar dataframe por las respuestas en la escala
df_frecuencia2a <- df_frecuencia2a[order(as.numeric(levels(df_frecuencia2a$Respuesta))), ]

# Crear el gráfico de barras
ggplot(df_frecuencia2a, aes(x = Respuesta, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "#FFA500") +
  labs(title = "Valoración de satisfacción con Primera Línea",
       x = "Valoración",
       y = "Frecuencia de Respuestas") +
  theme_minimal()

#2b 

# Eliminar respuestas perdidas (NA) en la variable 1c
Respuestas_PAE_ <- Respuestas_PAE_[!is.na(Respuestas_PAE_$`2b`), ]

# Calcular la proporción de cada valor
Respuestas_PAE_$`2b` <- factor(Respuestas_PAE_$`2b`)
proporcion_datos2b <- as.data.frame(prop.table(table(Respuestas_PAE_$`2b`)))
colnames(proporcion_datos2b) <- c("Valor", "Proporción")

# Reordenar los niveles del factor en base a la frecuencia (de mayor a menor)
proporcion_datos2b$Valor <- reorder(proporcion_datos2b$Valor, proporcion_datos2b$Proporción)

# Crear el gráfico de barras horizontal con orden invertido
ggplot(proporcion_datos2b, aes(x = Valor, y = Proporción)) +
  geom_bar(stat = "identity", fill = "#FFA500") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "La persona que le hizo la primera entrevista se mantuvo en contacto 
con usted iniciar su proceso de terapia o el de su hijo.",
       x = "Valoración",
       y = "Porcentaje") +
  coord_flip() +
  theme_minimal()


#2c

Respuestas_PAE_ <- Respuestas_PAE_[!is.na(Respuestas_PAE_$`2c`), ]

# Calcular la proporción de cada valor
Respuestas_PAE_$`2c` <- factor(Respuestas_PAE_$`2c`)
proporcion_datos2c <- as.data.frame(prop.table(table(Respuestas_PAE_$`2c`)))
colnames(proporcion_datos2c) <- c("Valor", "Proporción")


ggplot(proporcion_datos2c, aes(x = Valor, y = Proporción)) +
  geom_bar(stat = "identity", fill = "#FFA500") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Satisfacción con terapeuta.",
       x = "Valoración",
       y = "Porcentaje") +
  theme_minimal()


#2d 

Respuestas_PAE_ <- Respuestas_PAE_[!is.na(Respuestas_PAE_$`2d`), ]

# Calcular la proporción de cada valor
Respuestas_PAE_$`2d` <- factor(Respuestas_PAE_$`2d`)
proporcion_datos2d <- as.data.frame(prop.table(table(Respuestas_PAE_$`2d`)))
colnames(proporcion_datos2d) <- c("Valor", "Proporción")


ggplot(proporcion_datos2d, aes(x = Valor, y = Proporción)) +
  geom_bar(stat = "identity", fill = "#FFA500") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Satisfacción con horarios",
       x = "Valoración",
       y = "Porcentaje") +
  theme_minimal()


#2e 
Respuestas_PAE_ <- Respuestas_PAE_[!is.na(Respuestas_PAE_$`2e`), ]

# Calcular la proporción de cada valor
Respuestas_PAE_$`2e` <- factor(Respuestas_PAE_$`2e`)
proporcion_datos2e <- as.data.frame(prop.table(table(Respuestas_PAE_$`2e`)))
colnames(proporcion_datos2e) <- c("Valor", "Proporción")


ggplot(proporcion_datos2e, aes(x = Valor, y = Proporción)) +
  geom_bar(stat = "identity", fill = "#FFA500") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Satisfacción con compromiso terapeuta",
       x = "Valoración",
       y = "Porcentaje") +
  theme_minimal()

#2f 
colnames(Respuestas_PAE_)[colnames(Respuestas_PAE_) == "2f\r\n"] <- "2f"
Respuestas_PAE_ <- Respuestas_PAE_[!is.na(Respuestas_PAE_$`2f`), ]

# Calcular la proporción de cada valor
Respuestas_PAE_$`2f` <- factor(Respuestas_PAE_$`2f`)
proporcion_datos2f <- as.data.frame(prop.table(table(Respuestas_PAE_$`2f`)))
colnames(proporcion_datos2f) <- c("Valor", "Proporción")


ggplot(proporcion_datos2f, aes(x = Valor, y = Proporción)) +
  geom_bar(stat = "identity", fill = "#FFA500") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = " La forma de atención online me acomoda al desarrollo de la terapia",
       x = "Valoración",
       y = "Porcentaje") +
  theme_minimal()

#2g 
colnames(Respuestas_PAE_)[colnames(Respuestas_PAE_) == "2g\r\n"] <- "2g"
proporcion_datos2g <- as.data.frame(prop.table(table(Respuestas_PAE_$`2g`)))
colnames(proporcion_datos2g) <- c("Valor", "Proporción")


ggplot(proporcion_datos2g, aes(x = Valor, y = Proporción)) +
  geom_bar(stat = "identity", fill = "#FFA500") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "  Me conecto desde un lugar apropiado para el desarrollo de la terapia",
       x = "Valoración",
       y = "Porcentaje") +
  theme_minimal()

#2h 

proporcion_datos2h <- as.data.frame(prop.table(table(Respuestas_PAE_$`2h`)))
colnames(proporcion_datos2h) <- c("Valor", "Proporción")


ggplot(proporcion_datos2h, aes(x = Valor, y = Proporción)) +
  geom_bar(stat = "identity", fill = "#FFA500") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = " Me siento motivado a seguir asistiendo a terapiaa",
       x = "Valoración",
       y = "Porcentaje") +
  theme_minimal()
