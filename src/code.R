##################################################
## Observaciones Generales:
## - El registro de los datos no está estandarizado
##   la sintaxis es distinta para clases iguales.
## - Muchos datos faltantes:
##      averiguación previa,
##      estudios_pr
##################################################

##------------------------------
## Libraries
##------------------------------
## Data manipulation
library(plyr)
library(dtplyr)
## Strings manipulation
library(stringr)
## Plots
library(ggplot2)

##################################################
### Detalle
##################################################

##------------------------------
## Read in datasets
##------------------------------
data      <- fread("../data/detalle.csv")

##------------------------------
## Clean dataset
##------------------------------
data$ZONA   <- NULL
names(data) <- tolower(names(data)) %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_replace_all(" ", "_")


## ------------------------------
## Vis 1: Caracterizar comportam
## iento de detenciones a lo largo
## de los años y entidades.
## ------------------------------
## año
## entidad
## estatus
##-------------------------------
vis1 <- data[str_detect(data$estatus, "[A-Z]"),]
vis1 <- vis1[, .N, by = c("año", "entidad_reportada_poa",
                         "estatus")]

p1 <- ggplot(vis1,
       aes(y    = entidad_reportada_poa,
           x    = año,
           col  = estatus,
           size = N)) + geom_point() + geom_jitter() +
    theme(panel.background = element_blank(),
          axis.title = element_text(face = "bold",
                                    color = "#212121"),
          legend.title = element_text(face = "bold",
                                    color = "#212121"),
          panel.grid.major = element_line(colour = "#BDBDBD",
                                          linetype = "dotted"),
          panel.grid.minor = element_line(colour = "#E0E0E0",
                                          linetype = "dotted")) +
    scale_colour_manual(name = "Estatus",
                        values = c("#00cc99",
                                   "#9E9E9E",
                                   "#212121",
                                   "#00C853",
                                   "#ff6666")) +
scale_size_continuous(name = "Cantidad",
                      breaks = c(1, 2, 5, 10, 14),
                      range = c(1, 6)) +
    ylab("Entidad") + xlab("Año")

##
## Esta gráfica nos dice la relación
## existente entre la cantidad de delitos
## que resultan en status de detención,
## bolas verdes cada vez más grandes
## podrían ser una señal de un mejor sistema
## de detención.
##

##------------------------------
## Vis 2: Determinar si hay alguna
##        relación entre número
##        de cateos y detenidos.
## señal
## cateos
## estatus
##------------------------------
vis2 <- data[str_detect(data$estatus, "[A-Z]"),]
vis2 <- vis2[str_detect(vis2$cateos, "[0-9]"),]
vis2 <- vis2[, sum(readr::parse_number(cateos)), by = c("año","estatus",
                                                       "entidad_reportada_poa")]
names(vis2)[4] <- "cateos"
p2 <- ggplot(data = vis2,
            aes(x = año, y = entidad_reportada_poa,
                col = estatus, size = cateos)) +
    geom_point() +
    geom_jitter()+
    theme(panel.background = element_blank(),
          axis.title = element_text(face = "bold",
                                    color = "#212121"),
          legend.title = element_text(face = "bold",
                                    color = "#212121"),
          panel.grid.major = element_line(colour = "#BDBDBD",
                                          linetype = "dotted"),
          panel.grid.minor = element_line(colour = "#E0E0E0",
                                          linetype = "dotted")) +
        scale_colour_manual(name = "Estatus",
                        values = c("#00cc99",
                                   "#9E9E9E",
                                   "#212121",
                                   "#ff6666",
                                   "#ff6666")) +
scale_size_continuous(name = "Cateos",
                      breaks = c(1, 2, 5, 10, 14),
                      range = c(1, 6)) +

xlab("Año") + ylab("Entidad")
p2

##
## Bolas grandes se asocian con muchos cateos
## idealmente las bolas grandes deberían ser verdes
## es decir resultar en detenidos, esto nos da
## una idea de la efectividad del cateo.
##

##---------------------------------
## Vis 3: Caracterización detenciones
## por tipo de delito
## delitos
## estatus
## año
## entidad
##---------------------------------
vis3 <- data[str_detect(data$estatus, "[A-Z]"),]
vis3 <- vis3[str_detect(vis3$clasificación_del_delito, "[A-Z]"),]
vis3 <- vis3[, .N, by = c("año","estatus",
                         "entidad_reportada_poa",
                         "clasificación_del_delito")]

p3 <- ggplot(vis3,
       aes(y    = entidad_reportada_poa,
           x    = año,
           col  = estatus,
           size = N,
           shape = clasificación_del_delito)) + geom_point() +
    geom_jitter() +
    theme(panel.background = element_blank(),
          axis.title = element_text(face = "bold",
                                    color = "#212121"),
          legend.title = element_text(face = "bold",
                                    color = "#212121"),
          panel.grid.major = element_line(colour = "#BDBDBD",
                                          linetype = "dotted"),
          panel.grid.minor = element_line(colour = "#E0E0E0",
                                          linetype = "dotted")) +
    scale_colour_manual(name = "Estatus",
                        values = c("#00cc99",
                                   "#9E9E9E",
                                   "#212121",
                                   "#ff6666",
                                   "#00C853")) +
scale_shape_manual(name = "Tipo Delito",
                   values = c(16,17)) +
scale_size_continuous(name = "Cantidad",
                      breaks = c(1, 2, 5, 10, 14),
                      range = c(2, 6)) +
    ylab("Entidad") + xlab("Año")
p3
##---------------------------------
## Vis 4: Caracterización detenciones
## por número de víctimas
## delitos
## estatus
## año
## entidad
##---------------------------------
vis4 <- data[str_detect(data$estatus, "[A-Z]"),]
vis4 <- vis4[str_detect(vis4$clasificación_del_delito, "[A-Z]"),]
vis4 <- vis4[!is.na(vis4$total_vict),]
vis4 <- vis4[, sum(readr::parse_number(total_vict)),
            by = c("año","estatus",
                   "entidad_reportada_poa",
                   "clasificación_del_delito")]
names(vis4)[5] <- "total_victimas"
p4 <- ggplot(vis4,
       aes(y    = entidad_reportada_poa,
           x    = año,
           col  = estatus,
           size = total_victimas,
           shape = clasificación_del_delito)) + geom_point() +
    geom_jitter() +
    theme(panel.background = element_blank(),
          axis.title = element_text(face = "bold",
                                    color = "#212121"),
          legend.title = element_text(face = "bold",
                                    color = "#212121"),
          panel.grid.major = element_line(colour = "#BDBDBD",
                                          linetype = "dotted"),
          panel.grid.minor = element_line(colour = "#E0E0E0",
                                          linetype = "dotted")) +
    scale_colour_manual(name = "Estatus",
                        values = c("#00cc99",
                                   "#9E9E9E",
                                   "#212121",
                                   "#ff6666",
                                   "#00C853")) +
scale_shape_manual(name = "Tipo Delito",
                   values = c(16,17)) +
scale_size_continuous(name = "Victimas",
                      breaks = c(1, 2, 5, 10, 14),
                      range = c(2, 6)) +
    ylab("Entidad") + xlab("Año")
p4
##
## Uno esperaría ver las figuras más grandes
## cómo las que tienen mayor número de detenidos
## De todos los que no son detenidos, en su
## mayoría son delitos de alto impacto.
##

##---------------------------------
## Vis 5: Caracterización detenciones
## por area que reporta.
## delitos
## estatus
## año
## entidad
##---------------------------------

## Que area detiene más
vis5 <- data[str_detect(data$estatus, "[A-Z]"),]
vis5 <- vis5[str_detect(vis5$clasificación_del_delito, "[A-Z]"),]
vis5 <- vis5[str_detect(vis5$area_reporta_poa, "[A-Z]"),]
vis5 <- vis5[, .N, by = c("año","estatus",
                         "entidad_reportada_poa",
                         "clasificación_del_delito",
                         "area_reporta_poa")]
p5 <- ggplot(vis5,
       aes(y    = entidad_reportada_poa,
           x    = año,
           col  = estatus,
           size = N,
           shape = area_reporta_poa)) + geom_point() +
    geom_jitter() +
    theme(panel.background = element_blank(),
          axis.title = element_text(face = "bold",
                                    color = "#212121"),
          legend.title = element_text(face = "bold",
                                    color = "#212121"),
          panel.grid.major = element_line(colour = "#BDBDBD",
                                          linetype = "dotted"),
          panel.grid.minor = element_line(colour = "#E0E0E0",
                                          linetype = "dotted")) +
    scale_colour_manual(name = "Estatus",
                        values = c("#00cc99",
                                   "#9E9E9E",
                                   "#212121",
                                   "#ff6666",
                                   "#00C853")) +
scale_shape_manual(name = "Área reporta",
                   values = 16:(16 + length(unique(vis5$entidad_reportada_poa)))) +
scale_size_continuous(name = "Cantidad",
                      breaks = c(1, 2, 5, 10, 14),
                      range = c(2, 6)) +
    ylab("Entidad") + xlab("Año")
p5

##
## Comenzó reportando CENADEM y las demás
## le siguieron conforme fueron pasando
## los años, principalemente a partir del
## 2014.
##

## Que tipos de crimen se repotan más por area
p6 <- ggplot(vis5,
       aes(y    = entidad_reportada_poa,
           x    = año,
           col  = area_reporta_poa,
           size = N,
           shape = clasificación_del_delito)) + geom_point() +
    geom_jitter() +
    theme(panel.background = element_blank(),
          axis.title = element_text(face = "bold",
                                    color = "#212121"),
          legend.title = element_text(face = "bold",
                                    color = "#212121"),
          panel.grid.major = element_line(colour = "#BDBDBD",
                                          linetype = "dotted"),
          panel.grid.minor = element_line(colour = "#E0E0E0",
                                          linetype = "dotted")) +
    scale_colour_manual(name = "Area reporta",
                        values = c("#00cc99",
                                   "#9E9E9E",
                                   "#212121",
                                   "#ff6666",
                                   "#00C853")) +
scale_shape_manual(name = "Tipo Delito",
                   values = 16:17) +
scale_size_continuous(name = "Cantidad",
                      breaks = c(1, 2, 5, 10, 14),
                      range = c(2, 6)) +
    ylab("Entidad") + xlab("Año")
p6

##
## Cenadem es la que más reporta todo
## tipo de delitos, las otras se enfocan
## en delitos de pornografía infantil.
##

##---------------------------------
## Vis 6: Caracterización detenidos
## por número de víctimas
## delitos
## estatus
## año
## entidad
##---------------------------------
vis6 <- data[str_detect(data$estatus, "[A-Z]"),]
vis6 <- vis6[str_detect(vis6$clasificación_del_delito, "[A-Z]"),]
vis6 <- vis6[str_detect(vis6$estudios_pr, "[a-zA-Z]"),]
vis6 <- vis6[str_detect(vis6$edad_pr, "[0-9]"),]

vis6$estudios_pr <- tolower(vis6$estudios_pr) %>%
str_replace_all("[[:punct:]]", "") %>%
str_replace_all(" ", "_")
vis6 <- vis6[, .N, by = c("año","estatus",
                         "clasificación_del_delito",
                         "estudios_pr")]

####
#### Generar estadística descriptiva que respalden findings.
####

##################################################
### ARES
##################################################
