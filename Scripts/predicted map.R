rm(list=ls())
library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)
library(ggsn)
library(RColorBrewer)
library(scico)

####
setwd("c:\\users\\gerson.laurindo\\desktop\\Imperial\\scripts\\")
data_folder<-"c:\\users\\gerson.laurindo\\desktop\\Imperial\\Dados\\"
map_folder<-"c:\\users\\gerson.laurindo\\desktop\\Imperial\\Mapas\\"

df <- read.csv(paste(data_folder,"data_sjrp_0603.csv", sep=""),dec=',', sep=';')
load("modelos/model_intI.RData")

fit <- model.st.intI
 
## select output elements and refine fitted values
### select elements of the output to save
rnams <- c( paste0(
    "summary.", c("fixed", "hyperpar", "random", "fitted.values")
  ), "internal.marginals.hyperpar", "mlik", "mode", "cpu.used")

ndata <- nrow(df)
sres <- fit[rnams]
sres$summary.fitted.values <- fit$summary.fitted.values[1:ndata, c(1, 2)]

# posterior mean
df$pred_mean = fit$summary.fitted.values[1:ndata, "mean"]
# lower limits of the 95% credible intervals
df$pred_ll = fit$summary.fitted.values[1:ndata, "0.025quant"]
# upper limits of the 95% credible intervals
df$pred_ul = fit$summary.fitted.values[1:ndata, "0.975quant"]

#Calculating the adjusted incidence rate
df$pred_mean <-  fit$summary.fitted.values$mean*1000/df$pessoa_ajustada
df$pred_ll <-  fit$summary.fitted.values$'0.025quant'*1000/df$pessoa_ajustada
df$pred_ul <-  fit$summary.fitted.values$'0.975'*1000/df$pessoa_ajustada

shp <- sf::st_read(paste(map_folder,"sjrp_final_utm.shp",sep="")) # lendo mapa

#plot (shp)

#names(df)
#table(df$ano, df$trimestre)

df$id_key_time <- paste (df$trimestre, "_", df$ano, sep="")

#df$censitario

dw <-df %>% select (id_key_time, censitario, pred_mean) %>% 
  reshape(timevar = "id_key_time",
          idvar = "censitario", direction = "wide")

#names(shp)

map <- merge(shp[,c("CD_SETOR")], dw, by.x = "CD_SETOR", by.y = "censitario")
map_sf <- st_as_sf(map)

map_sf <- gather(map_sf, id_key_time, pred_mean, paste0("pred_mean.", 1:4, "_", rep(2012:2024, each = 4)))
#table(map_sf$id_key_time)

map_sf$id <- gsub("pred_mean.", "", map_sf$id_key_time) 
map_sf <- map_sf  %>% separate(id , c("season", "year"), sep="_")

map_sf$season [map_sf$season =="1"] <- "Q1"
map_sf$season [map_sf$season =="2"] <- "Q2"
map_sf$season [map_sf$season =="3"] <- "Q3"
map_sf$season [map_sf$season =="4"] <- "Q4"
map_sf$season <- factor(map_sf$season , levels = c("Q1", "Q2", "Q3","Q4" ))

map_sf$id <- paste(map_sf$season, map_sf$year)

map_sf$id <- factor(map_sf$id,
                    levels =  c("Q1 2012","Q1 2013","Q1 2014","Q1 2015","Q1 2016","Q1 2017","Q1 2018","Q1 2019","Q1 2020","Q1 2021" ,"Q1 2022", "Q1 2023","Q1 2024",
                                "Q2 2012","Q2 2013","Q2 2014","Q2 2015","Q2 2016","Q2 2017","Q2 2018","Q2 2019","Q2 2020","Q2 2021" ,"Q2 2022", "Q2 2023","Q2 2024",
                                "Q3 2012","Q3 2013","Q3 2014","Q3 2015","Q3 2016","Q3 2017","Q3 2018","Q3 2019","Q3 2020","Q3 2021" ,"Q3 2022", "Q3 2023","Q3 2024",
                                "Q4 2012","Q4 2013","Q4 2014","Q4 2015","Q4 2016","Q4 2017","Q4 2018","Q4 2019","Q4 2020","Q4 2021" ,"Q4 2022", "Q4 2023","Q4 2024"
                    ))

###### Categoric Map
################### Creating categories by quantil
options(scipen = 999)
#defining quantil - using 6
ver_quantis <- function(vetor, n_classes) {
  quantile(vetor, probs = seq(0, 1, length.out = n_classes + 1), na.rm = TRUE)
}
ver_quantis(map_sf$pred_mean,6)

map_sf$quantil <- NA

#boxplot(map_sf$pred_mean)

#map_sf$quantil[map_sf$pred_mean == 0] <- "0"
map_sf$quantil[map_sf$pred_mean > 0 & map_sf$pred_mean < 0.3] <- "0.01-0.30"
map_sf$quantil[map_sf$pred_mean >= 0.3 & map_sf$pred_mean < 0.8] <- "0.30-0.80"
map_sf$quantil[map_sf$pred_mean >= 0.8 & map_sf$pred_mean < 2] <- "0.80-2.00"
map_sf$quantil[map_sf$pred_mean >= 2 & map_sf$pred_mean < 6] <- "2.00-6.00"
map_sf$quantil[map_sf$pred_mean >= 6 & map_sf$pred_mean < 20] <- "6.00-20.00"
map_sf$quantil[map_sf$pred_mean >= 20] <-">20.00"

map_sf$quantil <- factor(map_sf$quantil, 
                         levels =  c("0.01-0.30","0.30-0.80","0.80-2.00","2.00-6.00","6.00-20.00",">20.00"))

#table(map_sf$quantil)
#library(scico)
paleta <- scico(6, palette = "lajolla") #buda #hawaii #bamako #imola #nuuk #managua  #berlin #navia #bilbao
#paleta <- c("#0277BD",'#abdda4', '#ffffbf', '#fdae61', "red", "darkred")
ggp <- ggplot() +
  geom_sf(data = map_sf, aes(fill = quantil), color = "transparent") +
  theme_classic() + 
  scale_fill_manual(name = "pred_mean", values= rev(paleta), 
                    limits =  c("0.01-0.30","0.30-0.80","0.80-2.00","2.00-6.00","6.00-20.00",">20.00")) + # Especificando as cores
  # Se usar RcolorBrewer - abrir esse abaixo
  #   scale_fill_brewer(name = "pred_mean", palette = "Blues"
  #                   #, direction = -1 
  # ) + 
  guides(fill = guide_legend(title = element_blank(), 
                             #label.theme = element_text(size = 6),  # Tamanho do texto menor
                             keywidth = unit(0.3, "cm"),  # Tamanho do ícone menor
                             keyheight = unit(0.3, "cm"),  # Tamanho do ícone menor
                             nrow = 1)) +  # Divide a legenda em duas linhas
  theme(legend.text = element_text(size = 8, face = "bold"), 
        legend.direction = "horizontal",
        legend.position = "top", #"bottom",
        legend.margin = margin(0, 0, 0, 0),
        text = element_text(size = 9), 
        #axis.text.x = element_blank(), 
        #axis.text.y = element_blank(),
        axis.line = element_blank(),  # Remove as linhas dos eixos
        axis.text = element_blank(),  # Remove os rótulos dos eixos
        axis.ticks = element_blank(),

    strip.text.x = element_text(size = 7, face = "bold"),  # Anos (em cima)
    strip.text.y = element_text(size = 7, face = "bold"),  # Trimestres (lado esquerdo)

#        strip.text = element_text(size = 5.0), #color = "dark green"), facet
        strip.background = element_rect(linewidth = 0.1),
        panel.spacing = unit(0.00, "lines"),# Change spacing between facets on both axis
        panel.spacing.x = unit(0.00, "lines"),# Change horizontal spacing between facets
        panel.spacing.y = unit(0.00, "lines")) +# Change vertical spacing between facets
  facet_grid(season ~ year, switch = "y") +
#  facet_wrap(~ id, ncol = 13 #,
# ) +
  labs(title = "",
       x = "",
       y = "")  
#Scale bar - below
#   +
#  scalebar(map_sf,
#           dist = 5000,           # 4 km
#           dist_unit = "m",        # em metros, pois o CRS está em metros
#           st.size = 0.55,
#           height = 0.1,
#           border.size = 0.1,###
#           st.dist = 0.05,
#           anchor = c(x = 673000, y = 7681000),  # canto inferior esquerdo
#           transform = FALSE,
#           model = "WGS84",        # pode deixar ou remover se transform = FALSE
#           facet.var = c("id"),
#           facet.lev = c("Q4"))

ggp
# x 0 is the left side and 1 the right side
# y 0 is the bottom and 1 the top.
# st_crs(map_sf)
# st_bbox(map_sf)

jpeg("mapas/map_predmean.jpeg", width = 14, height = 10, 
     units = "cm", res = 2000, type = "cairo"
     # compression = "lzw"
)
ggp 
north2(ggp = ggp, scale = 0.050, x = 0.98, y = 0.158, symbol = 10)
dev.off()

tiff("mapas/map_predmean.tiff", 
     width = 14, height = 10, 
     units = "cm", res = 2000, type = "cairo")
ggp
north2(ggp = ggp, scale = 0.050, x = 0.97, y = 0.098, symbol = 10)
dev.off()

##########################################################################
#### dw_ll ####
dw_ll <-df %>% select (id_key_time, censitario, pred_ll) %>% 
  reshape(timevar = "id_key_time",
          idvar = "censitario", direction = "wide")

map <- merge(shp[,c("CD_SETOR")], dw_ll, by.x = "CD_SETOR", by.y = "censitario")
map_sf <- st_as_sf(map)


map_sf <- gather(map_sf, id_key_time, pred_ll, paste0("pred_ll.", 1:4, "_", rep(2012:2024, each = 4)))
table(map_sf$id_key_time)

map_sf$id <- gsub("pred_ll.", "", map_sf$id_key_time) 
map_sf <- map_sf  %>% separate(id , c("season", "year"), sep="_")

map_sf$season [map_sf$season =="1"] <- "Q1"
map_sf$season [map_sf$season =="2"] <- "Q2"
map_sf$season [map_sf$season =="3"] <- "Q3"
map_sf$season [map_sf$season =="4"] <- "Q4"
map_sf$season <- factor(map_sf$season , levels = c("Q1", "Q2", "Q3","Q4" ))

map_sf$id <- paste(map_sf$season, map_sf$year)

map_sf$id <- factor(map_sf$id,
                    levels =  c("Q1 2012","Q1 2013","Q1 2014","Q1 2015","Q1 2016","Q1 2017","Q1 2018","Q1 2019","Q1 2020","Q1 2021" ,"Q1 2022", "Q1 2023","Q1 2024",
                                "Q2 2012","Q2 2013","Q2 2014","Q2 2015","Q2 2016","Q2 2017","Q2 2018","Q2 2019","Q2 2020","Q2 2021" ,"Q2 2022", "Q2 2023","Q2 2024",
                                "Q3 2012","Q3 2013","Q3 2014","Q3 2015","Q3 2016","Q3 2017","Q3 2018","Q3 2019","Q3 2020","Q3 2021" ,"Q3 2022", "Q3 2023","Q3 2024",
                                "Q4 2012","Q4 2013","Q4 2014","Q4 2015","Q4 2016","Q4 2017","Q4 2018","Q4 2019","Q4 2020","Q4 2021" ,"Q4 2022", "Q4 2023","Q4 2024"
                    ))

options(scipen = 999)
ver_quantis <- function(vetor, n_classes) {
  quantile(vetor, probs = seq(0, 1, length.out = n_classes + 1), na.rm = TRUE)
}
ver_quantis(map_sf$pred_ll,6)
map_sf$quantil <- NA
summary(map_sf$pred_ll)

map_sf$quantil[map_sf$pred_ll > 0 & map_sf$pred_ll < 0.3] <- "0.01-0.30"
map_sf$quantil[map_sf$pred_ll >= 0.3 & map_sf$pred_ll < 0.8] <- "0.30-0.80"
map_sf$quantil[map_sf$pred_ll >= 0.8 & map_sf$pred_ll < 2] <- "0.80-2.00"
map_sf$quantil[map_sf$pred_ll >= 2 & map_sf$pred_ll < 6] <- "2.00-6.00"
map_sf$quantil[map_sf$pred_ll >= 6 & map_sf$pred_ll < 20.0] <- "6.00-20.00"
map_sf$quantil[map_sf$pred_ll >= 20] <-">20.00"

map_sf$quantil <- factor(map_sf$quantil, 
                         levels =  c("0.01-0.30","0.30-0.80","0.80-2.00","2.00-6.00","6.00-20.00",">20.00"))

table(map_sf$quantil)
paleta <- scico(6, palette = "lajolla") #buda #hawaii #bamako #imola #nuuk #managua  #berlin #navia #bilbao
ggp_ll <- ggplot() +
  geom_sf(data = map_sf, aes(fill = quantil), color = "transparent") +
  theme_classic() + 
  scale_fill_manual(name = "pred_ll", values= rev(paleta),
                    limits =  c("0.01-0.30","0.30-0.80","0.80-2.00","2.00-6.00","6.00-20.00",">20.00")) + # Especificando as cores
  #   scale_fill_brewer(name = "", palette = "Reds"
  #                   #, direction = -1 
  # ) + 
  guides(fill = guide_legend(title = element_blank(), 
                             #label.theme = element_text(size = 6),  # Tamanho do texto menor
                             keywidth = unit(0.3, "cm"),  # Tamanho do ícone menor
                             keyheight = unit(0.3, "cm"),  # Tamanho do ícone menor
                             nrow = 1)) +  # Divide a legenda em duas linhas
  theme(legend.text = element_text(size = 8, face = "bold"), 
        legend.direction = "horizontal",
        legend.position = "top", #"bottom",
        legend.margin = margin(0, 0, 0, 0),
        text = element_text(size = 9), 
        #axis.text.x = element_blank(), 
        #axis.text.y = element_blank(),
        axis.line = element_blank(),  # Remove as linhas dos eixos
        axis.text = element_blank(),  # Remove os rótulos dos eixos
        axis.ticks = element_blank(),
        strip.text.x = element_text(size = 7, face = "bold"),  # Anos (em cima)
        strip.text.y = element_text(size = 7, face = "bold"),  # Trimestres (lado esquerdo)
        strip.background = element_rect(linewidth = 0.1),
        panel.spacing = unit(0.00, "lines"),# Change spacing between facets on both axis
        panel.spacing.x = unit(0.00, "lines"),# Change horizontal spacing between facets
        panel.spacing.y = unit(0.00, "lines")) +# Change vertical spacing between facets
  facet_grid(season ~ year, switch = "y") +
#  facet_wrap(~ id, ncol = 13 #, 
#  ) +
  labs(title = "",
       x = "",
       y = "") 
# +
#  scalebar(map_sf,
#           dist = 5000,           # 4 km
#           dist_unit = "m",        # em metros, pois o CRS está em metros
#           st.size = 0.55,
#           height = 0.1,
#           border.size = 0.1,
#           st.dist = 0.05,
#           anchor = c(x = 673000, y = 7681000),  # canto inferior esquerdo
#           transform = FALSE,
#           model = "WGS84",        # pode deixar ou remover se transform = FALSE
#           facet.var = c("id"),
#           facet.lev = c("Q4"))
ggp_ll

jpeg("mapas/map_pred_lowerl.jpeg", width = 14, height = 10, 
     units = "cm", res = 2000, type = "cairo"
     # compression = "lzw"
)
ggp_ll
north2(ggp = ggp_ll, scale = 0.050, x = 0.98, y = 0.158, symbol = 10)
dev.off()
  
tiff("mapas/map_pred_lowerl.tiff", 
     width = 14, height = 10, 
     units = "cm", res = 2000, type = "cairo")
ggp_ll
north2(ggp = ggp_ll, scale = 0.050, x = 0.97, y = 0.098, symbol = 10)
dev.off()

################################################################################
##### dw_ul ####
dw_ul <-df %>% select (id_key_time, censitario, pred_ul) %>% 
  reshape(timevar = "id_key_time",
          idvar = "censitario", direction = "wide")

map <- merge(shp[,c("CD_SETOR")], dw_ul, by.x = "CD_SETOR", by.y = "censitario")
map_sf <- st_as_sf(map)

map_sf <- gather(map_sf, id_key_time, pred_ul, paste0("pred_ul.", 1:4, "_", rep(2012:2024, each = 4)))
table(map_sf$id_key_time)

map_sf$id <- gsub("pred_ul.", "", map_sf$id_key_time) 
map_sf <- map_sf  %>% separate(id , c("season", "year"), sep="_")

map_sf$season [map_sf$season =="1"] <- "Q1"
map_sf$season [map_sf$season =="2"] <- "Q2"
map_sf$season [map_sf$season =="3"] <- "Q3"
map_sf$season [map_sf$season =="4"] <- "Q4"
map_sf$season <- factor(map_sf$season , levels = c("Q1", "Q2", "Q3","Q4" ))

map_sf$id <- paste(map_sf$season, map_sf$year)

map_sf$id <- factor(map_sf$id,
                    levels =  c("Q1 2012","Q1 2013","Q1 2014","Q1 2015","Q1 2016","Q1 2017","Q1 2018","Q1 2019","Q1 2020","Q1 2021" ,"Q1 2022", "Q1 2023","Q1 2024",
                                "Q2 2012","Q2 2013","Q2 2014","Q2 2015","Q2 2016","Q2 2017","Q2 2018","Q2 2019","Q2 2020","Q2 2021" ,"Q2 2022", "Q2 2023","Q2 2024",
                                "Q3 2012","Q3 2013","Q3 2014","Q3 2015","Q3 2016","Q3 2017","Q3 2018","Q3 2019","Q3 2020","Q3 2021" ,"Q3 2022", "Q3 2023","Q3 2024",
                                "Q4 2012","Q4 2013","Q4 2014","Q4 2015","Q4 2016","Q4 2017","Q4 2018","Q4 2019","Q4 2020","Q4 2021" ,"Q4 2022", "Q4 2023","Q4 2024"
                    ))

options(scipen = 999)
map_sf$quantil <- NA
summary(map_sf$pred_ul)

map_sf$quantil[map_sf$pred_ul > 0 & map_sf$pred_ul < 0.3] <- "0.01-0.30"
map_sf$quantil[map_sf$pred_ul >= 0.3 & map_sf$pred_ul < 0.8] <- "0.30-0.80"
map_sf$quantil[map_sf$pred_ul >= 0.8 & map_sf$pred_ul < 2] <- "0.80-2.00"
map_sf$quantil[map_sf$pred_ul >= 2 & map_sf$pred_ul < 6] <- "2.00-6.00"
map_sf$quantil[map_sf$pred_ul >= 6 & map_sf$pred_ul < 20.0] <- "6.00-20.00"
map_sf$quantil[map_sf$pred_ul >= 20] <-">20.00"

map_sf$quantil <- factor(map_sf$quantil, 
                         levels =  c("0.01-0.30","0.30-0.80","0.80-2.00","2.00-6.00","6.00-20.00",">20.00"))

table(map_sf$quantil)
paleta <- scico(6, palette = "lajolla") #buda #hawaii #bamako #imola #nuuk #managua  #berlin #navia #bilbao
ggp_ul <- ggplot() +
  geom_sf(data = map_sf, aes(fill = quantil), color = "transparent") +
  theme_classic() + 
  scale_fill_manual(name = "pred_ul", values= rev(paleta),
                    limits =  c("0.01-0.30","0.30-0.80","0.80-2.00","2.00-6.00","6.00-20.00",">20.00")) + # Especificando as cores
  #   scale_fill_brewer(name = "", palette = "Reds"
  #                   #, direction = -1 
  # ) + 
  guides(fill = guide_legend(title = element_blank(), 
                             #label.theme = element_text(size = 6),  # Tamanho do texto menor
                             keywidth = unit(0.3, "cm"),  # Tamanho do ícone menor
                             keyheight = unit(0.3, "cm"),  # Tamanho do ícone menor
                             nrow = 1)) +  # Divide a legenda em duas linhas
  theme(legend.text = element_text(size = 8, face = "bold"), 
        legend.direction = "horizontal",
        legend.position = "top", #"bottom",
        legend.margin = margin(0, 0, 0, 0),
        text = element_text(size = 9), 
        #axis.text.x = element_blank(), 
        #axis.text.y = element_blank(),
        axis.line = element_blank(),  # Remove as linhas dos eixos
        axis.text = element_blank(),  # Remove os rótulos dos eixos
        axis.ticks = element_blank(),
        strip.text.x = element_text(size = 7, face = "bold"),  # Anos (em cima)
        strip.text.y = element_text(size = 7, face = "bold"),  # Trimestres (lado esquerdo)
        strip.background = element_rect(linewidth = 0.1),
        panel.spacing = unit(0.00, "lines"),# Change spacing between facets on both axis
        panel.spacing.x = unit(0.00, "lines"),# Change horizontal spacing between facets
        panel.spacing.y = unit(0.00, "lines")) +# Change vertical spacing between facets
  facet_grid(season ~ year, switch = "y") +
#  facet_wrap(~ id, ncol = 13 #, 
#  ) +
  labs(title = "",
       x = "",
       y = "") 
# +
#  scalebar(map_sf,
#           dist = 5000,           # 4 km
#           dist_unit = "m",        # em metros, pois o CRS está em metros
#           st.size = 0.55,
#           height = 0.1,
#           border.size = 0.1,
#           st.dist = 0.05,
#           anchor = c(x = 673000, y = 7681000),  # canto inferior esquerdo
#           transform = FALSE,
#           model = "WGS84",        # pode deixar ou remover se transform = FALSE
#           facet.var = c("id"),
#           facet.lev = c("Q4"))

ggp_ul
jpeg("mapas/map_pred_upperl.jpeg", width = 14, height = 10, 
     units = "cm", res = 2000, type = "cairo"
     # compression = "lzw"
)
ggp_ul
north2(ggp = ggp_ul, scale = 0.050, x = 0.98, y = 0.158, symbol = 10)
dev.off()


tiff("mapas/map_pred_upperl.tiff", 
     width = 14, height = 10, 
     units = "cm", res = 2000, type = "cairo")
ggp_ul
north2(ggp = ggp_ul, scale = 0.050, x = 0.97, y = 0.098, symbol = 10)
dev.off()
