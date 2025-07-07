rm(list=ls())

library(INLA)
library(lattice)
library(spdep)
library(car) # VIF
library(ggplot2)
library(caret)
library(gstat)
library(sf)
library(nlme)
library(corrplot)
library(dplyr)
library(cowplot)
library(ggcorrplot)
library(tidyr)
library(scico)

setwd("c:\\users\\gerson.laurindo\\desktop\\Imperial\\scripts\\")
data_folder<-"C:\\Users\\gerson.laurindo\\desktop\\Imperial\\Dados\\"
map_folder<-"C:\\Users\\gerson.laurindo\\desktop\\Imperial\\Mapas\\"
source('reading data.R')
#for (i in 1:29) {
# if (any(is.na(data[i]))) 
#    {
#      print(names(data[i]))
#    }
# }
source("HighstatLibV10.R") 

################################ EXPLORING VARIABLES
##GROUPING BY CENSUS TRACT
dados_census <- data %>% group_by(censitario) %>%
summarise(
    casos = sum(casos,na.rm = TRUE),
    ) %>%
  mutate(
    # Substituir NAs por 0 para evitar remoções indesejadas
    casos = ifelse(is.na(casos), 0, casos)
  )

#GROUPING BY SEASON
data <- data %>%  mutate(Periodo = paste0(ano, " Q", trimestre)) 
dados_resumo <- data %>% group_by(Periodo) %>%
summarise(
    Casos = sum(casos,na.rm = TRUE),
    sc_casos = sum(casos > 0, na.rm = TRUE), 
    trab_neb=sum(trab_neb,na.rm = TRUE),
    trab_vis_cc=sum(trab_vis_cc,na.rm = TRUE),
    pend_viscc=mean(pend_vis_cc,na.rm = TRUE),
    pend_neb=mean(pend_neb,na.rm = TRUE),
    tmax=mean(tmax,na.rm = TRUE),
    tmin=mean(tmin,na.rm = TRUE),
    prec_ac=mean(prec_ac,na.rm = TRUE),
    med_alf=mean(perc_alfabet,na.rm = TRUE),
    med_agua=mean(perc_agua,na.rm = TRUE),
    med_esgoto=mean(perc_esgoto,na.rm = TRUE),
    med_lixo=mean(pec_lixo,na.rm = TRUE),
    ip=mean(ip_aeg,na.rm = TRUE),    .groups = "drop"
) %>%
  mutate(
    # Substituir NAs por 0 para evitar remoções indesejadas
    ip = ifelse(is.na(ip), 0, ip)
  )

MyVar <- c('ip_aeg','trab_neb','pend_neb','trab_vis_cc','pend_vis_cc','tmax','tmin','prec_ac','perc_alfabet','perc_esgoto','pec_lixo')

summary(data[, MyVar])

#Z-score (standardization)
data_standardized <- scale(data[, MyVar])  # Padroniza todas as variáveis

################################# DENGUE CASES AND % CENSUS TRACT WITH CASES
# Filter the periods with "Q1"
dados_q1 <- dados_resumo[grepl("Q1$", dados_resumo$Periodo), ]
periodos_q1 <- unique(dados_q1$Periodo)
# Graphic
dengue_sc <- ggplot(dados_resumo, aes(x = Periodo)) +
  # Cases (bar)
  geom_col(aes(y = Casos, fill = "Dengue Cases"), alpha = 1.5) +
  # Lines of % SC
  geom_line(aes(y = (sc_casos*100/1082) * max(Casos, na.rm = TRUE) / max((sc_casos*100/1082), na.rm = TRUE), 
                 color = "% of Census Tracts with Cases", group = 1), size = 1.5) +
  geom_point(aes(y = (sc_casos*100/1082) * max(Casos, na.rm = TRUE) / max((sc_casos*100/1082), na.rm = TRUE), 
                 color = "% of Census Tracts with Cases", group = 1), size = 2) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  geom_segment(data = dados_q1, aes(x = Periodo, xend = Periodo, y = -max(Casos, na.rm = TRUE)*0.02, yend = 0),
               color = "black", linetype = "dashed", size = 1) +  
  scale_y_continuous(name = "Dengue Cases",
             sec.axis = sec_axis(~ . * max((dados_resumo$sc_casos*100/1082), na.rm = TRUE) / max(dados_resumo$Casos, na.rm = TRUE)+1, 
              name = "% of Census Tracts with Cases", breaks = scales::pretty_breaks(n = 10))) +
  scale_x_discrete(breaks = periodos_q1, labels = periodos_q1) +
  # Defining color legend
  scale_fill_manual(name = "", values = c("Dengue Cases" = "darkred")) +
  scale_color_manual(name = "", values = c("% of Census Tracts with Cases" = "steelblue")) +
    guides(
    fill = guide_legend(order = 1),
    color = guide_legend(order = 2)
  ) +
  labs(x = "") + theme_minimal() +
  theme(
    axis.text.x = element_text(face="bold",color="black",size=18,angle = 45, hjust = 1),
    axis.title.y.left = element_text(face="bold",size=22,color = "darkred"),
    axis.title.y.right = element_text(face="bold",size=22,color = "steelblue"),
    panel.grid = element_blank(),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_text(color="black",face="bold",size = 16),
    axis.text.y = element_text(color="black",face="bold",size = 18),
    legend.title = element_blank(),
    legend.text = element_text(size = 20, face = "bold"),
    legend.position = "bottom"
  )
dengue_sc
################################# TEMPERATURE AND PRECIPITATION JOIN
dados_resumo$tmax <- as.numeric(dados_resumo$tmax)
dados_resumo$prec_ac <- as.numeric(dados_resumo$prec_ac)
# Create graphic with two axis
temp_prec<-ggplot(dados_resumo, aes(x = Periodo)) +
  geom_col(aes(y = prec_ac, fill = "Accumulated Rainfall"), alpha = 1.5) +
  geom_line(aes(y = tmax * max(prec_ac, na.rm = TRUE) / max(tmax, na.rm = TRUE), 
            color = "Maximum Temperature", group = 1), size = 1.5) +
  geom_point(aes(y = tmax * max(prec_ac, na.rm = TRUE) / max(tmax, na.rm = TRUE), 
            color = "Maximum Temperature", group = 1), size = 2) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  # Adding segments (traces) above the bars "Q1"
  geom_segment(data = dados_q1, aes(x = Periodo, xend = Periodo, y = -max(prec_ac, na.rm = TRUE)*0.02, yend = 0),
         color = "black", linetype = "dashed", size = 1) +
  scale_y_continuous(name = "Accumulated Precipitation",
  sec.axis = sec_axis(~ . * max(dados_resumo$tmax, na.rm = TRUE) / max(dados_resumo$prec_ac, na.rm = TRUE), 
                        name = "Maximum Temperature", breaks = scales::pretty_breaks(n = 10))) +
  scale_x_discrete( breaks = periodos_q1, labels = periodos_q1 ) +
  # Labels e tema
  #labs(x = "Year and Season") +
  labs(x = "") +
  scale_fill_manual(name = "", values = c("Accumulated Rainfall" = "darkred")) +
  scale_color_manual(name = "", values = c("Maximum Temperature" = "steelblue")) +
    guides(
    fill = guide_legend(order = 1),
    color = guide_legend(order = 2)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(face="bold",color="black",size=18,angle = 45, hjust = 1),
    axis.title.y.left = element_text(face="bold",size=22,color = "darkred"),
    axis.title.y.right = element_text(face="bold",size=22,color = "steelblue"),
    panel.grid = element_blank(),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_text(color="black",face="bold",size = 16),   # Título do eixo X
    axis.text.y = element_text(color="black",face="bold",size = 18),     # Texto do eixo Y
    legend.title = element_blank(),
    legend.text = element_text(size = 20, face = "bold"),
    legend.position = "bottom"
  )
temp_prec

##########################################################
#  PLOTING VISITING AND FOGGING TOGETHER
dados_resumo_normalizado <- dados_resumo %>%
  mutate(
    trab_vis_cc = (trab_vis_cc - min(trab_vis_cc, na.rm = TRUE)) / (max(trab_vis_cc, na.rm = TRUE) - min(trab_vis_cc, na.rm = TRUE)),
    trab_neb = (trab_neb - min(trab_neb, na.rm = TRUE)) / (max(trab_neb, na.rm = TRUE) - min(trab_neb, na.rm = TRUE))
  )
# Reorganize the data to long format
dados_long <- dados_resumo_normalizado %>% pivot_longer(cols = c(trab_vis_cc, trab_neb), 
               names_to = "Variavel",   values_to = "Valor")
# Create graphic with bars to normalized variable
dados_long$Variavel <- factor(dados_long$Variavel, levels = c("trab_vis_cc", "trab_neb"))
visit_neb<-ggplot(dados_long, aes(x = Periodo, y = Valor, fill = Variavel)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  # Adding segments (traces) above the bars "Q1"
  geom_segment(data = dados_q1, aes(x = Periodo, xend = Periodo, y = 0, yend = -0.02),
         inherit.aes = FALSE, color = "black", linetype = "dashed", size = 1) +
  scale_fill_manual(
    values = c("trab_vis_cc" = "red","trab_neb" = "blue"),
    labels = c("trab_vis_cc" = "Visited Homes", "trab_neb" = "Fogged Homes")) +
  scale_color_manual(
    values = c("trab_vis_cc" = "red","trab_neb" = "blue"),
    labels = c("trab_vis_cc" = "Visited Homes", "trab_neb" = "Fogged Homes")) +
  geom_smooth(aes(group = Variavel, color = Variavel), method = "loess", size = 1.5, se = FALSE) +
  scale_x_discrete( breaks = periodos_q1, labels = periodos_q1 ) +
  labs(
    x = "",
    y = "Normalized Value",
    fill = NULL,
    color = NULL
 ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(face="bold",color="black",size=18,angle = 45, hjust = 1),
    axis.title.y = element_text(face="bold",size=22,color = "darkred"),
    panel.grid = element_blank(),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_text(color="black",face="bold",size = 16),   # Título do eixo X
    axis.text.y = element_text(color="black",face="bold",size = 18),     # Texto do eixo Y
    legend.text = element_text(face="bold",size=18),
    legend.position = "bottom"
  )
visit_neb

##############################################################
visit_rotcc<-ggplot(dados_resumo, aes(x = Periodo, y = trab_vis_cc)) +
  geom_col(fill = "steelblue") +
  geom_line(aes(y = ip * max(trab_vis_cc, na.rm = TRUE) / max(ip, na.rm = TRUE), group = 1), 
            color = "red", size = 1, na.rm = TRUE) +
  geom_point(aes(y = ip * max(trab_vis_cc, na.rm = TRUE) / max(ip, na.rm = TRUE)), 
             color = "red", size = 2, na.rm = TRUE) +
  labs(
    x = "Year and Season",
    y = "Breading Sites Control + routine visit",
    title = "Breading Sites Control + routine visit by Season and Year",
    caption = "Red line is the IP mean"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
visit_rotcc

visit_neb<-ggplot(dados_resumo, aes(x = Periodo, y = trab_neb)) +
  geom_col(fill = "steelblue") +
  geom_line(aes(y = ip * max(trab_neb, na.rm = TRUE) / max(ip, na.rm = TRUE), group = 1), 
            color = "red", size = 1, na.rm = TRUE) +
  geom_point(aes(y = ip * max(trab_neb, na.rm = TRUE) / max(ip, na.rm = TRUE)), 
             color = "red", size = 2, na.rm = TRUE) +
  labs(
    x = "Year and Season",
    y = "Fogging visit",
    title = "Fogging visit by Season and Year",
    caption = "Red line is the IP mean"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
visit_neb

############
# Converting Period to ordered factor
dados_resumo <- dados_resumo %>%
  mutate(Periodo = factor(Periodo, levels = unique(Periodo), ordered = TRUE))

# Calculating factor of normalized scale
fator <- max(dados_resumo$trab_vis_cc, na.rm = TRUE) / max(dados_resumo$trab_neb, na.rm = TRUE)

# Create the graphic
ggplot(dados_resumo, aes(x = Periodo, group = 1)) +
  geom_line(aes(y = trab_vis_cc, color = "Trab Vis CC"), size = 1) + 
  geom_line(aes(y = trab_neb * fator, color = "Trab NEB"), size = 1) +
  scale_y_continuous(
    name = "Trab Vis CC",
    sec.axis = sec_axis(~ . / fator, name = "Trab NEB")
  ) +
  labs(x = "Período", color = "Legenda") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona rótulos do eixo X para melhor visualização
#########

pend_viscc<-ggplot(dados_resumo, aes(x = Periodo, y = pend_viscc)) +
  geom_col(fill = "steelblue") +
  geom_line(aes(y = ip * max(pend_viscc, na.rm = TRUE) / max(ip, na.rm = TRUE), group = 1), 
            color = "red", size = 1, na.rm = TRUE) +
  geom_point(aes(y = ip * max(pend_viscc, na.rm = TRUE) / max(ip, na.rm = TRUE)), 
             color = "red", size = 2, na.rm = TRUE) +
  labs(
    x = "Year and Season",
    y = "Pending Breading site + routine visit",
    title = "Pending Breading site + routine visit by Season and Year",
    caption = "Red line is the IP mean"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
pend_viscc

pend_neb<-ggplot(dados_resumo, aes(x = Periodo, y = pend_neb)) +
  geom_col(fill = "steelblue") +
  geom_line(aes(y = ip * max(pend_neb, na.rm = TRUE) / max(ip, na.rm = TRUE), group = 1), 
            color = "red", size = 1, na.rm = TRUE) +
  geom_point(aes(y = ip * max(pend_neb, na.rm = TRUE) / max(ip, na.rm = TRUE)), 
             color = "red", size = 2, na.rm = TRUE) +
  labs(
    x = "Year and Season",
    y = "Pending fogging visit",
    title = "Pending fogging visit by Season and Year",
    caption = "Red line is the IP mean"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
pend_neb

tmin<-ggplot(dados_resumo, aes(x = Periodo, y = tmin)) +
  geom_col(fill = "steelblue") +
  geom_line(aes(y = ip * max(tmin, na.rm = TRUE) / max(ip, na.rm = TRUE), group = 1), 
            color = "red", size = 1, na.rm = TRUE) +
  geom_point(aes(y = ip * max(tmin, na.rm = TRUE) / max(ip, na.rm = TRUE)), 
             color = "red", size = 2, na.rm = TRUE) +
  labs(
    x = "Year and Season",
    y = "Minimum Temperature",
    title = "Minimum Temperature by Season and Year",
    caption = "Red line is the IP mean"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
tmin

tmax<-ggplot(dados_resumo, aes(x = Periodo, y = tmax)) +
  geom_col(fill = "steelblue") +
  geom_line(aes(y = ip * max(tmax, na.rm = TRUE) / max(ip, na.rm = TRUE), group = 1), 
            color = "red", size = 1, na.rm = TRUE) +
  geom_point(aes(y = ip * max(tmax, na.rm = TRUE) / max(ip, na.rm = TRUE)), 
             color = "red", size = 2, na.rm = TRUE) +
  labs(
    x = "Year and Season",
    y = "Maximum Temperature",
    title = "Maximum Temperature by Season and Year",
    caption = "Red line is the IP mean"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
tmax

prec_ac<-ggplot(dados_resumo, aes(x = Periodo, y = prec_ac)) +
  geom_col(fill = "steelblue") +
  geom_line(aes(y = ip * max(prec_ac, na.rm = TRUE) / max(ip, na.rm = TRUE), group = 1), 
            color = "red", size = 1, na.rm = TRUE) +
  geom_point(aes(y = ip * max(prec_ac, na.rm = TRUE) / max(ip, na.rm = TRUE)), 
             color = "red", size = 2, na.rm = TRUE) +
  labs(
    x = "Year and Season",
    y = "Precipitation",
    title = "Acumulated Precipitation by Season and Year",
    caption = "Red line is the IP mean"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
prec_ac

grafico_combinado1 <- plot_grid(dengue_sc,visit_rotcc,pend_viscc, ncol = 2, align = 'v')
grafico_combinado2 <- plot_grid(visit_neb,pend_neb, ncol = 1, align = 'v')
grafico_combinado3 <- plot_grid(tmax,prec_ac, ncol = 2, align = 'v')

print(grafico_combinado1)
print(grafico_combinado2)
print(grafico_combinado3)

#### CORRELATION
#MYVAR
cor <- cor(na.omit(data[,MyVar])) # criterio de corte - coef de correl >= 0.5 a 0.8

corrplot(cor, method="circle", type="upper", 
         col=colorRampPalette(c("blue", "white", "red"))(200), 
         tl.cex = 0.8, tl.col = "black", number.cex = 0.7)

ggcorrplot(cor, type="upper", lab=TRUE, 
           lab_size=3, colors = c("blue", "white", "red"),
           outline.col = "white")

par(mfrow=c(5,4), mar=c(1,1,1,1))
for (k in 1: 34) 
{
   if (names(data[k]) %in% MyVar) 
   {
     var<-names(data[k])
     boxplot(na.omit=TRUE,(data[,k]),main=var)
   }
}

# comparing two graphics
## Outliers in independent variables
Mydotplot(data[,MyVar]) # usando a funcao 'HighstatLibV10.R'

par(mfrow=c(5,4), mar=c(2,2,2,2))
for (m in 1: 34) 
{
   if (names(data[m]) %in% MyVar) 
   {
     var<-names(data[m])
     hist(data[,m],main=var,xlab="", 
       col="lightblue", 
       border="black", 
       breaks=20)
   }
}


par(mfrow=c(5,4), mar=c(2,2,2,2))
for (i in 1:34) 
{
   if (names(data[i]) %in% MyVar) 
   {
     var<-names(data[i])
     qqnorm((data[,i]),main=var)
     qqline((data[,i]),main=var, col="red")
   }
}  


# Excess of zeros
sum(data$casos == 0) / nrow(data) * 100 # 10%?de valores iguais a zero

data_clean <- data
for (var in MyVar) 
{
   data_clean[[var]][is.na(data_clean[[var]])] <- median(data_clean[[var]],na.rm=TRUE)
   }
corvif(data_clean[,MyVar])


#png(filename="figura6.png", height=20, width=20, unit="cm", res=300)
Myxyplot(data, MyVar, "SIR", MyYlab = "Tx Padron")
#dev.off()

################################ EXPLORING VARIABLES
data <- data %>%  mutate(Periodo = paste0(ano)) 
dados_resumo2 <- data %>% group_by(censitario,Periodo) %>%
summarise(
    Casos = sum(casos,na.rm = TRUE),
  .groups = "drop"
 )

dados_resumo2 = left_join(shp, dados_resumo2, by=c("CD_SETOR" = "censitario")) #from here data_st have the space, time and coordinates
library(scico)
paleta <- scico(10, palette = "roma")
for (k in 2012:2024) {
   filtro <- dados_resumo2 %>% filter(Periodo == k)
   max<-max(dados_resumo2$Casos)
   mapa<- ggplot() +
             geom_sf(data = filtro, aes(fill = Casos)) + 
             theme_bw() +  
 # scale_fill_manual(values = paleta, na.value = "grey90") + 
            # scale_fill_gradientn(colors = brewer.pal(6, "Pastel1"), limits = c(-3.668018, 2.949081)) +  
             scale_fill_gradientn(colors = paleta , limits = c(0, max)) +  
            # guides(fill = guide_colorbar(title = "Valores de intI")) +  
             theme(text = element_text(size = 20), 
             axis.text.x = element_blank(), 
             axis.text.y = element_blank())
       arquivo <- paste("mapas\\mapa_",k,".png",sep="")
       png(filename=arquivo, height=14, width=14, unit="cm", res=600)
       print(mapa)
       dev.off()
  
}

mapa

########MAP WITH DENGUE CASES
dados_census = left_join(shp, dados_census, by=c("CD_SETOR" = "censitario")) #from here data_st have the space, time and coordinates
paleta <- scico(6, palette = "roma")
max<-max(dados_resumo2$Casos)
mapa_sc<- ggplot() +
             geom_sf(data = dados_census, aes(fill = casos)) + 
             theme_bw() +  
             scale_fill_gradientn(colors = paleta , limits = c(0, max)) +  
             theme(text = element_text(size = 20), 
             axis.text.x = element_blank(), 
             axis.text.y = element_blank())

mapa_sc
