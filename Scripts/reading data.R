#First open Exploratroy Data Analysis.R
#### 1 - reading the database
data <- read.csv(paste(data_folder,"data_sjrp_0603.csv",sep=""),dec=',',sep=';')
#### 2 - reading the map and obtaining the neighborhood matrix
contour <- read_sf(paste(map_folder,"outline_sjrp.shp",sep=""))
shp <- read_sf(paste(map_folder,"sjrp_final_utm.shp",sep=""))
shp$CD_SETOR <- as.numeric(shp$CD_SETOR)
ggplot() + geom_sf(data=shp) + theme_bw()
# neighborhood
nb <- poly2nb(as(shp, "Spatial"))
list(nb)
nb2INLA("graph", nb)
adj <- paste(getwd(), "/graph", sep ="")
list(nb)
### transf the map in dataframe
sjrp.df <- shp %>% st_drop_geometry()
shp.df <- as.data.frame(sjrp.df)
################################
### Union of tables
data <- left_join(data,shp.df,by=c("censitario" = "CD_SETOR"))
#data$E <- data$tot_pessoas*(sum(data$casos)/sum(data$tot_pessoas))
data$E <- data$pessoa_ajustada*(sum(data$casos)/sum(data$pessoa_ajustada))
data$SIR <- ifelse(data$E == 0, 0, data$casos / data$E)
##CREATE SPACE, TIME AND SPACETIME
data <- data %>% arrange(ano, trimestre) %>% mutate(time = (ano - min(ano)) * 4 + trimestre)

# Create the space variable (from 1 to 1082 for each census tract)
data <- data %>% mutate(space = as.numeric(factor(censitario)))

# Create the space-time variable (from 1 to n, considering space-time)
data <- data %>% arrange(ano, trimestre, censitario) %>% mutate(space_time = row_number())

# Create variable for number of property by census tract
data$imoveis_setor <- data$pessoa_ajustada / data$med_pes_sc

# Create variable of work coverage
data$cobertura_vis <- data$trab_vis_cc / data$imoveis_setor *100
data$cobertura_vis[is.na(data$cobertura_vis)]<-0

# Classify the coverage considering the defined interval
data$cobertura_vis_cat <- cut(data$cobertura_vis,  
  breaks = c(Inf, 90, 70, 60, 50, 0),
  labels = c( "1", "2", "3", "4","5"),  # Apenas 6 rótulos
  include.lowest = TRUE)

# Convert to caracter
data$cobertura_vis_cat <- as.character(data$cobertura_vis_cat)

# If trab_vis_cc is NA, define like "5"
data$cobertura_vis_cat[is.na(data$trab_vis_cc)] <- "5"

# Convert again to factor with the new levels
data$cobertura_vis_cat <- factor(data$cobertura_vis_cat, levels = c("1", "2", "3", "4","5"))

#CATEGORIZE PENDING IN DOMICILIARY VISIT AND FOGGING
categorizar <- function(pend_vis_cc, trab_vis_cc) {
  if (is.na(trab_vis_cc) || is.na(pend_vis_cc)) {
    return('5')  # Lida com valores ausentes
  } else if (trab_vis_cc == 0) {
    return('5')  # Defina aqui o valor que você quer atribuir quando trab_vis_cc=0
  } else if (pend_vis_cc <= 25) {
    return('1')
  } else if (pend_vis_cc > 25 & pend_vis_cc <= 45) {
    return('2')
  } else if (pend_vis_cc > 45) {
    return('3')
  } else {
    return('Negativo')
  }
}
# create new column pend_vis_cat
data$pend_vis_cat <- mapply(categorizar, data$pend_vis_cc, data$trab_vis_cc)

categorizar_neb <- function(pend_neb, trab_neb) {
  if (is.na(trab_neb) || is.na(pend_neb)) {
    return('5')  # Lida com valores ausentes
  } else if (trab_neb == 0) {
    return('5')  # Defina aqui o valor que você quer atribuir quando trab_neb=0
  } else if (pend_neb <= 25) {
    return('1')
  } else if (pend_neb > 25 & pend_neb <= 45) {
    return('2')
  } else if (pend_neb > 45) {
    return('3')
  } else {
    return('Negativo')
  }
}
# create new column pend_vis_cat
data$pend_neb_cat <- mapply(categorizar_neb, data$pend_neb, data$trab_neb)
