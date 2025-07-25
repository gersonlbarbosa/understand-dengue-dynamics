rm(list=ls())
library(INLA)
library(tidyverse)
library(dplyr)
library(sf)
library(lattice)
library(spdep)
library(ggplot2)
library(viridis)
library(cowplot)
library(INLAOutputs)
library(RSpectra)
library(RColorBrewer)
library(scico)

setwd("c:\\users\\gerson.laurindo\\desktop\\Imperial\\scripts\\")
data_folder<-"c:\\users\\gerson.laurindo\\desktop\\Imperial\\Dados\\"
map_folder<-"c:\\users\\gerson.laurindo\\desktop\\Imperial\\Mapas\\"
source('reading data.R')
source("HighstatLibV10.R") 

#Create the variables and the data_st
data$ID.TEMP = inla.group(data$tmax, n = 18, method = "quantile")
data$ID.PREC = inla.group(data$prec_ac, n = 18, method = "quantile")
data_st = left_join(shp, data, by=c("CD_SETOR" = "censitario")) #from here data_st have the space, time and coordinates
data_st$time2<-data_st$time

#To open the model - open the file below
#load('modelos\\model_intI.RData')

####### TEMPERATURE AND PRECIPTATION NOT LINEAR
# MODEL WITH ONLY VARIABLES, WITHOUT SPACE AND TIME
formula <- casos ~ 1 + scale(ip_aeg) + scale(trab_vis_cc) + as.factor(pend_vis_cat) + 
          scale(trab_neb) + as.factor(pend_neb_cat) + scale(perc_alfabet) + scale(perc_esgoto) + scale(grdi) +
          f(ID.TEMP, model='rw1',  scale.model=TRUE)+
          f(ID.PREC, model='rw1',  scale.model=TRUE)
model <- inla(formula,
          family = "poisson", data = data,
          offset = log(pessoa_ajustada/100),
          control.compute = list(waic = TRUE, cpo = TRUE, config = TRUE))

print(round(exp(round(model$summary.fixed[,1:5],3)),3))
model$waic$waic
-mean(log(model$cpo$cpo), na.rm=TRUE) 
model$summary.hyperpar[c("0.025quant","mean","0.975quant")]

tmax = cbind(model$summary.random$ID.TEMP[,1:2], low=model$summary.random$ID.TEMP[,4], high=model$summary.random$ID.TEMP[, 6])
prec = cbind(model$summary.random$ID.PREC[,1:2], low=model$summary.random$ID.PREC[,4], high=model$summary.random$ID.PREC[, 6])

ribbon_plot_tm <- ggplot(tmax, aes(x = ID, ymin = low, ymax = high)) +
  geom_ribbon(fill = "grey", alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey")+
  # geom_line(aes(y = median)) +
  geom_line(aes(y = mean), colour = "darkgreen") +
  xlab(paste("Maximum Temperature", summary(model$model.random))) +
  ylab("Posterior mean of the coefficients") +
  theme_bw()

ribbon_plot_prec <- ggplot(prec, aes(x = ID, ymin = low, ymax = high)) +
  geom_ribbon(fill = "grey", alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey")+
  # geom_line(aes(y = median)) +
  geom_line(aes(y = mean), colour = "darkgreen") +
  xlab(paste("Acumulated Precipitation",summary(model$model.random))) +
  ylab("Posterior mean of the coefficients") +
  theme_bw()
 # Return the graphic in a list

grafico_combinado <- plot_grid(ribbon_plot_tm, ribbon_plot_prec, ncol = 1, align = 'v')
print(grafico_combinado)

####### INCLUDING ONLY SPACE
#aggregate the data in SPACE - data_agg
data_agg = data %>% group_by(censitario) %>% 
              summarize(casos = sum(casos), ip_aeg=mean(ip_aeg,na.rm=TRUE), pop = mean(pessoa_ajustada), trab_vis_cc = mean(trab_vis_cc,na.rm=TRUE),
                        pend_vis_cat =  as.character(round(mean(as.numeric(pend_vis_cat),na.rm=TRUE))),
                        trab_neb=mean(trab_neb,na.rm=TRUE), pend_neb_cat =  as.character(round(min(as.numeric(pend_neb_cat),na.rm=TRUE))),
                        perc_alfabet = mean(perc_alfabet,na.rm=TRUE), perc_esgoto = mean(perc_esgoto,na.rm=TRUE), grdi = mean(grdi,na.rm=TRUE),
                        tmax = mean(tmax),prec_ac=mean(prec_ac)) %>%  mutate(id = as.numeric(as.factor(censitario)))

data_agg[is.na(data_agg)] <- 0  # se tiver NA, passa pra zero
data_agg$ID.TEMP = inla.group(data_agg$tmax, n = 18, method = "quantile")
data_agg$ID.PREC = inla.group(data_agg$prec_ac, n = 18, method = "quantile")

formula.space <- casos ~ 1 + 
          #Só espaço sem variáveis 
          #scale(ip_aeg) + scale(trab_vis_cc) + as.factor(pend_vis_cat) + 
          #scale(trab_neb) + as.factor(pend_neb_cat) + scale(perc_alfabet) + scale(perc_esgoto) + scale(grdi) +
          f(id, model = "bym2", graph=adj, hyper=list(prec = list(prior = "pc.prec", 
               param = c(0.5 / 0.31, 0.01)), phi = list(prior = "pc", param = c(0.5, 0.5)))) +
          f(ID.TEMP, model='rw1',  scale.model=TRUE)+
          f(ID.PREC, model='rw1',  scale.model=TRUE)

model.space <- inla(formula.space, family = "poisson", data = data_agg,
                   offset = log(pessoa_ajustada/100),
                   control.compute = list(waic = TRUE, cpo = TRUE, config = TRUE))

print(round(exp(round(model.space$summary.fixed[,1:5],3)),3))
model.space$waic$waic
-mean(log(model.space$cpo$cpo), na.rm=TRUE) 
model.space$summary.hyperpar[c("0.025quant","mean","0.975quant")]

tmax_sp = cbind(model.space$summary.random$ID.TEMP[,1:2], low=model.space$summary.random$ID.TEMP[,4], high=model.space$summary.random$ID.TEMP[, 6])
prec_sp = cbind(model.space$summary.random$ID.PREC[,1:2], low=model.space$summary.random$ID.PREC[,4], high=model.space$summary.random$ID.PREC[, 6])

ribbon_plot_tm_sp <- ggplot(tmax_sp, aes(x = ID, ymin = low, ymax = high)) +
  geom_ribbon(fill = "grey", alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey")+
  # geom_line(aes(y = median)) +
  geom_line(aes(y = mean), colour = "darkgreen") +
  xlab(paste("Maximum Temperature", summary(model.space$model.random))) +
  ylab("Posterior mean of the coefficients") +
  theme_bw()

ribbon_plot_prec_sp <- ggplot(prec_sp, aes(x = ID, ymin = low, ymax = high)) +
  geom_ribbon(fill = "grey", alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey")+
  # geom_line(aes(y = median)) +
  geom_line(aes(y = mean), colour = "darkgreen") +
  xlab(paste("Acumulated Precipitation",summary(model.space$model.random))) +
  ylab("Posterior mean of the coefficients") +
  theme_bw()
 # Return the graphic in a list

grafico_combinado_sp <- plot_grid(ribbon_plot_tm_sp, ribbon_plot_prec_sp, ncol = 1, align = 'v')
print(grafico_combinado_sp)

####### INCLUDING ONLY TIME
formula.time <- casos ~ 1 + 
          # without variables
          #scale(ip_aeg) + scale(trab_vis_cc) + as.factor(pend_vis_cat) + 
          #scale(trab_neb) + as.factor(pend_neb_cat) + scale(perc_alfabet) + scale(perc_esgoto) + scale(grdi) +
          f(time,model="rw1", hyper=list(prec = list(
              prior = "pc.prec",
              param = c(0.5 / 0.31, 0.01))))+
          f(ID.TEMP, model='rw1',  scale.model=TRUE)+
          f(ID.PREC, model='rw1',  scale.model=TRUE)

model.time = inla(formula=formula.time, family="poisson", 
                   data=data_st, offset=log(pessoa_ajustada/100), 
                   control.compute=list(waic = TRUE, cpo = TRUE, config = TRUE))

print(round(exp(round(model.time$summary.fixed[,1:5],3)),3))
model.time$waic$waic
-mean(log(model.time$cpo$cpo), na.rm=TRUE) 
model.time$summary.hyperpar[c("0.025quant","mean","0.975quant")]

tmax_time = cbind(model.time$summary.random$ID.TEMP[,1:2], low=model.time$summary.random$ID.TEMP[,4], high=model.time$summary.random$ID.TEMP[, 6])
prec_time = cbind(model.time$summary.random$ID.PREC[,1:2], low=model.time$summary.random$ID.PREC[,4], high=model.time$summary.random$ID.PREC[, 6])

ribbon_plot_tm_time <- ggplot(tmax_time, aes(x = ID, ymin = low, ymax = high)) +
  geom_ribbon(fill = "grey", alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey")+
  # geom_line(aes(y = median)) +
  geom_line(aes(y = mean), colour = "darkgreen") +
  xlab(paste("Maximum Temperature", summary(model.time$model.random))) +
  ylab("Posterior mean of the coefficients") +
  theme_bw()

ribbon_plot_prec_time <- ggplot(prec_time, aes(x = ID, ymin = low, ymax = high)) +
  geom_ribbon(fill = "grey", alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey")+
  # geom_line(aes(y = median)) +
  geom_line(aes(y = mean), colour = "darkgreen") +
  xlab(paste("Acumulated Precipitation",summary(model.time$model.random))) +
  ylab("Posterior mean of the coefficients") +
  theme_bw()
 # Return the graphic in a list

grafico_combinado_time <- plot_grid(ribbon_plot_tm_time, ribbon_plot_prec_time, ncol = 1, align = 'v')
print(grafico_combinado_time)

####### INCLUDING SPACE AND TIME - WITHOUT INTERACTION
formula.st <- casos ~ scale(ip_aeg) + scale(trab_vis_cc) + as.factor(pend_vis_cat) + 
          scale(trab_neb) + as.factor(pend_neb_cat) + scale(perc_alfabet) + scale(perc_esgoto) + scale(grdi) +
          f(space, model="bym2", graph=adj, hyper=list(prec = list(
                   prior = "pc.prec",
                   param = c(0.5 / 0.31, 0.01)),
                   phi = list(prior = "pc",param = c(0.5, 0.5)))) +
          f(time,model="rw1", hyper=list(prec = list(
                   prior = "pc.prec",
                   param = c(0.5 / 0.31, 0.01)))) +
          f(space_time,model="iid", constr = TRUE, hyper=hyper) + 
          f(ID.TEMP, model='rw1', scale.model=TRUE) +
          f(ID.PREC, model='rw1', scale.model=TRUE)

model.st = inla(formula=formula.st, family="poisson", 
                   data=data_st, offset=log(pessoa_ajustada/100), 
                   control.compute=list(waic = TRUE, cpo = TRUE, config = TRUE))

print(round(exp(round(model.st$summary.fixed[,1:5],3)),3))
model.st$waic$waic
-mean(log(model.st$cpo$cpo), na.rm=TRUE) 
model.st$summary.hyperpar[c("0.025quant","mean","0.975quant")]

tmax_st = cbind(model.st$summary.random$ID.TEMP[,1:2], low=model.st$summary.random$ID.TEMP[,4], high=model.st$summary.random$ID.TEMP[, 6])
prec_st = cbind(model.st$summary.random$ID.PREC[,1:2], low=model.st$summary.random$ID.PREC[,4], high=model.st$summary.random$ID.PREC[, 6])

ribbon_plot_tm_st <- ggplot(tmax_st, aes(x = ID, ymin = low, ymax = high)) +
  geom_ribbon(fill = "grey", alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey")+
  geom_line(aes(y = mean), colour = "darkgreen") +
  xlab(paste("Maximum Temperature", summary(model.st$model.random))) +
  ylab("Posterior mean of the coefficients") +
  theme_bw()

ribbon_plot_prec_st <- ggplot(prec_st, aes(x = ID, ymin = low, ymax = high)) +
  geom_ribbon(fill = "grey", alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey")+
  geom_line(aes(y = mean), colour = "darkgreen") +
  xlab(paste("Acumulated Precipitation",summary(model.st$model.random))) +
  ylab("Posterior mean of the coefficients") +
  theme_bw()

grafico_combinado_st <- plot_grid(ribbon_plot_tm_st, ribbon_plot_prec_st, ncol = 1, align = 'v')
print(grafico_combinado_st)

save.image('modelos\\modelo_no_interaction.RData')
####### INCLUDING SPACE AND TIME - INTERACTION I
#defining Priori 
hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01))) # Pr(sd>1) = 0.01 for RW1
formula.st.intI = casos ~ scale(ip_aeg) + scale(trab_vis_cc) + as.factor(pend_vis_cat) + 
                  scale(trab_neb) + as.factor(pend_neb_cat) + scale(perc_alfabet) + scale(perc_esgoto) + scale(grdi) +
                      f(space, model="bym2", graph=adj,
                            hyper=list(prec = list(
                            prior = "pc.prec", param = c(1, 0.01)),
                            phi = list(prior = "pc", param = c(0.5, 0.5)))) + 
                      f(time, model="rw1", constr = TRUE, scale.model = TRUE,  hyper = hyper)  +                     
                      f(time2, model="iid", constr=TRUE, hyper=hyper) +
                      f(space_time,model="iid", constr = TRUE, hyper=hyper) + 
                      f(ID.TEMP, model='rw2',constr=TRUE, scale.model=TRUE,hyper=hyper) +
                      f(ID.PREC, model='rw2',constr=TRUE, scale.model=TRUE,hyper=hyper)
                    
model.st.intI = inla(formula=formula.st.intI, family="poisson", 
                       data=data_st, offset=log(pessoa_ajustada/1000),
                       control.compute=list(waic = TRUE, dic=T,cpo = TRUE, config = TRUE,
                       return.marginals.predictor=TRUE),
                       control.fixed = list(prec.intercept=0.01,prec=0.01))

print(round(exp(round(model.st.intI$summary.fixed[,1:5],3)),3))
model.st.intI$dic$dic
model.st.intI$waic$waic
-mean(log(model.st.intI$cpo$cpo), na.rm=TRUE) 
-sum(log(model.st.intI$cpo$cpo))
model.st.intI$summary.hyperpar[c("0.025quant","mean","0.975quant")]
FixedEffects(model.st.intI)
tmax_st.intI = cbind(ID=model.st.intI$summary.random$ID.TEMP[,1],mean=exp(model.st.intI$summary.random$ID.TEMP[,2]), low=exp(model.st.intI$summary.random$ID.TEMP[,4]), high=exp(model.st.intI$summary.random$ID.TEMP[, 6]))
prec_st.intI = cbind(ID=model.st.intI$summary.random$ID.PREC[,1],mean=exp(model.st.intI$summary.random$ID.PREC[,2]), low=exp(model.st.intI$summary.random$ID.PREC[,4]), high=exp(model.st.intI$summary.random$ID.PREC[, 6]))

ribbon_plot_tm_st.intI <- ggplot(tmax_st.intI, aes(x = ID, ymin = low, ymax = high)) +
  geom_ribbon(fill = "grey", alpha=0.5) +
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  geom_vline(xintercept=30.1, linetype="dashed", color = "red")+
  geom_line(aes(y = mean), colour = "darkgreen") +
  xlab(paste("Maximum Temperature")) +
  #ylab("Posterior mean of the coefficients") +
  theme_bw()+ theme(
      panel.grid=element_blank(),axis.title.y = element_blank(), axis.title.x = element_text(size = 14)
)

ribbon_plot_prec_st.intI <- ggplot(prec_st.intI, aes(x = ID, ymin = low, ymax = high)) +
  geom_ribbon(fill = "grey", alpha=0.5) +
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  geom_vline(xintercept=615, linetype="dashed", color = "red")+
  geom_line(aes(y = mean), colour = "darkgreen") +
  xlab(paste("Acumulated Precipitation")) +
 # ylab("Posterior mean of the coefficients") +
  theme_bw()+ theme(panel.grid=element_blank(), axis.title.x = element_text(size = 14),axis.title.y = element_blank()
)
 # Return the graphic in a list

grafico_combinado_st.intI <- plot_grid(ribbon_plot_tm_st.intI, ribbon_plot_prec_st.intI, ncol = 1, align = 'v')
print(grafico_combinado_st.intI)
save(model.st.intI,file='modelos\\model_intI.RData')
####### INCLUDING SPACE AND TIME - INTERACTION II
######################### INTERACTION 2
# Create the identity matrix with the number of spaces
I_1082 <- Diagonal(1082) 
# Create diference matrix of firsto order
D1 <- diff(Diagonal(52), differences = 1)
# Construct the matrix of penalization
Q.gammaRW1 <- t(D1) %*% D1
# Aplicate the Kronecker product
R_i2 <- kronecker(I_1082, Q.gammaRW1, make.dimnames = FALSE)
# show the first elements
R_i2[1:5, 1:5]
eigenQ_i2 <- eigs(R_i2, 30)
ids_i2 <- which(eigenQ_i2$values < 0.00001)
# restriction Matrix
cMat_i2 <- t(eigenQ_i2$vectors[, ids_i2])

formula.st.intII <- casos ~ scale(ip_aeg) + scale(trab_vis_cc) + as.factor(pend_vis_cat) + 
                            scale(trab_neb) + as.factor(pend_neb_cat) + scale(perc_alfabet) + scale(perc_esgoto) + scale(grdi) +
                      f(space,model="bym2",graph=adj, hyper=list(prec = list(prior = "pc.prec", param = c(1, 0.01)),
                            phi = list(prior = "pc", param = c(0.5, 0.5))))  +
                      f(time,model="rw1", constr=TRUE,scale.model=TRUE,hyper=hyper) +
                      f(time2,model="iid", constr=TRUE,hyper=hyper) +
                      f(space_time, model="generic0", constr=TRUE,hyper=hyper, 
                            Cmatrix=R_i2, rankdef = nrow(cMat_i2),
                            extraconstr = list(A = cMat_i2, e = rep(0, nrow(cMat_i2))))  +
                      f(ID.TEMP, model='rw2', constr=TRUE, scale.model=TRUE,hyper=hyper) +
                      f(ID.PREC, model='rw2', constr=TRUE,scale.model=TRUE,hyper=hyper)
inicio<-Sys.time()                            
model.st.intII = inla(formula=formula.st.intII, family="poisson", data=data_st, 
                        offset=log(pessoa_ajustada/1000),
                       control.compute=list(waic = TRUE, dic=T,cpo = TRUE, config = TRUE,
                       return.marginals.predictor=TRUE),
                       control.fixed = list(prec.intercept=0.01,prec=0.01))

print(Sys.time()-inicio)
print(round(exp(round(model.st.intII$summary.fixed[,1:5],3)),3))
model.st.intII$dic$dic
model.st.intII$waic$waic
-mean(log(model.st.intII$cpo$cpo), na.rm=TRUE) 
-sum(log(model.st.intII$cpo$cpo))
model.st.intII$summary.hyperpar[c("0.025quant","mean","0.975quant")]

tmax_st.intII = cbind(model.st.intII$summary.random$ID.TEMP[,1:2], low=model.st.intII$summary.random$ID.TEMP[,4], high=model.st.intII$summary.random$ID.TEMP[, 6])
prec_st.intII = cbind(model.st.intII$summary.random$ID.PREC[,1:2], low=model.st.intII$summary.random$ID.PREC[,4], high=model.st.intII$summary.random$ID.PREC[, 6])

ribbon_plot_tm_st.intII <- ggplot(tmax_st.intII, aes(x = ID, ymin = low, ymax = high)) +
  geom_ribbon(fill = "grey", alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey")+
  geom_line(aes(y = mean), colour = "darkgreen") +
  xlab(paste("Maximum Temperature", summary(model.st.intII$model.random))) +
  ylab("Posterior mean of the coefficients") +
  theme_bw()

ribbon_plot_prec_st.intII <- ggplot(prec_st.intII, aes(x = ID, ymin = low, ymax = high)) +
  geom_ribbon(fill = "grey", alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey")+
  geom_line(aes(y = mean), colour = "darkgreen") +
  xlab(paste("Acumulated Precipitation",summary(model.st.intII$model.random))) +
  ylab("Posterior mean of the coefficients") +
  theme_bw()
 # Return the graphic in a list

grafico_combinado_st.intII <- plot_grid(ribbon_plot_tm_st.intII, ribbon_plot_prec_st.intII, ncol = 1, align = 'v')
print(grafico_combinado_st.intII)
save(model.st.intII,file='modelos\\model_intII.RData')

####### INCLUDING SPACE AND TIME - INTERACTION III
######################### INTERACTION 3
#to make diference,using i3 - means interaction 3
g <- inla.read.graph(adj)
Q.xi <- matrix(0, g$n, g$n)
for (i in 1:g$n){
 Q.xi[i,i]=g$nnbs[[i]]
 Q.xi[i,g$nbs[[i]]]=-1
}
Q.xi <- as(Q.xi, "sparseMatrix")
R_i3 <- kronecker(Q.xi,Diagonal(52))
R_i3[1:5,1:5]
eigenQ_i3 <- eigs(R_i3, k=30)
ids_i3 <- which(eigenQ_i3$values < 0.00001)
cMat_i3 <- t(eigenQ_i3$vectors[,ids_i3])
hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01))) # Pr(sd>1) = 0.01 for RW1
formula.st.intIII <- casos ~ scale(ip_aeg) + scale(trab_vis_cc) + as.factor(pend_vis_cat) + 
                           scale(trab_neb) + as.factor(pend_neb_cat) + scale(perc_alfabet) + scale(perc_esgoto) + scale(grdi) +
                      f(space,model="bym2",graph=adj, hyper=list(prec = list(prior = "pc.prec", param = c(1, 0.01)),
                            phi = list(prior = "pc", param = c(0.5, 0.5))))  +
                      f(time, model = "rw1",  constr = TRUE, scale.model = TRUE,  hyper = hyper)  +                     
                      f(time2,model="iid", constr=TRUE, hyper=hyper) +
                      f(space_time, model="generic0", constr=TRUE, hyper=hyper, 
                            Cmatrix=R_i3, rankdef = nrow(cMat_i3),
                            extraconstr = list(A = cMat_i3, e = rep(0, nrow(cMat_i3)))) +
                      f(ID.TEMP, model="rw2", constr=TRUE, scale.model=TRUE,hyper=hyper)+
                      f(ID.PREC, model="rw2", constr=TRUE, scale.model=TRUE,hyper=hyper)

inicio <- Sys.time()
model.st.intIII = inla(formula=formula.st.intIII, family="poisson", data=data_st, 
                        offset=log(pessoa_ajustada/1000),
                        control.compute=list(waic = TRUE,dic=TRUE, cpo = TRUE, config = TRUE,
                        return.marginals.predictor=TRUE), 
                        control.fixed = list(prec.intercept=0.01,prec=0.01))

print(Sys.time()-inicio)

print(round(exp(round(model.st.intIII$summary.fixed[,1:5],3)),3))
model.st.intIII$dic$dic
model.st.intIII$waic$waic
-mean(log(model.st.intIII$cpo$cpo), na.rm=TRUE) 
-sum(log(model.st.intIII$cpo$cpo))
model.st.intIII$summary.hyperpar[c("0.025quant","mean","0.975quant")]
FixedEffects(model.st.intIII)


tmax_st.intIII = cbind(model.st.intIII$summary.random$ID.TEMP[,1:2], low=model.st.intIII$summary.random$ID.TEMP[,4], high=model.st.intIII$summary.random$ID.TEMP[, 6])
prec_st.intIII = cbind(model.st.intIII$summary.random$ID.PREC[,1:2], low=model.st.intIII$summary.random$ID.PREC[,4], high=model.st.intIII$summary.random$ID.PREC[, 6])

ribbon_plot_tm_st.intIII <- ggplot(tmax_st.intIII, aes(x = ID, ymin = low, ymax = high)) +
  geom_ribbon(fill = "grey", alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey")+
  geom_line(aes(y = mean), colour = "darkgreen") +
  xlab(paste("Maximum Temperature", summary(model.st.intIII$model.random))) +
  ylab("Posterior mean of the coefficients") +
  theme_bw()

ribbon_plot_prec_st.intIII <- ggplot(prec_st.intIII, aes(x = ID, ymin = low, ymax = high)) +
  geom_ribbon(fill = "grey", alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey")+
  geom_line(aes(y = mean), colour = "darkgreen") +
  xlab(paste("Acumulated Precipitation",summary(model.st.intIII$model.random))) +
  ylab("Posterior mean of the coefficients") +
  theme_bw()
 # Return the graphic in a list

grafico_combinado_st.intIII <- plot_grid(ribbon_plot_tm_st.intIII, ribbon_plot_prec_st.intIII, ncol = 1, align = 'v')
print(grafico_combinado_st.intIII)
save(model.st.intIII,file="modelos\\model_intIII.RData")
####### INCLUDING SPACE AND TIME - INTERACTION IV
######################### INTERACTION 4   (RUN ITERACTION III BEFORE)
R_i4 <- kronecker(Q.xi,Q.gammaRW1)
inicio<-Sys.time()
eigenQ_i4 <- eigs(R_i4,k=30)
Sys.time()-inicio
ids_i4 <- which(eigenQ_i4$values < 0.00001)
cMat_i4 <- t(eigenQ_i4$vectors[,ids_i4])

formula.st.intIV <- casos ~ scale(ip_aeg) + scale(trab_vis_cc) + as.factor(pend_vis_cat) + 
                            scale(trab_neb) + as.factor(pend_neb_cat) + scale(perc_alfabet) + scale(perc_esgoto) + scale(grdi) +
                      f(space,model="bym2",graph=adj,
                            hyper=list(prec = list(prior = "pc.prec",
                            param = c(1, 0.01)),
                            phi = list(prior = "pc",
                            param = c(0.5, 0.5))))  +
                      f(time,model="rw1", constr=TRUE, scale.model=TRUE, hyper=hyper) +
                      f(time2,model="iid", constr=TRUE, hyper=hyper) +
                      f(space_time, model="generic0", constr=TRUE, hyper=hyper, 
                            Cmatrix=R_i4, rankdef = nrow(cMat_i4),
                            extraconstr = list(A = cMat_i4, e = rep(0, nrow(cMat_i4)))) +
                      f(ID.TEMP, model="rw2", constr=TRUE, scale.model=TRUE,hyper=hyper)+
                      f(ID.PREC, model="rw2", constr=TRUE, scale.model=TRUE,hyper=hyper)
inicio<-Sys.time()                            
model.st.intIV = inla(formula=formula.st.intIV, family="poisson", data=data_st, 
				offset=log(pessoa_ajustada/1000),
                        control.compute=list(waic = TRUE,dic=TRUE, cpo = TRUE, config = TRUE,
                        return.marginals.predictor=TRUE), 
                        control.fixed = list(prec.intercept=0.01,prec=0.01))

print(Sys.time()-inicio)

model.st.intIV$dic$dic
model.st.intIV$waic$waic
-mean(log(model.st.intIV$cpo$cpo), na.rm=TRUE) 
-sum(log(model.st.intIV$cpo$cpo))
model.st.intIV$summary.hyperpar[c("0.025quant","mean","0.975quant")]
FixedEffects(model.st.intIV)

tmax_st.intIV = cbind(model.st.intIV$summary.random$ID.TEMP[,1:2], low=model.st.intIV$summary.random$ID.TEMP[,4], high=model.st.intIV$summary.random$ID.TEMP[, 6])
prec_st.intIV = cbind(model.st.intIV$summary.random$ID.PREC[,1:2], low=model.st.intIV$summary.random$ID.PREC[,4], high=model.st.intIV$summary.random$ID.PREC[, 6])

ribbon_plot_tm_st.intIV <- ggplot(tmax_st.intIV, aes(x = ID, ymin = low, ymax = high)) +
  geom_ribbon(fill = "grey", alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey")+
  geom_line(aes(y = mean), colour = "darkgreen") +
  xlab(paste("Maximum Temperature", summary(model.st.intIV$model.random))) +
  ylab("Posterior mean of the coefficients") +
  theme_bw()

ribbon_plot_prec_st.intIV <- ggplot(prec_st.intIV, aes(x = ID, ymin = low, ymax = high)) +
  geom_ribbon(fill = "grey", alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey")+
  geom_line(aes(y = mean), colour = "darkgreen") +
  xlab(paste("Acumulated Precipitation",summary(model.st.intIV$model.random))) +
  ylab("Posterior mean of the coefficients") +
  theme_bw()
 # Return the graphic in a list

grafico_combinado_st.intIV <- plot_grid(ribbon_plot_tm_st.intIV, ribbon_plot_prec_st.intIV, ncol = 1, align = 'v')
print(grafico_combinado_st.intIV)

save.image('modelos\\model_intIV.RData')
###########################################################################
RR_stIntI = c()

for(i in 1:1082){
  RR_stIntI[i] = inla.emarginal(function(x) exp(x), model.st.intI$marginals.random$space[[i]])
}

#Posterior probabilities (for spatial RR)
RR_stIntI_marg = model.st.intI$marginals.random$space[1:1082]
PP_stIntI = lapply(RR_stIntI_marg, function(x) {1-inla.pmarginal(0,x)})

#Temporal Relative risks and CI95
RR_stIntI.RW_RR = c()
RR_stIntI.RW_lo = c()
RR_stIntI.RW_hi = c()

for(i in 1:52){
  #Posterior mean
  RR_stIntI.RW_RR[i] = inla.emarginal(function(x) exp(x), model.st.intI$marginals.random$time[[i]])
  #2.5% quantile 
  RR_stIntI.RW_lo[i] = inla.qmarginal(0.025,inla.tmarginal(function(x) exp(x), model.st.intI$marginals.random$time[[i]]))
  #97.5% quantile 
  RR_stIntI.RW_hi[i] = inla.qmarginal(0.975, inla.tmarginal(function(x) exp(x), model.st.intI$marginals.random$time[[i]]))
}

RR_stIntI.RW = data.frame(RR_I=RR_stIntI.RW_RR,low_I=RR_stIntI.RW_lo,high_I=RR_stIntI.RW_hi)

anos <- rep(2012:2024, each = 4)  
trimestres <- rep(sprintf("%02d", 1:4), length.out = length(anos))  
seq_trimestres <- paste(anos, trimestres, sep = "-")  # Formato "YYYY-MM"

# Add a sequency in the dataframe
RR_stIntI.RW$periodo <- factor(seq_trimestres, levels = seq_trimestres)  # Garantir ordem correta

# Plot the temporal residual RRs (`RR_stWR`)
dados_q1 <- RR_stIntI.RW[grepl("01$", RR_stIntI.RW$periodo), ]
TempI = ggplot(RR_stIntI.RW, aes(x=periodo,y=RR_I,group=1)) + 
  geom_line() + 
  #ggtitle("ST model Int I") + 
  geom_ribbon(aes(ymin = low_I, ymax = high_I+0.1), alpha = 0.2) + 
  geom_segment(data = dados_q1, aes(x = periodo, xend = periodo),
         color = "black", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0, color = "black", size = 0.7) +
  geom_segment(data = dados_q1, aes(x = periodo, xend = periodo, y = 0, yend = -0.25),
         color = "black", linetype = "dashed", size = 1) +
  scale_x_discrete( breaks = dados_q1$periodo, labels = sub("01$", "Q1", dados_q1$periodo) ) +
 # labs(x = "Year-Quarter") + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_blank(), #element_text(size = 14), 
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12)     # Texto do eixo Y
)  # Melhor visualização do eixo X
TempI

#########################################################
#  FILE: PREDICTED MAP
