library(ggplot2)
library(tidyverse)

Covariates = c("Predial Index",
               "Visited Homes",
               "Pending Homes <=20%",
               "Pending Homes - 20 a 45%",
               "Pending Homes > 45",
               "No activity Visited Homes",
               "Fogged Homes",
               "Pending Fogged Homes <=20%",
               "Pending Fogged Homes - 20 a 45%",
               "Pending Fogged Homes > 45",
               "No activity Fogged Homes",
               "Literacy rate",
               "Sewage",
               "GRDI")


lower=c(1.009,1.125,1,1.049,1.044,0.665,1.025,1,0.795,0.932,0.799,0.856,0.91,1.047)
IRR=c(1.023,1.137,1,1.082,1.079,0.693,1.04,1,0.894,1.04,0.83,0.895,1.033,1.086)
upper=c(1.038,1.15,1,1.116,1.115,0.723,1.055,1,1.002,1.166,0.996,0.935,1.076,1.126)
type <- c( rep("Entomologico",2),"Ref.", rep("Entomologico", 4),"Ref.",
  rep("Entomologico", 3), rep("Socioeconomico",3) )

Reference = c(rep(FALSE, 2), TRUE, rep(FALSE, 4), TRUE, rep(FALSE, 6))  # Marca as referências

data <- data.frame(Covariates,IRR,lower, upper, type, Reference)
data$Covariates <- factor(data$Covariates, levels = rev(data$Covariates))
data$type <- factor(data$type, levels = c("Entomologico", "Socioeconomico", "Ref."))

data$color <- ifelse(data$upper < 1 & data$lower < 1, "steelblue3",       # Proteção
                     ifelse(data$lower > 1 & data$upper > 1, "firebrick2", # Risco aumentado
                            "gray50"))   
# Criar o gráfico
grafico <- ggplot(data, aes(x = IRR, y = Covariates, color = color)) +
  geom_point(size = 4) +  
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.3, linewidth = 0.8) +  
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +  
  labs(
    x = "IRR (95% IC)",
    y = NULL
  ) +
  scale_color_identity(guide = "legend", 
                       labels = c("Protection", "Amplified Risk", "No effect"),
                       breaks = c("steelblue3", "firebrick2", "gray50")) +
  scale_shape_manual(
    values = c(16, 17, 15),  
    labels = c("Entomológico", "Socioeconômico", "Referência")
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line.x = element_line(color = "black"),  
    axis.line.y = element_blank(),   
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )
grafico
#ggsave("grafico_tipo.png", width = 6, height = 6, dpi = 300,bg = "white")
dir()