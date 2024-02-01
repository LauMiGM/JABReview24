#Cargar el paquete de datos para hacer el gráfico
library(ggplot2)
library(ggimage)

#Cargar los datos del excel
library(readxl)
Gráfico_PorcentajeRapaces <- read_excel("C:/Users/laura/Desktop/Doctorado_2024/JABReview_02.2024/JAB_Review_Papers.xlsx", 
                                        sheet = "Gráfico1R")
View(Gráfico_PorcentajeRapaces)

library(readxl)
BroodSize <- read_excel("C:/Users/laura/Desktop/Doctorado_2024/JABReview_02.2024/JAB_Review_Papers.xlsx", 
                        sheet = "Gráfico 2R")
View(BroodSize)

library(readxl)
OffspringSurvirval <- read_excel("C:/Users/laura/Desktop/Doctorado_2024/JABReview_02.2024/JAB_Review_Papers.xlsx", 
                                 sheet = "Gráfico 3R")
View(OffspringSurvirval)

#BarPlot1: Representación en % de las rapaces en la review
#Transformar a factor la variable Familia
factor_familia <- factor(Gráfico_PorcentajeRapaces$Family)

#Hacer el plot
P1 <- ggplot(data = Gráfico_PorcentajeRapaces, aes(x = Porct, y = factor_familia, fill= factor_familia)) + 
labs(x = "% of representation") +  
geom_bar(stat = "identity", width = 0.3) +
scale_x_continuous(limits = c(0,60)) +
scale_fill_manual(values = c("#D0D0D0", "#858F94", "#49525E"))+
theme(axis.line = element_line(colour = "black"),
      axis.title.y = element_blank(),
      axis.text.y= element_text(size = 11),
      legend.position="none",
      panel.background = element_blank(),
      panel.grid.major.x = element_blank(), #removes vertical grid lines
      panel.grid.major.y = element_blank()) #keeps horizontal grid lines))
  
P1

#BarPlot2: Changes in brood size
factor_weather <- factor(BroodSize$Weather_var)

P2 <- ggplot(data = BroodSize, aes(x = Porcent, y =factor_weather, fill= FactorColor)) + 
  labs(x = "% of cases", y = "     Precipitation            Snow            Temperature") + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_discrete(labels=c("Temp +" = "+", "Temp -" = "-",
                            "Snow +" = "+", "Snow -"= "-", "Precip +" = "+", "Precip -" = "-")) +
  scale_x_continuous(limits = c(0,50))+
  scale_fill_manual(values= c("#C3C3C3","#2B2B2B","#7B7B7B"))+
  expand_limits(x = 0, y = 0) +
  theme(axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=12, margin = margin(t=10)),
        axis.title.y = element_text(size=12),
        axis.text.y=element_text(size=15, margin =margin(0,10,0,20)),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(), #removes vertical grid lines
        panel.grid.major.y = element_blank()) #keeps horizontal grid lines
        
P2 

#BarPlot3: Changes in offspring survirval
P3 <- ggplot(data = OffspringSurvirval, aes(x = Percentage, y = Weather_var, fill= FactorColor2)) + 
  labs(x = "% of cases", y = "Diet shift          Precip.            Temp.           Winds") + 
  geom_bar(stat = "identity", width = 0.5) + 
  scale_fill_manual(values= c("#9A9A9A","#4D4D4D","#1E1E1E","#C3C3C3"))+
  scale_y_discrete(labels=c("Temp +" = "+", "Temp -" = "-",
                            "Diet shift +" = "+", "Diet shift -"= "-", 
                            "Precip +" = "+", "Precip -" = "-", 
                            "Winds +"="+","Winds -"="-")) +
  scale_x_continuous(limits = c(0,35))+
  expand_limits(x = 0, y = 0) +
  theme(axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=12, margin = margin(t=10)),
        axis.title.y = element_text(size=12),
        axis.text.y=element_text(size=15, margin =margin(0,10,0,20)),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(), #removes vertical grid lines
        panel.grid.major.y = element_blank()) #keeps horizontal grid lines

P3