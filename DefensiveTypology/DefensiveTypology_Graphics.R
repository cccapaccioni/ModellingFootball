##  ARTICLE: The Defensive Signature of Football Players: Characterising Aggressiveness and Consistency in Defensive Pressure
##  IUMPA. Programa Doctorado Matematicas Aplicadas
##  SCRIPT TITLE: DefensiveSignature_Graphics
##  Fecha revisión: 10/04/26
##  Revisión: 2.1
##  Notas: Este script debe ser usado con los datos de presiones defensivas unitarias obtenidas en un partido concreto

# Cargar los packetes de R que pueden ser necesarios

if(!require(psych))install.packages("psych");library("psych")
if(!require(tidyverse))install.packages("tidyverse");library("tidyverse")
if(!require(reshape2))install.packages("reshape2");library("reshape2")
if(!require(readr))install.packages("readr");library("readr") #para usar la funci?n read.file()
if(!require(readtext))install.packages("readtext");library("readtext")  #para poder leer el archivo
if(!require(XML))install.packages("XML");library("XML") #para leer archivos .xml y ejecutar FCrSTATS
if(!require(xml2))install.packages("xml2");library("xml2")  #para leer archivos .xml
if(!require(dplyr))install.packages("dplyr");library("dplyr") #para ordenar archivos y manipular datos
if(!require(e1071))install.packages("e1071");library("e1071") #para calcular Kurtosis, Fisher, etc.
if(!require(stats))install.packages("stats");library("stats") #para calculo de regresiones lineales
if(!require(Metrics))install.packages("Metrics");library("Metrics") #para calculo de RMSE
if(!require(ggtern))install.packages("ggtern");library("ggtern")  #para el calculo de funciones de densidad weighted NO PARECE FUNCIONAR!!!!
#para graficos
if(!require(ggplot2))install.packages("ggplot2");library("ggplot2") #para representar graficos de varias variables
if(!require(ggpubr))install.packages("ggpubr");library("ggpubr")  #para combinar varios ggplots
if(!require(gridExtra))install.packages("gridExtra");library("gridExtra") #for adding special plots to ggplot2.
if(!require(scales))install.packages("scales");library("scales")  #para modificar las escalas de los ejes en ggplot2
if(!require(raster))install.packages("raster");library("raster")  #to create maps
if(!require(plotly))install.packages("plotly");library("plotly")  #?3D plots surface. "plot_ly()"
if(!require(listviewer))install.packages("listviewer");library("listviewer") #para ver los typos de trazas en plotly
# schema(jsonedit = interactive()) esta instruccion se usa para mostrar las propiedades de plotly
if(!require(viridis))install.packages("viridis");library("viridis") #color scales
if(!require(RColorBrewer))install.packages("RColorBrewer");library("RColorBrewer")  #color package
      # scale_fill_brewer() for box plot, bar plot, violin plot, dot plot, etc
      # scale_color_brewer() for lines and points
if(!require(hrbrthemes))install.packages("hrbrthemes");library("hrbrthemes")  #themas
if(!require(ggthemes))install.packages("ggthemes");library("ggthemes")  #para variar los temas de los gráficos
if(!require(ggforce))install.packages("ggforce");library("ggforce") #para dibujar sobre ggplot (voronoi, circle, etc..)
if(!require(latex2exp))install.packages("latex2exp");library("latex2exp") #para utilizar latex en gráficos y expresiones
if(!require(fmsb))install.packages("fmsb");library("fmsb")  #para graficos de radar y spider
if(!require(ggrepel))install.packages("ggrepel");library("ggrepel") #para repeler texto en gráficos
if(!require(raster))install.packages("raster");library("raster")  #to create maps
if(!require(grid))install.packages("grid");library("grid")  # para dibujar
# if(!require(igraph))install.packages("igraph");library("igraph")  # para grafos y network diagrams

if(!require(factoextra))install.packages("factoextra");library("factoextra")

install.packages("xtable") #para exportar las tablas a Latex
library(xtable)

  #cargamos el archivo de presiones unitarias del partido seleccionado
pathdata <- "c:/Users/Cesar/Documents/Formacion/INVESTMAT/Doctorado_Datos/KPIs/DefensiveTypology/"
KPIab <- readRDS(file=str_c(pathdata,"pab.rds"))


################################################################################
#
# Indice de Graficos:
#   0.- Analisis variables estadísticas pAb, pBa y pAB
#   1.- Analisis de la distribucion pAb
#   2.- Analisis de las distribuciones pAB Total
#   3.- Grafico Cajas y Bigotes pAb, pBa y pAB
#   4.- Graficos dispersion y clustering CA & CC
#   
#
################################################################################


###################### 0.- Analisis variables estadísticas pAb, pBa y pAB

  #Jugadores del equipo b
pab_statJug_b <- KPIab %>% filter(Bowing=="H") #%>% select(Fr,Bowing,pab,a,b)
pab_statJug_b <- pab_statJug_b %>% group_by(b) %>% 
            summarise(nAb=n(),pAb=sum(pab),Mean_pAb=mean(pab), Median_pAb=median(pab), Sd_pAb=sd(pab),CV_pAb=Sd_pAb/Mean_pAb,
            Q1=quantile(pab,probs = 0.25),Q2=quantile(pab,probs = 0.50),Q3=quantile(pab,probs = 0.75),
            Fisher_pAb=skewness(pab,na.rm = TRUE,type=1),
            Kurtosis_pAb=kurtosis(pab,na.rm = TRUE,type=1),CA_sigma=Fisher_pAb/Sd_pAb,CC_sigma=Kurtosis_pAb/Sd_pAb) %>% ungroup() %>% arrange(Fisher_pAb)
pab_statJug_bTotal <- KPIab %>% filter(Bowing=="H")
pab_statJug_bTotal <- pab_statJug_bTotal  %>% 
            summarise(nAB=n(),pAB=sum(pab),Mean_pAB=mean(pab), Median_pAB=median(pab), Sd_pAB=sd(pab),CV_pAB=Sd_pAB/Mean_pAB,
            Q1=quantile(pab,probs = 0.25),Q2=quantile(pab,probs = 0.50),Q3=quantile(pab,probs = 0.75),
            Fisher_pAB=skewness(pab,na.rm = TRUE,type=1),
            Kurtosis_pAB=kurtosis(pab,na.rm = TRUE,type=1),CA_sigma=Fisher_pAB/Sd_pAB,CC_sigma=Kurtosis_pAB/Sd_pAB)

#Jugadores del equipo a
pab_statJug_a <- KPIab %>% filter(Bowing=="A")
pab_statJug_a <- pab_statJug_a %>% group_by(a) %>% 
            summarise(nBa=n(),pBa=sum(pab),Mean_pBa=mean(pab), Median_pBa=median(pab), Sd_pBa=sd(pab),CV_pBa=Sd_pBa/Mean_pBa,
            Q1=quantile(pab,probs = 0.25),Q2=quantile(pab,probs = 0.50),Q3=quantile(pab,probs = 0.75),
            Fisher_pBa=skewness(pab,na.rm = TRUE,type=1),
            Kurtosis_pBa=kurtosis(pab,na.rm = TRUE,type=1),CA_sigma=Fisher_pBa/Sd_pBa,CC_sigma=Kurtosis_pBa/Sd_pBa) %>% ungroup() %>% arrange(Fisher_pBa)
pab_statJug_aTotal <- KPIab %>% filter(Bowing=="A")
pab_statJug_aTotal <- pab_statJug_aTotal  %>% 
            summarise(nBA=n(),pBA=sum(pab),Mean_pBA=mean(pab), Median_pBA=median(pab), Sd_pBA=sd(pab),CV_pBA=Sd_pBA/Mean_pBA,
            Q1=quantile(pab,probs = 0.25),Q2=quantile(pab,probs = 0.50),Q3=quantile(pab,probs = 0.75),
            Fisher_pBA=skewness(pab,na.rm = TRUE,type=1),
            Kurtosis_pBA=kurtosis(pab,na.rm = TRUE,type=1),CA_sigma=Fisher_pBA/Sd_pBA,CC_sigma=Kurtosis_pBA/Sd_pBA)

#Jugadores del equipo a+b
pab_statJug_Total <- KPIab %>% group_by(Bowing) %>%
            summarise(nAB=n(),pAB=sum(pab),Mean_pAB=mean(pab), Median_pAB=median(pab), Sd_pAB=sd(pab), Min_pAB=min(pab), Max_pAB=max(pab),
            Q1=quantile(pab,probs = 0.25),Q2=quantile(pab,probs = 0.50),Q3=quantile(pab,probs = 0.75),
            Fisher_pAB=skewness(pab,na.rm = TRUE,type=1),
            Kurtosis_pAB=kurtosis(pab,na.rm = TRUE,type=1))
pab_statJug_Total <- pab_statJug_Total %>% mutate(CV=Sd_pAB/Median_pAB,Team=c("A","B"))
pab_statJug_TotalB <- pab_statJug_Total %>% filter(Bowing=="H")
pab_statJug_TotalA <- pab_statJug_Total %>% filter(Bowing=="A")

#Jugadores del equipo a+b (No sirve de momento)
pab_statJug_ab <- KPIab  #%>% select(Fr,Bowing,pab,a,b)
pab_statJug_ab <- pab_statJug_ab %>% 
            summarise(nBA=n(),pBA=sum(pab),Mean_pBA=mean(pab), Median_pBA=median(pab), Sd_pBA=sd(pab), Min_pBA=min(pab), Max_pBA=max(pab),
            Q1=quantile(pab,probs = 0.25),Q2=quantile(pab,probs = 0.50),Q3=quantile(pab,probs = 0.75),
            Fisher_pBA=skewness(pab,na.rm = TRUE,type=1),
            Kurtosis_pBA=kurtosis(pab,na.rm = TRUE,type=1)) %>% ungroup()
pab_statJug_ab <- pab_statJug_ab %>% mutate(CV=Sd_pBA/Median_pBA)

#Poblacion(interacciones)
Nb<-pab_statJug_b %>% summarise(N=sum(nAb),PAB=sum(pAb))
Na<-pab_statJug_a %>% summarise(N=sum(nBa),PBA=sum(pBa))
Nab<-pab_statJug_ab %>% summarise(N=sum(nBA),PBA=sum(pBA))

###################### 1.- Analisis de la distribucion pAb ##############################

#Obtenemos un string de los jugadores de B en orden descendiente por la Media, CA o CC (a usar con facet_wrap() )
pAb_orderByMean <- pab_statJug_b %>% arrange(desc(Mean_pAb))
pAb_orderByMean <- as.character(pAb_orderByMean$b)
pAb_orderByCA <- pab_statJug_b %>% arrange(Fisher_pAb) 
pAb_orderByCA <- as.character(pAb_orderByCA$b)
pAb_orderByCC <- pab_statJug_b %>% arrange(desc(Kurtosis_pAb)) 
pAb_orderByCC <- as.character(pAb_orderByCC$b)



############################## Histograma pAb ###################################

pab_Ab <- KPIab %>% filter(Bowing=="A")
pAb_Histogram<-ggplot(pab_Ab, aes(x=pab,fill=b,color=b))+
  geom_histogram(alpha=0.5,position="dodge",bins=100)+ #,binwidth = 1
  scale_x_continuous(breaks = pretty_breaks())+ 
  labs(title = TeX(r"($p_{ab}$, team B. Histogram functions ordered by Kurtosis coefficient (CC))"),
       x = TeX(r'($B team$)'))+
#Dibujamos la linea vertical de la media y los valores
  geom_vline(data=pab_statJug_b,aes(xintercept=Mean_pAb, color=b), size = 1, linetype = "dashed")+
  geom_vline(data=pab_statJug_b,aes(xintercept=Median_pAb, color=b), size = 0.5, linetype = "dotted")+
  geom_text(data=pab_statJug_b,mapping=aes(x=Mean_pAb+0.2,y=c(550:550),size=0.10,label=paste("Mean:",round(Mean_pAb,6))))+
  geom_text(data=pab_statJug_b,mapping=aes(x=Mean_pAb+0.2,y=c(490:490),size=0.10,label=paste("Median:",round(Median_pAb,6))))+
  geom_text(data=pab_statJug_b,mapping=aes(x=Mean_pAb+0.25,y=c(430:430),size=0.10,label=paste("CA:",round(Fisher_pAb,4))))+
  geom_text(data=pab_statJug_b,mapping=aes(x=Mean_pAb+0.25,y=c(370:370),size=0.10,label=paste("CC:",round(Kurtosis_pAb,4))))+
  #Indicamos los valores totales de N(t) si queremos
#  geom_text(data=pab_statJug_b,mapping=aes(x=c(30:30),y=c(3500:3500),label=paste("Total pAb: ",round(pAb,2))))+
  facet_wrap(~factor(b, levels=pAb_orderByCC),nrow=4)+
  #  theme_classic()
  #theme_economist()
  #theme_ipsum()+
  theme_few()+
  theme(
      legend.position="none",
      panel.spacing = unit(0.3, "lines"),
      strip.text.x = element_text(size = 6)
    )+
  xlab("pAb") +
  ylab("Count")

#  theme(legend.position = "none")
pAb_Histogram

############################## Density pAb ###################################
pab_Ab <- KPIab %>% filter(Bowing=="A")
pAb_Density<-ggplot(pab_Ab, aes(x=pab,fill=b,color=b))+
  geom_density(alpha=0.5,position="dodge")+ #
  scale_x_continuous(breaks = pretty_breaks())+
  labs(title = TeX(r"($p_{ab}$, team B. Density functions ordered by Kurtosis coefficient (CC))"),
       x = TeX(r'($B team$)'))+
#Dibujamos la linea vertical de la media y los valores
  geom_vline(data=pab_statJug_b,aes(xintercept=Mean_pAb, color=b), size = 1, linetype = "dashed")+
  geom_vline(data=pab_statJug_b,aes(xintercept=Median_pAb, color=b), size = 0.5, linetype = "dotted")+
  geom_text(data=pab_statJug_b,mapping=aes(x=Mean_pAb+0.2,y=c(4:4),size=0.25,label=paste("Mean:",round(Mean_pAb,6))))+
  geom_text(data=pab_statJug_b,mapping=aes(x=Mean_pAb+0.2,y=c(3.5:3.5),size=0.25,label=paste("Median:",round(Median_pAb,6))))+
  geom_text(data=pab_statJug_b,mapping=aes(x=Mean_pAb+0.25,y=c(3:3),size=0.25,label=paste("CA:",round(Fisher_pAb,4))))+
  geom_text(data=pab_statJug_b,mapping=aes(x=Mean_pAb+0.25,y=c(2.5:2.5),size=0.25,label=paste("CC:",round(Kurtosis_pAb,4))))+
  #Indicamos los valores totales de N(t) si queremos
#  geom_text(data=pab_statJug_b,mapping=aes(x=c(30:30),y=c(3500:3500),label=paste("Total pAb: ",round(pAb,2))))+
  facet_wrap(~factor(b, levels=pAb_orderByCC),nrow=4)+  #Los ordenamos segun queramos con el string que hemos creado antes
  #  theme_classic()
  #theme_economist()
  #theme_ipsum()+
  theme_few()+
  theme(
      legend.position="none",
      panel.spacing = unit(0.3, "lines"),
      strip.text.x = element_text(size = 8)
    )+
  xlab("pAb") +
  ylab("Count")
#  theme(legend.position = "none")
pAb_Density


###################### 2.- Analisis de la distribucion pAB TOTAL 

############################## Histograma pAB ###################################

pab_AB <- KPIab
pAB_Histogram<-ggplot(pab_AB, aes(x=pab,fill=Bowing,color=Bowing))+
  geom_histogram(alpha=0.5,position="dodge",bins=100)+ #,binwidth = 1
  scale_x_continuous(breaks = pretty_breaks())+  # Improve scales pero no parece funcionar
  labs(title = TeX(r'(Histogram functions)'),
       x = TeX(r'($A and B teams$)'))+
#Dibujamos la linea vertical de la media y los valores
  geom_vline(data=pab_statJug_Total,aes(xintercept=Mean_pAB, color=Bowing), size = 1, linetype = "dashed")+
  geom_vline(data=pab_statJug_Total,aes(xintercept=Median_pAB, color=Bowing), size = 0.5, linetype = "dotted")+
  geom_text(data=pab_statJug_Total,mapping=aes(color=Bowing,x=Mean_pAB+0.30,y=c(3500:3500),size=0.90,label=paste("Team ",Team)))+
  geom_text(data=pab_statJug_Total,mapping=aes(color=Bowing,x=Mean_pAB+0.05,y=c(3500:3500),size=0.90,label=paste("Mean:",round(Mean_pAB,6))))+
  geom_text(data=pab_statJug_Total,mapping=aes(color=Bowing,x=Mean_pAB+0.05,y=c(3200:3200),size=0.90,label=paste("Median:",round(Median_pAB,6))))+
  geom_text(data=pab_statJug_Total,mapping=aes(color=Bowing,x=Mean_pAB+0.15,y=c(3500:3500),size=0.90,label=paste("CA:",round(Fisher_pAB,4))))+
  geom_text(data=pab_statJug_Total,mapping=aes(color=Bowing,x=Mean_pAB+0.15,y=c(3200:3200),size=0.90,label=paste("CC:",round(Kurtosis_pAB,4))))+
  geom_text(data=pab_statJug_Total,mapping=aes(color=Bowing,x=Mean_pAB+0.30,y=c(3200:3200),size=0.90,label=paste("pAB:",round(pAB,0))))+
  geom_text(data=pab_statJug_Total,mapping=aes(color=Bowing,x=Mean_pAB+0.30,y=c(2900:2900),size=0.90,label=paste("nAB:",round(nAB,0))))+
  #Indicamos los valores totales de N(t) si queremos
#  geom_text(data=pab_statJug_b,mapping=aes(x=c(30:30),y=c(3500:3500),label=paste("Total pAb: ",round(pAb,2))))+
  facet_wrap(~Bowing,nrow=2)+
  #  theme_classic()
  #theme_economist()
  #theme_ipsum()+
  theme_few()+
  theme(
      legend.position="none",
      panel.spacing = unit(0.3, "lines"),
      strip.text.x = element_text(size = 8)
    )+
  xlab("pAB") +
  ylab("Count")
#  theme(legend.position = "none")
pAB_Histogram

############################## Density pAB ###################################

pab_AB <- KPIab
geom_density<-ggplot(pab_AB, aes(x=pab,fill=Bowing,color=Bowing))+
  geom_density(alpha=0.5,position="dodge")+ 
  scale_x_continuous(breaks = pretty_breaks())+  # Improve scales pero no parece funcionar
  labs(title = TeX(r'(Density functions)'),
       x = TeX(r'($A and B teams$)'))+
#Dibujamos la linea vertical de la media y los valores
  geom_vline(data=pab_statJug_Total,aes(xintercept=Mean_pAB, color=Bowing), size = 1, linetype = "dashed")+
  geom_vline(data=pab_statJug_Total,aes(xintercept=Median_pAB, color=Bowing), size = 0.5, linetype = "dotted")+
  geom_text(data=pab_statJug_Total,mapping=aes(color=Bowing,x=Mean_pAB+0.30,y=c(3.500:3.500),size=0.90,label=paste("Team ",Team)))+
  geom_text(data=pab_statJug_Total,mapping=aes(color=Bowing,x=Mean_pAB+0.05,y=c(3.500:3.500),size=0.90,label=paste("Mean:",round(Mean_pAB,6))))+
  geom_text(data=pab_statJug_Total,mapping=aes(color=Bowing,x=Mean_pAB+0.05,y=c(3.200:3.200),size=0.90,label=paste("Median:",round(Median_pAB,6))))+
  geom_text(data=pab_statJug_Total,mapping=aes(color=Bowing,x=Mean_pAB+0.15,y=c(3.500:3.500),size=0.90,label=paste("CA:",round(Fisher_pAB,4))))+
  geom_text(data=pab_statJug_Total,mapping=aes(color=Bowing,x=Mean_pAB+0.15,y=c(3.200:3.200),size=0.90,label=paste("CC:",round(Kurtosis_pAB,4))))+
  geom_text(data=pab_statJug_Total,mapping=aes(color=Bowing,x=Mean_pAB+0.30,y=c(3.200:3.200),size=0.90,label=paste("pAB:",round(pAB,0))))+
  geom_text(data=pab_statJug_Total,mapping=aes(color=Bowing,x=Mean_pAB+0.30,y=c(2.900:2.900),size=0.90,label=paste("nAB:",round(nAB,0))))+
  #Indicamos los valores totales de N(t) si queremos
#  geom_text(data=pab_statJug_b,mapping=aes(x=c(30:30),y=c(3500:3500),label=paste("Total pAb: ",round(pAb,2))))+
  facet_wrap(~Bowing,nrow=2)+
  #  theme_classic()
  #theme_economist()
  #theme_ipsum()+
  theme_few()+
  theme(
      legend.position="none",
      panel.spacing = unit(0.3, "lines"),
      strip.text.x = element_text(size = 8)
    )+
  xlab("pAB") +
  ylab("fx(pAB)")
#  theme(legend.position = "none")
geom_density


###################### 3.- Graficos cajas y bigotes, pAb, pBa y pAB

############################## Diagrama de Box-Whisker pAb ###################################
pab_Ab <- KPIab %>% filter(Bowing=="A")
pAb_boxplot<-ggplot(pab_Ab,aes(x=reorder(b,-pab,mean),y=pab,fill=b))+
  geom_boxplot()+ 
  labs(title = TeX(r"($p_{ab}$, team B. Box-Whisker diagram)"),
       x = TeX(r'($B team$)'))+
  #scale_x_discrete(b)+
  geom_jitter(width = 0.01, alpha = 0.005)+ #representa los puntos de forma traslucida
  #facet_wrap(~b)+  #Los ordenamos segun queramos con el string que hemos creado antes
  #  theme_classic()
  #theme_economist()
  #theme_ipsum()+
  theme_few()+
  theme(
      legend.position="none",
      panel.spacing = unit(0.3, "lines"),
      strip.text.x = element_text(size = 8)
    )+
  xlab("Team B") +
  ylab("pAb")
#  theme(legend.position = "none")
pAb_boxplot

############################## Diagrama de Box-Whisker pBa ###################################
pab_Ba <- KPIab %>% filter(Bowing=="H")
pBa_boxplot<-ggplot(pab_Ba,aes(x=reorder(a,-pab,mean),y=pab,fill=a))+
  geom_boxplot()+ 
  labs(title = TeX(r"($p_{ab}$, team A. Box-Whisker diagram)"),
       x = TeX(r'($A team$)'))+
  #scale_x_discrete(b)+
  geom_jitter(width = 0.01, alpha = 0.005)+ #representa los puntos de forma traslucida
  #facet_wrap(~b)+  #Los ordenamos segun queramos con el string que hemos creado antes
  #  theme_classic()
  #theme_economist()
  #theme_ipsum()+
  theme_few()+
  theme(
      legend.position="none",
      panel.spacing = unit(0.3, "lines"),
      strip.text.x = element_text(size = 8)
    )+
  xlab("Team A") +
  ylab("pBa")
#  theme(legend.position = "none")
pBa_boxplot

print(xtable(pab_statJug_b), include.rownames = FALSE)  #asi exportamos la tabla a Latex

###################### 4.- Comparacion CA & CC ##############################

################################ Jugadores equipo A
pab_statJug_a_bis <- pab_statJug_a %>% 
  mutate(
  player=a,n=nBa,p=pBa,Mean=Mean_pBa,Median=Median_pBa,Sd=Sd_pBa,CV=CV_pBa,CA=Fisher_pBa,CC=Kurtosis_pBa, Team="A")
#Cambiamos nombres y añadimos variable Team
pab_statJug_a_bis <- pab_statJug_a_bis %>% dplyr::select(Team,player,n,p,Mean,Median,Sd,CV,Q1,Q2,Q3,CA,CC,CA_sigma,CC_sigma) 

################################ Jugadores equipo B
pab_statJug_b_bis <- pab_statJug_b %>% 
  mutate(
  player=b,n=nAb,p=pAb,Mean=Mean_pAb,Median=Median_pAb,Sd=Sd_pAb,CV=CV_pAb,CA=Fisher_pAb,CC=Kurtosis_pAb, Team="B")
#Cambiamos nombres y añadimos variable Team
pab_statJug_b_bis <- pab_statJug_b_bis %>% dplyr::select(Team,player,n,p,Mean,Median,Sd,CV,Q1,Q2,Q3,CA,CC,CA_sigma,CC_sigma) 

################################ Equipo A Resumen
pab_statEquipoA <- pab_statJug_aTotal %>% mutate(
  n=nBA,p=pBA,Mean=Mean_pBA,Median=Median_pBA,Sd=Sd_pBA,CV=CV_pBA,CA=Fisher_pBA,CC=Kurtosis_pBA,Team="A"
  )
pab_statEquipoA <- pab_statEquipoA %>% dplyr::select(Team,n,p,Mean,Median,Sd,CV,Q1,Q2,Q3,CA,CC,CA_sigma,CC_sigma)  #Cambiamos nombres y añadimos variable Team

################################ Equipo B Resumen
pab_statEquipoB <- pab_statJug_bTotal %>% mutate(
  n=nAB,p=pAB,Mean=Mean_pAB,Median=Median_pAB,Sd=Sd_pAB,CV=CV_pAB,CA=Fisher_pAB,CC=Kurtosis_pAB,Team="B"
  )
pab_statEquipoB <- pab_statEquipoB %>% dplyr::select(Team,n,p,Mean,Median,Sd,CV,Q1,Q2,Q3,CA,CC,CA_sigma,CC_sigma)  #Cambiamos nombres y añadimos variable Team

################################ Juadores Equipos A+B
pab_statJug_ab_bis <- bind_rows(pab_statJug_a_bis,pab_statJug_b_bis)
################################ Juadores Equipos A+B sin porteros
pab_statJug_ab_tris <- pab_statJug_ab_bis %>% filter((Team =='A' & player !='13') | (Team =='B' & player !='34'))

################################ Equipos A+B
pab_statEquipoAB <- bind_rows(pab_statEquipoA,pab_statEquipoB)

####################### REPRESENTACION GRAFICO DISPERSION CA & CC ########################################

############################## Diagrama de dispersion ###################################
pAB_CACC_dispersion<-ggplot(pab_statJug_ab_bis,aes(x=CA_sigma,y=CC_sigma,color=Team,shape=Team))+
  geom_point(size=5)+
  #geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+  #linea de regresión
  #geom_text(aes(label=player),vjust = -1.5, hjust = 0.5)+ #
  geom_label_repel(aes(x=CA_sigma,y=CC_sigma,label=player),max.overlaps=5,show.legend = FALSE)+
    #el titulo y subtítulo lo ponemos si queremos
#labs(title = TeX(r'(Skewness. typified Assymetric and  Fisher coefficient. $\frac{CA}{{\sigma$)')),
#     x = TeX(r'($CA$)'),
#subtitle=paste(paste("Game:","2136432",". ")," teams A and B",sep=""))+
  #scale_x_discrete(b)+
  #facet_wrap(~Team)+  #Los ordenamos segun queramos con el string que hemos creado antes
  #  theme_classic()
  #theme_economist()
  #theme_ipsum()+
  scale_color_brewer(palette="Set1")+
  theme_few()+ 
  coord_fixed()+#para mantener la proporcion de los ejes visualmente
  #theme(
  #    legend.position="none",
  #    panel.spacing = unit(0.3, "lines"),
  #    strip.text.x = element_text(size = 8)
  #  )+

#Utilizar estos títulos para los coeficientes NO tipificados
  #xlab(TeX(r'(Skewness. Assymetric Fisher coefficient. $CA$)')) + #"Skewness. typified Assymetric Fisher coefficient. CA") +
  #ylab(TeX(r'(Tailedness. Kurtosis coefficient. $CC$)')) #"Tailedness. typified Kurtosis coefficient. CC")
  
  xlab(TeX(r'(Skewness. Typified Assymetric Fisher coefficient. $CA$/$\sigma$)')) + #"Skewness. typified Assymetric Fisher coefficient. CA") +
  ylab(TeX(r'(Tailedness. Typified Kurtosis coefficient. $CC$/$\sigma$)')) #"Tailedness. typified Kurtosis coefficient. CC")

  #  theme(legend.position = "none")
pAB_CACC_dispersion

############################## Clustering KMeans ###################################

pab_statJug_ab_tris <- pab_statJug_ab_bis %>% dplyr::select(CA_sigma,CC_sigma)

#Eliminamos los porteros (Team A=13, y Team B=34) si queremos
pab_statJug_ab_tris <- pab_statJug_ab_bis %>% filter((Team =='A' & player !='13') | (Team =='B' & player !='34')) 
  #cambiamos el nombre de las filas para representar el grafico
prueba <- as.data.frame(paste(pab_statJug_ab_tris$Team,pab_statJug_ab_tris$player))
prueba <- prueba %>% rename(TeamPlayer=1)
pab_statJug_ab_tris <- pab_statJug_ab_tris %>% mutate(TP=prueba$TeamPlayer)
  #nos quedamos solo con las variables para el cluster
pab_statJug_ab_tris <- pab_statJug_ab_tris %>% dplyr::select(CA_sigma,CC_sigma)
#rownames(pab_statJug_ab_tris) <- pab_statJug_ab_tris$TP
  #ejecutamos el cluster
k2 <- kmeans(pab_statJug_ab_tris, centers = 3, nstart = 25)
pab_Cluster=fviz_cluster(k2, data = pab_statJug_ab_tris,ellipse.type = "convex", ellipse.level = 0.99, ellipse.alpha = 0.2,
             repel=TRUE, ggtheme=theme_few())
pab_Cluster

