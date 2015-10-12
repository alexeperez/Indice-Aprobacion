setwd("C:/Users/cecheverriaciencias/Desktop/Clases EPN/Indice aprobacion")
list.files()
library(readxl)
d <- read_excel("data.xlsx", sheet = 1, col_names = TRUE)
d$SEMESTRE <- factor(d$SEMESTRE)
str(d)
names(d)

reg <- lm(TAPR~EST2+SEMESTRE+ProfTit,
          data = d)
summary(reg)

tapply(X = d[,"TAPR"],INDEX = d[,"SEMESTRE"] , mean)


    ## diag densidad
library(ggplot2)
    g1 <- ggplot(d, aes(x=TAPR, fill=SEMESTRE))+
        geom_density(color="gray60",alpha=0.6) + 
        labs(x="", y="",fill="Semestre:") +
        xlim(c(min(d$TAPR)-1,max(d$TAPR)+1))+
        theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),
              axis.title.x=element_blank())
    ## diag cajas        
    g2 <- ggplot(d, aes(x=SEMESTRE, y=TAPR, fill=SEMESTRE))+
        geom_boxplot(outlier.colour = "red",color="gray50",width=1)+
        labs(y = "Indice aprobacion", 
             x = "", fill="Semestre:")+
        ylim(c(min(d$TAPR)-1,max(d$TAPR)+1))+
        theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
    g2

    
    
## diag barras error
        mediana <- tapply(d[,"TAPR"], INDEX = d[,"SEMESTRE"], FUN = median)
    media <-  tapply(d[,"TAPR"], INDEX = d[,"SEMESTRE"], FUN = mean)
    de <- tapply(d[,"TAPR"], INDEX = d[,"SEMESTRE"], FUN = sd)
    ee <- de/sqrt(3)
    adata <- data.frame(Semestre=c("A","B"), mediana, media, ee)

    library(ggplot2)
    g <- ggplot(adata, aes(x=Semestre, y=media, colour=Semestre))+
        geom_point(size=4) +
        geom_errorbar(aes(ymin=media-ee, ymax=media+ee), width=.2)+
        labs(y = "Indice de Aprobacion", x = "Semestre", colour="Semestre:")
    print(g)
