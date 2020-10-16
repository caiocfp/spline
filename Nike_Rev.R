
getwd()


NikeRev <- read.csv('bq-results-20201014-153015-so46jbh2aozr.csv',
                    sep = ",",
                    stringsAsFactors = T)

pacotes <- c("dplyr","reshape2","ggplot2","ggthemes","ggrepel","RColorBrewer",
             "ChannelAttribution","markovchain","xlsx","shape","diagram",
             "igraph","stringr","tidyr","shiny","plotly",
             "glmnet","glmnet","boot","lmtest","qpcR", "reshape2","splines")

lapply(pacotes, library, character.only = T)
rm(pacotes)




str(NikeRev)
NikeRev$data <- as.Date(NikeRev$data)

##############################################################
#EXPLORACAO##
##############################################################

attach(NikeRev)
NikeRevNum <- as.data.frame(cbind(investimento,
                                  impressoes,                                          
                                  views,             
                                  clicks,            
                                  entries,                                     
                                  orders,                                      
                                  revenue,
                                  bounces))




summary(NikeRevNum)
############################################################################
#REVENUE E INVESTIMENTO#
############################################################################

ggplot(NikeRev_1, aes(investimento,revenue)) +
  geom_point(aes(color = campanha)) +
  geom_smooth(se = F) +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=10, angle=90),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=10, angle=45)) +
  labs(
    title = paste(
      "Receita por investimento"
    ),
    subtitle = paste("considerando a campanha"
    ))



############################################################################
#REVENUE E PARTICIPATION#
############################################################################

ggplot(NikeRevNum, aes(participationrevenue,revenue)) +
  geom_point(aes(color = NikeRev$campanha )) +
  geom_smooth(se = F) +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                         size=2, angle=90),
              axis.text.y = element_text(face="bold", color="black", 
                                         size=5, angle=45)) +
  labs(
    title = paste(
      "Receita por participação"))


############################################################################
#Revenue por campanha##
############################################################################

ggplot(NikeRevNum, aes(canal,revenue)) +
  geom_point(aes(color = campanha)) +
  geom_smooth(se = F) +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=2, angle=90),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=5, angle=45)) +
  labs(
    title = paste(
      "Receita por canal"
    ),
    subtitle = paste("considerando a campanha"
    ))

###########################################################################
#Revenue por uniquevisitor###########
###########################################################################

ggplot(NikeRevNum, aes(uniquevisitors,revenue)) +
  geom_point(aes(color = campanha)) +
  geom_smooth(se = F) +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=2, angle=90),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=5, angle=45)) +
  labs(
    title = paste(
      "Receita por uniquevisitor"
    ),
    subtitle = paste("considerando a campanha"
    ))


###########################################################################
#Revenue por Orders###########
###########################################################################

ggplot(NikeRevNum_1, aes(orders,revenue)) +
  geom_point(aes(color = NikeRev_1$campanha)) +
  geom_smooth(se = F) +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=2, angle=90),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=5, angle=45)) +
  labs(
    title = paste(
      "Receita por Orders"
    ),
    subtitle = paste("considerando a campanha"
    ))




###########################################################################
#Inevstimento por Orders###########
###########################################################################

ggplot(NikeRevNum_1, aes(investimento,orders)) +
  geom_point(aes(color = NikeRev_1$campanha)) +
  geom_smooth(se = F) +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=10, angle=90),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=10, angle=45)) +
  labs(
    title = paste(
      "Orders por investimento"
    ),
    subtitle = paste("considerando a campanha"
    ))




#####################################################################
##outliers###
#####################################################################


NikeRev_1 <- NikeRev %>%
                filter(revenue < 15000)

NikeRevNum_1 <- NikeRev %>%
                filter(revenue < 15000)


#####################################################################
##correlograma###
#####################################################################

Correlograma <- round(cor(NikeRevNum_1),2)

triSuperior <- function(Correlograma) {
  Correlograma[lower.tri(Correlograma)] <- NA
  return(Correlograma)
}

reordenarCorr <- function(Correlograma) {
  dd <- as.dist((1-Correlograma)/2)
  hc <- hclust(dd)
  Correlograma <- Correlograma[hc$order, hc$order]
}


Correlograma <- reordenarCorr(Correlograma)
corrSup <- triSuperior(Correlograma)
fusCorInf <- melt(corrSup, na.rm = T)


MapaCor <- ggplot(fusCorInf, aes(Var2, Var1, fill = value))+
             geom_tile(color = "white")+
             scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
             theme_minimal()+ # minimal theme
             theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
                coord_fixed()
 
 
MapaCor +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))



###############################################################################
#separar vetores de campanha#
##############################################################################
unique(NikeRev_1$campanha)


Campanhas <- cbind(digitalenablement <- ifelse(NikeRev_1$campanha == "digitalenablement",1,0),
                   bnt <- ifelse(NikeRev_1$campanha == "bnt",1,0),
                   react2 <- ifelse(NikeRev_1$campanha == "react2",1,0),
                   airmax <- ifelse(NikeRev_1$campanha == "airmax",1,0),          
                   joyride <- ifelse(NikeRev_1$campanha == "joyride",1,0),
                   airforce1 <- ifelse(NikeRev_1$campanha == "airforce1",1,0),
                   react <- ifelse(NikeRev_1$campanha == "react",1,0),
                   phantom <- ifelse(NikeRev_1$campanha == "phantom",1,0),         
                   wwc <- ifelse(NikeRev_1$campanha == "wwc",1,0),
                   yoga <- ifelse(NikeRev_1$campanha == "yoga",1,0),
                   pegasus37 <- ifelse(NikeRev_1$campanha == "pegasus37",1,0),
                   womens19 <- ifelse(NikeRev_1$campanha == "womens19",1,0),         
                   athetestories <- ifelse(NikeRev_1$campanha == "athetestories",1,0),
                   ycss <- ifelse(NikeRev_1$campanha == "ycss",1,0),
                   corinthians <- ifelse(NikeRev_1$campanha == "corinthians",1,0),
                   bntcopaamerica <- ifelse(NikeRev_1$campanha == "bntcopaamerica",1,0))

 
colnames(Campanhas) <- c("digitalenablement",
                         "bnt",
                         "react2",
                         "airmax",          
                         "joyride",
                         "airforce1", 
                         "react",
                         "phantom",         
                         "wwc",
                         "yoga",
                         "pegasus37",
                         "womens19",         
                         "athetestories",
                         "ycss",
                         "corinthians",
                         "bntcopaamerica" )

Campanhas <- as.data.frame(Campanhas)

NikeRevNum_1 <- (cbind(NikeRevNum_1$investimento,
                NikeRevNum_1$revenue))

NikeRevNum_1 <- as.data.frame(NikeRevNum_1)

colnames(NikeRevNum_1) <- c("investimento",
                            "receita")


###############################################################################
##separar amostra de treino e amostra de teste#
##############################################################################
set.seed(616)

treino <- sample(15823,7911)


attach(NikeRevNum_1)

investLims <- range(investimento)
invest.Grid <- seq(from = investLims[1], to = investLims[2])

##############################################################################
  #SPLINE#
##############################################################################
##Receita e invetimnento####
##############################################################################

NikSpline <- lm(revenue ~ bs(investimento, knots = c(1000,10000,30000)),
                data = NikeRevNum_1,
                subset = treino)
summary(NikSpline)

nikPredSpline <- predict(NikSpline, newdata = list(investimento = invest.Grid), se = T) 



plot(investimento,receita, col = "gray")
lines(invest.Grid, nikPredSpline$fit, col = "red")
lines(invest.Grid, nikPredSpline$fit + 2*nikPredSpline$se, lty = "dashed")
lines(invest.Grid, nikPredSpline$fit - 2*nikPredSpline$se, lty = "dashed")
