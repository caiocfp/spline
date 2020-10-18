getwd()



#############################################################################
##pacotes###
############################################################################

pacotes <- c("dplyr","splines","reshape2","ggplot2")
lapply(pacotes, library, character.only = T)
rm(pacote)



##########################################################################
#criar dataframe###
##########################################################################

gen_data1 <- function(N=200,s =25, beta1 = 10, beta2 = 100){
  Erro <- rnorm(N,25,s)
  Investimento  <- rnorm(N,100,s)
  Receita       <- rnorm(N,100,s) + beta1*Investimento^2 + beta2*Investimento + Erro
  data.frame(Investimento,Receita)
  
}


gen_data2 <- function(N=200,s = 50, beta1 = 0.005, beta2 = 10, beta3 = 100){
  Erro <- rnorm(N,100,s)
  Investimento  <- rnorm(N,50,s)
  Receita       <- rnorm(N,100,s) -beta1*Investimento^3 + beta2*Investimento^2 + beta3*Investimento + Erro
  data.frame(Investimento,Receita)
  
}





set.seed(279)
Marketing1 <- gen_data1()
Marketing2 <- gen_data2()
Marketing <- rbind.data.frame(Marketing1,Marketing2)
##########################################################################
##analise exploratoria##
#########################################################################

summary(Marketing)

ggplot(Marketing, aes(Investimento, Receita)) + 
  geom_point(aes(color = Receita))




#########################################################################
##prepaprar a amostra de treino##
#########################################################################

###############################################################################
##separar amostra de treino e amostra de teste#
##############################################################################
set.seed(616)

treino <- sample(400,200)


attach(Marketing)

investLims <- range(Investimento)
invest.Grid <- seq(from = investLims[1], to = investLims[2])



##############################################################################
#SPLINE#
##############################################################################
##Receita e investimnento####
##############################################################################

fit.Spline <- lm(Receita ~ bs(Investimento, knots = c(0,50,100)),
                data = Marketing,
                subset = treino)
summary(fit.Spline)  


Predspline <- predict(fit.Spline, newdata = list(Investimento = invest.Grid), se = T) 



plot(Investimento,Receita, col = "gray")
lines(invest.Grid, Predspline$fit, col = "red")
lines(invest.Grid, Predspline$fit + 2*Predspline$se, lty = "dashed")
lines(invest.Grid, Predspline$fit - 2*Predspline$se, lty = "dashed")
