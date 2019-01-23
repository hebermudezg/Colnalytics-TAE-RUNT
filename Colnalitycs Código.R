
#### Lectura de la base de datos RUNT ************


library(readxl)
train <- read_excel("BaseTotal.xlsx", 
                        sheet = "Diario", col_types = c("numeric", 
                                                        "date", "numeric", "text", "text", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric"))

train <- data.frame(train)
head(train)
tail(train)

require(ggplot2)


pdf(file = "xxx.pdf", height = 4.5, width= 6.8)
ggplot(train, aes(x=Fecha , y=Unidades)) + geom_line(colour = 'black') + ggtitle("Serie de tiempo de vehículos registrados en el RUNT")
ggplot(train, aes(x=Fecha , y=TRM_Promedio)) + geom_line(colour = 'black') + ggtitle("Serie de tiempo de vehículos registrados en el RUNT")
ggplot(train, aes(x=Fecha , y=Unidades)) + geom_point(colour = 'black') + ggtitle("Grafico de dispersión cantidad de vehículos registrados en el RUNT")


ggplot(train, aes(x=Mes , y=Unidades)) + geom_bar(colour = 'black') + ggtitle("Grafico de dispersión cantidad de vehículos registrados en el RUNT")
train$Mes <- factor(train$Mes ,levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November","December"))
train$Dia <- factor(train$Dia, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
ggplot(train, aes(x=Dia, y=Unidades)) + geom_bar(stat="identity") + ggtitle("Número de vehícuos registrados por Día")


(train$Dia)
dev.off()


barplot(table(train$Unidades, train$Dia), frec)



### Un grafico descriptivo melo 

pairs(train[,-c(2,4,5)])
#?Mejorando el grafico
panel.reg <- function (x, y)
{
  points(x, y, pch=20)
  abline(lm(y ~ x), lwd=2, col='dodgerblue2')
}
# Funcion para crear el histograma
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="dodgerblue2", ...)
}
# Funcion para obtener la correlacion
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * abs(r))
}

pdf(file = "D:/descriptivo222.pdf", height = 5, width = 6.8)
pairs(train[,-c(2,4,5)],
      upper.panel = panel.reg,
      diag.panel = panel.hist,
      lower.panel = panel.cor)
dev.off()


####----------------------------------------------

names(train)
train <- train[, -c(2)]  # quitandole la fecha larga 
dim(train)
names(train)
train$Mes <- as.factor(train$Mes)
train$Dia <- as.factor(train$Dia)


dim(train)

'Cooks distance (or Leverage)'
cutoff <- 4/(2192-11-2)   #Di >= 4/n-p-2
plot(mod00, which=4, cook.levels=cutoff)
abline(h=cutoff, col='lightpink', lty='dashed')











###### Modelo lineal simple ####################
mod00 <- lm(Unidades ~. , data = train)
summary(mod00)
R2 <- cor(train$Unidades, predict(mod00))^2
cor(mod00$fitted.values, train$Unidades )
plot(mod00$fitted.values, train$Unidades )
a <- data.frame(Unidades= train$Unidades, Ajutadas = mod00$fitted.values)
#View(a)
mse(mod00$fitted.values, train$Unidades)
AIC(mod00)
## Calculando el Pseudocoeficiente de determinación.
numerador <- (train$Unidades-mod00$fitted.values)^2
denominador <- (train$Unidades - mean(train$Unidades))^2
PseudoR2 <-1 - (sum(numerador)/sum(denominador))
PseudoR2







# Modelo GLM
##################################################################
require(stats)
mod11 <- glm(formula = Unidades ~Año+Mes+Dia+ICC+TRM_Promedio+PET+TGP+TO+TD+Google_Trends, family = poisson, 
         data = train)
names(train)
newdataa <- train[,-c(1)]
prediccionglm <- predict(mod11, newdata = newdataa)
cor(mod11$fitted.values, exp(prediccionglm))
cor(train$Unidades,  fitted(mod11))
plot(train$Unidades, exp(prediccionglm))
mse(fitted(mod11), train$Unidades)
rsq(mod11)
rsq(mod11, adj = T)
AIC(mod11)
plot(mod11)
a <- data.frame(Unidades= train$Unidades, Ajutadas = exp(prediccionglm))
View(a)
## Calculando el Pseudocoeficiente de determinación.
numerador <- (train$Unidades-fitted(mod11))^2
denominador <- (train$Unidades - mean(train$Unidades))^2
PseudoR2 <-1 - (sum(numerador)/sum(denominador))
PseudoR2
cor(train$Unidades,mod11$fitted.values)
plot(mod11$fitted.values, train$Unidades)
plot(mod11)


##### para el glm es fifted values (valores ajustados )





# modelo gamlss
require(gamlss)
names(train)
mod000 <- gamlss(Unidades ~ Año+Mes+Dia+ICC+TRM_Promedio+PET+TGP+TO+TD+Google_Trends, data = train)
summary(mod000)
AIC(mod000)
a <- data.frame(Unidades= train$Unidades, Ajutadas = mod000$mu.fv)
View(a)
exp(predict(mod000))
Rsq(mod000)
rsq(mod000)
coef(mod000)
plot(mod000)
plot(mod000$mu.fv, train$Unidades)
cor(mod000$mu.fv, train$Unidades)

# este modelo gamlss es mu parecido al modelo glm
## Calculando el Pseudocoeficiente de determinación.
numerador <- (train$Unidades-mod000$mu.fv)^2
denominador <- (train$Unidades - mean(train$Unidades))^2
PseudoR2 <-1 - (sum(numerador)/sum(denominador))
PseudoR2






### proceso de seleccion de variables

FORM2 <- as.formula("~Año+Mes+Dia+ICC+TRM_Promedio+PET+TGP+TO+TD+Google_Trends")
mod33 <- stepGAICAll.A(mod000, scope=list(lower=~1, upper=FORM2))
summary(mod33)
Rsq(mod33)

numerador <- (train$Unidades-mod33$mu.fv)^2
denominador <- (train$Unidades - mean(train$Unidades))^2
PseudoR2 <-1 - (sum(numerador)/sum(denominador))
PseudoR2

plot(mod33$mu.fv, train$Unidades)
plot(mod33)


### hagamos otro proceso de seleccion de variables analisis de autliers y colinealidad


require(MASS)
modback <- stepAIC(object=mod00, trace=TRUE, direction="backward", k=2)
modback$anova
summary(modback)
numerador <- (train$Unidades-modback$fitted.values)^2
denominador <- (train$Unidades - mean(train$Unidades))^2
PseudoR2 <-1 - (sum(numerador)/sum(denominador))
PseudoR2
# este modelo es un poco mejor por que explica lo mismo conn menos variables


### 


empty.model <- lm(Unidades ~ 1, data=train)
horizonte <- formula(Unidades ~Año+Mes+Dia+ICC+TRM_Promedio+PET+TGP+TO+TD+Google_Trends)
modforw <- stepAIC(empty.model, trace=T, direction="forward", scope=horizonte)
summary(modforw)
modforw$anova
numerador <- (train$Unidades-modforw$fitted.values)^2
denominador <- (train$Unidades - mean(train$Unidades))^2
PseudoR2 <-1 - (sum(numerador)/sum(denominador))
PseudoR2
# estos modelos casi que son lo mismo (estan iguales )



### Seleccion de variables en ambos sentidos.

modboth <- stepAIC(empty.model, trace=T, direction="both", scope=horizonte)
summary(modboth)



### comparaacion de los r2 

summary(mod00)$adj.r.squared
Rsq(mod000)
summary(modback)$adj.r.squared
summary(modforw)$adj.r.squared
summary(modboth)$adj.r.squared
Rsq(mod0000)


cor(mod00$fitted.values, train$Unidades)
cor(mod0000$mu.fv, train$Unidades)




### comparacion de los sigmas.
summary(mod00)$sigma
sigma(mod000)
summary(modback)$sigma
summary(modforw)$sigma
summary(modboth)$sigma
sigma(mod0000)



### comparacion de los pseudo R2

numerador1 <- (train$Unidades-mod00$fitted.values)^2
numerador2 <- (train$Unidades-mod000$mu.fv)^2
numerador3 <- (train$Unidades-modback$fitted.values)^2
numerador4 <- (train$Unidades-modforw$fitted.values)^2
numerador5 <- (train$Unidades-modboth$fitted.values)^2
numerador6 <- (train$Unidades-mod0000$mu.fv)^2

denominador <- (train$Unidades - mean(train$Unidades))^2

PseudoR21 <-1 - (sum(numerador1)/sum(denominador))
PseudoR22 <-1 - (sum(numerador2)/sum(denominador))
PseudoR23 <-1 - (sum(numerador3)/sum(denominador))
PseudoR24 <-1 - (sum(numerador4)/sum(denominador))
PseudoR25 <-1 - (sum(numerador5)/sum(denominador))
PseudoR26 <-1 - (sum(numerador6)/sum(denominador))


PseudoR21
PseudoR22
PseudoR23
PseudoR24
PseudoR25
PseudoR26


### analicemos tambien las correlacion de los modelos







##  Ajutemos un modelo gamlss con la variables seleccionadas con back
####################################################################

require(gamlss)
mod0000 <- gamlss(Unidades ~ Año + Mes + Dia + ICC + TRM_Promedio + PET + TGP + 
                    Google_Trends, family =PO, data=train)



#test2018 <- read_excel("C:/Users/Esteban Bermúdez/Desktop/test2018.xlsx")
#dim(test2018)

head(test2018)
test2018 <- data.frame(test2018)

prediccione <- predict(mod0000, newdata = test2018)
tabla <- data.frame(Predicciones = exp(prediccione))
write.csv(tabla, file = "nomejodaahorasi.csv")




write.csv(exp(daots2018), file = "20182018.csv")


names(train)
newdataa <- test2018
names(newdataa)
prediccione <- predict(mod0000, newdata = newdataa)
tabla <- data.frame(Predicciones = exp(prediccione), Predicciones2 = fitted(mod0000))
write.csv(tabla, file = "2018.csv")



plot(fitted(mod0000), train$Unidades)  # este es el plor calculado ya con la funcion de enlace
abline(lm(train$Unidades~fitted(mod0000)))

#pdf(file="chicharra33.pdf")
datos <-  data.frame(Estimados=fitted(mod0000), Vehículos_registrados = train$Unidades ) 
ggplot(datos, aes(Vehículos_registrados, Estimados)) + geom_point() +geom_smooth(method='lm')
#dev.off()
#+ scale_x_continuous(limits = c(0, 3000)) + scale_y_continuous(limits = c(0, 3000)) +
#ggtitle("Valores estimados vs valores reales ") + geom_abline(Estimados ~ Vehículos_registrados)



summary(mod0000)
Rsq(mod0000)
cor(fitted(mod0000), train$Unidades)
Rsq(mod0000)
mse(fitted(mod0000), train$Unidades)
AIC(mod0000)
names(train)
prediccion <- predict(mod0000, newdata = newdataa)
b <- data.frame(train$Unidades, exp(prediccion))
View(b)
plot(mod0000)
wp(mod0000)
AIC(mod0000)
numerador <- (train$Unidades-fitted(mod0000))^2
denominador <- (train$Unidades - mean(train$Unidades))^2
PseudoR2 <-1 - (sum(numerador)/sum(denominador))
PseudoR2



# ESTE MODELO ESTA MELO 


fitted.gamlss(mod0000)





### Analisis de la serie de tiempo (Unidades)*******************************************

library(ggplot2)
names(train)
ggplot(train1, aes(x=Fecha , y=Unidades)) + geom_line()         # Series de Tasa Representativa del Mercado (cambio del dolar)
ggplot(train, aes(x=Consecutivo , y=ICC)) + geom_line()      
ggplot(train, aes(x=Consecutivo , y=Unidades)) + geom_line()      
acf(train1$Unidades)
pacf(train1$Unidades)


##************************************************************************************