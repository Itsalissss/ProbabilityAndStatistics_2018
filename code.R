rm(list=ls())



npm400_prova <- read.table('npm400.txt', header = TRUE)

View(npm400_prova)
dim(npm400_prova)

npm400 <- as.matrix(npm400_prova[,1:6]) #da aggiornare con il numero di colonne!!


mean3 <-mean(npm400)
sd3 <-sd(npm400)

hist(npm400,probability=TRUE,col=col)
n = 48
alpha = 0.1
qnorm(mean3, sd3, lower.tail = TRUE, log.p = FALSE)
quantile3 <- qnorm(1-alpha/2)
IC.npm400 <- c(mean3 - quantile3*sd3/sqrt(n), mean3 + quantile3*sd3/sqrt(n))
IC.npm400
mu0 = 0
Z03 <- (mean3-mu0)/(sd3/sqrt(n))
quantile
Z03
abs(Z03)<quantile3 #TRUE non rifiuto H0 (MU=0)

pnorm(quantile3, mean3, sd3, lower.tail = TRUE, log.p = FALSE)
pval3 <- 2*(1-pnorm(abs(Z03)))
pval3

boxplot(npm400)

shapiro.test(npm400)
qqnorm(npm400)

qqline(npm400)

f<- c(npm400[1,])
alpha <- 0.05
mean <-mean(f)
t<- qt(f, 5, 1-alpha/2)

IP <- c("inf"=mean-t*s, "sup"=mean+t*s)
IP


z <- qnorm(1-alpha/2)
s <- sd3/sqrt(6)
confidence <- c("inf"=mean-z*s, "sup"=mean+z*s)
confidence



x.dnorm<-dnorm(48,mean=0, sd=1)
med.norm<-mean(x.dnorm)
var.norm<-var(x.dnorm)
m.est<-med.norm/var.norm
s.est<-((med.norm)^2)/var.norm
m.est
s.est


sd<- sd(22, 20, 19, 25)
 

library(stats4)
 
LL <- function(m.est, s.est) {
  -n*s.est*log(m.est)-n*log((s.est))+(s.est-1)*sum(log(x.dnorm))+m.est*sum(x.dnorm)}
    
est<- mle(minuslogl=LL, start=list(m.est=2,s.est=1), method = "BFGS", fixed= list(m.est=2, s.est=1))
summary(est)


help(t.test)


t <- (mean(npm400[2,]) - mu0)/(sd(x)/sqrt(4))


x<- npm400[2,]


res<- t.test(x, sigma.x=sd(x), mu=mean(x), alternative= "less", conf.level = 0.95)
res$statistic

p.value <- 2 * pnorm(-abs(t))



summary(res)

p<-npm400[1,]
t.test(p, alternative= "less", mu=mean(p), sigma.p=sd(p), conf.level= 0.95 )

sd(npm400[2,])
help(sd)

g<-c(489, 544, 509, 514, 535, 498, 545, 529, 506, 536)
mean(g)

library(stats)
u<- c(22.0, 20.00, 19.00, 25.12, 10.00, 8.50)
y<- c(7.7, 25.25, 48.50, 67.00, 22.75, 8.00)
n<- 6
modello <- lm(formula = y ~ u, u = TRUE, y = TRUE)
modello$coefficients
modello$residuals
modello$rank
modello$fitted.values
modello$df.residual
modello$u
modello$y
summary(modello)
modello


regressori<- read.table('regressori2016.txt', header = TRUE)
regr <- regressori[,1]
y<- regressori[,2]

modello1 = lm(formula = y ~ regr, x = TRUE, y = TRUE)
summary(modello1)
plot(modello1)


regressori<- read.table('regressori2017.txt', header = TRUE)
regr2 <- regressori[,1]
j<- regressori[,2]

modello2 = lm(formula = j ~ regr2, x = TRUE, y = TRUE)
summary(modello2)
plot(modello2)
