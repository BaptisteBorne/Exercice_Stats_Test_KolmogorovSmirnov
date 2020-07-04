# Exercices Statistiques - Kolmogorov-Smirnov

set.seed(254)
k=1:100
l=1:100
l
for (i in 1:100)
{
  u=rexp(1,1)
  l[i]=u
  
}

l

df<-data.frame(k,l)
ggplot(df, aes(k,l))+geom_line(size=0.3,colour="darkblue")

l
?order
order(l)

so=l[order(l)]
#les statistiques d'ordres

l2=1:100
for (i in 1:100)
{
    l2[i]=i/100
}
l2

df2<-data.frame(so,l2)
ggplot(df, aes(so,l2))+geom_point(size=1,colour="red")
par(new=TRUE)
ggplot(df, aes(so,l2))+geom_step(colour="darkblue")
?geom_point

#geom_line(data = df, aes(x = date, y = unRate, colour = country, shape = country)) +

ggplot()+geom_point(data=df,aes(so,l2, colour="red"))+geom_step(data=df,aes(so,l2,colour="darkblue")) +stat_function(fun =pnorm, colour="black")


set.seed(1234)
df3 <- data.frame(
  sex=factor(rep(c("F", "M"), each=200)),
  weight=round(c(rnorm(200, mean=55, sd=5),
                 rnorm(200, mean=65, sd=5)))
)
df3

par(new=FALSE)

curve(pexp(x,1), from = 0, to = 16)
plot(x=so,y=l2)
?pexp

p1 <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + ylab("") +
  scale_y_continuous(breaks = NULL)
p1


cubeFun <- function(x) {
  x^3 * 0.5
}

p9 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = cubeFun)
p9

p2 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun =pnorm) +geom_step(data=df,aes(so,l2,colour="darkblue"))
p2

?pnorm

?stat_function

alph=0.05
zalpha=sqrt(1/2*log(2/alph))
zalpha/10

plotdata <- data.frame(x=so, y=l2, lower = (l2-zalpha/10), upper = (l2+zalpha/10), qdown=qnorm((l2-zalpha/10)), qup=qnorm((l2+zalpha/10)))

pp<-ggplot() + geom_step(data=plotdata,aes(so,l2,colour="darkblue")) +geom_ribbon(data=plotdata, aes(ymin=lower, ymax=upper, x=x, fill = "band"), alpha = 0.3, fill="darkseagreen4")
pp                       


test <- function(x) {pnorm(x,1,1/4)}

a=1.3
b=-1.1
droite<-function(x){a*x+b}

ggplot(data.frame(x=c(0,4)),aes(x=x))+stat_function(fun=droite)



p4 <- ggplot(data.frame(x = c(0, 4)), aes(x = x)) +
  stat_function(fun =test, colour="red") +geom_step(data=plotdata,aes(so,l2),colour="darkblue")+geom_ribbon(data=plotdata, aes(ymin=lower, ymax=upper, x=x, fill = "band"), alpha = 0.3, fill="darkseagreen4")
p4


p5 <- ggplot(data.frame(x = c(0, 4)), aes(x = x)) +geom_step(data=plotdata,aes(so,lower),colour="darkblue")+geom_step(data=plotdata,aes(so,upper),colour="darkblue")
p5

ptest<- ggplot(data.frame(x = c(0, 4)), aes(x=x)) +geom_step(data=plotdata,aes(so,qdown),colour="darkblue")+geom_step(data=plotdata,aes(so,qup),colour="darkblue")+stat_function(fun =droite, colour="red")
ptest

test_norm<-function(x){pnorm(x,-b/a,1/a)}

ppp<- ggplot(data.frame(x = c(0, 4)), aes(x = x)) + stat_function(fun =test, colour="lightskyblue")+
  stat_function(fun =test_norm, colour="red") +geom_step(data=plotdata,aes(so,l2),colour="darkblue")+geom_ribbon(data=plotdata, aes(ymin=lower, ymax=upper, x=x, fill = "band"), alpha = 0.3, fill="darkseagreen4")
ppp

teest=0.5*log(2/0.05)
teest/10

sqrt(0.5*log(2/0.05))

?pnorm
