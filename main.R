setwd("C:/Users/aniap/Documents/super-duper-octo-potato")
data<-read.csv("songs with countries.csv", sep=";", dec=".",header = TRUE)
data<-data[,1:15]
attach(data)
names(data)
coutries<-as.data.frame(country)

speechiness<-as.data.frame(speechiness)

countries_tab<-data.frame(table(coutries))
countries_tab[2]

combained<-data.frame(coutries,speechiness)
pcol <- c('antiquewhite2', 'aquamarine', 'azure3', 'cadetblue2', 'burlywood',
          'darkslategray1', 'lavenderblush', 'lightcoral','darksalmon')
library(ggplot2)

ggplot(transform(transform(countries_tab, Freq=Freq/sum(Freq)), labPos=cumsum(Freq)-Freq/2), 
       aes(x="", y = Freq, fill = coutries)) +
  geom_bar(width = 1, stat = "identity") +
 # scale_fill_manual(values=pcol) +
  coord_polar(theta = "y") +
  labs(title = "",x="",y="",fill="Countries")

ggplot(as.data.frame(countries_tab), aes(coutries,fill=factor(coutries))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(x="Opinie",y="Liczba odpowiedzi",fill="Opinie",title="Zadowolenie ze swojego ma³¿eñstwa")+
  theme(axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank())# + 
 # scale_fill_manual(values=pcol)+ scale_x_discrete(limits = kolejnosc)
scatterplot(speechiness ~ , data=data,
            xlab="Weight of Car", ylab="Miles Per Gallon",
            main="Enhanced Scatter Plot",
            labels=row.names(data))

qplot(data=data,x=speechiness, y=countries, log = "xy", binwidth = 0.01, color=data$danceability)
qplot(data=data,x=data$coutries,y=data$speechiness, geom = "boxplot")
qplot(data=data,x=data$speechiness,y=data$danceability,color=data$energy,facets =~data$mode)

ggplot(data, aes(x=data$speechiness, y=data$danceability)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)

ggplot(data, aes(x=data$danceability, y=data$speechiness, fill=factor(data$key))) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE)    # Don't add shaded confidence region


tab<-classIntervals(data$speechiness, style = 'jenks', n=11, intervalClosure = 'right')
tab
jenks.tests(tab)
color_code<-findColours(tab,pcol)
plot(tab, pal=pcol , main='Dochody', xlab='Dochody[PLN]', ylab='Udzia³')
plot(data$speechiness, pch=20, col=color_code, xlab='indeks', ylab='Dochody[PLN]', main='Dochody',
     ylim=c(min(data$speechines), max(data$speechines)*4/3 ))
plot(density(data$speechines),col="darksalmon",main="Dochody[PLN]",xlab="Dochody[PLN]")

library(psych)

mod<-function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

IQR<-function(x){
  return(quantile(x,0.75)-quantile(x,0.25))
}
cv<-function(v){return(sd(v)/mean(v))}
variability.fun <- function(x) {
  c(Qx=Qx(x),Vx=Vx(x),IQR = IQR(x), mode = mod(x), var = var(x), cv = cv(x))
}

Qx<-function(v){return((quantile(v,0.75)-quantile(v,0.25))/2)}
Vx<-function(v){return(100*(Qx(v)/quantile(v,0.5)))}

pearson<-function(v){return((mean(v)-mod(v))/sd(v))}
skewnessIQR<-function(v){
  return(((quantile(v,0.75)-median(v,na.rm = FALSE))-(median(v,na-rm=FALSE)-quantile(v,0.25)))/
           + ((quantile(v,0.75)-median(v,na.rm = FALSE))+(median(v,na.rm=FALSE)-quantile(v,0.25))))
}
skewnessIQR<-function(v){
  return(((quantile(v,0.75)-median(v))-(median(v)-quantile(v,0.25)))/
           + ((quantile(v,0.75)-median(v))+(median(v)-quantile(v,0.25))))
}
kurtosisIQR<-function(v){
  return( (quantile(v,0.75)-quantile(v,0.25))/
            + (2*(quantile(v,0.9)-quantile(v,0.1))) )
}
skewkurtosi.fun<-function(x){
  c(skewness=skew(x),kurtosis=kurtosi(x),pearson=pearson(x),skewnessIQR=skewnessIQR(x),kurtosisIQR=kurtosisIQR(x))
}
robust.fun<-function(x){
  c(meanAD=mad(x,center=mean(x),na.rm=FALSE),medianAD=mad(x,center=median(x),na.rm=FALSE),trimmean=mean(x,trim=0.05),
    winsormean=winsor.mean(x,trim=0.1),winsorsd=winsor.sd(x,trim=0.1))
}

summary(data$speechiness)
variability.fun(data$speechiness)
skewkurtosi.fun(data$speechiness)
robust.fun(data$speechiness)

summary(data$danceability)
variability.fun(data$danceability)
skewkurtosi.fun(data$danceability)
robust.fun(data$danceability)

summary(data$liveness)
variability.fun(data$liveness)
skewkurtosi.fun(data$liveness)
robust.fun(data$liveness)

summary(data$energy)
variability.fun(data$energy)
skewkurtosi.fun(data$energy)
robust.fun(data$energy)

summary(data$loudness)
variability.fun(data$loudness)
skewkurtosi.fun(data$loudness)
robust.fun(data$loudness)

par(mar=c(7,4,5,2))
qplot(data$speechiness, geom="histogram",binwidth = 0.05,  
      main = "Histogram for speechiness", 
      xlab = "speechiness",  
      fill=I("green"), 
      alpha=I(.4))
qplot(data$energy, geom="histogram",binwidth = 0.05,  
      main = "Histogram for energy", 
      xlab = "energy",  
      fill=I("green"), 
      alpha=I(.4))
qplot(data$liveness, geom="histogram",binwidth = 0.05,  
      main = "Histogram for liveness", 
      xlab = "liveness",  
      fill=I("green"), 
      alpha=I(.4))
qplot(data$danceability, geom="histogram",binwidth = 0.05,  
      main = "Histogram for danceability", 
      xlab = "danceability",  
      fill=I("green"), 
      alpha=I(.4))
#predicted_df <- data.frame(mpg_pred = predict(x, eh), y=danceability)
#ggplot(data = eh, aes(x = speechiness, y=danceability)) + 
#geom_point(color='blue') +
#geom_line(color='red',data = predicted_df, aes(x=mpg_pred))

library(classInt)
tab <- classIntervals(spfree, n=11, style='jenks', intervalClosure = 'right');
jenks.tests(tab)
boxplot(speechiness, col='lightblue')

library(np)

qplot(log(data$speechiness), geom="histogram",binwidth = 0.5,  
      main = "Histogram for speechiness", 
      xlab = "speechiness",  
      fill=I("green"), 
      alpha=I(.4))

skim<-data[,4:7]
skim<-data.frame(skim,data[,10:15])
skim2<-data.frame(skim[,1:5],skim[,7:10])
full<-lm(log(data$speechiness)~.,data=skim)
summary(full)
step(full,direction = "both")
#DIAGNOSTICS (we check assumptions)
library(car)
#linearity?
crPlots(full)
#services is boolean (you work - 1, you don't - 0, dumming variable)
qqPlot(model1$residuals)
#homoscedasticity?
ncvTest(model1)
plot(model1)
#not significant p-value,
#p-value = 0.14 - area under right tale of function

#are data correlated (is one residual influencing another?)
#265. - not an outlier, but slighly changing coefficients
#HOMEWORK variance inflation factor VIR (multicolinearinty problem)
#VIF(1/(1-Rj^2))

