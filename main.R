setwd("C:/Users/aniap/Documents/semestr 2/Descriptive statistic/bivariate variables analiysis/featuresdf")
data<-read.csv("featuresdf.csv", sep=",", dec=".",header = TRUE)
data<-data[,4:15]
attach(data)
names(data)
data$speechiness

library(classInt)
pcol <- c('antiquewhite2', 'aquamarine', 'azure3', 'cadetblue2', 'burlywood',
          'darkslategray1', 'lavenderblush', 'lightcoral','darksalmon')

tab<-classIntervals(data$speechiness, style = 'jenks', n=11, intervalClosure = 'right')
tab
jenks.tests(tab)
color_code<-findColours(tab,pcol)
plot(tab, pal=pcol , main='Dochody', xlab='Dochody[PLN]', ylab='Udzia³')
plot(data$speechiness, pch=20, col=color_code, xlab='indeks', ylab='Dochody[PLN]', main='Dochody',
     ylim=c(min(data$speechines), max(data$speechines)*4/3 ))
plot(density(data$speechines),col="darksalmon",main="Dochody[PLN]",xlab="Dochody[PLN]")


mod<-function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

IQR<-function(x){
  return(quantile(x,0.75)-quantile(x,0.25))
}
IQR_deviation <-function(x){
  sd(x[which(x>=quantile(x,0.25) & x<=quantile(x,0.75))])
}
IQR_cv <-function(x){
  cv(x[which(x>=quantile(x,0.25) & x<=quantile(x,0.75))])
}
IQR_deviation(IQ)
cv<-function(v){return(sd(v)/mean(v))}
Qx<-function(v){return((quantile(v,0.75)-quantile(v,0.25))/2)}
Vx<-function(v){return(100*(Qx(v)/quantile(v,0.5)))}

zad2.fun<- function(x){
  c(summary = summary(x), variability=var(x), mode = mod(x),
    standard_deviation=sd(x), perc95 = quantile(x, 0.95), coefficient_of_variancy = cv(x),
    IQR = IQR(x), IQR_deviation = IQR_deviation(x), IQR_cv = IQR_cv(x), skewness = skew(x),
    kurtosis = kurtosi(x))
}

zad2.fun(data$speechiness)

robust.fun<-function(x){
  c(meanAD=mad(x,center=mean(x),na.rm=FALSE),medianAD=mad(x,center=median(x),na.rm=FALSE),trimmean=mean(x,trim=0.05),
    winsormean=winsor.mean(x,trim=0.1),winsorsd=winsor.sd(x,trim=0.1))
}

robust.fun(data$speechiness)

library(ggplot2)
qplot(data$speechiness)
qplot(data=data,x=data$speechiness, y=data$energy, log = "xy", binwidth = 0.01, color=data$danceability)
qplot(data=data,x=data$speechiness,y=data$danceability, geom = "boxplot")
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

