#!/usr/bin/Rscript
library(FactoMineR)
library(Rcmdr)#je sais pas si elle est necessaire


smp <- read.csv("forestfires.csv",header=TRUE,sep=",")

(smp[,3])->month_col
(smp[,4])->day_col
as.numeric(unlist(smp[,5]))->dmc_col
as.numeric(unlist(smp[,6]))->dc_col
as.numeric(unlist(smp[,7]))->isi_col
as.numeric(unlist(smp[,8]))->temp_col
as.numeric(unlist(smp[,9]))->rh_col
as.numeric(unlist(smp[,10]))->wind_col
as.numeric(unlist(smp[,11]))->rain_col
as.numeric(unlist(smp[,12]))->area_col


skewness <-function(colonne)
{
  col_length=length(colonne)
  col_average=mean(colonne)
  somme=0
  for(i in colonne)
  {
    somme=somme+(i-col_average)**3
  }
  somme=somme/(col_length*(sd(colonne)**3))
  return(somme)
}


kurtosis <-function(colonne)
{
  col_length=length(colonne)
  col_average=mean(colonne)
  somme=0
  for(i in colonne)
  {
    somme=somme+(i-col_average)**4
  }
  somme=somme/(col_length*(sd(colonne)**4))
  return(somme-3)
}

pdf("Month.pdf")
pct <- round(summary(month_col)/sum(summary(month_col))*100)
pie(summary(month_col),labels=paste(names(summary(month_col)),pct,"%",sep=" "),main="Month Repartition of forestfires")
dev.off()

pdf("Day.pdf")
pie(summary(day_col),labels=paste(names(summary(day_col)),pct,"%",sep=" "),main="Day Repartition of forestfires")
dev.off()

pdf("Histo_DMC.pdf")
hist(dmc_col,xlab="DMC",main="Histo_DMC.pdf")
boxplot(dmc_col,horizontal=TRUE,col="brown",main="Repartition of DMC",outline=FALSE)
#summary(dmc_col)
dmc_skewness=skewness(dmc_col)
dmc_kurtosis=kurtosis(dmc_col)
dev.off()

pdf("Histo_DC.pdf")
hist(dc_col,xlab="DC",main="Histo_DC.pdf")
boxplot(dc_col,horizontal=TRUE,col="brown",main="Repartition of DC",outline=FALSE)
#summary(dc_col)
dmc_skewness=skewness(dc_col)
dmc_kurtosis=kurtosis(dc_col)
dev.off()

pdf("Histo_ISI.pdf")
hist(isi_col,xlab="ISI",main="Histo_ISI.pdf")
boxplot(isi_col,horizontal=TRUE,col="brown",main="Repartition of ISI",outline=FALSE)
#summary(isi_col)
dmc_skewness=skewness(isi_col)
dmc_kurtosis=kurtosis(isi_col)
dev.off()

pdf("Histo_Temp.pdf")
hist(temp_col,xlab="Temp",main="Histo_Temp.pdf")
boxplot(temp_col,horizontal=TRUE,col="brown",main="Repartition of Temp",outline=FALSE)
#summary(temp_col)
dmc_skewness=skewness(temp_col)
dmc_kurtosis=kurtosis(temp_col)
dev.off()

pdf("Histo_RH.pdf")
hist(rh_col,xlab="RH",main="Histo_RH.pdf")
boxplot(rh_col,horizontal=TRUE,col="brown",main="Repartition of RH",outline=FALSE)
#summary(rh_col)
dmc_skewness=skewness(rh_col)
dmc_kurtosis=kurtosis(rh_col)
dev.off()

pdf("Histo_Wind.pdf")
hist(wind_col,xlab="Wind",main="Histo_Wind.pdf")
boxplot(wind_col,horizontal=TRUE,col="brown",main="Repartition of Wind",outline=FALSE)
#summary(wind_col)
dmc_skewness=skewness(wind_col)
dmc_kurtosis=kurtosis(wind_col)
dev.off()

pdf("Histo_Rain.pdf")
hist(rain_col,xlab="Rain",main="Histo_Rain.pdf")
boxplot(rain_col,horizontal=TRUE,col="brown",main="Repartition of Rain",outline=FALSE)
#summary(rain_col)
dmc_skewness=skewness(rain_col)
dmc_kurtosis=kurtosis(rain_col)
dev.off()

pdf("Histo_Area.pdf")
hist(area_col,xlab="Area",main="Histo_Area.pdf")
boxplot(area_col,horizontal=TRUE,col="brown",main="Repartition of Area",outline=FALSE)
#summary(area_col)
dmc_skewness=skewness(area_col)
dmc_kurtosis=kurtosis(area_col)
dev.off()


#############################################################################################

#plot(sort(rh_col),area_col)


#ACP

#lala=smp[5:12]#on prend toute les colonnes a partir de la 5eme
#lala2=merge(smp[3],lala)
#res.pca = PCA(lala,quanti.sup=c(8),scale.unit=TRUE, ncp=5, graph=T)


#AFC

res.ca.rows = CA(smp[,3:4])
res.ca.col = CA(smp[,3:4])











#
