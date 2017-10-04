library(gplots)
library(beanplot)
library(RColorBrewer)
library(openxlsx)
library(readxl)

colPal<- brewer.pal(8,"Set2")

dataRaw<- list()
dataFile<- "MDD_Dataset_GM170926.xlsx"
dataRaw$H <- (read_excel(dataFile, sheet="Heath ALL"))
dataRaw$T <- (read_excel(dataFile, sheet="Just Tambo"))
dataRaw$M <- (read_excel(dataFile, sheet="Just Mali"))
# dataRaw$M <- read_excel("TambopataDataSheet_GM170413.xlsx")
  
# keepC <- c("SITE	ANGLER	METHOD	TIME	SAMPLE #	SPECIES NAME	TROPHIC LEVEL	SPECIES REF	SCIENTIFIC NAME	MASS (kg)	STRD. LENGTH (cm)	COMMENTS	MASS (g)	STRD. LENGTH (mm)	Total Hg Dry (µg/kg)	% Moisture	Total Hg Wet (ppb)	Total Hg Wet (ppm)")
keepC <- c("TIME", "TROPHIC LEVEL", "Total Hg Wet (ppm)", "SPECIES NAME", "MASS (g)", "STRD. LENGTH (mm)")
data2<- list()
for(i in 1:length(dataRaw)){
  data2[[i]] <- cbind(Location=names(dataRaw)[i], data.frame(dataRaw[[i]][,keepC]))
  names(data2[[i]])[grep("Hg.Wet..(ppm)", names(data2[[i]]))] <- "ppm"
  names(data2[[i]])[grep("TROPHIC", names(data2[[i]]))] <- "Trophic"
  names(data2[[i]])[grep("SPECIES", names(data2[[i]]))] <- "Species"
  names(data2[[i]])[grep("MASS", names(data2[[i]]))] <- "Mass"
  names(data2[[i]])[grep("LENGTH", names(data2[[i]]))] <- "Length"
  
}
names(data2)<- names(dataRaw)
data2

data3<- do.call(rbind,data2)

data3<- data3[data3$Trophic!="--", ]
data3$Trophic <- factor(data3$Trophic)


pdf("GM_normal_LT.pdf", width=12, height=12)

par(mfrow=c(2,2), pty="s")
for(ll in c("H", "T", "M")){
  a <- shapiro.test(data3$ppm[data3$Location==ll])
  resultO<- aov(ppm ~ Trophic, data=data3[data3$Location==ll, ] )
  plot(resultO, which=2, main=ll)
}
par(mfrow=c(2,2), pty="s")
for(tt in 1:3){
  # a <- shapiro.test(data3$ppm[data3$Location==ll])
  resultO<- aov(ppm ~ Location, data=data3[data3$Trophic==tt, ] )
  plot(resultO, which=2, main=tt)
}
dev.off()


# resultByT[[t]]<- aov(ppm ~ locations, data[data$TROFICO==t,])
pdf("GM_normal.pdf", width=8.5, height=11)

# plot(a)

shapiro.test(log(data3$ppm))
st<- shapiro.test(data3$ppm)

par(mfrow=c(2,1))
resultO<- aov(ppm ~ Location*Trophic, data=data3)
result<- aov(log(ppm) ~ Location*Trophic, data=data3)
plot(resultO, which=2, main="Original scale ppm")
legend("topleft", legend=sprintf("Shapiro-Wilk normality test pvalue: %e", st$p.value), bty="n")
plot(result, which=2, main="Log transformed ppm")
dev.off()


pdf("GM_ANOVA.pdf", width=8.5, height=11)
result<- aov(log(ppm) ~ Location*Trophic, data=data3)
textplot(c("Two-way ANOVA",capture.output(summary(result))))

par(mfrow=c(2,1))
for(tt in 1:3){

result<- aov(log(ppm) ~ Location, data=data3[data3$Trophic==tt, ])
textplot(c(capture.output(summary(result)),
capture.output(TukeyHSD(result))))
# print(capture.output(TukeyHSD(result)))
beanplot(log(ppm)~Location, data3[data3$Trophic==tt, ],   col=c(colPal[1],1,1,colPal[2]), log="", main=paste0("Trophic = ",tt), ylab="log(ppm)" )
# beanplot(log(ppm)~Trophic, data3[data3$Location=="M", ],   col=c(colPal[1],1,1,colPal[2]), log="", main=paste0("Location",tt) )
abline(h=log(c(0.3)), col=2)
}


for(ll in c("H", "M", "T")){
  result<- aov(log(ppm) ~ Trophic, data=data3[data3$Location==ll, ])
  textplot(c(capture.output(summary(result)),
  capture.output(TukeyHSD(result))))
  # print(capture.ou
  beanplot(log(ppm)~Trophic, data3[data3$Location==ll, ],   col=c(colPal[1],1,1,colPal[2]), log="", main=paste0("Location ",ll) )
  abline(h=log(c(0.3)), col=2)
}

dev.off()


## species with in X

aa<- data3[data3$Trophic==3 & data3$Location=="T" , ]
aa$Up<- aa[,"ppm"] > 0.3
table(aa[aa$Species>1,c( "Species", "Up")])

plot(rep(0,NROW(aa)), log(aa$ppm), col=as.numeric(factor(aa$Species)), pch=19)
abline(h=log(c(0.2,0.3,0.4)), col=2:4)

tt=3
aa<- data3[data3$Trophic==tt & data3$Location=="T", ]


## TIME
Sys.time()

as.POSIXct(Sys.time(), "UTC")


cutTime<- as.POSIXct(strptime("2017-03-31 00:00:00", "%Y-%m-%d %H:%M:%S"), "UTC")
data3$Season <- factor(data3$TIME > cutTime, labels=c("Wet", "Dry"))


pdf("GM_ANOVA_Season.pdf", width=8.5, height=11)
result<- aov(log(ppm) ~ Location*Trophic*Season, data=data3)
# result<- aov(log(ppm) ~ Season, data=data3)
summary(result)
textplot(c("Three-way ANOVA",capture.output(summary(result))))

par(mfrow=c(1,3))
for(tt in 1:3){

# result<- aov(log(ppm) ~ Location, data=data3[data3$Trophic==tt, ])
# textplot(c(capture.output(summary(result)),
# capture.output(TukeyHSD(result))))
# print(capture.output(TukeyHSD(result)))
tt<- 3
tempPlot <- data3[data3$Trophic==tt, ]
tempPlot$ppm <- log(tempPlot$ppm)
result<- aov((ppm) ~ Season*Location, data=tempPlot)
tempPlot <- split(tempPlot$ppm, list(tempPlot$Location, tempPlot$Season))
# summary(result)
# TukeyHSD(result)
beanplot(ppm ~ Season, data=tempPlot, side="both",
  col=list(c(colPal[1],1,1,colPal[2]), c(colPal[3],1,1,colPal[2])), log="")

#
# beanplot(ppm ~ Season, , side="both",
#   col=list(c(colPal[1],1,1,colPal[2]), c(colPal[3],1,1,colPal[2])), log="")
# par(mfrow=)

pdf("GM_BySeason.pdf", width=12, height=8)
for(tt in 1:3){
  tempPlot <- data3[data3$Trophic==tt, ]
  tempPlot$ppm <- log(tempPlot$ppm)
  # result<- aov((ppm) ~ Season*Location, data=tempPlot)
  tempPlot <- split(tempPlot$ppm, list(tempPlot$Location, tempPlot$Season))

  dataPlot <- tempPlot[c(1,4,2,5,3,6)]
  beanplot(dataPlot, side="both",
    col=list(c(colPal[1],1,1,colPal[2]), c(colPal[3],1,1,colPal[2])),
    log="", main=paste0("Trophic = ",tt), ylab="log(ppm)")
  legend("bottomright", legend=c("Wet", "Dry"), fill=colPal[c(1,3)], bty="n")
  # abline(h=log(c(0.3)), col=2)
  
}
for(ll in c("H", "T", "M")){
  tempPlot <- data3[data3$Location==ll, ]
  tempPlot$ppm <- log(tempPlot$ppm)
  # result<- aov((ppm) ~ Season*Location, data=tempPlot)
  tempPlot <- split(tempPlot$ppm, list(tempPlot$Trophic, tempPlot$Season))

  dataPlot <- tempPlot[c(1,4,2,5,3,6)]
  beanplot(dataPlot, side="both",
    col=list(c(colPal[1],1,1,colPal[2]), c(colPal[3],1,1,colPal[2])),
    log="", main=paste0("Location = ",ll), ylab="log(ppm)")
  legend("bottomright", legend=c("Wet", "Dry"), fill=colPal[c(1,3)], bty="n")

}
dev.off()



#correlation


pdf("GM_Correlation.pdf")
for(r in c("Mass","Length")){
  for(t in 1:3){
    dataSub<- log(data3[data3$Trophic==t, c(r, "ppm")])
    # dataSub$ppm<- log(dataSub$ppm)
    textplot(c(paste0("Trophic=",t),
      capture.output(cor.test(~.,dataSub))
      # capture.output(cor.test(~.,dataSub, method="spearman") )
    ))
    plot(dataSub, xlab=paste0("log(",r,")"), ylab="log(ppM)")
    abline(lm(dataSub[,2]~dataSub[,1]))
  }
  
  for(ll in c("H", "T", "M")){
    dataSub<- log(data3[data3$Location==ll, c(r, "ppm")])
    # dataSub$ppm<- log(dataSub$ppm)
    textplot(c(paste0("Location=", ll),
      capture.output(cor.test(~.,dataSub))
      # capture.output(cor.test(~.,dataSub, method="spearman") )
    ))
    plot(dataSub, xlab=paste0("log(",r,")"), ylab="log(ppmzz)")
    abline(lm(dataSub[,2]~dataSub[,1]))
  }

}
dev.off()




  # beanplot(log(ppm)~Location, data3[data3$Trophic==tt  & data3$Season=="Wet", ],   col=c(colPal[1],1,1,colPal[q2]), log="", main=paste0("Wet Trophic = ",tt), ylab="log(ppm)" )
# beanplot(log(ppm)~Trophic, data3[data3$Location=="M", ],   col=c(colPal[1],1,1,colPal[2]), log="", main=paste0("Location",tt) )
}


for(ll in c("H", "M", "T")){
  result<- aov(log(ppm) ~ Trophic, data=data3[data3$Location==ll, ])
  textplot(c(capture.output(summary(result)),
  capture.output(TukeyHSD(result))))
  # print(capture.ou
  beanplot(log(ppm)~Trophic, data3[data3$Location==ll, ],   col=c(colPal[1],1,1,colPal[2]), log="", main=paste0("Location ",ll) )
  abline(h=log(c(0.3)), col=2)
}

dev.off()


############################################################################3
#############################################################
    plot(tt, type="response", las=2)
  qqnorm(log(data3$ppm))
  print(lattice::qqmath(log(data3$ppm)))# move after 2/3 pages, after uni check
abline(b=1)
plot(result)
  textplot(c(sprintf("Trofico = %d", t),
    capture.output(summary(resultByT[[t]])),
    "\n\n",
    capture.output(TukeyHSD(resultByT[[t]]))
    
    )
  )
  
  
  
  beanplot(ppm~Location, data3,   col=c(colPal[1],1,1,colPal[2]), log="", main=paste0("Trofico = ",1) )
  beanplot(ppm~Location, data3,   col=c(colPal[1],1,1,colPal[2]), log="y", main=paste0("Trofico = ",1) )
  # dev.off()
  
  
  beanplot(ppm~locations, data[data$TROFICO==t, c("ppm", "locations")],
  col=c(colPal[1],1,1,colPal[2]), log="", main=paste0("Trofico = ",t) )
  # dev.off()
  boxplot(ppm~locations, data[data$TROFICO==t, c("ppm", "locations")], main=paste0("Trofico = ",t) )
}











  
  names(dataRaw)[grep("Trt",
cairo_pdf("TambapataResult.pdf", onefile=T)
data<- dataRaw
data$SITE<- factor(data$SITE)
data$TROFICO<- factor(data$TROFICO)

result<- aov(ppm ~ SITE*TROFICO, data=data)
textplot(c("All sites 1-15\n",
  capture.output(summary(result))) )

data$locations<- vector(length=NROW(data))
data$locations[data$SITE %in% c(1:5,10)]<- "T"
data$locations[data$SITE %in% 6:9]<- "B"
data$locations[data$SITE %in% 11:15]<- "H"
data$locations<- factor(data$locations)

result<- aov(ppm ~ locations*TROFICO, data=data)
textplot(c("Three locations T(1-5,10), B(6-9), H(11-15)\n",
  capture.output(summary(result))) )

# locationName<-
resultByT<- list()
for(t in 1:3){
  resultByT[[t]]<- aov(ppm ~ locations, data[data$TROFICO==t,])
  
  textplot(c(sprintf("Trofico = %d", t),
    capture.output(summary(resultByT[[t]])),
    "\n\n",
    capture.output(TukeyHSD(resultByT[[t]]))
    
    )
  )
  
  beanplot(ppm~locations, data[data$TROFICO==t, c("ppm", "locations")],
  col=c(colPal[1],1,1,colPal[2]), log="", main=paste0("Trofico = ",t) )
  # dev.off()
  boxplot(ppm~locations, data[data$TROFICO==t, c("ppm", "locations")], main=paste0("Trofico = ",t) )
}

sapply(resultByT, summary)


dev.off()



cairo_pdf("correlation.pdf", onefile=T)
for(r in c("MASS","LENGTH")){
for(t in 1:3){
  dataSub<- data[data$TROFICO==t, c(r, "ppm")]
  textplot(c(paste0("TROFICO=",t),
    capture.output(cor.test(~.,dataSub) ),
    capture.output(cor.test(~.,dataSub, method="spearman") )
  ))
  plot(dataSub)
  abline(lm(dataSub[,2]~dataSub[,1]))
}
}
dev.off()



dataT3<- data[data$TROFICO==3 & data$locations=="T", c("ppm", "MASS")]
summary(lm(ppm~MASS, data=dataT3))

dataT3a<- dataT3[dataT3$MASS<1,]
summary(lm(ppm~MASS, data=dataT3a))


cc<- brewer.pal(11, "BrBG")

summary(lm(HgWet~MASS, data=dataT3a))

png("lmData3T.png", width=4, height=6, unit="in", res=300)
plot(ppm~MASS, data=dataT3a, cex=1.5, pch=19, col=cc[10], xlim=c(0, 0.6), ylim=c(0,1.2), xlab="Mass (kg)", ylab="Fish THg (ppm, wet)")
abline(a=0.085, b=1.55)
text(0.32,400, "y=0.085x+1.550\nR^2=0.38\nP-value:0.057")
dev.off()
