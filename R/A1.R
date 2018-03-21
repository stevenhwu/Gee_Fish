  require(gplots)
  require(beanplot)
  require(RColorBrewer)

  colPal<- brewer.pal(8,"Set2")

  dataRaw<- read.csv("tambopata.csv")

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
