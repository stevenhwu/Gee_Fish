data <- read.csv("AllData2-CSV.csv")

names(data)[grep("Total.Hg.Wet..ppm.", names(data))] <- "ppm"
names(data)[grep("TROPHIC.LEVEL", names(data))] <- "TROPHIC"
data[,"TROPHIC"] <- factor(data[,"TROPHIC"], label=c("T1", "T2", "T3"))
data[,"Season"] <- factor(data[,"Season"], label=c("Wet", "Dry"))
data[,"MIGRATORY"] <- factor(data[,"MIGRATORY"], label=c("NoM", "M"))
data[, "ppm"] <- log(data[, "ppm"])
data[,"RiverShort"] <- data[,"River"]
data[,"River"] <- factor(data[,"RiverShort"], labels=c("Heath", "Malinowski", "Tambopata"))
data[,"River"] <- factor(data[,"River"], levels=c("Tambopata", "Malinowski", "Heath"))

table(data$River, data$RiverShort)


result <- aov(glm(ppm ~ (TROPHIC + River + Season )^2, data=data ))
summary(result)


result2 <- list()
posthoc <- list()
for(tt in levels(data$TROPHIC)){
  subD <- data[data$TROPHIC==tt, ]
  # result2[[tt]] <- aov(glm(ppm ~ ( River + Season + MIGRATORY)^2, data=subD ))
  result2[[tt]] <- aov(glm(ppm ~ ( River + Season )^2, data=subD ))
  posthoc[[tt]] <- TukeyHSD(x=result2[[tt]], "River")
}

library(gplots)
cairo_pdf("result20180409P.pdf", width=4, height=10, onefile=T)

par(mfrow=c(3,1))
LABEL <- c("A", "B", "C")
for(i in 1:nlevels(data$TROPHIC)){
  tt <- levels(data$TROPHIC)[[i]]
  subD <- data[data$TROPHIC==tt, ]
  # TMH
  if(i < 3){
    par(mar=c(1,4,1,3))
    boxplot(ppm ~ River, data=subD, xaxt="n", ylab="log total Hg (ppm)")
  }
  else{
    par(mar=c(5,4,1,3))
    boxplot(ppm ~ River, data=subD, xlab="River", ylab="log total Hg (ppm)")
  }
  abline(h=log(0.3), col=2)
  
  legend("topright", LABEL[i], bty="n")
}
dev.off()

cairo_pdf("result20180409.pdf", width=8, height=8, onefile=T)

textplot(capture.output(summary(result)), cex=1)
textplot(capture.output(sapply(result2, summary)), cex=1)
textplot(capture.output(posthoc), cex=0.7)
par(mfrow=c(2,2))
for(i in 1:3){
  plot(posthoc[[i]])
  mtext(side=3, sprintf("Trophic level %s", i), line=1)
}
dev.off()






##
summary(data)
sapply(unique(data$River), function(x){
  c(mean(data[data$River==x, "STRD..LENGTH..mm."]),
    median(data[data$River==x, "STRD..LENGTH..mm."])
  )
})



r <- aov(glm(STRD..LENGTH..mm. ~ River, data=data ))
post <- TukeyHSD(x=r, "River")
post
plot(post)
library(beanplot)
png("LengthvsRiver.png", width=800, height=600)
beanplot(STRD..LENGTH..mm. ~ River, ylab="Length (mm)", data=data, las=3, log="y", beanlinewd=3, col=c(cPal[2], 1, 1, cPal[1]))
dev.off()


beanplot(ppm ~ River + Season, data=data )

r <- aov(glm(ppm ~ River*Season, data=data ))
r <- aov(glm(ppm ~ River, data=data ))
library(RColorBrewer)
png("ppmVsRiverSeason.png", width=1200, height=600)
cPal <- brewer.pal(8, "Set2")
beanplot(ppm ~ River + Season, data=data, las=3, log="", beanlinewd=3,
  col=list(c(cPal[2], 1, 1, cPal[1]), c(cPal[3], 1, 1, cPal[1]), c(cPal[4], 1, 1, cPal[1]))
)
dev.off()



    },
summary(r)
r <- aov(glm(ppm ~ Season, data=data ))

boxplot(exp(ppm) ~ Season, data=data)
result[[rr]] <-
t.test(exp(ppm) ~ Season, data=data)
t.test((ppm) ~ Season, data=data)

summary(subD)
plot.default(subD$Season, exp(subD$ppm))

result <- list()
par(mfrow=c(2,3))
for(rr in levels(data$River)){
  subD <- data[data$River==rr, ]
  result[[rr]] <- t.test(exp(ppm) ~ Season, data=subD)
  boxplot(exp(ppm) ~ Season, data=subD, main=rr)
  result[[paste("log",rr)]] <- t.test((ppm) ~ Season, data=subD)
  boxplot((ppm) ~ Season, data=subD, main=paste0("log", rr))
  summary(subD)
  # plot.default(subD$Season, exp(subD$ppm))
}
sapply(result, function(x){x$p.value})


post <- TukeyHSD(x=r, "River")
post
post <- TukeyHSD(x=r, "Season")
post


dataCount <- table(data$River, data$TROPHIC, data$Season)
dataCount <- table(data$River, data$TROPHIC)
dataCount <- table(data$River, data$TROPHIC)
pvalueChi <- c(chisq.test(dataCount[c(1, 3),])$p.value,
  chisq.test(dataCount[c(1, 2),])$p.value,
  chisq.test(dataCount[c(2, 3),])$p.value)
names(pvalueChi) <- c("HvsT", "HvsM", "MvsT")
p.adjust(pvalueChi, method="bonferroni")
dataCount
