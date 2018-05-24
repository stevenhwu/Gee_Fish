library(RColorBrewer)
library(gplots)
cPal <- brewer.pal(8, "Dark2")

data <- read.csv("PhysicochemPar.csv")

# pdf("PhysicochemPar.pdf")
summary(data)
cat("", file="PhysiSummary.txt")
for(r in c("pH", "Temp", "Conduct.", "DO", "Nitrate",
"Turbidity")){
  result <- aov(formula(paste(r, " ~ River * M.T * Season")) , data=data)
  cat(r, capture.output(summary(result)), "\n", sep="\n", file="PhysiSummary.txt", append=T)
  cat(capture.output(TukeyHSD(result, "River")), sep="\n", file="PhysiSummary.txt", append=T)
  
  for( ss in c("D", "W")){
    datasub <- data[data$Season==ss, ]
    result <- aov(formula(paste(r, " ~ River * M.T")) , data=datasub)
    cat(ss, capture.output(summary(result)), "\n", sep="\n", file="PhysiSummary.txt", append=T)
    cat(capture.output(TukeyHSD(result, "River")), sep="\n", file="PhysiSummary.txt", append=T)
  }

}


summary(data)
cat("", file="PhysiSummary2.txt")
for(r in c("pH", "Temp", "Conduct.", "DO", "Nitrate",
"Turbidity")){
  result <- aov(formula(paste(r, " ~ River + M.T + Season")) , data=data)
  cat(r, capture.output(summary(result)), "\n", sep="\n", file="PhysiSummary2.txt", append=T)
  cat(capture.output(TukeyHSD(result, "River")), sep="\n", file="PhysiSummary2.txt", append=T)
}


r <- "Turbidity"

result <- aov(Temp ~ Season * River * M.T , data=data)
summary(result)
TukeyHSD(result, "River")



table(data$River, data$Season)
data$RS <- interaction(data$result <- aov(pH ~ River * M.T * Season  , data=data)
summary(result)
TukeyHSD(result, "River")
River, data$Season)
# plot.default(data$RS, Hgnorm ~ RS, data=data, type="p")
plot(Hgnorm ~ RS, data=data)
# data$River <- factor()

png("Hg_RS.png", width=1000, height=1000)
par(mar=c(5,4,0.5,0.5), cex=2)
plot.default(data$River, data$Hgnorm, col=cPal[data$Season], ylab="Total Hg (mg/kg)", xlab="River", xaxt="n", cex=1.5)
axis(1, at=1:3, levels(data$River))
legend("topright", legend=levels(data$Season), fill=cPal[1:2], bty="n")
dev.off()
# Method
# Two-way ANOVA are used to test Hg concentrations between two factors, these are seasons and rivers. Tukey HSD were used to perform pairwise comparision between three rivers.
# Result
# ANOVA result showed that there are no evidence of interaction between river and season (p-value=0.491). Hence we can inteprete these two factors from the ANOVA table. Both factors are significant (season p-value < 0.001, river p-value=0.01). Tukey HSD showed that there is significant difference between M and H, but not significant between other pairwise comparisions.

# For your reference
# ANOVA Table
Df Sum Sq Mean Sq F value   Pr(>F)
River         2  71.80   35.90   7.118 0.010390 *
Season        1 105.88  105.88  20.991 0.000788 ***
River:Season  2   7.66    3.83   0.759 0.491160
Residuals    11  55.48    5.04
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Tukey HSD table
$River
         diff       lwr      upr     p adj
M-H  4.706667  1.331973 8.081360 0.0080421
T-H  1.897500 -1.904435 5.699435 0.3996668
T-M -2.809167 -6.724617 1.106284 0.1742488













names(data)[grep("Total.Hg.Wet..ppm.", names(data))] <- "ppm"
names(data)[grep("TROPHIC.LEVEL", names(data))] <- "TROPHIC"
data[,"TROPHIC"] <- factor(data[,"TROPHIC"], label=c("T1", "T2", "T3"))
data[,"Season"] <- factor(data[,"Season"], label=c("Wet", "Dry"))
data[,"MIGRATORY"] <- factor(data[,"MIGRATORY"], label=c("NoM", "M"))
data[, "RawPpm"] <- (data[, "ppm"])
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


cairo_pdf("result20180409NotLog.pdf", width=4, height=10, onefile=T)

par(mfrow=c(3,1))
LABEL <- c("A", "B", "C")
for(i in 1:nlevels(data$TROPHIC)){
  tt <- levels(data$TROPHIC)[[i]]
  subD <- data[data$TROPHIC==tt, ]
  # TMH
    boxplot(exp(ppm) ~ River, data=subD, ylab="total Hg (ppm)")
  abline(h=(0.3), col=2)
  
  legend("topright", LABEL[i], bty="n")
}
dev.off()

ss <- list()
for(i in 1:nlevels(data$TROPHIC)){
  tt <- levels(data$TROPHIC)[[i]]
  subD <- data[data$TROPHIC==tt, ]
  ss[[i]] <- sapply(levels(subD$River), function(x){
    y <- subD[subD$River==x, "ppm"]
    n <- length(y)
    cat(n, "\n")
    return(c(mean(y), sd(y), sd(y)/sqrt(n), mean(exp(y)), sd(exp(y)), sd(exp(y))/sqrt(n) ))
      
    
  })
}
# str(ss[[1]])

lapply(ss, function(x){
  rownames(x) <- c("mean log", "sd log", "se log", "mean raw", "sd raw", "se raw")
  return(x)
})


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



## log vs not Log
