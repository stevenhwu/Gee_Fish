data <- read.csv("AllData2-CSV.csv")

names(data)[grep("Total.Hg.Wet..ppm.", names(data))] <- "ppm"
names(data)[grep("TROPHIC.LEVEL", names(data))] <- "TROPHIC"
data[,"TROPHIC"] <- factor(data[,"TROPHIC"], label=c("T1", "T2", "T3"))
data[,"Season"] <- factor(data[,"Season"], label=c("Wet", "Dry"))
data[,"MIGRATORY"] <- factor(data[,"MIGRATORY"], label=c("NoM", "M"))
data[, "ppm"] <- log(data[, "ppm"])

result <- aov(glm(ppm ~ (TROPHIC + River + Season + MIGRATORY)^2, data=data ))
summary(result)


result2 <- list()
posthoc <- list()
for(tt in levels(data$TROPHIC)){
  subD <- data[data$TROPHIC==tt, ]
  result2[[tt]] <- aov(glm(ppm ~ ( River + Season + MIGRATORY)^2, data=subD ))
  posthoc[[tt]] <- TukeyHSD(x=result2[[tt]], "River")
}

library(gplots)
cairo_pdf("result20180320.pdf", width=8.5, height=11, onefile=T)
textplot(capture.output(summary(result)))
textplot(capture.output(sapply(result2, summary)), cex=1)
textplot(capture.output(posthoc), cex=0.8)
par(mfrow=c(2,2))
for(i in 1:3){
  plot(posthoc[[i]])
  mtext(side=3, sprintf("Trophic level %s", i), line=1)
}
dev.off()
