library(readxl)
library(reshape2)
library(plyr)
library(ggplot2)
library(here)
library(Rmisc)
Full <- read_excel(here("combined","Online.xlsx"))

Full2 <- subset(Full, select = c(
  "PID", "Robots", "Drone", "To.what.degree.did.you.feel.that.the.drone.acknowledged.your.presence.",
  "To.what.degree.did.you.feel.that.the.drone.acknowledged.your.presence..1",
  "To.what.degree.did.you.feel.that.the.drone.acknowledged.your.presence..2",
  "To.what.degree.did.you.feel.that.the.drone.acknowledged.your.presence..3"
)
)
colnames(Full2) <- c("PID", "Robots", "Drone", "P1", "P2", "P3", "P4")

Full2 <- melt(Full2, id.vars=c("PID", "Robots", "Drone"))
Full2$Drone <- as.factor(Full$Drone)
Full2$Robots <- as.factor(Full$Robots)

PID <- Full2$PID
Robots <- Full2$Robots
Drone <- Full2$Drone
Encounter <- Full2$variable
Degree <- Full2$value

Experience <- aov(Degree ~ Robots)
summary(Experience)

TukeyHSD(Experience, conf.level = 0.95)


gg <- melt(Full2, id.vars=c("Robots"))
gg <- subset(gg, variable == "value")
gg[,3] <- sapply(gg[,3], as.numeric)
gg[,3] <- gg[,3] * 10
gg2 <- summarySE(gg, measurevar="value", groupvars=c("Robots"))



pdf(file="ci-plot-online-robot.pdf",width=5,height=2.82)
ggplot(gg2, aes(Robots, value, colour=Robots)) + theme_bw() +
  geom_point(position=position_dodge(0.2), size=2.5, colour="black") +
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.2, position = position_dodge(0.2), colour="black") +
  scale_x_discrete(limits=c("low", "none", "high", "medium")) +
  scale_y_continuous(limits = c(0,100)) +
  ylab("Degree of feeling acknowledged(%) ") + xlab("Robot Experience") + theme(legend.position="none")
dev.off()

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {

  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

