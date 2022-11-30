# Fetch data from google spreadsheets and show response matrix. -----------
library(plyr)
require(nlme)
require(multcomp)
library(lsmeans)
library(here)
rm(list=ls());
source(here("combined","FetchData.R"), local=FALSE);
FetchData();
responses

PID <- CleanedData$PID
Gender <- CleanedData$Gender
Gender <- factor(Gender) ## Remove the attack helicopter gender
summary(Gender) / 4
Age <- CleanedData$Age
table(Age) / 4
Pan <- CleanedData$Pan
Order <- CleanedData$Order
Motion <- CleanedData$Motion
Degree <- CleanedData$Degree
Encounter <- CleanedData$Encounter
Encounter <- factor(Encounter)
CleanedData$Encounter <- factor(CleanedData$Encounter)

# Show boxplot of data ----------------------------------------------------
# windows()
boxplot(Degree~Motion)
boxplot(Degree~Motion*Encounter)
# title("Acknowledgement per Motion")
# windows()
boxplot(Degree~Pan*Motion)
# title("Acknolwedgement vs Camera Motion")
# windows()
boxplot(Degree~Gender*Motion)
# title("Acknowledgement vs Gender")
# windows()
boxplot(Degree~Order*Motion)
# title("Acknowledgement vs Order")
boxplot(Degree ~ Age)


# Fool around after here ! ------------------------------------------------

aovTable <- aov(Degree ~ Motion*Encounter + Error(PID/(Motion*Encounter)))
summary(aovTable)
summary(glht(aovTable, linfct=mcp(Motion="Tukey")))

aovT <- aov(Degree ~ Motion + Error(PID/Motion))
summary(aovT)

library(sjstats)
etaSquared(aovTable, anova=TRUE)

lmeTable <- lme(Degree ~ Motion, random = ~1 | PID/Motion, data=CleanedData)
anova(lmeTable)

summary(glht(lmeTable, linfct=mcp(Motion="Tukey")))

lme.graph <- lsmeans::lsmeans(lmeTable, pairwise ~ Motion, glhargs = list())
plot(lme.graph[[2]])


library(reshape2)
library(plyr)
library(ggplot2)
library(Hmisc)
library(Rmisc)

gg <- melt(CleanedData, id.vars=c("Motion"))
gg <- subset(gg, variable == "Degree")
gg[,3] <- sapply(gg[,3], as.numeric)
gg[,3] <- gg[,3] * 10
gg2 <- summarySE(gg, measurevar="value", groupvars=c("Motion"))

pdf(file="ci-plot-online.pdf",width=5,height=3)
ggplot(gg2, aes(Motion, value, colour=Motion)) + theme_bw() + 
  geom_jitter(data = gg, aes(Motion, value), width=0.2, color="black", alpha=0.2, size=0.8) +  
  geom_point(position=position_dodge(0.2), size=2.5, colour="red") +
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.2, position = position_dodge(0.2), color="red") +
  scale_x_discrete(limits=c("Rotation", "Waggle", "Toss", "Nod"), labels=c("Orienting", "Orienting +\nWaggle", "Orienting +\nToss", "Orienting +\nNod")) + 
  scale_y_continuous(limits = c(-0,100)) + 
  ylab("Degree of feeling acknowledged (%)") + xlab("") + theme(legend.position="none")
dev.off()

ggplot(gg2, aes(Motion, value)) + theme_bw() + 
  geom_bar(fill="white", colour="black",stat="identity") +
  geom_jitter(data = gg, aes(Motion, value), width=0.2, color="black", alpha=0.2, size=0.8) +
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.2, position = position_dodge(0.2), color="black") +
  scale_x_discrete(limits=c("Rotation", "Waggle", "Toss", "Nod"), labels=c("Orienting", "Orienting +\nWaggle", "Orienting +\nToss", "Orienting +\nNod")) + 
  scale_y_continuous(limits = c(-0,100)) + 
  ylab("Degree of feeling acknowledged (%)") + xlab("") + theme(legend.position="none")


below <- subset(gg, value <= 50)




lmeTableEncounter <- lme(Degree ~ Encounter, random = ~1 | PID/Encounter, data=CleanedData)
anova(lmeTableEncounter)
summary(glht(lmeTableEncounter, linfct=mcp(Encounter="Tukey")))


library(sqldf)
AgeDF = sqldf("select pid, age, avg(degree) from CleanedData group by pid")
AgeDF.lm = lm(AgeDF$`avg(degree)` ~ AgeDF$Age)
summary(AgeDF.lm)

plot(AgeDF$`avg(degree` ~ AgeDF$Age, col='blue', cex=2, main="Average Degree per Age", xlab="Age",ylab="Average Degree")
plot(AgeDF.lm)


# First Encounter: Data Select and Motion Anova---------------------------

library(sqldf)
FEDF = sqldf('select * from CleanedData where ("order" = "0123" and "motion" = "Rotation") or ("order" = "1230" and "motion" = "Nod") or ("order" = "2301" and "motion" = "Toss") or ("order" = "3012" and "motion" = "Waggle")')
FEAOV = aov(FEDF$Degree ~ FEDF$Motion + Error(FEDF$PID/FEDF$Motion))
summary(FEAOV)

SEDF = sqldf('select * from CleanedData where ("order" = "0123" and "motion" = "Nod") or ("order" = "1230" and "motion" = "Toss") or ("order" = "2301" and "motion" = "Waggle") or ("order" = "3012" and "motion" = "Rotation")')
t.test(FEDF$Degree, SEDF$Degree)

RestDF = sqldf('select * from CleanedData where ("order" = "0123" and "motion" <> "Rotation") or ("order" = "1230" and "motion" <> "Nod") or ("order" = "2301" and "motion" <> "Toss") or ("order" = "3012" and "motion" <> "Waggle")')
RestDF.avr = sqldf('select PID, avg(Degree) from RestDF group by PID')
t.test(FEDF$Degree, RestDF.avr$`avg(Degree)`)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
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


