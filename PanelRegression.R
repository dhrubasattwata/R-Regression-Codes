# ---
# title: "Panel Regression"
# author: "Dhrubasattwata Roy Choudhury"
# ---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Panel Regression

### Dataset
mydata <- read.csv("C:/Users/Dhruba/Documents/R/data/internetplm.csv")
head(mydata)

### Setting up the Fixed Effects Model
library(plm)
plmwithin <- plm(usage~income+videohours+webpages+gender+age, data = mydata, model = "within")
summary(plmwithin)

### Setting up the Random Effects Model
plmrandom <- plm(usage~income+videohours+webpages+gender+age, data = mydata, model = "random")
summary(plmrandom)

### Fixed Effects regression: Further Analysis

Analysing with fixef to isolate the effects of time on internet usage.
#Effect of time on internet usage
fixef(plmwithin,type="dmean")

summary(fixef(plmwithin, type = "dmean"))

df <- mydata[mydata$week== 10,]
df <- df[,c("week", "usage")]
df

df <- mydata[mydata$week== 33,]
df <- df[,c("week", "usage")]
df

twoway <-  plm(usage~income+videohours+webpages+gender+age+company,data=mydata,model="within",effect="time")
fixef(twoway,effect="time")

a <- subset(mydata, company=="a")
b <- subset(mydata, company=="b")
c <- subset(mydata, company=="c")

mean(a$usage)
mean(b$usage)
mean(c$usage)

# Visualization for better understanding

plot(a$usage,type='l')
lines(a$usage, col="darkgreen")

plot(b$usage,type='l')
lines(b$usage, col="darkgreen")

plot(c$usage,type='l')
lines(c$usage, col="darkgreen")








