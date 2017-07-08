library('MASS')
library('ggplot2')
library('visreg')
library('tidyr')
library('dplyr')
library('plotly')
library('knitr')
library('DT')
#Project : How many goals would they score in the MLS

fname = file.choose()
LaLigaStats = read.csv(fname, header = T)

fname1 = file.choose()
Messi = read.csv(fname1, header = T)

fname2 = file.choose()
MLS = read.csv(fname2, header = T)

fname3 = file.choose()
BPL = read.csv(fname3, header = T)

fname4 = file.choose()
LIGA = read.csv(fname4, header = T)

fname5 = file.choose()
BULI = read.csv(fname5, header = T)

fname6 = file.choose()
LIG1 = read.csv(fname6, header = T)

fname7 = file.choose()
SERA = read.csv(fname6, header = T)

Messi.Liga =  Messi[Messi$COMP == 'La Liga',]


Messi.Liga["Defense"] = ""
Messi.Liga$RES = NULL
Messi.Liga$APPEAR = NULL

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub(" ", "", x)

LaLigaStats$Name = tolower(LaLigaStats$Name)
Messi.Liga$OPPO = tolower(Messi.Liga$OPPO)

LaLigaStats$Name = trim(LaLigaStats$Name)
Messi.Liga$OPPO = trim(Messi.Liga$OPPO)


for (i in 1:length(Messi.Liga$OPPO)){
  
  Messi.Liga$Defense[i] = LaLigaStats$DEF[LaLigaStats$Name == Messi.Liga$OPPO[i]]
  print(i)
}

Messi.Liga$Defense <- as.integer(Messi.Liga$Defense)

#Average Defensive Rating For Each Leauge
mean(MLS$DEF)
mean(BPL$DEF)
mean(LIGA$DEF)
mean(BULI$DEF)
mean(LIG1$DEF)
mean(SERA$DEF)

#build model
plot(Messi.Liga$Goals ~ Messi.Liga$Defense)
hist(Messi.Liga$Goals)

mod1 = glm.nb(Goals~Defense, data = Messi.Liga)
visreg(mod1)
summary(mod1)

#testing assumptions
modtest = glm(Goals~Defense, family = "poisson", data = Messi.Liga)

Assump = 2 * (logLik(mod1) - logLik(modtest))

#MLS Calculation
mls.df =  data.frame(MLS$DEF)
mls.df["Defense"] = data.frame(MLS$DEF)

MLSpredictions = data.frame(predict(mod1, mls.df, type = "response", se.fit = TRUE))

sum(MLSpredictions$fit)*2

# 95% CI
z = 1.96
MLSpredictions["upr"] = MLSpredictions$fit + (z * MLSpredictions$se.fit)
MLSpredictions["lwr"] = MLSpredictions$fit - (z * MLSpredictions$se.fit)

MLSSeason = NULL
MLSSeason = MLSpredictions[c(sample(nrow(MLSpredictions), 19), sample(nrow(MLSpredictions), 19)), ]
MLSSeason$residual.scale = NULL
MLSSeason$se.fit = NULL
MLSSeason["Running fit"] = as.integer("")
MLSSeason["Running lwr"] = as.integer("")
MLSSeason["Running upr"] = as.integer("")


MLSSeason$`Running fit`[1] = MLSSeason$fit[1]
MLSSeason$`Running lwr`[1] = MLSSeason$lwr[1]
MLSSeason$`Running upr`[1] = MLSSeason$upr[1]

for (i in 2:length(MLSSeason$fit)){
  
  MLSSeason$`Running fit`[i] = MLSSeason$`Running fit`[i-1] + MLSSeason$fit[i]
  MLSSeason$`Running lwr`[i] = MLSSeason$`Running lwr`[i-1] + MLSSeason$lwr[i]
  MLSSeason$`Running upr`[i] = MLSSeason$`Running upr`[i-1] + MLSSeason$upr[i]
}

ggplot(MLSSeason, aes(x = 1:38, y = `Running fit`)) + 
  geom_line(aes(fill = "black")) +
  geom_ribbon(aes(ymin = `Running lwr`, ymax = `Running upr`, fill = "red"),alpha = 0.25)


#BPL Calculations
bpl.df=  data.frame(BPL$DEF)
bpl.df["Defense"] = data.frame(BPL$DEF)


BPLpredictions = data.frame(predict(mod1, bpl.df, type = "response", se.fit = TRUE))
sum(BPLpredictions)*2

# 95% CI
z = 1.96
BPLpredictions["upr"] = BPLpredictions$fit + (z * BPLpredictions$se.fit)
BPLpredictions["lwr"] = BPLpredictions$fit - (z * BPLpredictions$se.fit)

BPLSeason = 0
BPLSeason = BPLpredictions[c(sample(nrow(BPLpredictions), 19), sample(nrow(BPLpredictions), 19)), ]
BPLSeason$residual.scale = NULL
BPLSeason$se.fit = NULL
BPLSeason["Running fit"] = as.integer("")
BPLSeason["Running lwr"] = as.integer("")
BPLSeason["Running upr"] = as.integer("")

#BPLSeason$`Running fit` = as.integer(BPLSeason$`Running fit`)

BPLSeason$`Running fit`[1] = BPLSeason$fit[1]
BPLSeason$`Running lwr`[1] = BPLSeason$lwr[1]
BPLSeason$`Running upr`[1] = BPLSeason$upr[1]

for (i in 2:length(BPLSeason$fit)){
  
  BPLSeason$`Running fit`[i] = BPLSeason$`Running fit`[i-1] + BPLSeason$fit[i]
  BPLSeason$`Running lwr`[i] = BPLSeason$`Running lwr`[i-1] + BPLSeason$lwr[i]
  BPLSeason$`Running upr`[i] = BPLSeason$`Running upr`[i-1] + BPLSeason$upr[i]
}


ggplot(BPLSeason, aes(x = 1:length(BPLSeason$fit), y = `Running fit`)) + 
  geom_line(aes(fill = "black")) +
  geom_ribbon(aes(ymin = `Running lwr`, ymax = `Running upr`, fill = "red"),alpha = 0.25)

#Liga Calculations
liga.df=  data.frame(LIGA$DEF)
liga.df["Defense"] = data.frame(LIGA$DEF)

LIGApredictions = data.frame(predict(mod1, liga.df, type = "response", se.fit = TRUE))
sum(LIGApredictions)*2

# 95% CI
z = 1.96
LIGApredictions["upr"] = LIGApredictions$fit + (z * LIGApredictions$se.fit)
LIGApredictions["lwr"] = LIGApredictions$fit - (z * LIGApredictions$se.fit)

LIGASeason = 0
LIGASeason = LIGApredictions[c(sample(nrow(LIGApredictions), 19), sample(nrow(LIGApredictions), 19)), ]
LIGASeason$residual.scale = NULL
LIGASeason$se.fit = NULL
LIGASeason["Running fit"] = as.integer("")
LIGASeason["Running lwr"] = as.integer("")
LIGASeason["Running upr"] = as.integer("")


LIGASeason$`Running fit`[1] = LIGASeason$fit[1]
LIGASeason$`Running lwr`[1] = LIGASeason$lwr[1]
LIGASeason$`Running upr`[1] = LIGASeason$upr[1]

for (i in 2:length(LIGASeason$fit)){
  
  LIGASeason$`Running fit`[i] = LIGASeason$`Running fit`[i-1] + LIGASeason$fit[i]
  LIGASeason$`Running lwr`[i] = LIGASeason$`Running lwr`[i-1] + LIGASeason$lwr[i]
  LIGASeason$`Running upr`[i] = LIGASeason$`Running upr`[i-1] + LIGASeason$upr[i]
}

ggplot(LIGASeason, aes(x = 1:length(LIGASeason$fit), y = `Running fit`)) + 
  geom_line(aes(fill = "black")) +
  geom_ribbon(aes(ymin = `Running lwr`, ymax = `Running upr`, fill = "red"),alpha = 0.25)


#Bundesliga Calculations
buli.df=  data.frame(BULI$DEF)
buli.df["Defense"] = data.frame(BULI$DEF)
 
BULIpredictions = data.frame(predict(mod1, buli.df, type = "response", se.fit = TRUE))
sum(BULIpredictions)*2

# 95% CI
z = 1.96
BULIpredictions["upr"] = BULIpredictions$fit + (z * BULIpredictions$se.fit)
BULIpredictions["lwr"] = BULIpredictions$fit - (z * BULIpredictions$se.fit)

BULISeason = 0
BULISeason = BULIpredictions[c(sample(nrow(BULIpredictions), 17), sample(nrow(BULIpredictions), 17)), ]
BULISeason$residual.scale = NULL
BULISeason$se.fit = NULL
BULISeason["Running fit"] = as.integer("")
BULISeason["Running lwr"] = as.integer("")
BULISeason["Running upr"] = as.integer("")


BULISeason$`Running fit`[1] = BULISeason$fit[1]
BULISeason$`Running lwr`[1] = BULISeason$lwr[1]
BULISeason$`Running upr`[1] = BULISeason$upr[1]

for (i in 2:length(BULISeason$fit)){
  
  BULISeason$`Running fit`[i] = BULISeason$`Running fit`[i-1] + BULISeason$fit[i]
  BULISeason$`Running lwr`[i] = BULISeason$`Running lwr`[i-1] + BULISeason$lwr[i]
  BULISeason$`Running upr`[i] = BULISeason$`Running upr`[i-1] + BULISeason$upr[i]
}

ggplot(BULISeason, aes(x = 1:length(BULISeason$`Running fit`), y = `Running fit`)) + 
  geom_line(aes(fill = "black")) +
  geom_ribbon(aes(ymin = `Running lwr`, ymax = `Running upr`, fill = "red"),alpha = 0.25)


#Ligue 1Calculations
lig1.df=  data.frame(LIG1$DEF)
lig1.df["Defense"] = data.frame(LIG1$DEF)


LIG1predictions = data.frame(predict(mod1, lig1.df, type = "response", se.fit = TRUE))
sum(LIG1predictions)*2

# 95% CI
z = 1.96
LIG1predictions["upr"] = LIG1predictions$fit + (z * LIG1predictions$se.fit)
LIG1predictions["lwr"] = LIG1predictions$fit - (z * LIG1predictions$se.fit)

LIG1Season = 0
LIG1Season = LIG1predictions[c(sample(nrow(LIG1predictions), 19), sample(nrow(LIG1predictions), 19)), ]
LIG1Season$residual.scale = NULL
LIG1Season$se.fit = NULL
LIG1Season["Running fit"] = as.integer("")
LIG1Season["Running lwr"] = as.integer("")
LIG1Season["Running upr"] = as.integer("")


LIG1Season$`Running fit`[1] = LIG1Season$fit[1]
LIG1Season$`Running lwr`[1] = LIG1Season$lwr[1]
LIG1Season$`Running upr`[1] = LIG1Season$upr[1]

for (i in 2:length(LIG1Season$fit)){
  
  LIG1Season$`Running fit`[i] = LIG1Season$`Running fit`[i-1] + LIG1Season$fit[i]
  LIG1Season$`Running lwr`[i] = LIG1Season$`Running lwr`[i-1] + LIG1Season$lwr[i]
  LIG1Season$`Running upr`[i] = LIG1Season$`Running upr`[i-1] + LIG1Season$upr[i]
}

ggplot(LIG1Season, aes(x = 1:length(LIG1Season$`Running fit`), y = `Running fit`)) + 
  geom_line(aes(fill = "black")) +
  geom_ribbon(aes(ymin = `Running lwr`, ymax = `Running upr`, fill = "red"),alpha = 0.25)

#Serie A Calculations
SERA.df=  data.frame(SERA$DEF)
SERA.df["Defense"] = data.frame(SERA$DEF)


SERApredictions = data.frame(predict(mod1, SERA.df, type = "response", se.fit = TRUE))
sum(SERApredictions)*2

# 95% CI
z = 1.96
SERApredictions["upr"] = SERApredictions$fit + (z * SERApredictions$se.fit)
SERApredictions["lwr"] = SERApredictions$fit - (z * SERApredictions$se.fit)

SERASeason = 0
SERASeason = SERApredictions[c(sample(nrow(SERApredictions), 19), sample(nrow(SERApredictions), 19)), ]
SERASeason$residual.scale = NULL
SERASeason$se.fit = NULL
SERASeason["Running fit"] = as.integer("")
SERASeason["Running lwr"] = as.integer("")
SERASeason["Running upr"] = as.integer("")


SERASeason$`Running fit`[1] = SERASeason$fit[1]
SERASeason$`Running lwr`[1] = SERASeason$lwr[1]
SERASeason$`Running upr`[1] = SERASeason$upr[1]

for (i in 2:length(SERASeason$fit)){
  
  SERASeason$`Running fit`[i] = SERASeason$`Running fit`[i-1] + SERASeason$fit[i]
  SERASeason$`Running lwr`[i] = SERASeason$`Running lwr`[i-1] + SERASeason$lwr[i]
  SERASeason$`Running upr`[i] = SERASeason$`Running upr`[i-1] + SERASeason$upr[i]
}

ggplot(SERASeason, aes(x = 1:length(SERASeason$`Running fit`), y = `Running fit`)) + 
  geom_line(aes(fill = "black")) +
  geom_ribbon(aes(ymin = `Running lwr`, ymax = `Running upr`, fill = "red"),alpha = 0.25)

#Build DF and Plot Trend Lines

df.combined = data.frame(MLSSeason$`Running upr`)
df.combined["MLSupr"] = MLSSeason$`Running upr`
df.combined$MLSSeason..Running.upr. = NULL
df.combined["MLSpred"]= MLSSeason$`Running fit`
df.combined["MLSlwr"] = MLSSeason$`Running lwr`

df.combined["BPLupr"] = BPLSeason$`Running upr`
df.combined["BPLpred"] = BPLSeason$`Running fit`
df.combined["BPLlwr"] = BPLSeason$`Running lwr`

df.combined["LGAupr"] = LIGASeason$`Running upr`
df.combined["LGApred"] = LIGASeason$`Running fit`
df.combined["LGAlwr"] = LIGASeason$`Running lwr`

newrow2 = NULL
newrow2 = data.frame(matrix(c(rep.int(NA, 6)), nrow = 4, ncol = 6))
colnames(newrow2) = colnames(BULISeason)
BULISeason = rbind(BULISeason, newrow2)


df.combined["BLIupr"] = BULISeason$`Running upr`
df.combined["BLIpred"] = BULISeason$`Running fit`
df.combined["BLIlwr"] = BULISeason$`Running lwr`

df.combined["LG1upr"] = LIG1Season$`Running upr`
df.combined["LG1pred"] = LIG1Season$`Running fit`
df.combined["LG1lwr"] = LIG1Season$`Running lwr`

df.combined["SRAupr"] = SERASeason$`Running upr`
df.combined["SRApred"] = SERASeason$`Running fit`
df.combined["SRAlwr"] = SERASeason$`Running lwr`

#Plotting
tidy.data  <- df.combined %>% 
  # add id variable
  mutate(x = 1:38) %>% 
  # reshape to long format
  gather("variable", "value", 1:18) %>% 
  # separate variable names at position 3
  separate(variable, 
           into = c("model", "line"), 
           sep = 3, 
           remove = TRUE)

#plot wire

# plot
ggplot(data = tidy.data, aes(x        = x, 
                             y        = value, 
                             linetype = line, 
                             color    = model)) + 
  geom_line() + 
  scale_linetype_manual(values = c("dashed", "solid", "dashed"))


#plot ribbon
# back to wide
wide.data <- tidy.data %>% 
  spread(line, value)

# plot with ribbon
ggplot(data = wide.data, aes(x = x, y = pred)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = model), alpha = .5) +
  geom_line(aes(group = model))


# interactive plot with ribbon
test <- ggplot(data = wide.data, aes(x = x, y = pred)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = model), alpha = 0.5) + 
  geom_point(aes(group = model, fill = model)) + 
  labs(x = "Number of Games Played", y = "Total Number of Goals")

# interactive plot with ribbon
test <- ggplot(data = wide.data, aes(x = x, y = pred)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = model), alpha = .5) + 
  geom_point(aes( group = model, fill = model)) + 
  labs(x = "Number of Games Played", y = "Total Number of Goals") + scale_y_continuous(breaks=seq(0,70,5)) +
  scale_x_continuous(breaks=seq(0,50,5))

plot <- ggplotly(test)

plotly_POST(plot, filename = 'r-docs/knitr-example')

#interactive plot testing

my.data = data.frame(x=1:40, y=1:40)
plot_ly(my.data, x=x, y=y, mode="markers", name="foo") %>%
  add_trace(type="area", x=c(x,rev(x)), y= c(y+10, rev(y)-10), 
            fill = "toself", name="bar") %>%
  add_trace(type="area", x=c(x,rev(x)), y= c(y-10, rev(y)-30), 
            fill = "toself", visible = "lengendonly",name="bar1")
