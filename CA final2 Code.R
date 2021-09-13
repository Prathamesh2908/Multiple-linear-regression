lpg <- read.csv("C:/Users/prath/Desktop/STATS CA/LPG/LPGfinal1.csv")
head(lpg)
lpg1 = subset(lpg, select = -c(Country.or.Area) )



pairs(lpg1)
pairs(lpg1, panel = panel.smooth)



model1<-lm(final~household*construction*transport+I(household^2)+I(construction^2)+I(transport^2),data = lpg1)
summary(model1)



model1a<-update(model1,~.-household:construction:transport)
summary (model1a)

model1b<-update(model1a,~.-household:construction)
summary(model1b)

model1c<-update(model1b,~.-household:transport)
summary(model1c)


model1d<-update(model1c,~.-construction:transport)
summary(model1d)


model1e<-update(model1d,~.-I(transport^2))
summary (model1e)

model1f<-update(model1e,~.-I(household^2))
summary (model1f)

model1g<-update(model1f,~.-I(construction^2))
summary (model1g)


par(mfrow = c(2,2))
plot(model1g)

hist(residuals(model1g))
durbinWatsonTest(model1g)
ncvTest(model1g)
vif(model1g)

cooks.distance(model1g)
influencePlot(model=model1g, scale=3)


model2<-lm(sqrt(final)~household*construction*transport+I(household^2)+I(construction^2)+I(transport^2),data = lpg1)
model3<-step(model3)
summary(model3)


model3a<-update(model3,~.-household:construction)
summary(model3a)

model3b<-update(model3a,~.-I(transport^2))
summary(model3b)

par(mfrow = c(2,2))
plot(model3b)

hist(residuals(model3b))
durbinWatsonTest(model3b)
ncvTest(model3b)
vif(model3b)

cooks.distance(model3b)
influencePlot(model=model3b, scale=3)



