length<-c(75, 57, 58, 58, 62,
67, 58, 61, 59, 66,
70, 60, 56, 58, 65,
75, 59, 58, 61, 63,
65, 62, 57, 57, 64,
71, 60, 56, 56, 62,
67, 60, 61, 58, 65,
67, 57, 60, 57, 65,
76, 59, 57, 57, 62,
68, 61, 58, 59, 67)

sug<-rep(1:5,10)

sugar<-factor(sug)

growth<-data.frame(length,sugar)
rm(length,sug,sugar)
growth.aov <- aov(length ~ sugar, data = growth)
summary(growth.aov)
model.tables(growth.aov)
model.tables(growth.aov, type = "means")
coef(growth.aov)

c1 <- c(4,-1,-1,-1,-1)
c2 <- c(0,-1,-1,-1,3)
c3 <- c(0,1,-1,0,0)
c4 <- c(0,-1,-1,2,0)
ctr <- matrix(c(c1,c2,c3,c4), nrow=5)
ctr
contrasts(growth$sugar) <- ctr
growth.aov <- aov(length ~ sugar, data = growth)
summary(growth.aov)
summary(growth.aov, split = list(sugar = list("control v sugars" = 1,
                                              "sucrose v gl,fr" = 2,
                                              "gl v fr" = 3, "gl,fr interaction" = 4)))



growth.lm<-lm(growth$length~growth$sugar)
summary(growth.lm)

library(MASS)
plot(fitted(growth.aov),stdres(growth.aov))
abline(h=0)
