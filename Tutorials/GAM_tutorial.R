mcycle <- MASS::mcycle
head(mcycle)
plot(mcycle)
lm_mod<-lm(times~accel, data=mcycle)
termplot(lm_mod, partial.resid = TRUE, se = TRUE)
install.packages("mgcv")
library(mgcv)
gam_mod <- gam(accel~ s(times), data = mcycle)
plot(gam_mod, residuals = TRUE, pch = 1)
coef(gam_mod)

# Fit a GAM with 3 basis functions
gam_mod_k3 <- gam(accel ~ s(times, k = 3), data = mcycle)
gam_mod_k20<-gam(accel~s(times, k=20), data=mcycle)

par(mfrow = c(1, 2))
plot(gam_mod_k3, residuals = TRUE, pch = 1)
plot(gam_mod_k20, residuals = TRUE, pch = 1)

gam_mod <- gam(accel ~ s(times), data = mcycle, method = "REML")
gam_mod$sp

gam_mod_s1 <- gam(accel ~ s(times), data = mcycle, sp = 0.1)
gam_mod_s2 <- gam(accel ~ s(times), data = mcycle, sp = 0.0001)

plot(gam_mod_s1, residuals = TRUE, pch = 1)
plot(gam_mod_s2, residuals = TRUE, pch = 1)

gam_mod_sk<-gam(accel~s(times, k=50), data=mcycle, sp=0.0001)
plot(gam_mod_sk, residuals=TRUE, pch=1)


##multiple regression gams
library(gamair)
data("mpg", package="gamair")

head(mpg)

mod_city<-gam(city.mpg~s(weight)+s(length)+s(price), data=mpg, method="REML")
plot(mod_city, pages=3)

mod_city2 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + fuel+ drive+ style, 
                 data = mpg, method = "REML")
plot(mod_city2, all.terms = TRUE, pages = 6)


mod_city3 <- gam(city.mpg ~ s(weight, by = drive) + s(length, by = drive) + s(price, by = drive) + drive,
                 data = mpg, method = "REML")
plot(mod_city3, pages = 6)

##Part 2#############################################
library(gamair)
data("mpg", package="gamair")

library(mgcv)

mod_city4 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + s(rpm) + s(width),
                 data = mpg, method = "REML")
summary(mod_city4)

mod <- gam(accel ~ s(times), data = mcycle, method = "REML")
plot(mod, residuals=TRUE)
plot(mod, residuals=TRUE, pch=1, cex=1)
mod <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio, 
           data = mpg, method = "REML")
plot(mod, select=3)
plot(mod, pages = 5, all.terms = TRUE)


mod <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio, 
           data = mpg, method = "REML")
plot(mod, select=1, shade=TRUE, shade.col="hotpink", pages=3)

# Make another plot adding the intercept value and uncertainty
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink", 
     shift = coef(mod)[1], seWithMean = TRUE, pages=3)

set.seed(0)
dat <- gamSim(1,n=200)

mod <- gam(y ~ s(x0, k = 5) + s(x1, k = 5) + s(x2, k = 5) + s(x3, k = 5),
           data = dat, method = "REML")
gam.check(mod)

set.seed(0)
dat <- mgcv::gamSim(1,n=600, scale=0.6, verbose=FALSE)

mod <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + s(x2, k = 3) + s(x3, k = 3),
           data = dat, method = "REML")
gam.check(mod)
mod2 <- gam(y ~ s(x0, k =3) + s(x1, k = 3) + s(x2, k = 10) + s(x3, k =3),
            data = dat, method = "REML")
gam.check(mod2)

set.seed(0)
data("mpg", package="gamair", verbose=FALSE)
mod <- gam(hw.mpg ~ s(length) + s(width) + s(height) + s(weight),
           data = mpg, method = "REML")
concurvity(mod, full = TRUE)

set.seed(0)
data("mpg", package="gamair", verbose=FALSE)

mod <- gam(hw.mpg ~ s(length) + s(width) + s(height) + s(weight),
           data = mpg, method = "REML")

concurvity(mod, full=FALSE)
