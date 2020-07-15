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
