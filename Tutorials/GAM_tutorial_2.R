#####Chapter 3###############################
library(mgcv)
data(meuse, package="sp")
head(meuse)

mod2d<-gam(cadmium~s(x,y), data=meuse, method="REML")
summary(mod2d)
coef(mod2d)

mod2da<-gam(cadmium~s(x,y)+s(elev)+s(dist), data=meuse, method="REML")
summary(mod2da)
coef(mod2da)

plot(mod2da, scheme=2, pages=3)

vis.gam(mod2d, view=c("x","y"), plot.type="persp", se=2, theta=100)

vis.gam(mod2d, view = c("x", "y"), 
        plot.type = "contour", too.far = 0.25)
points(meuse)


mod_sep <- gam(copper ~ s(dist, by = landuse) + landuse,
               data = meuse, method = "REML")
summary(mod_sep)
mod_fs <- gam(copper ~ s(dist, landuse, bs = "fs"),
              data = meuse, method = "REML")
summary(mod_fs)

plot(mod_sep, pages = 2)
plot(mod_fs, pages = 1)

vis.gam(mod_sep, view = c("dist", "landuse"), plot.type = "persp")
vis.gam(mod_fs, view = c("dist", "landuse"), plot.type = "persp")

tensor_mod <- gam(cadmium ~ te(x, y, elev), 
                  data = meuse, method = "REML")
summary(tensor_mod)
plot(tensor_mod)

tensor_mod2 <- gam(cadmium ~ s(x, y) + s(elev) + ti(x, y, elev), 
                   data = meuse, method = "REML")

summary(tensor_mod2)
plot(tensor_mod2, pages = 1)