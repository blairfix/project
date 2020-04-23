library(data.table)
library(ggplot2)
library(magrittr)
library(Rcpp)
library(RcppArmadillo)
library(gridExtra)

sourceCpp("project.cpp")

# 3d pyramid data
pyramid = fread("pyramids.csv")

# camera parameters
e = c(-4, -4, -4)
c1 = c(-300, -300, 200)
theta = c(pi/2, 0, -pi/2.2)

# order points by distance from camera
r = ( (pyramid$x - c1[1] )^2 + (pyramid$y - c1[2])^2 )^0.5
pyramid = pyramid[order(-r),]

# 2D projection of top of pyramid
pyramid = as.matrix(pyramid)
top = project(pyramid, c1, theta, e)  %>% data.table()
colnames(top) = c("x", "y")
z = pyramid[,3]

# 2D projection of bottom of pyramid
pyramid[,3] = 0
bottom = project(pyramid, c1, theta, e)  %>% data.table()
colnames(bottom) = c("x", "y")

# bind plot data
plot_data = data.table(x_bottom = bottom$x,
                       y_bottom = bottom$y,
                       x_top = top$x,
                       y_top = top$y,
                       z = z)


# plot in ggplot
pyramid_plot = ggplot() +
  geom_segment(data = plot_data, 
               aes(x = x_bottom, 
                   y = y_bottom, 
                   xend = x_top, 
                   yend = y_top, 
                   col = z), 
               size = 0.15
               ) +
  scale_color_gradientn(colours = rainbow(10)) +
  theme_void() +
  theme(legend.position = "none")


gA = ggplotGrob(pyramid_plot)

png("pyramids.png", width = 8, height = 3.8, units = 'in', res = 600)
grid.arrange(gA)
dev.off()

