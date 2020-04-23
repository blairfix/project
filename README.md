# project

`project` is an R function that projects 3D data onto a 2D plane. It is useful for creating 3D plots using 2D plotting packages like  [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html). 

### Inputs

The `project` function uses the formula for [perspective projection](https://en.wikipedia.org/wiki/3D_projection#Perspective_projection), which takes the following inputs:


* `a` = a matrix of the (x, y, z) coordinates that you wish to plot (Note: `a` must be a matrix. `project` will not accept a dataframe)

* `c` = a vector of (x, y, z) coordinates representing the position of the camera

* `theta` =  a vector of [Tait–Bryan angles](https://en.wikipedia.org/wiki/Euler_angles#Tait%E2%80%93Bryan_angles) (in radians) representing the orientation of the camera 

* `e` = a vector of (x, y, z) coordinates representing the display surface's position relative to the camera


### Output
`project` returns a two-column matrix containing (x, y) values for each point to be visualized on a 2D plane. This data can then be inputted into any 2D plotting software to create a 3D image.



### Example 1:

```R

library(Rcpp)
library(RcppArmadillo)
library(ggplot2)

sourceCpp("project.cpp")

# make plot data
r = seq(0, 5, length.out = 10^4)
theta = seq(0, 500, length.out = 10^4)

x = r*sin(theta)*cos(theta)
y = r*cos(theta)
z = r^0.4

plot_data = cbind(x, y, z)

# camera parameters
c = c(-400, -400, 50)
theta = c(pi/2, 0, -pi/2.2)
e = c(-4, -4, -4)

# project 3d data onto 2d plane
projection =  project(plot_data, c, theta, e)  

# plot using ggplot
projection = data.frame(projection)
names(projection) = c("x", "y")

plot_spiral = ggplot(data = projection, aes(x = x, y = y, col = r)) +
    scale_color_gradientn(colours = rainbow(4)) +
    geom_path(alpha = 0.8) +
    theme_void() +
    theme(legend.position = "none")

```

![Example Projection 1](https://economicsfromthetopdown.files.wordpress.com/2020/04/polar_plot.png)


### Example 2:

The code for this example is lengthier and contained in the file `plot_pyramid.R`. The output is a landscape of pyramids that nicely shows of the true perspective generated by `project`.

![Example Projection 2](https://economicsfromthetopdown.files.wordpress.com/2020/04/pyramids.png)

### Why use `project`?

There are two reasons to use `project` rather than plot your data using a 3D plotting package. First, many 3D plotters use [weak perspective projection](https://en.wikipedia.org/wiki/3D_projection#Weak_perspective_projection). This is okay if you're plotting something that should look close to the viewer (i.e. something small). But to accurately visualize a large object, you want true perspective.

The other reason to use `project` is that it allows you to use 2D plotting software like ggplot2. This gives you more options (like color, alpha layers, etc.) than are available in 3D plotting software. 


### Installation
To use `project`, install the following R packages:
 * [Rcpp](https://cran.r-project.org/web/packages/Rcpp/index.html) 
 * [RcppArmadillo](https://cran.r-project.org/web/packages/RcppArmadillo/index.html) 

Put the source code (`project.cpp`) in the directory of your R script. Then source it with the command `sourceCpp(project.cpp)`.


