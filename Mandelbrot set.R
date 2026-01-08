x <- y <- seq(0.34, 0.38, length.out = 1200)
c <- outer(x, y, function(a, b) complex(real = a, imaginary = b))
z <- c
n <- matrix(0, length(x), length(y))

for (i in 1:60) {
  z <- z^2 + c
  n[n == 0 & abs(z) > 2] <- i
}

image(x, y, n, col = hcl.colors(50, "Inferno"), useRaster = TRUE)
# Note that the black areas are actually part of the Mandelbrot set
# The boundary of the Mandelbrot set is a fractal curve
# No matter how much you zoom in, the edge stays "rough" 