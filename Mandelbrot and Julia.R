# Grid
x <- y <- seq(-2, 2, length.out = 600)
c <- outer(x, y, function(a, b) complex(real = a, imaginary = b))

# ---- Mandelbrot ----
z <- c
nM <- matrix(0, length(x), length(y))

for (i in 1:50) {
  z <- z^2 + c
  nM[nM == 0 & abs(z) > 2] <- i
}

# ---- Julia ----
k <- complex(real = -0.8, imaginary = 0.156)  # Julia parameter
z <- c
nJ <- matrix(0, length(x), length(y))

for (i in 1:60) {
  z <- z^2 + k
  nJ[nJ == 0 & abs(z) > 2] <- i
}

# Plot
par(mfrow = c(1, 2), mar = c(1, 1, 2, 1))
image(x, y, nM, col = hcl.colors(50, "Inferno"),
      main = "Mandelbrot", useRaster = TRUE)
image(x, y, nJ, col = hcl.colors(50, "Inferno"),
      main = paste("Julia, c =", k), useRaster = TRUE)