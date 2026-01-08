# Animated Mandelbrot + Julia (creates a GIF)
#
# Output: "julia_animation.gif" in your working directory
#
# Dependencies: gifski (or magick), png
# Install if needed: install.packages(c("gifski","png"))
#
# This script keeps the Mandelbrot set static (left) and animates the Julia
# set (right) by rotating the complex parameter k along a small circle.

# --- Parameters (tweak these to change resolution / length / speed) ---
res <- 500            # grid size (res x res). Lower for faster rendering.
maxM <- 50            # max iterations for Mandelbrot
maxJ <- 60            # max iterations for Julia
frames <- 60          # number of frames in the animation
fps <- 20             # frames per second in the resulting GIF
outfile <- "julia_animation.gif"
palette <- hcl.colors(50, "Inferno")  # color palette

# Radius / center for Julia parameter motion (centered on your original k)
k_center <- complex(real = -0.8, imaginary = 0.156)
k_radius <- 0.35      # how far k moves from k_center (tweak for different effect)

# --- Grid / complex plane ---
x <- y <- seq(-2, 2, length.out = res)
c_grid <- outer(x, y, function(a, b) complex(real = a, imaginary = b))

# --- Compute Mandelbrot once (left panel) ---
zM <- c_grid
nM <- matrix(0L, res, res)
for (i in 1:maxM) {
  zM <- zM^2 + c_grid
  escaped <- (nM == 0L) & (Mod(zM) > 2)
  nM[escaped] <- i
}
# Replace zeros (points that didn't escape) with max iteration for nicer display
nM[nM == 0L] <- maxM

# --- Helper: compute Julia iteration counts for a given k ---
compute_julia <- function(k, c_grid, maxiter) {
  z <- c_grid
  n <- matrix(0L, nrow(z), ncol(z))
  escaped <- matrix(FALSE, nrow(z), ncol(z))
  for (i in 1:maxiter) {
    z <- z^2 + k
    newly_escaped <- (!escaped) & (Mod(z) > 2)
    if (!any(newly_escaped)) {
      # No new escapes this iteration
      next
    }
    n[newly_escaped] <- i
    escaped <- escaped | newly_escaped
    if (all(escaped)) break
  }
  n[n == 0L] <- maxiter
  n
}

# --- Make a temporary directory and render frames as PNGs ---
tmpdir <- file.path(tempdir(), "julia_frames")
if (!dir.exists(tmpdir)) dir.create(tmpdir, recursive = TRUE)
png_files <- character(frames)

# image size: two panels side-by-side; each panel ~square:
panel_px <- 600   # pixels per panel (tweak if you want larger GIF)
img_width <- panel_px * 2
img_height <- panel_px

for (f in seq_len(frames)) {
  theta <- 2 * pi * (f - 1) / frames
  k <- k_center + k_radius * complex(real = cos(theta), imaginary = sin(theta))
  nJ <- compute_julia(k, c_grid, maxJ)
  fname <- file.path(tmpdir, sprintf("frame_%03d.png", f))
  png(filename = fname, width = img_width, height = img_height, res = 100)
  par(mfrow = c(1, 2), mar = c(1, 1, 2, 1))
  # Mandelbrot (left)
  image(x, y, nM, col = palette, main = "Mandelbrot", useRaster = TRUE, axes = FALSE)
  # Julia (right)
  title_j <- paste0("Julia, k = ", format(k, digits = 4))
  image(x, y, nJ, col = palette, main = title_j, useRaster = TRUE, axes = FALSE)
  dev.off()
  png_files[f] <- fname
  message(sprintf("Rendered frame %d/%d  (k = %s)", f, frames, format(k, digits = 4)))
}

# --- Stitch PNGs into a GIF ---
if (requireNamespace("gifski", quietly = TRUE)) {
  gifski::gifski(png_files, gif_file = outfile, delay = 1 / fps)
  message("Wrote GIF to: ", normalizePath(outfile))
} else if (requireNamespace("magick", quietly = TRUE)) {
  imgs <- magick::image_read(png_files)
  anim <- magick::image_animate(imgs, fps = fps)
  magick::image_write(anim, path = outfile)
  message("Wrote GIF to: ", normalizePath(outfile))
} else {
  stop("Please install 'gifski' or 'magick' to create the GIF: install.packages('gifski')")
}

# --- Cleanup (optional) ---
# Uncomment to remove the temporary PNG frames after creating the GIF:
# unlink(tmpdir, recursive = TRUE)
