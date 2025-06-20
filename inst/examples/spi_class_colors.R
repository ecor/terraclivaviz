# Define RGB components
red   <- rev(c(192, 255, 237, 219, 180, 143, 47))
green <- rev(c(0, 0, 125, 219, 199, 170, 85))
blue  <- rev(c(0, 0, 49, 219, 231, 220, 151))

# Create color palette
col <- rgb(red = red, green = green, blue = blue, alpha = 255, maxColorValue = 255)

# Plot color swatches
barplot(rep(1, length(col)), col = col, border = NA, space = 0, axes = FALSE)
print(col)