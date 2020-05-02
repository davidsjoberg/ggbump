## Packages
library(ggbump)
library(ggplot2)
library(dplyr)
library(Cairo)

## Data
df <- data.frame(time = c(1, 2, 3), val = c(1, 2, 1))

## Colours
customblue <- "#4488bb"
white <- "#ffffff"

## Bump chart
CairoPNG(filename = "man/figures/bump.png", width = 3, height = 2, units = "in", dpi = 300)

bump <- ggplot(df, aes(x = time, y = val)) +
            geom_bump(smooth = 6, colour = customblue, size = 1.5) +
            geom_point(size = 6, shape = 21, fill = customblue, colour = white, stroke = 1) +
            coord_cartesian(xlim = c(1, 3), ylim = c(1, 2)) +
            theme_void()

bump

dev.off()



## More Packages
library(hexSticker)
library(magick)
library(bunny)

bump <- image_read("man/figures/bump.png")

## Full size Hex Sticker
sticker(bump, package = "ggbump", p_size = 22, p_color = customblue, p_y = 1.45,
        h_fill = "#ffffff", h_color = customblue,
        s_x = 1, s_y = 0.85, s_width = 1.25, s_height = 0.85,
        filename = "man/figures/sticker.png")

## Smaller README logo
image_read("man/figures/sticker.png") %>%
  image_resize("171x198") %>%
  image_write("man/figures/logo.png", density = 600)




## GH Card

img_hex <- image_read("man/figures/sticker.png") %>%
                image_scale("400x400")

gh_logo <- bunny::github %>%
                image_scale("50x50")

gh <- image_canvas_ghcard("white") %>%
          image_compose(img_hex, gravity = "East", offset = "+120+0") %>%
          image_annotate("Bump Chart and Sigmoid Curves", gravity = "West", location = "+120-30",
                         color = customblue, size = 32, weight = 700) %>%
          image_compose(gh_logo, gravity="West", offset = "+120+40") %>%
          image_annotate("davidsjoberg/ggbump", gravity="West", location="+180+42",
                         size=28)%>%
          image_resize("1280x640")

gh

gh %>%
  image_write("man/figures/ggbump_ghcard.png")
