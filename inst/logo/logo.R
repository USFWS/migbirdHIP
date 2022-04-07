library(hexSticker)
library(usmap)
library(ggplot2)
library(dplyr)
library(png)
library(cowplot)

sysfonts::font_add_google("Lato", "pf", regular.wt = 700)

wodu <- readPNG("man/figures/wodu_silhouette_sm.png", native = TRUE)

usa49 <-
  plot_usmap(
    data = statepop,
    values = "pop_2015",
    fill = "#FFFFFF", color = "#0270bb", exclude = "Hawaii") +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    plot.margin = unit(c(0, 0, 0, 0), "cm"))

orange_wodu <-
  ggdraw(usa49) +
  draw_image(wodu, x = 0, y = .1, width = .95, height = .9)

# PNG sticker
MBHS_hex <- hexSticker::sticker(
  orange_wodu,
  package = "migbirdHIP", p_color = "#FFFFFF",
  p_size = 19, s_x = 1, s_y = .75, s_width = 1.2, s_height = .9,
  h_fill = "#faa225", h_color = "#ffd151",
  dpi = 320,
  filename = "man/figures/logo.png")
print(MBHS_hex)

# SVG sticker
MBHS_hex <- hexSticker::sticker(
  orange_wodu,
  package = "migbirdHIP", p_color = "#FFFFFF",
  p_size = 6, s_x = 1, s_y = .75, s_width = 1.2, s_height = .9,
  h_fill = "#faa225", h_color = "#ffd151",
  dpi = 320,
  filename = "man/figures/logo.svg")
print(MBHS_hex)
