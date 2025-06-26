# Create hex sticker for migbirdHIP

sysfonts::font_add_google("Lato", "pf", regular.wt = 700)

wodu <- png::readPNG("man/figures/wodu_silhouette_sm.png", native = TRUE)

usa49 <-
  usmap::plot_usmap(
    regions = "state",
    data = statepop,
    values = "pop_2022",
    fill = "#FFFFFF",
    color = "#0270bb",
    exclude = "Hawaii"
  ) +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = NA, color = NA),
    plot.margin = ggplot2::unit(c(0, .2, 0, 0), "cm")
  )

map_wodu <-
  cowplot::ggdraw(usa49) +
  cowplot::draw_image(
    wodu,
    x = 0,
    y = .11,
    width = .95,
    height = .9
  )

# PNG sticker
hexSticker::sticker(
  map_wodu,
  package = "migbirdHIP",
  p_color = "#FFFFFF",
  p_size = 19,
  s_x = 1,
  s_y = .75,
  s_width = 1.4,
  s_height = .9,
  h_fill = "#faa225",
  h_color = "#ffd151",
  dpi = 320,
  filename = "man/figures/logo.png"
)

# SVG sticker
hexSticker::sticker(
  map_wodu,
  package = "migbirdHIP",
  p_color = "#FFFFFF",
  p_size = 6,
  s_x = 1,
  s_y = .75,
  s_width = 1.4,
  s_height = .9,
  h_fill = "#faa225",
  h_color = "#ffd151",
  dpi = 320,
  filename = "man/figures/logo.svg"
)
