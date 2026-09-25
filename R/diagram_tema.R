# ---- Diagramtypografi (samma som i brott-appen) ----
diagram_text_storlek <- 11
diagram_rubrik_storlek <- 14
diagram_caption_storlek <- 9
diagram_axeltext_storlek <- 11

tema_diagram <- function() {
  theme_minimal(base_size = diagram_text_storlek) +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background  = element_rect(fill = "transparent", color = NA),
      plot.title = element_text(size = diagram_rubrik_storlek, lineheight = 1.1, face = "bold", margin = margin(b = 6)),
      plot.caption = element_text(size = diagram_caption_storlek, color = "#666", hjust = 0, margin = margin(t = 6)),
      plot.margin = margin(t = 4, r = 6, b = 2, l = 6),
      axis.text = element_text(size = diagram_axeltext_storlek)
    )
}

skapa_girafe <- function(p, klickbar = FALSE) {
  girafe(
    ggobj = p,
    width_svg = 10,
    height_svg = 4,
    bg = "transparent",
    options = list(
      opts_sizing(rescale = TRUE, width = 1),
      opts_hover(css = if (klickbar) "stroke-width:2;stroke:black;cursor:pointer;" else "stroke-width:2;stroke:black;"),
      opts_selection(type = if (klickbar) "single" else "none", only_shiny = TRUE),
      opts_toolbar(saveaspng = TRUE, pngname = "diagram")
    )
  )
}

# ---- Formatering av tal ----
formatera_tal <- function(x) {
  vapply(x, function(v) format(v, big.mark = " ", decimal.mark = ",", scientific = FALSE), character(1))
}

# Andelar visas som "34,5 % (120 av 348)", rena antal som "120"
formatera_varde <- function(varde, taljare, namnare) {
  ifelse(is.na(varde), "Uppgift saknas",
         ifelse(is.na(namnare), formatera_tal(varde),
                paste0(formatera_tal(varde), " % (", formatera_tal(taljare), " av ", formatera_tal(namnare), ")")))
}
