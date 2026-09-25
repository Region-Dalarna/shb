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
      plot.title.position = "plot",              # rubrik och underrubrik börjar vid diagrammets vänsterkant
      plot.caption = element_text(size = diagram_caption_storlek, color = "#666", hjust = 0, margin = margin(t = 6)),
      plot.margin = margin(t = 4, r = 6, b = 2, l = 6),
      axis.text = element_text(size = diagram_axeltext_storlek)
    )
}

# width/height i tum. Använd diagram_storlek() så att diagrammet får samma proportioner
# som sin cell, annars krymps det och hamnar mitt i cellen med tomrum runt
skapa_girafe <- function(p, klickbar = FALSE, width = 8, height = 3.9) {
  girafe(
    ggobj = p,
    width_svg = width,
    height_svg = height,
    bg = "transparent",
    options = list(
      opts_sizing(rescale = TRUE, width = 1),
      opts_hover(css = if (klickbar) "stroke-width:2;stroke:black;cursor:pointer;" else "stroke-width:2;stroke:black;"),
      opts_selection(type = if (klickbar) "single" else "none", only_shiny = TRUE),
      opts_toolbar(saveaspng = TRUE, pngname = "diagram")
    )
  )
}

# Diagrammets storlek i tum utifrån hur stor output-cellen är i webbläsaren.
# 80 px per tum ger lagom textstorlek (färre px per tum = mindre text i förhållande till diagrammet). Shiny ritar om diagrammet när fönstret ändrar storlek.
diagram_storlek <- function(session, output_id, px_per_tum = 80) {
  bredd <- session$clientData[[paste0("output_", output_id, "_width")]]
  hojd  <- session$clientData[[paste0("output_", output_id, "_height")]]
  if (is.null(bredd) || is.null(hojd) || bredd == 0 || hojd == 0) {
    bredd <- 900
    hojd  <- 450
  }
  list(width = max(bredd, 300) / px_per_tum, height = max(hojd, 150) / px_per_tum)
}

# Radbryter en rubrik så att den ryms i diagrammets bredd (i tum). Textens bredd mäts med
# systemfonts i samma typsnitt som diagrammet ritas med. Marginalen täcker diagrammets
# kantluft och att webbläsarens typsnitt kan bli något bredare.
radbryt <- function(text, bredd_tum, storlek_pt = diagram_rubrik_storlek, fet = TRUE, marginal = 0.85) {
  max_pt <- bredd_tum * 72 * marginal
  bredd_pt <- function(x) systemfonts::string_width(x, size = storlek_pt, res = 72, weight = if (fet) "bold" else "normal")

  rader <- character(0)
  rad <- ""
  for (ord in strsplit(text, " ", fixed = TRUE)[[1]]) {
    forsok <- if (rad == "") ord else paste(rad, ord)
    if (rad != "" && bredd_pt(forsok) > max_pt) {
      rader <- c(rader, rad)
      rad <- ord
    } else {
      rad <- forsok
    }
  }
  paste(c(rader, rad), collapse = "\n")
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
