# ---- Områdesprofil: slå ihop valda områden ----

# Slår ihop de valda områdena per år, ägarkategori och indikator genom att summera täljare och nämnare.
#
# Sekretess: statistiken är redan sekretessgranskad per område, och en sammanslagning får inte göra
# det möjligt att räkna fram ett dolt värde (A + B minus B ger A). Därför:
#   - är något av områdena dolt för en indikator blir även det sammanslagna värdet dolt
#   - är något av områdena ungefärligt (färre än 5) räknas täljaren som ett intervall och det
#     sammanslagna värdet blir ett intervall, som inte avslöjar mer än det som redan visas per område
#
# Resultatet har kolumnerna varde_lag och varde_hog (lika om värdet är exakt) och kommentar om dolt.
sla_ihop_omraden <- function(statistik, koder, min_taljare) {
  statistik %>%
    filter(regionkod %in% koder) %>%
    mutate(
      t_lag = case_when(skyddad ~ NA_real_,
                        farre_an ~ 1,
                        farre_utan ~ namnare - (min_taljare - 1),
                        TRUE ~ taljare),
      t_hog = case_when(skyddad ~ NA_real_,
                        farre_an ~ min_taljare - 1,
                        farre_utan ~ namnare - 1,
                        TRUE ~ taljare)
    ) %>%
    group_by(ar, agarkategori, grupp, indikator, indikator_namn, enhet, typ) %>%
    summarise(
      antal_omraden = n(),
      dolda = sum(skyddad),
      t_lag = sum(t_lag),
      t_hog = sum(t_hog),
      namnare = sum(namnare),
      .groups = "drop"
    ) %>%
    mutate(
      kommentar = if_else(dolda > 0, "Visas inte: minst ett av de valda områdena har för litet underlag", NA_character_),
      varde_lag = case_when(dolda > 0 ~ NA_real_, typ == "antal" ~ t_lag, TRUE ~ round(t_lag / namnare * 100, 1)),
      varde_hog = case_when(dolda > 0 ~ NA_real_, typ == "antal" ~ t_hog, TRUE ~ round(t_hog / namnare * 100, 1))
    )
}

# Samma kolumner (varde_lag, varde_hog, kommentar) för statistik som inte slås ihop, t.ex. kommun,
# Dalarna och riket, eller ett enskilt område. Ungefärliga värden blir intervall.
som_intervall <- function(statistik, min_taljare) {
  statistik %>%
    mutate(
      t_lag = case_when(skyddad ~ NA_real_, farre_an ~ 1, farre_utan ~ namnare - (min_taljare - 1), TRUE ~ taljare),
      t_hog = case_when(skyddad ~ NA_real_, farre_an ~ min_taljare - 1, farre_utan ~ namnare - 1, TRUE ~ taljare),
      varde_lag = case_when(skyddad ~ NA_real_, typ == "antal" ~ t_lag, TRUE ~ round(t_lag / namnare * 100, 1)),
      varde_hog = case_when(skyddad ~ NA_real_, typ == "antal" ~ t_hog, TRUE ~ round(t_hog / namnare * 100, 1))
    )
}

# Text till tooltips: "34,5 % (120 av 348 personer)", "4,1–4,7 % (21–24 av 508 personer)",
# "1 781 personer" eller kommentaren om värdet är dolt
# Täljare 1 till min_taljare - 1 skrivs som i övriga appen: "Under 2,4 % (färre än 5 av 208 personer)",
# och motsvarande "Över ..." när nämnaren minus täljaren är det.
formatera_intervall <- function(varde_lag, varde_hog, t_lag, t_hog, namnare, typ, enhet, kommentar,
                                min_taljare = shb_min_taljare) {
  enhet <- tolower(enhet)
  intervall <- function(a, b) ifelse(a == b, formatera_tal(a), paste0(formatera_tal(a), "–", formatera_tal(b)))
  case_when(
    !is.na(kommentar) ~ kommentar,
    is.na(varde_lag)  ~ "Uppgift saknas",
    typ == "andel" & t_lag == 1 & t_hog == min_taljare - 1 ~
      paste0("Under ", formatera_tal(round(min_taljare / namnare * 100, 1)), " % (färre än ", min_taljare, " av ",
             formatera_tal(namnare), " ", enhet, ")"),
    typ == "andel" & t_hog == namnare - 1 & t_lag == namnare - (min_taljare - 1) ~
      paste0("Över ", formatera_tal(round((namnare - min_taljare) / namnare * 100, 1)), " % (alla utom färre än ",
             min_taljare, " av ", formatera_tal(namnare), " ", enhet, ")"),
    typ == "antal"    ~ paste0(intervall(t_lag, t_hog), " ", enhet),
    TRUE              ~ paste0(intervall(varde_lag, varde_hog), " % (", intervall(t_lag, t_hog), " av ",
                               formatera_tal(namnare), " ", enhet, ")")
  )
}

# ---- Indikatorkort i områdesprofilen ----

# Kort text för ett värde i ett indikatorkort, t.ex. "15,8 %", "6,8–6,9 %", "Under 2,4 %" eller "Visas inte"
kort_varde <- function(varde_lag, varde_hog, t_lag, t_hog, namnare, kommentar, min_taljare = shb_min_taljare) {
  case_when(
    !is.na(kommentar) ~ "Visas inte",
    is.na(varde_lag) ~ "Uppgift saknas",
    t_lag == 1 & t_hog == min_taljare - 1 ~ paste0("Under ", formatera_tal(round(min_taljare / namnare * 100, 1)), " %"),
    t_hog == namnare - 1 & t_lag == namnare - (min_taljare - 1) ~
      paste0("Över ", formatera_tal(round((namnare - min_taljare) / namnare * 100, 1)), " %"),
    varde_lag == varde_hog ~ paste0(formatera_tal(varde_lag), " %"),
    TRUE ~ paste0(formatera_tal(varde_lag), "–", formatera_tal(varde_hog), " %")
  )
}

# Litet linjediagram som inbyggd SVG: området (med intervall för ungefärliga värden), Dalarna och riket över tid.
# Byggs som text i stället för med ggplot, så att många kort kan visas utan att sidan blir långsam.
# Varje år har en osynlig träffyta i full höjd med tooltiptext i data-tips, som visas av JavaScript i ui.R.
linjediagram_svg <- function(omrade, dalarna, riket, valt_ar, ar_min, ar_max, farg_omrade, farg_dalarna, farg_riket,
                             bredd = 220, hojd = 56, marginal = 5) {
  varden <- c(omrade$varde_lag, omrade$varde_hog, dalarna$mitt, riket$mitt)
  varden <- varden[!is.na(varden)]
  if (length(varden) == 0) return(NULL)
  y_min <- min(varden); y_max <- max(varden)
  if (y_max - y_min < 0.5) { y_min <- y_min - 0.5; y_max <- y_max + 0.5 }

  x <- function(ar) if (ar_max == ar_min) bredd / 2 else marginal + (ar - ar_min) / (ar_max - ar_min) * (bredd - 2 * marginal)
  y <- function(v) hojd - marginal - (v - y_min) / (y_max - y_min) * (hojd - 2 * marginal)
  xs <- function(ar) vapply(ar, x, numeric(1))

  # Linje genom år med värde, bruten där ett år saknar värde
  linje <- function(df, farg, tjocklek) {
    df <- df %>% filter(!is.na(mitt)) %>% arrange(ar)
    if (nrow(df) < 2) return("")
    grupp <- cumsum(c(1, diff(df$ar) > 1))
    paste(vapply(split(df, grupp), function(d) {
      if (nrow(d) < 2) return("")
      sprintf('<polyline points="%s" fill="none" stroke="%s" stroke-width="%s" stroke-linejoin="round"/>',
              paste(sprintf("%.1f,%.1f", xs(d$ar), y(d$mitt)), collapse = " "), farg, tjocklek)
    }, character(1)), collapse = "")
  }

  omr <- omrade %>% filter(!is.na(mitt))
  intervall <- omr %>% filter(ungefarlig)

  # Träffytor: en kolumn per år, så att man inte behöver pricka punkten
  alla_ar <- ar_min:ar_max
  steg <- if (ar_max > ar_min) (bredd - 2 * marginal) / (ar_max - ar_min) else bredd
  tips <- vapply(alla_ar, function(a) {
    o <- omrade %>% filter(ar == a)
    d <- dalarna %>% filter(ar == a)
    r <- riket %>% filter(ar == a)
    htmltools::htmlEscape(paste0(
      "<b>", a, "</b><br>Området: ", if (nrow(o) == 1) o$text else "Uppgift saknas",
      if (nrow(d) == 1) paste0("<br>Dalarna: ", d$text) else "",
      if (nrow(r) == 1) paste0("<br>Riket: ", r$text) else ""), attribute = TRUE)
  }, character(1))
  traffytor <- paste(sprintf('<rect x="%.1f" y="0" width="%.1f" height="%s" class="kort-traff" data-tips="%s"/>',
                             xs(alla_ar) - steg / 2, steg, hojd, tips), collapse = "")

  paste0(
    sprintf('<svg viewBox="0 0 %s %s" class="kort-linje" role="img" aria-label="Utveckling över tid">', bredd, hojd),
    if (ar_max > ar_min) sprintf('<line x1="%.1f" x2="%.1f" y1="0" y2="%s" class="kort-valt-ar"/>', x(valt_ar), x(valt_ar), hojd) else "",
    linje(riket, farg_riket, 1.1),
    linje(dalarna, farg_dalarna, 1.3),
    linje(omrade, farg_omrade, 2),
    paste(sprintf('<line x1="%.1f" x2="%.1f" y1="%.1f" y2="%.1f" stroke="%s" stroke-width="4" stroke-opacity="0.3"/>',
                  xs(intervall$ar), xs(intervall$ar), y(intervall$varde_lag), y(intervall$varde_hog), farg_omrade), collapse = ""),
    paste(sprintf('<circle cx="%.1f" cy="%.1f" r="%s" fill="%s" stroke="%s" stroke-width="%s"/>',
                  xs(omr$ar), y(omr$mitt), ifelse(omr$ar == valt_ar, 3.5, 2),
                  ifelse(omr$ungefarlig, "#fff", farg_omrade), farg_omrade, ifelse(omr$ungefarlig, 1.5, 0)), collapse = ""),
    traffytor,
    "</svg>"
  )
}
