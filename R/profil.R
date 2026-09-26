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
