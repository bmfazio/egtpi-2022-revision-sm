## Descargable
data %>%
  unnest(c(H1, H2, H3, H4, H5, ESTADO)) %>% unnest(H1) %>%
  arrange(UBIGEO, ymd(FECHA)) %>%
  transmute(
    UBIGEO, DEPARTAMENTO, PROVINCIA, DISTRITO, SM2022, MMAAAA,
    H1 = ifelse(is.na(H1_ACTA), "NO", "SI"),
    H2 = ifelse(is.na(H2_ACTA), "NO", "SI"),
    H3 = ifelse(is.na(H3_MAPA), "NO", "SI"),
    H4 = ifelse(is.na(H4_SEGN), "NO", "SI"),
    H5 = ifelse(is.na(H5_COBE), "NO", "SI"),
    TOTAL, ESTADO, ESTADO_OBS,
    H1_EV, H2_EV, H3_EV, H4_EV, H5_EV) %>%
  writexl::write_xlsx("out/descargable/raw_descargable.xlsx")

####
