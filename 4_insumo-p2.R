### Analisi de avanse
## NIVE REGIONAL
data %>%
  group_by(DEPARTAMENTO, MMAAAA) %>%
  summarise(total = n(),
            revisado =
              sum(FILTRAR_REV %in% c("Observado", "Cumple"))) %>%
  pivot_wider(
    id_cols = DEPARTAMENTO,
    names_from = MMAAAA,
    values_from = c("total", "revisado")
    ) %>%
  select(
    total_Abril, revisado_Abril,
    total_Mayo,  revisado_Mayo,
    total_Junio,  revisado_Junio,
    `total_Fuera del periodo`,
    `revisado_Fuera del periodo`
  ) %>%
  writexl::write_xlsx("out/reporte-revision.xlsx")

## COMO LE ASE DISTRITO
data %>%
  filter(MMAAAA != "Fuera del periodo") %>%
  unnest(c(H1, H2, H3, H4, H5, ESTADO)) %>% unnest(H1) %>%
  mutate(H1 = ifelse(!is.na(H1_QUORUM)&!is.na(H1_ACTA), 1, 0),
         H2 = ifelse(!is.na(H2_ACTA), 1, 0),
         H3 = ifelse(!is.na(H3_MAPA), 1, 0),
         H4 = ifelse(!is.na(H4_SEGN), 1, 0),
         H5 = ifelse(!is.na(H5_COBE), 1, 0)) %>%
  select(-H1_QUORUM, -H1_ACTA, -H2_ACTA, -H3_MAPA, -H4_SEGN, -H5_COBE) %>%
  ungroup %>%
  transmute(
    UBIGEO,
    FECHA = ymd(FECHA),
    MES = month(FECHA),
    ESTADO,
    H1 = coalesce(H1_EV, H1),
    H2 = coalesce(H2_EV, H2),
    H3 = coalesce(H3_EV, H3),
    H4 = coalesce(H4_EV, H4),
    H5 = coalesce(H5_EV, H5),
    TOTAL = H1+H2+H3+H4+H5,
    PRIORIDAD =
      case_when(
        ESTADO == "Cumple" ~ 2,
        ESTADO == "Observado" ~ 1,
        ESTADO == "En revisión" ~ 0
      )
  ) -> revisado_sm

writexl::write_xlsx(revisado_sm, "../matriz-p2-2022/in/revisado_sm.xlsx")