eval_url <- "https://docs.google.com/forms/d/e/1FAIpQLSfEE7M3ygPDZYHG-071Mc34n-CDXtlLkzv1ez7p8yGCc0L3PQ/viewform?usp=pp_url&entry.1107513281="
eval_results <- "14fenF9h4qJynn9AqlTpZ1bJknaF8iWG1MaLpmE1RHrU"

read_xlsx("../egtpi-catalogo-datos/egtpi_tableau_data.xlsx") %>%
  select(UBIGEO, DEPARTAMENTO, PROVINCIA, DISTRITO, SM2022) -> ubigeo
read_xlsx("../egtpi-catalogo-datos/_ready_ses.xlsx") -> rawdata
gs4_deauth()
read_sheet(eval_results) -> revision

revision %>%
  filter(tolower(`Código del registro a evaluar [NO MODIFICAR]`) != "prueba",
         tolower(`DNI de la persona que evalúa`) != "prueba") %>%
  group_by(`Código del registro a evaluar [NO MODIFICAR]`) %>%
  mutate(ORDEN = n():1) %>%
  filter(ORDEN == 1) %>%
  select(-ORDEN) %>%
  ungroup() %>%
  transmute(
    UBIGEO = substr(`Código del registro a evaluar [NO MODIFICAR]`, 1, 6),
    ROW_ID = as.numeric(substr(`Código del registro a evaluar [NO MODIFICAR]`, 8, 999)),
    H1_EV = ifelse(`Evaluar herramientas [Herramienta 1: Sesión IAL + quórum]` == "Cumple",
                1, 0),
    H2_EV = ifelse(`Evaluar herramientas [Herramienta 2: Acta de homologación]` == "Cumple",
                1, 0),
    H3_EV = ifelse(`Evaluar herramientas [Herramienta 3: Mapa de sectorización]` == "Cumple",
                1, 0),
    H4_EV = ifelse(`Evaluar herramientas [Herramienta 4: Seguimiento nominal]` == "Cumple",
                1, 0),
    H5_EV = ifelse(`Evaluar herramientas [Herramienta 5: Análisis de cobertura]` == "Cumple",
                1, 0),
    TOTAL_EV = ifelse(H1_EV == 0, 0, H1_EV+H2_EV+H3_EV+H4_EV+H5_EV),
    ESTADO = `Resultado de la evaluación`,
    ESTADO_OBS = `Ingrese su observación`) -> rev_total
# FALTA FILTRAR DNIs

rawdata %>%
  select(ROW_ID, UBIGEO,
         FECHA = `Fecha de la sesión IAL`,
         H1_ACTA = `Acta de sesión`,
         H1_QUORUM = `Indicar si la sesión cumple con el quórum mínimo`,
         H2_ACTA = `Acta de homologación del Padrón Nominal`,
         H3_MAPA = `Herramienta de sectorización`,
         H4_SEGN = `Herramienta para el seguimiento nominal`,
         H5_COBE = `Herramienta para el análisis de cobertura`) %>%
  mutate(FECHA = substr(FECHA, 1, 10),
         MMAAAA = case_when(
           is.na(FECHA) ~ "Sin fecha",
           TRUE ~ paste0(year(FECHA),"/",
                         sprintf("%02d", as.numeric(month(FECHA))))),
         H1 = ifelse(!is.na(H1_ACTA) & !is.na(H1_QUORUM), 1, 0),
         H2 = ifelse(!is.na(H2_ACTA) & H1 == 1, 1, 0),
         H3 = ifelse(!is.na(H3_MAPA) & H1 == 1, 1, 0),
         H4 = ifelse(!is.na(H4_SEGN) & H1 == 1, 1, 0),
         H5 = ifelse(!is.na(H5_COBE) & H1 == 1, 1, 0),
         TOTAL = H1+H2+H3+H4+H5) %>%
  group_by(UBIGEO, MMAAAA) %>%
  left_join(ubigeo, by = "UBIGEO") -> pre_ses

pre_ses %>%
  transmute(
    ROW_ID, UBIGEO, DEPARTAMENTO, PROVINCIA, DISTRITO, SM2022,
    # MMAAAA = case_when(MMAAAA == "042022" ~ "Abril",
    #                    MMAAAA == "052022" ~ "Mayo",
    #                    MMAAAA == "062022" ~ "Junio",
    #                    MMAAAA == "072022" ~ "Julio",
    #                    MMAAAA == "082022" ~ "Agosto",
    #                    TRUE ~ "Fuera del periodo"),
    MMAAAA, FECHA, H1_QUORUM, H1_ACTA,
    H2_ACTA, H3_MAPA, H4_SEGN, H5_COBE, TOTAL) %>%
  nest(H1 = matches("^((FECHA)|(H1))")) %>%
  relocate(H1, .before = H2_ACTA) %>%
  mutate(LINK_EVAL =
           ifelse(MMAAAA == "Sin fecha",
                  NA, paste0(UBIGEO,"-",ROW_ID))) -> pre_data

pre_data %>%
  left_join(
    rev_total,
    by = c("UBIGEO", "ROW_ID")) %>%
  mutate(ESTADO = ifelse(is.na(ESTADO), "En revisión", ESTADO),
         TOTAL = ifelse(is.na(TOTAL_EV), TOTAL, TOTAL_EV)) %>%
  select(-TOTAL_EV) %>%
  mutate(FILTRAR_REV = ESTADO,
         # MMAAAA =
         #   factor(MMAAAA,
         #          c("Abril", "Mayo", "Junio", "Julio", "Agosto",
         #            "Fuera del periodo"),
         #          ordered = TRUE)
         ) %>%
  nest(H1 = starts_with("H1"),
       H2 = starts_with("H2"),
       H3 = starts_with("H3"),
       H4 = starts_with("H4"),
       H5 = starts_with("H5"),
       ESTADO = starts_with("ESTADO")) %>%
  relocate(TOTAL, .after = H5) %>%
  relocate(LINK_EVAL, .after = ESTADO) -> data