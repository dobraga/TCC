library(magrittr)

# Informações das locações
download.file(
  "http://data.insideairbnb.com/brazil/rj/rio-de-janeiro/2020-05-24/data/listings.csv.gz", 
  "../input/llistings.csv.gz"
)
R.utils::gunzip("../input/llistings.csv.gz")

# Informações de bairros e subprefeituras
bairros <- httr::GET(
  "https://pt.wikipedia.org/wiki/Lista_de_bairros_da_cidade_do_Rio_de_Janeiro"
  ) %>%
  httr::content() %>%
  rvest::html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  rvest::html_table(fill = TRUE) %>%
  .[[1]] %>%
  dplyr::select(-X4) %>%
  `colnames<-`(c("zona", "subprefeitura", "bairro")) %>%
  dplyr::filter(dplyr::row_number() > 2) %>%
  dplyr::mutate(
    subprefeitura = stringr::str_remove_all(
      subprefeitura, "(\\().+"
    ) %>% str_trim()
  ) %>%
  tidyr::separate_rows(bairro, sep = "•") %>%
  dplyr::mutate(
    bairro = stringr::str_trim(bairro),
    bairro = dplyr::case_when(
      bairro == "Freguesia" ~ "Freguesia (Ilha)",
      bairro == "Freguesia de Jacarepaguá" ~ "Freguesia (Jacarepaguá)",
      bairro == "Oswaldo Cruz" ~ "Osvaldo Cruz",
      bairro == "Quintino Bocaiuva" ~ "Quintino Bocaiúva",
      bairro == "Bairro Imperial de São Cristóvão" ~ "São Cristóvão",
      bairro == "Jabour" ~ "Bangu",
      TRUE ~ bairro
    )
  ) %>%
  dplyr::bind_rows(
    dplyr::tibble(
      "zona" = "Sul", 
      "subprefeitura" = "Zona Sul", 
      "bairro" = "Rocinha"
    )
  )

# Informações do Data.RIO sobre IDH e outros
download.file(
  "https://www.arcgis.com/sharing/rest/content/items/58186e41a2ad410f9099af99e46366fd/data", 
  "../input/idh.xls"
)

idh <- readxl::read_excel("./input/idh.xls", sheet = "2000") 

colnames(idh) <-  c(
    "ordem_segundo_o_idh", "bairro", "esperanca_vida", 
    "tx_alfabetizacao_adulta", "tx_frequencia_escolar",
    "na", "renda_per_capita", "na_2", "idh_longevidade", 
    "idh_educacao", "idh_renda", "idh"
  )

idh <- idh %>% 
  dplyr::mutate(
    ordem_segundo_o_idh = as.numeric(ordem_segundo_o_idh)
  ) %>%
  dplyr::filter(!is.na(ordem_segundo_o_idh)) %>%
  dplyr::select(-na, -na_2, -ordem_segundo_o_idh) %>%
  dplyr::mutate_at(
    dplyr::vars(esperanca_vida:idh),
    function(x) {
      stringr::str_remove_all(x, "[^0-9\\.]") %>%
        readr::parse_number()
    }
  ) %>%
  dplyr::mutate(
    bairro = dplyr::case_when(
      grepl("Bangu", bairro) ~ paste(
        bairro, "Gericinó", "Vila Kennedy", sep = ","
      ),
      grepl("Centro", bairro) ~ paste(
        bairro, "Lapa", sep = ","
      ),
      grepl("Vila da Penha", bairro) ~ paste(
        bairro, "Vila Kosmos", sep = ","
      ),
      TRUE ~ bairro
    )
  ) %>%
  tidyr::separate_rows(bairro, sep = ",") %>%
  dplyr::mutate(
    bairro = str_trim(bairro),
    bairro = case_when(
      bairro == "Oswaldo Cruz" ~ "Osvaldo Cruz",
      bairro == "Freguesia" ~ "Freguesia (Ilha)",
      TRUE ~ bairro
    )
  )

bairros %<>% inner_join(idh, by = "bairro")

# saveRDS(bairros, "../output/bairros.rds")
