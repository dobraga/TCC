#######################
# DEFINIÇÂO DOS FOLDS #
#######################
library(magrittr)
set.seed(2108)

df <- readRDS("../output/df.rds") #base transformada
bc <- readRDS("../output/bc.rds") #transformação de boxcox

split <- rsample::initial_split(df, .8)
train <- rsample::training(split)

folds <- train %>%
    dplyr::mutate(price = bc$transform(price)) %>%
    dplyr::select(-id, -neighbourhood_cleansed) %>%
    rsample::vfold_cv(., 10)

grid_ini <- expand.grid(
    num.trees = seq(1, 2000, 1),
    max.depth = seq(0, 30),
    min.node = seq(0, 100, 1),
    response = NA
) %>% tibble::as_tibble()

file <- "../output/hiper-metricas.csv"
error <- "rmse"
metric <- yardstick::metric_set(
    yardstick::rmse,
    yardstick::mae,
    yardstick::mape
)

testa_avalia <- function(split, num.trees, max.depth, min.node) {
    # Função que treina e avalia um único fold
    model <- ranger::ranger(
        price ~ .,
        num.trees = num.trees,
        max.depth = max.depth,
        min.node = min.node,
        data = rsample::training(split),
        seed = 2108, verbose = F,
        num.threads = 4, save.memory = T
    )

    dplyr::tibble(
        price = rsample::testing(split)$price,
        .pred = predict(model, rsample::testing(split))$predictions
    ) %>%
        metric(truth = price, estimate = .pred) %>%
        dplyr::bind_rows(
            .,
            dplyr::tibble(
                .metric = "Fora da Amostra",
                .estimator = "standard",
                .estimate = model$prediction.error
            )
        )
}

testa_param <- function(num.trees, max.depth, min.node, round, tipo) {
    # Função que avalia todos os folds e tira as medidas
    id <- stringi::stri_rand_strings(1, 5)

    print(
        paste("   ", id, "- Parâmetros que serão testados:", 
              num.trees, max.depth, min.node)
        )

    # future::plan(future::multicore())

    aux <- folds %>%
            dplyr::transmute(
                avaliacao = purrr::pmap(
                    list(
                        folds$splits,
                        rep(num.trees, nrow(folds)),
                        rep(max.depth, nrow(folds)),
                        rep(min.node, nrow(folds))
                    ),
                    testa_avalia
                )
            )

    print(paste("   ", id, "- Modelos treinado e avaliado nas bases de teste"))

    aux <- suppressMessages(
        aux %>%
            tidyr::unnest(cols = avaliacao) %>%
            dplyr::group_by(.metric) %>%
            dplyr::summarise(
                sd = sd(.estimate),
                .estimate = mean(.estimate)
            ) %>%
            dplyr::mutate(
                num.trees = num.trees,
                max.depth = max.depth,
                min.node = min.node,
                round = round,
                tipo = tipo
            )
    )

    print(paste("   ", id, "- Sumarisando e criando tabela que será salva"))

    readr::write_csv(aux, file, append = file.exists(file))
}

read_metrics <- function(grid, error) {
    if (file.exists(file)) {
        metrics <- readr::read_csv(file, col_types = readr::cols())
        metrics <- suppressMessages(
            metrics %>%
                dplyr::filter(.metric == error) %>%
                dplyr::group_by(num.trees, max.depth, min.node, tipo) %>%
                dplyr::summarise(
                    .response = mean(.estimate),
                    round = min(round)
                )
        )

        return(
            suppressMessages(
                dplyr::left_join(grid, metrics) %>%
                    dplyr::transmute(
                        num.trees, max.depth, min.node,
                        response = .response,
                        tipo, round = round
                    )
            )
        )
    } else {
        grid$tipo <- NA
        grid$round <- NA
        return(grid)
    }
}

eval_sample <- function(sample, round, tipo) {
    # future::plan(future::multiprocess())
    # future::plan(future::multicore())

    purrr::pmap(
        # suppressMessages(
    # furrr::future_pmap(
        list(
            sample$num.trees, sample$max.depth, sample$min.node, 
            rep(round, nrow(sample)), rep(tipo, nrow(sample))
            ),
        testa_param
    )
    # )
}

train_model <- function(grid) {
    print("Filtrando parâmetros já testados")

    grid_model <- grid %>%
        dplyr::filter(!is.na(response)) %>%
        dplyr::select(num.trees, max.depth, min.node, response)

    if (nrow(grid_model) == 0) {
        return(grid)
    }
    else {
        print("Treinando modelo")

        model <- grid_model %>%
            ranger::ranger(
                response ~ ., .,
                num.trees = 10,
                verbose = F
            )

        print("Modelo treinado com sucesso")

        sample_grid <- grid %>% dplyr::filter(is.na(response))

        print("Avalia parâmetros não testados")
        sample_grid$response <- predict(model, sample_grid)$predictions

        return(sample_grid)
    }
}

run_round <- function(tipo_ = "model", n = 10) {
    grid <- read_metrics(grid = grid_ini, error = error)
    round <- max(
        dplyr::coalesce(
            grid %>% dplyr::filter(tipo == tipo_) %>% .$round, 
            0
        )) + 1

    print(paste("Iniciando rodada", round))

    if (tipo_ == "model") {
        grid_teste <- train_model(grid)

        sample <- grid_teste %>%
            dplyr::sample_frac(1) %>%
            dplyr::arrange(response) %>%
            dplyr::slice(1:n)
    } else {
        sample <- grid %>%
            dplyr::filter(is.na(response)) %>%
            dplyr::sample_n(n)
    }

    print("Amostra realizada")

    eval_sample(sample, round, tipo_)

    print(paste("Rodada", round, "terminou"))
}

possible_run_round <- purrr::possibly(run_round, otherwise = 'erro')

roda <- function(error = "rmse", rounds = 50) {
    for (i in seq(1:rounds)) {
        possible_run_round("sample")
        possible_run_round("model")
    }
}

roda()
