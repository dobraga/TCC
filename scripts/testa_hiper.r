#######################
# DEFINIÇÂO DOS FOLDS #
#######################
library(magrittr)
set.seed(2108)

df <- readRDS("../output/df.rds")
bc <- readRDS("../output/bc.rds")

metric <- yardstick::metric_set(
  yardstick::rmse,
  yardstick::mae,
  yardstick::mape
)

split <- rsample::initial_split(df, .8)
train <- rsample::training(split)

folds <- train %>%
  dplyr::mutate(price = bc$transform(price)) %>%
  dplyr::select(-id, -neighbourhood_cleansed) %>%
  rsample::vfold_cv(., 10)

##################################
# TESTANDO QUANTIDADE DE ARVORES #
##################################
file_trees <- "../output/trees-metricas.csv"

testa_qtd <- function(qtd) {
  if (file.exists(file_trees)) {
    if (qtd %in% readr::read_csv(file_trees, col_types = readr::cols())$qtd) {
      return(T)
    }
  }


  tictoc::tic()
  aux <- folds %>%
    dplyr::transmute(
      test = purrr::map(splits, rsample::testing),
      model = purrr::map(
        splits,
        function(x) {
          model <- ranger::ranger(
            price ~ .,
            num.trees = qtd,
            data = rsample::training(x),
            seed = 2108
          )
        }
      )
    )

  time <- tictoc::toc()
  time <- time$toc - time$tic

  aux %>%
    purrr::map2(
      .x = .$model,
      .y = .$test,
      .f = ~ dplyr::tibble(
        price = .y$price,
        .pred = predict(.x, .y)$predictions
      )
      %>%
        metric(truth = price, estimate = .pred) %>%
        dplyr::bind_rows(
          dplyr::tibble(
            .metric = "Fora da Amostra",
            .estimator = "standard",
            .estimate = .x$prediction.error
          )
        )
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(.metric) %>%
    dplyr::summarise(
      sd = sd(.estimate),
      .estimate = mean(.estimate)
    ) %>%
    dplyr::mutate(qtd = qtd, time_elapsed = time) %>%
    readr::write_csv(., file_trees, append = file.exists(file_trees))
}

# testa_qtd(1)

future::plan("multicore")

tictoc::tic()
furrr::future_map(
  .x = 1:1000,
  .f = testa_qtd,
  .progress = TRUE
)
tictoc::toc()


#####################################
# TESTANDO PROFUNDIDADE DAS ARVORES #
#####################################
qtd <- 500
file_depth <- "../output/depth-metricas.csv"

testa_depth <- function(depth) {
  if (file.exists(file_depth)) {
    if (depth %in% readr::read_csv(file_depth, col_types = readr::cols())$depth) {
      return(T)
    }
  }


  tictoc::tic()
  aux <- folds %>%
    dplyr::transmute(
      test = purrr::map(splits, rsample::testing),
      model = purrr::map(
        splits,
        function(x) {
          model <- ranger::ranger(
            price ~ .,
            num.trees = qtd,
            max.depth = depth,
            data = rsample::training(x),
            seed = 2108
          )
        }
      )
    )

  time <- tictoc::toc()
  time <- time$toc - time$tic

  aux %>%
    purrr::map2(
      .x = .$model,
      .y = .$test,
      .f = ~ dplyr::tibble(
        price = .y$price,
        .pred = predict(.x, .y)$predictions
      )
      %>%
        metric(truth = price, estimate = .pred) %>%
        dplyr::bind_rows(
          dplyr::tibble(
            .metric = "Fora da Amostra",
            .estimator = "standard",
            .estimate = .x$prediction.error
          )
        )
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(.metric) %>%
    dplyr::summarise(
      sd = sd(.estimate),
      .estimate = mean(.estimate)
    ) %>%
    dplyr::mutate(depth = depth, time_elapsed = time) %>%
    readr::write_csv(., file_depth, append = file.exists(file_depth))
}

# testa_depth(NULL)

future::plan("multicore")

tictoc::tic()
furrr::future_map(
  .x = 1:30,
  .f = testa_depth,
  .progress = TRUE
)
tictoc::toc()

##################
# Tamanho minimo #
##################

qtd <- 600
depth <- 24
file_min_node <- "../output/min-node-metricas.csv"

testa_min_node <- function(min_node, file = file_min_node) {
  if (file.exists(file)) {
    if (depth %in% readr::read_csv(file, col_types = readr::cols())$min_node) {
      return(T)
    }
  }


  tictoc::tic()
  aux <- folds %>%
    dplyr::transmute(
      test = purrr::map(splits, rsample::testing),
      model = purrr::map(
        splits,
        function(x) {
          model <- ranger::ranger(
            price ~ .,
            num.trees = qtd,
            max.depth = depth,
            min.node.size = min_node,
            data = rsample::training(x),
            seed = 2108
          )
        }
      )
    )

  time <- tictoc::toc()
  time <- time$toc - time$tic

  aux %>%
    purrr::map2(
      .x = .$model,
      .y = .$test,
      .f = ~ dplyr::tibble(
        price = .y$price,
        .pred = predict(.x, .y)$predictions
      )
      %>%
        metric(truth = price, estimate = .pred) %>%
        dplyr::bind_rows(
          dplyr::tibble(
            .metric = "Fora da Amostra",
            .estimator = "standard",
            .estimate = .x$prediction.error
          )
        )
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(.metric) %>%
    dplyr::summarise(
      sd = sd(.estimate),
      .estimate = mean(.estimate)
    ) %>%
    dplyr::mutate(min_node = min_node, time_elapsed = time) %>%
    readr::write_csv(., file, append = file.exists(file))
}

# testa_min_node(0)

future::plan("multicore")

tictoc::tic()
furrr::future_map(
  .x = seq(0, 100, 1),
  .f = testa_min_node,
  .progress = TRUE
)
tictoc::toc()

##################################
# TESTANDO REGULARIZAÇÃO #
##################################
file_reg <- "../output/reg-metricas.csv"

lambda_grid <- dials::grid_regular(dials::penalty(), levels = 100)
mixture_grid <- dials::grid_regular(dials::mixture(), levels = 100)

grid <- expand.grid(lambda_grid$penalty, mixture_grid$mixture)

testa_reg <- function(penalty_, mixture_, file = file_reg) {
  if (file.exists(file)) {
    if (
      readr::read_csv(file, col_types = readr::cols()) %>%
        dplyr::filter(penalty == penalty_, mixture == mixture_) %>%
        nrow() > 0
    ) {
      return(T)
    }
  }


  tictoc::tic()
  aux <- folds %>%
    dplyr::transmute(
      test = purrr::map(splits, rsample::testing),
      model = purrr::map(
        splits,
        function(x) {
          parsnip::linear_reg(penalty = penalty_, mixture = mixture_) %>%
            parsnip::set_engine("glmnet") %>%
            parsnip::fit(price ~ ., data = rsample::training(x))
        }
      )
    )

  time <- tictoc::toc()
  time <- time$toc - time$tic

  aux %>%
    purrr::map2(
      .x = .$model,
      .y = .$test,
      .f = ~ dplyr::tibble(
        price = .y$price,
        .pred = predict(.x, .y)$.pred
      )
      %>%
        metric(truth = price, estimate = .pred)
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(.metric) %>%
    dplyr::summarise(
      sd = sd(.estimate),
      .estimate = mean(.estimate)
    ) %>%
    dplyr::mutate(penalty = penalty_, mixture = mixture_, time_elapsed = time) %>%
    readr::write_csv(., file, append = file.exists(file))
}

testa_reg(1, .5)

future::plan("multicore")

tictoc::tic()
furrr::future_map2(
  .x = grid$Var1,
  .y = grid$Var2,
  .f = testa_reg,
  .progress = TRUE
)
tictoc::toc()