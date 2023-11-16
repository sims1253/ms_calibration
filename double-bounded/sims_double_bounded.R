library(brms)
library(bayesim) # Run with https://github.com/sims1253/bayesim/releases/tag/v0.29.5.9000
                 # To reproduce
library(dplyr)

data_generation_configuration <- expand.grid(
  z1_x_coef = 0.65,
  z1_y_coef = 0.65,
  z2_y_coef = 0.8,
  z3_x_coef = 0.8,
  x_z4_coef = 0.8,
  y_z4_coef = 1,
  sigma_z1 = 0.5,
  sigma_z2 = 0.5,
  sigma_z3 = 0.5,
  sigma_z4 = 0.5,
  sigma_x = 0.5,
  data_N = 100,
  dataset_N = 200,
  data_family = c("beta", "kumaraswamy", "logitnormal", "cauchitnormal", "cloglognormal", "simplex"),
  data_link = c("logit", "cauchit", "cloglog", "identity"),
  lb = 0.000001,
  ub = 0.999999,
  resample = 1.3,
  x_y_coef = c(NA,0),
  y_intercept = NA,
  sigma_y = NA,
  shape = c("symmetric", "asymmetric", "bathtub"),
  stringsAsFactors = FALSE
)
data_generation_configuration <- dplyr::filter(
  data_generation_configuration,
  !(data_link == "identity" & data_family != "logitnormal" & 
      data_family != "cauchitnormal" & data_family != "cloglognormal")
)

data_generation_configuration <- dplyr::filter(
  data_generation_configuration, 
  !(data_link != "identity" & 
      (data_family == "logitnormal" | data_family == "cauchitnormal" | data_family == "cloglognormal"))
)

sigma_y_list = list(
  "beta" = c(10, 10, 1.5),
  "kumaraswamy" = c(4, 2.25, 0.5),
  "logitnormal" = c(0.65, 0.8, 2.5),
  "cauchitnormal" = c(0.4, 1, 6),
  "cloglognormal" = c(0.3, 0.4, 3),
  "simplex" = c(1, 1.5, 8)
)

y_intercept_list = list(
  "logit" = c(logit(0.5), logit(0.25), logit(0.5)),
  "cauchit" = c(cauchit(0.5), cauchit(0.25), cauchit(0.5)),
  "cloglog" = c(cloglog(0.5), cloglog(0.25), cloglog(0.5))
)

x_y_coef_list = list(
  "logit" = 0.3,
  "cauchit" = 0.3,
  "cloglog" = 0.3
)

for (i in seq_len(nrow(data_generation_configuration))) {
  family <- data_generation_configuration$data_family[[i]]
  switch (family,
          "logitnormal" = link <- "logit",
          "cauchitnormal" = link <- "cauchit",
          "cloglognormal" = link <- "cloglog",
          link <- data_generation_configuration$data_link[[i]]
  )
  
  if(is.na(data_generation_configuration$x_y_coef[[i]])){
    data_generation_configuration$x_y_coef[[i]] <- x_y_coef_list[[link]]
  }
  if (data_generation_configuration$shape[[i]] == "symmetric") {
    data_generation_configuration$sigma_y[[i]] <- sigma_y_list[[family]][[1]]
    data_generation_configuration$y_intercept[[i]] <- y_intercept_list[[link]][[1]]
  }
  if (data_generation_configuration$shape[[i]] == "asymmetric") {
    data_generation_configuration$sigma_y[[i]] <- sigma_y_list[[family]][[2]]
    data_generation_configuration$y_intercept[[i]] <- y_intercept_list[[link]][[2]]
  }
  if (data_generation_configuration$shape[[i]] == "bathtub") {
    data_generation_configuration$sigma_y[[i]] <- sigma_y_list[[family]][[3]]
    data_generation_configuration$y_intercept[[i]] <- y_intercept_list[[link]][[3]]
  }
}
data_generation_configuration$id <- as.numeric(rownames(data_generation_configuration))

fit_configuration <- expand.grid(
  fit_family = c("beta", "kumaraswamy", "logitnormal", "cauchitnormal", 
                 "cloglognormal", "simplex", "gaussian"),
  fit_link =  c("logit", "cauchit", "cloglog", "identity"),
  formula = c("y ~ x + z1 + z2",
              "y ~ x + z2",
              "y ~ x + z1",
              "y ~ x + z1 + z2 + z3",
              "y ~ x + z1 + z2 + z4"
  ),
  stringsAsFactors = FALSE
)

fit_configuration <- dplyr::filter(
  fit_configuration, 
  !(fit_link == "identity" & fit_family != "gaussian" & 
      fit_family != "logitnormal" & fit_family != "cauchitnormal" & fit_family != "cloglognormal")
)

fit_configuration <- dplyr::filter(
  fit_configuration, 
  !(fit_link != "identity" & 
      (fit_family == "logitnormal" | fit_family == "cauchitnormal" | fit_family == "cloglognormal"))
)

numeric_metrics <- c("rmse_s",
                     "bias",
                     "divergents",
                     "p_mean",
                     "p_sd",
                     # "rstar",
                     "pareto_k",
                     "time",
                     "rhat",
                     "ess_bulk",
                     "ess_tail",
                     "q_true",
                     "mae_s",
                     "pq_0.025",
                     "pq_0.05",
                     "pq_0.1",
                     "pq_0.25",
                     "pq_0.5",
                     "pq_0.75",
                     "pq_0.9",
                     "pq_0.95",
                     "pq_0.975",
                     "pos_prob"
                   )

predictive_metrics <- c("elpd_loo",
                        "elpd_newdata",
                        "rmse_loo",
                        "rmse_newdata")

full_simulation(data_gen_confs = data_generation_configuration,
                fit_confs = fit_configuration,
                numeric_metrics = numeric_metrics,
                predictive_metrics = predictive_metrics,
                ncores_simulation = 20,
                brms_backend = "rstan",
                seed = 1235813,
                result_path = "./") 
