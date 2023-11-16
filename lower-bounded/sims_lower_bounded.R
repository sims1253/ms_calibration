library(brms)
library(bayesim) # Run with https://github.com/sims1253/bayesim/releases/tag/v0.29.5.9000
                 # To reproduce
library(dplyr)

data_generation_configuration <- expand.grid(
  z1_x_coef = NA,
  z1_y_coef = NA,
  z2_y_coef = 0.5,
  z3_x_coef = 0.8,
  x_z4_coef = NA,
  y_z4_coef = NA,
  sigma_z1 = 0.5,
  sigma_z2 = 0.5,
  sigma_z3 = 0.5,
  sigma_z4 = 0.5,
  sigma_x = 0.5,
  data_N = 100,
  dataset_N = 200,
  data_family = c("gamma",
                  "weibull",
                  "lognormal",
                  "softplusnormal",
                  "frechet",
                  "betaprime",
                  "gompertz"),
  data_link = c("log", "softplus","identity"),
  lb = 0.000001,
  ub = Inf,
  resample = 1.3,
  x_y_coef = c(NA,0),
  y_intercept = NA,
  sigma_y = NA,
  shape = c("ramp", "asymmetric", "symmetric"),
  stringsAsFactors = FALSE
)

data_generation_configuration <- dplyr::filter(data_generation_configuration, !(data_link == "identity" & data_family != "lognormal" & data_family != "softplusnormal"))

data_generation_configuration <- dplyr::filter(data_generation_configuration, !(data_link != "identity" & (data_family == "lognormal" | data_family == "softplusnormal")))

z1_x_coef_list = list(
  "log" = 0.6,
  "softplus" = 1.2
)

z1_y_coef_list = list(
  "log" = 0.8,
  "softplus" = 1.2
)

x_z4_coef_list = list(
  "log" = 0.5,
  "softplus" = 0.5
)

y_z4_coef_list = list(
  "log" = 1,
  "softplus" = 0.5
)

sigma_y_list = list(
    "gamma" = c(1, 10, 40),
    "weibull" = c(1, 4, 8),
    "lognormal" = c(1, 0.35, 0.15),
    "softplusnormal" = c(2, 4, 2),
    "frechet" = c(2, 5, 10),
    "inverse.gaussian" = c(1, 10, 1000),
    "betaprime" = c(1, 20, 50),
    "gompertz" = c(0.2, 0.3, 0.6)
  )

y_intercept_list = list(
  "log" = log(c(1, 10, 10)),
  "softplus" = softplus(c(1, 10, 10))
)

x_y_coef_list = list(
  "log" = list(
    "ramp" = 0.5,
    "asymmetric" = 0.2,
    "symmetric" = 0.1),
  "softplus" = list(
    "ramp" = 0.9,
    "asymmetric" = 1.4,
    "symmetric" = 0.8 )
)

for (i in seq_len(nrow(data_generation_configuration))) {
  family <- data_generation_configuration$data_family[[i]]
  shape <- data_generation_configuration$shape[[i]]
  switch (family,
          "lognormal" = link <- "log",
          "softplusnormal" = link <- "softplus",
          link <- data_generation_configuration$data_link[[i]]
  )
  data_generation_configuration$z1_x_coef[[i]] <- z1_x_coef_list[[link]]
  data_generation_configuration$z1_y_coef[[i]] <- z1_y_coef_list[[link]]
    data_generation_configuration$x_z4_coef[[i]] <- x_z4_coef_list[[link]]
  data_generation_configuration$y_z4_coef[[i]] <- y_z4_coef_list[[link]]

  if(is.na(data_generation_configuration$x_y_coef[[i]])){
    data_generation_configuration$x_y_coef[[i]] <- x_y_coef_list[[link]][[shape]]
  }
  if (shape == "ramp") {
    data_generation_configuration$sigma_y[[i]] <- sigma_y_list[[family]][[1]]
    data_generation_configuration$y_intercept[[i]] <- y_intercept_list[[link]][[1]]
  }
  if (shape == "asymmetric") {
    data_generation_configuration$sigma_y[[i]] <- sigma_y_list[[family]][[2]]
    data_generation_configuration$y_intercept[[i]] <- y_intercept_list[[link]][[2]]
  }
  if (shape == "symmetric") {
    data_generation_configuration$sigma_y[[i]] <- sigma_y_list[[family]][[3]]
    data_generation_configuration$y_intercept[[i]] <- y_intercept_list[[link]][[3]]
  }
}
data_generation_configuration$id <- as.numeric(rownames(data_generation_configuration))

fit_configuration <- expand.grid(
  fit_family = c(
    "gompertz",
    "gamma",
    "weibull",
    "lognormal",
    "softplusnormal",
    "frechet",
    "betaprime",
    "gaussian"),
  fit_link =  c("log", "softplus", "identity"),
  formula = c("y ~ x + z1 + z2",
              "y ~ x + z2",
              "y ~ x + z1",
              "y ~ x + z1 + z2 + z3",
              "y ~ x + z1 + z2 + z4"
  ),
  stringsAsFactors = FALSE
)

fit_configuration <- dplyr::filter(fit_configuration, !(fit_link == "identity" & fit_family != "gaussian" & fit_family != "lognormal" & fit_family != "softplusnormal" & fit_family != "lognormal_custom"))

fit_configuration <- dplyr::filter(fit_configuration, !(fit_link != "identity" & (fit_family == "lognormal" | fit_family == "softplusnormal" | fit_family == "lognormal_custom")))

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
                     "pq_0.005",
                     "pq_0.025",
                     "pq_0.05",
                     "pq_0.1",
                     "pq_0.15",
                     "pq_0.2",
                     "pq_0.25",
                     "pq_0.3",
                     "pq_0.35",
                     "pq_0.4",
                     "pq_0.45",
                     "pq_0.5",
                     "pq_0.55",
                     "pq_0.6",
                     "pq_0.65",
                     "pq_0.7",
                     "pq_0.75",
                     "pq_0.8",
                     "pq_0.85",
                     "pq_0.9",
                     "pq_0.95",
                     "pq_0.975",
                     "pq_0.995",
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
