# Load packages ----
library(tidyverse)
library(apollo)
library(gt)

# Estimating a choice model ----
## Initialize Apollo ----
apollo_initialise()

## Set core controls ----
apollo_control = list(
  modelName = "mnl",
  modelDescr = "Basic MNL model on the windmills dataset",
  indivID = "id_individual"
)

## Import data ----
# Read in the data and filter out missing choices
database <- read_csv(file.path("Data", "data-windmills.csv")) |>
  filter(!is.na(choice))

## Set the starting values of the parameters ----
apollo_beta <- c(
  b_asc_alt1 = 0.00,
  b_asc_alt2 = 0.50,
  b_asc_alt3 = 0.50,
  b_medium_farms = 0.25,
  b_small_farms = 0.50,
  b_medium_height = 0.25,
  b_low_height = 0.50,
  b_red_kite = -0.05,
  b_min_distance = 0.50,
  b_cost = -0.50
)

# Specify the vector of paramters to hold fixed at their starting values
apollo_fixed <- c("b_asc_alt1")

# Group and validate inputs
apollo_inputs <- apollo_validateInputs()

## Define the model and likelihood function ----
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  ## Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  # Define the list of utility functions
  V <- list(
    alt1 = (
      b_asc_alt1 +
      b_medium_farms * alt1_farm2 +
      b_small_farms * alt1_farm3 +
      b_medium_height * alt1_height2 +
      b_low_height * alt1_height3 +
      b_red_kite * alt1_redkite +
      b_min_distance * alt1_distance +
      b_cost * alt1_cost
    ),
    alt2 = (
      b_asc_alt2 +
      b_medium_farms * alt2_farm2 +
      b_small_farms * alt2_farm3 +
      b_medium_height * alt2_height2 +
      b_low_height * alt2_height3 +
      b_red_kite * alt2_redkite +
      b_min_distance * alt2_distance +
      b_cost * alt2_cost
    ),
    alt3 = (
      b_asc_alt3 +
      b_medium_farms * alt3_farm2 +
      b_small_farms * alt3_farm3 +
      b_medium_height * alt3_height2 +
      b_low_height * alt3_height3 +
      b_red_kite * alt3_redkite +
      b_min_distance * alt3_distance +
      b_cost * alt3_cost
    )
  )

  # Define settings for MNL model component
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar = choice,
    V = V
  )

  # Calculate the probabilities
  P <- list(
    model = apollo_mnl(mnl_settings, functionality)
  )

  # Take the product across observations
  P <- apollo_panelProd(P, apollo_inputs, functionality)

  # Prepare and return the outputs
  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  # Return the probabilities
  return(
    P
  )
}

## Estimate the model  ----
model <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(
    writeIter = FALSE,
    silent = TRUE,
    estimationRoutine = "bgw"
  )
)

## Print model output to console ----
apollo_modelOutput(
  model,
  modelOutput_settings = list(
    printOutliers = 10
  )
)

# Evaluating the choice model ----
## Eigenvalues ----
round(model$hessianEigenValue, digits = 3)

## Outliers ----
# Get the predicted probabilities
predicted_probabilities <- apollo_prediction(model, apollo_probabilities, apollo_inputs)

# Inspect the predicted probabilities for the first individual.
predicted_probabilities[predicted_probabilities$ID == 1, ]

# Calculate the average predicted probability by individual
avg_chosen <- predicted_probabilities |>
  group_by(ID) |>
  summarise(
    avg_prob = mean(chosen)
  ) |>
  arrange(avg_prob)

# Get the ID of the first outlier
first_outlier <- avg_chosen |>
  slice(1) |>
  pull(ID)

# Calculate the average predicted probability for the first outlier
avg_pred_prob_first <- predicted_probabilities |>
  filter(ID == first_outlier) |>
  summarize(
    avg_prob = mean(chosen)
  )

# MNL with socio-economic variables ----
## New starting values ----
apollo_beta <- c(
  b_asc_alt1 = 0.00,
  b_asc_alt2 = 0.50,
  b_asc_alt3 = 0.50,
  b_medium_farms = 0.25,
  b_small_farms = 0.50,
  b_medium_height = 0.25,
  b_low_height = 0.50,
  b_red_kite = -0.05,
  b_min_distance = 0.50,
  b_cost = -0.50,
  delta_asc2_age = 0.00,
  delta_asc2_female = 0.00,
  delta_asc2_educ = 0.00,
  delta_asc3_age = 0.00,
  delta_asc3_female = 0.00,
  delta_asc3_educ = 0.00,
  delta_mf_age = 0.00,
  delta_mf_female = 0.00,
  delta_mf_educ = 0.00,
  delta_sf_age = 0.00,
  delta_sf_female = 0.00,
  delta_sf_educ = 0.00,
  delta_mh_age = 0.00,
  delta_mh_female = 0.00,
  delta_mh_educ = 0.00,
  delta_lh_age = 0.00,
  delta_lh_female = 0.00,
  delta_lh_educ = 0.00,
  delta_rk_age = 0.00,
  delta_rk_female = 0.00,
  delta_rk_educ = 0.00,
  delta_md_age = 0.00,
  delta_md_female = 0.00,
  delta_md_educ = 0.00,
  delta_ct_age = 0.00,
  delta_ct_female = 0.00,
  delta_ct_educ = 0.00
)

# Specify the vector of paramters to hold fixed at their starting values
apollo_fixed <- c("b_asc_alt1")

# Group and validate inputs
apollo_inputs <- apollo_validateInputs()

## Define the model and likelihood function ----
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  ## Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  # Define the list of utility functions
  V <- list(
    alt1 = (
      b_asc_alt1 +
        (b_medium_farms + delta_mf_age * age + delta_mf_female * female + delta_mf_educ * education) * alt1_farm2 +
        (b_small_farms  + delta_sf_age * age + delta_sf_female * female + delta_sf_educ * education) * alt1_farm3 +
        (b_medium_height + delta_mh_age * age + delta_mh_female * female + delta_mh_educ * education) * alt1_height2 +
        (b_low_height  + delta_lh_age * age + delta_lh_female * female + delta_lh_educ * education) * alt1_height3 +
        (b_red_kite + delta_rk_age * age + delta_rk_female * female + delta_rk_educ * education) * alt1_redkite +
        (b_min_distance + delta_md_age * age + delta_md_female * female + delta_md_educ * education) * alt1_distance +
        (b_cost + delta_ct_age * age + delta_ct_female * female + delta_ct_educ * education) * alt1_cost
    ),
    alt2 = (
      b_asc_alt2 + delta_asc2_age * age + delta_asc2_female * female + delta_asc2_educ * education +
        (b_medium_farms + delta_mf_age * age + delta_mf_female * female + delta_mf_educ * education) * alt2_farm2 +
        (b_small_farms  + delta_sf_age * age + delta_sf_female * female + delta_sf_educ * education) * alt2_farm3 +
        (b_medium_height + delta_mh_age * age + delta_mh_female * female + delta_mh_educ * education) * alt2_height2 +
        (b_low_height  + delta_lh_age * age + delta_lh_female * female + delta_lh_educ * education) * alt2_height3 +
        (b_red_kite + delta_rk_age * age + delta_rk_female * female + delta_rk_educ * education) * alt2_redkite +
        (b_min_distance + delta_md_age * age + delta_md_female * female + delta_md_educ * education) * alt2_distance +
        (b_cost + delta_ct_age * age + delta_ct_female * female + delta_ct_educ * education) * alt2_cost
    ),
    alt3 = (
      b_asc_alt3 + delta_asc3_age * age + delta_asc3_female * female + delta_asc3_educ * education +
        (b_medium_farms + delta_mf_age * age + delta_mf_female * female + delta_mf_educ * education) * alt3_farm2 +
        (b_small_farms  + delta_sf_age * age + delta_sf_female * female + delta_sf_educ * education) * alt3_farm3 +
        (b_medium_height + delta_mh_age * age + delta_mh_female * female + delta_mh_educ * education) * alt3_height2 +
        (b_low_height  + delta_lh_age * age + delta_lh_female * female + delta_lh_educ * education) * alt3_height3 +
        (b_red_kite + delta_rk_age * age + delta_rk_female * female + delta_rk_educ * education) * alt3_redkite +
        (b_min_distance + delta_md_age * age + delta_md_female * female + delta_md_educ * education) * alt3_distance +
        (b_cost + delta_ct_age * age + delta_ct_female * female + delta_ct_educ * education) * alt3_cost
    )
  )

  # Define settings for MNL model component
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar = choice,
    V = V
  )

  # Calculate the probabilities
  P <- list(
    model = apollo_mnl(mnl_settings, functionality)
  )

  # Take the product across observations
  P <- apollo_panelProd(P, apollo_inputs, functionality)

  # Prepare and return the outputs
  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  # Return the probabilities
  return(
    P
  )
}

## Estimate the model  ----
model <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(
    writeIter = FALSE,
    silent = TRUE,
    estimationRoutine = "bgw"
  )
)

## Print model output to console ----
apollo_modelOutput(
  model,
  modelOutput_settings = list(
    printOutliers = 10
  )
)

# RP-MIXL with uncorrelated ASC ----

## Initialize Apollo ----
apollo_initialise()

## Set core controls ----
apollo_control = list(
  modelName = "rpl-mixl-uncorrelated-asc",
  modelDescr = "RP-MIXL with uncorrelated alternative specific constants on the windmills dataset",
  indivID = "id_individual",
  nCores = 7
)

## Import data ----
# Read in the data and filter out missing choices
database <- read_csv(file.path("Data", "data-windmills.csv")) |>
  filter(!is.na(choice))

## Set the starting values of the parameters ----
apollo_beta <- c(
  asc_alt1 = 0.00,
  asc_alt2 = 0.50,
  asc_alt3 = 0.50,
  mu_mf   =  0.25,
  sd_mf =0.1,
  mu_sf =  0.50,
  sd_sf = 0.2,
  mu_mh = 0.25,
  sd_mh = 0.1,
  mu_lh = 0.50,
  sd_lh = 0.2,
  mu_rk = -0.05,
  sd_rk = 0.01,
  mu_md = 0.50,
  sd_md = 0.2,
  mu_ct = -0.50,
  sd_ct = 0.1,
  delta_asc2_age = 0.00,
  delta_asc2_female = 0.00,
  delta_asc2_educ = 0.00,
  delta_asc3_age = 0.00,
  delta_asc3_female = 0.00,
  delta_asc3_educ = 0.00,
  delta_mf_age = -0.01,
  delta_sf_age = -0.01,
  delta_mf_female = -0.18,
  delta_sf_female = -0.06,
  delta_mf_educ = -0.04,
  delta_sf_educ = -0.02,
  delta_mh_age = -0.01,
  delta_lh_age = -0.01,
  delta_mh_female = 0.01,
  delta_lh_female = 0.04,
  delta_mh_educ = 0.02,
  delta_lh_educ = 0.12,
  delta_rk_age = -0.01,
  delta_md_age = -0.01,
  delta_rk_female = 0.01,
  delta_md_female = 0.01,
  delta_rk_educ = 0.12,
  delta_md_educ = -0.01,
  delta_ct_age = 0.01,
  delta_ct_female = -0.01,
  delta_ct_educ = -0.01
)

apollo_fixed = c("asc_alt1")

## Define the random components ----
apollo_draws <- list(
  interDrawsType = "sobol",
  interNDraws = 5000,
  interUnifDraws = c(),
  interNormDraws = c(
    "draws_mf",
    "draws_sf",
    "draws_mh",
    "draws_lh",
    "draws_rk",
    "draws_md",
    "draws_ct"
  ),
  intraDrawsType = "halton",
  intraNDraws = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
  randcoeff <- list(
    r_mf = mu_mf + sd_mf * draws_mf + delta_mf_age * age + delta_mf_female * female + delta_mf_educ * education,
    r_sf = mu_sf + sd_sf * draws_sf + delta_sf_age * age + delta_sf_female * female + delta_sf_educ * education,
    r_mh = mu_mh + sd_mh * draws_mh + delta_mh_age * age + delta_mh_female * female + delta_mh_educ * education,
    r_lh = mu_lh + sd_lh * draws_lh + delta_lh_age * age + delta_lh_female * female + delta_lh_educ * education,
    r_rk = mu_rk + sd_rk * draws_rk + delta_rk_age * age + delta_rk_female * female + delta_rk_educ * education,
    r_md = mu_md + sd_md * draws_md + delta_md_age * age + delta_md_female * female + delta_md_educ * education,
    r_ct = -exp(mu_ct + sd_ct * draws_ct + delta_ct_age * age + delta_ct_female * female + delta_ct_educ * education)
  )

  return(
    randcoeff
  )
}

## Group and validate inputs ----
apollo_inputs <- apollo_validateInputs()

## Define the model and likelihood function ----
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  ## Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  # Define the list of utility functions
  V <- list(
    alt1 = (
      asc_alt1 +
        r_mf * alt1_farm2 +
        r_sf * alt1_farm3 +
        r_mh * alt1_height2 +
        r_lh * alt1_height3 +
        r_rk * alt1_redkite +
        r_md * alt1_distance +
        r_ct * alt1_cost
    ),
    alt2 = (
      asc_alt2 +
        delta_asc2_age * age +
        delta_asc2_female * female +
        delta_asc2_educ * education +
        r_mf * alt2_farm2 +
        r_sf * alt2_farm3 +
        r_mh * alt2_height2 +
        r_lh * alt2_height3 +
        r_rk * alt2_redkite +
        r_md * alt2_distance +
        r_ct * alt2_cost
    ),
    alt3 = (
      asc_alt3 +
        delta_asc3_age * age +
        delta_asc3_female * female +
        delta_asc3_educ * education +
        r_mf * alt3_farm2 +
        r_sf * alt3_farm3 +
        r_mh * alt3_height2 +
        r_lh * alt3_height3 +
        r_rk * alt3_redkite +
        r_md * alt3_distance +
        r_ct * alt3_cost
    )
  )

  # Define settings for MNL model component
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar = choice,
    V = V
  )

  # Calculate the probabilities
  P <- list(
    model = apollo_mnl(mnl_settings, functionality)
  )

  # Take the product across observations
  P <- apollo_panelProd(P, apollo_inputs, functionality)

  # Average across inter-individual draws
  P <- apollo_avgInterDraws(P, apollo_inputs, functionality)

  # Prepare and return the outputs
  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  # Return the probabilities
  return(
    P
  )
}

## Estimate the model  ----
model <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(
    writeIter = FALSE,
    silent = FALSE,
    estimationRoutine = "bgw"
  )
)

## Print model output to console ----
apollo_modelOutput(
  model,
  modelOutput_settings = list(
    printOutliers = 10
  )
)

## Density plot ----

# First density plot parameters
est_mu_sf1 <- model$estimate["mu_sf"] + model$estimate["delta_sf_age"] * 30 + model$estimate["delta_sf_female"] * 1 + model$estimate["delta_sf_educ"] * 3
est_sd_sf1 <- abs(model$estimate["sd_sf"])

# Second density plot parameters
est_mu_sf2 <- model$estimate["mu_sf"] + model$estimate["delta_sf_age"] * 25 + model$estimate["delta_sf_female"] * 1 + model$estimate["delta_sf_educ"] * 1
est_sd_sf2 <- abs(model$estimate["sd_sf"])

# Create a data frame with a sequence of x values to plot
x_values <- tibble(
  x = seq(
    min(est_mu_sf1, est_mu_sf2) - 4 * max(est_sd_sf1, est_sd_sf2),
    max(est_mu_sf1, est_mu_sf2) + 4 * max(est_sd_sf1, est_sd_sf2),
    length.out = 1000
  )
)

# Create ggplot2 density plot with two distributions
x_values |>
  ggplot(mapping = aes(x = x)) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = est_mu_sf1,
      sd = est_sd_sf1
    ),
    color = "red",
    size = 1
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = est_mu_sf2,
      sd = est_sd_sf2
    ),
    color = "blue",
    size = 1,
    linetype = "dashed"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_blank()
  )



# Generate a density plot with true heterogeneity ----
tibble(
  true_heterogeneity = c(rnorm(20000, mean = -0.5, sd = 0.3), rnorm(30000, mean = 1, sd = 0.3)),
  estimated_distribution = rnorm(50000, mean = mean(true_heterogeneity), sd = sd(true_heterogeneity))
) |>
  pivot_longer(everything(), names_to = "group", values_to = "value") |>
  ggplot(aes(x = value, fill = group)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_blank()
  ) +
  labs(fill = " ")


# Generate a density plot with true heterogeneity ----
tibble(
  true_heterogeneity = c(rnorm(30000, mean = 0.3, sd = 0.05), rnorm(30000, mean = 1.5, sd = 0.3)),
  estimated_distribution = rnorm(60000, mean = 0.8, sd = 0.8)
) |>
  pivot_longer(everything(), names_to = "group", values_to = "value") |>
  ggplot(aes(x = value, fill = group)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_blank()
  ) +
  labs(fill = " ")

## RPL MIXL uncorrelated plot cost
# Existing mean and standard deviation calculation
est_mu_ct_1 <- model$estimate["mu_ct"] + model$estimate["delta_ct_age"] * 30 + model$estimate["delta_ct_female"] * 1 + model$estimate["delta_ct_educ"] * 3
est_sd_ct_1 <- abs(model$estimate["sd_ct"])

# Calculate mean and standard deviation for the second distribution
est_mu_ct_2 <- model$estimate["mu_ct"] + model$estimate["delta_ct_age"] * 25 + model$estimate["delta_ct_female"] * 1 + model$estimate["delta_ct_educ"] * 1
est_sd_ct_2 <- abs(model$estimate["sd_ct"])

tibble(
  data_1 = -exp(rnorm(10000, mean = est_mu_ct_1, sd = est_sd_ct_1)),
  data_2 = -exp(rnorm(10000, mean = est_mu_ct_2, sd = est_sd_ct_2))
) |>
  pivot_longer(everything(), names_to = "group", values_to = "value") |>
  ggplot() +
  geom_density(aes(x = value, fill = group), alpha = 0.5) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_blank()
  )

# RPL-MIXL with correlation ----


## Initialize Apollo ----
apollo_initialise()

## Set core controls ----
apollo_control = list(
  modelName = "rpl-mixl-correlated-asc",
  modelDescr = "RP-MIXL with correlated alternative specific constants on the windmills dataset",
  indivID = "id_individual",
  nCores = 7
)

## Import data ----
# Read in the data and filter out missing choices
database <- read_csv(file.path("Data", "data-windmills.csv")) |>
  filter(!is.na(choice))

## Set the starting values of the parameters ----
apollo_beta <- c(
  asc_alt1 = 0,
  asc_alt2 = 0.5,
  asc_alt3 = 0.5,
  mu_mf = 0.25,
  mu_sf = 0.5,
  mu_mh = 0.25,
  mu_lh = 0.5,
  mu_rk = -0.05,
  mu_md = 0.5,
  mu_ct = -1.2,
  ch_mf = 0.1,
  ch_mf_sf = 0,
  ch_sf = 0.2,
  ch_mf_mh = 0,
  ch_sf_mh = 0,
  ch_mh = 0.1,
  ch_mf_lh = 0,
  ch_sf_lh = 0,
  ch_mh_lh = 0,
  ch_lh = 0.2,
  ch_mf_rk = 0,
  ch_sf_rk = 0,
  ch_mh_rk = 0,
  ch_lh_rk = 0,
  ch_rk = 0.01,
  ch_mf_md = 0,
  ch_sf_md = 0,
  ch_mh_md = 0,
  ch_lh_md = 0,
  ch_rk_md = 0,
  ch_md = 0.2,
  ch_mf_ct = 0,
  ch_sf_ct = 0,
  ch_mh_ct = 0,
  ch_lh_ct = 0,
  ch_rk_ct = 0,
  ch_md_ct = 0,
  ch_ct = 1,
  delta_asc2_age = 0,
  delta_asc2_female = 0,
  delta_asc2_educ = 0,
  delta_asc3_age = 0,
  delta_asc3_female = 0,
  delta_asc3_educ = 0,
  delta_mf_age = -0.01,
  delta_sf_age = -0.01,
  delta_mf_female = -0.18,
  delta_sf_female = -0.06,
  delta_mf_educ = -0.04,
  delta_sf_educ = -0.02,
  delta_mh_age = -0.01,
  delta_lh_age = -0.01,
  delta_mh_female = 0.01,
  delta_lh_female = 0.04,
  delta_mh_educ = 0.02,
  delta_lh_educ = 0.12,
  delta_rk_age = -0.01,
  delta_md_age = -0.01,
  delta_rk_female = 0.01,
  delta_md_female = 0.01,
  delta_rk_educ = 0.12,
  delta_md_educ = -0.01,
  delta_ct_age = 0.01,
  delta_ct_female = -0.01,
  delta_ct_educ = -0.01
)

apollo_fixed = c("asc_alt1")

## Define the random components ----
apollo_draws <- list(
  interDrawsType = "sobol",
  interNDraws = 2000,
  interUnifDraws = c(),
  interNormDraws = c(
    "draws_mf",
    "draws_sf",
    "draws_mh",
    "draws_lh",
    "draws_rk",
    "draws_md",
    "draws_ct"
  ),
  intraDrawsType = "sobol",
  intraNDraws = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
  randcoeff <- list(
    r_mf = mu_mf + ch_mf * draws_mf + delta_mf_age * age + delta_mf_female * female + delta_mf_educ * education,
    r_sf = mu_sf + ch_mf_sf * draws_mf + ch_sf * draws_sf + delta_sf_age * age + delta_sf_female * female + delta_sf_educ * education,
    r_mh = mu_mh + ch_mf_mh * draws_mf + ch_sf_mh * draws_sf + ch_mh * draws_mh + delta_mh_age * age + delta_mh_female * female + delta_mh_educ * education,
    r_lh = mu_lh + ch_mf_lh * draws_mf + ch_sf_lh * draws_sf + ch_mh_lh * draws_mh + ch_lh * draws_lh + delta_lh_age * age + delta_lh_female * female + delta_lh_educ * education,
    r_rk = mu_rk + ch_mf_rk * draws_mf + ch_sf_rk * draws_sf + ch_mh_rk * draws_mh + ch_lh_rk * draws_lh  + ch_rk * draws_rk + delta_rk_age * age + delta_rk_female * female + delta_rk_educ * education,
    r_md = mu_md + ch_mf_md * draws_mf + ch_sf_md * draws_sf + ch_mh_md * draws_mh + ch_lh_md * draws_lh + ch_rk_md * draws_rk + ch_md * draws_md + delta_md_age * age + delta_md_female * female + delta_md_educ * education,
    r_ct = -exp(mu_ct + ch_mf_ct * draws_mf + ch_sf_ct * draws_sf + ch_mh_ct * draws_mh + ch_lh_ct * draws_lh + ch_rk_ct * draws_rk + ch_md_ct * draws_md + ch_ct * draws_ct + delta_ct_age * age + delta_ct_female * female + delta_ct_educ * education)
  )

  return(
    randcoeff
  )
}

## Group and validate inputs ----
apollo_inputs <- apollo_validateInputs()

## Define the model and likelihood function ----
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  ## Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  # Define the list of utility functions
  V <- list(
    alt1 = (
      asc_alt1 +
        r_mf * alt1_farm2 +
        r_sf * alt1_farm3 +
        r_mh * alt1_height2 +
        r_lh * alt1_height3 +
        r_rk * alt1_redkite +
        r_md * alt1_distance +
        r_ct * alt1_cost
    ),
    alt2 = (
      asc_alt2 +
        delta_asc2_age * age +
        delta_asc2_female * female +
        delta_asc2_educ * education +
        r_mf * alt2_farm2 +
        r_sf * alt2_farm3 +
        r_mh * alt2_height2 +
        r_lh * alt2_height3 +
        r_rk * alt2_redkite +
        r_md * alt2_distance +
        r_ct * alt2_cost
    ),
    alt3 = (
      asc_alt3 +
        delta_asc3_age * age +
        delta_asc3_female * female +
        delta_asc3_educ * education +
        r_mf * alt3_farm2 +
        r_sf * alt3_farm3 +
        r_mh * alt3_height2 +
        r_lh * alt3_height3 +
        r_rk * alt3_redkite +
        r_md * alt3_distance +
        r_ct * alt3_cost
    )
  )

  # Define settings for MNL model component
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar = choice,
    V = V
  )

  # Calculate the probabilities
  P <- list(
    model = apollo_mnl(mnl_settings, functionality)
  )

  # Take the product across observations
  P <- apollo_panelProd(P, apollo_inputs, functionality)

  # Average across inter-individual draws
  P <- apollo_avgInterDraws(P, apollo_inputs, functionality)

  # Prepare and return the outputs
  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  # Return the probabilities
  return(
    P
  )
}

## Estimate the model  ----
model <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(
    writeIter = FALSE,
    silent = FALSE,
    estimationRoutine = "bgw"
  )
)

## Print model output to console ----
apollo_modelOutput(
  model,
  modelOutput_settings = list(
    printOutliers = 10
  )
)


## Create the variance-covariance matrix ----

# Create a zero matrix
gamma <- matrix(0, nrow = 7, ncol = 7)

# Given how we have stored the parameters, we can fill the upper triangular of
# a zero matrix.
cholesky_elements <- model$estimate[str_detect(names(model$estimate), "ch_.*")]
gamma[upper.tri(gamma, diag = TRUE)] <- cholesky_elements

# Variance covariance of the betas
vcov_beta <- t(gamma) %*% gamma

# Correlation of the betas
corr_beta <- cov2cor(vcov_beta)

## Standard deviation of the random parameters ----
deltaMethod_settings <-
  list(
    expression = c(
      rob_se_ch_mf = "sqrt(ch_mf^2)",
      rob_se_ch_sf = "sqrt(ch_mf_sf^2 + ch_sf^2)",
      rob_se_ch_mh = "sqrt(ch_mf_mh^2 + ch_sf_mh^2 + ch_mh^2)",
      rob_se_ch_lh = "sqrt(ch_mf_lh^2 + ch_sf_lh^2 + ch_mh_lh^2 + ch_lh^2)",
      rob_se_ch_rk = "sqrt(ch_mf_rk^2 + ch_sf_rk^2 + ch_mh_rk^2 + ch_lh_rk^2 +ch_rk^2)",
      rob_se_ch_md = "sqrt(ch_mf_md^2 + ch_sf_md^2 + ch_mh_md^2 + ch_lh_md^2 +ch_rk_md^2 +ch_md^2)",
      rob_se_ch_ct = "sqrt(ch_mf_ct^2 + ch_sf_ct^2 + ch_mh_ct^2 + ch_lh_ct^2 +ch_rk_ct^2 +ch_md_ct^2 + ch_ct^2)"
    ),
    varcov="robust"
  )

est_sd <- apollo_deltaMethod(model, deltaMethod_settings)



# Table of outputs ----
tibble(
  attribute = c("Medium farms", "Small farms", "Medium turbines", "Low turbines", "Red kite", "Minimum distance", "Cost"),
  mean = model$estimate[str_detect(names(model$estimate), "mu_.*")],
  rob_se_mean = model$robse[str_detect(names(model$robse), "mu_.*")],
  std_dev = est_sd[, 2],
  rob_se_std_dev = est_sd[, 3]
) |>
  gt(
    rowname_col = "attribute"
  ) |>
  tab_header(
    title = "Estimated mean and standard deviation of the random parameters"
  ) |>
  fmt_number(
    columns = c(mean, rob_se_mean, std_dev, rob_se_std_dev),
    decimals = 3
  ) |>
  tab_footnote(
    "Standard errors are robust and calculated using the delta method."
  )

# Random parameter logit in WTP space ----

## Define the random components ----
apollo_draws <- list(
  interDrawsType = "sobol",
  interNDraws = 2000,
  interUnifDraws = c(),
  interNormDraws = c(
    "draws_mf",
    "draws_sf",
    "draws_mh",
    "draws_lh",
    "draws_rk",
    "draws_md",
    "draws_ct"
  ),
  intraDrawsType = "sobol",
  intraNDraws = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
  randcoeff <- list(
    r_mf = mu_mf + ch_mf * draws_mf + delta_mf_age * age + delta_mf_female * female + delta_mf_educ * education,
    r_sf = mu_sf + ch_mf_sf * draws_mf + ch_sf * draws_sf + delta_sf_age * age + delta_sf_female * female + delta_sf_educ * education,
    r_mh = mu_mh + ch_mf_mh * draws_mf + ch_sf_mh * draws_sf + ch_mh * draws_mh + delta_mh_age * age + delta_mh_female * female + delta_mh_educ * education,
    r_lh = mu_lh + ch_mf_lh * draws_mf + ch_sf_lh * draws_sf + ch_mh_lh * draws_mh + ch_lh * draws_lh + delta_lh_age * age + delta_lh_female * female + delta_lh_educ * education,
    r_rk = mu_rk + ch_mf_rk * draws_mf + ch_sf_rk * draws_sf + ch_mh_rk * draws_mh + ch_lh_rk * draws_lh  + ch_rk * draws_rk + delta_rk_age * age + delta_rk_female * female + delta_rk_educ * education,
    r_md = mu_md + ch_mf_md * draws_mf + ch_sf_md * draws_sf + ch_mh_md * draws_mh + ch_lh_md * draws_lh + ch_rk_md * draws_rk + ch_md * draws_md + delta_md_age * age + delta_md_female * female + delta_md_educ * education,
    r_ct = exp(mu_ct + ch_mf_ct * draws_mf + ch_sf_ct * draws_sf + ch_mh_ct * draws_mh + ch_lh_ct * draws_lh + ch_rk_ct * draws_rk + ch_md_ct * draws_md + ch_ct * draws_ct + delta_ct_age * age + delta_ct_female * female + delta_ct_educ * education)
  )

  return(
    randcoeff
  )
}

## Group and validate inputs ----
apollo_inputs <- apollo_validateInputs()

## Define the model and likelihood function ----
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  ## Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  # Define the list of utility functions
  V <- list(
    alt1 = (
      asc_alt1 +
        r_ct * (
          r_mf * alt1_farm2 +
          r_sf * alt1_farm3 +
          r_mh * alt1_height2 +
          r_lh * alt1_height3 +
          r_rk * alt1_redkite +
          r_md * alt1_distance -
          alt1_cost
        )
    ),
    alt2 = (
      asc_alt2 +
        delta_asc2_age * age +
        delta_asc2_female * female +
        delta_asc2_educ * education +
        r_ct * (
          r_mf * alt2_farm2 +
          r_sf * alt2_farm3 +
          r_mh * alt2_height2 +
          r_lh * alt2_height3 +
          r_rk * alt2_redkite +
          r_md * alt2_distance -
          alt2_cost
        )
    ),
    alt3 = (
      asc_alt3 +
        delta_asc3_age * age +
        delta_asc3_female * female +
        delta_asc3_educ * education +
        r_ct * (
          r_mf * alt3_farm2 +
          r_sf * alt3_farm3 +
          r_mh * alt3_height2 +
          r_lh * alt3_height3 +
          r_rk * alt3_redkite +
          r_md * alt3_distance -
          alt3_cost
        )
    )
  )

  # Define settings for MNL model component
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar = choice,
    V = V
  )

  # Calculate the probabilities
  P <- list(
    model = apollo_mnl(mnl_settings, functionality)
  )

  # Take the product across observations
  P <- apollo_panelProd(P, apollo_inputs, functionality)

  # Average across inter-individual draws
  P <- apollo_avgInterDraws(P, apollo_inputs, functionality)

  # Prepare and return the outputs
  P <- apollo_prepareProb(P, apollo_inputs, functionality)

  # Return the probabilities
  return(
    P
  )
}


## Estimate the model  ----
model <- apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(
    writeIter = FALSE,
    silent = FALSE,
    estimationRoutine = "bgw"
  )
)



## Create the variance-covariance matrix ----

# Create a zero matrix
gamma <- matrix(0, nrow = 7, ncol = 7)

# Given how we have stored the parameters, we can fill the upper triangular of
# a zero matrix.
cholesky_elements <- model$estimate[str_detect(names(model$estimate), "ch_.*")]
gamma[upper.tri(gamma, diag = TRUE)] <- cholesky_elements

# Variance covariance of the betas
vcov_beta <- t(gamma) %*% gamma

# Correlation of the betas
corr_beta <- cov2cor(vcov_beta)


## Standard deviation of the random parameters ----
deltaMethod_settings <-
  list(
    expression = c(
      rob_se_ch_mf = "sqrt(ch_mf^2)",
      rob_se_ch_sf = "sqrt(ch_mf_sf^2 + ch_sf^2)",
      rob_se_ch_mh = "sqrt(ch_mf_mh^2 + ch_sf_mh^2 + ch_mh^2)",
      rob_se_ch_lh = "sqrt(ch_mf_lh^2 + ch_sf_lh^2 + ch_mh_lh^2 + ch_lh^2)",
      rob_se_ch_rk = "sqrt(ch_mf_rk^2 + ch_sf_rk^2 + ch_mh_rk^2 + ch_lh_rk^2 +ch_rk^2)",
      rob_se_ch_md = "sqrt(ch_mf_md^2 + ch_sf_md^2 + ch_mh_md^2 + ch_lh_md^2 +ch_rk_md^2 +ch_md^2)",
      rob_se_ch_ct = "sqrt(ch_mf_ct^2 + ch_sf_ct^2 + ch_mh_ct^2 + ch_lh_ct^2 +ch_rk_ct^2 +ch_md_ct^2 + ch_ct^2)"
    ),
    varcov="robust"
  )

est_sd <- apollo_deltaMethod(model, deltaMethod_settings)

## Table of outputs ----
tibble(
  attribute = c("Medium farms", "Small farms", "Medium turbines", "Low turbines", "Red kite", "Minimum distance", "Cost"),
  mean = model$estimate[str_detect(names(model$estimate), "mu_.*")],
  rob_se_mean = model$robse[str_detect(names(model$robse), "mu_.*")],
  std_dev = est_sd[, 2],
  rob_se_std_dev = est_sd[, 3]
) |>
  gt(
    rowname_col = "attribute"
  ) |>
  tab_header(
    title = "Estimated mean and standard deviation of the random parameters"
  ) |>
  fmt_number(
    columns = c(mean, rob_se_mean, std_dev, rob_se_std_dev),
    decimals = 3
  ) |>
  tab_footnote(
    "Standard errors are robust and calculated using the delta method."
  )

# Latent class model ----
## Starting values ----
apollo_beta <- c(
  cl1_asc_alt1 = 0,
  cl1_asc_alt2 = 0.5,
  cl1_asc_alt3 = 0.5,
  cl1_b_medium_farms = 0.25,
  cl1_b_small_farms = 0.5,
  cl1_b_medium_height = 0.5,
  cl1_b_low_height = 1,
  cl1_b_red_kite = -0.05,
  cl1_b_min_distance = 0.5,
  cl1_b_cost = -0.9,
  cl2_asc_alt1 = 0,
  cl2_asc_alt2 = 0.5,
  cl2_asc_alt3 = 0.5,
  cl2_b_medium_farms = -0.5,
  cl2_b_small_farms = 0.2,
  cl2_b_medium_height = -0.7,
  cl2_b_low_height = 0.5,
  cl2_b_red_kite = -0.07,
  cl2_b_min_distance = -0.2,
  cl2_b_cost = -0.3,
  cl1_cst_alloc_fun = 0,
  cl1_b_age = 0,
  cl1_b_female = 0,
  cl1_b_educ = 0,
  cl2_cst_alloc_fun = -1.5,
  cl2_b_age = 0.55,
  cl2_b_female = 1.5,
  cl2_b_educ = 2.25
)

# Fixed
apollo_fixed = c(
  "cl1_asc_alt1",
  "cl2_asc_alt1",
  "cl1_cst_alloc_fun",
  "cl1_b_age",
  "cl1_b_female",
  "cl1_b_educ"
)

## Define the latent class components ----
apollo_lcPars <- function(apollo_beta, apollo_inputs) {

  lcpars = list(
    asc_alt1 = list(cl1_asc_alt1, cl2_asc_alt1),
    asc_alt2 = list(cl1_asc_alt2, cl2_asc_alt2),
    asc_alt3 = list(cl1_asc_alt3, cl2_asc_alt3),
    b_medium_farms = list(cl1_b_medium_farms, cl2_b_medium_farms),
    b_small_farms = list(cl1_b_small_farms, cl2_b_small_farms),
    b_medium_height = list(cl1_b_medium_height, cl2_b_medium_height),
    b_low_height = list(cl1_b_low_height, cl2_b_low_height),
    b_red_kite = list(cl1_b_red_kite, cl2_b_red_kite),
    b_min_distance = list(cl1_b_min_distance, cl2_b_min_distance),
    b_cost = list(cl1_b_cost, cl2_b_cost)
  )

  ### Utilities of class allocation model
  V=list(
    class_a = cl1_cst_alloc_fun  + cl1_b_age * age + cl1_b_female * female + cl1_b_educ * education,
    class_b = cl2_cst_alloc_fun  + cl2_b_age * age + cl2_b_female * female + cl2_b_educ * education
  )

  ### Settings for class allocation models
  classAlloc_settings = list(
    classes = c(class_a = 1, class_b = 2),
    utilities = V
  )

  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)

  return(lcpars)
}


## Group and validate inputs ----
apollo_inputs <- apollo_validateInputs()


## Define the model and likelihood function ----
apollo_probabilities <- function(
    apollo_beta,
    apollo_inputs,
    functionality = "estimate"
  ) {

  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P <- list()

  ### Define settings for MNL model component that are generic across classes
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar = choice
  )

  ### Loop over classes
  for (s in 1:length(pi_values)) {
    ### Compute class-specific utilities
    V <- list(
      alt1 = (
        asc_alt1[[s]] +
        b_medium_farms[[s]] * alt1_farm2 +
        b_small_farms[[s]] * alt1_farm3 +
        b_medium_height[[s]] * alt1_height2 +
        b_low_height[[s]] * alt1_height3 +
        b_red_kite[[s]] * alt1_redkite +
        b_min_distance[[s]] * alt1_distance +
        b_cost[[s]] * alt1_cost
      ),
      alt2 = (
        asc_alt2[[s]] +
        b_medium_farms[[s]] * alt2_farm2 +
        b_small_farms[[s]] * alt2_farm3 +
        b_medium_height[[s]] * alt2_height2 +
        b_low_height[[s]] * alt2_height3 +
        b_red_kite[[s]] * alt2_redkite +
        b_min_distance[[s]] * alt2_distance +
        b_cost[[s]] * alt2_cost
      ),
      alt3 = (
        asc_alt3[[s]] +
        b_medium_farms[[s]] * alt3_farm2 +
        b_small_farms[[s]] * alt3_farm3 +
        b_medium_height[[s]] * alt3_height2 +
        b_low_height[[s]] * alt3_height3 +
        b_red_kite[[s]] * alt3_redkite +
        b_min_distance[[s]] * alt3_distance +
        b_cost[[s]] * alt3_cost
      )
    )

    mnl_settings$utilities <- V
    mnl_settings$componentName <- paste0("Class_", s)

    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_", s)]] <- apollo_mnl(mnl_settings, functionality)

    ### Take product across observation for same individual
    P[[paste0("Class_", s)]] <- apollo_panelProd(
      P[[paste0("Class_", s)]],
      apollo_inputs,
      functionality
    )

  }

  ### Compute latent class model probabilities
  lc_settings  <- list(inClassProb = P, classProb = pi_values)
  P[["model"]] <- apollo_lc(lc_settings, apollo_inputs, functionality)

  ### Prepare and return outputs of function
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


## Estimate the model ----
model = apollo_estimate(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  estimate_settings = list(
    writeIter = FALSE,
    silent    = FALSE,
    iterMax   = 500,
    estimationRoutine = "bgw"
    )
  )


model <- apollo_lcEM(
  apollo_beta,
  apollo_fixed,
  apollo_probabilities,
  apollo_inputs,
  lcEM_settings = list(
    EMmaxIterations = 500
    )
  )
