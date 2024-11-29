# Load packages ----
library(tidyverse)
library(janitor)
library(apollo)
library(gt)


# MNL ----
model <- readRDS(gzcon(url("https://raw.githubusercontent.com/edsandorf/evdce/refs/heads/main/Models/mnl.rds")))

## Hidden chunk ----
v1 <- sum(
  c(
    mean(model$estimate[2:3]),
    model$estimate[c(5, 7)],
    model$estimate[8:10]
    ) * c(1, 1, 1, -5, 1, 4.5)
  )

betas <- format(round(as.vector(model$estimate), 3), nsmall = 3)
betas <- c(betas, format(round(as.vector(mean(model$estimate[2:3])), 3), nsmall = 3))
betas <- gsub(" ", "", betas)

## Print coefficient table ----
tibble(
  var = names(model$estimate),
  estimate = model$estimate,
  std_err = model$robse,
  t_stat = estimate/std_err,
  p_value = 2 * pt(abs(t_stat), df = (model$nObs - model$numParams), lower.tail = FALSE)
) |>
  gt(
    rowname_col = "var",
    caption = "Parameter estimates from the MNL model"
  ) |>
  fmt_number(
    columns = c(estimate, std_err, t_stat, p_value),
    decimals = 3
  )

## Calculate standard errors using the delta method ----
deltaMethod_settings <- list(
  expression = c(
    wtp_medium_farms = "-b_medium_farms / b_cost",
                 wtp_small_farms = "-b_small_farms / b_cost",
                 wtp_medium_height = "-b_medium_height / b_cost",
                 wtp_low_height = "-b_low_height / b_cost",
                 wtp_red_kite = "-b_red_kite / b_cost",
                 wtp_min_distance = "-b_min_distance / b_cost"
    )
  )

# Apply the Delta method to calculate the standard errors of marginal WTP
wtp_results_delta <- apollo_deltaMethod(model, deltaMethod_settings)

# Calculate the WTP values and CIs
wtp_results_delta <- wtp_results_delta |>
  clean_names() |>
  mutate(
    lower_ci = value - qnorm(0.975) * s_e,
    upper_ci = value + qnorm(0.975) * s_e
  )

## Plot the sampling distribution of WTP ----
wtp_results_delta |>
  group_by(expression) |>
  nest() |>
  mutate(
    wtp = map(data, ~ rnorm(10000, .x$value, .x$s_e)),
  ) |>
  unnest(cols = c(data, wtp)) |>
  ggplot() +
  geom_density(mapping = aes(x = wtp)) +
  geom_vline(data = wtp_results_delta, mapping = aes(xintercept = value)) +
  geom_vline(data = wtp_results_delta, mapping = aes(xintercept = lower_ci), lty = 2) +
  geom_vline(data = wtp_results_delta, mapping = aes(xintercept = upper_ci), lty = 2) +
  facet_wrap(vars(expression), nrow = 2, scales = "free") +
  theme_bw()

## Table with CIs ----
wtp_results_delta |>
  gt(
    rowname_col = "expression",
    caption = "WTP estimates and 95% confidence intervals using the Delta method"
  ) |>
  fmt_number(
    decimals = 3
  )

## Welfare change ----
deltaMethod_settings <- list(
  expression = c(
    welf_change = "-((b_asc_alt2 + b_asc_alt3)/2 +
                      b_small_farms + b_low_height + b_red_kite * -5 + b_min_distance +
                      b_cost * 4.5) / b_cost"
  )
)

# Apply the Delta method to calculate the standard errors of the welfare change
welf_results_delta <- apollo_deltaMethod(model, deltaMethod_settings) |>
  clean_names() |>
  mutate(
    lower_ci = value - qnorm(0.975) * s_e,
    upper_ci = value + qnorm(0.975) * s_e
  )

welf_results_delta |>
  gt(
    rowname_col = "expression",
    caption = "Welfare estimates and 95% confidence intervals using the Delta method"
  ) |>
  fmt_number(
    decimals = 3
  )


## Krinsky-Robb ----
simulate_dist <- function(model, nsim){
  # Initialize matrix X to store simulated parameter estimates
  X <- matrix(model$estimate, nrow = nsim, ncol = length(model$estimate),
              byrow = TRUE, dimnames = list(NULL, names(model$estimate)))

  # Extract means and variance-covariance matrix for multivariate normal distribution
  mu <- model$estimate[!is.na(model$robse)]
  sigma <- model$robvarcov

  # Cholesky decomposition of the variance-covariance matrix
  L <- chol(sigma)

  # Generate matrix of standard normal random variables
  Z <- matrix(rnorm(nsim * model$numParams), ncol = model$numParams)

  # Transform standard normal variables to multivariate normal and update matrix X
  X[, !is.na(model$robse)] <- t(mu + L %*% t(Z))

  # Return the matrix 'X', which contains the simulated parameter estimates
  return(X)
}

# Generate 10,000 simulated distributions
sim_dists <- simulate_dist(model, 10000)

# Calculate the simulated marginal WTP for small wind farms
sim_wtp <- -sim_dists[, "b_small_farms"] / sim_dists[, "b_cost"]

# 95% confidence interval the simulated distribution
quantile(sim_wtp, c(0.025, 0.975))

## Change in consumer surplus ----
# Compute the simulated distribution for V1
sim_V1 <- rowMeans(sim_dists[, c("b_asc_alt2", "b_asc_alt3")]) +
  sim_dists[, "b_small_farms"] +
  sim_dists[, "b_low_height"] +
  sim_dists[, "b_red_kite"] * -5 +
  sim_dists[, "b_min_distance"] +
  sim_dists[, "b_cost"] * 4.5

# Calculate the simulated welfare change (V1 divided by negative cost)
sim_welf <- sim_V1 / -sim_dists[, "b_cost"]

# 95% confidence interval the simulated distribution
quantile(sim_welf, c(0.025, 0.975))

## Simulate WTP ----
sim_dists |>
  as_tibble() |>
  pivot_longer(-b_cost, names_to = "attribute", values_to = "wtp") |>
  group_by(attribute) |>
  mutate(
    wtp = wtp / -b_cost,
    lower_ci = quantile(wtp, 0.025),
    upper_ci = quantile(wtp, 0.975),
    mean = mean(wtp),
    fill_color = case_when(
      lower_ci < wtp ~ rgb(1, 0.5, 0.5),
      upper_ci > wtp ~ rgb(1, 0.5, 0.5),
      TRUE ~ grey(0.95)
    )
  ) |>
  filter(!(attribute %in% c("b_cost", "b_asc_alt1"))) |>
  ggplot() +
  geom_histogram(mapping = aes(x = wtp)) +
  # scale_fill_manual(values = fill_color) +
  facet_wrap(vars(attribute), scales = "free") +
  theme_bw()

### HIDDEN SIM WTP CHUNK ----
col1 <- rgb(1, 0.5, 0.5)
col2 <- grey(0.95)
col3 <- rgb(0.5, 1, 0.5)

sim.dist <- sim_wtp

mean.x <- -model$estimate["b_small_farms"]/model$estimate[10]
brks <- seq(quantile(sim.dist, 0.025), quantile(sim.dist, 0.975), length = 50)
brks <- c(brks[1] - (brks[2]-brks[1]) * rev(1:1000), brks)
brks <- c(brks, brks[length(brks)] + (brks[2]-brks[1]) * 1:1000)

brks <- brks[brks >= (min(sim.dist) - (brks[2]-brks[1]))]
brks <- brks[brks <= (max(sim.dist) + (brks[2]-brks[1]))]

cols <- c(rep(col1, sum(brks < quantile(sim.dist, 0.025))),
          rep(col2, (length(brks)) - (sum(brks < quantile(sim.dist, 0.025)) + sum(brks >= quantile(sim.dist, 0.975)))),
          rep(col1, sum(brks >= quantile(sim.dist, 0.975))))
h.hist <- hist(sim.dist, breaks = brks, plot = FALSE)
cols <- cols[h.hist$counts > 0]

xx <- c(quantile(sim.dist, c(0.025, 0.975)), mean.x)

tibble(sim.dist = sim.dist) |>
  ggplot(aes(x = sim.dist)) +
  geom_histogram(breaks = brks, color = 1, lwd = 0.5, aes(fill = cut(sim.dist, breaks = brks))) +
  scale_fill_manual(values = cols) +
  geom_vline(xintercept = xx, color = c(rep(col1, 2), col3), lwd = 1) +
  xlab("Marginal WTP (€ per month)") +
  ylab("Frequency") +
  theme_bw() +
  theme(legend.position = "none")

### HIDDEN SIM WELFARE CHUNK ----
sim.dist <- sim_welf

mean.x <- -v1/model$estimate[10]
brks <- seq(quantile(sim.dist, 0.025), quantile(sim.dist, 0.975), length = 50)
brks <- c(brks[1] - (brks[2]-brks[1]) * rev(1:1000), brks)
brks <- c(brks, brks[length(brks)] + (brks[2]-brks[1]) * 1:1000)

brks <- brks[brks >= (min(sim.dist) - (brks[2]-brks[1]))]
brks <- brks[brks <= (max(sim.dist) + (brks[2]-brks[1]))]

cols <- c(rep(col1, sum(brks < quantile(sim.dist, 0.025))),
          rep(col2, (length(brks)) - (sum(brks < quantile(sim.dist, 0.025)) + sum(brks >= quantile(sim.dist, 0.975)))),
          rep(col1, sum(brks >= quantile(sim.dist, 0.975))))
h.hist <- hist(sim.dist, breaks = brks, plot = FALSE)
cols <- cols[h.hist$counts > 0]

xx <- c(quantile(sim.dist, c(0.025, 0.975)), mean.x)
tibble(sim.dist = sim.dist) |>
  ggplot(aes(x = sim.dist)) +
  geom_histogram(breaks = brks, color = 1, lwd = 0.5, aes(fill = cut(sim.dist, breaks = brks))) +
  scale_fill_manual(values = cols) +
  geom_vline(xintercept = xx, color = c(rep(col1, 2), col3), lwd = 1) +
  xlab("Expected change in consumer surplus (€ per month)") +
  ylab("Frequency") +
  theme_bw() +
  theme(legend.position = "none")


## Create a table of simulated WTPs ----
sim_dists |>
  as_tibble() |>
  pivot_longer(-b_cost, names_to = "attribute", values_to = "wtp") |>
  group_by(attribute) |>
  mutate(
    wtp = wtp / -b_cost,

  ) |>
  summarize(
    mean = mean(wtp),
    lower_ci = quantile(wtp, 0.025),
    upper_ci = quantile(wtp, 0.975)
  ) |>
  filter(!(attribute %in% c("b_cost", "b_asc_alt1"))) |>
  slice(1, 2, 8, 4, 3, 5, 6, 7) |>
  gt() |>
  fmt_number()

## Simulate welfare changes ----
costs <- seq(0, 7.5, by = 0.01)

# Compute the mean welfare changes
welf <- (mean(model$estimate[c("b_asc_alt2", "b_asc_alt3")]) +
           model$estimate["b_small_farms"] +
           model$estimate["b_low_height"] +
           model$estimate["b_red_kite"] * -5 +
           model$estimate["b_min_distance"] +
           model$estimate["b_cost"] * costs) / -model$estimate["b_cost"]

# Compute the simulated welfare changes
sim_welf <- (rowMeans(sim_dists[, c("b_asc_alt2", "b_asc_alt3")]) +
               sim_dists[, "b_small_farms"] +
               sim_dists[, "b_low_height"] +
               sim_dists[, "b_red_kite"] * -5 +
               sim_dists[, "b_min_distance"] + sim_dists[, "b_cost"] %*% t(costs)) / -sim_dists[, "b_cost"]

# Plot the prediction (with confidence interval envelope)
tibble(
  costs = costs,
  welf = welf,
  lower = apply(sim_welf, 2, quantile, probs = 0.025),
  upper = apply(sim_welf, 2, quantile, probs = 0.975)
) |>
  ggplot(aes(x = costs)) +
  geom_line(aes(y = welf)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "red") +
  labs(
    x = "Policy cost (€ per month)",
    y = "Expected change in consumer surplus (€ per month)",
    title = "Welfare Changes with Confidence Interval"
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = max(costs[apply(sim_welf, 2, quantile, probs = 0.025) > 0]), linetype = "dashed") +
  geom_vline(xintercept = min(costs[apply(sim_welf, 2, quantile, probs = 0.975) < 0]), linetype = "dashed") +
  theme_bw()





# MNL - Observed heterogeneity ----
model <- readRDS(gzcon(url("https://raw.githubusercontent.com/edsandorf/evdce/refs/heads/main/Models/mnl-socdem-asc.rds")))

database <- read_csv(gzcon(url("https://raw.githubusercontent.com/edsandorf/evdce/refs/heads/main/Data/data-windmills.csv"))) |>
  clean_names() |>
  filter(!is.na(choice))

# Create a data frame with the desired socio-demographic combinations
soc_dem <- expand.grid(
  age = 18:89,
  female = 0:1,
  education = 1:3
)

# Calculate b_small_farms and b_cost
b_small_farms <- model$estimate["b_small_farms"] +
  colSums(t(soc_dem) * model$estimate[c("delta_sf_age", "delta_sf_female", "delta_sf_educ")])

b_cost <- model$estimate["b_cost"] +
  colSums(t(soc_dem) * model$estimate[c("delta_ct_age", "delta_ct_female", "delta_ct_educ")])

# Calculate marginal WTP
wtp <- -b_small_farms / b_cost


## Plot marginal WTP by age holding all else constant ----
soc_dem |>
  mutate(wtp)  |>
  group_by(age) |>
  summarise(
    mean_wtp = mean(wtp)
  ) |>
  ungroup() |>
  ggplot(aes(x = age, y = mean_wtp)) +
  geom_line() +
  labs(
    x = "Age (in years)",
    y = "Marginal WTP (€ per month)",
  ) +
  theme_bw()

## Tabulate marginal WTP by gender holding all else constant ----
soc_dem |>
  mutate(
    wtp = wtp
  )  |>
  group_by(female) |>
  summarise(
    mean_marginal_wtp = mean(wtp)) |>
  ungroup() |>
  gt() |>
  fmt_number(
    columns = mean_marginal_wtp,
  )

## Tabulate marginal WTP by educational attainment holding all else constant ----
soc_dem |>
  mutate(
    wtp = wtp
  )  |>
  group_by(education) |>
  summarise(
    mean_marginal_wtp = mean(wtp)) |>
  ungroup() |>
  gt() |>
  fmt_number(
    columns = mean_marginal_wtp,
  )


## Creating a count for each socio-demographic combination ----

counts <- database |>
  select(id_individual, age, female, education) |>
  distinct() |>
  count(age, female, education) |>
  full_join(soc_dem, by = c("age", "female", "education")) |>
  replace_na(
    list(
      n = 0
    )
  ) |>
  arrange(education, female, age) |>
  pull(n)


# Generate the marginal WTP distribution using the count for each socio-demographic combination
tibble(
  wtp = rep(wtp, times = counts)
) |>
  ggplot(aes(x = wtp)) +
  geom_histogram(binwidth = 0.01, color = 1, fill = "red") +
  labs(
    x = "Marginal WTP (€ per month)",
    y = "Frequency",
    title = "Marginal WTP Distribution by Socio-demographic Characteristics"
  ) +
  theme_bw()

## Delta method ----
deltaMethod_settings <- list(
  expression = c(
    wtp_lowest = "-(b_small_farms + delta_sf_age * 80 + delta_sf_female * 0 + delta_sf_educ * 3) /
                   (b_cost + delta_ct_age * 80 + delta_ct_female * 0 + delta_ct_educ * 3)",
    wtp_highest = "-(b_small_farms + delta_sf_age * 20 + delta_sf_female * 0 + delta_sf_educ * 1) /
                   (b_cost + delta_ct_age * 20 + delta_ct_female * 0 + delta_ct_educ * 1)",
    wtp_diff = "(-(b_small_farms + delta_sf_age * 20 + delta_sf_female * 0 + delta_sf_educ * 1) /
                   (b_cost + delta_ct_age * 20 + delta_ct_female * 0 + delta_ct_educ * 1)) -
                 -(b_small_farms + delta_sf_age * 80 + delta_sf_female * 0 + delta_sf_educ * 3) /
                   (b_cost + delta_ct_age * 80 + delta_ct_female * 0 + delta_ct_educ * 3)"
  )
)

# Apply the Delta method to calculate the standard errors of marginal WTP
wtp_results_delta_dif <- apollo_deltaMethod(model, deltaMethod_settings)

# Calculate the WTP values and CIs
wtp_results_delta_dif <- wtp_results_delta_dif |>
  clean_names() |>
  mutate(
    lower_ci = value - qnorm(0.975) * s_e,
    upper_ci = value + qnorm(0.975) * s_e
  )

wtp_results_delta_dif |>
  gt() |>
  fmt_number()

#### HIDDDEN CODE: MNL SOCIO CORR ----
soc_dem <- expand.grid(age = 18:89, female = 0:1, education = 1:3)

# Calculate b_small_farms and b_cost
b_medium_farms <- model$estimate["b_medium_farms"] +
  colSums(t(soc_dem) * model$estimate[c("delta_mf_age", "delta_mf_female", "delta_mf_educ")])
b_small_farms <- model$estimate["b_small_farms"] +
  colSums(t(soc_dem) * model$estimate[c("delta_sf_age", "delta_sf_female", "delta_sf_educ")])
b_medium_height <- model$estimate["b_medium_height"] +
  colSums(t(soc_dem) * model$estimate[c("delta_mh_age", "delta_mh_female", "delta_mh_educ")])
b_low_height <- model$estimate["b_low_height"] +
  colSums(t(soc_dem) * model$estimate[c("delta_lh_age", "delta_lh_female", "delta_lh_educ")])
b_red_kite <- model$estimate["b_red_kite"] +
  colSums(t(soc_dem) * model$estimate[c("delta_rk_age", "delta_rk_female", "delta_rk_educ")])
b_min_distance <- model$estimate["b_min_distance"] +
  colSums(t(soc_dem) * model$estimate[c("delta_md_age", "delta_md_female", "delta_md_educ")])
b_cost <- model$estimate["b_cost"] +
  colSums(t(soc_dem) * model$estimate[c("delta_ct_age", "delta_ct_female", "delta_ct_educ")])

# Calculate marginal WTP
wtp_medium_farms <- -b_medium_farms / b_cost
wtp_small_farms <- -b_small_farms / b_cost
wtp_medium_height <- -b_medium_height/ b_cost
wtp_low_height <- -b_low_height / b_cost
wtp_red_kite <- -b_red_kite / b_cost
wtp_min_distance <- -b_min_distance / b_cost

wtp_samp_all <- cbind(wtp_medium_farms, wtp_small_farms,
                      wtp_medium_height,
                      wtp_low_height,
                      wtp_red_kite,
                      wtp_min_distance)


soc_dem_count <- soc_dem |>
  # Perform a left join with a summary table from the data
  left_join(
    # Create a summarised table from the data
    database |>
      # Group by the individual ID to ensure we only take one observation per individual
      group_by(id_individual) |>
      # Keep only the first row for each individual (this handles cases where individuals have multiple rows)
      slice(1) |>
      # Ungroup to remove individual-based grouping
      ungroup() |>
      # Select the columns that correspond to socio-demographic variables
      select(names(soc_dem)) |>
      # Group the data by the socio-demographic variables
      group_by(age, female, education) |>
      # Summarize the grouped data by counting the number of occurrences (i.e., number of individuals per group)
      summarise(count = n(), .groups = 'drop'), # Drop grouping after summarisation
    # Specify the join condition based on the socio-demographic variables
    by = c("age", "female", "education")
  ) |>
  # Replace any NA values in the count column with 0 (indicating no matches were found for that soc_dem combination)
  replace_na(list(count = 0)) |>
  # Select count and convert to vector
  pull(count)

# Generate the marginal WTP distribution using the count for each socio-demographic combination
wtp_samp_all <- wtp_samp_all[rep(1:nrow(wtp_samp_all), times = soc_dem_count), ]

# shorten names
colnames(wtp_samp_all) <- substring(colnames(wtp_samp_all), 5)

round(cor(wtp_samp_all), 3)

# RP - MIXL ----
model <- readRDS(gzcon(url("https://raw.githubusercontent.com/edsandorf/evdce/refs/heads/main/Models/rp-mxl-uncorrelated-asc.rds")))


#### HIDDEN CHUNK ----
# We need to include the model here to use the `apollo` functions
apollo_initialise()

apollo_control = list(
  modelName = "rpl-mixl-uncorrelated-asc",
  modelDescr = "RP-MIXL with uncorrelated alternative specific constants on the windmills dataset",
  indivID = "id_individual",
  nCores = 7
)

# Set the starting values of the parameters
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

# Define the random components
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

# Group and validate inputs
apollo_inputs <- apollo_validateInputs()

# Define the model and likelihood function
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

## Print coefficient table ----
tibble(
  var = names(model$estimate),
  estimate = model$estimate,
  std_err = model$robse,
  t_stat = estimate/std_err,
  p_value = 2 * pt(abs(t_stat), df = (model$nObs - model$numParams), lower.tail = FALSE)
) |>
  gt(
    rowname_col = "var",
    caption = "Parameter estimates from the RP-MIXL model"
  ) |>
  fmt_number(
    columns = c(estimate, std_err, t_stat, p_value),
    decimals = 3
  )

## Define the mean and standard deviation of the normal distribution ----
mean_value <- model$estimate["mu_lh"] +
  model$estimate["delta_lh_age"] * 40 +
  model$estimate["delta_lh_female"] * 0 +
  model$estimate["delta_lh_educ"] * 1

sd_value <- abs(model$estimate["sd_lh"])

## Create marginal utility plot ----

tibble(
  x_range = c(
    qnorm(pnorm(-3), mean = mean_value, sd = sd_value),
    qnorm(pnorm(3), mean = mean_value, sd = sd_value)
  )
) |>
  ggplot(mapping = aes(x = x_range)) +
  stat_function(fun = dnorm, args = list(mean = mean_value, sd = sd_value)) +
  labs(
    x = "Marginal utility (unconditional distribution)",
    y = "Density"
  ) +
  theme_bw()

## Unconditional distributions ----
unconditionals <- apollo_unconditionals(
  model,
  apollo_probabilities,
  apollo_inputs
)

## Conditional distributions ----
conditionals <- apollo_conditionals(
  model,
  apollo_probabilities,
  apollo_inputs
)

## Probabilities of choice sequences for every draw ----
P <- apollo_probabilities(
  model$estimate,
  apollo_inputs,
  functionality = "conditionals"
)

## Conditional weight for every draw ----
w <- P / rowSums(P)

## Retrieve the parameters of the individual-specific conditional distributions ----
conditionals_lh <- tibble(
  id_individual = unique(database$id_individual),
  post_mean = rowSums(w * unconditionals$r_lh),
  post_sd = sqrt(rowSums(w * (rowSums(w * unconditionals$r_lh) - unconditionals$r_lh)^2))
)

# Check that they are equivalent to those produced using apollo
all(round(conditionals_lh, 10) == round(conditionals$r_lh, 10))

## First 10 rows of the unconditionals ----
tibble(
  id = conditionals$r_lh[1:10, 1],
  post_mean = rnd(conditionals$r_lh[1:10, 2],10),
  post_sd = rnd(conditionals$r_lh[1:10, 3],10),
  id_individual = unlist(conditionals_lh[1:10, 1]),
  post_mean1 = rnd(unlist(conditionals_lh[1:10, 2]),10),
  post_sd1 = rnd(unlist(conditionals_lh[1:10, 3]),10)
) |>
  gt() |>
  cols_label(
    post_mean1 = "post_mean",
    post_sd1 = "post_sd"
  ) |>
  tab_spanner(
    label = "Generated using `apollo_conditionals`",
    columns = 1:3
  ) |>
  tab_spanner(
    label = "Generated using $w$",
    columns = 4:6
  )  |>
  tab_header(title = "First 10 rows of the conditional distributions")

## Graph of the unconditionals ----
conditionals_lh |>
  ggplot(aes(x = post_mean)) +
  geom_density() +
  labs(
    x = "Marginal utility for low turbines (conditional means)",
    y = "Density"
  ) +
  theme_bw()


## Kernel density means ----
#### HIDDEN CHUNK ----

conditionals_lh.temp <- conditionals_lh

conditionals_lh <- left_join(conditionals_lh,
                             database |>
                               group_by(id_individual) |>
                               slice(1) |>
                               ungroup() |>
                               select(c(id_individual, age, female, education)) ,
                             by = "id_individual")


cond.draws.subsamp <- t((unconditionals$r_lh)[as.vector(unlist(conditionals_lh[conditionals_lh$age == 40 &  conditionals_lh$female == 0 & conditionals_lh$education == 1, 1])), ])

w.subsamp <- t((w)[as.vector(unlist(conditionals_lh[conditionals_lh$age == 40 &  conditionals_lh$female == 0 & conditionals_lh$education == 1, 1])), ])

# set this back to what it was so it works with below chucnks
conditionals_lh <- conditionals_lh.temp

xx <- seq(mean_value - 3*sd_value, mean_value + 3*sd_value, by = 0.05)
mids <- (xx[-length(xx)] + xx[-1]) / 2
dens <- matrix(NA, length(mids), ncol(cond.draws.subsamp))
for(i in seq_along(dens[,1])){
  for(j in seq_along(dens[1,])){
    y1 <- cond.draws.subsamp[, j]
    w1 <- w.subsamp[, j]
    dens[i, j] <- sum(w1[y1 > xx[i] & y1 <= xx[i +1]])
  }
}

dens <- t(t(dens) * 0.96 / apply(dens, 2, max))

df <- tibble(id = as.factor(rep(1:ncol(dens), each = nrow(dens))),
             mids = rep(mids, ncol(dens)),
             dens = as.vector(dens))
ggplot(df, aes(x = mids, y = dens, colour = id)) +
  geom_line(linewidth = 1) +
  stat_function(fun = dnorm, args = list(mean = mean_value, sd = sd_value),
                aes(color = "Normal Distribution"), linewidth = 1, lty = 2) +
  # Primary y-axis label
  xlab("Marginal utility (individual-specific conditional distribution)") +
  ylab("Density") +
  # Secondary y-axis label
  #  scale_y_continuous(sec.axis = sec_axis(~ . * 1, name = "Density (unconditional distribution)")) +
  # Customize colors and theme
  # scale_color_manual(values = c("1" = "#e41a1c",
  #                               "2" = "#377eb8",
  #                               "3" = "#4daf4a",
  #                               "Normal Distribution" = "#984ea3"))
  scale_color_manual(values = c("1" = 1,
                                "2" = 1,
                                "3" = 1,
                                "Normal Distribution" = "#e41a1c")) +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks = element_blank(),   # Remove axis ticks
    legend.position = "none"        # Remove legend
  )



## Plotting the conditionals ----
conditionals_lh <- database |>
  select(id_individual, age, female, education) |>
  distinct() |>
  right_join(conditionals_lh, by = "id_individual")


conditionals_lh |>
  mutate(
    female = factor(female)
  ) |>
  ggplot(aes(x = post_mean, fill = female)) +
  geom_density(alpha = 0.4) +
  labs(
    x = "Marginal utility for low turbines (conditional means)",
    y = "Density"
  ) +
  theme_bw()


conditionals_lh |>
  mutate(
    education = factor(education)
  ) |>
  ggplot(aes(x = post_mean, fill = education)) +
  geom_density(alpha = 0.4) +
  labs(
    x = "Marginal utility for low turbines (conditional means)",
    y = "Density"
  ) +
  theme_bw()

## Plotting the conditionals posterior mean WTP ----

# Add the means of the conditional marginal WTP distributions to the tibble
conditionals_lh <- conditionals_lh |>
  mutate(
    post_mean_wtp = rowSums(w * (- unconditionals$r_lh / unconditionals$r_ct))
  )

conditionals_lh |>
  mutate(
    female = factor(female)
  ) |>
  ggplot(aes(x = post_mean_wtp, fill = female)) +
  geom_density(alpha = 0.4) +
  labs(
    x = "Marginal utility for low turbines (conditional means)",
    y = "Density"
  ) +
  theme_bw()

conditionals_lh |>
  mutate(
    education = factor(education)
  ) |>
  ggplot(aes(x = post_mean_wtp, fill = education)) +
  geom_density(alpha = 0.4) +
  labs(
    x = "Marginal utility for low turbines (conditional means)",
    y = "Density"
  ) +
  theme_bw()

## Poe-test ----
# Extract the mean of individual-specific marginal WTP distributions for females
wtp_female <- conditionals_lh |>
  filter(female == 1) |>
  select(post_mean) |>
  pull()

# Extract the mean of individual-specific marginal WTP distributions for males
wtp_male <- conditionals_lh |>
  filter(female == 0) |>
  select(post_mean) |>
  pull()

# Compare each element of wtp_female with all elements of wtp_male
condition <- rowSums(outer(wtp_female, wtp_male, `<`))

# To obtain the test statistic gamma, sum over the condition vector and weight
# by the inverse of the product of the lengths of the vectors
n_female <- length(wtp_female)
n_male <- length(wtp_male)
statistic <- sum(condition) / (n_female * n_male)

# Print the resulting test statistic
print(statistic)

## Simulation ----
# Extract the first row of untransformed draws (standard normals) for each individual
draws <- apollo_firstRow(apollo_inputs$draws, apollo_inputs)

# Extract socio-demographic variables for mean shifts
socio_demo <- apollo_firstRow(database, apollo_inputs)[, c("age", "female", "education")]

# Transform untransformed draws using model parameters (matches apollo_randCoeff, optimised for Krinsky-Robb simulations)
randCoeff <- function(params) {
  with(as.list(params), {
    list(
      r_mf = mu_mf + sd_mf * draws$draws_mf + delta_mf_age * socio_demo$age + delta_mf_female * socio_demo$female + delta_mf_educ * socio_demo$education,
      r_sf = mu_sf + sd_sf * draws$draws_sf + delta_sf_age * socio_demo$age + delta_sf_female * socio_demo$female + delta_sf_educ * socio_demo$education,
      r_mh = mu_mh + sd_mh * draws$draws_mh + delta_mh_age * socio_demo$age + delta_mh_female * socio_demo$female + delta_mh_educ * socio_demo$education,
      r_lh = mu_lh + sd_lh * draws$draws_lh + delta_lh_age * socio_demo$age + delta_lh_female * socio_demo$female + delta_lh_educ * socio_demo$education,
      r_rk = mu_rk + sd_rk * draws$draws_rk + delta_rk_age * socio_demo$age + delta_rk_female * socio_demo$female + delta_rk_educ * socio_demo$education,
      r_md = mu_md + sd_md * draws$draws_md + delta_md_age * socio_demo$age + delta_md_female * socio_demo$female + delta_md_educ * socio_demo$education,
      r_ct = -exp(mu_ct + sd_ct * draws$draws_ct + delta_ct_age * socio_demo$age + delta_ct_female * socio_demo$female + delta_ct_educ * socio_demo$education)
    )
  })
}

# Generate draws based on model estimates
randCoeff_model <- randCoeff(model$estimate)

# Calculate marginal WTP for variable of interest
wtp_lh_model <- -randCoeff_model$r_rk / randCoeff_model$r_ct

# Summarise predicted WTP distribution
wtp_lh_summary <- c(
  mean = mean(wtp_lh_model),
  sd = sd(wtp_lh_model),
  quantile(wtp_lh_model, c(0.1, 0.25, 0.5, 0.75, 0.9))
)

# Set number of Krinsky-Robb simulations
nsims <- 10000

# Generate empirical distributions for simulations
sim_dists <- simulate_dist(model, nsims)

# Summarise WTP distributions across simulations
wtp_lh_summaries <- t(apply(sim_dists, 1, function(row) {
  randCoeff_s <- randCoeff(row)
  wtp_lh <- -randCoeff_s$r_lh / randCoeff_s$r_ct

  c(
    mean = mean(wtp_lh),
    sd = sd(wtp_lh),
    quantile(wtp_lh, c(0.1, 0.25, 0.5, 0.75, 0.9))
  )
}))

# Combine predictive distribution and confidence intervals
wtp_lh_dist <- tibble(
  statistic = c("Mean", "SD", "D1", "Q1", "Q2 (median)", "Q3", "D9"),
  predict = wtp_lh_summary,
  lower_ci = apply(wtp_lh_summaries, 2, quantile, probs = 0.025),
  upper_ci = apply(wtp_lh_summaries, 2, quantile, probs = 0.975)
)

wtp_lh_dist <- readRDS(gzcon(url("https://raw.githubusercontent.com/edsandorf/evdce/refs/heads/main/Models/wtp-lh-dist.rds")))

## Table of WTP distribution ----
wtp_lh_dist |>
  gt()  |>
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  )

#### HIDDEN CHUNK ----

# individual 1
dat1 <- database[database$id_individual == 1, ]

choices <- matrix(dat1$choice, nrow(dat1), ncol(apollo_inputs$draws[[1]]))
choices1 <- ifelse(choices == 1, 1, 0)
choices2 <- ifelse(choices == 2, 1, 0)
choices3 <- ifelse(choices == 3, 1, 0)


nsims <- 10000
sim_dists <- simulate_dist(model, nsims)

attach(apollo_inputs$draws)
draws_mf <- draws_mf[1:nrow(dat1), ]
draws_sf <- draws_sf[1:nrow(dat1), ]
draws_mh <- draws_mh[1:nrow(dat1), ]
draws_lh <- draws_lh[1:nrow(dat1), ]
draws_rk <- draws_rk[1:nrow(dat1), ]
draws_md <- draws_md[1:nrow(dat1), ]
draws_ct <- draws_ct[1:nrow(dat1), ]


# Simulate the coefficients
randCoeff <- function(beta) {
  with(as.list(beta), {
    list(
      asc_alt1 = beta[1],
      asc_alt2 = beta[2],
      asc_alt3 = beta[3],
      r_mf = mu_mf + sd_mf * draws_mf + delta_mf_age * dat1$age + delta_mf_female * dat1$female + delta_mf_educ * dat1$education,
      r_sf = mu_sf + sd_sf * draws_sf + delta_sf_age * dat1$age + delta_sf_female * dat1$female + delta_sf_educ * dat1$education,
      r_mh = mu_mh + sd_mh * draws_mh + delta_mh_age * dat1$age + delta_mh_female * dat1$female + delta_mh_educ * dat1$education,
      r_lh = mu_lh + sd_lh * draws_lh + delta_lh_age * dat1$age + delta_lh_female * dat1$female + delta_lh_educ * dat1$education,
      r_rk = mu_rk + sd_rk * draws_rk + delta_rk_age * dat1$age + delta_rk_female * dat1$female + delta_rk_educ * dat1$education,
      r_md = mu_md + sd_md * draws_md + delta_md_age * dat1$age + delta_md_female * dat1$female + delta_md_educ * dat1$education,
      r_ct = -exp(mu_ct + sd_ct * draws_ct + delta_ct_age * dat1$age + delta_ct_female * dat1$female + delta_ct_educ * dat1$education)
    )
  })
}

# Compute conditional WTP
cond.lt1 <- function(randdraws, alt_data) {
  V <- lapply(1:3, function(i) {
    alt <- alt_data[[i]]
    with(randdraws, {
      asc <- get(paste0("asc_alt", i))
      asc + r_mf * alt$farm2 + r_sf * alt$farm3 + r_mh * alt$height2 +
        r_lh * alt$height3 + r_rk * alt$redkite + r_md * alt$distance +
        r_ct * alt$cost
    })
  })


  P <- (exp(V[[1]]) * choices1 + exp(V[[2]]) * choices2 + exp(V[[3]]) * choices3) /
    (exp(V[[1]]) + exp(V[[2]]) + exp(V[[3]]))

  P <- apply(P, 2, prod)
  w <- P / sum(P)

  #  sum(w * -randdraws$r_lh[1, ] / randdraws$r_ct[1, ])
  sum(w * randdraws$r_lh[1, ])
}



alt_data <- list()
alt_data[["alt1"]] = data.frame(farm2    = dat1$alt1_farm2,
                                farm3 = dat1$alt1_farm3,
                                height2 = dat1$alt1_height2,
                                height3 = dat1$alt1_height3,
                                redkite = dat1$alt1_redkite,
                                distance = dat1$alt1_distance,
                                cost = dat1$alt1_cost)
alt_data[["alt2"]] = data.frame(farm2    = dat1$alt2_farm2,
                                farm3 = dat1$alt2_farm3,
                                height2 = dat1$alt2_height2,
                                height3 = dat1$alt2_height3,
                                redkite = dat1$alt2_redkite,
                                distance = dat1$alt2_distance,
                                cost = dat1$alt2_cost)
alt_data[["alt3"]] = data.frame(farm2    = dat1$alt3_farm2,
                                farm3 = dat1$alt3_farm3,
                                height2 = dat1$alt3_height2,
                                height3 = dat1$alt3_height3,
                                redkite = dat1$alt3_redkite,
                                distance = dat1$alt3_distance,
                                cost = dat1$alt3_cost)



# Simulate the cond means for individual 1
cond_means_sim <- map_dbl(1:nsims, ~ cond.lt1(randCoeff(sim_dists[.x, ]), alt_data))

conds1 <- as.vector(c(cond.lt1(randCoeff(model$estimate), alt_data), quantile(cond_means_sim, c(0.025,0.975))))

#### HIDDEN CHUNK ----
sim.dist <- cond_means_sim

mean.x <- unlist(conditionals_lh[1, 2])

brks <- seq(quantile(sim.dist, 0.025), quantile(sim.dist, 0.975), length = 50)
brks <- c(brks[1] - (brks[2]-brks[1]) * rev(1:1000), brks)
brks <- c(brks, brks[length(brks)] + (brks[2]-brks[1]) * 1:1000)

brks <- brks[brks >= (min(sim.dist) - (brks[2]-brks[1]))]
brks <- brks[brks <= (max(sim.dist) + (brks[2]-brks[1]))]

cols <- c(rep(col1, sum(brks < quantile(sim.dist, 0.025))),
          rep(col2, (length(brks)) - (sum(brks < quantile(sim.dist, 0.025)) + sum(brks >= quantile(sim.dist, 0.975)))),
          rep(col1, sum(brks >= quantile(sim.dist, 0.975))))
h.hist <- hist(sim.dist, breaks = brks, plot = FALSE)
cols <- cols[h.hist$counts > 0]

xx <- c(quantile(sim.dist, c(0.025, 0.975)), mean.x)
tibble(sim.dist = sim.dist) |>
  ggplot(aes(x = sim.dist)) +
  geom_histogram(breaks = brks, color = 1, lwd = 0.5, aes(fill = cut(sim.dist, breaks = brks))) +
  scale_fill_manual(values = cols) +
  geom_vline(xintercept = xx, color = c(rep(col1, 2), col3), lwd = 1) +
  xlab("Marginal utility (mean of the conditional distribution for individual 1)") +
  ylab("Frequency") +
  theme_bw() +
  theme(legend.position = "none")


# LC-MIXL ----

model <- readRDS(gzcon(url("https://raw.githubusercontent.com/edsandorf/evdce/refs/heads/main/Models/lc-mxl-2cls.rds")))

#### HIDDEN CHUNK ----
apollo_initialise()

# Set core controls
apollo_control = list(
  modelName  = "LC_MXL_2Class",
  modelDescr = "LC MXL_2Class ",
  indivID    = "id_individual"
)

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


## Group and validate inputs
apollo_inputs <- apollo_validateInputs()


## Define the model and likelihood function
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


## Print coefficient table ----
tibble(
  var = names(model$estimate),
  estimate = model$estimate,
  std_err = model$robse,
  t_stat = estimate/std_err,
  p_value = 2 * pt(abs(t_stat), df = (model$nObs - model$nFreeParams), lower.tail = FALSE)
) |>
  gt(
    rowname_col = "var",
    caption = "Parameter estimates from the RP-MIXL model"
  ) |>
  fmt_number(
    columns = c(estimate, std_err, t_stat, p_value),
    decimals = 3
  )

## Delta method - WTP ----
# Settings for the function apollo_deltaMethod
deltaMethod_settings <- list(
  expression = c(
    cl1_wtp_medium_farms = "-cl1_b_medium_farms / cl1_b_cost",
    cl1_wtp_small_farms = "-cl1_b_small_farms / cl1_b_cost",
    cl1_wtp_medium_height = "-cl1_b_medium_height / cl1_b_cost",
    cl1_wtp_low_height = "-cl1_b_low_height / cl1_b_cost",
    cl1_wtp_red_kite = "-cl1_b_red_kite / cl1_b_cost",
    cl1_wtp_min_distance = "-cl1_b_min_distance / cl1_b_cost",
    cl2_wtp_medium_farms = "-cl2_b_medium_farms / cl2_b_cost",
    cl2_wtp_small_farms = "-cl2_b_small_farms / cl2_b_cost",
    cl2_wtp_medium_height = "-cl2_b_medium_height / cl2_b_cost",
    cl2_wtp_low_height = "-cl2_b_low_height / cl2_b_cost",
    cl2_wtp_red_kite = "-cl2_b_red_kite / cl2_b_cost",
    cl2_wtp_min_distance = "-cl2_b_min_distance / cl2_b_cost"
  )
)

# Apply the Delta method to calculate the standard errors of marginal WTP
wtp_results_delta <- apollo_deltaMethod(model, deltaMethod_settings)

## Delta method - WTP ----
# Settings for the function apollo_deltaMethod
deltaMethod_settings <- list(
  expression = c(
    cl1_prob = "1 / (1 +
exp(cl2_cst_alloc_fun + cl2_b_age * 70 + cl2_b_female * 1 + cl2_b_educ * 3))",
    cl2_prob = "exp(cl2_cst_alloc_fun + cl2_b_age * 70 + cl2_b_female * 1 + cl2_b_educ * 3) / (1 +
exp(cl2_cst_alloc_fun + cl2_b_age * 70 + cl2_b_female * 1 + cl2_b_educ * 3))",
    logit_cl1_prob = "log(
  (1 / (1 + exp(cl2_cst_alloc_fun + cl2_b_age * 70 + cl2_b_female * 1 + cl2_b_educ * 3)))
  /(1 - (1 / (1 + exp(cl2_cst_alloc_fun + cl2_b_age * 70 + cl2_b_female * 1 + cl2_b_educ * 3)))))",
    logit_cl2_prob = "log(
  (exp(cl2_cst_alloc_fun + cl2_b_age * 70 + cl2_b_female * 1 + cl2_b_educ * 3) / (1 + exp(cl2_cst_alloc_fun + cl2_b_age * 70 + cl2_b_female * 1 + cl2_b_educ * 3)))
  /(1 - (exp(cl2_cst_alloc_fun + cl2_b_age * 70 + cl2_b_female * 1 + cl2_b_educ * 3) / (1 + exp(cl2_cst_alloc_fun + cl2_b_age * 70 + cl2_b_female * 1 + cl2_b_educ * 3)))))"
  )
)

# Apply the Delta method to calculate the standard errors
lc_prob_results_Delta <- apollo_deltaMethod(model, deltaMethod_settings)

## Class probabilities ----
c1 <- c(lc_prob_results_Delta[3, 2], lc_prob_results_Delta[3, 2] + (lc_prob_results_Delta[3, 3] * qnorm(0.975)) * c(-1, 1))
c1 <- rbind(c1, exp(c1)/ (exp(c1) + 1))
c2 <- c(lc_prob_results_Delta[4, 2], lc_prob_results_Delta[4, 2] + (lc_prob_results_Delta[4, 3] * qnorm(0.975)) * c(-1, 1))
c2 <- rbind(c2, exp(c2)/ (exp(c2) + 1))

## Unconditional distributions ----
unconditionals <- apollo_unconditionals(
  model,
  apollo_probabilities,
  apollo_inputs
)

## Conditional distributions ----
conditionals <- apollo_conditionals(
  model,
  apollo_probabilities,
  apollo_inputs
)

## Graphs ----
tibble(
  probs = apollo_firstRow(unconditionals$pi_values$class_a, apollo_inputs)
) |>
  ggplot(aes(x = probs)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), color = 1, fill = "#87CEEB") +
  labs(
    x = "Unconditional class membership probability (class 1)",
    y = "Frequency"
  ) +
  theme_bw()

tibble(
  probs = conditionals$X1
) |>
  ggplot(aes(x = probs)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), color = 1, fill = "#87CEEB") +
  labs(
    x = "Conditional class membership probability (class 1)",
    y = "Frequency"
  ) +
  theme_bw()

# Merge the conditional probabilities with demographic data and plot ----

conditionals |>
  select(ID, X1) |>
  right_join(
    database |>
      group_by(id_individual) |>
      slice(1) |>
      ungroup() |>
      select(id_individual, age, female, education),
    by = "id_individual") |>
  mutate(
    female = factor(female)
  ) |>
  ggplot(aes(x = probs)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), color = "black", fill = "#B8860B") +
  labs(
    x = "Conditional class membership probability by female (class 1)",
    y = "Frequency"
  ) +
  facet_wrap(vars(female)) +
  theme_bw()

# Compute conditional marginal WTPs for all specified names ----
wtps_lc <- lapply(
  c("b_medium_farms", "b_small_farms", "b_medium_height",
    "b_low_height", "b_red_kite", "b_min_distance"), function(name) {
      wtps_name <- mapply(
        function(x, y) {-x / y},
        unconditionals[[name]],
        unconditionals[["b_cost"]]
      )

      # Calculate the conditional marginal WTPs
      wtps_name[1] * conditionals$X1 + wtps_name[2] * conditionals$X2
  }
)

# Plot the resulting distribution of marginal WTP
tibble(
  wtp = wtps_lc[[5]]
) |>
  ggplot(aes(x = wtp)) +
  geom_histogram(binwidth = 0.01, color = 1, fill = "#F1C40F") +
  labs(
    x = "Marginal WTP for red kites (€ per month)",
    y = "Frequency"
  ) +
  theme_bw()

#### HIDDEN SIMS ----

# Calculate marginal WTP for variable of interest
wtp_rk_model <- -unconditionals[["b_red_kite"]][[1]] / unconditionals[["b_cost"]][[1]] * conditionals$X1 +
  -unconditionals[["b_red_kite"]][[2]] / unconditionals[["b_cost"]][[2]] * conditionals$X2

# Summarise predicted WTP distribution
wtp_rk_summary <- c(
  mean = mean(wtp_rk_model),
  sd = sd(wtp_rk_model),
  quantile(wtp_rk_model, c(0.1, 0.25, 0.5, 0.75, 0.9))
)

# Set number of Krinsky-Robb simulations
nsims <- 10

# Generate empirical distributions for simulations
sim_dists <- simulate_dist(model, nsims)

# Define a function to compute the summary statistics for each simulation
compute_wtp_summary <- function(sim_row) {
  # Change coefficients to simulated draw
  model_s$estimate <- sim_row

  # Unconditional distributions for simulation draw
  unconditionals_s <- apollo_unconditionals(model_s, apollo_probabilities, apollo_inputs)

  # Conditional distributions for simulation draw
  conditionals_s <- apollo_conditionals(model_s, apollo_probabilities, apollo_inputs)

  # Compute class-specific marginal WTPs for simulation draw
  wtps_s <- mapply(
    function(x, y) {
      -x / y
    },
    unconditionals_s[["b_red_kite"]],
    unconditionals_s[["b_cost"]]
  )

  # Calculate the conditional marginal WTPs for simulation draw
  wtp_cond <- wtps_s[1] * conditionals_s$X1 + wtps_s[2] * conditionals_s$X2

  # Calculate summary statistics for simulation draw
  stats <- c(
    mean = mean(wtp_cond),
    sd = sd(wtp_cond),
    quantile(wtp_cond, c(0.1, 0.25, 0.5, 0.75, 0.9))
  )

  return(stats)
}

# Use apply to process simulations
wtp_rk_summaries <- t(apply(sim_dists, 1, compute_wtp_summary))

# Combine predictive distribution and confidence intervals
wtp_rk_dist <- tibble(
  statistic = c("Mean", "SD", "D1", "Q1", "Q2 (median)", "Q3", "D9"),
  predict = wtp_rk_summary,
  lower_ci = apply(wtp_rk_summaries, 2, quantile, probs = 0.025),
  upper_ci = apply(wtp_rk_summaries, 2, quantile, probs = 0.975)
)

wtp_rk_dist <- readRDS(gzcon(url("https://raw.githubusercontent.com/edsandorf/evdce/refs/heads/main/Models/wtp-rh-dist.rds")))


wtp_rk_dist |>
  gt()  |>
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  )
