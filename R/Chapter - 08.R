# Load packages ----
library(tidyverse)
library(patchwork)
library(janitor)
library(purrr)
library(maxLik)
library(evd)

# Global functions used in the chapter ----

#' Function for generating the data
#'
#' @param asc1 The assumed population value for asc1
#' @param beta1 The assumed population value for beta1
#' @param nobs The number of observations
#' @param corr The degree of correlation
#' @param seed The seed of the data. The default value is NULL
#'
generate_data_mnl <- function(asc1, beta1, nobs, corr, seed = NULL){
  # Set seed if specified
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # The data frame
  data_mnl <- tibble(
    alt1_attr1 = runif(nobs, 0, 10),
    alt2_attr1 = corr * alt1_attr1 + (1 - corr) * runif(nobs, 0, 10),
    utility1   = asc1 + beta1 * alt1_attr1 + rgumbel(nobs),
    utility2   =        beta1 * alt2_attr1 + rgumbel(nobs)
  ) |>
    rowwise() |>
    mutate(
      choice = which.max(c_across(starts_with("utility")))
    )

  return(data_mnl)
}

#' The log likelihood function
#'
#' @param parameters A named list of starting values/parameters of the model
#'
ll <- function(parameters){
  # The parameters are passed in through the vector and they are assigned locally
  estimation_asc1 <- parameters[1]
  estimation_beta1 <- parameters[2]

  # Utility functions
  est_utility1 <- estimation_asc1 + estimation_beta1 * data_mnl$alt1_attr1
  est_utility2 <-                   estimation_beta1 * data_mnl$alt2_attr1

  # Saving the utility chosen by each individual
  chosen_utility <- est_utility1 * (ifelse(data_mnl$choice == 1,1,0)) +
                    est_utility2 * (ifelse(data_mnl$choice == 2,1,0))

  # Computing the probability of each individual's chosen utility
  prob_chosen_alternative <- exp(chosen_utility) / (exp(est_utility1) + exp(est_utility2))

  ll <- log(prob_chosen_alternative)

  return(ll)
}

#' Create contour data
#'
#' Creates the contour data by evaluating the log-likelihood function along a
#' pre-specified gradient for the parameter values.
#'
#' NB! This function relies on scoping to find data and ll
#'
generate_contour <- function(sequence_1, sequence_2) {
  return(
    expand_grid(
      beta_1 = sequence_1,
      beta_2 = sequence_2
    ) |>
      rowwise() |>
      mutate(
        z = sum(ll(c(beta_1, beta_2)))
      )
  )
}

#' Function for creating the contour plot
#'
#' @param start_x x coordinate of the starting value
#' @param start_y y coordinate of the starting value
#' @param expression_x The expression for the x-axis
#' @param expression_y The expression for the y-axis
#' @param bins The number of bins for the contour plot
#'
contour_plot <- function(start_x, start_y, x_expression, y_expression, bins, title) {
  db_contour |>
    ggplot() +
    geom_contour_filled(mapping = aes(x = beta_1, y = beta_2, z = z),
                        color = "grey", binwidth = bins) +
    annotate("point", x = coef(ll_max)[1], y = coef(ll_max)[2], shape = 3) +
    annotate("text", x = coef(ll_max)[1], y = coef(ll_max)[2], label = "Global Maximum", vjust = ifelse(coef(ll_max)[2] < start_y, 1.35, -1.35), size = unit(10, "pt") / .pt) +
    annotate("point", x = start_x, y = start_y) +
    annotate("text", x = start_x, y = start_y, label = "Starting Values", vjust = ifelse(coef(ll_max)[2] < start_y, -1.35, 1.35), size = unit(10, "pt") /.pt) +
    annotate("segment", x = start_x, y = start_y + 0.1, xend = coef(ll_max)[1], yend = coef(ll_max)[2] - 0.1, arrow = arrow()) +
    labs(
      title = title,
      x = x_expression,
      y =  y_expression,
      fill = "LL"
    ) +
    scale_fill_brewer(palette = "Blues", direction = -1) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      legend.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      legend.text = element_text(size = 10)
    )
}

# Figure 8.1 ----
## Generate the data ----
data_mnl <- generate_data_mnl(
  asc1 = 0.5,
  beta1 = 0.5,
  nobs = 20,
  corr = 0.0,
  seed = 1234
)

## Estimate the model ----
ll_max <- maxLik(ll, start = c(2.0, -2.0), print.level = 2, method = "BHHH")

# Summary of the model
summary(ll_max)

## Create the contour plot ----
# Note, this needs to be run each time because the ll() is evaluated with the
# generated data
db_contour <- generate_contour(seq(-2.5, 6.0, 0.01), seq(-4, 4, 0.01))

# Plot the data
contour_plot(
  start_x = 2,
  start_y = -2,
  x_expression = expression(ASC1),
  y_expression = expression(beta[1]),
  bins = 30,
  title = ""
)

# Save the plot to the "Figures" folder
ggsave(file.path("Figures", "Figure-8-1.png"), width = 7, height = 7, units = "in")

## Hessian matrix ----
ll_max$hessian

## Covariance matrix ----
-solve(ll_max$hessian)

# Figure 8.2 ----
## Generate the data ----
data_mnl <- generate_data_mnl(
  asc1 = 0.5,
  beta1 = 0.5,
  nobs = 20,
  corr = 0.95,
  seed = 1234
)

## Estimate the model ----
ll_max <- maxLik(ll, start = c(2.0, -2.0), print.level = 2, method = "BHHH")

# Summary of the model
summary(ll_max)

## Create the contour plot ----
# Generate the contour
db_contour <- generate_contour(seq(-2.5, 6.0, 0.01), seq(-4, 4, 0.01))

# Plot the data
contour_plot(
  start_x = 2,
  start_y = -2,
  x_expression = expression(ASC1),
  y_expression = expression(beta[1]),
  bins = 5,
  title = ""
)

# Save the plot to the "Figures" folder
ggsave(file.path("Figures", "Figure-8-2.png"), width = 7, height = 7, units = "in")

## Hessian matrix ----
ll_max$hessian

## Covariance matrix ----
-solve(ll_max$hessian)

# Figure 8.3 ----
## Generate the data ----
data_mnl <- generate_data_mnl(
  asc1 = 0.5,
  beta1 = 0.5,
  nobs = 20,
  corr = 1,
  seed = 1234
)

## Estimate the model ----
ll_max <- maxLik(ll, start = c(2.0, -2.0), print.level = 2, method = "BHHH")

# Summary of the model
summary(ll_max)

## Create the contour plot ----
# Generate the contour
db_contour <- generate_contour(seq(-2.5, 6.0, 0.01), seq(-4, 4, 0.01))

# Plot the data
contour_plot(
  start_x = 2,
  start_y = -2,
  x_expression = expression(ASC1),
  y_expression = expression(beta[1]),
  bins = 5,
  title = ""
)

# Save the plot to the "Figures" folder
ggsave(file.path("Figures", "Figure-8-3.png"), width = 7, height = 7, units = "in")

## Hessian matrix ----
ll_max$hessian

## Covariance matrix ----
# -solve(ll_max$hessian)

# Figure 8.4 - Different algorithms ----

#' The log likelihood function
#'
#' @param parameters A named list of starting values/parameters of the model
#'
ll <- function(parameters){
  # The parameters are passed in through the vector and they are assigned locally
  estimation_beta1 <- parameters[1]
  estimation_beta2 <- parameters[2]

  lik <- -(-a * exp(-b * sqrt(0.5 * (estimation_beta1^2 + estimation_beta2^2))) - exp(0.5 * (cos(c * estimation_beta1) + cos(c * estimation_beta2))) + a + exp(1))

  return(lik)
}


# Define the data
a <- 20
b <- 0.2
c <- 2 * pi

## BFGS ----
# Estimate the model
ll_max <- maxLik(ll, start = c(0, -1.5), print.level = 2, method = "BFGS")

# Generate the contour
db_contour <- generate_contour(seq(-2, 2, 0.01), seq(-2, 2, 0.01))

# Plot the data
p1 <- contour_plot(
  start_x = 0,
  start_y = -1.5,
  x_expression = expression(beta[2]),
  y_expression = expression(beta[1]),
  bins = 1,
  title = "a. BFGS"
)

## NM ----
# Estimate the model
ll_max <- maxLik(ll, start = c(0, -1.5), print.level = 2, method = "NM")

# Plot the data
p2 <- contour_plot(
  start_x = 0,
  start_y = -1.5,
  x_expression = expression(beta[2]),
  y_expression = expression(beta[1]),
  bins = 1,
  title = "b. Nelder-Mead"
)

## SANN ----
# Estimate the model
ll_max <- maxLik(ll, start = c(0, -1.5), print.level = 2, method = "SANN")

# Plot the data
p3 <- contour_plot(
  start_x = 0,
  start_y = -1.5,
  x_expression = expression(beta[2]),
  y_expression = expression(beta[1]),
  bins = 1,
  title = "c. SANN"
)

# Assemble the plot
p1 + p2 + p3 +
  plot_layout(ncol = 2, guides = "collect") +
  guide_area()


# Save the plot to the "Figures" folder
ggsave(file.path("Figures", "Figure-8-4.png"), width = 11, height = 11, units = "in")


# Figure 3.5 - BFGS - Diff starting values ----
## First set of starting values ----
ll_max <- maxLik(ll, start = c(0.5, -1.5), print.level = 2, method = "BFGS")

# Plot the data
p1 <- contour_plot(
  start_x = 0.5,
  start_y = -1.5,
  x_expression = expression(beta[2]),
  y_expression = expression(beta[1]),
  bins = 1,
  title = "a. Starting values: 0.5, -1.5"
)

## Second set of starting values ----
ll_max <- maxLik(ll, start = c(0.5, 1.5), print.level = 2, method = "BFGS")

# Plot the data
p2 <- contour_plot(
  start_x = 0.5,
  start_y = 1.5,
  x_expression = expression(beta[2]),
  y_expression = expression(beta[1]),
  bins = 1,
  title = "b. Starting values: 0.5, 1.5"
)

## Third set of starting values ----
ll_max <- maxLik(ll, start = c(0.5, 0.5), print.level = 2, method = "BFGS")

# Plot the data
p3 <- contour_plot(
  start_x = 0.5,
  start_y = 0.5,
  x_expression = expression(beta[2]),
  y_expression = expression(beta[1]),
  bins = 1,
  title = "c. Starting values: 0.5, 0.5"
)

p1 + p2 + p3 +
  plot_layout(ncol = 2, guides = "collect") +
  guide_area()


# Save the plot to the "Figures" folder
ggsave(file.path("Figures", "Figure-8-5.png"), width = 11, height = 11, units = "in")

# Sample variation in practice ----
# NOTE! There is some code duplication from above to ensure that the file
#       follows the book.

## Generate a sample and estimate the model ----
#' The log likelihood function
#'
#' @param parameters A named list of starting values/parameters of the model
#'
ll <- function(parameters){
  # The parameters are passed in through the vector and they are assigned locally
  estimation_asc1 <- parameters[1]
  estimation_beta1 <- parameters[2]

  # Utility functions
  est_utility1 <- estimation_asc1 + estimation_beta1 * data_mnl$alt1_attr1
  est_utility2 <-                   estimation_beta1 * data_mnl$alt2_attr1

  # Saving the utility chosen by each individual
  chosen_utility <- est_utility1 * (ifelse(data_mnl$choice == 1,1,0)) +
    est_utility2 * (ifelse(data_mnl$choice == 2,1,0))

  # Computing the probability of each individual's chosen utility
  prob_chosen_alternative <- exp(chosen_utility) / (exp(est_utility1) + exp(est_utility2))

  ll <- log(prob_chosen_alternative)

  return(ll)
}

data_mnl <- generate_data_mnl(
  asc1 = 0.5,
  beta1 = 0.5,
  nobs = 20,
  corr = 0.0,
  seed = 1234
)

ll_max <- maxLik(ll, start = c(0.5, 0.5), print.level = 2, method = "BHHH")

## Run a simple simulation with 50 observations ----
models <- vector(mode = "list", length = 500)
for (i in seq_along(models)) {
  data_mnl <- generate_data_mnl(asc1 = 0.5,
                                beta1 = 0.5,
                                nobs = 50,
                                corr = 0.0)

  model <- maxLik(ll, start = c(0.5, 0.5), method = "BHHH")

  models[[i]] <- model
}

# Extract all parameter information in a data frame
estimates_1 <- map_df(models, tidy) |>
  mutate(
    simulation = "50 observations"
  )

## Run a simple simulation with 100 observations ----
models <- vector(mode = "list", length = 500)
for (i in seq_along(models)) {
  data_mnl <- generate_data_mnl(asc1 = 0.5,
                                beta1 = 0.5,
                                nobs = 100,
                                corr = 0.0)

  model <- maxLik(ll, start = c(0.5, 0.5), method = "BHHH")

  models[[i]] <- model
}

# Extract all parameter information in a data frame
estimates_2 <- map_df(models, tidy) |>
  mutate(
    simulation = "100 observations"
  )

# Create a graph of the results
estimates_1 |>
  bind_rows(estimates_2) |>
  ggplot() +
  geom_histogram(mapping = aes(x = estimate), bins = 75, color = "white") +
  geom_vline(xintercept = 0.5, color = "red", lty = 2, lwd = 1) +
  facet_grid(factor(simulation, levels = c("50 observations", "100 observations"), labels = c("'50 observations'", "'100 observations'")) ~ factor(term, labels = c("ASC", "beta[1]")), labeller = label_parsed) +
  labs(x = "Parameter estimate", y = "Count") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

ggsave(file.path("Figures", "Figure-8-6.png"), width = 7.5, height = 7.5, units = "in")
