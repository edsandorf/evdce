# Load packages ----
library(tidyverse)
library(spdesign)

# Read in the experimental design ----
design <- readRDS(gzcon(url("https://raw.githubusercontent.com/edsandorf/evdce/refs/heads/main/Data/design-windmills.rds")))

# Full factorial ----
## Generating the full factorial ----
full_fact <- full_factorial(
  list(
    alt1_sq = 1,
    alt1_farm = 0,
    alt1_height = 0,
    alt1_redkite = 0,
    alt1_distance = 0,
    alt1_cost = 0,
    alt2_sq = 0,
    alt2_farm = c(1, 2, 3),
    alt2_height = c(1, 2, 3),
    alt2_redkite = c(-5, -2.5, 0, 2.5, 5),
    alt2_distance = c(0, 0.25, 0.5, 0.75, 1),
    alt2_cost = 1:10,
    alt3_sq = 0,
    alt3_farm = c(1, 2, 3),
    alt3_height = c(1, 2, 3),
    alt3_redkite = c(-5, -2.5, 0, 2.5, 5),
    alt3_distance = c(0, 0.25, 0.5, 0.75, 1),
    alt3_cost = 1:10
  )
)

# Show a subset of rows and columns from the design matrix
full_fact[1:6, c(1, 8:12)]

## The candidate set ----
candidate_set <- full_fact[!((full_fact$alt2_height == 1 & full_fact$alt2_distance < 0.75) | (full_fact$alt3_height == 1 & full_fact$alt3_distance < 0.75)), ]

candidate_set[1:6, c(1, 8:12)]

summary(candidate_set)

candidate_set[candidate_set$alt2_height == 1 & candidate_set$alt2_distance < 0.75, ]

# Generating a design ----
## Define the utility function ----
utility <- list(
  alt1 = "b_sq[0] * sq[1]",
  alt2 = "b_farm_dummy[c(0.25, 0.5)] * farm[c(1, 2, 3)] +
          b_height_dummy[c(0.25, 0.5)] * height[c(1, 2, 3)] +
          b_redkite[-0.05] * redkite[c(-5, -2.5, 0, 2.5, 5)] +
          b_distance[0.5] * distance[c(0, 0.25, 0.5, 0.75, 1)] +
          b_cost[-0.05] * cost[seq(1, 10)]",
  alt3 = "b_farm_dummy * farm +
          b_height_dummy * height +
          b_redkite * redkite +
          b_distance * distance +
          b_cost * cost"
)

## Generate the design ----
design <- generate_design(utility,
                          rows = 100,
                          model = "mnl",
                          efficiency_criteria = "d-error",
                          algorithm = "rsc")

# Inspect the design the design ----
## Summary of the design ----
summary(design)

## Correlations of the design ----
cor(design)

## Level balance of the design ----
level_balance(design)[1:3]

## Dominating and dominated alternatives ----
### Inspecting the probabilites in a table ----
probabilities(design) |>
  head()

### Plotting the choice probabilities ----
probabilities(design) |>
  as_tibble() |>
  rowid_to_column() |>
  pivot_longer(-rowid, names_to = "alt", values_to = "prob") |>
  ggplot(aes(x = rowid, y = prob, fill = alt)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Choice task", y = "Choice probability", fill = "Alternative") +
  scale_x_continuous(breaks = seq(1, 100, by = 2)) +
  scale_fill_discrete(label = c("SQ", "Alt 1", "Alt 2")) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 315),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

ggsave(file.path("Figures", "Figure-5-1.png"), width = 14, height = 7, units = "in")

## Utility balance ----

#' A function to calculate the utility balance
#'
#' @param x A matrix of choice probabilities
#'
utility_balance <- function(x) {
  # Ensure that it is a matrix (and not a data.frame()/tibble())
  x <- as.matrix(x)

  # Find number of non-zero alts where 0 or NA can be non-available
  n_alts <- apply(x, 1, function(y) sum(y > 0, na.rm = TRUE))

  # Calculate for each alternative
  x <- x / (1 / n_alts)

  # Replace all zero with 1 to enable taking the product
  index_zero <- x == 0
  x[index_zero] <- 1

  # Take the product. This line requires the Rfast package.
  x <- Rfast::rowprods(x)

  return(x)
}

# Use the function for utility balance on the choice probabilities
utility_balance(probabilities(design)) |>
  head()


# Blocking the design ----
design <- block(design, 10)
