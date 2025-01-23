# Load the packages ----
library(tidyverse)
library(gt)
library(janitor)
library(patchwork)

# Load the data ----
data_wind <- read_csv(gzcon(url("https://raw.githubusercontent.com/edsandorf/evdce/refs/heads/main/Data/data-windmills.csv"))) |>
  clean_names()

# Mutate some of the variables
data_wind <- data_wind |>
  mutate(
    choice = as_factor(choice),
    choice_task = as_factor(choice_task),
    female = as_factor(female),
    education = as_factor(education),
    age_group = cut(age, breaks = seq(15, 90, by = 15))
  )

# Create a set of global variables
n_alts <- 3
n_choices <- 10
n_rows_data <- nrow(data_wind)
n_individuals <- select(data_wind, id_individual) |>
  n_distinct()
sq_alt <- 1


# Missing values ----
# Create a table with missing values.
data_wind |>
  mutate(
    missing = is.na(choice)
  ) |>
  count(missing, name = "frequency") |>
  mutate(
    share = round(100 * frequency / sum(frequency), 2)
  ) |>
  gt()

# Create a table with missing values by individual
data_wind |>
  filter(!is.na(choice)) |>
  count(id_individual, name = "completed_tasks") |>
  count(completed_tasks, name = "frequency") |>
  mutate(
    share = round(100 * frequency / sum(frequency), 2)
  ) |>
  gt()


# Figure 7.1 ----
## Figure 7.1a ----
p1 <- data_wind |>
  group_by(choice_task) |>
  summarize(
    group_share = mean(is.na(choice)),
    .groups = "drop"
  ) |>
  ggplot(mapping = aes(x = choice_task, y = group_share, group = 1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Choice task",
    y = "Share of missing values"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

## Figure 7.1b ----
p2 <- data_wind |>
  group_by(choice_task, female) |>
  summarize(
    group_share = mean(is.na(choice)),
    .groups = "drop"
  ) |>
  ggplot(mapping = aes(x = choice_task, y = group_share, group = female, col = female)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Choice task",
    y = "Share of missing values",
    col = "Female"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

# Combine the plots
p1 + p2 + plot_layout(ncol = 2)


ggsave(file.path("Figures", "Figure-7-1.png"), width = 14, height = 7, units = "in")

# Choice shares ----
# Create a table
data_wind |>
  filter(!is.na(choice)) |>
  count(choice, name = "frequency") |>
  mutate(
    share = round(100 * frequency / sum(frequency), 2)
  ) |>
  gt()

## Figure 7.2 - Choice shares ----
data_wind |>
  filter(!is.na(choice)) |>
  count(choice_task, choice) |>
  group_by(choice_task) |>
  mutate(
    share = n/sum(n)
  ) |>
  ggplot(mapping = aes(x = choice_task, y = share, group = choice, col = choice)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Choice task",
    y = "Share of choices",
    col = "Choice"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

ggsave(file.path("Figures", "Figure-7-2.png"), width = 14, height = 7, units = "in")

## Figure 7.3 - Choice shares by education level ----
data_wind |>
  filter(!is.na(choice)) |>
  count(choice_task, choice, education) |>
  group_by(choice_task, education) |>
  mutate(
    share = n/sum(n)
  ) |>
  ggplot(mapping = aes(x = choice_task, y = share, group = choice, col = choice)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Choice task",
    y = "Share of choices",
    col = "Choice"
  ) +
  facet_wrap(~education, labeller = label_both) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

ggsave(file.path("Figures", "Figure-7-3.png"), width = 14, height = 7, units = "in")

# Status quo choices ----
## Create a table with choices
choice_table <-  data_wind |>
  filter(!is.na(choice)) |>
  count(id_individual, choice, .drop = FALSE, name = "frequency")

# We can add socio-demographics to this table by joining it with our data
choice_table <- data_wind |>
  select(id_individual, age, age_group, female, education) |>
  distinct() |>
  left_join(choice_table, by = "id_individual")


# Choice patterns ----
# Create a table to show the choice patterns
choice_table |>
  count(choice, frequency, name = "number_of_choices") |>
  mutate(
    share_of_choices = number_of_choices / sum(number_of_choices)
  ) |>
  pivot_wider(names_from = choice, values_from = c(number_of_choices, share_of_choices), names_glue = "{.value}_{choice}") |>
  gt() |>
  tab_header(title = "Choice patterns") |>
  tab_spanner(label = "Number of choices", columns = vars(starts_with("number_of_choices"))) |>
  tab_spanner(label = "Share of choices", columns = vars(starts_with("share_of_choices"))) |>
  cols_label(
    frequency = "Frequency",
    number_of_choices_1 = "SQ",
    number_of_choices_2 = "Alternative B",
    number_of_choices_3 = "Alternative C",
    share_of_choices_1 = "SQ",
    share_of_choices_2 = "Alternative B",
    share_of_choices_3 = "Alternative C"
  ) |>
  fmt_number(columns = vars(starts_with("share")), decimals = 2)

# Individuals who always chose the SQ
choice_table |>
  group_by(id_individual) |>
  mutate(
    number_of_choices = sum(frequency)
  ) |>
  filter(choice == 1 & frequency == number_of_choices)


# Figure 7.4 ----
choice_table |>
  count(choice, frequency, female, name = "number_of_choices") |>
  ggplot(mapping = aes(x = frequency, y = number_of_choices, fill = female)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~choice) +
  labs(
    x = "Frequency",
    y = "Number of respondents",
    fill = "Female"
  ) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

ggsave(file.path("Figures", "Figure-7-4.png"), width = 14, height = 7, units = "in")

# Attribute level analysis ----
## Creating dataframes ----
# This is what I get with the redkite
redkite_levels <- data_wind |>
  filter(!is.na(choice)) |>
  select(choice, choice_task, female, contains("redkite")) |>
  pivot_longer(
    cols = contains("redkite"),
    names_to = "alternative",
    values_to = "level"
  ) |>
  mutate(
    alternative = parse_number(alternative)
  )

# Farm is a bit trickier
farm_levels <- data_wind |>
  filter(!is.na(choice)) |>
  select(choice, choice_task, contains("farm")) |>
  pivot_longer(
    cols = contains("farm"),
    names_to = c("alternative", ".value"),
    names_sep = "_"
  ) |>
  mutate(
    alternative = parse_number(alternative),
    farm = farm2 * 2 + farm3 * 3,
    farm = ifelse(farm == 0, 1, farm)
  ) |>
  select(-farm2, -farm3)


# Figure 7.5 ----
## Figure 7.5a ----
p1 <- redkite_levels |>
  group_by(level) |>
  summarize(
    share_chosen = mean(choice == alternative)
  ) |>
  ggplot(mapping = aes(x = level, y = share_chosen)) +
  geom_bar(stat = "summary") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = seq(-5, 5, by = 2.5)) +
  labs(
    title = "Including SQ choices",
    x = "Attribute (Redkite) level",
    y = "Choice share (%) for level when available"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

## Figure 7.5b ----
p2 <- redkite_levels |>
  filter(alternative != sq_alt & choice != sq_alt) |>
  group_by(level) |>
  summarize(
    share_chosen = mean(choice == alternative)
  ) |>
  ggplot(mapping = aes(x = level, y = share_chosen)) +
  geom_bar(stat = "summary") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = seq(-5, 5, by = 2.5)) +
  labs(
    title = "Excluding SQ choices",
    x = "Attribute (Redkite) level",
    y = "Choice share (%) for level when available"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

p1 + p2 + plot_layout(ncol = 2)

ggsave(file.path("Figures", "Figure-7-5.png"), width = 14, height = 7, units = "in")

# Figure 7.6 ----
## Figure 7.6a ----
p1 <- farm_levels |>
  group_by(farm) |>
  summarize(
    share_chosen = mean(choice == alternative)
  ) |>
  ggplot(mapping = aes(x = farm, y = share_chosen)) +
  geom_bar(stat = "summary") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Including SQ choices",
    x = "Attribute (Farm) level",
    y = "Choice share (%) for level when available"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

## Figure 7.6b ----
p2 <- farm_levels |>
  filter(alternative != sq_alt & choice != sq_alt) |>
  group_by(farm) |>
  summarize(
    share_chosen = mean(choice == alternative)
  ) |>
  ggplot(mapping = aes(x = farm, y = share_chosen)) +
  geom_bar(stat = "summary") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Excluding SQ choices",
    x = "Attribute (Farm) level",
    y = "Choice share (%) for level when available"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

p1 + p2 + plot_layout(ncol = 2)

ggsave(file.path("Figures", "Figure-7-6.png"), width = 14, height = 7, units = "in")

# Figure 7.7 ----
redkite_levels |>
  filter(alternative != sq_alt & choice != sq_alt) |>
  group_by(level, female) |>
  summarize(
    share_chosen = mean(choice == alternative)
  ) |>
  ggplot(mapping = aes(x = level, y = share_chosen, fill = female)) +
  geom_bar(stat = "summary", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = seq(-5, 5, by = 2.5)) +
  labs(
    title = "Excluding SQ choices",
    x = "Attribute (red kite) level",
    y = "Choice share (%) for level when available"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    plot.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

ggsave(file.path("Figures", "Figure-7-7.png"), width = 14, height = 7, units = "in")
