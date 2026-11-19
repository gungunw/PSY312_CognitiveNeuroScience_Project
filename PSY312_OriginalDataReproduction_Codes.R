library(dplyr)
library(tidyr)
library(ggplot2)

original_data <- read.csv(file.choose())

# Step 1: Recalculate inhibition proportions

inhibition_scores <- original_data %>%
  group_by(subnum, condition) %>%
  summarise(
    total_nogo = sum(output_rec_trial == 1, na.rm = TRUE),
    successful_inhibitions = sum(
      output_rec_trial == 1 &
        output_correct == 0 &
        output_rt == 0,
      na.rm = TRUE
    ),
    prop_inhibition = successful_inhibitions / total_nogo,
    .groups = "drop"
  )

# Step 2: Put both conditions side by side

participant_scores <- inhibition_scores %>%
  select(subnum, condition, prop_inhibition) %>%
  pivot_wider(
    names_from = condition,
    values_from = prop_inhibition,
    names_prefix = "condition_"
  ) %>%
  rename(
    prop_immediate = condition_0,
    prop_lag = condition_450
  ) %>%
  filter(
    !is.na(prop_immediate),
    !is.na(prop_lag)
  )

# Step 3: Apply the lower -2 SD exclusion

participant_scores <- participant_scores %>%
  mutate(
    z_prop_immediate = as.numeric(scale(prop_immediate)),
    z_prop_lag = as.numeric(scale(prop_lag))
  ) %>%
  filter(
    z_prop_immediate >= -2,
    z_prop_lag >= -2
  )

# Step 4: Correct trial count
# Count actual rows per participant

trial_counts <- original_data %>%
  group_by(subnum) %>%
  summarise(
    total_trials = n(),
    .groups = "drop"
  )

participant_scores <- participant_scores %>%
  left_join(trial_counts, by = "subnum") %>%
  filter(total_trials == 360)

# Step 5: Apply Go accuracy exclusion

go_accuracy <- original_data %>%
  filter(output_rec_trial != 1) %>%
  group_by(subnum) %>%
  summarise(
    go_accuracy = mean(output_correct, na.rm = TRUE),
    .groups = "drop"
  )

participant_scores <- participant_scores %>%
  left_join(go_accuracy, by = "subnum") %>%
  filter(go_accuracy >= 0.80)

# Step 6: Check final sample

cat("Final participants:", nrow(participant_scores), "\n")

print(
  participant_scores %>%
    summarise(
      N = n(),
      Immediate_Mean = mean(prop_immediate),
      Immediate_SD = sd(prop_immediate),
      Lagged_Mean = mean(prop_lag),
      Lagged_SD = sd(prop_lag)
    )
)

# Step 7: Paired t-test

inhibition_test <- t.test(
  participant_scores$prop_immediate,
  participant_scores$prop_lag,
  paired = TRUE,
  alternative = "greater"
)

print(inhibition_test)

# Step 8: Effect size

difference <- participant_scores$prop_immediate -
  participant_scores$prop_lag

cohens_dz <- mean(difference) / sd(difference)

cat("Cohen's dz:", cohens_dz, "\n")


# 9. PAIRED-SAMPLES T-TEST
# Direction: Immediate > Lagged
# One-tailed, as reported for this comparison in the paper.

inhibition_test <- t.test(
  participant_scores$prop_immediate,
  participant_scores$prop_lag,
  paired = TRUE,
  alternative = "greater"
)

cat("\nPAIRED-SAMPLES T-TEST\n")
print(inhibition_test)


# 10. CALCULATE PAIRED-SAMPLES EFFECT SIZE

difference <- participant_scores$prop_immediate -
  participant_scores$prop_lag

cohens_dz <- mean(difference) / sd(difference)

cat("\nCOHEN'S DZ:\n")
print(cohens_dz)


# 11. PREPARE DATA FOR PLOTS

plot_data <- participant_scores %>%
  select(subnum, prop_immediate, prop_lag) %>%
  pivot_longer(
    cols = c(prop_immediate, prop_lag),
    names_to = "condition",
    values_to = "inhibition"
  ) %>%
  mutate(
    condition = recode(
      condition,
      prop_immediate = "Immediate (0 ms)",
      prop_lag = "Lagged (450 ms)"
    ),
    condition = factor(
      condition,
      levels = c("Immediate (0 ms)", "Lagged (450 ms)")
    )
  )


# 12. FIGURE 2b: PROPORTION OF SUCCESSFUL NO-GO TRIALS
# Violin + boxplot + individual participant points

figure_2b <- ggplot(
  plot_data,
  aes(x = condition, y = inhibition)
) +
  geom_violin(
    trim = FALSE,
    fill = "grey85",
    color = "black",
    width = 0.8
  ) +
  geom_boxplot(
    width = 0.15,
    fill = "white",
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.08,
    alpha = 0.45,
    size = 1.5
  ) +
  labs(
    title = "Successful No-Go Inhibition",
    x = "Action-effect condition",
    y = "Proportion of successful inhibitions"
  ) +
  theme_classic(base_size = 13)

print(figure_2b)


# 13. FIGURE 2d: DIFFERENCE IN INHIBITION BETWEEN CONDITIONS

difference_data <- participant_scores %>%
  mutate(
    difference = prop_immediate - prop_lag
  )

figure_2d <- ggplot(
  difference_data,
  aes(x = "", y = difference)
) +
  geom_violin(
    trim = FALSE,
    fill = "grey85",
    color = "black",
    width = 0.8
  ) +
  geom_boxplot(
    width = 0.15,
    fill = "white",
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.08,
    alpha = 0.45,
    size = 1.5
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Difference in Successful No-Go Inhibition",
    x = NULL,
    y = "Immediate minus Lagged"
  ) +
  theme_classic(base_size = 13)

print(figure_2d)


# 14. PAIRED PARTICIPANT PLOT

figure_paired <- ggplot(
  plot_data,
  aes(
    x = condition,
    y = inhibition,
    group = subnum
  )
) +
  geom_line(
    alpha = 0.25,
    color = "grey40"
  ) +
  geom_point(
    size = 1.8,
    alpha = 0.65
  ) +
  labs(
    title = "Individual Changes in No-Go Inhibition",
    x = "Action-effect condition",
    y = "Proportion of successful inhibitions"
  ) +
  theme_classic(base_size = 13)

print(figure_paired)


# 15. SAVE THE FIGURES AND ANALYSIS DATA

ggsave(
  "Figure_2b_Inhibition.png",
  figure_2b,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  "Figure_2d_Inhibition_Difference.png",
  figure_2d,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  "Paired_Inhibition_Plot.png",
  figure_paired,
  width = 7,
  height = 5,
  dpi = 300
)

write.csv(
  participant_scores,
  "Final_Participant_Scores.csv",
  row.names = FALSE
)

cat("\nANALYSIS COMPLETE.\n")
cat("Three plots and the participant-level data have been saved in:\n")
print(getwd())
