# INSTALL PACKAGES (only once)
install.packages("readxl")
install.packages("tidyverse")
# LOAD PACKAGES
library(readxl)
library(tidyverse)
install.packages("readxl")
library(readxl)

#1. CHeck Dataset

# Number of participants
length(unique(data$participant))

# Total number of trials
nrow(data)

# Trials per participant
table(data$participant)

# Go and NoGo trial counts
table(data$trial_type)

# Trials per delay
table(data$effect_delay)

# Go and NoGo counts at each delay
table(data$effect_delay, data$trial_type)

# Missing values
colSums(is.na(data))


############################
# 2. CHECK ACCURACY CODING
############################

# Accuracy by trial type
table(data$trial_type, data$accuracy)

# Key responses by trial type
table(data$trial_type, data$key_resp.keys, useNA = "ifany")

# Overall accuracy
mean(data$accuracy) * 100

# Go accuracy
mean(data$accuracy[data$trial_type == "Go"]) * 100

# NoGo accuracy
mean(data$accuracy[data$trial_type == "NoGo"]) * 100


############################
# 3. PARTICIPANT-LEVEL SCORES
############################

# Calculate accuracy for each participant,
# delay and trial type

participant_scores <- aggregate(
  accuracy ~ participant + effect_delay + trial_type,
  data = data,
  FUN = mean
)

# Separate NoGo and Go scores

NoGo_scores <- subset(
  participant_scores,
  trial_type == "NoGo"
)

Go_scores <- subset(
  participant_scores,
  trial_type == "Go"
)

# Display participant-level NoGo scores

print(NoGo_scores)


############################
# 4. GROUP MEAN AND STANDARD DEVIATION
############################

# NoGo mean accuracy

NoGo_mean <- aggregate(
  accuracy ~ effect_delay,
  data = NoGo_scores,
  FUN = mean
)

# NoGo standard deviation

NoGo_SD <- aggregate(
  accuracy ~ effect_delay,
  data = NoGo_scores,
  FUN = sd
)

# Combine the results

NoGo_summary <- data.frame(
  effect_delay = NoGo_mean$effect_delay,
  Mean = NoGo_mean$accuracy,
  SD = NoGo_SD$accuracy
)

NoGo_summary$SE <- NoGo_summary$SD / sqrt(15)

# 95% confidence intervals

NoGo_summary$CI_lower <- NoGo_summary$Mean -
  qt(0.975, df = 14) * NoGo_summary$SE

NoGo_summary$CI_upper <- NoGo_summary$Mean +
  qt(0.975, df = 14) * NoGo_summary$SE

print(NoGo_summary)


# Go mean accuracy

Go_mean <- aggregate(
  accuracy ~ effect_delay,
  data = Go_scores,
  FUN = mean
)

Go_SD <- aggregate(
  accuracy ~ effect_delay,
  data = Go_scores,
  FUN = sd
)

Go_summary <- data.frame(
  effect_delay = Go_mean$effect_delay,
  Mean = Go_mean$accuracy,
  SD = Go_SD$accuracy
)

print(Go_summary)


############################
# 5. FIGURE 1: NOGO ACCURACY
############################

boxplot(
  accuracy ~ factor(effect_delay),
  data = NoGo_scores,
  main = "NoGo Accuracy Across Action-Effect Delays",
  xlab = "Effect Delay (seconds)",
  ylab = "NoGo Accuracy",
  col = "lightblue",
  ylim = c(0, 1)
)

stripchart(
  accuracy ~ factor(effect_delay),
  data = NoGo_scores,
  vertical = TRUE,
  method = "jitter",
  add = TRUE,
  pch = 16
)


############################
# 6. FIGURE 2: INDIVIDUAL PARTICIPANT PERFORMANCE
############################

participants <- unique(NoGo_scores$participant)

delays <- c(0, 0.15, 0.45, 0.75)

# Create participant-by-delay matrix

NoGo_matrix <- sapply(
  delays,
  function(d) {
    NoGo_scores$accuracy[
      match(
        participants,
        NoGo_scores$participant[
          NoGo_scores$effect_delay == d
        ]
      )
    ]
  }
)

colnames(NoGo_matrix) <- c(
  "0 ms",
  "150 ms",
  "450 ms",
  "750 ms"
)

rownames(NoGo_matrix) <- participants

# Plot individual lines

matplot(
  delays,
  t(NoGo_matrix),
  type = "b",
  pch = 16,
  lty = 1,
  col = "grey50",
  xaxt = "n",
  ylim = c(0, 1),
  main = "Individual NoGo Accuracy Across Delays",
  xlab = "Effect Delay (seconds)",
  ylab = "NoGo Accuracy"
)

axis(
  1,
  at = delays,
  labels = c("0", "150", "450", "750")
)

# Add group mean line

lines(
  delays,
  colMeans(NoGo_matrix),
  type = "b",
  lwd = 3,
  pch = 18,
  col = "red"
)

legend(
  "bottomleft",
  legend = c("Individual participants", "Group mean"),
  col = c("grey50", "red"),
  lty = 1,
  pch = c(16, 18),
  lwd = c(1, 3)
)


############################
# 7. FIGURE 3: MEAN NOGO ACCURACY WITH 95% CI
############################

plot(
  NoGo_summary$effect_delay,
  NoGo_summary$Mean,
  type = "b",
  pch = 19,
  lwd = 2,
  xaxt = "n",
  ylim = c(0, 1),
  main = "Mean NoGo Accuracy Across Delays",
  xlab = "Effect Delay (seconds)",
  ylab = "Mean NoGo Accuracy"
)

axis(
  1,
  at = delays,
  labels = c("0", "150", "450", "750")
)

arrows(
  NoGo_summary$effect_delay,
  NoGo_summary$CI_lower,
  NoGo_summary$effect_delay,
  NoGo_summary$CI_upper,
  angle = 90,
  code = 3,
  length = 0.08
)


############################
# 8. FRIEDMAN TEST
############################

# Each row represents one participant.
# Each column represents one delay.

friedman_result <- friedman.test(NoGo_matrix)

print(friedman_result)

# Kendall's W effect size

kendalls_W <- as.numeric(
  friedman_result$statistic
) / (
  nrow(NoGo_matrix) * (ncol(NoGo_matrix) - 1)
)

print(kendalls_W)


############################
# 9. POST-HOC PAIRWISE COMPARISONS
############################

# Six pairwise comparisons between delays

pairwise_results <- pairwise.wilcox.test(
  NoGo_scores$accuracy,
  interaction(
    NoGo_scores$participant,
    NoGo_scores$effect_delay
  ),
  paired = FALSE
)

# The above is NOT the correct repeated-measures
# comparison for our design.
# Instead, run paired tests individually:

comparisons <- combn(1:4, 2)

posthoc_results <- data.frame(
  Comparison = character(),
  W = numeric(),
  p_value = numeric()
)

for (i in 1:ncol(comparisons)) {
  
  col1 <- comparisons[1, i]
  col2 <- comparisons[2, i]
  
  test <- wilcox.test(
    NoGo_matrix[, col1],
    NoGo_matrix[, col2],
    paired = TRUE,
    exact = FALSE
  )
  
  posthoc_results[i, ] <- list(
    paste(colnames(NoGo_matrix)[col1],
          "vs",
          colnames(NoGo_matrix)[col2]),
    as.numeric(test$statistic),
    test$p.value
  )
}

# Holm correction

posthoc_results$adjusted_p <- p.adjust(
  posthoc_results$p_value,
  method = "holm"
)

print(posthoc_results)


############################
# 10. GO REACTION TIME
############################

# Keep only correct Go responses with recorded RT

Go_RT_data <- subset(
  data,
  trial_type == "Go" &
    key_resp.keys == "space" &
    !is.na(key_resp.rt)
)

# Median RT per participant per delay
# Convert seconds into milliseconds

Go_RT <- aggregate(
  key_resp.rt ~ participant + effect_delay,
  data = Go_RT_data,
  FUN = median
)

Go_RT$RT_ms <- Go_RT$key_resp.rt * 1000

print(Go_RT)


# Group reaction-time summary

Go_RT_summary <- aggregate(
  RT_ms ~ effect_delay,
  data = Go_RT,
  FUN = mean
)

Go_RT_SD <- aggregate(
  RT_ms ~ effect_delay,
  data = Go_RT,
  FUN = sd
)

Go_RT_summary$SD <- Go_RT_SD$RT_ms

print(Go_RT_summary)


############################
# 11. FIGURE 4: GO REACTION TIME
############################

boxplot(
  RT_ms ~ factor(effect_delay),
  data = Go_RT,
  main = "Go Reaction Time Across Delays",
  xlab = "Effect Delay (seconds)",
  ylab = "Median Reaction Time (ms)",
  col = "lightgreen"
)

stripchart(
  RT_ms ~ factor(effect_delay),
  data = Go_RT,
  vertical = TRUE,
  method = "jitter",
  add = TRUE,
  pch = 16
)


############################
# 12. ACTION-EFFECT TIMING CHECK
############################

# Calculate estimated time between keypress
# and visual consequence onset

timing_data <- subset(
  data,
  key_resp.keys == "space" &
    !is.na(key_resp.rt) &
    !is.na(key_resp.started) &
    !is.na(circle_effect.started)
)

timing_data$observed_delay_ms <- (
  timing_data$circle_effect.started -
    (timing_data$key_resp.started +
       timing_data$key_resp.rt)
) * 1000

# Timing summary by intended delay

timing_median <- aggregate(
  observed_delay_ms ~ effect_delay,
  data = timing_data,
  FUN = median
)

timing_mean <- aggregate(
  observed_delay_ms ~ effect_delay,
  data = timing_data,
  FUN = mean
)

timing_summary <- data.frame(
  effect_delay = timing_median$effect_delay,
  Median_observed_ms = timing_median$observed_delay_ms,
  Mean_observed_ms = timing_mean$observed_delay_ms
)

print(timing_summary)


############################
# 13. FIGURE 5: OBSERVED TIMING
############################

boxplot(
  observed_delay_ms ~ factor(effect_delay),
  data = timing_data,
  main = "Observed Action-to-Effect Timing",
  xlab = "Intended Effect Delay (seconds)",
  ylab = "Observed Delay (ms)",
  col = "lightyellow"
)

abline(
  h = c(0, 150, 450, 750),
  lty = 2
)


############################
# 14. EXPORT RESULTS
############################

write.csv(
  participant_scores,
  "Participant_Level_Scores.csv",
  row.names = FALSE
)

write.csv(
  NoGo_summary,
  "NoGo_Accuracy_Summary.csv",
  row.names = FALSE
)

write.csv(
  Go_summary,
  "Go_Accuracy_Summary.csv",
  row.names = FALSE
)

write.csv(
  posthoc_results,
  "Posthoc_Comparisons.csv",
  row.names = FALSE
)

write.csv(
  Go_RT_summary,
  "Go_Reaction_Time_Summary.csv",
  row.names = FALSE
)

write.csv(
  timing_summary,
  "Observed_Timing_Summary.csv",
  row.names = FALSE
)
NoGo_matrix <- xtabs(
  accuracy ~ participant + effect_delay,
  data = NoGo_scores
)

friedman.test(NoGo_matrix)
t.test(
  NoGo_matrix[, "0"],
  NoGo_matrix[, "0.45"],
  paired = TRUE
)