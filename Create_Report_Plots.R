################################################################################
# Plots for Final Project 
################################################################################

# import libraries 
library(tidyverse) 
library(ggh4x)

# load data 
results = read.csv('/Users/rmcnamara4/Documents/BINF_Project/performance_summary_table.csv', 
                   header = TRUE)

# add group indicator column
results = results %>%
  group_by(Init_Weights, Gender, Num_Epochs) %>%
  mutate(
    GroupIndicator = paste0(Init_Weights, ' - ', Gender, ' - ', Num_Epochs)
  ) %>%
  ungroup()

results$Model = factor(results$Model, 
                       levels = c('ResNet-18', 'ResNet-50', 'AlexNet', 'VGG-16-BN'))
results$GroupIndicator = factor(results$GroupIndicator, 
                                levels = c('ImageNet - No - 7', 
                                           'ImageNet - Yes - 7', 
                                           'Xavier - No - 7', 
                                           'Xavier - Yes - 7', 
                                           'Xavier - No - 10', 
                                           'Xavier - Yes - 10'))
results$Metric = factor(results$Metric, 
                        levels = c('MAE', 'MAPE', 'MSE', 'R2', 'Pearson'))

################################################################################

# plot results for 7 epochs 
performance_results_7_epochs = results %>%
  filter(Num_Epochs == 7, Metric != 'MSE') %>%
  ggplot(aes(x = Model, y = Mean, fill = GroupIndicator)) +
  geom_bar(position = 'dodge2', stat = 'identity', color = 'black') +
  # geom_point(size = 3, position = position_dodge(width = 0.5)) + 
  facet_wrap(~ Metric, nrow = 1, ncol = 4, scales = 'free') +
  geom_errorbar(
    aes(ymin = Lower_CI, ymax = Upper_CI), 
    width = 0.25, 
    size = 0.8,
    position = position_dodge(width = 0.9)
  ) +
  scale_fill_manual(values = c('deepskyblue', 'dodgerblue4', 'lightgreen', 'darkgreen')) +
  labs(
    x = NULL, 
    y = NULL,
    fill = NULL
  ) +
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 12, face = 'bold'), 
    axis.text.y = element_text(size = 12, face = 'bold'), 
    strip.text = element_text(size = 14, face = 'bold'),
    legend.title = element_text(size = 12, face = 'bold'), 
    legend.text = element_text(size = 12, face = 'bold'), 
    legend.text.align = 1,
    legend.position = 'bottom', 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank()
  )

performance_results_7_epochs = performance_results_7_epochs + 
  facetted_pos_scales(
    y = list(
      Metric == 'Pearson' ~ scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)), 
      Metric == 'R2' ~ scale_y_continuous(limits = c(-7, 1), breaks = seq(-7, 1, 2))
    )
  )

performance_results_7_epochs

################################################################################

performance_results_xavier = results %>%
  filter(Init_Weights == 'Xavier', Metric != 'MSE') %>%
  ggplot(aes(x = Model, y = Mean, fill = GroupIndicator)) + 
  geom_bar(position = 'dodge2', stat = 'identity', color = 'black') + 
  facet_wrap(~ Metric, nrow = 1, ncol = 4, scales = 'free') + 
  geom_errorbar(
    aes(ymin = Lower_CI, ymax = Upper_CI), 
    width = 0.25, 
    size = 0.8, 
    position = position_dodge(width = 0.9)
  ) + 
  scale_fill_manual(values = c('lightgreen', 'darkgreen', 'firebrick1', 'firebrick4')) + 
  labs(
    x = NULL, 
    y = NULL, 
    fill = NULL
  ) + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 12, face = 'bold'), 
    axis.text.y = element_text(size = 12, face = 'bold'), 
    strip.text = element_text(size = 14, face = 'bold'),
    legend.title = element_text(size = 12, face = 'bold'), 
    legend.text = element_text(size = 12, face = 'bold'), 
    legend.text.align = 1,
    legend.position = 'bottom', 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank()
  )

performance_results_xavier = performance_results_xavier + 
  facetted_pos_scales(
    y = list(
      Metric == 'Pearson' ~ scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)), 
      Metric == 'R2' ~ scale_y_continuous(limits = c(-7, 1), breaks = seq(-7, 1, 2))
    )
  )

performance_results_xavier

################################################################################
# Gender Results 

gender_results = read.csv('/Users/rmcnamara4/Documents/BINF_Project/performance_summary_table_gender_cohorts.csv', 
                   header = TRUE)

gender_results = gender_results %>%
  mutate(
    Gender = case_when(
      Gender == 'No' ~ 'Gender Not Included', 
      Gender == 'Yes' ~ 'Gender Included'
    ), 
    Num_Epochs = case_when(
      Num_Epochs == 7 ~ '7 Epochs', 
      Num_Epochs == 10 ~ '10 Epochs'
    )
  )



gender_results$Model = factor(gender_results$Model, 
                       levels = c('ResNet-18', 'ResNet-50', 'AlexNet', 'VGG-16-BN'))
gender_results$Metric = factor(gender_results$Metric, 
                        levels = c('MAE', 'MAPE', 'MSE', 'R2', 'Pearson'))
gender_results$Gender = factor(gender_results$Gender, 
                               levels = c('Gender Not Included', 
                                          'Gender Included'))
gender_results$Num_Epochs = factor(gender_results$Num_Epochs, 
                                   levels = c('7 Epochs', 
                                              '10 Epochs'))

################################################################################
# Full Gender Results

# MAE
full_gender_results_mae = gender_results %>%
  filter(Metric == 'MAE', Num_Epochs == '7 Epochs') %>%
  ggplot(aes(x = Model, y = Mean, fill = Gender_Split)) + 
  geom_bar(position = 'dodge2', stat = 'identity', color = 'black') + 
  facet_grid(Init_Weights ~ Gender, scales = 'fixed') + 
  geom_errorbar(
    aes(ymin = Lower_CI, ymax = Upper_CI), 
    width = 0.25, 
    size = 0.8, 
    position = position_dodge(width = 0.9)
  ) + 
  scale_fill_manual(values = c('palevioletred3', 'royalblue3')) +
  scale_y_continuous(limits = c(0, 125), breaks = seq(0, 125, 25)) +
  labs(
    x = NULL,
    y = 'MAE', 
    fill = NULL
  ) + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 8, face = 'bold'), 
    axis.text.y = element_text(size = 8, face = 'bold'), 
    strip.text = element_text(size = 10, face = 'bold'),
    legend.title = element_text(size = 8, face = 'bold'), 
    legend.text = element_text(size = 8, face = 'bold'), 
    legend.text.align = 1,
    legend.position = 'bottom', 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(size = 8, face = 'bold', 
                                margin = margin(r = 0.3, unit = 'cm'))
  )

# MSE
full_gender_results_mse = gender_results %>%
  filter(Metric == 'MSE', Num_Epochs == '7 Epochs') %>%
  ggplot(aes(x = Model, y = Mean, fill = Gender_Split)) + 
  geom_bar(position = 'dodge2', stat = 'identity', color = 'black') + 
  facet_grid(Init_Weights ~ Gender, scales = 'fixed') + 
  geom_errorbar(
    aes(ymin = Lower_CI, ymax = Upper_CI), 
    width = 0.25, 
    size = 0.8, 
    position = position_dodge(width = 0.9)
  ) + 
  scale_fill_manual(values = c('palevioletred3', 'royalblue3')) +
  # scale_y_continuous(limits = c(-10, 1), breaks = seq(-9, 1, 1)) +
  labs(
    x = NULL,
    y = 'MSE', 
    fill = NULL
  ) + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 8, face = 'bold'), 
    axis.text.y = element_text(size = 8, face = 'bold'), 
    strip.text = element_text(size = 10, face = 'bold'),
    legend.title = element_text(size = 8, face = 'bold'), 
    legend.text = element_text(size = 8, face = 'bold'), 
    legend.text.align = 1,
    legend.position = 'bottom', 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(size = 8, face = 'bold', 
                                margin = margin(r = 0.3, unit = 'cm'))
  )

# MAPE
full_gender_results_mape = gender_results %>%
  filter(Metric == 'MAPE', Num_Epochs == '7 Epochs') %>%
  ggplot(aes(x = Model, y = Mean, fill = Gender_Split)) + 
  geom_bar(position = 'dodge2', stat = 'identity', color = 'black') + 
  facet_grid(Init_Weights ~ Gender, scales = 'fixed') + 
  geom_errorbar(
    aes(ymin = Lower_CI, ymax = Upper_CI), 
    width = 0.25, 
    size = 0.8, 
    position = position_dodge(width = 0.9)
  ) + 
  scale_fill_manual(values = c('palevioletred3', 'royalblue3')) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 25)) +
  labs(
    x = NULL,
    y = 'MAPE', 
    fill = NULL
  ) + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 8, face = 'bold'), 
    axis.text.y = element_text(size = 8, face = 'bold'), 
    strip.text = element_text(size = 10, face = 'bold'),
    legend.title = element_text(size = 8, face = 'bold'), 
    legend.text = element_text(size = 8, face = 'bold'), 
    legend.text.align = 1,
    legend.position = 'bottom', 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(size = 8, face = 'bold', 
                                margin = margin(r = 0.3, unit = 'cm'))
  )

# Pearson
full_gender_results_pearson = gender_results %>%
  filter(Metric == 'Pearson', Num_Epochs == '7 Epochs') %>%
  ggplot(aes(x = Model, y = Mean, fill = Gender_Split)) + 
  geom_bar(position = 'dodge2', stat = 'identity', color = 'black') + 
  facet_grid(Init_Weights ~ Gender, scales = 'fixed') + 
  geom_errorbar(
    aes(ymin = Lower_CI, ymax = Upper_CI), 
    width = 0.25, 
    size = 0.8, 
    position = position_dodge(width = 0.9)
  ) + 
  scale_fill_manual(values = c('palevioletred3', 'royalblue3')) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
  labs(
    x = NULL,
    y = 'Pearson', 
    fill = NULL
  ) + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 8, face = 'bold'), 
    axis.text.y = element_text(size = 8, face = 'bold'), 
    strip.text = element_text(size = 10, face = 'bold'),
    legend.title = element_text(size = 8, face = 'bold'), 
    legend.text = element_text(size = 8, face = 'bold'), 
    legend.text.align = 1,
    legend.position = 'bottom', 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(size = 8, face = 'bold', 
                                margin = margin(r = 0.3, unit = 'cm'))
  )

# R2
full_gender_results_r2 = gender_results %>%
  filter(Metric == 'R2', Num_Epochs == '7 Epochs') %>%
  ggplot(aes(x = Model, y = Mean, fill = Gender_Split)) + 
  geom_bar(position = 'dodge2', stat = 'identity', color = 'black') + 
  facet_grid(Init_Weights ~ Gender, scales = 'fixed') + 
  geom_errorbar(
    aes(ymin = Lower_CI, ymax = Upper_CI), 
    width = 0.25, 
    size = 0.8, 
    position = position_dodge(width = 0.9)
  ) + 
  scale_fill_manual(values = c('palevioletred3', 'royalblue3')) +
  scale_y_continuous(limits = c(-10, 1), breaks = seq(-9, 1, 1)) +
  labs(
    x = NULL,
    y = 'R2', 
    fill = NULL
  ) + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 8, face = 'bold'), 
    axis.text.y = element_text(size = 8, face = 'bold'), 
    strip.text = element_text(size = 10, face = 'bold'),
    legend.title = element_text(size = 8, face = 'bold'), 
    legend.text = element_text(size = 8, face = 'bold'), 
    legend.text.align = 1,
    legend.position = 'bottom', 
    panel.grid.major.x = element_blank(), 
    axis.title.y = element_text(size = 8, face = 'bold', 
                                margin = margin(r = 0.3, unit = 'cm'))
  )
  
################################################################################
# Xavier Gender Results 

# MAE
xavier_gender_results_mae = gender_results %>%
  filter(Init_Weights == 'Xavier', Metric == 'MAE') %>%
  ggplot(aes(x = Model, y = Mean, fill = Gender_Split)) + 
  geom_bar(position = 'dodge2', stat = 'identity', color = 'black') + 
  facet_grid(Num_Epochs ~ Gender, scales = 'fixed') + 
  geom_errorbar(
    aes(ymin = Lower_CI, ymax = Upper_CI), 
    width = 0.25, 
    size = 0.8, 
    position = position_dodge(width = 0.9)
  ) + 
  scale_fill_manual(values = c('palevioletred3', 'royalblue3')) + 
  scale_y_continuous(limits = c(0, 125), breaks = seq(0, 125, 25)) +
  labs(
    x = NULL,
    y = 'MAE', 
    fill = NULL
  ) + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 8, face = 'bold'), 
    axis.text.y = element_text(size = 8, face = 'bold'), 
    strip.text = element_text(size = 10, face = 'bold'),
    legend.title = element_text(size = 8, face = 'bold'), 
    legend.text = element_text(size = 8, face = 'bold'), 
    legend.text.align = 1,
    legend.position = 'bottom', 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(size = 8, face = 'bold', 
                                margin = margin(r = 0.3, unit = 'cm'))
  )

# MSE
xavier_gender_results_mse = gender_results %>%
  filter(Init_Weights == 'Xavier', Metric == 'MSE') %>%
  ggplot(aes(x = Model, y = Mean, fill = Gender_Split)) + 
  geom_bar(position = 'dodge2', stat = 'identity', color = 'black') + 
  facet_grid(Num_Epochs ~ Gender, scales = 'fixed') + 
  geom_errorbar(
    aes(ymin = Lower_CI, ymax = Upper_CI), 
    width = 0.25, 
    size = 0.8, 
    position = position_dodge(width = 0.9)
  ) + 
  scale_fill_manual(values = c('palevioletred3', 'royalblue3')) + 
  # scale_y_continuous(limits = c(0, 125), breaks = seq(0, 125, 25)) +
  labs(
    x = NULL,
    y = 'MSE', 
    fill = NULL
  ) + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 8, face = 'bold'), 
    axis.text.y = element_text(size = 8, face = 'bold'), 
    strip.text = element_text(size = 10, face = 'bold'),
    legend.title = element_text(size = 8, face = 'bold'), 
    legend.text = element_text(size = 8, face = 'bold'), 
    legend.text.align = 1,
    legend.position = 'bottom', 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(size = 8, face = 'bold', 
                                margin = margin(r = 0.3, unit = 'cm'))
  )

# MAPE
xavier_gender_results_mape = gender_results %>%
  filter(Init_Weights == 'Xavier', Metric == 'MAPE') %>%
  ggplot(aes(x = Model, y = Mean, fill = Gender_Split)) + 
  geom_bar(position = 'dodge2', stat = 'identity', color = 'black') + 
  facet_grid(Num_Epochs ~ Gender, scales = 'fixed') + 
  geom_errorbar(
    aes(ymin = Lower_CI, ymax = Upper_CI), 
    width = 0.25, 
    size = 0.8, 
    position = position_dodge(width = 0.9)
  ) + 
  scale_fill_manual(values = c('palevioletred3', 'royalblue3')) + 
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 25)) +
  labs(
    x = NULL,
    y = 'MAPE', 
    fill = NULL
  ) + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 8, face = 'bold'), 
    axis.text.y = element_text(size = 8, face = 'bold'), 
    strip.text = element_text(size = 10, face = 'bold'),
    legend.title = element_text(size = 8, face = 'bold'), 
    legend.text = element_text(size = 8, face = 'bold'), 
    legend.text.align = 1,
    legend.position = 'bottom', 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(size = 8, face = 'bold', 
                                margin = margin(r = 0.3, unit = 'cm'))
  )

# Pearson
xavier_gender_results_pearson = gender_results %>%
  filter(Init_Weights == 'Xavier', Metric == 'Pearson') %>%
  ggplot(aes(x = Model, y = Mean, fill = Gender_Split)) + 
  geom_bar(position = 'dodge2', stat = 'identity', color = 'black') + 
  facet_grid(Num_Epochs ~ Gender, scales = 'fixed') + 
  geom_errorbar(
    aes(ymin = Lower_CI, ymax = Upper_CI), 
    width = 0.25, 
    size = 0.8, 
    position = position_dodge(width = 0.9)
  ) + 
  scale_fill_manual(values = c('palevioletred3', 'royalblue3')) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    x = NULL,
    y = 'Pearson', 
    fill = NULL
  ) + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 8, face = 'bold'), 
    axis.text.y = element_text(size = 8, face = 'bold'), 
    strip.text = element_text(size = 9, face = 'bold'),
    legend.title = element_text(size = 8, face = 'bold'), 
    legend.text = element_text(size = 8, face = 'bold'), 
    legend.text.align = 1,
    legend.position = 'bottom', 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(size = 8, face = 'bold', 
                                margin = margin(r = 0.3, unit = 'cm'))
  )

# R2
xavier_gender_results_r2 = gender_results %>%
  filter(Init_Weights == 'Xavier', Metric == 'R2') %>%
  ggplot(aes(x = Model, y = Mean, fill = Gender_Split)) + 
  geom_bar(position = 'dodge2', stat = 'identity', color = 'black') + 
  facet_grid(Num_Epochs ~ Gender, scales = 'fixed') + 
  geom_errorbar(
    aes(ymin = Lower_CI, ymax = Upper_CI), 
    width = 0.25, 
    size = 0.8, 
    position = position_dodge(width = 0.9)
  ) + 
  scale_fill_manual(values = c('palevioletred3', 'royalblue3')) + 
  scale_y_continuous(limits = c(-10, 1), breaks = seq(-9, 1, 1)) +
  labs(
    x = NULL,
    y = 'R2', 
    fill = NULL
  ) + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 8, face = 'bold'), 
    axis.text.y = element_text(size = 8, face = 'bold'), 
    strip.text = element_text(size = 9, face = 'bold'),
    legend.title = element_text(size = 8, face = 'bold'), 
    legend.text = element_text(size = 8, face = 'bold'), 
    legend.text.align = 1,
    legend.position = 'bottom', 
    panel.grid.major.x = element_blank(), 
    axis.title.y = element_text(size = 8, face = 'bold', 
                                margin = margin(r = 0.3, unit = 'cm'))
  )

################################################################################

ggsave('/Users/rmcnamara4/Documents/BINF_Project/performance_results_7_epochs.png', 
       performance_results_7_epochs, device = 'png', height = 17, width = 45, unit = 'cm')

ggsave('/Users/rmcnamara4/Documents/BINF_Project/performance_results_xavier.png', 
       performance_results_xavier, device = 'png', height = 17, width = 45, unit = 'cm') 

ggsave('/Users/rmcnamara4/Documents/BINF_Project/full_gender_results_mae.png', 
       full_gender_results_mae, device = 'png', height = 10, width = 21, unit = 'cm')

ggsave('/Users/rmcnamara4/Documents/BINF_Project/full_gender_results_mse.png', 
       full_gender_results_mse, device = 'png', height = 10, width = 21, unit = 'cm')

ggsave('/Users/rmcnamara4/Documents/BINF_Project/full_gender_results_mape.png', 
       full_gender_results_mape, device = 'png', height = 10, width = 21, unit = 'cm')

ggsave('/Users/rmcnamara4/Documents/BINF_Project/full_gender_results_pearson.png', 
       full_gender_results_pearson, device = 'png', height = 10, width = 21, unit = 'cm')

ggsave('/Users/rmcnamara4/Documents/BINF_Project/full_gender_results_r2.png', 
       full_gender_results_r2, device = 'png', height = 10, width = 21, unit = 'cm')

ggsave('/Users/rmcnamara4/Documents/BINF_Project/xavier_gender_results_mae.png', 
       xavier_gender_results_mae, device = 'png', height = 10, width = 21, unit = 'cm')

ggsave('/Users/rmcnamara4/Documents/BINF_Project/xavier_gender_results_mse.png', 
       xavier_gender_results_mse, device = 'png', height = 10, width = 21, unit = 'cm')

ggsave('/Users/rmcnamara4/Documents/BINF_Project/xavier_gender_results_mape.png', 
       xavier_gender_results_mape, device = 'png', height = 10, width = 21, unit = 'cm')

ggsave('/Users/rmcnamara4/Documents/BINF_Project/xavier_gender_results_pearson.png', 
       xavier_gender_results_pearson, device = 'png', height = 10, width = 21, unit = 'cm')

ggsave('/Users/rmcnamara4/Documents/BINF_Project/xavier_gender_results_r2.png', 
       xavier_gender_results_r2, device = 'png', height = 10, width = 21, unit = 'cm')



