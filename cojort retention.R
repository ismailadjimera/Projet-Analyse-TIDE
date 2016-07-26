# loading libraries
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(gridExtra)
# creating data sample
set.seed(10)
cohorts <- data.frame(cohort = paste('cohort', formatC(c(1:36), width=2, format='d', flag='0'), sep = '_'),
                      Y_00 = sample(c(1300:1500), 36, replace = TRUE),
                      Y_01 = c(sample(c(800:1000), 36, replace = TRUE)),
                      Y_02 = c(sample(c(600:800), 24, replace = TRUE), rep(NA, 12)),
                      Y_03 = c(sample(c(400:500), 12, replace = TRUE), rep(NA, 24)))
# simulating seasonality (Black Friday)
cohorts[c(11, 23, 35), 2] <- as.integer(cohorts[c(11, 23, 35), 2] * 1.25)
cohorts[c(11, 23, 35), 3] <- as.integer(cohorts[c(11, 23, 35), 3] * 1.10)
cohorts[c(11, 23, 35), 4] <- as.integer(cohorts[c(11, 23, 35), 4] * 1.07)

# calculating retention rate and preparing data for plotting
df_plot <- melt(cohorts, id.vars = 'cohort', value.name = 'number', variable.name = 'year_of_LT')

df_plot <- df_plot %>%
  group_by(cohort) %>%
  arrange(year_of_LT) %>%
  mutate(number_prev_year = lag(number),
         number_Y_00 = number[which(year_of_LT == 'Y_00')]) %>%
  ungroup() %>%
  mutate(ret_rate_prev_year = number / number_prev_year,
         ret_rate = number / number_Y_00,
         year_cohort = paste(year_of_LT, cohort, sep = '-'))

##### The first way for plotting cycle plot via scaling
# calculating the coefficient for scaling 2nd axis
k <- max(df_plot$number_prev_year[df_plot$year_of_LT == 'Y_01'] * 1.15) / min(df_plot$ret_rate[df_plot$year_of_LT == 'Y_01'])

# retention rate cycle plot
ggplot(na.omit(df_plot), aes(x = year_cohort, y = ret_rate, group = year_of_LT, color = year_of_LT)) +
  theme_bw() +
  geom_point(size = 4) +
  geom_text(aes(label = percent(round(ret_rate, 2))),
            size = 4, hjust = 0.4, vjust = -0.6) +
  # smooth method can be changed (e.g. for "lm")
  geom_smooth(size = 2.5, method = 'loess', color = 'darkred', aes(fill = year_of_LT)) +
  geom_bar(aes(y = number_prev_year / k, fill = year_of_LT), alpha = 0.2, stat = 'identity') +
  geom_bar(aes(y = number / k, fill = year_of_LT), alpha = 0.6, stat = 'identity') +
  geom_text(aes(y = 0, label = cohort), color = 'white', angle = 90, size = 4, hjust = -0.05, vjust = 0.4) +
  geom_text(aes(y = number_prev_year / k, label = number_prev_year),
            angle = 90, size = 4, hjust = -0.1, vjust = 0.4) +
  geom_text(aes(y = number / k, label = number),
            angle = 90, size = 4, hjust = -0.1, vjust = 0.4) +
  theme(legend.position='none',
        plot.title = element_text(size=20,  vjust=2),
        axis.title.x = element_text(size=18 ),
        axis.title.y = element_text(size=18),
        axis.text = element_text(size=16),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = 'Year of Lifetime by Cohorts', y = 'Number of Customers / Retention Rate') +
  ggtitle("Customer Retention Rate - Cycle plot")

##### The second way for plotting cycle plot via multi-plotting
# plot #1 - Retention rate
p1 <- ggplot(na.omit(df_plot), aes(x = year_cohort, y = ret_rate, group = year_of_LT, color = year_of_LT)) +
  theme_bw() +
  geom_point(size = 4) +
  geom_text(aes(label = percent(round(ret_rate, 2))),
            size = 4, hjust = 0.4, vjust = -0.6,) +
  geom_smooth(size = 2.5, method = 'loess', color = 'darkred', aes(fill = year_of_LT)) +
  theme(legend.position='none',
        plot.title = element_text(size=20,vjust=2),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=18),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(y = 'Retention Rate') +
  ggtitle("Customer Retention Rate - Cycle plot")

# plot #2 - number of customers
p2 <- ggplot(na.omit(df_plot), aes(x = year_cohort, group = year_of_LT, color = year_of_LT)) +
  theme_bw() +
  geom_bar(aes(y = number_prev_year, fill = year_of_LT), alpha = 0.2, stat = 'identity') +
  geom_bar(aes(y = number, fill = year_of_LT), alpha = 0.6, stat = 'identity') +
  geom_text(aes(y = number_prev_year, label = number_prev_year),
            angle = 90, size = 4, hjust = -0.1, vjust = 0.4) +
  geom_text(aes(y = number, label = number),
            angle = 90, size = 4, hjust = -0.1, vjust = 0.4) +
  geom_text(aes(y = 0, label = cohort), color = 'white', angle = 90, size = 4, hjust = -0.05, vjust = 0.4) +
  theme(legend.position='none',
        plot.title = element_text(size=20,vjust=2),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0, max(df_plot$number_Y_00 * 1.1))) +
  labs(x = 'Year of Lifetime by Cohorts', y = 'Number of Customers')

# multiplot
grid.arrange(p1, p2, ncol = 1)
# retention rate bubble chart
ggplot(na.omit(df_plot), aes(x = cohort, y = ret_rate, group = cohort, color = year_of_LT)) +
  theme_bw() +
  scale_size(range = c(15, 40)) +
  geom_line(size = 2, alpha = 0.3) +
  geom_point(aes(size = number_prev_year), alpha = 0.3) +
  geom_point(aes(size = number), alpha = 0.8) +
  geom_smooth(linetype = 2, size = 2, method = 'loess', aes(group = year_of_LT, fill = year_of_LT), alpha = 0.2) +
  geom_text(aes(label = paste0(number, '/', number_prev_year, '\n', percent(round(ret_rate, 2)))),
            color = 'white', size = 3, hjust = 0.5, vjust = 0.5) +
  theme(legend.position='none',
        plot.title = element_text(size=20, vjust=2),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        axis.text = element_text(size=16),
        axis.text.x = element_text(size=10, angle=90, hjust=.5, vjust=.5),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = 'Cohorts', y = 'Retention Rate by Year of Lifetime') +
  ggtitle("Customer Retention Rate - Bubble chart")

# retention rate falling drops chart
ggplot(df_plot, aes(x = cohort, y = ret_rate, group = cohort, color = year_of_LT)) +
  theme_bw() +
  scale_size(range = c(15, 40)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(size = 2, alpha = 0.3) +
  geom_point(aes(size = number), alpha = 0.8) +
  geom_text(aes(label = paste0(number, '\n', percent(round(ret_rate, 2)))),
            color = 'white', size = 3, hjust = 0.5, vjust = 0.5) +
  theme(legend.position='none',
        plot.title = element_text(size=20, vjust=2),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        axis.text = element_text(size=16),
        axis.text.x = element_text(size=10, angle=90, hjust=.5, vjust=.5),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = 'Cohorts', y = 'Retention Rate by Year of Lifetime') +
  ggtitle("Customer Retention Rate - Falling Drops chart")
