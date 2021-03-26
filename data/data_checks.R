# Data checks

library(reshape2)
library(tidyr)
library(ggplot2)
library(gridExtra)


source("kc_read-data.R")

# compare to prior data source
# reshape kc_data to long
kc_data_long <- gather(kc_data, measure, value, cases:deaths)
kc_data_long <- kc_data_long %>%
  group_by(measure) %>%
  mutate(newvalue = value - lag(value)) %>%
  ungroup()
kc_data2_join <- kc_data2 %>%
  mutate(wanewvalue = age1 + age2 + age3 + age4,
         wavalue = cumsum(wanewvalue),
         measure = tolower(Type),
         date = WeekStartDate) %>%
  group_by(measure) %>%
  mutate(wavalue = cumsum(wanewvalue)) %>%
  ungroup() %>%
  select(date, wavalue, wanewvalue, doy, measure)
kc_data_long <- left_join(kc_data_long, kc_data2_join, by = c("date", "measure"))


plot_cases <- kc_data_long %>%
  filter(measure == "cases") %>%
  ggplot(aes(x = doy)) +
  geom_line(aes(y = value)) +
  geom_point(aes(y = wavalue)) +
  ggtitle("Cases")

plot_deaths <- kc_data_long %>%
  filter(measure == "deaths") %>%
  ggplot(aes(x = doy)) +
  geom_line(aes(y = value)) +
  geom_point(aes(y = wavalue)) +
  ggtitle("Deaths")

pdf("newsourcecases.pdf", width = 8, height = 4)
grid.arrange(plot_cases, plot_deaths, ncol = 2)
dev.off()

kc_data_long %>%
  filter(measure == "cases") %>%
  ggplot(aes(x = doy)) +
  geom_line(aes(y = newvalue)) +
  geom_point(aes(y = wanewvalue/7))
