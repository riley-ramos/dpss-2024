library(ggpubr)

census_results5 <- census4 %>%
  distinct(tractce10, median_home_value, perc_white, racial_majority) %>% 
  mutate(perc_white = perc_white * 100 )
View(census_results5)

model <- lm(median_home_value ~ perc_white, data=census_results5)
summary(model)

# plot perc_white on x-axis, median_home_value on y-axis
census_results5 %>% 
  mutate(perc_white = perc_white * 100 ) %>%
  ggplot(aes(x = perc_white, y = median_home_value)) +
  geom_point(color = "#a6a6a6", size = 1) + # point color and size
  geom_smooth(method = "lm", 
              se = FALSE, 
              color = "#a4343a") +
  stat_cor(method = "pearson",
           label.x = 50,
           label.y = 50) +
  labs(title = "Percent White vs. Median Home Value", x = "% White", y = "Median Home Value") +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16, family = "GothamBold", hjust=0.5),
    axis.title.x = element_text(size = 12, hjust=0.5, family = "GothamBold"),
    axis.title.y = element_text(size = 12, family = "GothamBold"),
    axis.text.x = element_text(size = 8, hjust = 0.5, family = "GothamBook"),
    axis.text.y = element_text(size = 8, family = "GothamBook")
  ) 