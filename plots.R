library(dslabs)
data(olive)
head(olive)
olive$palmitic
plot(olive$palmitic ,olive$palmitoleic)


boxplot(palmitic ~ region, data = olive)
boxplot(palmitoleic ~ region , data = olive)

library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in% c(1970, 2010) & !is.na(dollars_per_day) & !is.na(infant_mortality)) %>%
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +
  geom_text() + 
  scale_x_continuous(trans = "log2") +
  facet_grid(year~.)