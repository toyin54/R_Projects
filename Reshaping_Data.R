library(tidyverse)
library(dslabs)

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

head(co2_wide)

co2_tody <-  gather(co2_wide,month,co2,-year)
head(co2_tody)

co2_tody %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
head(dat)

dat_tidy <- dat %>% spread(gender , admitted)
head(dat_tidy)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp

tmp2 <- unite(tmp, column_name, c(key, gender))
head(tmp2)

