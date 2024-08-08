
# Willkommen --------------------------------------------------------------

#install.packages('pacman')
pacman::p_load(tidyverse, rstatix, 
               broom, performance, see)

# Zusammenfassung Datensatz
summary(mtcars)

# Datensatz speichern
df_cars <- mtcars

# deskr Statistik einzelne Variablen
fivenum(df_cars$mpg)  
mean(df_cars$mpg)
fivenum(df_cars$hp)

df_cars %>% 
  get_summary_stats(mpg, hp)

# Umrechnungen
df_cars <- 
  df_cars %>%
  mutate(lkm = 235.2145/mpg)

# Streudiagramm
df_cars %>% 
  ggplot(aes(x = hp, y = lkm)) + 
  geom_point()

# lineare Regression
lm_obj <- lm(lkm ~ hp, df_cars)

# Zusammenfassung lin Reg
summary(lm_obj)
broom::tidy(lm_obj)

df_cars[28, ]

fitted(lm_obj)
residuals(lm_obj)

lm_res <- broom::augment(lm_obj)

# Residuenplot
lm_res %>% 
  ggplot(aes(x = hp, y = .resid)) + 
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth()


# neues Modell , trans. Präd
df_cars %>% 
  mutate(hp_wurzel = log(hp)) %>% 
  lm(lkm ~ hp_wurzel, data = .) %>% 
  broom::augment() %>% 
  ggplot(aes(x = hp_wurzel, y = lkm)) + 
  geom_point() +
  #geom_hline(yintercept = 0) +
  geom_smooth()

# Modellprüfung
performance::check_model(lm_obj, check = 'linearity')

df_cars %>% 
  mutate(hp_log = log(hp)) %>% 
  lm(lkm ~ hp_log, data = .) %>% 
  performance::check_model(., check = 'linearity')

lmtest::raintest(lm_obj_log, fraction = .5) %>% 
  broom::tidy() %>%
  flex()


lm_res %>% 
  ggplot(aes(.resid)) +
  geom_histogram(bins = 32)

lm_res %>% 
  rstatix::shapiro_test(.resid)

performance::check_normality(lm_obj)

lm_obj_wo_outliers <- 
  df_cars %>% 
  rownames_to_column('Automodell') %>% 
  filter(!Automodell %in% 
           c('Maserati Bora', 
             'Lincoln Continental', 
             'Cadillac Fleetwood')) %>% 
  lm(lkm ~ hp, data = .)

lm_obj_wo_outliers %>% 
  broom::augment() %>% 
  ggplot(aes(.std.resid)) +
  geom_density()

lm_obj_wo_outliers %>% 
  broom::augment() %>% 
  rstatix::shapiro_test(.std.resid)

bc <- MASS::boxcox(lm_obj, plotit = T)


df_cars %>% 
  mutate(log_lkm = log(lkm)) %>% 
  lm(log_lkm ~ hp, data = .) %>% 
  tidy()
  
  augment() %>% 
  ggplot(aes(x=hp, y=log_lkm)) +
  geom_point() +
  geom_smooth(method = 'lm')
  
  ggplot(aes(x = .resid)) +
  geom_histogram()
  
  
  https://github.com/Statistican/Presentations