library(tidyverse)
library(plm)
library(rstatix)
library(slider)
library(patchwork)
library(ggbreak)
library(ggsci)
library(ggstats)
library(eoffice)


# read the data.
data_raw = read_csv('C:\\Users\\21217\\Desktop\\JMIR_0615\\final_model_r1\\data_r3_sec80.csv') %>%
  as_tibble()



# Test: when the main authors definition contains the last author.
data_med = data_raw %>%
  filter(is_med_90==TRUE)

data_oth = data_raw %>%
  filter(is_not_med==TRUE)

f_model_benchmark = function(dataset, var, model_type){
  
  sym_dataset = sym(dataset)
  term_name = var
  
  formula_vars = '+ sli_mp_last + sli_p + c_cumsum_ln + age + sli_noca + age_p_cumsum_ln'
  formula <- as.formula(str_c('r ~', term_name, formula_vars)) # create the formula
  
  if (var %in% c('s_15_tc_last', 's_2_tc_last')){
    filter_year = 2020
  }
  if (var %in% c('s_05_tc_last', 's_1_tc_last')){
    filter_year = 2021
  }
  
  model = eval(sym_dataset) %>% 
    filter(year <= filter_year) %>%
    pdata.frame(index=c('oa_id','year')) %>%
    plm(formula,
        data = .,
        effect = 'twoways',
        model = 'within')
  
  num_units <- length(unique(index(model)$oa_id))

  summary(model)
  r_squared_within <- summary(model)$r.squared["rsq"]

  summ = tidy(model, conf.int = TRUE) %>%
    filter(term==var) %>% 
    mutate(sign = case_when(
      p.value > 0.1 ~ 'No',
      p.value > 0.05 & p.value <= 0.1 ~ '†',
      p.value > 0.01 & p.value <= 0.05 ~ '*',
      p.value > 0.001 & p.value <= 0.01 ~ '**',
      p.value <= 0.001 ~ '***',
    )) %>% 
    mutate(r_squared = r_squared_within) %>% 
    mutate(across(c(p.value, estimate, conf.low, conf.high, r_squared),
                  \(x) round(x, 3))) %>% 
    select(-std.error, -statistic) %>% 
    mutate(dataset=dataset) %>%
    mutate(var=var) %>% 
    mutate(N=num_units)
  
  return(summ)
}

datasets = c('data_med', 'data_oth')
vars = c('s_05_tc_last', 's_1_tc_last', 's_15_tc_last', 's_2_tc_last')

result_tibble = tibble()

for (dataset in datasets){
  for (var in vars){ 
    a_result = f_model_benchmark(dataset, var, model_type)
    result_tibble = bind_rows(result_tibble, a_result)
  }
}

result_tibble %>% arrange(dataset)



# Test: when the threshold for defining the medical authors == 80 or 75.

data_med_80 = data_raw %>%
  filter(is_med_80==TRUE)

data_med_75 = data_raw %>%
  filter(is_med_75==TRUE)

f_model_benchmark = function(dataset, var, model_type){
  
  sym_dataset = sym(dataset)
  term_name = var
  
  formula_vars = '+ sli_mp + sli_p + c_cumsum_ln + age + sli_noca + age_p_cumsum_ln'
  formula <- as.formula(str_c('r ~', term_name, formula_vars)) # create the formula
  
  if (var %in% c('s_15_tc', 's_2_tc')){
    filter_year = 2020
  }
  if (var %in% c('s_05_tc', 's_1_tc')){
    filter_year = 2021
  }
  
  model = eval(sym_dataset) %>% 
    filter(year <= filter_year) %>%
    pdata.frame(index=c('oa_id','year')) %>%
    plm(formula,
        data = .,
        effect = 'twoways',
        model = 'within')
  
  num_units <- length(unique(index(model)$oa_id))
  
  summary(model)
  r_squared_within <- summary(model)$r.squared["rsq"]
  
  summ = tidy(model, conf.int = TRUE) %>%
    filter(term==var) %>% 
    mutate(sign = case_when(
      p.value > 0.1 ~ 'No',
      p.value > 0.05 & p.value <= 0.1 ~ '†',
      p.value > 0.01 & p.value <= 0.05 ~ '*',
      p.value > 0.001 & p.value <= 0.01 ~ '**',
      p.value <= 0.001 ~ '***',
    )) %>% 
    mutate(r_squared = r_squared_within) %>% 
    mutate(across(c(p.value, estimate, conf.low, conf.high, r_squared),
                  \(x) round(x, 3))) %>% 
    select(-std.error, -statistic) %>% 
    mutate(dataset=dataset) %>%
    mutate(var=var) %>% 
    mutate(N=num_units)
  
  return(summ)
}

datasets = c('data_med_80', 'data_med_75')
vars = c('s_05_tc', 's_1_tc', 's_15_tc', 's_2_tc')

result_tibble = tibble()

for (dataset in datasets){
  for (var in vars){ 
    a_result = f_model_benchmark(dataset, var, model_type)
    result_tibble = bind_rows(result_tibble, a_result)
  }
}

result_tibble %>% arrange(dataset)



# Test: Hausman test, suggesting the fixed model(in economics) should be selected.
data_med_90 = data_raw %>%
  filter(is_med_90==TRUE)

ht_formula = as.formula('r ~ s_15_tc + sli_mp_last + sli_p + c_cumsum_ln + age + sli_noca + age_p_cumsum_ln')

model_fe = data_med_90 %>% 
  pdata.frame(index=c('oa_id','year')) %>%
  plm(ht_formula, data =., model = "within")

model_re = data_med_90 %>% 
  pdata.frame(index=c('oa_id','year')) %>%
  plm(ht_formula, data = ., model = "random")

phtest(model_fe, model_re)



# test: test for different author weighting algorithm. 
# nowei: no weighting, avg: avg weighting.
data_oth = data_raw %>%
  filter(is_not_med==TRUE)

data_med_90 = data_raw %>%
  filter(is_med_90==TRUE)

f_model_benchmark = function(dataset, var, type){
  type = type
  sym_dataset = sym(dataset)
  term_name = str_c(var, '_', type)
  
  formula_vars = '+ sli_mp + sli_p + c_cumsum_ln + age + sli_noca + age_p_cumsum_ln'
  formula <- as.formula(str_c('r ~', term_name, formula_vars)) # create the formula

  if (term_name %in% c(str_c('s_15_', type), str_c('s_2_', type))){
    filter_year = 2020
  }
  if (term_name %in% c(str_c('s_05_', type), str_c('s_1_', type))){
    filter_year = 2021
  }
  
  model = eval(sym_dataset) %>% 
    filter(year <= filter_year) %>%
    pdata.frame(index=c('oa_id','year')) %>%
    plm(formula,
        data = .,
        effect = 'twoways',
        model = 'within')
  
  num_units <- length(unique(index(model)$oa_id))
  
  summary(model)
  r_squared_within <- summary(model)$r.squared["rsq"]
  
  summ = tidy(model, conf.int = TRUE) %>%
    filter(term==term_name) %>% 
    mutate(sign = case_when(
      p.value > 0.1 ~ 'No',
      p.value > 0.05 & p.value <= 0.1 ~ '†',
      p.value > 0.01 & p.value <= 0.05 ~ '*',
      p.value > 0.001 & p.value <= 0.01 ~ '**',
      p.value <= 0.001 ~ '***',
    )) %>% 
    mutate(r_squared = r_squared_within) %>% 
    mutate(across(c(p.value, estimate, conf.low, conf.high, r_squared),
                  \(x) round(x, 3))) %>% 
    select(-std.error, -statistic) %>% 
    mutate(dataset=dataset) %>%
    mutate(var=var) %>% 
    mutate(N=num_units)
  
  return(summ)
}

datasets = c('data_med_90', 'data_oth')
vars = c('s_05', 's_1', 's_15', 's_2')
types = c('tc', 'nowei', 'avg')

result_tibble = tibble()

for (dataset in datasets){
  for (var in vars) {
    for (type in types) { 
      a_result = f_model_benchmark(dataset, var, type)
      result_tibble = bind_rows(result_tibble, a_result)
  }
 }
}

r=result_tibble %>% arrange(term)



# test: quantify the free rider; test for those who are not free rider.
data_oth = data_raw %>%
  filter(is_not_med==TRUE)

data_med_90 = data_raw %>%
  filter(is_med_90==TRUE)

## test: quantify the free rider
data_rp_prop = data_med_90 %>% 
  mutate(r_sum_all = sum(r),
         p_sum_all = sum(p)) %>% 
  summarise(r_sum = sum(r),
            p_sum = sum(p),
            r_sum_all = mean(r_sum_all),
            p_sum_all = mean(p_sum_all),
            .by=aid) %>% 
  mutate(r_prop = r_sum/r_sum_all,
         p_prop = p_sum/p_sum_all) %>%
  arrange(-r_prop) %>% 
  mutate(r_prop_cumsum = cumsum(r_prop)) %>% 
  mutate(p_prop_cumsum = cumsum(p_prop)) %>% 
  mutate(index=c(1: nrow(data_med_90)))
  
x_value <- 0.75
y1 <- data_rp_prop$p_prop_cumsum[which.min(abs(data_rp_prop$r_prop_cumsum - x_value))]

p_ridder_rp = data_rp_prop %>% 
  ggplot() +
  geom_line(aes(r_prop_cumsum, p_prop_cumsum)) +
  geom_vline(xintercept = 0.75, linetype = "dashed") + 
  geom_hline(yintercept = y1, linetype = "dashed") + 
  geom_text(data = data_rp_prop[data_rp_prop$r_prop_cumsum == 0.25, ],
            aes(label = paste("y1 =", round(y1, 2)), x =0.15, y=y1+0.02), vjust=-0.5) +  
  theme_bw()

data_rp_prop = data_med_90 %>% 
  mutate(r_sum_all = sum(r),
         n_sum_all = n()) %>% 
  summarise(r_sum = sum(r),
            n_sum = 11,
            r_sum_all = mean(r_sum_all),
            n_sum_all = mean(n_sum_all),
            .by=aid) %>% 
  mutate(r_prop = r_sum/r_sum_all,
         n_prop = n_sum/n_sum_all) %>%
  arrange(-r_prop) %>% 
  mutate(r_prop_cumsum = cumsum(r_prop)) %>% 
  mutate(n_prop_cumsum = cumsum(n_prop)) %>% 
  mutate(index=c(1: nrow(data_med_90)))

x_value <- 0.75
y2 <- data_rp_prop$n_prop_cumsum[which.min(abs(data_rp_prop$r_prop_cumsum - x_value))]

p_ridder_rn = data_rp_prop %>% 
  ggplot() +
  geom_line(aes(r_prop_cumsum, n_prop_cumsum)) +
  geom_vline(xintercept = 0.75, linetype = "dashed") +
  geom_hline(yintercept = y2, linetype = "dashed") + 
  geom_text(data = data_rp_prop[data_rp_prop$r_prop_cumsum == 0.25, ],
            aes(label = paste("y2 =", round(y2, 2)), x =0.15, y=y2+0.02), vjust=-0.5) +  
  theme_bw()

# topptx(p_ridder_rn+p_ridder_rp, "ridder.pptx")

## rider robust moel
oaid_with_noridder = data_med %>% 
  mutate(r_sum_all = sum(r),
         p_sum_all = sum(p)) %>% 
  summarise(r_sum = sum(r),
            p_sum = sum(p),
            r_sum_all = mean(r_sum_all),
            p_sum_all = mean(p_sum_all),
            .by=oa_id) %>% 
  mutate(r_prop = r_sum/r_sum_all,
         p_prop = p_sum/p_sum_all) %>%
  arrange(-r_prop) %>% 
  mutate(r_prop_cumsum = cumsum(r_prop)) %>% 
  mutate(p_prop_cumsum = cumsum(p_prop)) %>% 
  filter(r_prop_cumsum<=0.5) %>% 
  pull(oa_id)

data_not_ridder = data_med %>%
  filter(oa_id %in% oaid_with_noridder)

f_model_benchmark = function(dataset, var, model_type){
  
  sym_dataset = sym(dataset)
  term_name = var
  
  formula_vars = '+ sli_mp + sli_p + c_cumsum_ln + age + sli_noca + age_p_cumsum_ln'
  formula <- as.formula(str_c('r ~', term_name, formula_vars)) # create the formula
  
  if (var %in% c('s_15_tc', 's_2_tc')){
    filter_year = 2020
  }
  if (var %in% c('s_05_tc', 's_1_tc')){
    filter_year = 2021
  }
  
  model = eval(sym_dataset) %>% 
    filter(year <= filter_year) %>%
    pdata.frame(index=c('oa_id','year')) %>%
    plm(formula,
        data = .,
        effect = 'twoways',
        model = 'within')
  
  num_units <- length(unique(index(model)$oa_id))
  
  summary(model)
  r_squared_within <- summary(model)$r.squared["rsq"]
  
  summ = tidy(model, conf.int = TRUE) %>%
    filter(term==var) %>% 
    mutate(sign = case_when(
      p.value > 0.1 ~ 'No',
      p.value > 0.05 & p.value <= 0.1 ~ '†',
      p.value > 0.01 & p.value <= 0.05 ~ '*',
      p.value > 0.001 & p.value <= 0.01 ~ '**',
      p.value <= 0.001 ~ '***',
    )) %>% 
    mutate(r_squared = r_squared_within) %>% 
    mutate(across(c(p.value, estimate, conf.low, conf.high, r_squared),
                  \(x) round(x, 3))) %>% 
    select(-std.error, -statistic) %>% 
    mutate(dataset=dataset) %>%
    mutate(var=var) %>% 
    mutate(N=num_units)
  
  return(summ)
}

datasets = c('data_not_ridder')
vars = c('s_05_tc', 's_1_tc', 's_15_tc', 's_2_tc')

result_tibble = tibble()

for (dataset in datasets){
  for (var in vars){ 
    a_result = f_model_benchmark(dataset, var, model_type)
    result_tibble = bind_rows(result_tibble, a_result)
  }
}

result_tibble %>% arrange(dataset)



# test: unit root test(ips).
data_oth = data_raw %>%
  filter(is_not_med==TRUE)

data_med_90 = data_raw %>%
  filter(is_med_90==TRUE)

p_data_med_90 = data_med_90 %>% 
  pdata.frame(index = c("oa_id", "year"))

p_data_oth = data_oth %>% 
  pdata.frame(index = c("oa_id", "year")) 

purtest(p_data_med_90['r'], test='ips', exo='trend')
purtest(p_data_oth['r'], test='ips', exo='trend')

purtest(p_data_med_90['s_15_tc'], test='ips', exo='trend')
purtest(p_data_oth['s_15_tc'], test='ips', exo='trend')

purtest(p_data_med_90['s_05_tc'], test='ips', exo='trend')
purtest(p_data_oth['s_05_tc'], test='ips', exo='trend')

purtest(p_data_med_90['s_1_tc'], test='ips', exo='trend')
purtest(p_data_oth['s_1_tc'], test='ips', exo='trend')

purtest(p_data_med_90['s_2_tc'], test='ips', exo='trend')
purtest(p_data_oth['s_2_tc'], test='ips', exo='trend')



# the result are independent of samle size.
set.seed(1234)
sampled_ids <- sample(unique(data_oth$oa_id), 3000)

sampled_data_oth <- data_oth %>%
  filter(oa_id %in% sampled_ids)

f_model_benchmark = function(dataset, var, model_type){
  
  sym_dataset = sym(dataset)
  term_name = var
  
  formula_vars = '+ sli_mp + sli_p + c_cumsum_ln + age + sli_noca + age_p_cumsum_ln'
  formula <- as.formula(str_c('r ~', term_name, formula_vars)) # create the formula
  
  if (var %in% c('s_15_tc', 's_2_tc')){
    filter_year = 2020
  }
  if (var %in% c('s_05_tc', 's_1_tc')){
    filter_year = 2021
  }
  
  model = eval(sym_dataset) %>% 
    filter(year <= filter_year) %>%
    pdata.frame(index=c('oa_id','year')) %>%
    plm(formula,
        data = .,
        effect = 'twoways',
        model = 'within')
  
  num_units <- length(unique(index(model)$oa_id))
  
  summary(model)
  r_squared_within <- summary(model)$r.squared["rsq"]
  
  summ = tidy(model, conf.int = TRUE) %>%
    filter(term==var) %>% 
    mutate(sign = case_when(
      p.value > 0.1 ~ 'No',
      p.value > 0.05 & p.value <= 0.1 ~ '†',
      p.value > 0.01 & p.value <= 0.05 ~ '*',
      p.value > 0.001 & p.value <= 0.01 ~ '**',
      p.value <= 0.001 ~ '***',
    )) %>% 
    mutate(r_squared = r_squared_within) %>% 
    mutate(across(c(p.value, estimate, conf.low, conf.high, r_squared),
                  \(x) round(x, 3))) %>% 
    select(-std.error, -statistic) %>% 
    mutate(dataset=dataset) %>%
    mutate(var=var) %>% 
    mutate(N=num_units)
  
  return(summ)
}

datasets = c('sampled_data_oth')
vars = c('s_05_tc', 's_1_tc', 's_15_tc', 's_2_tc')

result_tibble = tibble()

for (dataset in datasets){
  for (var in vars){ 
    a_result = f_model_benchmark(dataset, var, model_type)
    result_tibble = bind_rows(result_tibble, a_result)
  }
}

result_tibble %>% arrange(dataset)


# test: increase the credit of female and young scholars to easy bias
data_med = data_raw %>%
  filter(is_med_90==TRUE)

data_oth = data_raw %>%
  filter(is_not_med==TRUE)

data_med_ga = data_med_90 %>% 
  mutate(
    s_05_tc = if_else(gender_p > 0.9 | is.na(gender_p) | age < 15, s_05_tc * 1.1, s_05_tc*0.9),
    s_1_tc = if_else(gender_p > 0.9 | is.na(gender_p) | age < 15, s_1_tc * 1.1, s_1_tc*0.9),
    s_15_tc = if_else(gender_p > 0.9 | is.na(gender_p) | age < 15, s_15_tc * 1.1, s_15_tc*0.9),
    s_2_tc = if_else(gender_p > 0.9 | is.na(gender_p) | age < 15, s_2_tc * 1.1, s_2_tc*0.9),
    
    s_05_tc = if_else(is.na(gender_p), s_05_tc * 0.909, s_05_tc),
    s_1_tc = if_else(is.na(gender_p), s_1_tc * 0.909, s_1_tc),
    s_15_tc = if_else(is.na(gender_p), s_15_tc * 0.909, s_15_tc),
    s_2_tc = if_else(is.na(gender_p), s_2_tc * 0.909, s_2_tc)
  )

data_oth_ga = data_oth %>% 
  mutate(
    s_05_tc = if_else(gender_p > 0.9 | is.na(gender_p) | age < 15, s_05_tc * 1.1, s_05_tc*0.9),
    s_1_tc = if_else(gender_p > 0.9 | is.na(gender_p) | age < 15, s_1_tc * 1.1, s_1_tc*0.9),
    s_15_tc = if_else(gender_p > 0.9 | is.na(gender_p) | age < 15, s_15_tc * 1.1, s_15_tc*0.9),
    s_2_tc = if_else(gender_p > 0.9 | is.na(gender_p) | age < 15, s_2_tc * 1.1, s_2_tc*0.9),
    
    s_05_tc = if_else(is.na(gender_p), s_05_tc * 0.909, s_05_tc),
    s_1_tc = if_else(is.na(gender_p), s_1_tc * 0.909, s_1_tc),
    s_15_tc = if_else(is.na(gender_p), s_15_tc * 0.909, s_15_tc),
    s_2_tc = if_else(is.na(gender_p), s_2_tc * 0.909, s_2_tc)
  )


data_med_g = data_med_90 %>% 
  mutate(
    s_05_tc = if_else(gender_p > 0.9 | is.na(gender_p), s_05_tc * 1.1, s_05_tc*0.9),
    s_1_tc = if_else(gender_p > 0.9 | is.na(gender_p), s_1_tc * 1.1, s_1_tc*0.9),
    s_15_tc = if_else(gender_p > 0.9 | is.na(gender_p), s_15_tc * 1.1, s_15_tc*0.9),
    s_2_tc = if_else(gender_p > 0.9 | is.na(gender_p), s_2_tc * 1.1, s_2_tc*0.9),
    
    s_05_tc = if_else(is.na(gender_p), s_05_tc * 0.909, s_05_tc),
    s_1_tc = if_else(is.na(gender_p), s_1_tc * 0.909, s_1_tc),
    s_15_tc = if_else(is.na(gender_p), s_15_tc * 0.909, s_15_tc),
    s_2_tc = if_else(is.na(gender_p), s_2_tc * 0.909, s_2_tc)
  )

data_oth_g = data_oth %>% 
  mutate(
    s_05_tc = if_else(gender_p > 0.9 | is.na(gender_p), s_05_tc * 1.1, s_05_tc*0.9),
    s_1_tc = if_else(gender_p > 0.9 | is.na(gender_p), s_1_tc * 1.1, s_1_tc*0.9),
    s_15_tc = if_else(gender_p > 0.9 | is.na(gender_p), s_15_tc * 1.1, s_15_tc*0.9),
    s_2_tc = if_else(gender_p > 0.9 | is.na(gender_p), s_2_tc * 1.1, s_2_tc*0.9),
    
    s_05_tc = if_else(is.na(gender_p), s_05_tc * 0.909, s_05_tc),
    s_1_tc = if_else(is.na(gender_p), s_1_tc * 0.909, s_1_tc),
    s_15_tc = if_else(is.na(gender_p), s_15_tc * 0.909, s_15_tc),
    s_2_tc = if_else(is.na(gender_p), s_2_tc * 0.909, s_2_tc)
  )

data_med_a = data_med_90 %>% 
  mutate(
    s_05_tc = if_else(age < 15, s_05_tc * 1.1, s_05_tc*0.9),
    s_1_tc = if_else(age < 15, s_1_tc * 1.1, s_1_tc*0.9),
    s_15_tc = if_else(age < 15, s_15_tc * 1.1, s_15_tc*0.9),
    s_2_tc = if_else(age < 15, s_2_tc * 1.1, s_2_tc*0.9)
  )

data_oth_a = data_oth %>% 
  mutate(
    s_05_tc = if_else(age < 15, s_05_tc * 1.1, s_05_tc*0.9),
    s_1_tc = if_else(age < 15, s_1_tc * 1.1, s_1_tc*0.9),
    s_15_tc = if_else(age < 15, s_15_tc * 1.1, s_15_tc*0.9),
    s_2_tc = if_else(age < 15, s_2_tc * 1.1, s_2_tc*0.9)
  )

f_model_benchmark = function(dataset, var, model_type){
  
  sym_dataset = sym(dataset)
  term_name = var
  
  formula_vars = '+ sli_mp + sli_p + c_cumsum_ln + age + sli_noca + age_p_cumsum_ln'
  formula <- as.formula(str_c('r ~', term_name, formula_vars)) # create the formula
  
  if (var %in% c('s_15_tc', 's_2_tc')){
    filter_year = 2020
  }
  if (var %in% c('s_05_tc', 's_1_tc')){
    filter_year = 2021
  }
  
  model = eval(sym_dataset) %>% 
    filter(year <= filter_year) %>%
    pdata.frame(index=c('oa_id','year')) %>%
    plm(formula,
        data = .,
        effect = 'twoways',
        model = 'within')
  
  num_units <- length(unique(index(model)$oa_id))
  
  summary(model)
  r_squared_within <- summary(model)$r.squared["rsq"]
  
  summ = tidy(model, conf.int = TRUE) %>%
    filter(term==var) %>% 
    mutate(sign = case_when(
      p.value > 0.1 ~ 'No',
      p.value > 0.05 & p.value <= 0.1 ~ '†',
      p.value > 0.01 & p.value <= 0.05 ~ '*',
      p.value > 0.001 & p.value <= 0.01 ~ '**',
      p.value <= 0.001 ~ '***',
    )) %>% 
    mutate(r_squared = r_squared_within) %>% 
    mutate(across(c(p.value, estimate, conf.low, conf.high, r_squared),
                  \(x) round(x, 3))) %>% 
    select(-std.error, -statistic) %>% 
    mutate(dataset=dataset) %>%
    mutate(var=var) %>% 
    mutate(N=num_units)
  
  return(summ)
}

datasets = c('data_oth_ga', 'data_med_ga', 'data_oth_g', 'data_med_g', 'data_oth_a', 'data_med_a')
vars = c('s_05_tc', 's_1_tc', 's_15_tc', 's_2_tc')

result_tibble = tibble()

for (dataset in datasets){
  for (var in vars){ 
    a_result = f_model_benchmark(dataset, var, model_type)
    result_tibble = bind_rows(result_tibble, a_result)
  }
}

r = result_tibble %>% arrange(dataset)



# The descriptive analysis
data_med = data_raw %>%
  filter(is_med_90==TRUE)

data_med %>% 
  group_by(oa_id) %>% 
  mutate(max_age = max(age)) %>% 
  ungroup() %>% 
  select(c(s_1_tc, s_05_tc, s_15_tc, s_2_tc, r, p, max_age)) %>% 
  get_summary_stats() %>% 
  select(c(variable, n, mean, sd))

data_oth %>% 
  group_by(oa_id) %>% 
  mutate(max_age = max(age)) %>% 
  ungroup() %>% 
  select(c(s_1_tc, s_05_tc, s_15_tc, s_2_tc, r, p, max_age)) %>% 
  get_summary_stats() %>% 
  select(c(variable, n, mean, sd))

data_med %>% 
  group_by(oa_id) %>% 
  mutate(max_age=max(age)) %>% 
  ungroup() %>% 
  mutate(age_class = case_when(
    0<=max_age & max_age<15 ~ 'C-Young',
    15<=max_age & max_age<30 ~ 'B-Intermediat',
    max_age>=30 ~ 'A-Senior')) %>% 
  select(age_class) %>% 
  summarise(n=n()/11, .by=age_class)

data_oth %>% 
  group_by(oa_id) %>% 
  mutate(max_age=max(age)) %>% 
  ungroup() %>% 
  mutate(age_class = case_when(
    0<max_age & max_age<15 ~ 'C-Young',
    15<=max_age & max_age<30 ~ 'B-Intermediat',
    max_age>=30 ~ 'A-Senior')) %>% 
  select(age_class) %>% 
  summarise(n=n()/11, .by=age_class)


## count of each sec dis
data_med %>% 
  select(-c(is_med_90, is_not_med, is_med_80, is_med_75)) %>% 
  pivot_longer(cols = starts_with("is_"), names_to = "sec_dis", values_to = "value") %>%
  mutate(sec_dis = sub("is_", "", sec_dis)) %>%
  filter(value != 0) %>% 
  summarise(n=n()/11, .by = sec_dis)


#  count of gender
data_med %>% 
  mutate(gender=if_else(gender_p>.8, "Female", "Male")) %>% 
  summarise(n=n()/11, .by=gender)


data_oth %>% 
  mutate(gender=if_else(gender_p>.8, "Female", "Male")) %>% 
  summarise(n=n()/11, .by=gender)


#  count of counties
data_med %>% 
  select(develop_level) %>% 
  summarise(n=n()/11, .by=develop_level)

data_oth %>% 
  select(develop_level) %>% 
  summarise(n=n()/11, .by=develop_level)


