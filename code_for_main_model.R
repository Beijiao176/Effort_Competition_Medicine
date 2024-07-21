library(tidyverse)
library(plm)
library(rstatix)
library(slider)
library(patchwork)
library(ggbreak)
library(ggsci)
library(ggstats)
library(eoffice)


# 
enq_to_chr <- functienq_to_chr <- functienq_to_chr <- function(var_quo) {
  var_label <- rlang::as_label(var_quo)
  return(var_label)
}

# plette and theme setting...
palette_fill = c('#F0F0F0', '#4DBBD5FF', '#E64B35FF')
palette_color = c('#000000', '#000000', '#000000')

theme_wj = function(){
  require(grid)
  theme_bw(base_size = 12)+
    theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.text.x=element_text(color="black"),
      axis.title.y=element_blank(),
      plot.margin = unit(rep(0,4),"cm"),
      legend.title = element_blank(),
      legend.key=element_blank(),   # 图例键为空
      legend.text = element_text(color="black",size=8,face="bold"), # 定义图例文本
      legend.spacing.x=unit(0.1,'cm'), # 定义文本书平距离
      legend.spacing.y=unit(0.1,'cm'), 
      legend.key.width=unit(0.08,'cm'), # 定义图例水平大小
      legend.key.height=unit(0.08,'cm'), # 定义图例垂直大小
      legend.background=element_blank(), # 设置背景为空
      legend.box.background=element_rect(colour="black"), # 图例绘制边框
      legend.position=c(1,1),legend.justification=c(1,1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title=element_text(size=14,face="bold"),
      panel.grid.major.y = element_line(size=1),
      panel.grid.minor.y = element_line(size=1))
}


# read the data.
data_raw = read_csv('.\\data.csv') %>%
  as_tibble()


# the medicine and non-medicine datasets.
data_med = data_raw %>%
  filter(is_med_90==TRUE) # threshold greater than 90.

data_oth = data_raw %>%
  filter(is_not_med==TRUE)


# The main effect modelling.
## r: number of reviews
## s_05: the research activities with the 0.5 year publication period assumption
## s_1: the research activities with the 0.5 year publication period assumption
## s_15: the research activities with the 1.5 year publication period assumption
## s_2: the research activities with the 2 year publication period assumption
## sli (slide): denote the average of the that year and the past two years
## ln: denote the logarithmically transformed variable
## sli_mp: num of publications as main authors
## sli_noca: number of coauthors
## c_cumsum: number of citations accumulation
## p_cumsum: number of publications accumulation
## age_p_cumsum: age * p_cumsum


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
    pdata.frame(index=c('oa_id', 'year')) %>%
    plm(formula,
        data = .,
        effect = 'twoways',
        model = 'within')
  
  num_units <- length(unique(index(model)$oa_id)) # num of samples

  summary(model)
  r_squared_within <- summary(model)$r.squared["rsq"] # r square

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
vars = c('s_05_tc', 's_1_tc', 's_15_tc', 's_2_tc')

result_tibble = tibble()

for (dataset in datasets){
  for (var in vars){ 
    a_result = f_model_benchmark(dataset, var, model_type)
    result_tibble = bind_rows(result_tibble, a_result)
  }
}

result_tibble %>% arrange(dataset)



# sub group model develop
term_year <- new.env() ## a env, (similar to a dict in python)
term_year[["s_1_tc"]] <- "C-1 year"
term_year[["s_15_tc"]] <- "B-1.5 years"
term_year[["s_05_tc"]] <- "D-0.5 years"
term_year[["s_2_tc"]] <- "A-2 years"

formula_vars = '+ sli_mp + sli_p + c_cumsum_ln + age_ln + sli_noca + age_p_cumsum_ln'

sub_group_model = function(data, group_var, term_name){
  
  enq_group_var = enquo(group_var)
  formula <- as.formula(str_c('r ~', term_name, formula_vars))
  if (term_name %in% c('s_15_tc', 's_2_tc')){
    filter_year = 2020
  }
  if (term_name %in% c('s_05_tc', 's_1_tc')){
    filter_year = 2021
  }
  
  resu = data %>% 
    filter(year <= filter_year) %>%
    group_nest(!!enq_group_var) %>% 
    filter(map_int(data, nrow) >= 270) %>% 
    mutate(data = map(data, \(x) pdata.frame(x, index=c('oa_id','year')))) %>% 
    mutate(model = map(data, ~ plm(formula,
                                    data = .x, 
                                    effect = "twoways",
                                    model = "within"))) %>%
    mutate(result = map(model, tidy, conf.int = 0.95)) %>% 
    select(!!enq_group_var, result) %>% 
    unnest(result) %>% 
    filter(term == term_name) %>% 
    mutate(across(c(conf.low, conf.high), ~ round(.x, 2))) %>% 
    mutate(across(c(p.value), ~ round(.x, 3))) %>% 
    mutate(hic_text = sprintf('%s \n [%s, %s]', round(estimate, 3), conf.high, conf.low)) %>% 
    mutate(significal = case_when(
      p.value<=0.05 & estimate>0 ~ 'Statistically Postive Significant',
      p.value<=0.05 & estimate<0 ~ 'Statistically Negative Significant',
      p.value>0.05 ~ 'Statistically Insignificant'
    )) %>%
    drop_na() %>%
    mutate(type=term_year[[term_name]])
  
  return(resu)
}

concat_subgroup = function(...) {
  
  sp2_group_level = sub_group_model(..., 's_05_tc')
  ss2_group_level = sub_group_model(..., 's_15_tc')
  s_group_level = sub_group_model(..., 's_1_tc')
  s2_group_level = sub_group_model(..., 's_2_tc')
  
  args_list <- quos(...)
  enq_col = args_list[[2]]
  
  resu = bind_rows(ss2_group_level, 
                   sp2_group_level, 
                   s_group_level, 
                   s2_group_level) %>%
    mutate(id_text = str_c(!!enq_col, type))
  
  return(resu)
}

creat_pic = function(data, x_low, x_up, ...){

  quos = quos(...)
  data = data %>% arrange(!!quos[[1]])
  
  p_eff = data %>%
    ggplot(aes(estimate, id_text))+
    geom_stripped_rows(width = 4, nudge_y = 2.5)+
    geom_errorbarh(aes(xmax=conf.high, xmin=conf.low, color=significal), linewidth=0.7, height=0.2)+
    geom_point(aes(fill=significal), size=2.2, shape=21)+
    scale_x_continuous(limits=c(x_low, x_up))+
    geom_vline(aes(xintercept=0))+
    xlab('HR(95% CI)') + ylab(' ')+
    scale_fill_manual(values=palette_fill)+
    scale_color_manual(values=palette_color)+
    theme_wj()
  
  p_type = data %>%
    mutate(group="A") %>% 
    ggplot(aes(group, id_text))+
    geom_stripped_rows(width = 4, nudge_y = 2.5)+
    geom_text(aes(group, label=!!quos[[1]]),
              size=3, color="black") +
    theme(panel.grid.major = element_blank(),
          axis.text=element_blank(),
          axis.ticks = element_blank(),
          axis.title.y=element_blank(),
          axis.title=element_text(size=9,face="bold"),
          plot.margin = unit(rep(0,4),"cm"))+
    labs(x="replaced")+
    scale_x_discrete(position = "top")
  
  p_years = data %>% 
    mutate(group="A") %>% 
    ggplot(aes(group, id_text))+
    geom_stripped_rows(width = 4, nudge_y = 2.5)+  
    geom_text(aes(group, label=type),
              size=3,color="black") +
    theme(panel.grid.major = element_blank(),
          axis.text=element_blank(),
          axis.ticks = element_blank(),
          axis.title.y=element_blank(),
          axis.title=element_text(size=9,face="bold"),
          plot.margin = unit(rep(0,4),"cm"))+
    labs(x="Time")+
    scale_x_discrete(position = "top")
  
  p3 = data %>% 
    mutate(group="HR(95% Cl)") %>% 
    ggplot(aes(group, id_text))+
    geom_stripped_rows(width = 4, nudge_y = 2.5)+
    geom_text(aes(group, label=hic_text),size=3,color="black") +
    theme(panel.grid.major = element_blank(),
          axis.text=element_blank(),
          axis.ticks = element_blank(),
          axis.title.y=element_blank(),
          axis.title=element_text(size=9,face="bold"),
          plot.margin = unit(rep(0,4),"cm"))+
    labs(x="HR(95%Cl)")+
    scale_x_discrete(position = "top")
  
  p4 = data %>% 
    mutate(group="A", p.value=as.character(p.value)) %>% 
    ggplot(aes(group, id_text)) +
    geom_stripped_rows(width = 4, nudge_y = 2.5)+
    geom_text(aes(group, label=p.value), size=3,color="black") +
    theme(panel.grid.major = element_blank(),
          axis.text=element_blank(),
          axis.ticks = element_blank(),
          axis.title.y=element_blank(),
          axis.title=element_text(size=9,face="bold"),
          plot.margin = unit(rep(0,4),"cm"))+
    labs(x="P Value")+
    scale_x_discrete(position = "top")
  
  p6 = (p_type+p_years+p3+p4+p_eff)+plot_layout(widths = c(.12,.09,.15,.09,.7))
  
  return(p6)
}


regions_group = data_med %>% 
  concat_subgroup(develop_level)

regions_pic = creat_pic(regions_group, -2.5, 1, develop_level)
# topptx(regions_pic, 'regions_pic.pptx', height=6, width=10)


age_group = data_med %>% 
  group_by(oa_id) %>% 
  mutate(max_age=max(age)) %>% 
  ungroup() %>% 
  mutate(age_class = case_when(
    0<max_age & max_age<15 ~ 'C-Young',
    15<=max_age & max_age<30 ~ 'B-Intermediat',
    max_age>=30 ~ 'A-Senior')) %>%
  concat_subgroup(age_class) %>%
  arrange(age_class)

age_pic = creat_pic(age_group,  -2.5, 2.5, age_class)

# topptx(age_pic, 'age_pic.pptx', height=6, width=10)


sec_dis_group = data_med %>% 
  select(-c(is_med_90, is_not_med, is_med_80, is_med_75)) %>% 
  pivot_longer(cols = starts_with("is_"), names_to = "sec_dis", values_to = "value") %>%
  mutate(sec_dis = sub("is_", "", sec_dis)) %>%
  filter(value != 0) %>%
  select(-value) %>% 
  ungroup() %>% 
  concat_subgroup(., sec_dis) %>%
  arrange(sec_dis) %>%
  mutate(
    conf.low = asinh(conf.low),
    conf.high = asinh(conf.high),
    estimate = asinh(estimate)) %>% # 
  group_by(sec_dis) %>%
  mutate(is_sig = if_else(p.value<=0.1, 1, 0)) %>%
  mutate(sum_sig = sum(is_sig)) %>%
  mutate(is_post = if_else(estimate>0,1,0)) %>%
  mutate(sum_is_post = sum(is_post)) %>%
  mutate(is_neg = if_else(estimate<0,1,0)) %>%
  mutate(sum_is_neg = sum(is_neg))

sec_dis_pic = creat_pic(sec_dis_group, -6, 6, sec_dis)

# topptx(sec_dis_pic, 'sec_dis_pic.pptx', height=6, width=10)





