---
title: "Supplement 3"
subtitle: 'Descriptive statistics on participant follow-up'
author: 'Peter Kamerman and Prinisha Pillay'
date: "17 November 2018"
---



----

This analysis describes the follow-up pattern of participants in the study.

----

# Import data

```r
data <- read_rds('data-clean/clean_data.rds') %>% 
    # Select columns
    select(ID, visit_number, visit_day, visit_months, hivsn_present)
```

# Process data
Add a column indicating whether a participant developed SN at any time during the follow-up period.

```r
# Identify andf extract information on SN development (at anytime) 
# by looking at the presence of SN at the final visit
data_sn <- data %>%
    select(ID, visit_number, hivsn_present) %>%
    group_by(ID) %>%
    mutate(max_visit = max(visit_number)) %>%
    filter(visit_number == max_visit) %>%
    select(ID, hivsn_present) %>%
    rename(sn = hivsn_present)

# Join data_sn to data
data %<>%
    left_join(data_sn) 
```

# Inspect data

```r
# Dimensions 
dim(data) 
```

```
## [1] 273   6
```

```r
# Column names
names(data)
```

```
## [1] "ID"            "visit_number"  "visit_day"     "visit_months" 
## [5] "hivsn_present" "sn"
```

```r
# Head and tail
head(data)
```

```
## # A tibble: 6 x 6
##   ID    visit_number visit_day visit_months hivsn_present sn   
##   <chr>        <int>     <int>        <dbl> <fct>         <fct>
## 1 001              1         0            0 no            no   
## 2 001              2        40            1 no            no   
## 3 001              3       210            7 no            no   
## 4 002              1         0            0 no            no   
## 5 002              2        71            2 no            no   
## 6 002              3       191            6 no            no
```

```r
tail(data)
```

```
## # A tibble: 6 x 6
##   ID    visit_number visit_day visit_months hivsn_present sn   
##   <chr>        <int>     <int>        <dbl> <fct>         <fct>
## 1 082              1         0            0 no            no   
## 2 082              2        77            3 no            no   
## 3 082              3       186            6 no            no   
## 4 083              1         0            0 no            no   
## 5 083              2        75            2 no            no   
## 6 083              3       169            6 no            no
```

```r
# Data structure
glimpse(data)
```

```
## Observations: 273
## Variables: 6
## $ ID            <chr> "001", "001", "001", "002", "002", "002", "003",...
## $ visit_number  <int> 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, ...
## $ visit_day     <int> 0, 40, 210, 0, 71, 191, 0, 57, 207, 0, 84, 189, ...
## $ visit_months  <dbl> 0, 1, 7, 0, 2, 6, 0, 2, 7, 0, 3, 6, 0, 2, 6, 0, ...
## $ hivsn_present <fct> no, no, no, no, no, no, no, no, no, no, no, no, ...
## $ sn            <fct> no, no, no, no, no, no, no, no, no, no, no, no, ...
```

----

# Clinic visits 

## Whole cohort

```r
data %>%
    # Process data for plotting
    group_by(ID) %>%
    mutate(visit = ifelse(visit_day > 0,
                          yes = 1,
                          no = 0),
           visit_count = cumsum(visit),
           max_visits = max(visit_count),
           max_duration = max(visit_day)) %>%
    ungroup() %>%
    select(ID, visit_day, visit_months, 
           visit_count, max_visits, 
           max_duration) %>%
    # Clean-up and order dataframe
    arrange(max_duration, max_visits, desc(ID)) %>%
    mutate(ID = as_factor(ID),
           visit_count = as.character(visit_count)) %>%
    complete(ID, visit_months) %>%
    # Add a dummy variable for faceting
    bind_cols(facet = c(rep('Panel 1', 420), rep('Panel 2', 832-420))) %>%
    # Clean-up and order dataframe
    filter(complete.cases(.)) %>%
    # Plot
    ggplot(data = .) +
    aes(x = visit_day,
        y = ID) +
    geom_path(colour = '#888888') +
    geom_point(aes(fill = visit_count),
               shape = 21, 
               size = 3) +
    scale_x_continuous(breaks = seq(0, 250, 25)) +
    scale_fill_viridis_d(name = 'Visit count: ',
                         direction = -1) +
    labs(title = 'Timing of clinic visits',
         subtitle = 'Visit number coded by fill colour',
         y = 'Participant ID',
         x = 'Days') +
    facet_wrap(~facet, 
               ncol = 2, 
               scales = 'free_y') +
    theme(legend.position = 'top',
          legend.margin = margin(t = -0.3, 
                                 r = 0, 
                                 b = -0.3, 
                                 l = 0, 'lines'),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank())
```

<img src="./figures/suppl_03-follow-up-analysis/visit_full-1.png" width="672" style="display: block; margin: auto;" />

## By SN status

```r
data_plot <- data %>%
    # Process data for plotting
    group_by(ID) %>%
    mutate(visit = ifelse(visit_day > 0,
                          yes = 1,
                          no = 0),
           visit_count = cumsum(visit),
           max_visits = max(visit_count),
           max_duration = max(visit_day)) %>%
    ungroup() %>%
    select(ID, hivsn_present, visit_day, visit_months, 
           visit_count, max_visits, 
           max_duration, sn) %>%
    # Clean-up and order dataframe
    arrange(max_duration, max_visits, desc(ID)) %>%
    mutate(ID = as_factor(ID),
           visit_count = as.character(visit_count)) %>%
    complete(ID, visit_months) %>%
    # Clean-up and order dataframe
    filter(complete.cases(.)) %>%
    # Recode facetting column
    mutate(sn = as.character(sn),
           sn = ifelse(sn == 'yes',
                       yes = 'Neuropathy developed',
                       no = 'No neuropathy'),
           sn = factor(sn,
                       levels = c('No neuropathy', 'Neuropathy developed'),
                       ordered = TRUE)) %>% 
    mutate(hivsn_present = str_to_title(hivsn_present))

# Plot
ggplot(data = data_plot) +
    aes(x = visit_day,
        y = ID) +
    geom_path(colour = '#888888') +
    geom_point(aes(fill = hivsn_present),
               shape = 21, 
               size = 3) +
    labs(title = 'Timing of clinic visits, facetted by whether SN developed',
         subtitle = 'Time-points were SN was present are coded by fill colour',
         x = 'Days',
         y = 'Participant ID') +
    scale_x_continuous(breaks = seq(0, 250, 25)) +
    scale_fill_manual(name = 'Neuropathy present: ',
                      values = c('#FFFFFF', '#000000')) +
    facet_wrap(~ sn, 
               ncol = 2, 
               scales = 'free_y') +
    theme(legend.position = 'top',
          legend.margin = margin(t = -0.3, 
                                 r = 0, 
                                 b = -0.3, 
                                 l = 0, 'lines'),
          panel.grid = element_blank())
```

<img src="./figures/suppl_03-follow-up-analysis/visit_sn-1.png" width="672" style="display: block; margin: auto;" />

```r
# Publication plot
gg_plot <- data_plot %>% 
    ggplot(data = .) +
    aes(x = visit_day,
        y = ID) +
    geom_path(colour = '#888888') +
    geom_point(aes(fill = hivsn_present),
               shape = 21, 
               size = 3) +
    labs(x = 'Days',
         y = 'Participant ID') +
    scale_x_continuous(breaks = seq(0, 250, 25)) +
    scale_fill_manual(name = 'Neuropathy present: ',
                      values = c('#FFFFFF', '#000000')) +
    facet_wrap(~ sn, 
               ncol = 2, 
               scales = 'free_y') +
    theme(legend.position = 'top',
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11),
          axis.text.y = element_text(colour = '#000000'),
          axis.text.x = element_text(colour = '#000000',
                                     size = 12),
          axis.title = element_text(size = 14),
          panel.grid = element_blank(),
          panel.border = element_rect(size = 1),
          strip.text = element_text(size = 12))

ggsave(filename = 'followup.png', plot = gg_plot, width = 9, height = 8)
```

----

# Analysis

## Process data

```r
# Generate visit data
data_v1 <- data %>%
    # Calculations per individual
    group_by(ID) %>%
    mutate(# Extract the max number of visits
           max_visits = max(visit_number), 
           # Time from visit 1 to the last visit
           max_duration = max(visit_day), 
           # Time between successive visits
           visit_break = visit_day - lag(visit_day),
           # Running mean time (days) between visits
           cummulative_mean = round(cummean(visit_day))) %>% 
    ungroup() %>% 
    # Select required columns
    select(ID, visit_number, visit_day, visit_break, max_visits, 
           max_duration, cummulative_mean) %>%
    left_join(data_sn)

# Filter based on maximum number of visits
data_v2 <- data_v1 %>%
    # Per individual
    group_by(ID) %>%
    # Get maximum number of visits
    filter(visit_number == max_visits) %>%
    ungroup()
```

## Number of clinic visits

```r
# Summary table
data_v2 %>%
    summarise(n = n(),
              Median.visits = median(max_visits),
              Q25 = quantile(max_visits, 0.25),
              Q75 = quantile(max_visits, 0.75),
              min = min(max_visits),
              max = max(max_visits))
```

```
## # A tibble: 1 x 6
##       n Median.visits   Q25   Q75   min   max
##   <int>         <dbl> <dbl> <dbl> <dbl> <dbl>
## 1    83             3     3     3     3     5
```

```r
# Summary table (conditioned on sn)
data_v2 %>%
    group_by(sn) %>%
    summarise(n = n(),
              Median.visits = median(max_visits),
              Q25 = quantile(max_visits, 0.25),
              Q75 = quantile(max_visits, 0.75),
              min = min(max_visits),
              max = max(max_visits))
```

```
## # A tibble: 2 x 7
##   sn        n Median.visits   Q25   Q75   min   max
##   <fct> <int>         <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 no       63             3     3  3        3     5
## 2 yes      20             3     3  3.25     3     5
```

```r
# Plots
gg_v2a <- data_v2 %>%
    group_by(max_visits) %>%
    summarise(count = n()) %>%
    mutate(visits = fct_relevel(factor(max_visits), '5', '4', '3')) %>%
    ggplot(data = .) +
    aes(y = count,
        x = 'all',
        fill = visits) +
    geom_bar(stat = 'identity') +
    scale_fill_grey() +
    labs(title = 'Number of clinic visits (counts)',
         subtitle = '(All participants)',
         y = 'Count') +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank())

gg_v2b <- data_v2 %>%
    group_by(max_visits) %>%
    summarise(count = n()) %>%
    mutate(visits = fct_relevel(factor(max_visits), '5', '4', '3')) %>%
    ggplot(data = .) +
    aes(y = count,
        x = 'all',
        fill = visits) +
    geom_bar(stat = 'identity', 
             position = position_fill()) +
    scale_fill_grey() +
    labs(title = 'Number of clinic visits (proportion)',
         subtitle = '(All participants)',
         y = 'Proportion') +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank())

# Plot next to each other using patchwork package
gg_v2a + gg_v2b
```

<img src="./figures/suppl_03-follow-up-analysis/n_visits-1.png" width="672" style="display: block; margin: auto;" />

```r
## Conditional on SN
gg_v2c <- data_v2 %>%
    group_by(sn, max_visits) %>%
    summarise(count = n()) %>%
    mutate(visits = fct_relevel(factor(max_visits), '5', '4', '3')) %>%
    ggplot(data = .) +
    aes(y = count,
        x = sn,
        fill = visits) +
    geom_bar(stat = 'identity') +
    scale_fill_grey() +
    labs(title = 'Number of clinic visits (proportion)',
         subtitle = '(All participants)',
         y = 'Proportion') +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank())

gg_v2d <- data_v2 %>%
    group_by(sn, max_visits) %>%
    summarise(count = n()) %>%
    mutate(visits = fct_relevel(factor(max_visits), '5', '4', '3')) %>%
    ggplot(data = .) +
    aes(y = count,
        x = sn,
        fill = visits) +
    geom_bar(stat = 'identity', 
             position = position_fill()) +
    scale_fill_grey() +
    labs(title = 'Number of clinic visits (proportion)',
         subtitle = '(All participants)',
         y = 'Proportion') +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank())

# Plot next to each other using patchwork package
gg_v2c + gg_v2d
```

<img src="./figures/suppl_03-follow-up-analysis/n_visits-2.png" width="672" style="display: block; margin: auto;" />

```r
# Stats
chisq_test(xtabs(~ sn + visit_number, 
                 data = data_v2),
           distribution = approximate(B = 10000))
```

```
## 
## 	Approximative Pearson Chi-Squared Test
## 
## data:  visit_number by sn (no, yes)
## chi-squared = 0.33484, p-value = 0.9004
```

## Time between first and last visit

```r
# Summary table
data_v2 %>%
    summarise(n = n(),
              Median.days = median(max_duration),
              Q25 = quantile(max_duration, 0.25),
              Q75 = quantile(max_duration, 0.75),
              min = min(max_duration),
              max = max(max_duration))
```

```
## # A tibble: 1 x 6
##       n Median.days   Q25   Q75   min   max
##   <int>       <dbl> <dbl> <dbl> <dbl> <dbl>
## 1    83         196   185   209   166   263
```

```r
# Summary table (conditioned on sn)
data_v2 %>%
    group_by(sn) %>% 
    summarise(n = n(),
              Median.days = median(max_duration),
              Q25 = quantile(max_duration, 0.25),
              Q75 = quantile(max_duration, 0.75),
              min = min(max_duration),
              max = max(max_duration))
```

```
## # A tibble: 2 x 7
##   sn        n Median.days   Q25   Q75   min   max
##   <fct> <int>       <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 no       63         197  186.  209    169   263
## 2 yes      20         195  182.  208.   166   243
```

```r
# Plots
data_v2 %>%
    ggplot(data = .) +
    aes(y = max_duration, 
        x = 'All patients') +
    geom_boxplot() +
    geom_jitter(height = 0) +
    labs(title = 'Number of days of follow-up',
         subtitle = '(All participants)',
         y = 'Days') +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank())
```

<img src="./figures/suppl_03-follow-up-analysis/time_visits-1.png" width="672" style="display: block; margin: auto;" />

```r
## Conditional on SN
data_v2 %>%
    ggplot(data = .) +
    aes(y = max_duration, 
        x = sn) +
    geom_boxplot() +
    geom_jitter(height = 0) +
    labs(title = 'Number of days of follow-up',
         subtitle = 'SN:yes vs SN:no',
         y = 'Days',
         x = 'SN present')
```

<img src="./figures/suppl_03-follow-up-analysis/time_visits-2.png" width="672" style="display: block; margin: auto;" />

```r
# Stats
normal_test(max_duration ~ factor(sn),
            data = data_v2,
            distribution = approximate(B = 10000))
```

```
## 
## 	Approximative Two-Sample van der Waerden (Normal Quantile) Test
## 
## data:  max_duration by factor(sn) (no, yes)
## Z = 0.91044, p-value = 0.372
## alternative hypothesis: true mu is not equal to 0
```

## Time between successive clinic visits

```r
# Summary table
data_v1 %>%
    filter(visit_number != 1) %>%
    group_by(visit_number) %>%
    summarise(n = n(),
              Mean.days = mean(visit_break),
              Median.days = median(visit_break),
              SD = sd(visit_break),
              Q25 = quantile(visit_break, 0.25),
              Q75 = quantile(visit_break, 0.75),
              min = min(visit_break),
              max = max(visit_break))
```

```
## # A tibble: 4 x 9
##   visit_number     n Mean.days Median.days    SD   Q25   Q75   min   max
##          <int> <int>     <dbl>       <int> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1            2    83      69.6          62  28.5  54    90.5    14   146
## 2            3    83     112.          112  41.5  88   140      14   204
## 3            4    17      58.7          60  34.4  28    82       9   132
## 4            5     7      57.7          61  21.8  46.5  63      28    96
```

```r
# Summary table (conditioned on sn)
data_v1 %>%
    filter(visit_number != 1) %>%
    group_by(visit_number, sn) %>%
    summarise(n = n(),
              Mean.days = mean(visit_break),
              Median.days = median(visit_break),
              SD = sd(visit_break),
              Q25 = quantile(visit_break, 0.25),
              Q75 = quantile(visit_break, 0.75),
              min = min(visit_break),
              max = max(visit_break))
```

```
## # A tibble: 8 x 10
## # Groups:   visit_number [?]
##   visit_number sn        n Mean.days Median.days    SD   Q25   Q75   min
##          <int> <fct> <int>     <dbl>       <dbl> <dbl> <dbl> <dbl> <dbl>
## 1            2 no       63      67.0        61    26.7  54.5  83      14
## 2            2 yes      20      77.6        69    33.1  52   104.     28
## 3            3 no       63     116.        114    37.6  90.5 142      47
## 4            3 yes      20      99.2       102.   50.7  66.8 120.     14
## 5            4 no       12      58.8        62    29.0  39    79.8     9
## 6            4 yes       5      58.6        28    49.2  27    87      19
## 7            5 no        5      62.6        61    21.3  56    63      37
## 8            5 yes       2      45.5        45.5  24.7  36.8  54.2    28
## # ... with 1 more variable: max <dbl>
```

```r
# Plots
data_v1 %>%
    filter(visit_number != 1) %>%
    ggplot(data = .) +
    aes(y = visit_break, 
        x = factor(visit_number)) +
    geom_boxplot() +
    geom_jitter(height = 0) +
    scale_x_discrete(labels = c('1 to 2', '2 to 3',
                                '3 to 4', '4 to 5')) +
    labs(title = 'Days between successive clinic visit ',
         subtitle = '(All participants)',
         y = 'Days',
         x = 'Visit interval')
```

<img src="./figures/suppl_03-follow-up-analysis/cummulative_visits-1.png" width="672" style="display: block; margin: auto;" />

```r
## Conditional on SN
data_v1 %>%
    filter(visit_number != 1) %>%
    ggplot(data = .) +
    aes(y = visit_break, 
        x = factor(visit_number),
        fill = sn) +
    geom_boxplot(alpha = 0.6) +
    geom_point(position = position_jitterdodge(jitter.height = 0)) +
    scale_x_discrete(labels = c('1 to 2', '2 to 3',
                                '3 to 4', '4 to 5')) +
    scale_fill_grey(name = 'Neuropathy\npresent') +
    labs(title = 'Days between successive clinic visits',
         subtitle = 'SN:yes vs SN:no',
         y = 'Days',
         x = 'Visit interval') 
```

<img src="./figures/suppl_03-follow-up-analysis/cummulative_visits-2.png" width="672" style="display: block; margin: auto;" />

```r
# Stats
## Generate date
data_lmer <- data_v1 %>%
    filter(visit_number != 1) %>%
    select(ID, visit_number, visit_break, sn) 

## Generate model (ID included as random effects)
mod <- lmer(visit_break ~ sn * visit_number + (1|ID), 
            data = data_lmer)

## Analysis of Deviance Table (Type II Wald F tests with Kenward-Roger df)
car::Anova(mod = mod, 
           type = 'II', 
           test.statistic = 'F')
```

```
## Analysis of Deviance Table (Type II Wald F tests with Kenward-Roger df)
## 
## Response: visit_break
##                      F Df  Df.res  Pr(>F)  
## sn              0.3907  1  73.194 0.53387  
## visit_number    3.2803  1 170.193 0.07188 .
## sn:visit_number 2.6095  1 169.954 0.10808  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
## Basic diagnostics
plot(mod,
     main = 'Residuals vs Fitted') 
```

<img src="./figures/suppl_03-follow-up-analysis/cummulative_visits-3.png" width="672" style="display: block; margin: auto;" />

```r
qqnorm(resid(mod))
qqline(resid(mod))
```

<img src="./figures/suppl_03-follow-up-analysis/cummulative_visits-4.png" width="672" style="display: block; margin: auto;" />

----

# Session information

```r
sessionInfo()
```

```
## R version 3.5.1 (2018-07-02)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS  10.14
## 
## Matrix products: default
## BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] bindrcpp_0.2.2  lmerTest_3.0-1  lme4_1.1-19     Matrix_1.2-15  
##  [5] coin_1.2-2      survival_2.43-1 patchwork_0.0.1 forcats_0.3.0  
##  [9] stringr_1.3.1   dplyr_0.7.8     purrr_0.2.5     readr_1.1.1    
## [13] tidyr_0.8.2     tibble_1.4.2    ggplot2_3.1.0   tidyverse_1.2.1
## [17] magrittr_1.5   
## 
## loaded via a namespace (and not attached):
##  [1] httr_1.3.1        jsonlite_1.5      viridisLite_0.3.0
##  [4] splines_3.5.1     carData_3.0-2     modelr_0.1.2     
##  [7] assertthat_0.2.0  stats4_3.5.1      cellranger_1.1.0 
## [10] yaml_2.2.0        numDeriv_2016.8-1 pillar_1.3.0     
## [13] backports_1.1.2   lattice_0.20-38   glue_1.3.0       
## [16] digest_0.6.18     rvest_0.3.2       minqa_1.2.4      
## [19] colorspace_1.3-2  sandwich_2.5-0    htmltools_0.3.6  
## [22] plyr_1.8.4        pkgconfig_2.0.2   broom_0.5.0      
## [25] haven_1.1.2       mvtnorm_1.0-8     scales_1.0.0     
## [28] openxlsx_4.1.0    rio_0.5.10        car_3.0-2        
## [31] TH.data_1.0-9     withr_2.1.2.9000  lazyeval_0.2.1   
## [34] pbkrtest_0.4-7    cli_1.0.1         crayon_1.3.4     
## [37] readxl_1.1.0      evaluate_0.12     fansi_0.4.0      
## [40] nlme_3.1-137      MASS_7.3-51.1     foreign_0.8-71   
## [43] xml2_1.2.0        data.table_1.11.8 tools_3.5.1      
## [46] hms_0.4.2         multcomp_1.4-8    munsell_0.5.0    
## [49] zip_1.0.0         compiler_3.5.1    rlang_0.3.0.1    
## [52] grid_3.5.1        nloptr_1.2.1      rstudioapi_0.8   
## [55] labeling_0.3      rmarkdown_1.10    gtable_0.2.0     
## [58] codetools_0.2-15  abind_1.4-5       curl_3.2         
## [61] R6_2.3.0          zoo_1.8-4         lubridate_1.7.4  
## [64] knitr_1.20        utf8_1.1.4        bindr_0.1.1      
## [67] rprojroot_1.3-2   modeltools_0.2-22 stringi_1.2.4    
## [70] parallel_3.5.1    Rcpp_1.0.0        tidyselect_0.2.5
```

