################################################################################
########### US Bank systematic risk exposure: results and analysis #############
################################################################################

### Load libraries ###

library(tidyverse)
library(moments)
library(lmtest)
library(sandwich)
library(plm)
library(poweRlaw)

### Source prior script for computing systematic risk exposures ### 
name_script_file <- "US_Bank_SRE.R"
source(name_script_file, echo = F) #Compute systematic risk exposure using daily price

######################################################
### Descriptive SRE statistics #######################
######################################################

#########################
### Bank-wise summary ###
#########################

func_summ_vec <- function(vec)
{
  min <- min(vec, na.rm = T)
  max <- max(vec, na.rm = T)
  med <- median(vec, na.rm = T)
  mean <- mean(vec, na.rm = T)
  std <- sd(vec, na.rm = T)
  iqr <- IQR(vec, na.rm = T)
  skew <- moments::skewness(vec, na.rm = T)
  kurt <- moments::kurtosis(vec, na.rm = T)
  frac_NA <- sum(is.na(vec))/length(vec)
  quant_5 <- quantile(vec, 0.05, na.rm = T)
  quant_25 <- quantile(vec, 0.25, na.rm = T)
  quant_75 <- quantile(vec, 0.75, na.rm = T)
  quant_95 <- quantile(vec, 0.95, na.rm = T)
  
  summ_vec <- return(tibble::tibble('Min' = min, 
                                    'Max' = max, 
                                    'Med' = med, 
                                    'Mean' = mean, 
                                    'Std_Dev' = std,
                                    'Skew' = skew,
                                    'Kurt' = kurt,
                                    'IQR' = iqr,
                                    'SRE_%_Missing' = frac_NA,
                                    'Quant_5' = quant_5,
                                    'Quant_25' = quant_25,
                                    'Quant_75' = quant_75,
                                    'Quant_95' = quant_95))
  
  return(summ_vec)
}

# Summary for each bank separately
SRE_summ_bank_list <- apply(SRE_US_banks_wide[, -1], 2, func_summ_vec)
SRE_summ_bank_df <- dplyr::bind_rows(SRE_summ_bank_list) %>%
  tibble::add_column('Banks' = names(SRE_summ_bank_list)) %>%
  dplyr::select(Banks, everything())


# Summary of the whole US banking sector: Summary of summary of banks #
SRE_summ_US_list <- apply(SRE_summ_bank_df[, -1], 2, func_summ_vec)
SRE_summ_US_df <- dplyr::bind_rows(SRE_summ_US_list) %>%
  tibble::add_column('Parameters' = names(SRE_summ_US_list)) %>%
  dplyr::select(Parameters, everything())

#########################
### Quarterly summary ###
#########################

# Summary for each quarter separately
SRE_summ_quarterly_list <- apply(SRE_US_banks_wide[, -1], 1, func_summ_vec)
SRE_summ_quarterly_df <- dplyr::bind_rows(SRE_summ_quarterly_list) %>%
  tibble::add_column('Quarters' = nest_quarter_pc_regression$Q_num) %>%
  dplyr::select(Quarters, everything()) 


###################################################
#### Explanatory share of top eigenvectors ########
############ Figure 1 #############################
###################################################

nest_share <- nest_quarter_banks_US %>%
  dplyr::select(Q_num, share) %>%
  dplyr::mutate('top_eig_vec' = purrr::map(share, function(vec){vec[1:10]})) %>%
  dplyr::mutate('eig_vec_1' = purrr::map_dbl(top_eig_vec, function(vec){vec[1]}))

share_long <- nest_share %>%
  dplyr::select(Q_num, top_eig_vec) %>%
  tidyr::unnest(., cols = top_eig_vec)

### Boxplots of top 10 eigenvector explanatory power ###

x_breaks_share <- seq(1, 108, by = 4)
x_labels_share <- paste0(seq(1993, 2019), "Q1")


plot_box_share <- ggplot(data = share_long %>% 
                           dplyr::filter(Q_num %in% x_breaks_share),
                         mapping = aes(x = Q_num, y = top_eig_vec, group = Q_num)) +
  geom_boxplot() +
  scale_x_continuous(breaks = x_breaks_share,
                     labels = x_labels_share) +
  labs(x = "", y = "Explanatory power of top 10 eigenvectors") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 


### Topmost eigenvector's explanatory power share ###

plot_share_eig_vec_1 <- ggplot(data = nest_share, 
                               mapping = aes(x = Q_num, y = eig_vec_1)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = x_breaks_share,
                     labels = x_labels_share) +
  labs(x = "", y = "Explanatory power of the 1st (topmost) eigenvector") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

### Figure 1 ###

top_eig_mat <- sapply(nest_share$top_eig_vec, rbind) %>% t() 
colnames(top_eig_mat) <- paste0('Eig_', seq(1:ncol(top_eig_mat)))
top_eig_tib <- top_eig_mat %>%
  tibble::as_tibble() %>%
  tibble::add_column('Q_num' = seq(1, nrow(top_eig_mat))) %>%
  dplyr::select(Q_num, everything())

top_eig_long <- top_eig_tib %>%
  tidyr::gather(Eig_1:Eig_10, key = 'Eig_vec', value = 'Proportion')

plot_top_eig <- ggplot() +
  geom_line(data = top_eig_long, 
            mapping = aes(x = Q_num, y = Proportion, linetype = Eig_vec)) +
  scale_x_continuous(breaks = x_breaks_share,
                     labels = x_labels_share) +
  labs(x = "", y = "Cumulative explanatory power of top eigenvectors") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  theme(legend.position = "none")
  

df_rect_crises_0 <- data.frame(x_1 = c(60, 70), x_2 = c(66, 78), 
                               x_3 = c(22, 38), x_4 = c(24, 40),
                               y_1 = c(0, 0), y_2 = c(1, 1),
                               y_3 = c(0,0), y_4 = c(1, 1))

plot_top_eig_crises <- plot_top_eig +
  geom_rect(data = df_rect_crises_0,
            mapping = aes(xmin = x_1, 
                          xmax = x_2, 
                          ymin = y_1, 
                          ymax = y_2),
            color = "grey",
            alpha = 0.1) +
  geom_rect(data = df_rect_crises_0,
            mapping = aes(xmin = x_3, 
                          xmax = x_4, 
                          ymin = y_3, 
                          ymax = y_4),
            color = "grey",
            alpha = 0.1)
  
  

###################################################
######### Trend plot for median bank ##############
############ Figure 2 #############################
###################################################

# Median bank's quarterly behavior
qtr_grid <- SRE_summ_quarterly_df$Quarters
x_breaks <- seq(qtr_grid[1], qtr_grid[105], by = 8)
x_labels <- paste0(seq(1993, 2019, by = 2), "Q1")

# All quarters
plot_trend_median <- ggplot(SRE_summ_quarterly_df,
                            aes(Quarters, Med)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm", 
              linetype = "dashed", 
              color = "black") +
  scale_x_continuous(breaks = x_breaks_share,
                     labels = x_labels_share) +
  labs(x = "", y = "Median US bank's systematic risk exposure") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 


plot_trend_median_quants <- plot_trend_median +
  # geom_point(data = SRE_summ_quarterly_df, 
  #            mapping = aes(x = Quarters, y = Quant_75)) +
  # geom_line(data = SRE_summ_quarterly_df, 
  #           mapping = aes(x = Quarters, y = Quant_75), linetype = 'dotted') +
  # geom_point(data = SRE_summ_quarterly_df, 
  #            mapping = aes(x = Quarters, y = Quant_25)) +
  # geom_line(data = SRE_summ_quarterly_df, 
  #           mapping = aes(x = Quarters, y = Quant_25), linetype = 'dotted') +
  geom_point(data = SRE_summ_quarterly_df, 
             mapping = aes(x = Quarters, y = Quant_95)) +
  geom_line(data = SRE_summ_quarterly_df, 
            mapping = aes(x = Quarters, y = Quant_95), linetype = 'dotted') +
  geom_point(data = SRE_summ_quarterly_df, 
             mapping = aes(x = Quarters, y = Quant_5)) +
  geom_line(data = SRE_summ_quarterly_df, 
            mapping = aes(x = Quarters, y = Quant_5), linetype = 'dotted')

###################################
###### SRE Boxplots ###############
####### Figure 3 ##################
###################################

# Yearly

x_breaks_y <- seq(qtr_grid[1], qtr_grid[105], by = 4)
x_labels_y <- paste0(seq(1993, 2019), "Q1")

data_boxplot <- SRE_US_banks_long %>%
  dplyr::filter(Q_num %in% x_breaks_y)

plot_box_yearly <- ggplot(data = data_boxplot, 
                          mapping = aes(x = Q_num, y = SRE_2, group = Q_num)) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_continuous(breaks = x_breaks_y,
                     labels = x_labels_y) +
  labs(x = "", y = "US banks' systematic risk exposure") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 


###############################################################
################### GSIB+DSIB US Banks ########################
###############################################################

name_banks_US <- name_banks_US %>%
  dplyr::arrange(comnam)

### GSIBs ###
# Banks have different names
bank_jpm <- c("J P MORGAN CHASE & CO", 
              "JPMORGAN CHASE & CO", 
              "MORGAN J P & CO INC")
bank_citi <- c("CITICORP","CITIGROUP INC")
bank_bofa <- c("BANK OF AMERICA CORP",
               "BANKAMERICA CORP",
               "BANKAMERICA CORP NEW")
bank_goldman <- c("GOLDMAN SACHS GROUP INC")
# bank_wells_fargo <- c("WELLS FARGO & CO NEW", 
#                       "NORWEST CORP", 
#                       "WACHOVIA CORP NEW",
#                       "WACHOVIA CORP 2ND NEW")
bank_wells_fargo <- c("WELLS FARGO & CO NEW")
bank_bny <- c("BANK OF NEW YORK MELLON CORP",
              "MELLON BANK CORP", 
              "MELLON FINANCIAL CORP")
bank_morgan_stanley <- c("MORGAN STANLEY DEAN WITTER & CO")
bank_state_street <- c("STATE STREET CORP",
                       "STATE STREET BOSTON CORP")

banks_gsib <- c(bank_jpm, bank_citi, bank_bofa,
                bank_goldman, bank_wells_fargo,
                bank_bny, bank_morgan_stanley,
                bank_state_street)

### DSIBs ###

bank_ally <- "ALLY FINANCIAL INC"
bank_amex <- "AMERICAN EXPRESS CO"
bank_truist <- "TRUIST FINANCIAL CORP"
bank_capital_one <- "CAPITAL ONE FINANCIAL CORP"
bank_comerica <- "COMERICA INC"
bank_discover <- "DISCOVER FINANCIAL SERVICES"
bank_fifth_third <- "FIFTH THIRD BANCORP"
bank_huntington <- "HUNTINGTON BANCSHARES INC"
bank_keycorp <- "KEYCORP NEW"
bank_mt <- "M & T BANK CORP"
bank_north_trust <- "NORTHERN TRUST CORP"
bank_pnc <- c("P N C BANK CORP",
              "P N C FINANCIAL CORP",
              "P N C FINANCIAL SERVICES GRP INC")
bank_regions <- c("REGIONS FINANCIAL CORP",
                  "REGIONS FINANCIAL CORP NEW")
bank_suntrust <- "SUNTRUST BANKS INC"
bank_us_bancorp <- "UNITED STATES BANCORP"
bank_union_bankcal <- "UNION BANK SAN FRANCISCO CA"
bank_zion <- c("ZIONS BANCORPORATION",
               "ZIONS BANCORPORATION N A")

banks_dsib <- c(bank_ally, bank_amex, bank_truist,
                bank_capital_one, bank_comerica,
                bank_discover, bank_fifth_third, 
                bank_huntington, bank_keycorp,
                bank_mt, bank_north_trust, bank_pnc,
                bank_regions, bank_suntrust, 
                bank_us_bancorp, bank_union_bankcal,
                bank_zion)

banks_systemic <- c(banks_gsib, banks_dsib)

SRE_US_systemic <- SRE_US_banks_long %>%
  dplyr::filter(Bank %in% banks_systemic) %>%
  dplyr::mutate("Type" = dplyr::case_when(Bank %in% banks_gsib ~ "GSIB",
                                          Bank %in% banks_dsib ~ "DSIB"))

median_quarter_full <- SRE_US_banks_long %>%
  dplyr::group_by(Q_num) %>%
  dplyr::summarise('med_full' = median(SRE_2, na.rm = T))
median_quarter_systemic <- SRE_US_systemic %>%
  dplyr::group_by(Q_num) %>%
  dplyr::summarise('med_sys' = median(SRE_2, na.rm = T))
median_quarter_GSIB <- SRE_US_systemic %>%
  dplyr::filter(Type == 'GSIB') %>%
  dplyr::group_by(Q_num) %>%
  dplyr::summarise('med_gsib' = median(SRE_2, na.rm = T))
median_quarter_DSIB <- SRE_US_systemic %>%
  dplyr::filter(Type == 'DSIB') %>%
  dplyr::group_by(Q_num) %>%
  dplyr::summarise('med_dsib' = median(SRE_2, na.rm = T))

median_quarter <- median_quarter_full %>%
  dplyr::full_join(., median_quarter_systemic, by = 'Q_num') %>%
  dplyr::full_join(., median_quarter_GSIB, by = 'Q_num') %>%
  dplyr::full_join(., median_quarter_DSIB, by = 'Q_num') 

median_quarter_long <- median_quarter %>%
  tidyr::gather(c(med_full, med_sys, med_gsib, med_dsib),
                key = "Medians", value = "SRE")

##############################################################
################## Figure 4 ##################################
##############################################################

df_rect_crises <- data.frame(x_1 = c(60, 70),
                             x_2 = c(66, 78),
                             y_1 = c(0, 0),
                             y_2 = c(100, 100))

data_plot_med_sys <- median_quarter_long %>%
  dplyr::filter(Medians %in% c('med_full', 'med_sys'))

plot_med_systemic <- ggplot(data_plot_med_sys,
                            aes(x = Q_num, y = SRE)) +
  geom_point() +
  geom_line(mapping = aes(linetype = Medians)) +
  scale_x_continuous(breaks = x_breaks_share,
                     labels = x_labels_share) +
  labs(x = "", y = "Median systemic and median US bank's SRE") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

### With crises shaded in grey ###
plot_med_systemic_crises <- ggplot() +
  geom_rect(data = df_rect_crises_0,
            mapping = aes(xmin = x_1, 
                          xmax = x_2, 
                          ymin = 100*y_1, 
                          ymax = 100*y_2),
            color = "grey",
            alpha = 0.1) +
  geom_rect(data = df_rect_crises_0,
            mapping = aes(xmin = x_3, 
                          xmax = x_4, 
                          ymin = 100*y_3, 
                          ymax = 100*y_4),
            color = "grey",
            alpha = 0.1) +
  geom_point(data = data_plot_med_sys,
            mapping = aes(x = Q_num, y = SRE)) +
  geom_line(data = data_plot_med_sys, 
            mapping = aes(x = Q_num, y = SRE, linetype = Medians)) +
  scale_linetype_manual(values = c('solid', "longdash")) +
  # geom_smooth(data = data_plot_med_sys, 
  #             mapping = aes(x = Q_num, y = SRE, group = Medians),
  #             method = 'lm',
  #             linetype = 'dotdash',
  #             color = 'black') +
  scale_x_continuous(breaks = x_breaks_share,
                     labels = x_labels_share) +
  labs(x = "", y = "Median systemic and median US bank's SRE") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  theme(legend.position = "none")


#######################################################################
############## First and second sample halves #########################
######################### Figure 5 ####################################
#######################################################################

median_quarter <- median_quarter %>% 
  dplyr::mutate('Period' = dplyr::case_when(Q_num < max(Q_num)/2 ~ 'H1', 
                                            Q_num >= max(Q_num)/2 ~ 'H2'))

plot_med_H1H2 <- ggplot() +
  geom_point(data = median_quarter, 
             mapping = aes(x = Q_num, y = med_full)) +
  geom_line(data = median_quarter, 
            mapping = aes(x = Q_num, y = med_full)) +
  geom_smooth(data = median_quarter, 
              mapping = aes(x = Q_num, y = med_full, group = Period),
              method = 'lm',
              linetype = 'dotdash',
              color = 'black') +
  scale_x_continuous(breaks = x_breaks_share,
                     labels = x_labels_share) +
  labs(x = "", y = "Median US bank's SRE") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

plot_med_H1H2_crises <- plot_med_H1H2 +
  geom_rect(data = df_rect_crises_0,
            mapping = aes(xmin = x_1, 
                          xmax = x_2, 
                          ymin = 15, 
                          ymax = 70*y_2),
            color = "grey",
            alpha = 0.1) +
  geom_rect(data = df_rect_crises_0,
            mapping = aes(xmin = x_3, 
                          xmax = x_4, 
                          ymin = 15, 
                          ymax = 70*y_4),
            color = "grey",
            alpha = 0.1) 
  

###################################################################
##################### TABLE 1 #####################################
###################################################################

SRE_summ_full <- SRE_US_banks_long %>%
  dplyr::ungroup() %>%
  dplyr::select(SRE_2) %>%
  dplyr::summarise('Min' = min(SRE_2, na.rm = T),
                   'Max' = max(SRE_2, na.rm = T),
                   'Mean' = mean(SRE_2, na.rm = T),
                   'Med' = median(SRE_2, na.rm = T),
                   'Std_Dev' = sd(SRE_2, na.rm = T),
                   'IQR' = IQR(SRE_2, na.rm = T),
                   'Skew' = moments::skewness(SRE_2, na.rm = T),
                   'Kurt' = moments::kurtosis(SRE_2, na.rm = T))

SRE_summ_sys <- SRE_US_banks_long %>%
  dplyr::filter(Bank %in% banks_systemic) %>%
  dplyr::ungroup() %>%
  dplyr::select(SRE_2) %>%
  dplyr::summarise('Min' = min(SRE_2, na.rm = T),
                   'Max' = max(SRE_2, na.rm = T),
                   'Mean' = mean(SRE_2, na.rm = T),
                   'Med' = median(SRE_2, na.rm = T),
                   'Std_Dev' = sd(SRE_2, na.rm = T),
                   'IQR' = IQR(SRE_2, na.rm = T),
                   'Skew' = moments::skewness(SRE_2, na.rm = T),
                   'Kurt' = moments::kurtosis(SRE_2, na.rm = T))

SRE_summ_H1 <- SRE_US_banks_long %>%
  dplyr::ungroup() %>%
  dplyr::filter(Q_num < max(Q_num)/2) %>%
  dplyr::select(SRE_2) %>%
  dplyr::summarise('Min' = min(SRE_2, na.rm = T),
                   'Max' = max(SRE_2, na.rm = T),
                   'Mean' = mean(SRE_2, na.rm = T),
                   'Med' = median(SRE_2, na.rm = T),
                   'Std_Dev' = sd(SRE_2, na.rm = T),
                   'IQR' = IQR(SRE_2, na.rm = T),
                   'Skew' = moments::skewness(SRE_2, na.rm = T),
                   'Kurt' = moments::kurtosis(SRE_2, na.rm = T))

SRE_summ_H2 <- SRE_US_banks_long %>%
  dplyr::ungroup() %>%
  dplyr::filter(Q_num >= max(Q_num)/2) %>%
  dplyr::select(SRE_2) %>%
  dplyr::summarise('Min' = min(SRE_2, na.rm = T),
                   'Max' = max(SRE_2, na.rm = T),
                   'Mean' = mean(SRE_2, na.rm = T),
                   'Med' = median(SRE_2, na.rm = T),
                   'Std_Dev' = sd(SRE_2, na.rm = T),
                   'IQR' = IQR(SRE_2, na.rm = T),
                   'Skew' = moments::skewness(SRE_2, na.rm = T),
                   'Kurt' = moments::kurtosis(SRE_2, na.rm = T))

##### PRINTING TABLE 1 ##################################

print_table_1 <- dplyr::bind_rows('All' = SRE_summ_full,
                                'Sys' = SRE_summ_sys,
                                'H1' = SRE_summ_H1,
                                'H2' = SRE_summ_H2)
print_table_1 <- print_table_1 %>%
  tibble::add_column('Sample' = c('All', 'Sys', 'H1', 'H2')) %>%
  dplyr::select(Sample, everything())

#########################################################

#########################################################
################## TABLE 2 ##############################
#########################################################


func_trend_NW <- function(df)
{
  # This function computes the linear trend and reports
  # heteroskedasticity and autocorrelation consistent errors
  # according to Newey West
  
  rhs <- df[, 1] #dependent variable
  lhs <- df[, 2] #independent variable
  
  lm_trend <- lm(lhs ~ rhs, data = df)
  summ_trend <- summary(lm_trend)
  # vcov_err <- sandwich::NeweyWest(lm_trend, lag = 2, prewhite = F, adjust = T)
  vcov_err <- sandwich::NeweyWest(lm_trend)
  summ_trend$coefficients <- unclass(lmtest::coeftest(lm_trend, vcov. = vcov_err))
  
  return(summ_trend)
}

# Trends all banks
trend_full <- func_trend_NW(data.frame(median_quarter$Q_num, median_quarter$med_full))
# Trends systemic banks
trend_sys <- func_trend_NW(data.frame(median_quarter$Q_num, median_quarter$med_sys))

# Trends first half H1 and second half
median_quarter_H1 <- median_quarter %>%
  dplyr::filter(Period == 'H1')
median_quarter_H2 <- median_quarter %>%
  dplyr::filter(Period == 'H2')

trend_H1 <- func_trend_NW(data.frame(median_quarter_H1$Q_num, 
                                     median_quarter_H1$med_full))
trend_H2 <- func_trend_NW(data.frame(median_quarter_H2$Q_num, 
                                     median_quarter_H2$med_full))

# Trends systemic banks H1 and H2
median_quarter_H1_sys <- median_quarter_systemic %>%
  dplyr::filter(Q_num < max(Q_num)/2)
median_quarter_H2_sys <- median_quarter_systemic %>%
  dplyr::filter(Q_num >= max(Q_num)/2) 

trend_H1_sys <- func_trend_NW(data.frame(median_quarter_H1_sys$Q_num, 
                                     median_quarter_H1$med_sys))
trend_H2_sys <- func_trend_NW(data.frame(median_quarter_H2_sys$Q_num, 
                                     median_quarter_H2$med_sys))

func_trend_print <- function(lm_trend)
{
  lm_coef <- lm_trend$coefficients
  lm_coef_year <- lm_coef['rhs', ]
  
  return(lm_coef_year)
}


########## PRINTING TABLE 2 ##################################

print_table_2 <- dplyr::bind_rows(func_trend_print(trend_full),
                                  func_trend_print(trend_sys),
                                  func_trend_print(trend_H1),
                                  func_trend_print(trend_H2),
                                  func_trend_print(trend_H1_sys),
                                  func_trend_print(trend_H2_sys))

print_table_2 <- print_table_2 %>%
  tibble::add_column('Sample' = c('All', 'Sys', 'H1', 'H2',
                                  'Sys H1', 'Sys H2')) %>%
  dplyr::select(Sample, everything())

###############################################################


#### Computing Newey-West trends for all banks ####

nest_bank_trend <- SRE_US_banks_long %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Bank) %>%
  tidyr::nest()

func_trend_NW_nested <- function(df)
{
  # This function computes the linear trend and reports
  # heteroskedasticity and autocorrelation consistent errors
  # according to Newey West
  
  rhs <- df$Q_num #dependent variable
  lhs <- df$SRE_2 #independent variable
  
  lm_trend <- lm(lhs ~ rhs, data = df)
  summ_trend <- summary(lm_trend)
  # vcov_err <- sandwich::NeweyWest(lm_trend, lag = 2, prewhite = F, adjust = T)
  vcov_err <- sandwich::NeweyWest(lm_trend)
  summ_trend$coefficients <- unclass(lmtest::coeftest(lm_trend, vcov. = vcov_err))
  
  return(summ_trend)
}

nest_bank_trend <- nest_bank_trend %>%
  dplyr::mutate('obs' = purrr::map_dbl(data, nrow)) %>%
  dplyr::filter(obs > 10) %>%
  dplyr::mutate('trend_NW' = purrr::map(data, func_trend_NW_nested)) %>%
  dplyr::mutate('print_trend' = purrr::map(trend_NW, func_trend_print))

### p values for bank trends ###
func_coeff <- function(vec){return(vec[1])}
func_pvalue <- function(vec) {return(vec[4])}

nest_bank_trend <- nest_bank_trend %>%
  dplyr::mutate('p_value' = purrr::map_dbl(print_trend, func_pvalue),
                'coef' = purrr::map_dbl(print_trend, func_coeff)) %>%
  dplyr::arrange(p_value)

banks_trend_10 <- nest_bank_trend %>%
  dplyr::filter(p_value <= 0.10 & coef > 0)
banks_trend_5 <- nest_bank_trend %>%
  dplyr::filter(p_value <= 0.05 & coef > 0)
banks_trend_1 <- nest_bank_trend %>%
  dplyr::filter(p_value <= 0.01 & coef > 0)



########## PRINTING TRENDS FOR ALL BANKS ################

print_trend_full <- nest_bank_trend$print_trend
names(print_trend_full) <- nest_bank_trend$Bank

print_trend_full_2 <- dplyr::bind_rows(print_trend_full) %>% t()

######################################
########## SYSTEMIC BANKS ############
######################################

##### DOMESTIC SYSTEMICALLY IMPORTANT BANKS #####

nest_bank_trend_dsib <- nest_bank_trend %>%
  dplyr::filter(Bank %in% banks_dsib) 

# For banks with different names in the past
temp_pnc <- nest_bank_trend_dsib %>%
  dplyr::filter(Bank %in% bank_pnc)

data_pnc <- dplyr::full_join(temp_pnc$data[[1]], 
                             temp_pnc$data[[2]], 
                             by = c('Q_num', 'SRE_2'))
data_pnc_2 <- tibble::tibble('Bank' = rep('PNC', nrow(data_pnc)),
                             data_pnc)


temp_regions <- nest_bank_trend_dsib %>%
  dplyr::filter(Bank %in% bank_regions)

data_regions <- dplyr::full_join(temp_regions$data[[1]],
                                 temp_regions$data[[2]],
                                 by = c('Q_num', 'SRE_2'))
data_regions_2 <- tibble::tibble('Bank' = rep('REGIONS', nrow(data_regions)),
                                 data_regions)


data_dsib_2 <- dplyr::full_join(data_pnc_2, data_regions_2, 
                                by = c('Bank', 'Q_num', 'SRE_2'))

nest_dsib_2 <- data_dsib_2 %>%
  dplyr::group_by(Bank) %>%
  tidyr::nest()

nest_dsib_2 <- nest_dsib_2 %>%
  dplyr::mutate('obs' = purrr::map_dbl(data, nrow)) %>%
  dplyr::mutate('trend_NW' = purrr::map(data, func_trend_NW_nested)) %>%
  dplyr::mutate('print_trend' = purrr::map(trend_NW, func_trend_print))


nest_dsib_final <- nest_bank_trend_dsib %>%
  dplyr::filter(!(Bank %in% c(bank_pnc, bank_regions))) %>%
  dplyr::bind_rows(., nest_dsib_2) %>%
  dplyr::arrange(Bank)

##### GLOBAL SYSTEMICALLY IMPORTANT BANKS #####

nest_bank_trend_gsib <- nest_bank_trend %>%
  dplyr::filter(Bank %in% banks_gsib)

# For banks with different names in the past

### JP Morgan
temp_jpm <- nest_bank_trend_gsib %>%
  dplyr::filter(Bank %in% bank_jpm)

data_jpm <- dplyr::full_join(temp_jpm$data[[1]], 
                             temp_jpm$data[[2]], 
                             by = c('Q_num', 'SRE_2'))
data_jpm <- dplyr::full_join(data_jpm, temp_jpm$data[[3]],
                             by = c('Q_num', 'SRE_2'))

data_jpm_2 <- tibble::tibble('Bank' = rep('JPM', nrow(data_jpm)), data_jpm)


### CITI
temp_citi <- nest_bank_trend_gsib %>%
  dplyr::filter(Bank %in% bank_citi)

data_citi <- dplyr::full_join(temp_citi$data[[1]], 
                             temp_citi$data[[2]], 
                             by = c('Q_num', 'SRE_2'))

data_citi_2 <- tibble::tibble('Bank' = rep('CITI', nrow(data_citi)), data_citi)

### BANK OF AMERICA
temp_bofa <- nest_bank_trend_gsib %>%
  dplyr::filter(Bank %in% bank_bofa)

data_bofa <- dplyr::full_join(temp_bofa$data[[1]], 
                             temp_bofa$data[[2]], 
                             by = c('Q_num', 'SRE_2'))

data_bofa_2 <- tibble::tibble('Bank' = rep('BofA', nrow(data_bofa)), data_bofa)

### BNY MELLON
temp_bny <- nest_bank_trend_gsib %>%
  dplyr::filter(Bank %in% bank_bny)

data_bny <- dplyr::full_join(temp_bny$data[[1]], 
                             temp_bny$data[[2]], 
                             by = c('Q_num', 'SRE_2'))

data_bny <- dplyr::full_join(data_bny, temp_bny$data[[3]],
                             by = c('Q_num', 'SRE_2'))

data_bny_2 <- tibble::tibble('Bank' = rep('BNY', nrow(data_bny)), data_bny)

### STATE STREET
temp_stt <- nest_bank_trend_gsib %>%
  dplyr::filter(Bank %in% bank_state_street)

data_stt <- dplyr::full_join(temp_stt$data[[1]], 
                             temp_stt$data[[2]], 
                             by = c('Q_num', 'SRE_2'))

data_stt_2 <- tibble::tibble('Bank' = rep('STT', nrow(data_stt)), data_stt)


### JOINING ALL IN LONG FORMAT

data_gsib_2 <- data_jpm_2 %>%
  dplyr::full_join(., data_citi_2, by = c('Bank', 'Q_num', 'SRE_2')) %>%
  dplyr::full_join(., data_bofa_2, by = c('Bank', 'Q_num', 'SRE_2')) %>%
  dplyr::full_join(., data_bny_2, by = c('Bank', 'Q_num', 'SRE_2')) %>%
  dplyr::full_join(., data_stt_2, by = c('Bank', 'Q_num', 'SRE_2'))


nest_gsib_2 <- data_gsib_2 %>%
  dplyr::group_by(Bank) %>%
  tidyr::nest()

nest_gsib_2 <- nest_gsib_2 %>%
  dplyr::mutate('obs' = purrr::map_dbl(data, nrow)) %>%
  dplyr::mutate('trend_NW' = purrr::map(data, func_trend_NW_nested)) %>%
  dplyr::mutate('print_trend' = purrr::map(trend_NW, func_trend_print))


nest_gsib_final <- nest_bank_trend_gsib %>%
  dplyr::filter(!(Bank %in% c(bank_jpm, bank_citi, bank_bofa, 
                              bank_bny, bank_state_street))) %>%
  dplyr::bind_rows(., nest_gsib_2) %>%
  dplyr::arrange(Bank)

### COMBINING THE DSIBs AND GSIBs

nest_bank_trend_systemic <- nest_gsib_final %>%
  dplyr::bind_rows(., nest_dsib_final) %>%
  dplyr::arrange(Bank)

######## PRINTING TRENDS FOR SYSTEMIC BANKS #############
######## TABLE X ########################################

print_trend_systemic <- nest_bank_trend_systemic$print_trend
names(print_trend_systemic) <- nest_bank_trend_systemic$Bank

print_trend_systemic_2 <- dplyr::bind_rows(print_trend_systemic) %>% t()

##########################################################
################ Trend during crises #####################
##########################################################

# Note that indices are displaced by one unit due to 
# SRE estimation starting from period 2 as opposed to 1

# Dummy for the great recession
GR <- rep(0, nrow(median_quarter))
GR[59:65] <- 1

# Dummy for the eurozone crisis
EZ <- rep(0, nrow(median_quarter))
EZ[69:77] <- 1

# Dummy for LTCM
LTCM <- rep(0, nrow(median_quarter))
LTCM[21:23] <- 1

# Dummy for Dotcom bust
Dotcom <- rep(0, nrow(median_quarter))
Dotcom[37:39] <- 1

median_quarter <- median_quarter %>%
  dplyr::mutate('LTCM' = LTCM, 'Dotcom' = Dotcom, 'GR' = GR, 'EZ' = EZ,
                'Crisis' = GR + EZ + LTCM + Dotcom)

formula_full_trend_crises <- med_full ~ Q_num + GR + EZ + LTCM + Dotcom
formula_sys_trend_crises <- med_sys ~ Q_num + GR + EZ + LTCM + Dotcom

func_trend_crises <- function(df, formula = formula_full_trend_crises)
{
  lm_trend <- lm(formula = formula, data = df)
  summ_trend <- summary(lm_trend)
  vcov_err <- sandwich::NeweyWest(lm_trend)
  summ_trend$coefficients <- unclass(lmtest::coeftest(lm_trend, vcov. = vcov_err))
  
  return(summ_trend)
}


trend_crises_full <- func_trend_crises(median_quarter)
trend_crises_sys <- func_trend_crises(median_quarter, formula_sys_trend_crises)

################## TABLE 3 PRINTING #####################

print_crises_full <- trend_crises_full$coefficients
print_crises_sys <- trend_crises_sys$coefficients

#########################################################


