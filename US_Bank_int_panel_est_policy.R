
################################################################################
########### US Bank SRE: Panel estimation & policy analysis ####################
################################################################################

### Load libraries ###
library(tidyverse)
library(moments)
library(lmtest)
library(sandwich)
library(plm)
library(poweRlaw)
library(tseries)
library(fitdistrplus)
library(Matrix)

### Source prior script for computing systematic risk exposures ### 
name_script_file <- "US_Bank_SRE_results.R"
source(name_script_file, echo = F) #Compute systematic risk exposure using daily price

name_data_file <- 'CStat_Bank.dta'
data_US_banks_quarterly <- haven::read_dta(name_data_file)

# Explantory variables
data_Cstat_expl <- data_US_banks_quarterly %>%
  dplyr::select(gvkey, datacqtr, fyearq, datadate,
                cusip, conm, conml, atq,
                capr1q, capr2q, capr3q,
                ceqq, cshoq, cstkq, curcdq, 
                curncdq, dvcq, fic, glaq,
                lseq, ltq, nimq, tniiq,
                piq, seqq, stboq, tbq, teqq, 
                tfdq, niinty, piy, tcoey, tcory,
                dlcq, dlttq, 
                dptcq, dpdcq, dpscq, fdfrq, ffsq,
                fhlbq, loq, mbshsq, mtgiq, niintq, 
                npatq, tdomdq, teqq,
                dd1q, dibq, ireoq,
                olbmq, ltmibq)

# Renaming for clarity
data_Cstat_expl_2 <- data_Cstat_expl %>%
  dplyr::rename('total_assets' = atq, 
                'total_borrowing' = tbq,
                'total_liab' = ltq,
                'net_interest_margin' = nimq,
                'gain_post_tax' = glaq,
                'income_pre_tax' = piq,
                'T1_ratio' = capr1q,
                'T2_ratio' = capr2q,
                'T1_T2_comb_ratio' = capr3q,
                'tot_liab_shareholder_equity' = lseq,
                'common_equity' = ceqq,
                'common_stock' = cstkq,
                'cash_div_common_stock' = dvcq,
                'total_deposits' = dptcq,
                'total_foreign_deposits' = tfdq,
                'debt_in_curr_liab' = dlcq,
                'total_long_term_debt' = dlttq,
                'total_shareholder_equity' = seqq,
                'total_stockholder_equity' = teqq,
                'total_noninterest_income' = tniiq,
                'net_interest_income' = niintq,
                'total_non_performing_assets'= npatq,
                'long_term_debt_due_1_yr' = dd1q)

# Summarizing 
table_summary_expl <- apply(data_Cstat_expl_2[, -c(1:7, 15:16, 18)], 
                            2, summary) %>% t()

### Based on how many non-missing values there are we select the following ###
### variables for panel estimation exercise ##################################

data_Cstat_expl_3 <- data_Cstat_expl_2 %>%
  dplyr::select(gvkey, fyearq, datadate, datacqtr, cusip, fic, conm,
                total_assets, total_deposits, common_equity,
                total_shareholder_equity, total_borrowing,
                total_long_term_debt, net_interest_income,
                debt_in_curr_liab, common_stock,
                total_noninterest_income, cash_div_common_stock,
                total_non_performing_assets, net_interest_margin,
                T1_T2_comb_ratio)


func_log10 <- function(vec)
{
  # This function returns log after ignoring
  # all entries with subzero values
  vec[vec <= 0] <- NA
  return(log10(vec))
}


data_Cstat_expl_4 <- data_Cstat_expl_3 %>%
  dplyr::mutate('bank_size' = func_log10(total_assets),
                'deposit_ratio' = 100*(total_deposits/total_assets),
                'com_eq_ratio' = 100*(common_equity/total_assets),
                'shareholder_eq_ratio' = 100*(total_shareholder_equity/total_assets),
                'debt_ratio' = 100*(total_borrowing/total_assets),
                'debt_ratio_long_term' = 100*(total_long_term_debt/total_assets),
                'net_int_income_ratio' = 100*(net_interest_income/total_assets),
                'debt_ratio_current' = 100*(debt_in_curr_liab/total_assets),
                'com_stock_ratio' = 100*(common_stock/total_assets),
                'non_int_income_ratio' = 100*(total_noninterest_income/total_assets),
                'cash_div_ratio' = 100*(cash_div_common_stock/total_assets),
                'npa_ratio' = 100*(total_non_performing_assets/total_assets),
                'net_int_margin' = net_interest_margin,
                't1_t2_ratio' = T1_T2_comb_ratio)

data_Cstat_panel <- data_Cstat_expl_4 %>%
  dplyr::select(gvkey:conm, bank_size:t1_t2_ratio)

#### 8-digit cusip column for matching ####

data_Cstat_panel <- data_Cstat_panel %>%
  dplyr::mutate('cusip_8' = substr(cusip, 1, 8)) %>%
  dplyr::select(conm, datacqtr, cusip_8, 
                everything()) %>%
  dplyr::select(-c(gvkey, datadate, cusip))

### Matching bank name to 8-digit cusip ###
bank_cusip_file <- returns_daily_banks_US %>%
  dplyr::select(comnam, ncusip, cusip) %>%
  dplyr::distinct() %>%
  dplyr::rename('Bank' = comnam, 'cusip_8' = cusip)

### Attaching cusip code to bank SRE calculation ###

SRE_US_banks_long_2 <- SRE_US_banks_long %>%
  dplyr::left_join(., bank_cusip_file, by = 'Bank') %>%
  dplyr::arrange(Bank)

# Remove duplicate cusip-Q_num entries
SRE_US_banks_long_3 <- SRE_US_banks_long_2 %>%
  dplyr::distinct(cusip_8, Q_num, .keep_all = T)

## Attaching year-quarter to SRE long data ##

year_min <- min(returns_daily_banks_US$Year)
year_max <- max(returns_daily_banks_US$Year)

year_quarter <- paste0(rep(1993:2019, each = 4),
                       rep(c('Q1', 'Q2', 'Q3', 'Q4'), 
                           (year_max-year_min+1)))
quarter_numbers <- 1:108

tibble_year_quarter <- tibble::tibble('Q_num' = quarter_numbers,
                                      'datacqtr' = year_quarter)

data_Cstat_panel_2 <- data_Cstat_panel %>%
  dplyr::left_join(., tibble_year_quarter, by = 'datacqtr') %>%
  dplyr::select(conm, datacqtr, Q_num, fyearq, cusip_8, everything())  

# Remove duplicate cusip-Q_num entries
data_Cstat_panel_3 <- data_Cstat_panel_2 %>%
  dplyr::distinct(cusip_8, Q_num, .keep_all = T)

func_cusip_check <- function(cusip_8)
{
  # This function accepts an 8 digit cusip
  # and returns 1 if the last two digits
  # indicate common stocks (10 or 11) else
  # returns 0
  last_2_char <- substr(cusip_8, 7, 8)
  if (last_2_char == '10' |
      last_2_char == '11')
  {
    return(1)
  } else
  {
    return(0)
  }
}


#### Arranging the final panel data form ####

panel_SRE_full_left <- SRE_US_banks_long_3 %>%
  dplyr::left_join(., data_Cstat_panel_3, by = c('Q_num', 'cusip_8')) %>%
  dplyr::select(cusip_8, Q_num, datacqtr, ncusip, 
                Bank, conm, fyearq, fic, everything()) %>%
  dplyr::mutate('cusip_status' = purrr::map_dbl(cusip_8, func_cusip_check))


#############################################################################
################# Panel estimations begin here ##############################
#############################################################################

panel_SRE_full <- panel_SRE_full_left %>%
  dplyr::select(-c(ncusip, fyearq, fic)) %>%
  dplyr::select(cusip_8, Q_num, cusip_status, 
                datacqtr, Bank, conm, everything()) %>%
  dplyr::filter(cusip_status == 1) %>%
  dplyr::ungroup() 

panel_SRE_full_2 <- panel_SRE_full %>%
  dplyr::distinct(cusip_8, Q_num, .keep_all = T)


### Main model specification ###

formula_main <- SRE_2 ~ bank_size + deposit_ratio + com_eq_ratio +
  net_int_margin + npa_ratio + t1_t2_ratio + non_int_income_ratio +
  cash_div_ratio + debt_ratio_current 

func_panel_est <- function(formula = formula_main, 
                           panel_data = panel_SRE_full_2, 
                           model_spec = 'within',
                           fixed_effect = 'twoways')
{
  # This function accepts a formula, panel data matrix and model spec
  # and returns the summary of an unbalanced, fixed-effects panel regression
  # with clustered robust standard errors with clustering at both 
  # Country and Year levels
  
  # Panel estimation with fixed effects
  plm_fixed <- plm::plm(formula, 
                        data = panel_data, 
                        model = model_spec,
                        type = "HC0", 
                        effect = fixed_effect)
  
  # Robust, clustered standard errors
  vcov_err <- plm::vcovDC(plm_fixed) #Double clustering
  
  plm_fixed_robust <- lmtest::coeftest(plm_fixed, vcov. = vcov_err)
  
  plm_out <- summary(plm_fixed)
  
  # Include robust clustered errors
  plm_out$coefficients <- unclass(plm_fixed_robust) 
  
  return(plm_out)
}

###########################################################################
################# TABLE 6: Descriptive Stats ##############################
###########################################################################

func_summary <- function(vec)
{
  return(data.frame('Min' = min(vec),
                    'Max' = max(vec),
                    'Mean' = mean(vec),
                    'Med' = median(vec),
                    'SD' = sd(vec),
                    'IQR' = IQR(vec)))
}

panel_variables <- panel_SRE_full_2 %>%
  dplyr::select(SRE_2, bank_size, deposit_ratio, com_eq_ratio, 
                net_int_margin, npa_ratio, t1_t2_ratio, 
                non_int_income_ratio, cash_div_ratio, debt_ratio_current)

print_table_6 <- apply(na.omit(panel_variables), 2, func_summary) %>% 
  sapply(., rbind)

###########################################################################
################# TABLE 7: Correlation Matrix #############################
###########################################################################

print_table_7 <- cor(panel_variables, use = 'complete.obs')

################################
### Panel estimation results ###
################################

## All banks full time
panel_est_main_model_full <- func_panel_est(formula = formula_main,
                                       panel_data = panel_SRE_full_2,
                                       fixed_effect = 'twoways')

# Pooled
panel_est_main_model_pool <- func_panel_est(model_spec = 'pooling')

## All banks pre 2006
panel_est_main_model_H1 <- func_panel_est(formula = formula_main,
                                          panel_data = panel_SRE_full_2 %>%
                                            dplyr::filter(Q_num < max(Q_num)/2),
                                          fixed_effect = 'twoways')
# Pooled pre 2006
panel_est_main_model_H1_pool <- func_panel_est(panel_data = panel_SRE_full_2 %>%
                                              dplyr::filter(Q_num < max(Q_num)/2),
                                            model_spec = 'pooling')

## All banks post 2006
panel_est_main_model_H2 <- func_panel_est(formula = formula_main,
                                       panel_data = panel_SRE_full_2 %>%
                                         dplyr::filter(Q_num >= max(Q_num)/2),
                                       fixed_effect = 'twoways')
# Pooled post 2006
panel_est_main_model_H2_pool <- func_panel_est(panel_data = panel_SRE_full_2 %>%
                                                 dplyr::filter(Q_num >= max(Q_num)/2),
                                               model_spec = 'pooling')


### Subsample: Large banks (>$1B in 2019Q1) ###

cusip_large_2019 <- panel_SRE_full_2 %>%
  dplyr::filter(Q_num == 105) %>%
  dplyr::filter(bank_size >= 3) %>%
  dplyr::select(cusip_8, Bank, conm)

panel_SRE_large_2019 <- panel_SRE_full_2 %>%
  dplyr::filter(cusip_8 %in% cusip_large_2019$cusip_8)

# Large banks full time
panel_est_main_model_large <- func_panel_est(formula = formula_main,
                                             panel_data = panel_SRE_large_2019,
                                             fixed_effect = 'twoways')
# Large banks pooled
panel_est_main_model_large_pool <- func_panel_est(panel_data = panel_SRE_large_2019,
                                                  model_spec = 'pooling')

# Large banks pre 2006
panel_est_main_model_large_H1 <- func_panel_est(formula = formula_main,
                                             panel_data = panel_SRE_large_2019 %>%
                                               dplyr::filter(Q_num < max(Q_num)/2),
                                             fixed_effect = 'twoways')
# Large banks pre 2006 pooled
panel_est_main_model_large_H1_pool <- func_panel_est(panel_data = panel_SRE_large_2019 %>%
                                                  dplyr::filter(Q_num < max(Q_num)/2),
                                                  model_spec = 'pooling')

# Large banks post 2006
panel_est_main_model_large_H2 <- func_panel_est(formula = formula_main,
                                                panel_data = panel_SRE_large_2019 %>%
                                                  dplyr::filter(Q_num >= max(Q_num)/2),
                                                fixed_effect = 'twoways')
# Large banks post 2006 pooled
panel_est_main_model_large_H2_pool <- func_panel_est(panel_data = panel_SRE_large_2019 %>%
                                                       dplyr::filter(Q_num >= max(Q_num)/2),
                                                     model_spec = 'pooling')


#########################################################################
############## Dodd-Frank Act Policy Effects ############################
#########################################################################

bank_cusip_sys <- bank_cusip_file %>%
  dplyr::filter(Bank %in% banks_systemic)

### Tier 1 and 2 combined capital ratio ###

nest_SRE_t1t2_com_eq <- panel_SRE_full_2 %>%
  dplyr::select(Q_num, com_eq_ratio, t1_t2_ratio) %>%
  dplyr::group_by(Q_num) %>%
  dplyr::arrange(Q_num) %>%
  tidyr::nest() %>%
  dplyr::mutate('med_t1t2' = purrr::map_dbl(data, function(tib){return(median(tib$t1_t2_ratio, 
                                                                            na.rm = T))}),
                'med_com_eq' = purrr::map_dbl(data, function(tib){return(median(tib$com_eq_ratio, 
                                                                              na.rm = T))}))

nest_SRE_sys_t1t2_com_eq <- panel_SRE_full_2 %>%
  dplyr::select(Q_num, cusip_8, t1_t2_ratio, com_eq_ratio) %>%
  dplyr::filter(cusip_8 %in% bank_cusip_sys$cusip_8) %>%
  dplyr::group_by(Q_num) %>%
  dplyr::arrange(Q_num) %>%
  tidyr::nest() %>%
  dplyr::mutate('med_t1t2_sys' = purrr::map_dbl(data, 
                                                function(tib){return(median(tib$t1_t2_ratio, 
                                                                            na.rm = T))}),
                'med_com_eq_sys' = purrr::map_dbl(data, 
                                                  function(tib){return(median(tib$com_eq_ratio, 
                                                                              na.rm = T))}))


##################################################################
###################### Figures 6 and 7 ###########################
##################################################################

nest_SRE_combined <- nest_SRE_t1t2_com_eq %>%
  dplyr::select(-data) %>%
  dplyr::full_join(., nest_SRE_sys_t1t2_com_eq, by = 'Q_num') %>%
  dplyr::select(-data)

### Plotting the median systemic bank's ratios (Fig 6+7) ###
x_breaks_sys <- seq(1, 108, by = 4)
x_labels_sys <- paste0(seq(1993, 2019), "Q1")


### Combining the median systemic and the median US bank's T1 T2 ratios ###

plot_med_t1t2_combined <- ggplot() +
  geom_point(data = nest_SRE_combined, mapping = aes(x = Q_num, y = med_t1t2)) +
  geom_line(data = nest_SRE_combined, mapping = aes(x = Q_num, y = med_t1t2), 
            linetype = 'dotted') +
  geom_point(data = nest_SRE_combined, mapping = aes(x = Q_num, y = med_t1t2_sys), 
             shape = 1) +
  geom_line(data = nest_SRE_combined, mapping = aes(x = Q_num, y = med_t1t2_sys), 
            linetype = 'twodash') +
  geom_vline(xintercept = 71, linetype = 'dashed') +
  scale_x_continuous(breaks = x_breaks_sys, labels = x_labels_sys) +
  labs(x = "", y = "Median banks' Tier 1 and 2 ratio (combined, in %)") +
  theme_bw() +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

# Crises rectangle
df_rect_crises_2 <- data.frame(x_1 = c(60, 70),
                               x_2 = c(66, 78),
                               y_1 = c(11, 11),
                               y_2 = c(16.5, 16.5))

### Plot with crises superimposed ###

plot_med_t1t2_combined_crises <- plot_med_t1t2_combined +
  geom_rect(data = df_rect_crises_2,
            mapping = aes(xmin = x_1, 
                          xmax = x_2, 
                          ymin = y_1, 
                          ymax = y_2),
            color = "grey",
            alpha = 0.1)


## Common equity ratio combined


plot_med_com_eq_combined <- ggplot() +
  geom_point(data = nest_SRE_combined, mapping = aes(x = Q_num, y = med_com_eq)) +
  geom_line(data = nest_SRE_combined, mapping = aes(x = Q_num, y = med_com_eq), 
            linetype = 'dotted') +
  geom_point(data = nest_SRE_combined, mapping = aes(x = Q_num, y = med_com_eq_sys), 
             shape = 1) +
  geom_line(data = nest_SRE_combined, mapping = aes(x = Q_num, y = med_com_eq_sys), 
            linetype = 'twodash') +
  geom_vline(xintercept = 71, linetype = 'dashed') +
  scale_x_continuous(breaks = x_breaks_sys, labels = x_labels_sys) +
  labs(x = "", y = "Median banks' common equity ratio (combined, in %)") +
  theme_bw() +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Crises rectangle
df_rect_crises_2 <- data.frame(x_1 = c(60, 70),
                               x_2 = c(66, 78),
                               y_2 = c(11.5, 11.5),
                               y_1 = c(7, 7))

plot_med_com_eq_combined_crises <- plot_med_com_eq_combined +
  geom_rect(data = df_rect_crises_2,
            mapping = aes(xmin = x_1, 
                          xmax = x_2, 
                          ymin = y_1, 
                          ymax = y_2),
            color = "grey",
            alpha = 0.1)

###############################################################################
######## TABLE 5: Mean variables before and after Dodd-Frank ##################
###############################################################################

SRE_sys_Dodd_Frank_pre <- panel_SRE_full_2 %>%
  dplyr::select(Q_num, cusip_8, SRE_2, t1_t2_ratio, com_eq_ratio) %>%
  dplyr::filter(cusip_8 %in% bank_cusip_sys$cusip_8) %>%
  dplyr::filter(Q_num < 71)
#  dplyr::filter(Q_num < 63)

SRE_sys_Dodd_Frank_post <- panel_SRE_full_2 %>%
  dplyr::select(Q_num, cusip_8, SRE_2, t1_t2_ratio, com_eq_ratio) %>%
  dplyr::filter(cusip_8 %in% bank_cusip_sys$cusip_8) %>%
  dplyr::filter(Q_num >= 71)
#  dplyr::filter(Q_num >= 63)

### Test of equality of pooled means ###

## Tier 1 and 2 ratio
# T test (parametric)
mean_test_t1t2 <- t.test(SRE_sys_Dodd_Frank_pre$t1_t2_ratio,
                         SRE_sys_Dodd_Frank_post$t1_t2_ratio,
                         alternative = "less")
# (Mann-Whitney) Wilcoxon test (nonparametric)
mean_test_t1t2_wilcox <- wilcox.test(SRE_sys_Dodd_Frank_pre$t1_t2_ratio, 
                                SRE_sys_Dodd_Frank_post$t1_t2_ratio, 
                                alternative = "less")
# KS test
mean_test_t1t2_ks <- ks.test(SRE_sys_Dodd_Frank_pre$t1_t2_ratio, 
                        SRE_sys_Dodd_Frank_post$t1_t2_ratio,
                        alternative = "greater")

## Common equity ratio
# T test (parametric)
mean_test_com_eq <- t.test(SRE_sys_Dodd_Frank_pre$com_eq_ratio,
                         SRE_sys_Dodd_Frank_post$com_eq_ratio,
                         alternative = "less")
# (Mann-Whitney) Wilcoxon test (nonparametric)
mean_test_com_eq_wilcox <- wilcox.test(SRE_sys_Dodd_Frank_pre$com_eq_ratio, 
                                     SRE_sys_Dodd_Frank_post$com_eq_ratio, 
                                     alternative = "less")
# KS test
mean_test_com_eq_ks <- ks.test(SRE_sys_Dodd_Frank_pre$com_eq_ratio, 
                        SRE_sys_Dodd_Frank_post$com_eq_ratio,
                        alternative = "greater")

## SRE
# T test (parametric)
mean_test_SRE <- t.test(SRE_sys_Dodd_Frank_pre$SRE_2,
                           SRE_sys_Dodd_Frank_post$SRE_2,
                           alternative = "less")
# (Mann-Whitney) Wilcoxon test (nonparametric)
mean_test_SRE_wilcox <- wilcox.test(SRE_sys_Dodd_Frank_pre$SRE_2, 
                                       SRE_sys_Dodd_Frank_post$SRE_2, 
                                       alternative = "less")
# KS test
mean_test_SRE_ks <- ks.test(SRE_sys_Dodd_Frank_pre$SRE_2, 
                               SRE_sys_Dodd_Frank_post$SRE_2,
                               alternative = "greater")

############################################################################
######################### Mean tests during crises #########################
############################################################################

####### SRE ########

panel_SRE_crises <- panel_SRE_full_2 %>%
  dplyr::select(Q_num, cusip_8, SRE_2) %>%
  dplyr::arrange(Q_num) %>%
  dplyr::mutate('GR' = dplyr::case_when(Q_num %in% seq(59, 65) ~ 1,
                                        TRUE ~ 0),
                'EZ' = dplyr::case_when(Q_num %in% seq(69, 77) ~ 1,
                                        TRUE ~ 0),
                'LTCM' = dplyr::case_when(Q_num %in% seq(21, 23) ~ 1,
                                          TRUE ~ 0), 
                'Dotcom' = dplyr::case_when(Q_num %in% seq(37, 39) ~ 1, 
                                            TRUE ~ 0),
                'Crises' = dplyr::case_when(GR == 1 | EZ == 1 | LTCM == 1 | Dotcom == 1 ~ 1,
                                            TRUE ~ 0))

### The Great Recession ###

# T test
mean_test_SRE_GR <- t.test(filter(panel_SRE_crises, GR == 1)$SRE_2,
                           filter(panel_SRE_crises, GR == 0)$SRE_2,
                           alternative = "greater")

# Wilcox test
mean_test_SRE_GR_wilcox <- wilcox.test(filter(panel_SRE_crises, GR == 1)$SRE_2,
                                       filter(panel_SRE_crises, GR == 0)$SRE_2,
                                       alternative = "greater")

# KS test
mean_test_SRE_GR_ks <- ks.test(filter(panel_SRE_crises, GR == 1)$SRE_2,
                               filter(panel_SRE_crises, GR == 0)$SRE_2,
                               alternative = "less")

### The Eurozone Crisis ###

# T test
mean_test_SRE_EZ <- t.test(filter(panel_SRE_crises, EZ == 1)$SRE_2,
                           filter(panel_SRE_crises, EZ == 0)$SRE_2,
                           alternative = "greater")

# Wilcox test
mean_test_SRE_EZ_wilcox <- wilcox.test(filter(panel_SRE_crises, EZ == 1)$SRE_2,
                                       filter(panel_SRE_crises, EZ == 0)$SRE_2,
                                       alternative = "greater")

# KS test
mean_test_SRE_EZ_ks <- ks.test(filter(panel_SRE_crises, EZ == 1)$SRE_2,
                               filter(panel_SRE_crises, EZ == 0)$SRE_2,
                               alternative = "less")

### The LTCM Crisis ###

# T test
mean_test_SRE_LTCM <- t.test(filter(panel_SRE_crises, LTCM == 1)$SRE_2,
                           filter(panel_SRE_crises, LTCM == 0)$SRE_2,
                           alternative = "greater")

# Wilcox test
mean_test_SRE_LTCM_wilcox <- wilcox.test(filter(panel_SRE_crises, LTCM == 1)$SRE_2,
                                       filter(panel_SRE_crises, LTCM == 0)$SRE_2,
                                       alternative = "greater")

# KS test
mean_test_SRE_LTCM_ks <- ks.test(filter(panel_SRE_crises, LTCM == 1)$SRE_2,
                               filter(panel_SRE_crises, LTCM == 0)$SRE_2,
                               alternative = "less")

### The Dotcom Crisis ###

# T test
mean_test_SRE_Dotcom <- t.test(filter(panel_SRE_crises, Dotcom == 1)$SRE_2,
                             filter(panel_SRE_crises, Dotcom == 0)$SRE_2,
                             alternative = "greater")

# Wilcox test
mean_test_SRE_Dotcom_wilcox <- wilcox.test(filter(panel_SRE_crises, Dotcom == 1)$SRE_2,
                                         filter(panel_SRE_crises, Dotcom == 0)$SRE_2,
                                         alternative = "greater")

# KS test
mean_test_SRE_Dotcom_ks <- ks.test(filter(panel_SRE_crises, Dotcom == 1)$SRE_2,
                                 filter(panel_SRE_crises, Dotcom == 0)$SRE_2,
                                 alternative = "less")


### Crises ###

# T test
mean_test_SRE_crises <- t.test(filter(panel_SRE_crises, Crises == 1)$SRE_2,
                               filter(panel_SRE_crises, Crises == 0)$SRE_2,
                               alternative = "greater")

# Wilcox test
mean_test_SRE_crises_wilcox <- wilcox.test(filter(panel_SRE_crises, Crises == 1)$SRE_2,
                                           filter(panel_SRE_crises, Crises == 0)$SRE_2,
                                           alternative = "greater")

# KS test
mean_test_SRE_crises_ks <- ks.test(filter(panel_SRE_crises, Crises == 1)$SRE_2,
                                   filter(panel_SRE_crises, Crises == 0)$SRE_2,
                                   alternative = "less")


#######################################################
####### Trends for full sample during crises ##########
#######################################################

nest_trend_crises <- panel_SRE_crises %>%
  dplyr::group_by(cusip_8) %>%
  tidyr::nest() 

form_trend_crises <- SRE_2 ~ Q_num + LTCM + Dotcom + GR + EZ
form_trend_any_crisis <- SRE_2 ~ Q_num + Crises

func_trend_crises_2 <- function(tib)
{
  formula = form_trend_crises
  lm_trend <- lm(formula = formula, data = tib)
  summ_trend <- summary(lm_trend)
  summ_trend_coef <- summ_trend$coefficients
  
  return(summ_trend_coef)
}

nest_trend_crises_2 <- nest_trend_crises %>%
  dplyr::mutate('trend_crises' = purrr::map(data, func_trend_crises_2),
                'coef' = purrr::map(trend_crises, function(df){df[, 1]}),
                'p_val' = purrr::map(trend_crises, function(df){df[, 4]}))

func_Q_num_pick <- function(name_vec) {return(name_vec['Q_num'])}

func_ltcm_pick <- function(name_vec)
{
  if ('LTCM' %in% names(name_vec))
  {
    return(name_vec['LTCM'])
  } else
  {
    return(NA)
  }
}

func_Dotcom_pick <- function(name_vec)
{
  if ('LTCM' %in% names(name_vec))
  {
    return(name_vec['Dotcom'])
  } else
  {
    return(NA)
  }
}

func_GR_pick <- function(name_vec)
{
  if ('GR' %in% names(name_vec))
  {
    return(name_vec['GR'])
  } else
  {
    return(NA)
  }
}

func_EZ_pick <- function(name_vec)
{
  if ('EZ' %in% names(name_vec))
  {
    return(name_vec['EZ'])
  } else
  {
    return(NA)
  }
}

nest_trend_crises_3 <- nest_trend_crises_2 %>%
  dplyr::select(cusip_8, coef, p_val) %>%
  dplyr::mutate('coef_ltcm' = purrr::map_dbl(coef, func_ltcm_pick),
                'coef_dotcom' = purrr::map_dbl(coef, func_Dotcom_pick),
                'coef_GR' = purrr::map_dbl(coef, func_GR_pick),
                'coef_EZ' = purrr::map_dbl(coef, func_EZ_pick),
                'p_val_ltcm' = purrr::map_dbl(p_val, func_ltcm_pick),
                'p_val_dotcom' = purrr::map_dbl(p_val, func_Dotcom_pick),
                'p_val_GR' = purrr::map_dbl(p_val, func_GR_pick),
                'p_val_EZ' = purrr::map_dbl(p_val, func_EZ_pick)) %>%
  dplyr::select(-c(coef, p_val))

ltcm_tib <- nest_trend_crises_3 %>%
  dplyr::select(cusip_8, coef_ltcm, p_val_ltcm)
dotcom_tib <- nest_trend_crises_3 %>%
  dplyr::select(cusip_8, coef_dotcom, p_val_dotcom)
GR_tib <- nest_trend_crises_3 %>%
  dplyr::select(cusip_8, coef_GR, p_val_GR)
EZ_tib <- nest_trend_crises_3 %>%
  dplyr::select(cusip_8, coef_EZ, p_val_EZ)

#####################################################
####### Any crisis trends ###########################
#####################################################

func_trend_crises_2_alt <- function(tib)
{
  formula = form_trend_any_crisis
  lm_trend <- lm(formula = formula, data = tib)
  summ_trend <- summary(lm_trend)
  summ_trend_coef <- summ_trend$coefficients
  
  return(summ_trend_coef)
}

nest_trend_crises_2_alt <- nest_trend_crises %>%
  dplyr::mutate('trend' = purrr::map(data, func_trend_crises_2_alt),
                'coef' = purrr::map(trend, function(df){df[, 1]}),
                'p_val' = purrr::map(trend, function(df){df[, 4]}))

func_crisis_pick <- function(name_vec)
{
  if ('Crises' %in% names(name_vec))
  {
    return(name_vec['Crises'])
  } else
  {
    return(NA)
  }
}


nest_trend_crises_3_alt <- nest_trend_crises_2_alt %>%
  dplyr::select(cusip_8, coef, p_val) %>%
  dplyr::mutate('coef_crisis' = purrr::map_dbl(coef, func_crisis_pick),
                'p_val_crisis' = purrr::map_dbl(p_val, func_crisis_pick)) %>%
  dplyr::select(-c(coef, p_val))

crisis_tib_any <- nest_trend_crises_3_alt %>%
  dplyr::select(cusip_8, coef_crisis, p_val_crisis)

####################################################
####### Mean PC1 contribution during crises ########
####################################################


nest_share_crises <- nest_share %>%
  dplyr::mutate('GR' = dplyr::case_when(Q_num %in% seq(59, 65) ~ 1,
                                        TRUE ~ 0),
                'EZ' = dplyr::case_when(Q_num %in% seq(69, 77) ~ 1,
                                        TRUE ~ 0),
                'LTCM' = dplyr::case_when(Q_num %in% seq(21, 23) ~ 1,
                                          TRUE ~ 0), 
                'Dotcom' = dplyr::case_when(Q_num %in% seq(37, 39) ~ 1, 
                                            TRUE ~ 0),
                'Crises' = dplyr::case_when(GR == 1 | EZ == 1 | LTCM == 1 | Dotcom == 1 ~ 1,
                                            TRUE ~ 0))
######## ANY CRISIS #########

# T test (parametric)
mean_test_share_crises <- t.test(filter(nest_share_crises, Crises == 1)$eig_vec_1,
                                 filter(nest_share_crises, Crises == 0)$eig_vec_1,
                                 alternative = "greater")

# (Mann-Whitney) Wilcoxon test (nonparametric)
mean_test_share_crises_wilcox <- wilcox.test(filter(nest_share_crises, Crises == 1)$eig_vec_1,
                                           filter(nest_share_crises, Crises == 0)$eig_vec_1,
                                           alternative = "greater")
# KS test
mean_test_share_crises_ks <- ks.test(filter(nest_share_crises, Crises == 1)$eig_vec_1,
                                   filter(nest_share_crises, Crises == 0)$eig_vec_1,
                                   alternative = "less")
######### GR or EZ ##########

# T test (parametric)
mean_test_share_GREZ <- t.test(filter(nest_share_crises, GR == 1 | EZ == 1)$eig_vec_1,
                                 filter(nest_share_crises, Crises == 0)$eig_vec_1,
                                 alternative = "greater")

# (Mann-Whitney) Wilcoxon test (nonparametric)
mean_test_share_GREZ_wilcox <- wilcox.test(filter(nest_share_crises, GR == 1 | EZ == 1)$eig_vec_1,
                                             filter(nest_share_crises, GR == 1 | EZ == 1)$eig_vec_1,
                                             alternative = "greater")
# KS test
mean_test_share_GREZ_ks <- ks.test(filter(nest_share_crises, GR == 1 | EZ == 1)$eig_vec_1,
                                     filter(nest_share_crises, GR == 1 | EZ == 1)$eig_vec_1,
                                     alternative = "less")


#####################################################################
############ Tests for mean reversion and distribution ##############
#####################################################################

test_SRE_adf <- tseries::adf.test(SRE_US_banks_long$SRE_2, alternative = 'stationary')
test_SRE_KPSS <- tseries::kpss.test(SRE_US_banks_long$SRE_2, null = "Trend")

hist_SRE_pool <- hist(SRE_US_banks_long$SRE_2[SRE_US_banks_long$SRE_2 > 0], 
                      breaks = 100)
plot_density_SRE_pool <- plot(density(SRE_US_banks_long$SRE_2[SRE_US_banks_long$SRE_2 > 0]))
normfit_SRE_pool <- fitdist(SRE_US_banks_long$SRE_2[SRE_US_banks_long$SRE_2 > 0],
                                 'norm')
plot_normfit <- normfit_SRE_pool %>% plot()


