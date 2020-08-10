########################################################################
########### Redoing US Bank project for journal submission #############
########################################################################

### Load libraries ###

library(tidyverse)
library(moments)



############################
### Directory_Management ###
############################

# For reproduction of this script change the address
# of the directories to conform to their location in 
# the host machine.

### Read file ### 

file_name_US_banks_daily <- "SIC_6000_6799_202003.dta" #CRSP daily return file

# Calculate time needed to read whole ~9 GB file, not suitable for small RAMs
time_pre_read_CRSP <- Sys.time()
data_US_banks_daily <- haven::read_dta(file_name_US_banks_daily)
time_post_read_CRSP <- Sys.time()

message("Read CRSP file. Time taken to read file = ", 
        round(time_post_read_CRSP - time_pre_read_CRSP, 2), " min")


#############################################
### Data Filtration, Cleaning and Tidying ###
#############################################

### Admissible SIC codes
ind_comm_banks <- c(6020:6029) #commercial banks
ind_saving_inst <- c(6030:6039) #saving institutions
ind_credit_union <- c(6060:6069) #credit unions
ind_bank_hold <- c(6710:6712) #bank holding companies

ind_bank_use <- c(ind_comm_banks, ind_saving_inst,
                  ind_credit_union, ind_bank_hold) #use only these

### Admissible share codes (source: http://www.crsp.com/products/documentation/data-definitions-1)

ind_share_code_common <- c(10, 11) #only common shares included

# start year of sample
year_min <- 1993

### Filter CRSP data ###

# Calculate time needed to filter whole ~9 GB file
time_pre_filter_CRSP <- Sys.time()
# Filter banks with common shares post year_min and nominal price > 1
data_US_banks_daily_2 <- data_US_banks_daily %>% 
  dplyr::filter(siccd %in% ind_bank_use | hsiccd %in% ind_bank_use) %>% #ignore non-banks
  dplyr::filter(shrcd %in% ind_share_code_common) %>% #include common shares only
  dplyr::filter(lubridate::year(date) >= year_min) %>% #only banks post year_min
  dplyr::filter(prc > 1) #ignore banks with nominal price <= $1

time_post_filter_CRSP <- Sys.time()
message("Filtered CRSP file. Time taken to filter file = ", 
        round(time_post_filter_CRSP - time_pre_filter_CRSP, 2), " min")

# Remove from workspace, the (very) heavy original data file
rm(data_US_banks_daily)

# Select bank names, codes and daily stock returns
returns_daily_banks_US <- data_US_banks_daily_2 %>%
  dplyr::select(date, comnam, siccd, ret, hsiccd, shrcd, ncusip, cusip)

# Remove the (heavy) derived data file
# rm(data_US_banks_daily_2)

# Label banks according to type
returns_daily_banks_US <- returns_daily_banks_US %>%
  dplyr::mutate('type_sic' = dplyr::case_when(siccd %in% ind_bank_hold ~ "HC", #holding company
                                               siccd %in% ind_comm_banks ~ "CB", #commercial banks
                                               siccd %in% ind_credit_union ~ "CU", #credit unions
                                               siccd %in% ind_saving_inst ~ "SI"), #savings institutions
                'type_hsic' = dplyr::case_when(hsiccd %in% ind_bank_hold ~ "HC", #holding company
                                                   hsiccd %in% ind_comm_banks ~ "CB", #commercial banks
                                                   hsiccd %in% ind_credit_union ~ "CU", #credit unions
                                                   hsiccd %in% ind_saving_inst ~ "SI") #savings institutions
                ) 

# Name of banks in the full sample
name_banks_US <- returns_daily_banks_US %>% 
  dplyr::select(comnam) %>%
  dplyr::distinct()

# Add year and quarter column
returns_daily_banks_US <- returns_daily_banks_US %>%
  dplyr::mutate('Year' = lubridate::year(date),
                'Quarter' = lubridate::quarter(date)) %>%
  dplyr::select(date, Year, Quarter, everything())

# Generate quarter sequence from 1 to 4*year_last
returns_daily_banks_US <- returns_daily_banks_US %>%
  # the following formula generates quarter sequence
  dplyr::mutate('Q_num' = 4*(Year - (year_min - 1) - 1) + Quarter) %>% 
  dplyr::select(date, Year, Quarter, Q_num, everything())

# Nest quarterly 
nest_quarter_banks_US <- returns_daily_banks_US %>%
  dplyr::group_by(Q_num) %>%
  tidyr::nest()


func_rm_date_dupli <- function(df)
{
  # This function accepts a dataframe
  # and returns non-duplicated date rows 
  temp_df <- df %>%
    dplyr::group_by(date, comnam) %>%
    dplyr::distinct(., date, .keep_all = T)
  
  return(temp_df)
}

# Remove duplicated dates
nest_quarter_banks_US <- nest_quarter_banks_US %>%
  dplyr::mutate('data_no_duplic' = purrr::map(data, func_rm_date_dupli)) %>%
  dplyr::select(-data)

func_ret_wide <- function(df)
{
  # This function accepts the long-format full quarterly
  # dataframe and returns date, bank and daily return
  # in wide format
  temp_l <- df %>% dplyr::select(date, comnam, ret)
  # In wide format
  temp_w <- tidyr::spread(temp_l, key = comnam, value = ret)
  
  return(temp_w)
}

# How many valid returns to use per quarter?
min_obs_usable <- 30

func_valid_ret <- function(df, n = min_obs_usable)
{
  # This function accepts a return data matrix and 
  # accepts only those columns which have enough
  # usable entries ( >= min_obs_usable)
  temp_NA <- apply(df, 2, function(vec){sum(is.na(vec))})
  # ignore columns with too many missing values
  df_2 <- df[, temp_NA < (nrow(df) - n)] 
  
  return(df_2)
}

func_med_NA_df <- function(df)
{
  # This function accepts a data frame with missing values and
  # returns a data frame where missing values are replaced with
  # column medians after ignoring the date column
  func_med_NA_vec <- function(vec)
  {
    vec[is.na(vec) | is.infinite(vec) | is.nan(vec)] <- median(vec, na.rm = T)
    return(vec)
  }
  # Store date separately
  temp_date <- df$date
  # Replace residual missing values with column medians
  df_2 <- apply(df[, -1], 2, func_med_NA_vec) %>% 
    tibble::as_tibble()
  # Reattach the date column
  df_2 <- df_2 %>%
    tibble::add_column('date' = temp_date) %>%
    dplyr::select(date, everything())
  
  return(df_2)
}

func_cov_rm_date <- function(df)
{
  # This function accepts a dataframe and returns the 
  # covariance matrix after ignoring the first date column
  return(cov(df[, -1]))
}

## Time taken for eigenvector computation
time_pre_eigen_CRSP <- Sys.time()

nest_quarter_banks_US <- nest_quarter_banks_US %>%
 dplyr::mutate('data_qtr_wide' = purrr::map(data_no_duplic, func_ret_wide),
               'data_qtr_clean_col' = purrr::map(data_qtr_wide, func_valid_ret),
               'data_qtr_clean' = purrr::map(data_qtr_clean_col, func_med_NA_df),
               'cov_matrix' = purrr::map(data_qtr_clean, func_cov_rm_date),
               'eig_val' = purrr::map(cov_matrix, function(df){return(eigen(df)$values)}),
               'eig_vec' = purrr::map(cov_matrix, function(df){return(eigen(df)$vectors)}),
               'share' = purrr::map(eig_val, function(vec){return(cumsum(vec)/sum(vec))})
               ) %>%
  dplyr::select(-c(data_no_duplic, data_qtr_wide, data_qtr_clean_col))

time_post_eigen_CRSP <- Sys.time()

message("Eigenvectors computed. Time taken = ", 
        round(time_post_eigen_CRSP - time_pre_eigen_CRSP, 2), "(sec/min)")

###########################################################################################
################### Computing out of sample principal components ##########################
###########################################################################################

nest_quarter_PC <- nest_quarter_banks_US %>%
  dplyr::select(Q_num, data_qtr_clean, eig_vec, share)

eig_vec_lag_list <- dplyr::lag(nest_quarter_banks_US$eig_vec)

nest_quarter_PC <- nest_quarter_PC %>%
  tibble::add_column('eig_vec_lag' = eig_vec_lag_list)


func_eig_share <- function(vec_share, m = 1)
{
  # This function takes in the share of eigenvector 
  # contribution and returns the first 
  
  temp <- vec_share[1:m]
  
  return(temp)
}

# Share of top eigenvectors
eig_share_top <- purrr::map_dbl(nest_quarter_PC$share, func_eig_share)

func_pc_90 <- function(vec_share)
{
  # This function accepts a cumulative share of variance
  # vector and returns the number of PCs needed for 90% coverage 
  return(min(which(vec_share >= 0.90)))
}

# How many PCs needed each quarter to explain 90% return variance?

nest_quarter_PC <- nest_quarter_PC %>%
  dplyr::mutate('num_pc_90' = purrr::map_dbl(share, func_pc_90)) %>%
  dplyr::select(-share)


func_pc_out_sample <- function(df1, df2)
{
  # This function accepts 2 dataframes, compares their
  # columns and rows to see if they can be multiplied,
  # then finds submatrices compatible with multiplication,
  # and multiplies the two matrices
  
  data_matrix <- as.matrix(df1)
  ncol_data <- ncol(data_matrix) #column number in data
  nrow_eig <- nrow(df2) #row number in eigenmatrix
  
  if (ncol_data < nrow_eig) #if num of row is more
  {
    eig_mat <- df2[1:ncol_data, ] #select submatrix
    temp <- data_matrix%*%eig_mat #multiply
  } else 
  {
    data_mat <- data_matrix[, 1:nrow_eig] #select submatrix
    temp <- data_mat%*%df2 #multiply
  }
  
  pc_out_sample <- temp
  
  return(pc_out_sample)
}

# Compute list column of principal components
nest_quarter_PC <- nest_quarter_PC %>%
  # ignore NA items
  dplyr::filter(!(purrr::map_lgl(eig_vec_lag, 
                                 function(df){return(any(is.na(df)))}))) %>%
  # ignore empty (NULL) eigvec lag 
  dplyr::filter(!(purrr::map_lgl(eig_vec_lag, is.null))) %>%
  dplyr::mutate('data_qtr_clean_2' = purrr::map(data_qtr_clean, #ignore the first date column
                                                function(df){return(df[,-1])}),
                'pc_full' = purrr::map2(data_qtr_clean_2, eig_vec_lag, 
                                        func_pc_out_sample), #multiply compatible matrices
                'pc_out_sample_90' = purrr::map2(pc_full, num_pc_90, #pick columns till coverage = 90%
                                                 function(df, num_pc){return(df[, 1:num_pc])})) %>%
  dplyr::select(-c(eig_vec, data_qtr_clean))

#####################################################################################################
############################## Principal Component Regressions Begin Here ###########################
#####################################################################################################

func_lm_SRE <- function(df1, df2)
{
  # This function accepts the LHS and RHS matrices of regressions
  # and returns the systematic risk exposure of bank = adj_R_sqr
  lhs <- as.matrix(df1)
  rhs <- as.matrix(df2)
  
  adj_rsq <- list(NULL) #initialize list with NULL
  
  for (j in 1:ncol(lhs)) #for each LHS country
  {
    lm_summary <- summary(lm(formula = lhs[, j] ~ rhs))
    
    adj_rsq[[j]] <- 100*max(lm_summary$adj.r.squared, 0)
  }
  
  return(unlist(adj_rsq))
}


### Compute systematic risk exposure (SRE) ###

## Time taken for computing systematic risk exposure ##
time_pre_SRE_CRSP <- Sys.time()

nest_quarter_pc_regression <- nest_quarter_PC %>%
  dplyr::select(Q_num, data_qtr_clean_2, pc_out_sample_90)

nest_quarter_pc_regression <- nest_quarter_pc_regression %>%
  dplyr::mutate('SRE' = purrr::map2(data_qtr_clean_2, pc_out_sample_90, 
                                    func_lm_SRE))

time_post_SRE_CRSP <- Sys.time()

message("Systematic risk exposures computed. Time taken = ", 
        round(time_post_SRE_CRSP - time_pre_SRE_CRSP, 2), "(sec/min)")

func_attach_name <- function(df_1, vec)
{
  # Accepts regression LHS matrix and unnamed 
  # systematic risk exposure vector and attaches 
  # the column names of LHS to the vector of indices
  names(vec) <- colnames(df_1)
  return(vec)
}

# Attach names of banks to systematic risk exposure values
nest_quarter_pc_regression <- nest_quarter_pc_regression %>%
  dplyr::mutate('SRE_2' = purrr::map2(data_qtr_clean_2, SRE, func_attach_name))

func_pick_name <- function(vec_name) {return(names(vec_name))} #extract names

# Unnest results in long format
SRE_US_banks_long <- nest_quarter_pc_regression %>%
  dplyr::mutate('Bank' = purrr::map(SRE_2, func_pick_name)) %>%
  dplyr::select(Q_num, Bank, SRE_2) %>%
  tidyr::unnest(.)

# Spread in wide format
SRE_US_banks_wide <- SRE_US_banks_long %>%
  tidyr::spread(key = Bank, value = SRE_2) 

### Write out as .csv files ###

# Wide format
# readr::write_csv(SRE_US_banks_wide, 'Syst_Risk_Expos_wide.csv')

# Panel format
# readr::write_csv(SRE_US_banks_long, 'Syst_Risk_Expos_panel.csv')










