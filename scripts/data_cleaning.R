#load packages
packages <- c("foreign","dplyr","here","tidyr");

missing <- packages[!(packages %in% installed.packages()[,"Package"])];

if (length(missing) > 0)
{
  install.packages(missing)
}
lapply(packages,library,character.only = TRUE)
#load data + change column names
raw_data <- read.arff(here("data","raw","5year.arff"));
col_names <-c(
  "net_profit_to_total_assets",
  "total_liabilities_to_total_assets",
  "working_capital_to_total_assets",
  "current_ratio",
  "cash_sec_rec_liab_to_opex_depr_x365",
  "retained_earnings_to_total_assets",
  "ebit_to_total_assets",
  "book_equity_to_total_liabilities",
  "sales_to_total_assets",
  "equity_to_total_assets",
  "gross_extraord_fin_to_total_assets",
  "gross_profit_to_st_liabilities",
  "gross_profit_depr_to_sales",
  "gross_profit_interest_to_total_assets",
  "total_liab_x365_to_gross_profit_depr",
  "gross_profit_depr_to_total_liab",
  "total_assets_to_total_liabilities",
  "gross_profit_to_total_assets",
  "gross_profit_to_sales",
  "inventory_days",
  "sales_growth",
  "operating_profit_to_total_assets",
  "net_profit_margin",
  "gross_profit_3y_to_total_assets",
  "equity_minus_share_capital_to_assets",
  "net_profit_depr_to_total_liab",
  "operating_profit_to_fin_expenses",
  "working_capital_to_fixed_assets",
  "log_total_assets",
  "liabilities_minus_cash_to_sales",
  "gross_profit_interest_to_sales",
  "current_liab_x365_to_cogs",
  "opex_to_st_liabilities",
  "opex_to_total_liabilities",
  "profit_on_sales_to_total_assets",
  "total_sales_to_total_assets",
  "current_assets_no_inv_to_lt_liab",
  "constant_capital_to_total_assets",
  "profit_on_sales_to_sales",
  "quick_ratio_cash_only",
  "total_liab_to_op_profit_depr_annualized",
  "operating_profit_to_sales",
  "receivables_plus_inventory_turnover_days",
  "receivables_days",
  "net_profit_to_inventory",
  "quick_ratio",
  "inventory_turnover_cogs_days",
  "ebitda_to_total_assets",
  "ebitda_to_sales",
  "current_assets_to_total_liab",
  "st_liabilities_to_total_assets",
  "st_liabilities_x365_to_cogs",
  "equity_to_fixed_assets",
  "constant_capital_to_fixed_assets",
  "working_capital",
  "gross_margin_ratio",
  "adjusted_liquidity_ratio",
  "total_costs_to_total_sales",
  "lt_liabilities_to_equity",
  "sales_to_inventory",
  "sales_to_receivables",
  "st_liabilities_x365_to_sales",
  "sales_to_st_liabilities",
  "sales_to_fixed_assets"
)
colnames(raw_data)[1:64] <- col_names
colnames(raw_data)
#analyze NA's and decide what to do with them
head(raw_data)
tail(raw_data)
mean(is.na(raw_data)) * 100
nrow(raw_data)
bankrupt_data <- raw_data[raw_data$class == "1", ]
sort(colSums(is.na(bankrupt_data)), decreasing = TRUE)
slim_clean_df <- subset(raw_data,select=c(net_profit_to_total_assets,total_liabilities_to_total_assets,working_capital_to_total_assets,current_ratio,retained_earnings_to_total_assets,ebit_to_total_assets,book_equity_to_total_liabilities,equity_to_total_assets,sales_to_total_assets,class))
#99% cutdown on missing data
mean(is.na(slim_clean_df)) * 100
slim_clean_df <- drop_na(slim_clean_df)
#one company cut out
sum(slim_clean_df$class == 1)
saveRDS(slim_clean_df, file = here("data", "processed", "clean_5th_year.rds"))
fat_clean_df <- subset(raw_data,select=-c(current_assets_no_inv_to_lt_liab,operating_profit_to_fin_expenses,sales_growth))
fat_clean_df <- drop_na(fat_clean_df)
#49 companies cut out, but way more columns left
sum(fat_clean_df$class == 1)
saveRDS(fat_clean_df, file = here("data","processed","clean_5th_year_fat.rds"))










