
#load packages
packages <- c("foreign","dplyr","here","tidyr","ggplot2","patchwork");
missing <- packages[!(packages %in% installed.packages()[,"Package"])];

if (length(missing) > 0)
{
  install.packages(missing)
}
lapply(packages,library,character.only = TRUE)

#load the dataset
slim_clean_df <- readRDS(here("data", "processed", "clean_5th_year.rds"))
str(slim_clean_df)
sum(is.na(slim_clean_df))
#numeric data
bankrupt <- slim_clean_df[slim_clean_df$class == 1,]
bankrupt <- subset(bankrupt, select = c(-class))
healthy <- slim_clean_df[slim_clean_df$class == 0,]
healthy <- subset(healthy,select=c(-class))
bankrupt

medians_bankrupt <- sapply(bankrupt,median)
medians_healthy <- sapply(healthy,median)

medians_bankrupt
medians_healthy
#Create a bar chart differencing the companies by current ratio
bar_chart <- ggplot(slim_clean_df,aes(x = current_ratio, fill = as.factor(class),color=as.factor(class))) + 
  geom_histogram(aes(y=after_stat(density)),color = "white", bins = 50, position = "identity", alpha = 0.4) + 
  scale_x_continuous(limits=c(0,5))+
  geom_density(adjust=3,alpha = 0.2, linewidth = 1) + 
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  theme_minimal() +
  labs(y = "Gęstość (Proporcja)", x = "Płynność bieżąca (Current Ratio)")
bar_chart





#Create two violin plots to difference the data by retained and current earning ratios
violin_plot <- ggplot(slim_clean_df,aes(x=class,y=retained_earnings_to_total_assets,color=class,fill=class))+
  geom_violin(alpha=0.5)+
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  scale_y_continuous(limits=c(-0.5,0.5))+
  facet_wrap(~class,scales="free_x")+
  scale_x_discrete(labels = c("0" = "Zdrowa","1" = "Bankrut"))+
  labs(x="Status Firmy",y="Zyski zatrzymane względem aktywów")+
  theme(strip.background=element_blank(), strip.text = element_blank())
violin_plot



violin_plot2 <- ggplot(slim_clean_df,aes(x=class,y=working_capital_to_total_assets,color=class,fill=class))+
  geom_violin(alpha=0.5)+
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  scale_y_continuous(limits=c(-1.5,1.5))+
  facet_wrap(~class,scales="free_x")+
  scale_x_discrete(labels = c("0" = "Zdrowa","1" = "Bankrut"))+
  labs(x="Status Firmy",y="Kapitał krótkoterminowy względem aktywów")+
  theme(strip.background=element_blank(), strip.text = element_blank())
violin_plot2

#combine them into one plot
violins <- violin_plot / violin_plot2
violins 
#seeing the plots, there is a clear trend visible, so we will try to confirm it with hard data
#% of companies with negative working capital per class
mean(healthy$working_capital_to_total_assets < 0) * 100
mean(bankrupt$working_capital_to_total_assets < 0) * 100

# % of companies with negative retained earnings per class
mean(healthy$retained_earnings_to_total_assets < 0) * 100
mean(bankrupt$retained_earnings_to_total_assets < 0) * 100


density_plot <- ggplot(slim_clean_df,aes(x=sales_to_total_assets,fill=class,color=class)) +
  geom_density(alpha=0.5)+
  scale_x_continuous(limits=c(0,5)) +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  labs(x = "Wskaźnik rotacji aktywów", y ="Gęstość") +
  theme_minimal()
density_plot

#As we can see there is a slim range in which most healthy companies fall into, so
#we will try to confirm it with hard data as well

IQR(healthy$sales_to_total_assets)
IQR(bankrupt$sales_to_total_assets)
?IQR
Roa_debt_scatter <- ggplot(slim_clean_df, aes(x = net_profit_to_total_assets, y = total_liabilities_to_total_assets, color = class)) +
  geom_point(alpha = 0.3) + 
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(0, 3)) +
  scale_color_manual(values = c("0" = "skyblue", "1" = "red")) +
  theme_minimal() +
  theme(legend.position = "none")

Roa_debt_scatter

ggsave(here("presentation", "01_current_ratio_density.png"), 
       plot = bar_chart, 
       width = 10, 
       height = 6, 
       dpi = 300, 
       bg = "white")
ggsave(here("presentation","02_current_assets_turnover_density.png"),
       plot = density_plot,
       width = 10,
       height = 6,
       dpi = 300,
       bg = "white")
ggsave(here("presentation","03_investigative_plot_for_plot_2.png"),
       plot = combined,
       width = 10,
       height = 6,
       dpi = 300,
       bg = "white")
ggsave(here("presentation","04_retained_earnings.png"),
       plot = violins,
       width=10,
       height=6,
       dpi=300,
       bg="white")

