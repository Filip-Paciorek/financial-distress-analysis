packages <- c("foreign","dplyr","here","tidyr","ggplot2");
missing <- packages[!(packages %in% installed.packages()[,"Package"])];

if (length(missing) > 0)
{
  install.packages(missing)
}
lapply(packages,library,character.only = TRUE)
slim_clean_df <- readRDS(here("data", "processed", "clean_5th_year.rds"))

bar_chart <- ggplot(slim_clean_df,aes(x = current_ratio, fill = as.factor(class),color=as.factor(class))) + 
  geom_histogram(aes(y=after_stat(density)),color = "white", bins = 50, position = "identity", alpha = 0.4) + 
  scale_x_continuous(limits=c(0,10))+
  geom_density(adjust=3,alpha = 0.2, linewidth = 1) + 
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  theme_minimal() +
  labs(y = "Gęstość (Proporcja)", x = "Płynność bieżąca (Current Ratio)")
bar_chart
ggsave(here("presentation", "01_current_ratio_density.png"), 
       plot = bar_chart, 
       width = 10, 
       height = 6, 
       dpi = 300, 
       bg = "white")
