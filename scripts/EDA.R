packages <- c("foreign","dplyr","here","tidyr","ggplot2","patchwork");
missing <- packages[!(packages %in% installed.packages()[,"Package"])];

if (length(missing) > 0)
{
  install.packages(missing)
}
lapply(packages,library,character.only = TRUE)
slim_clean_df <- readRDS(here("data", "processed", "clean_5th_year.rds"))

bar_chart <- ggplot(slim_clean_df,aes(x = current_ratio, fill = as.factor(class),color=as.factor(class))) + 
  geom_histogram(aes(y=after_stat(density)),color = "white", bins = 50, position = "identity", alpha = 0.4) + 
  scale_x_continuous(limits=c(0,5))+
  geom_density(adjust=3,alpha = 0.2, linewidth = 1) + 
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  theme_minimal() +
  labs(y = "Gęstość (Proporcja)", x = "Płynność bieżąca (Current Ratio)")
bar_chart
boxplot <- ggplot(slim_clean_df,aes(x=class,y=retained_earnings_to_total_assets,color=class))+
  geom_boxplot()+
  geom_jitter(position=position_jitter(0.2))+
  scale_y_continuous(limits=c(-1,1))
boxplot

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
violin_plot
violin_plot2

violins <- violin_plot / violin_plot2
violins 



density_plot <- ggplot(slim_clean_df,aes(x=sales_to_total_assets,fill=class,color=class)) +
  geom_density(alpha=0.5)+
  scale_x_continuous(limits=c(0,5)) +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  labs(x = "Wskaźnik rotacji aktywów", y ="Gęstość") +
  theme_minimal()
density_plot
debt_to_total_assets_rotation_scatter <- ggplot(slim_clean_df, aes(x=total_liabilities_to_total_assets,fill=class,color=class))+
  geom_point()+
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  scale_color_manual(values = c("0" = "skyblue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  scale_x_continuous(limits=c(0,10))+
  scale_y_continuous(limits=c(0,10))+
  facet_wrap(~class)+
  labs(x="Wskaźnik obrotu względem aktywów", y="Dług względem aktywtów")
ROA_scatter <- ggplot(slim_clean_df, aes(net_profit_to_total_assets,y=total_liabilities_to_total_assets,fill=class,color=class))+
  geom_point()+
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  scale_color_manual(values = c("0" = "skyblue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  scale_x_continuous(limits=c(0,2.5))+
  scale_y_continuous(limits=c(0,7))+
  labs(x="Wskaźnik zysku względem aktywów", y="Dług względem aktywtów")
EBIT_scatter <- ggplot(slim_clean_df, aes(x=ebit_to_total_assets,y=total_liabilities_to_total_assets,fill=class,color=class))+
  geom_point()+
  scale_fill_manual(values = c("0" = "skyblue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  scale_color_manual(values = c("0" = "skyblue", "1" = "red"), name = "Status Firmy",labels = c("0" = "Zdrowa", "1" = "Bankrut")) +
  scale_x_continuous(limits=c(0,2.5))+
  scale_y_continuous(limits=c(0,7))+
  labs(x="Wskaźnik obrotu względem aktywów", y="Dług względem aktywtów")
combined <- debt_to_total_assets_rotation_scatter / ROA_scatter / EBIT_scatter
combined

Roa_debt_scatter <- ggplot(slim_clean_df, aes(x = net_profit_to_total_assets, y = total_liabilities_to_total_assets, color = class)) +
  geom_point(alpha = 0.3) + 
  facet_wrap(~ class, labeller = as_labeller(c("0" = "Zdrowa", "1" = "Bankrut"))) +
  scale_x_continuous(limits = c(-3, 3)) +
  scale_y_continuous(limits = c(0, 3)) +
  scale_color_manual(values = c("0" = "skyblue", "1" = "red")) +
  theme_minimal() +
  theme(legend.position = "none")

Roa_debt_scatter



#facet wrap with EBIT + ROA
debt_to_total_assets_rotation_scatter
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

