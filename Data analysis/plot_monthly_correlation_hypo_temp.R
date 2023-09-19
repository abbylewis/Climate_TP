library(ppcor)
library(multcompView)
library(MASS)

plot_monthly_correlation_hypo_temp <- function(with_temp, model) {
  if(model == "ERA5"){prefix = ""}
  if(model == "JRA55"){prefix = "jra55_"}
  if(model == "NARR"){prefix = "narr_"}
  
  many_lake_temp_sum = with_temp%>%
    pivot_longer(cols = contains(paste0(prefix,"temp"), ignore.case = F), names_to = "mon")%>%
    mutate(mon = sub(prefix, "",mon))%>%
    filter(!mon=="mean_temp")%>%
    mutate(mon = month(as.Date(paste0("2022-",substr(mon,6,8),"-01"), format = "%Y-%b-%d")),
           Year = ifelse(mon>=9,Year+1,Year))
  # DO
  many_lake_stat = many_lake_temp_sum%>%
    filter(!is.na(Temp_C_HYPO),!is.na(mon), !is.na(value))%>%
    group_by(LakeID)%>%
    #mutate(hypoxic = ifelse(max(DO_mgL_HYPO)<1,"yes","no"))%>%
    #filter(hypoxic=="no")%>%
    group_by(LakeID, mon)%>%
    mutate(nyear = length(unique(Year)))%>%
    filter(nyear>=10,
           #max(value)-min(value)>2
    )%>%
    dplyr::summarize(temp_temp = pcor.test(Temp_C_HYPO,value,Year,method = "spearman")$estimate,
    )
  
  labs = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug")
  
  anova_res = aov(temp_temp~as.factor(mon),data=many_lake_stat)
  summary(anova_res)
  tukey = TukeyHSD(anova_res)
  cld = multcompLetters4(anova_res,tukey)$`as.factor(mon)`$Letters
  cld_df = data.frame(cld = cld, mon = names(cld))%>%
    mutate(mon = factor(as.numeric(mon), levels = c(9:12,1:8), labels = labs))
  
  jpeg(paste0("../Figures/Hypo and air temp by month - ",model,".jpg"), width = 6, height = 4, res = 300, units = "in")
  temp_temp_data = many_lake_stat%>%
    group_by(mon)%>%
    mutate(p = wilcox.test(temp_temp)$p.value,
           n = n(),
           mon = factor(mon, levels = c(9:12,1:8),labels = labs))
  temp_temp = temp_temp_data%>%
    ggplot(aes(x=mon, y = temp_temp))+
    geom_boxplot(aes(color=p<0.05))+
    geom_text(aes(x = mon, label = cld, y = 1.08),
              data = cld_df)+
    #geom_text(aes(label = paste0("n = ",n), y = max(do_temp)))+ #n is the same across all
    geom_hline(yintercept=0)+
    ylim(-1,1.08)+
    xlab("Air temp. month")+
    ylab(paste0("Correlation with\nlate-summer hypo. temp\n(n = ",unique(temp_temp_data$n),")"))+
    theme_bw()+
    ggtitle(model)+
    scale_color_manual(values = c("grey50","#FB8B24"))+
    theme(axis.text.x = element_text(hjust = ifelse(levels(temp_temp_data$mon) == "N:     Sep\nS:     Mar", .8, .5)))
  temp_temp
  dev.off()
  
  return(temp_temp)
}
