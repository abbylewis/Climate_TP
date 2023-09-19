
plot_monthly_correlation_map <- function(with_temp, model) {
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
    filter(!is.na(DO_mgL_HYPO),!is.na(mon), !is.na(value))%>%
    group_by(LakeID)%>%
    mutate(hypoxic = ifelse(max(DO_mgL_HYPO)<1,"yes","no"))%>%
    filter(hypoxic=="no")%>%
    group_by(LakeID, mon)%>%
    mutate(nyear = length(unique(Year)))%>%
    filter(nyear>=10,
           #max(value)-min(value)>2
    )%>%
    dplyr::summarize(do_temp = pcor.test(DO_mgL_HYPO,value,Year,method = "spearman")$estimate,
    )
  
  labs = c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug")
  
  anova_res = aov(do_temp~as.factor(mon),data=many_lake_stat)
  summary(anova_res)
  tukey = TukeyHSD(anova_res)
  cld = multcompLetters4(anova_res,tukey)$`as.factor(mon)`$Letters
  cld_df = data.frame(cld = cld, mon = names(cld))%>%
    mutate(mon = factor(as.numeric(mon), levels = c(9:12,1:8), labels = labs))
  
  do_temp_data = many_lake_stat%>%
    group_by(mon)%>%
    mutate(p = wilcox.test(do_temp)$p.value,
           n = n(),
           mon = factor(mon, levels = c(9:12,1:8),labels = labs))
  for_states <- do_temp_data%>%
    filter(mon=="Apr")%>%
    left_join(lat_long)
  states <- ne_states(returnclass = "sf",country = "United States of America")
  us_map = ggplot(data = states) +
    geom_sf() +
    coord_sf(expand = FALSE, ylim = c(42, 47.5), xlim = c(-93,-86.5))+
    geom_point(data = for_states, aes(Longitude_DD, Latitude_DD, fill = do_temp),shape = 21, color = "white", size = 2.8, alpha  =.5, stroke = .4)+
    theme_bw()+
    #annotation_north_arrow(location = "bl", which_north = "true", 
    #                       pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    #                       style = north_arrow_fancy_orienteering) +
    theme(plot.margin = margin(0, 0, 0, 0, "cm"),
          legend.box.background = element_rect(fill = "white", color = "white"))+
    scale_fill_viridis(name = "Correlation between\nDO and April temp",limits = c(-1,1), option = "H")+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())+
    ggtitle(model)
  
  jpeg(paste0("../Figures/DO and temp map April - ",model,".jpg"), 
       width = 6, height = 4, res = 300, units = "in")
  us_map
  dev.off()

  return(us_map)
}
