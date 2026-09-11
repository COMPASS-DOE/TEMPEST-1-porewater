```{r figures for COMPASS Meeting 2/12}
Start = as.POSIXct("2022-06-22 05:30:00", tz="UTC")
End = as.POSIXct("2022-06-22 14:30:00", tz="UTC")
Start_2023_1 <- as.POSIXct("2023-06-06 06:00:00", tz= "UTC")
End_2023_1 <- as.POSIXct("2023-06-06 16:00:00", tz= "UTC")
Start_2023_2 <- as.POSIXct("2023-06-07 06:00:00", tz="UTC")
End_2023_2 <- as.POSIXct("2023-06-07 16:00:00", tz= "UTC")
plot_order <- c('Control Plot', 'Freshwater Plot','Estuarine-water Plot')
Anyas_colors_alpha_order = c("springgreen2", "violetred2", "cyan2")

plot_all_the_doc <- all_the_doc %>%
  ggplot() +
  #     # Water 2022
  annotate("rect", xmin = Start, xmax = End, ymin = -Inf, ymax = Inf, fill = "blue")+
  #  # Water 1
  annotate("rect", xmin = Start_2023_1, xmax = End_2023_1, ymin = -Inf, ymax = Inf, fill = "blue") +
  # #Water 2
  annotate("rect", xmin = Start_2023_2 , xmax = End_2023_2, ymin = -Inf, ymax = Inf, fill = "blue") +
  scale_x_datetime(minor_breaks= waiver(),date_minor_breaks = "1 month", date_breaks="2 months",date_labels= '%b %y')+
  geom_errorbar(aes(x = plot_date, ymin =doc_mg_l - doc_mg_l_sd, ymax=doc_mg_l + doc_mg_l_sd, color = Plot))+
  geom_point(aes(x = plot_date, y =doc_mg_l, color = Plot), size = 2)+
  # geom_line(aes(x = plot_date, y =doc_mg_l, color = Plot))+
  ylab("DOC mgC/L")+
  xlab("Date")+
  ylim(0,100)+
  scale_color_manual(values=Anyas_colors_alpha_order) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_classic()

print(plot_all_the_doc)
plotly::ggplotly(plot_all_the_doc)
cowplot::save_plot("./figures/doc_pw_all_2022-2023.png",plot_all_the_doc, dpi=300, base_aspect_ratio = 2:1)
```

```{r}
plot_FI <- all_eems %>%
  ggplot() +
  #     # Water 2022
  annotate("rect", xmin = Start, xmax = End, ymin = -Inf, ymax = Inf, fill = "blue")+
  #  # Water 1
  annotate("rect", xmin = Start_2023_1, xmax = End_2023_1, ymin = -Inf, ymax = Inf, fill = "blue") +
  # #Water 2
  annotate("rect", xmin = Start_2023_2 , xmax = End_2023_2, ymin = -Inf, ymax = Inf, fill = "blue") +
  scale_x_datetime(minor_breaks= waiver(),date_minor_breaks = "1 month", date_breaks="2 months",date_labels= '%b %y')+
  geom_point(aes(x = plot_date, y = FI, color = Plot), size = 2)+
  # geom_line(aes(x = plot_date, y =doc_mg_l, color = Plot))+
  ylab("Fluorescence Index")+
  xlab("Date")+
  scale_color_manual(values=Anyas_colors_alpha_order) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_classic()

print(plot_FI)
plotly::ggplotly(plot_FI)
cowplot::save_plot("./figures/FI_pw_all_2022-2023.png",plot_FI, dpi=300, base_aspect_ratio = 2:1)

plot_HIX <- all_eems %>%
  ggplot() +
  #     # Water 2022
  annotate("rect", xmin = Start, xmax = End, ymin = -Inf, ymax = Inf, fill = "blue")+
  #  # Water 1
  annotate("rect", xmin = Start_2023_1, xmax = End_2023_1, ymin = -Inf, ymax = Inf, fill = "blue") +
  # #Water 2
  annotate("rect", xmin = Start_2023_2 , xmax = End_2023_2, ymin = -Inf, ymax = Inf, fill = "blue") +
  scale_x_datetime(minor_breaks= waiver(),date_minor_breaks = "1 month", date_breaks="2 months",date_labels= '%b %y')+
  geom_point(aes(x = plot_date, y = HIX, color = Plot), size = 2)+
  # geom_line(aes(x = plot_date, y =doc_mg_l, color = Plot))+
  ylab("Humification Index (HIX)")+
  xlab("Date")+
  scale_color_manual(values=Anyas_colors_alpha_order) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_classic()

print(plot_HIX)
plotly::ggplotly(plot_HIX)
cowplot::save_plot("./figures/HIX_pw_all_2022-2023.png",plot_HIX, dpi=300, base_aspect_ratio = 2:1)
```

```{r}
plotly_plot <- all_the_doc %>%
  ggplot() +
  #     # Water 2022
  annotate("rect", xmin = Start, xmax = End, ymin = -Inf, ymax = Inf, fill = "blue")+
  #  # Water 1
  annotate("rect", xmin = as.POSIXct("2023-06-06 06:00:00", tz= "UTC"), xmax = as.POSIXct("2023-06-06 16:00:00", tz= "UTC"), ymin = -Inf, ymax = Inf, fill = "blue") +
  # #Water 2
  annotate("rect", xmin = as.POSIXct("2023-06-07 06:00:00", tz="UTC"), xmax = as.POSIXct("2023-06-07 16:00:00", tz= "UTC"), ymin = -Inf, ymax = Inf, fill = "blue") +
  geom_errorbar(aes(x = as.POSIXct(plot_date, tz= "UTC"), ymin =doc_mg_l - doc_mg_l_sd, ymax=doc_mg_l + doc_mg_l_sd, color = Plot))+
  geom_point(aes(x = as.POSIXct(plot_date, tz = "UTC"), y =doc_mg_l, color = Plot), size = 2)+
  ylab("DOC mgC/L")+
  xlab("Date")+
  ylim(0,80)+
  scale_color_manual(values=Anyas_colors_alpha_order) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_classic()

plotly::ggplotly(plotly_plot)
````
