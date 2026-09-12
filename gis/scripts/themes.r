map_theme <- function(base_size = 8){ 
  theme_bw(base_size=base_size) +
    theme(plot.title =   element_text(face='bold',size=9,hjust=0.5),
          plot.background = element_rect(fill='white'),
          
          # PANEL
          panel.grid =  element_line(colour = "white"), #element_blank(),
          panel.background = element_rect(fill='white'),
          panel.border = element_blank(),
          
          # AXIS
          axis.text =  element_blank(), 
          axis.title = element_blank(), 
          axis.line = element_line(colour='white'),
          axis.ticks = element_blank(),
          
          # FACET STRIP
          strip.text = element_text(size=8, face='bold',hjust= 0.5), #, vjust = -0.5),
          strip.background = element_blank(),
          
          
          # LEGEND
          #legend.title = element_blank(), 
          legend.key =   element_blank(), 
          legend.position="bottom", 
          legend.box="horizontal",
          legend.text=element_text(size=7), 
          legend.spacing = unit(0, "mm"),
          legend.key.size = unit(3, "mm")) }




#===============================================================================
line_plot_theme <- function(base_size = 8){ 
  
  theme_bw(base_size=base_size) +
    theme(
      
      ### ALL TEXT (inherited everywhere)
      # text = element_text(size=8, colour='black'),
      
      # FACET STRIP
      strip.text = element_text(size=8, face='bold',hjust= 0.5), #, vjust = -0.5),
      strip.background = element_blank(),
      
      ### LEGEND
      
      legend.title = element_text(size=6, color="black"),
      legend.text=element_text(size=6, color="black"), 
      # legend.text = element_text(size = 8),
      legend.background = element_blank(),
      legend.key.size = unit(4, "mm"),
      # legend.title=element_blank(),
      #legend.position = 'top',
      legend.direction = 'vertical',
      legend.justification = "left",
      
      
      ### AXES
      axis.line  = element_line(colour = "black", size=0.3),
      axis.text  = element_text(size=6, colour='black'),
      axis.ticks = element_line(colour='black', size=0.3), 
      

      ### PANEL
      panel.spacing = unit(2, "mm"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
  
      # panel.grid =  element_line(colour = "white"), #element_blank(),
      # panel.background = element_rect(fill='white'),
      # panel.border = element_blank(),
      
      # panel.background = element_rect(fill=NA, colour = "black", size=0.1),
      # panel.spacing = unit(.05, "lines"),
      # panel.border = element_blank()
      #rect(color = "black", fill = NA, size = 0.1))
    )
}

