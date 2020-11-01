# -------------------------------- Data viz 1.0 Giro -----------------------------------------#
#                                                                                             #
# Omschrijving: Script for data visualization of Giro stages and power                        #
# Auteurs:      Stephan van der Zwaard                                                        #
# Datum:        01-11-2020                                                                    #
# Versie:       1.0                                                                           #
#                                                                                             #
# --------------------------------------------------------------------------------------------#


# --------------------------------- Prerequisites -------------------------------------------
  

  # Load libraries
  library(readxl);   library(ggplot2);   library(RColorBrewer);
  library(tidyverse); library(ggridges); library(httr);
  library(rvest)


# --------------------------------- Data collection -----------------------------------------


  # Load data (obtained from VeloFacts)
  giro_data <- read_excel("data/Giro d'Italia 2020.xlsx")
  
  
  # Scrape stage characteristics from Wikipedia
  webpage   <- read_html("https://en.wikipedia.org/wiki/2020_Giro_d'Italia")
  stages    <- webpage %>% html_nodes('td:nth-child(6)') %>%  html_text() %>% as.data.frame() %>% filter(row_number() <=21) %>% setNames("stage_description")


# ------------------------------- Data preprocessing ----------------------------------------
  
  
  # Preprocessing
  stages   <- stages %>% mutate(stage_description = gsub(" stage","",gsub("\n","",stage_description))) %>%
              rowid_to_column('Stage') %>%
              mutate(stage_description = factor(stage_description,levels = c("Flat","Hilly","Medium mountain","Mountain","Mountain time trial")))
  
  
  giro_data <- giro_data %>% pivot_longer(`Stage 1`: `Stage 21`,
                                         names_to  = "Stage",
                                         values_to = "Power") %>%
              mutate(Stage = as.numeric(gsub("Stage ","",Stage))) %>%
              left_join(stages, by = c("Stage"="Stage")) %>%
              mutate(Stage = as.factor(Stage)) 

  
# ------------------------------- Data visualization ----------------------------------------
  
    # Load XKCD theme for fonts
    font_import(".")   ## because we downloaded to working directory
    loadfonts()
    colorscale <- c('#5c9c55','#b0b657','#e7db54','#B48223','#90681c')
    
    # Create image with ggplot
     p <- ggplot(giro_data, aes(x=Power, y=reorder(Stage,-as.numeric(Stage)), fill = stage_description)) +
           geom_density_ridges( bandwidth=9, alpha = 0.8) +
           scale_fill_manual(values = colorscale) +
           labs(y="Stage",x="Average power",fill="Stage type",caption="Source: data from Velofacts and Wikipedia\nCreated by:     StephanvdZwaard") +
             ggtitle("Giro d'Italia 2020") +
             theme_classic() + 
             theme(legend.position="right",
                   legend.background = element_rect(fill = "transparent", color = NA), # bg of the panel
                   legend.text =  element_text(color = "black", hjust = 0, size = 10),
                   text=element_text(size=16, family="xkcd"),
                   panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
                   plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                   plot.caption = element_text(color = "black", hjust = 0, size = 10, margin = margin(t = 15, r = 0, b = 0, l = 0)),
                   plot.title   = element_text(color = "black", hjust = 0.1, size = 30, margin = margin(t = 0, r = 0, b = 15, l = 0)),
                   axis.line = element_line(color = "black"),
                   axis.ticks = element_line(color = "black"),
                   axis.text   =element_text(size=16, family="xkcd", color = "black"),
                   axis.title.x=element_text(size=22, family="xkcd", color = "black",margin = margin(t = 15, r = 0, b = 0, l = 0)),
                   axis.title.y=element_text(size=22, family="xkcd", color = "black",margin = margin(t = 0, r = 15, b = 0, l = 0)))
     
     # Export image
     png(file=paste0("results/Giro2020.png"), width=160,height=160,units = "mm", bg = "transparent", res = 200, type = "quartz")
     p
     dev.off()

# ------------------------------------ END OF SYNTAX --------------------------------------