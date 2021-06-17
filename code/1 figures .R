####################################
# SECO C4TP 

# Figures and plots

# Apolline Duclaux 14.06.2021
####################################

library(gtalibrary)
library(ggplot2)
library(tidyverse)
gta_colour_palette()

gta_setwd()

project.path="0 projects/57 SECO C4TP/"


## setup
seco.country="Serbia"
result.path=paste0(project.path,"results/",seco.country,"/")
data.path=paste0(project.path, "data/")
load(paste0(data.path, seco.country, ".Rdata"))


## Figure 1 ##
# Interventions per implementer: 
# Create a map for liberalising and one for harmful interventions. 
# Use the counts for the colour shading and green for liberalising, red for harmful.
plot.data = subset(gta.data, is.na(date.implemented)==F & (is.na(date.removed)|date.removed>=Sys.Date()))%>% group_by(implementing.jurisdiction, gta.evaluation)%>% summarize(value = n())
plot.data$implementing.jurisdiction = as.character(plot.data$implementing.jurisdiction)

# Liberalising

plotname = paste0("Map of liberalising interventions - ", seco.country)

plot.data.green = gta_plot_map_df(data = subset(plot.data, gta.evaluation=="Green"), countries="implementing.jurisdiction", values="value")


if(max(plot.data.green$value, na.rm = T)>=100){
  
  legend.max=round(max(plot.data.green$value, na.rm = T)/100,0)*100
  
} else {
  
  legend.max = 100
}


plot.green = ggplot() +
  geom_polygon(data= subset(plot.data.green, country != "Antarctica"), aes(x = long, y = lat, group = group), fill="#dadada", size = 0.15, color = "white") +
  geom_polygon(data= subset(plot.data.green, country != "Antarctica"), aes(x = long, y = lat, group = group, fill=(value)), size = 0.15, color = "white") +
  geom_polygon(data=subset(plot.data.green, country == "Greenland"), aes(x=long, y=lat, group = group), fill="#dadada", size = 0.15, colour = "white") +
  coord_fixed() + # Important to fix world map proportions
  scale_x_continuous(limits=c(-13900000,17000000))+
  labs(x="", y="",caption=paste0("Source: Global Trade Alert Database. Data extracted on ",Sys.Date(), ".")) +
  ggtitle(paste0("Increased export opportunities for ", seco.country))+
  scale_fill_gradientn(name="Number of interventions currently in force",
                       na.value="#c6c6c6",
                       limits=c(0,legend.max),
                       colors = gta_colour$green.shades(7)[c(7,5,3,1)],
                       breaks=round(seq(0, legend.max, legend.max / 5)),
                       guide=guide_colorbar(barwidth=13, label.hjust = 0.5, title.position = "top"),
                       labels=round(seq(0, legend.max, legend.max / 5)))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.5,0),
        legend.justification = c(0.5,0.3),
        legend.direction = "horizontal",
        plot.title = element_text(family = "Open Sans", face = "bold", colour = "#333333", size = 11.5, hjust = 0.5, margin = margin(b=10,t=20)),
        plot.subtitle = element_text(family = "Open Sans", face = "bold", colour = "#333333", size = 8, hjust = 0.5, margin = margin(b=10)),
        legend.title = element_text(vjust= 0.3, hjust = 0.5, family="", colour = "#333333", size = 8, margin = margin(r=10,b=5)),
        legend.text = element_text(family="", colour = "#333333", size = 8, angle = 0, hjust=0, vjust=1, margin = margin(r=10)),
        legend.text.align = 0,
        legend.key = element_rect(size=0),
        #legend.key.width = unit(1, 'line'),
        legend.key.height = unit(0.5, 'cm'),
        legend.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="#F9F9F9"),
        plot.margin = unit(c(0,0.04,0.05,0.04), "in"),
        plot.caption = element_text(hjust = 0.5, vjust = 0, margin = margin(t=43,b=1),size=7, color="#777777")
        
  ) +
  guides(
    #fill=guide_legend(title="", label.position = "bottom",title.position = "top",col = guide_legend(nrow = 1,byrow = F)),
         ymax=guide_legend(title="size"))
plot.green

gta_plot_saver(plot=plot.green,
               path=paste0(result.path),
               name=plotname,
               png=T,
               pdf=F,
               jpg=F,
               width = 20,
               height = 14)

# Harmful

plotname = paste0("Map of harmful interventions - ", seco.country)

for (country in unique(plot.data$implementing.jurisdiction)){
  plot.data$value[plot.data$implementing.jurisdiction==country & plot.data$gta.evaluation=="Red"] = 
    sum(plot.data$value[plot.data$implementing.jurisdiction==country & plot.data$gta.evaluation!="Green"])
}
plot.data = plot.data[plot.data$gta.evaluation!="Amber",]
plot.data.red = gta_plot_map_df(data = subset(plot.data, gta.evaluation=="Red"), countries="implementing.jurisdiction", values="value")


if(max(plot.data.red$value, na.rm = T)>=100){
  
  legend.max=round(max(plot.data.red$value, na.rm = T)/100,0)*100
  
} else {
  
  legend.max = 100
}

plot.red = ggplot() +
  geom_polygon(data= subset(plot.data.red, country != "Antarctica"), aes(x = long, y = lat, group = group), fill="#dadada", size = 0.15, color = "white") +
  geom_polygon(data= subset(plot.data.red, country != "Antarctica"), aes(x = long, y = lat, group = group, fill=(value)), size = 0.15, color = "white") +
  geom_polygon(data=subset(plot.data.red, country == "Greenland"), aes(x=long, y=lat, group = group), fill="#dadada", size = 0.15, colour = "white") +
  coord_fixed() + # Important to fix world map proportions
  scale_x_continuous(limits=c(-13900000,17000000))+
  labs(x="", y="",caption=paste0("Source: Global Trade Alert Database. Data extracted on ",Sys.Date(), ".")) +
  ggtitle(paste0("Diminished export opportunities for ", seco.country))+
  scale_fill_gradientn(name="Number of interventions currently in force",
                       na.value="#c6c6c6",
                       limits=c(0,legend.max),
                       colors = gta_colour$red.shades(7)[c(7,5,3,1)],
                       breaks=round(seq(0, legend.max, legend.max / 5)),
                       guide=guide_colorbar(barwidth=13, label.hjust = 0.5, title.position = "top"),
                       labels=round(seq(0, legend.max, legend.max / 5)))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.5,0),
        legend.justification = c(0.5,0.3),
        legend.direction = "horizontal",
        plot.title = element_text(family = "Open Sans", face = "bold", colour = "#333333", size = 11.5, hjust = 0.5, margin = margin(b=10,t=20)),
        plot.subtitle = element_text(family = "Open Sans", face = "bold", colour = "#333333", size = 8, hjust = 0.5, margin = margin(b=10)),
        legend.title = element_text(vjust= 0.3, hjust = 0.5, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10,b=5)),
        legend.text = element_text(family="", colour = "#333333", size = 11*0.8, angle = 0, hjust=0, vjust=1, margin = margin(r=10)),
        legend.text.align = 0,
        legend.key = element_rect(size=0),
        legend.key.height = unit(0.5, 'cm'),
        legend.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="#F9F9F9"),
        plot.margin = unit(c(0,0.04,0.05,0.04), "in"),
        plot.caption = element_text(hjust = 0.5, vjust = 0, margin = margin(t=43,b=1),size=7, color="#777777")
        
  ) +
  guides(
    #fill=guide_legend(title="", label.position = "bottom",title.position = "top",col = guide_legend(nrow = 1,byrow = F)),
         ymax=guide_legend(title="size"))
plot.red

gta_plot_saver(plot=plot.red,
               path=paste0(result.path),
               name=plotname,
               png=T,
               pdf=F,
               jpg=F,
               width = 20,
               height = 14)






