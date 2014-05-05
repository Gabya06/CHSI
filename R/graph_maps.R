#-----------------------------------------------------------------------------------
# obesity Maps
#-----------------------------------------------------------------------------------

library(ggmap)
library(maps)


# get state map
us_state_map <- map_data("state")
# change us_state_map names
names(us_state_map)<- c("long","lat","group","order","state.name","subregion")


# merge us_state_map data frame with risk_dat by state.name
# only include matching records
map_data<-merge(us_state_map, risk_dat, by ='state.name')

# preserve order
map_data<-map_data[order(map_data$order),]
# remove subregion column
map_data$subregion<-NULL

# split %'s into 8 cuts 
map_data$obesity_d <- cut_interval(map_data$obesity, 8)

# state data
state_df <- map_data("state")

# create dataframe with county information from maps 
# Longitude and Latitude information here
county_df<-map_data("county")

# change names of county_df
names(county_df)<- c("long","lat","group","order","state.name","county.name")

# check out state.abb and state.name Datasets 
# will add state.abbr to county_df based on match
head(state.abb)
head(state.name)

# add a column with state abbrevitions based on matching between 
# county_df$state_name and lowercase state.name dataset
county_df$state.abbr<- state.abb[match(x = county_df$state.name, tolower(state.name))]
# remove state_name column since have abbreviations
county_df$state.name<-NULL

# make all names lowercase to match
county_df <- data.frame(lapply(county_df, lower.df))

#--------------------------------------------------------------
# will use this to zoom in on % obesity at County level
#--------------------------------------------------------------

# merge county_df and risk_dat by county.name and state.abbr
risk_map <- merge(x = county_df, y = risk_dat, by=c("county.name","state.abbr"))
# retain order
risk_map <- risk_map[order(risk_map$order), ]

# add breaks for ranges in new map risk_map
risk_map$obesity_d <- cut_interval(risk_map$obesity ,8)


#-----------------------------------------------------------------------------------------------
# plot Obesity accross US states - not at county level
#-----------------------------------------------------------------------------------------------
# create dataframe to add state map abbreviations on map
# state.center - x=long, y=lat. state.abb is a list containing state abbreviations
state.info <- data.frame(state.center, state.abb)
# lower names
state.info <- data.frame(lapply(state.info, lower.df))
# add group info
state.info$group <- map_data$group[match(x = state.info$state.abb, map_data$state.abbr)]
# remove ak and hi (no group)
state.info <- state.info[!is.na(state.info$group),]

# map of obesity at state level - org palette
# doesnt include state names
obesity_map_all_org <- ggplot(map_data, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = obesity_d)) +
  geom_polygon(data = state_df, colour = "black", fill = NA, size =0.2) + 
  geom_polygon(data = county_df, colour = "snow", fill = NA, size =0.1) + 
  geom_text(data = state.info, aes(x=x, y=y, label = state.abb, group = group), colour ='black') +
  theme_classic() +  
  theme(legend.position="right") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing Obesity Intervals at the State Level") +
  scale_fill_brewer(type='div', palette = 'RdBu', name = "% Obesity")
obesity_map_all_org

# obesity map to add color to
# includes state abbreviations
obesity_map <- ggplot(map_data, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = obesity_d)) +
  geom_polygon(data = state_df, colour = "black", fill = NA, size =0.2) + 
  geom_polygon(data = county_df, colour = "snow", fill = NA, size =0.1) + 
  geom_text(data = state.info, aes(x=x, y=y, label = state.abb, group = group), colour ='black') +
  theme_classic() +  
  theme(legend.position="right") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing Obesity Intervals at the State Level")
# pink w state names
obesity_map_purd <- obesity_map + 
  scale_fill_brewer(type='seq',palette = 'PuRd', name ="% Obesity") 
  #geom_point(data = map_data[map_data$high.blood>30,], aes(long,lat, color = high.blood))





# map of obesity at state level
obesity_map_all_purd <- ggplot(map_data, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = obesity_d)) + 
  geom_polygon(data = state_df, colour = "black", fill = NA, size =0.2) + 
  geom_polygon(data = county_df, colour = "snow", fill = NA, size =0.1) + 
  theme_classic() +  
  theme(legend.position="right") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank() ) + 
  ggtitle(label = "US Map showing Obesity Intervals \nState Level") +
  scale_fill_brewer(type='seq',palette = 'PuRd', name ="% Obesity")
obesity_map_all_purd



#-----------------------------------------------------------------------------------------------
# obesity at state level > 10%
#-----------------------------------------------------------------------------------------------
# subset data which is at state level
# map_data: doesnt include county coordinates
map_data_10 <- subset(x = map_data, obesity >10.0)
# change intervals
map_data_10$obesity_d <- cut_interval(map_data_10$obesity ,8)
# retain order
map_data_10 <- map_data_10[order(map_data_10$order), ]

# map of obesity >10% - using paired palette
obesity10_map_all_paired <- ggplot(map_data_10, aes(long, lat, group = group)) +
  theme(panel.background = element_rect(fill = "snow")) +
  geom_polygon(aes(fill = obesity_d)) + 
  geom_polygon(data = state_df, colour = "midnightblue", fill = NA,size =0.2) + 
  geom_polygon(data = county_df, colour = "grey", fill = NA,size =0.1) + 
  theme_classic() +  
  theme(legend.position="bottom") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing obesity intervals over 10%\n at State Level") +
  scale_fill_brewer(palette = "Paired", name ="% Obesity")
obesity10_map_all_paired

# map of obesity >10% - use PuRd palette
obesity10_map_all_purd <- ggplot(map_data_10, aes(long, lat, group = group)) +
  theme(panel.background = element_rect(fill = "snow")) +
  geom_polygon(aes(fill = obesity_d)) + 
  geom_polygon(data = state_df, colour = "midnightblue", fill = NA,size =0.2) + 
  geom_polygon(data = county_df, colour = "grey", fill = NA,size =0.1) + 
  theme_classic() +  
  theme(legend.position="bottom") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing obesity intervals over 10%\n at State Level") +
  scale_fill_brewer(palette = "PuRd", name ="% Obesity")
obesity10_map_all_purd

#-----------------------------------------------------------------------------------------------
# obesity at state level > 20%
#-----------------------------------------------------------------------------------------------
# subset data which is at state level
# map_data: doesnt include county coordinates
map_data_20 <- subset(x = map_data, obesity >20.0)
# change intervals
map_data_20$obesity_d <- cut_interval(map_data_20$obesity ,8)
# retain order
map_data_20 <- map_data_20[order(map_data_20$order), ]

# map of obesity >20% - using paired palette
obesity20_map_all_paired <- ggplot(map_data_20, aes(long, lat, group = group)) +
  theme(panel.background = element_rect(fill = "snow")) +
  geom_polygon(aes(fill = obesity_d)) + 
  geom_polygon(data = state_df, colour = "midnightblue", fill = NA,size =0.2) + 
  geom_polygon(data = county_df, colour = "grey", fill = NA,size =0.1) + 
  theme_classic() +  
  theme(legend.position="bottom") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing obesity intervals over 20%\n at State Level") +
  scale_fill_brewer(type = 'qualitative', palette = "Paired", name ="% Obesity")
obesity20_map_all_paired

# map of obesity >20% - using Set2 palette
obesity20_map_all_set2 <- ggplot(map_data_20, aes(long, lat, group = group)) +
  theme(panel.background = element_rect(fill = "snow")) +
  geom_polygon(aes(fill = obesity_d)) + 
  geom_polygon(data = state_df, colour = "midnightblue", fill = NA,size =0.2) + 
  geom_polygon(data = county_df, colour = "grey", fill = NA,size =0.1) + 
  theme_classic() +  
  theme(legend.position="bottom") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing obesity intervals over 20%\n at State Level") +
  scale_fill_brewer(type = 'qual', palette = "Set2", name ="% Obesity")
obesity20_map_all_set2



#-------------------------------------------------------------------------------------------------------------
# plot Obesity accross US - using County information
#-------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# plot Obesity accross US states - At County level
#-----------------------------------------------------------------------------------------------

# map showing obesity percentages by county

# map showing obesity at county level - using PuRd Palette
obesity_Cmap_purd <- ggplot(risk_map, aes(long, lat, group = group)) +
  theme(panel.background = element_rect(fill = "snow")) +
  geom_polygon(aes(fill = obesity_d), colour = alpha("snow", 1/2), size = 0.2) + 
  geom_polygon(data = state_df, colour = "black", fill = NA,size =0.2) + 
  geom_polygon(data = county_df, colour = "grey", fill = NA, size =0.1) + 
  theme_classic() +  
  theme(legend.position="bottom") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing obesity \nIncludes County information") +
  scale_fill_brewer(type='seq',palette = 'PuRd', name ="% Obesity") 
obesity_Cmap_purd 

# map showing obesity at county level - using Paired Palette
obesity_Cmap_paired <- ggplot(risk_map, aes(long, lat, group = group)) +
  theme(panel.background = element_rect(fill = "snow")) +
  geom_polygon(aes(fill = obesity_d), colour = alpha("snow", 1/2), size = 0.2) + 
  geom_polygon(data = state_df, colour = "black", fill = NA,size =0.2) + 
  geom_polygon(data = county_df, colour = "grey", fill = NA, size =0.1) + 
  theme_classic() +  
  theme(legend.position="bottom") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing obesity \nIncludes County information") +
  scale_fill_brewer(type='div',palette = 'Paired', name ="% Obesity") 
obesity_Cmap_paired
#-----------------------------------------------------------------------------------------------
# obesity at county level > 10%
# no fill = no data at county level
#-----------------------------------------------------------------------------------------------

# subset on risk_map which already has mapping data
risk_map_10 <- subset(x = risk_map, obesity >10.0)
# change intervals 
risk_map_10$obesity_d <- cut_interval(risk_map_10$obesity ,8)
# retain order
risk_map_10 <- risk_map_10[order(risk_map_10$order), ]

# map of obesity > 10% - using yellow palette
# No fill = no data
obesity10_Cmap_org <- ggplot(risk_map_10, aes(long, lat, group = group)) +
  theme(panel.background = element_rect(fill = "snow")) +
  geom_polygon(aes(fill = obesity_d)) + 
  geom_polygon(data = state_df, colour = "midnightblue", fill = NA,size =0.2) + 
  geom_polygon(data = county_df, colour = "grey", fill = NA,size =0.1) + 
  theme_classic() +  
  theme(legend.position="bottom") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing obesity intervals over 10%\n at County Level") +
  scale_fill_brewer(palette = "YlOrRd", name ="% Obesity")
obesity10_Cmap_org

# map of obesity > 10% - using PuRd palette
obesity10_Cmap_purd <- ggplot(risk_map_10, aes(long, lat, group = group)) +
  theme(panel.background = element_rect(fill = "snow")) +
  geom_polygon(aes(fill = obesity_d)) + 
  geom_polygon(data = state_df, colour = "midnightblue", fill = NA,size =0.2) + 
  geom_polygon(data = county_df, colour = "grey", fill = NA,size =0.1) + 
  theme_classic(base_size = 11, base_family = "Verdana") + 
  theme(legend.position="bottom") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing obesity intervals over 10%\n at County Level") +
  scale_fill_brewer(type = 'seq',palette = "PuRd", name ="% Obesity")
obesity10_Cmap_purd

# map of obesity > 10% - using Paired palette
obesity10_Cmap_paired <- ggplot(risk_map_10, aes(long, lat, group = group)) +
  theme(panel.background = element_rect(fill = "snow")) +
  geom_polygon(aes(fill = obesity_d)) + 
  geom_polygon(data = state_df, colour = "midnightblue", fill = NA,size =0.2) + 
  geom_polygon(data = county_df, colour = "grey", fill = NA,size =0.1) + 
  theme_classic(base_size = 11, base_family = "Verdana") + 
  theme(legend.position="bottom") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing obesity intervals over 10%\n at County Level") +
  scale_fill_brewer(type = 'div',palette = "Paired", name ="% Obesity")
obesity10_Cmap_paired


#-----------------------------------------------------------------------------------------------
# obesity at county level > 20%
# no fill = no data at county level
#-----------------------------------------------------------------------------------------------

# subset on risk_map which already has mapping data
risk_map_20 <- subset(x = risk_map, obesity >20.0)
# change intervals 
risk_map_20$obesity_d <- cut_interval(risk_map_20$obesity ,8)
# retain order
risk_map_20 <- risk_map_20[order(risk_map_20$order), ]

# map of obesity > 20% - using yellow palette
# No fill = no data
obesity20_Cmap_org <- ggplot(risk_map_20, aes(long, lat, group = group)) +
  theme(panel.background = element_rect(fill = "snow")) +
  geom_polygon(aes(fill = obesity_d)) + 
  geom_polygon(data = state_df, colour = "midnightblue", fill = NA,size =0.2) + 
  geom_polygon(data = county_df, colour = "grey", fill = NA,size =0.1) + 
  theme_classic() +  
  theme(legend.position="bottom") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing obesity intervals over 20%\n at County Level") +
  scale_fill_brewer(palette = "YlOrRd", name ="% Obesity")
obesity20_Cmap_org

# map of obesity > 20% - using PuRd palette
obesity20_Cmap_purd <- ggplot(risk_map_20, aes(long, lat, group = group)) +
  theme(panel.background = element_rect(fill = "snow")) +
  geom_polygon(aes(fill = obesity_d)) + 
  geom_polygon(data = state_df, colour = "midnightblue", fill = NA,size =0.2) + 
  geom_polygon(data = county_df, colour = "grey", fill = NA,size =0.1) + 
  theme_classic(base_size = 11, base_family = "Verdana") + 
  theme(legend.position="bottom") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing obesity intervals over 20%\n at County Level") +
  scale_fill_brewer(type = 'seq',palette = "PuRd", name ="% Obesity")
obesity20_Cmap_purd

# map of obesity > 20% - using Paired palette
obesity20_Cmap_paired <- ggplot(risk_map_20, aes(long, lat, group = group)) +
  theme(panel.background = element_rect(fill = "snow")) +
  geom_polygon(aes(fill = obesity_d)) + 
  geom_polygon(data = state_df, colour = "midnightblue", fill = NA,size =0.2) + 
  geom_polygon(data = county_df, colour = "grey", fill = NA,size =0.1) + 
  theme_classic(base_size = 11, base_family = "Verdana") + 
  theme(legend.position="bottom") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing obesity intervals over 20%\n at County Level") +
  scale_fill_brewer(type = 'div',palette = "Paired", name ="% Obesity")
obesity20_Cmap_paired