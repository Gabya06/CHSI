#----------------------------------- Basic Map ------------------------------------------------------------
# install ggmap
#install.packages("ggmap")
library(ggmap)
library(maps)



us <- get_map(location = 'united states', zoom = 4, maptype = 'terrain', 
              source = 'google', messaging = FALSE, urlonly = FALSE)

# get state map
us_state_map <- map_data("state")
# change us_state_map names
names(us_state_map)<- c("long","lat","group","order","state.name","subregion")

# change name from state to region
# names(large.obesity) <- c("county","state_name","state","no.exercise","few.fruit","obesity",
#                           "high.blood","diabetes","no.ins")

# replace all states to lower case
large.obesity20 <- data.frame(lapply(large.obesity, 
                                   function(v) 
                                   {
                                     if(is.character(v)) return(tolower(v)) 
                                     else return(v)
                                   }))

# merge us_state_map data frame with large.obesity by region
# only include matching records
map_data<-merge(us_state_map, large.obesity, by ='state_name')

# preserve order
map_data<-map_data[order(map_data$order),]

# sort by obesity
#risk_dat[order(rank(risk_dat$obesity, ties.method = "first"), decreasing = T)[1:5],]

# basic plot 
map_obesity <- (qplot(x = long,y = lat,data = map_data, geom="polygon", group = group, fill = obesity)  + 
                  theme_minimal()  + 
                  labs(x="",y="") + #, fill ="") + 
                  scale_fill_gradient(low='lightblue', high='darkblue') + 
                  ggtitle(label = "US Map showing obesity over 20%")) 
map_obesity

#----------------------------------- chloropleth Maps obesity>20%------------------------------------------------------------
# using County information
#-------------------------------------------------------------------------------------------------------------

# create dataframe with county information from maps
county_df<-map_data("county")
# change names of county_df
names(county_df)<- c("long","lat","group","order","state_name","county")

# check out state.abb and state.name datasets 
# will add state.abbr to county_df based on match
head(state.abb)
head(state.name)

# add a column with state abbrevitions based on matching between 
# county_df$state_name and state.name dataset
county_df$state<- state.abb[match(x = county_df$state_name,tolower(state.name))]
# remove state_name column
county_df$state_name<-NULL

# make all names lowercase to match
county_df <- data.frame(lapply(county_df, 
                               function(v)
                               {
                                 if(is.character(v)) return(tolower(v)) 
                                 else return(v)
                               }))

# merge county_df and large.obesity by county and state abbreviation
choropleth <- merge(x = county_df, y = large.obesity, by=c("county","state"))
# retain order
choropleth <- choropleth[order(choropleth$order), ]

# state data
state_df <- map_data("state")

# add breaks for ranges in new map
choropleth$obesity_d <- cut(choropleth$obesity, breaks = c(seq(20, 43, by = 2), 8))

# plot Obesity >20% with county information
obesity_map1 <- ggplot(choropleth, aes(long, lat, group = group)) +
  theme(panel.background = element_rect(fill = "snow")) +
  geom_polygon(aes(fill = obesity)) + #, colour = alpha("snow", 1/2), size = 0.2) + 
  geom_polygon(data = state_df, colour = "black", fill = NA,size =0.2) + 
  geom_polygon(data = county_df, colour = alpha("grey", 1/2), fill = NA, size =0.1) + 
  theme_classic() +  
  theme(legend.position="bottom") + 
  xlab(label = "") +
  ylab(label = "") +
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing obesity over 20% \nIncludes County information") +
  scale_fill_continuous(low='pink', high='red', na.value = "white")
obesity_map1  

# map shows obesity intervals
# make obesity percentages discrete to be able use with Brewer colour scheme
# remove labels and tickmarks using theme(axis.tick and axis.text.x = element_blank())
choropleth$obesity_d <- cut_number(choropleth$obesity, 6)  
obesity_map_blgrn <- ggplot(choropleth, aes(long, lat, group = group)) +
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
  ggtitle(label = "US Map showing obesity intervals over 20%\n At County Level \nusing ColorBrewer") +
  scale_fill_brewer(palette = "GnBu", name ="% Obesity")
obesity_map_blgrn

# map at county level
obesity_map_purd <- ggplot(choropleth, aes(long, lat, group = group)) +
  theme(panel.background = element_rect(fill = "snow")) +
  geom_polygon(aes(fill = obesity_d)) + 
  geom_polygon(data = state_df, colour = "black", fill = NA,size =0.2) + 
  geom_polygon(data = county_df, colour = "grey", fill = NA,size =0.1) + 
  theme_classic() +  
  theme(legend.position="bottom") + 
  labs(x = NULL, y=NULL) + 
  scale_x_continuous(expand = c(0,0)) + # expand size of map along x axis 
  scale_y_continuous(expand = c(0,0)) + # expand size of map along y axis
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + 
  ggtitle(label = "US Map showing obesity ranges over 20%\n at County Level") +
  scale_fill_brewer(palette = "PuRd", name ="% Obesity")
obesity_map_purd


#-----------------------------------------------------------------------------------
# obesity >10%
#-----------------------------------------------------------------------------------

large.obesity10 <- subset(x = risk_dat, obesity>10.0)
names(large.obesity10) <- c("county","state_name","state","no.exercise","few.fruit","obesity",
                            "high.blood","diabetes","no.ins")

# replace all states to lower case
large.obesity10 <- data.frame(lapply(large.obesity10, 
                                     function(v) 
                                     {
                                       if(is.character(v)) return(tolower(v)) 
                                       else return(v)
                                     }))


# merge county_df and large.obesity by county and state abbreviation
choropleth_10 <- merge(x = county_df, y = large.obesity10, by=c("county","state"))
# retain order
choropleth_10 <- choropleth_10[order(choropleth_10$order), ]
# make ranges discrete
choropleth_10$obesity_d <- cut_number(choropleth_10$obesity, 6)  

# plot map of ppl obese >10
obesity_map_org_10 <- ggplot(choropleth_10, aes(long, lat, group = group)) +
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
  ggtitle(label = "US Map showing obesity intervals over 10%\n at County Level \nusing ColorBrewer") +
  scale_fill_brewer(palette = "OrRd", name ="% Obesity")
obesity_map_org_10

