#---------------------- 2nd cut at some graphs to explore correlations -------------------------------

# # --------------------Exploration of Obesity Correlations in Risk data ----------------------------
# Obesity: defined as The Calculate Percentage of adults at Risk for health problems
# related to being overweight based on BMI. BMI => 30 is considered Obese
# --------------------------------------------------------------------------------------------------

# explore correlations between obesity & no exercise --> highest
cor(x = risk_dat$obesity, y = risk_dat$no.exercise)

# explore correlations between obesity & few fruits
cor(x = risk_dat$obesity, y = risk_dat$few.fruit)

# explore correlations between obesity & high blood pressure
cor(x = risk_dat$obesity, y = risk_dat$high.blood)

# explore correlations between obesity & diabetes
cor(x = risk_dat$obesity, y = risk_dat$diabetes)

# explore correlations between obesity & no insurance 
cor(x = risk_dat$obesity, y = risk_dat$no.ins)

# check out the different correlations 
cor(risk_dat[, c(4:8)])

# use the ggpairs plotting function from the ggally package and plot the relationships
GGally::ggpairs(risk_dat[,c(4:8)], params = list(labelSize = 4), title = 'Correlations',
                axisLabels = "show",   upper = list(continuous = "density"), #combo = "denstrip"),
                lower = list(continuous = "points")) #, combo = "denstrip") )

      


# risk_dat correlation matrix - matrix of correlations between different features
risk_datCor <- cor(risk_dat[, c(4:8)])

# melt risk_dat correlation matrix into long format
risk_melt <- melt(data = risk_datCor, varnames = c("x","y"), 
                  value.name = "Correlation")
# order risk_melt by Correlation
risk_melt <- risk_melt[order(risk_melt$Correlation),]

# view risk_melt --> looks like correlation = 1 shouldnt be counted in the mean
mean(risk_melt$Correlation)
summary(risk_melt)
# remove correlations = 1 --> not useful
risk_melt <- risk_melt[(!risk_melt$Correlation==1),]
# compute mean
risk_meanCor <- mean(risk_melt$Correlation)



# # ----------------------HeatMaps of Obesity Correlations in Risk data ----------------------------
# display palettes 
#RColorBrewer::display.brewer.all()
# look for greens 
grep(pattern = "green",x = colors(),value = T)


# Heat map - using blues
plt_heat_blue <- ggplot(data = risk_melt, aes(x=x, y = y)) +
  theme(panel.background = element_rect(fill = "snow2")) +
  geom_tile(aes(fill = Correlation)) + 
  scale_fill_gradient2(low = "lightcyan", mid = "lightblue", high="midnightblue", 
                       midpoint = risk_meanCor,
                       limits = c(0, 0.61), name = "Correlations") +
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0)) +
  labs(x=NULL, y=NULL) + 
  theme(panel.background = element_rect(fill = "snow2")) +
  ggtitle("Heat map of correlations in Risk Factors data") 
plt_heat_blue 


# Heat Map - using greens & yellow
plt_heat_green <- ggplot(data = risk_melt,aes(x=x, y = y)) +
  geom_tile(aes(fill = Correlation)) + 
  scale_fill_gradient2(low = "yellowgreen", high="darkgreen", mid="lightgreen",
                       na.value = "snow2",
                       midpoint = risk_meanCor, 
                       limits = c(0, 0.61), name = "Correlations") +
  theme_economist(base_family = "Verdana", base_size = 11) + 
  labs(x = NULL, y=NULL) + 
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0)) +
  ggtitle("Heat map of correlations in Risk Factors data") 
plt_heat_green

# same Heat Map using red & green & blue
plt_heat_yellow <- ggplot(data = risk_melt,aes(x=x, y = y)) +
  geom_tile(aes(fill = Correlation)) + 
  scale_fill_gradient2(low = "yellow", mid = "orange", high = "red",
                       midpoint = risk_meanCor,
                       guide = "colorbar", limits = c(0, 0.61) ) +
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0)) +
  theme_tufte(base_family = "Verdana", base_size = 11) + 
  theme(panel.background = element_rect(fill = "snow2")) +
  labs(x = NULL, y=NULL) + 
  ggtitle("Heat map of correlations in Risk Factors data") 
plt_heat_yellow

# plot3 - n colors
# ggplot(data = risk_melt,aes(x=x, y = y)) +
#   geom_tile(aes(fill = Correlation)) + 
#   scale_fill_gradientn(colours = c("darkblue", "blue", "lightblue", "white","lightgreen", "green","darkgreen"),
#                        values = rescale(c(-3,-2-1,0,1,2,3)), 
#                        guide = "colorbar") +
#   theme_minimal() +
#   labs(x = NULL, y=NULL) +
#   ggtitle(label = "Correlation heat map")
# 

# highest correlations in risk data => obesity & no exercise AND obesity & few fruits
#-------------------------------------------------------------------------------------
# scatter plot of obesity & few fruits
#-------------------------------------------------------------------------------------

g6 <- ggplot(data = risk_dat, aes(x = obesity, y = few.fruit, 
                                  color = factor(signif(obesity, 0)))) + 
  geom_point() + 
  theme_classic() + 
  scale_color_discrete(name="Percentage of obesity") +
  ggtitle(label = "Percentage of obesity vs.\n Percentage of few fruits") 
g6
#summary(g6)

# few fruit on x axis and obesity on y axis
g7 <- ggplot(data = risk_dat, aes(x = few.fruit, y = obesity, color = factor(signif(obesity, 0)))) + 
  geom_point() + 
  theme_classic() + scale_color_discrete(name="Percentage of obesity") +
  ggtitle(label = "Percentage of obesity vs.\n Percentage of few fruits") 
g7
# should subset data for few.fruits: 75% of ppl who reported few fruit are obese 

#-------------------------------------------------------------------------------------
# scatter plot of obesity & no exercise
#-------------------------------------------------------------------------------------

plt_obNoEx_1 <- ggplot(data = risk_dat, aes(x = obesity, 
                                             y = no.exercise, 
                                             color = factor(signif(obesity, 0)))) + 
  geom_point() + 
  theme_classic() + 
  scale_color_discrete(name="% Obesity") +
  ggtitle(label = "Percentage of obesity vs.\n Percentage of No Exercise") 
plt_obNoEx_1



# no exercise on x axis and obesity on y axis
plt_obNoEx_2 <- ggplot(data = risk_dat, aes(x = (no.exercise/100), y = (obesity/100), 
                                             color = factor(signif(obesity, 0)))) + 
  geom_point() +  
  scale_color_discrete(name="% Obesity") +
  scale_x_continuous(labels = percent, name = 'No Exercise %s') +
  scale_y_continuous(labels = percent, name = 'Obesity %s') +
  theme_classic(base_size = 12, base_family = 'Verdana') +
  ggtitle(label = "Percentage of obesity vs.\n Percentage of No Exercise") 
plt_obNoEx_2

#-----------------------------------------------------------------------------------------------
# subset on states with obesity percentage > 20 %
# plot heat map of the correlation between obesity and other features
#-----------------------------------------------------------------------------------------------
large.obesity20 <- subset(x = risk_dat, obesity>20.0)
# dataframe with correlations for obesity >20
large.obesity20_Cor <- cor(large.obesity20[,c(4:8)])

# melt risk_dat correlation matrix into long format
lObesity20_melt <- melt(data = large.obesity20_Cor, varnames = c("x","y"), 
                      value.name = "Correlation")
# order risk_melt by Correlation
lObesity20_melt <- lObesity20_melt[order(lObesity20_melt$Correlation),]

# remove correlations = 1 --> not useful
lObesity20_melt <- lObesity20_melt[(lObesity20_melt$Correlation<1),]
# compute mean
lObesity20_meanCor <- mean(lObesity20_melt$Correlation)

# heat map of correlations --> shows larger correlations for obesity & diabetes
plt_obesity20_heat <- ggplot(data = lObesity20_melt,aes(x=x, y = y)) +
  geom_tile(aes(fill = Correlation)) + 
  #theme_economist(base_size = 11, base_family = 'Verdana',dkpanel = T)+
  theme(panel.background = element_rect(fill = "snow2")) +
  scale_fill_gradient2(low = "yellow", high="darkolivegreen", mid="seagreen",
                       na.value = "snow2", guide='colorbar',
                       midpoint = lObesity20_meanCor, 
                       limits = c(0,0.6), name = "Correlations") +
  labs(x = NULL, y=NULL) + 
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0)) +
  ggtitle("Heat map of correlations in states \nwith Obesity larger than 20%") 
plt_obesity20_heat