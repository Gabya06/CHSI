

#---------------------- 1st cut at some graphs to rough explore correlations ----------------------
# 1) States & no.exercise 2)no.exercise\few.fruits 3)obesity by state
# -------------------------------------------------------------------------------------------------

# plot obesity states where people reported no exercise 
plt1 <- ggplot(data = risk_dat, aes(x=state.abbr, y = no.exercise, color = no.exercise, group=state.abbr)) + 
  geom_point() +
  scale_color_continuous(name = "% No Exercise") +
  theme_classic() +
  ggtitle(label = "States and No exercise")
plt1

# plot of percentage of ppl who reported eating few fruit and ppl not exercising
# this is grouped by no exercise
g <- ggplot(data = risk_dat, aes(x=no.exercise, y = few.fruit, 
                                 color = no.exercise, group=state.abbr)) + 
  theme_classic() +  
  geom_point() + 
  xlab(label = "No Exercise") +
  ylab(label = "Eating few fruits") + 
  scale_colour_gradient(limits=c(1, 100), low="blue", high="dark red") +
  ggtitle(label = "Percentage of people reporting not exercising\nand eating few fruits
          \nGrouped by percentage not exercising")
g
  
# plot of percentage of ppl who reported eating few fruit and ppl not exercising
# this is grouped by state.abbr
g1 <- ggplot(data = risk_dat, aes(x=no.exercise, 
                                  y = few.fruit, 
                                  size= no.exercise, 
                                  color = state.abbr, 
                                  group=state.abbr)) +
  theme_classic() + 
  geom_point() + 
  scale_color_brewer(type = "seq",palette = "PuRd") + 
  xlab(label = "No Exercise") +
  ylab(label = "Eating few fruits") + 
  ggtitle(label = "Percentage of people reporting not exercising\nand eating few fruits")
g1


# plot of states and obessity
g2 <- ggplot(data = risk_dat, aes(x=obesity, y = state.abbr, group=state.abbr, color=obesity)) + 
  geom_point() + theme_gdocs() + 
  ylab(label = "State") + 
  scale_colour_gradient(limits=c(1, 100), low="blue", high="dark red") +
  ggtitle(label = "Obesity and States")
g2


# ---------------------------------------------------------------------------------------
# some Histograms:
# 1) obesity 2) few.fruits
# ---------------------------------------------------------------------------------------

# plot histogram of obesity percentage counts - round to factor by obesity percentages
g3 <- ggplot(data = risk_dat, aes(x=obesity)) + 
  geom_histogram(binwidth = 3, aes(color = factor(signif(obesity,0)))) + 
  theme_gdocs() + 
  ggtitle(label = "Count of Obesity %'s") +
  scale_color_discrete(name ="Obesity %s \nRounded")
g3

# show histogram of obesity %s counts
hist_obesity <- ggplot(data = risk_dat, aes(x=obesity)) + 
  geom_histogram(binwidth = 3, aes(fill = factor(signif(obesity,0)))) +
  theme_economist(base_size = 11,base_family = "Verdana",dkpanel = T) +  
  xlab(label = "Obesity Percentages") +
  ggtitle(label = "Count of Obesity Percentages") + 
  scale_fill_discrete(name ="Obesity %s Rounded")
hist_obesity


# show histogram of few fruit %s counts
g5 <- ggplot(data = risk_dat) + 
  geom_histogram(binwidth = 5, aes(x = few.fruit, fill = factor(signif(few.fruit,0)))) +
  theme_wsj() + 
  xlab(label = "Few Fruit Percentages") +
  ggtitle(label = "Count of People who reported \n eating less than 5 fruits per day \n in Percentages") + 
  scale_fill_discrete(name ="few fruits %s Rounded")
g5


# ---------------------------------------------------------------------------------------
# some scatter plots:
# 1) blood pressure\obesity 2) blood pressure\diabetes
# ---------------------------------------------------------------------------------------

# high blood pressure and obesity scatter
g8 <- ggplot(data = risk_dat, aes(x = high.blood, y = obesity, 
                                  color = factor(signif(obesity, 0)))) + 
  geom_point() + 
  theme_classic() + 
  scale_color_discrete(name="% of obesity") +
  xlab(label = "High Blood Pressure %'s") +
  ggtitle(label = "Percentage of obesity vs.\n Percentage of high blood pressure 
          colored by obesity %'s") 
g8

# high blood pressure and diabetes scatter
g9 <- ggplot(data = risk_dat, aes(x = high.blood, y = diabetes, 
                                  color = factor(round(diabetes)))) + 
  geom_point() + 
  theme_classic() + 
  scale_color_discrete(name="% of diabtes") +
  ggtitle(label = "Percentage of high blood pressure vs.\n Percentage of diabetes") 
g9

# ---------------------------------------------------------------------------------------
# some bar graphs:
# 1) diabetes counts  2)obesity counts
# ---------------------------------------------------------------------------------------

# Are there many people with high percentages of diabetes?
# bar plot of the number of ppl with certain percentages of diabetes - 
# bulk of ppl are [5-10]% range
g10 <- ggplot(data = risk_dat, aes(x = diabetes, fill = factor(round(diabetes)))) +
  geom_bar() 
g10


# Are there many people with high percentages reporting obesity?
# bar plot of the number of ppl with certain percentages of obesity - bulk of ppl are [20-30]% range
g11 <- ggplot(data = risk_dat, aes(x = obesity, fill = factor(round(obesity)))) +
  geom_bar() 
g11

# ---------------------------------------------------------------------------------------
# some nonsense plots
qplot(x = few.fruit, y = obesity,data = risk_dat, geom = 'line', color = factor(signif(obesity,0)) )
qplot(x = few.fruit, y = obesity,data = risk_dat, geom = 'line', color = factor(signif(few.fruit,0)) )


