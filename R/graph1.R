# first cut at some graphs to explore correlations

ggplot(data = risk_dat, aes(x=state.abbr, y = no.exercise, color = obesity, group=state.abbr))+geom_point()

# plot of percentage of ppl who reported eating few fruit and ppl not exercising
# this is grouped by no exercise
g <- ggplot(data = risk_dat, aes(x=no.exercise, y = few.fruit, 
                                 color = no.exercise, group=state.abbr)) +
  theme_classic() +  geom_point() +
  xlab(label = "No Exercise") +
  ylab(label = "Eating few fruits") + scale_colour_gradient(limits=c(1, 100), low="blue", high="dark red") +
  ggtitle(label = "Percentage of people reporting not exercising\nand eating few fruits
          \nGrouped by percentage not exercising")
g
  
# plot of percentage of ppl who reported eating few fruit and ppl not exercising
# this is grouped by state.abbr
g1 <- ggplot(data = risk_dat, aes(x=no.exercise, y = few.fruit, size= no.exercise, 
                                  color = state.abbr, group=state.abbr)) +
  theme_classic() + 
  geom_point() + 
  scale_color_brewer(type = "seq",palette = "PuRd") + 
  xlab(label = "No Exercise") +
  ylab(label = "Eating few fruits") + 
  ggtitle(label = "Percentage of people reporting not exercising\nand eating few fruits")
g1


# plot of states and obessity
g2 <- ggplot(data = risk_dat, aes(x=obesity, y = state.abbr, group=state.abbr, color=obesity)) + 
  geom_point() + theme_gdocs() + ylab(label = "State") + 
  scale_colour_gradient(limits=c(1, 100), low="blue", high="dark red") +
  ggtitle(label = "Obesity and States")
g2


