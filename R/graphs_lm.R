

# # ----------------------Load demographics Data and clean--------------------------------------------------- 
# only looking at 2 columns of the demographics data, using to attach obesity rates and look at pop.size
# #
# Load demo data and clean
demoData <- read.csv('~/dev/Rstudio/data/DEMOGRAPHICS.csv')

demoData <- demoData[][-(1:2)]
demoData <- demoData[][-(4)]


# subset only certain columns
demo_dat <- subset(demoData, select = c(CHSI_County_Name:CHSI_State_Abbr, 
                                        Population_Size, Population_Density, Poverty))

# use lower letters across all dataframes - easier for merging data
demo_dat <- data.frame(lapply(demo_dat, lower.df))

nms_demo <- c('county.name','state.name','state.abbr','pop.size','pop.density','poverty')
names(demo_dat) <- nms_demo



# # ----------------------2nd version of riskdata--------------------------------------------------- 
# 2nd version to only look at obesity and no exercise
risk_dat_v2 <- subset(riskData, select = c(CHSI_County_Name:CHSI_State_Abbr, 
                                         No_Exercise, Obesity))

names(risk_dat_v2)<- c('county.name','state.name','state.abbr','no.exercise','obesity')
risk_dat_v2 <- data.frame(lapply(risk_dat_v2, lower.df))
# subset data for values >0 to exclude the -1111 and -2222 values
risk_dat_v2 <- with(risk_dat_v2, subset(risk_dat_v2, (no.exercise>0) & 
                                      (obesity>0)))

# combine demographics data
risk_demo <- merge(x=risk_dat_v2, y=demo_dat, by = c('county.name','state.name'))
# plot data
ggplot(data = risk_demo, aes(x= (obesity/100), y = pop.size, color = state.name )) + 
  geom_point() + 
  geom_smooth(method = 'lm', colour ='black') + 
  facet_wrap(~state.name) + 
  scale_y_log10(label = comma, name ='Population size') + 
  scale_x_continuous(labels = percent) + 
  scale_color_discrete() + 
  theme(legend.position = 'none')

# check outliers
g_outliers<- ggplot(data = risk_demo, aes(x= pop.size, y = obesity , color = state.name)) + 
  geom_point() + scale_color_discrete()  + 
  theme(legend.position = 'none') + 
  scale_x_continuous(label = comma)
g_outliers

# remove anything over 2.5mil in pop.size
risk_demo_v2 <- risk_demo[risk_demo$pop.size<2500000, ]

# check if the data has any more outliers
g_outliers2 <- ggplot(data = risk_demo_v2, aes(x= log(pop.size), y = obesity , color = state.name)) + 
  geom_point() + 
  scale_color_discrete()  + 
  theme(legend.position = 'none') + 
  scale_x_continuous(label = comma)
g_outliers2



# plot state facets with linear 
ggplot(data = risk_demo_v2, aes(x= (obesity/100), y = pop.size, color = state.name )) + 
  geom_point() + 
  geom_smooth(method = 'lm', colour ='black') + 
  facet_wrap(~state.name)+ 
  scale_y_log10(label = comma, name ='Population size') + 
  scale_x_continuous(labels = percent) + 
  scale_color_discrete() + 
  theme(legend.position = 'none')



#linear model
risk1 <- lm(obesity~pop.size + state.name,data = risk_demo_v2)
coefplot(risk1)
# plot linear model residuals and fitted values
h1<- ggplot(aes(x=.fitted, y=.resid), data = risk1) + 
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(x='Fitted Vlaues', y = 'Residuals')
h1
h1 + geom_point(aes(color = state.name))
plot(risk1, which =1, col=as.numeric(factor(risk1$model$state.name)))
legend("bottomright", legend = levels(factor(risk1$model$state.name)), pch=1,
       col= as.numeric(factor(levels(factor(risk1$model$state.name)))),
       text.col = as.numeric(factor(levels(factor(risk1$model$state.name)))),
       title = 'State')

# look at QQ plot
ggplot(risk1, aes(sample = .stdresid)) + stat_qq() + geom_abline()
# histogram of residuals
ggplot(risk1, aes(x = .stdresid)) + geom_histogram()


# not too meangingful
risk2 <- lm(obesity~pop.size * state.name, data = risk_demo)
coefplot(risk2)
multiplot(risk1, risk2)


# another model
plt_obNoEx_2 <- ggplot(data = risk_dat, aes(x = no.exercise, y = obesity, 
                                            color = factor(signif(obesity, 0)))) + 
  geom_point() + geom_smooth(method = 'lm', stat = 'identity')+
  theme_classic() + 
  scale_color_discrete(name="Percentage of obesity") +
  ggtitle(label = "Percentage of obesity vs.\n Percentage of No Exercise") 
plt_obNoEx_2
