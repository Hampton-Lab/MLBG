library(tidyverse)
library(ggplot2)
library(AICcmodavg)
library(gridExtra)

#Checking data for Normality####
set.seed(1015)
qqnorm(df.cl$SAMPLE_DO)
qqnorm(df.cl$BEAKER_DO)


#Creating summary table for plotting####
df.cl.sum <- df.cl %>%
  group_by(TREAT, CYCLE, DATE_TIME) %>%
  summarise(avg_SAMDO = mean(SAMPLE_DO),
            avg_BEADO = mean(BEAKER_DO),
            avg_TEMP = mean(TEMP),
            sdev_SAMDO = sd(SAMPLE_DO),
            sdev_BEADO = sd(BEAKER_DO))

#plotting all experimental vs control SAMPLE_DO. Sediment samples are plotted in brown. 
#still need to figure out how to get it to actually show a legend
DOPLOT<- ggplot(df.cl.sum, aes(group = TREAT))+
  geom_smooth(aes(x = DATE_TIME, y = avg_BEADO), colour = "blue", linetype = "dashed")+
  geom_smooth(aes(x = DATE_TIME, y = avg_SAMDO), colour = "brown")+
  xlab("Date and Time")+ylab("Average Dissolved O2 (mg/L)")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  theme(legend.position = "right")+
  facet_wrap(~TREAT)

#Save the plot
ggsave(file=file.path("Plots/20250922_MLBG_DO-OVER-TIME-BY-TREATMENT-GRAPH.PNG"), plot =DOPLOT, width = 10,
       height = 10, units = "in", dpi = 1300)

#So plotting we can see that, across the 4 days the experiment lasted, average 
#DO values peaked on the second day. 
#Question 1####
#The first question is whether the lysimeter measurements actually differed 
#from those at the surface.

#T-test comparing Sample (Lysometer) to Beaker collected DO
t.test(df.cl$SAMPLE_DO, df.cl$BEAKER_DO)

#Based on the T-test, it does seem that sediment and beaker DO measurements 
#differed significantly, as expected.
#Question 2####
#Now onto our second question, is there a difference in DO when the sediments
#are incubated at higher temperatures?

#Assumed normal data. Based on the qq plots it seems pretty normal
#T-test for the Sediment Data
t.test(df.cl$SAMPLE_DO~df.cl$TREAT)  
#T-test for the beakers
t.test(df.cl$BEAKER_DO~df.cl$TREAT)

#Based on this, it does seem that the treatment had a significant effect on 
#the dissolved oxygen present in the sediment and beaker. What is a little bit
#interesting is that more oxygen was present in the experimental, which was 
#warmer. This is unexpected given that O2 decreases as temperatures increase.
#Its possible that these increases in DO are due to other biological activity,
#such as photosynthesis. Lets check how DO relates to other metrics.

#Model Comparison for question 2 ####
#Anova for DO and Day/Night Cycle
aov_TS <- aov(df.cl$SAMPLE_DO~df.cl$TREAT)
aov_TSC <- aov(df.cl$SAMPLE_DO~df.cl$TREAT+df.cl$CYCLE)
aov_TSCi <- aov(df.cl$SAMPLE_DO~df.cl$TREAT*df.cl$CYCLE)

summary(aov_TS)
summary(aov_TSC)
summary(aov_TSCi)

#Checking which model fits best using the akaike information criterion
#Defining our aovs as the model
model_set <- list(aov_TS, aov_TSC, aov_TSCi)
model_names <- c("aov_TS", "aov_TSC", "aov_TSCi")

#running the aic
aictab(model_set, modnames = model_names)

#plotting AOV comparisons
plot1 <- ggplot(df.cl, aes(x = factor(TREAT), y = SAMPLE_DO, fill = factor(TREAT))) +
  geom_boxplot(color = "black", alpha = 0.7) +
  labs(title = "One-Way ANOVA", x = "Treatment", y = "Dissolved O2") +
  theme_minimal() +
  theme(legend.position = "top")

plot2 <- ggplot(df.cl, aes(x = factor(TREAT), y = SAMPLE_DO, fill = factor(CYCLE))) +
  geom_boxplot(color = "black", alpha = 0.7) +
  labs(title = "Two-Way ANOVA", x = "Treatment", y = "Dissolved O2") +
  theme_minimal() +
  theme(legend.position = "top")

AOV_PLOT <- grid.arrange(plot1, plot2, ncol = 2)

#Save the plot
ggsave(file=file.path("Plots/20250922_MLBG_AOV_FOR_DO.PNG"), plot =AOV_PLOT, width = 10,
       height = 10, units = "in", dpi = 1300)
#Based on the output of this, it seems that the model which accounts for 
#temperature and the day/night cycle is the best model to 
#predict dissolved oxygen in the sediment. Given that temperature was pretty
#consistent throughout, this may have something to do with photosynthetic 
#activity or other chemical processes mediated by light.

