setwd("/Users/mauriciomartins/Dropbox/Slack_Hierarchies/Elena/DATA/discrimination/")
#setwd("/Users/lenys/Dropbox/Elena/DATA/generalization")
#rm(list=ls())


#Mauricio windows path
#setwd("C:\\Users\\Mauricio\\Dropbox\\Slack_Hierarchies\\Elena\\data\\discrimination")

dat<-read.csv("acc_rt_t2.csv", header=T)
library (lme4)
library(glmulti)
library(LMERConvenienceFunctions)
library(lmerTest)

library(dplyr)
select <- dplyr::select
rename <- dplyr::rename

library(car)

# Prepare the data

dat2 <- dat %>%
  mutate(Day = factor(day),
         Task = factor(task),
         Foil = factor(foil),
         Acc = factor(correct),
         Order = factor(order),
         Trial = trial,
         Subject = factor(name)) %>%
  select(Day,Task,Acc,Subject, Order, Trial)

# Run full generalized linear mixed model

full <- glmer(Acc~ (Task + Day +Order)^3 +Trial  +(1|Subject) , data=dat2, family = 'binomial')

summary(full)
anova(full, type = "II")

#Do model selection to find the best possible fit

fitLMER.fnc(full, method = "llrt",backfit.on = "F", log.file = FALSE)

#The best fit is the full model


dat2$Order <- relevel(dat2$Order, ref="IR")
final_gen_model <- glmer (Acc~ (Task + Day +Order)^3 +Trial  +(1|Subject) + (1|Language), data=dat2, family = 'binomial', control = glmerControl(optimizer = "bobyqa"))

summary(final_gen_model)
anova(final_gen_model, type = 'II')

#with p-values
Anova(final_gen_model, type = 'II')


# do pairwise comparisons
library(emmeans)
emmeans(final_gen_model, pairwise~Task|Order|Day)
emmeans(final_gen_model, pairwise~Day|Order|Task)
emmeans(final_gen_model, pairwise~Order|Day|Task)


# Check model for overdispersion 
plot(final_gen_model)
library(RVAideMemoire)
overdisp.glmer(final_gen_model)


### GRAPHS ###

#First lets regroup the data in new dataframes, with individual averages instead of correct/incorrect 0/1 per trial


dat3 <- dat %>%
  mutate(Day = factor(day),
         Task = factor(task),
         Foil = factor(foil),
         Acc = correct,
         Order = factor(order),
         Subject = factor(name)) %>%
  select(Day,Task,Acc,Subject, Order, Foil)


### Build dataframe ####

by_groups <- group_by(dat3, Subject, Task, Day, Order,Foil)
graph_data <- summarise(by_groups, acc = mean(Acc, na.omit = TRUE))

by_TD1 <- group_by(graph_data, Task, Day, Order,Foil)
by_TD <- na.omit(by_TD1)

library(plyr)

### BOXPLOT ###

library(ggplot2)
library(cowplot)
require(cowplot)
library(ggthemes)


#### Now separate by foil ### 

IR1_plot <- ggplot(subset(by_TD, Day == "1" & Order == "IR")) + 
  geom_boxplot(aes(x=Foil, y=acc, fill = Task), position=position_dodge(0.8)) + 
  
  
  scale_y_continuous(name = "Keypress accuracy",
                     breaks = seq(0, 1, 0.25),limits = c(0,1))+ 
  scale_x_discrete(name = "Foil") +
  ggtitle("Day 1: Order I-R") +
  theme(plot.title = element_text(size = 22, family = "Helvetica"),
        text = element_text(size = 22, family = "Helvetica"),
        axis.text.x=element_text(size = 22), 
        axis.text.y=element_text(size = 16)) +
  #geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(1))+
  scale_fill_manual(values=c("#999999", "#E69F00"))

IR2_plot <-ggplot(subset(by_TD, Day == "2" & Order == "IR")) + 
  geom_boxplot(aes(x=Foil, y=acc, fill = Task), alpha = 1,position=position_dodge(0.8))+ 
  
  scale_y_continuous(name = "Keypress accuracy",
                     breaks = seq(0, 1, 0.25),limits = c(0,1))+
  scale_x_discrete(name = "Foil") +
  ggtitle("Day 2: Order I-R") +
  theme(plot.title = element_text(size = 22, family = "Helvetica"),
        text = element_text(size = 22, family = "Helvetica"),
        axis.text.x=element_text(size = 22), 
        axis.text.y=element_text(size = 16)) +
  scale_fill_manual(values=c("#999999", "#E69F00"))  

plot_grid(IR1_plot, IR2_plot)

by_TD$Task <- factor(by_TD$Task, levels = rev(levels(by_TD$Task)))

RI1_plot <- ggplot(subset(by_TD, Day == "1" & Order == "RI")) + 
  geom_boxplot(aes(x=Foil, y=acc, fill = Task), position=position_dodge(0.8)) + 
  
  scale_y_continuous(name = "Keypress accuracy",
                     breaks = seq(0, 1, 0.25),limits = c(0,1))+
  scale_x_discrete(name = "Foil") +
  ggtitle("Day 1: Order R-I") +
  theme(plot.title = element_text(size = 22, family = "Helvetica"),
        text = element_text(size = 22, family = "Helvetica"),
        axis.text.x=element_text(size = 22), 
        axis.text.y=element_text(size = 16)) +
  #geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(1))+
  scale_fill_manual(values=c("#E69F00","#999999"))

RI2_plot <-ggplot(subset(by_TD, Day == "2" & Order == "RI")) + 
  geom_boxplot(aes(x=Foil, y=acc, fill = Task), alpha = 1,position=position_dodge(0.8))+ 
  
  scale_y_continuous(name = "Keypress accuracy",
                     breaks = seq(0, 1, 0.25),limits = c(0,1))+
  scale_x_discrete(name = "Foil") +
  ggtitle("Day 2: Order R-I") +
  theme(plot.title = element_text(size = 22, family = "Helvetica"),
        text = element_text(size = 22, family = "Helvetica"),
        axis.text.x=element_text(size = 22), 
        axis.text.y=element_text(size = 16)) +
  scale_fill_manual(values=c("#E69F00","#999999"))  

plot_grid(RI1_plot, RI2_plot)


plot_grid(IR1_plot, IR2_plot, RI1_plot, RI2_plot, hjust = 0, vjust = 1,
          scale = c(1., 1., 1, 1))
