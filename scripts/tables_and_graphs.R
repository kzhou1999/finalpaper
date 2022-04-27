#### Preamble ####
# Purpose: codes for plots and tables and models
# Author: Qiao Zhou
# Data: 5 April 2022
# Contact: katherine.zhou@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(patchwork)
library(kableExtra)
library(car)

##codes for the table1(Definitions of Variables)
#create data frame 
vardef=data_frame("Variables"=c("sub_health", "age", "gender", "network_size", "social quality", "self_efficacy", "self_reflection", "solitude", "anxiety", "mean_pos_affect", "mean_neg_affect"),
                  "Definition"=c("The rate of the overall wellbeing on the scale of 5", "Age at baseline session in years", "1 = female, 0 = male", "Total network size", "The social relationship quality of participants on the scale of 5","Social self-efficacy score on the scale of 5","Self-reflection score on the scale of 5","Overall time in solitude in proportion","Personal anxiety score for their social relationship on the scale of 5","The average positive solitude experience in 10 days","The average negative solitude experience in 10 days"))
#generate table and add caption
knitr::kable(vardef,caption="Definitions of Variables")

##codes for Figure1(Distribution of solitude experiences)
#add two new columns 
Positive_solitude <- final_data%>%mutate(value=mean_pos_affect)
Negative_solitude <-final_data%>%mutate(value=mean_neg_affect)
#combine the two dataframes into one.  
Positive_solitude$Experience_type <- 'Positive solitude experience'
Negative_solitude$Experience_type <- 'Negative solitude experience'
exp <- rbind(Positive_solitude, Negative_solitude)
#draw the plot
ggplot(exp, aes(value, fill = Experience_type)) + 
  geom_histogram(alpha = 0.4, position = 'identity',bins = 23)

##codes for Figure2(Relationship between the social quality and the positive solitude experiences)
#draw the scatterplot for pos
final_data%>%
  ggplot(aes(x=social_quality,y=mean_pos_affect,)) +labs(x="social quality",y="Positive solitude experiences")+
  geom_point()+
  geom_smooth(method = "lm")

##codes for Figure3(Relationship between the social quality and the negative solitude experiences)
#draw the scatterplot fot neg
final_data%>%
  ggplot(aes(x=social_quality,y=mean_neg_affect,)) +labs(x="social quality",y="Negative solitude experiences")+
  geom_point()+
  geom_smooth(method = "lm")

##codes for Figure4(Relationship between the social quality and the network size)
#draw the density plot
final_data %>% ggplot(aes(x = network_size, y = social_quality)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") + labs(
    x = "network size",
    y = "social quality",
  )+
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.major.y = element_line(
          color = "gray",
          size = 0.5,
          linetype = 1
        ),
        axis.line =  element_line(
          color = "black",
          size = 0.5,
          linetype = 1
        )
  )

##codes for Figure5(Relationship between the anxiety and the solitude experiences based on gender)
#add a new column Gender and draw the scatterplot for pos
p1<-final_data%>%mutate(Gender=case_when(gender==1~"female", gender==0~"male"))%>%
  ggplot(aes(x=anxiety,y=mean_pos_affect,color=Gender)) +labs(x="anxiety",y="Positivee solitude experiences")+
  geom_point()+
  geom_smooth(method = "lm")
#add a new column Gender and draw the scatterplot for neg
p2<-final_data%>%mutate(Gender=case_when(gender==1~"female", gender==0~"male"))%>%
  ggplot(aes(x=anxiety,y=mean_neg_affect,color=Gender)) +labs(x="anxiety",y="Negative solitude experiences")+
  geom_point()+
  geom_smooth(method = "lm")
#rearrange the plots
p1+p2

##codes for Figure6(Relationship between the age group and the solitude experiences)
#add a new column age group and draw the boxplot for pos
p3<-final_data%>%
  mutate(Age_group=case_when(age<50~"Student",age>=49~"Older adult"))%>%
  ggplot(aes(y=mean_pos_affect, x=Age_group))+geom_boxplot(color="black",fill="light blue")+labs(x="Age group",y="Positive solitude affect")+ coord_flip()
#add a new column age group and draw the boxplot for neg
p4<-final_data%>%
  mutate(Age_group=case_when(age<50~"Student",age>=49~"Older adult"))%>%
  ggplot(aes(y=mean_neg_affect, x=Age_group))+geom_boxplot(color="black",fill="light blue")+labs(x="Age group",y="Negatice solitude affect")+ coord_flip()
#rearrange the plots
p3+p4

#codes for fitting the full models for both positive and negative experiences
#fit model for pos
pos_mod=lm(mean_pos_affect~age+gender+network_size+social_quality+self_efficacy+self_reflection+solitude+anxiety+sub_health,data=final_data)
#fit model for neg
neg_mod=lm(mean_neg_affect~age+gender+network_size+social_quality+self_efficacy+self_reflection+solitude+anxiety+sub_health,data=final_data)
summary(pos_mod)
summary(neg_mod)
#check vif for pos_mod
pos_vif=vif(pos_mod)
#check vif for neg_mod
neg_vif=vif(neg_mod)

##codes for Table2(VIF value for the full linear models)
#create dataframe and generate table
Variable_pos <- c("age","gender","network_size","social_quality","self_efficacy","self_reflection","solitude",
                  "anxiety","sub_health")
Variable_neg <- c("age","gender","network_size","social_quality","self_efficacy","self_reflection","solitude",
                  "anxiety","sub_health")
vif_table<-data.frame(Variable_pos, pos_vif,Variable_neg, neg_vif)
knitr::kable(vif_table, col.names = c("variables of positive solitude model", "vif value", "variables of negative solitude model", "vif value"), caption = "VIF value for the full linear models")%>%
  kable_styling(latex_options = "HOLD_position")

##codes for doing the backward selection and decided the final models
m = length(final_data$mean_pos_affect)
#bic backward selection for pos_mod
bic_pos = step(pos_mod, direction = "backward", k = log(m))
n = length(final_data$mean_neg_affect)
#bic backward seclection for neg_mod
bic_neg = step(neg_mod, direction = "backward", k = log(n))
#fit final model for pos
final_pos_mod =lm(mean_pos_affect~age+social_quality+self_efficacy,data=final_data)
summary(final_pos_mod)
#fit final model for neg
final_neg_mod =lm(mean_neg_affect~age+social_quality+anxiety,data=final_data)
summary(final_neg_mod)

##codes for Table3(The estimations and the p values for the positive solitude affect model)
#create dataframe and generate table
varp=c("intercept","age","social_quality","self_efficacy")
estp=c(0.74659,0.24844,5.96085,5.78392)
pp=c(0.915136,1.8*10^(-8),0.000221,0.001318)
mod1_table=data.frame("variables"=varp, "estimations"=estp,"p_values"=pp)
knitr::kable(mod1_table,caption="The estimations and the p values for the positive solitude affect model")%>%
  kable_styling(latex_options = "HOLD_position")

##codes for Table4(The estimations and the p values for the negative solitude affect model)
#create dataframe and generate table
varn=c("intercept","age","social_quality","anxiety")
estn=c(50.09895,-0.14323,-6.83339,4.33072)
pn=c(2.82*10^-7,0.00695,0.00014,0.00743)
mod2_table=data.frame("variables"=varn, "estimations"=estn,"p_values"=pn)
knitr::kable(mod2_table,caption="The estimations and the p values for the negative solitude affect model")%>%
  kable_styling(latex_options = "HOLD_position")

















