library(survival)
library(powerSurvEpi)

expected_HR = 0.8
alpha = 0.1
alpha1 = 0.05
power = 0.8

power_data = df_time_ind 
# df_time_ind:
# tstart - 0, tstop - day of Severe Dengue/last day of hospitalization for non-SD grp
# treatment - 0 for no fluid, 1 for fluid
# event - 0 for non-SD, 1 for SD

power_data = power_data %>%
  mutate(treatment = ifelse(treatment == 0, 'C', 'E'))
# change treatment variable from 0/1 to 'C'/'E' according to the function requirement

# powerCT: Power calculation for the Comparison of Survival Curves Between Two Groups 
# under the Cox Proportional-Hazards Model for clinical trials.
# nE: number of participants in the experimental group
# nC: number of participants i the control group
# RR: postulated hazard ratio
# alpha: type 1 error rate

result =  powerCT(Surv(tstart,tstop,event)~factor(treatment),
                  alpha=alpha,nE=4269,nC=462,RR=0.8,dat = power_data)
# $power: 0.5395567

result1 =  powerCT(Surv(tstart,tstop,event)~factor(treatment),
                  alpha=alpha1,nE=4269,nC=462,RR=0.8,dat = power_data)
# $power: 0.4145744


for(rr in c(0.6,1.2,1.4)){
  result_power = powerCT(Surv(tstart,tstop,event)~factor(treatment),
                         alpha=alpha1,nE=4269,nC=462,RR=rr,dat = power_data)
  print(result_power$power)
}
