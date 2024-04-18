library(survival)
library(kableExtra)
library(tidyverse)
library(survminer)
library(ipw)
library(lme4)
library(ggplot2)
library(patchwork)
library(gridExtra)
library(grid)
library(gridtext)
library(stargazer)
library(broom)
library(RColorBrewer)
library(powerSurvEpi)
fluid_df = read.csv('Research Materials/BP/data/dengue_fluid.csv',header=TRUE)
colnames(fluid_df)[1] = "Study_Number"

hosp_df = read.csv('Research Materials/fluid/data/cleaned_df1.csv',header=TRUE)
hosp_df = hosp_df[,c(1,2,4,5,6,7,8,11,12,13,14,15,16,17,18,19,20)]
hosp_df$Abdo_bf = NA
hosp_df$Clin_bf = NA
hosp_df$Muco_bf = NA
hosp_df$Letha_bf = NA
hosp_df$Hepa_bf = NA
hosp_df$HCT_bf = NA
hosp_df$Hypo_bf = NA
hosp_df$Vomit_bf = NA
hosp_df$ws_bf = NA

for(sn in unique(hosp_df$Study_Number)){
  temp_df = hosp_df[hosp_df$Study_Number==sn,]
  for(d in temp_df$Day){
    if(d==1){
      hosp_df[hosp_df$Study_Number==sn,]$Abdo_bf[d] = 0
      hosp_df[hosp_df$Study_Number==sn,]$Clin_bf[d] = 0
      hosp_df[hosp_df$Study_Number==sn,]$Muco_bf[d] = 0
      hosp_df[hosp_df$Study_Number==sn,]$Letha_bf[d] = 0
      hosp_df[hosp_df$Study_Number==sn,]$Hepa_bf[d] = 0
      hosp_df[hosp_df$Study_Number==sn,]$HCT_bf[d] = 0
      hosp_df[hosp_df$Study_Number==sn,]$Hypo_bf[d] = 0
      hosp_df[hosp_df$Study_Number==sn,]$Vomit_bf[d] = 0
    }else{
      temp_df2 = temp_df[seq(d-1),]
      hosp_df[hosp_df$Study_Number==sn,]$Abdo_bf[d] = ifelse(is.element(1,temp_df2$Abdominal_Pain_Tenderness),1,0)
      hosp_df[hosp_df$Study_Number==sn,]$Clin_bf[d] = ifelse(is.element(1,temp_df2$Clinical_fluid_accumulation),1,0)
      hosp_df[hosp_df$Study_Number==sn,]$Muco_bf[d] = ifelse(is.element(1,temp_df2$Mucosal_Bleed),1,0)
      hosp_df[hosp_df$Study_Number==sn,]$Letha_bf[d] = ifelse(is.element(1,temp_df2$Lethargy),1,0)
      hosp_df[hosp_df$Study_Number==sn,]$Hepa_bf[d] = ifelse(is.element(1,temp_df2$Hepatomegaly),1,0)
      hosp_df[hosp_df$Study_Number==sn,]$HCT_bf[d] = ifelse(is.element(1,temp_df2$HCT),1,0)
      hosp_df[hosp_df$Study_Number==sn,]$Hypo_bf[d] = ifelse(is.element(1,temp_df2$Hypotension_for_age),1,0)
      hosp_df[hosp_df$Study_Number==sn,]$Vomit_bf[d] = ifelse(is.element(1,temp_df2$Persistent_vomiting),1,0)
      
    }
    wscheck = c(hosp_df[hosp_df$Study_Number==sn,]$Abdo_bf[d],
                hosp_df[hosp_df$Study_Number==sn,]$Clin_bf[d],
                hosp_df[hosp_df$Study_Number==sn,]$Muco_bf[d],
                hosp_df[hosp_df$Study_Number==sn,]$Letha_bf[d],
                hosp_df[hosp_df$Study_Number==sn,]$Hepa_bf[d],
                hosp_df[hosp_df$Study_Number==sn,]$HCT_bf[d],
                hosp_df[hosp_df$Study_Number==sn,]$Hypo_bf[d],
                hosp_df[hosp_df$Study_Number==sn,]$Vomit_bf[d])
    hosp_df[hosp_df$Study_Number==sn,]$ws_bf[d] = ifelse(is.element(1,wscheck),1,0)
    
  }
}


fluid_df$SD_day = NA
for (ind in fluid_df$Study_Number) {
  if (is.na(fluid_df[fluid_df$Study_Number==ind,'days_to_SD'])==FALSE && fluid_df[fluid_df$Study_Number==ind,'days_to_SD']>0){
    fluid_df[fluid_df$Study_Number==ind,'SD_day']=fluid_df[fluid_df$Study_Number==ind,'days_to_SD']+1
  }else{
    fluid_df[fluid_df$Study_Number==ind,'SD_day']=fluid_df[fluid_df$Study_Number==ind,'stay']
  }
}
hosp_df$Volume = as.numeric(hosp_df$Volume)

ps_model_fluid = glm(treatment~age+gender+CCMI+Abdominal_Pain_Tenderness_bf+Clinical_fluid_accumulation_bf+Mucosal_Bleed_bf+Lethargy_bf+Hepatomegaly_bf+
                       HCT_bf+Hypotension_for_age_bf+blood_product_bSD_avg,family=binomial,data=fluid_df)
ps_scores <- predict(ps_model_fluid, type = 'response')
ipw_fluid <- ifelse(fluid_df$treatment == 1, 
                    1 / ps_scores, 
                    1 / (1 - ps_scores))
ow_fluid = ifelse(fluid_df$treatment == 1,
                    1 - ps_scores,
                    ps_scores)
fluid_df$ipw_fluid = ipw_fluid
fluid_df$ow_fluid = ow_fluid

hosp_df$weights_fluid = NA
hosp_df$ow_fluid = NA
for(sn in fluid_df$Study_Number){
  hosp_df[hosp_df$Study_Number==sn,]$weights_fluid = fluid_df[fluid_df$Study_Number==sn,]$ipw_fluid
  hosp_df[hosp_df$Study_Number==sn,]$ow_fluid = fluid_df[fluid_df$Study_Number==sn,]$ow_fluid
  
}


for(sn in unique(hosp_bf$Study_Number)){
  if(fluid_df[fluid_df$Study_Number==sn,]$outcome==0){
    sd_day = fluid_df[fluid_df$Study_Number==sn,]$SD_day + 1
  }else{
    sd_day = fluid_df[fluid_df$Study_Number==sn,]$SD_day  
  }
  hosp_bf = subset(hosp_bf,!(Study_Number==sn & Day>=sd_day))
  
}

merged_data = hosp_df
merged_data$volume_level = NA

for(i in seq(dim(merged_data)[1])){
  if(is.na(merged_data[i,'Volume'])){
    merged_data[i,'Volume'] = 0
    merged_data[i,'Volume_level'] = 0
  }
  else if(merged_data[i,'Volume']==0){
    merged_data[i,'volume_level']=0
  }else if(merged_data[i,'Volume']<=1000){
    merged_data[i,'volume_level']=1
  }else if(merged_data[i,'Volume']<=2000){
    merged_data[i,'volume_level']=2
  }else if(merged_data[i,'Volume']>2000){
    merged_data[i,'volume_level']=3
  }
  
}
merged_data$Outcome = NA
for(sn in unique(merged_data$Study_Number)){
  temp_df = merged_data[merged_data$Study_Number==sn,]
  if(fluid_df[fluid_df$Study_Number==sn,]$outcome==1){
    for(d in temp_df$Day){
      merged_data[merged_data$Study_Number==sn & merged_data$Day==d,]$Outcome = 'Severe Dengue'
    }
  }else{
    for(d in temp_df$Day){
      merged_data[merged_data$Study_Number==sn & merged_data$Day==d,]$Outcome = 'Non Severe Dengue'
    }
  }
  
}

for(sn in fluid_df$Study_Number){
  temp_row = merged_data[merged_data$Study_Number==sn&merged_data$Day==1,]
  temp_row2 = merged_data[merged_data$Study_Number==sn&merged_data$Day==1,]
  temp_row$Day=0
  temp_row[temp_row==1]=0
  temp_row$gender=temp_row2$gender
  temp_row$CCMI=temp_row2$CCMI
  temp_row$volume_level = 0
  merged_data=rbind(merged_data,temp_row)
  
}
merged_data = merged_data[order(merged_data$Study_Number,merged_data$Day),]
rownames(merged_data)=NULL

# outliers of hospitalization period
outlier = boxplot.stats(fluid_df$stay)$out
outlier
to_drop = fluid_df[fluid_df$stay > 8,]$Study_Number
new_fluid = fluid_df[! fluid_df$Study_Number %in% to_drop,]
new_hosp = hosp_df[! hosp_df$Study_Number %in% to_drop,]
merged_data = merged_data[! merged_data$Study_Number %in% to_drop,]


view(merged_data)
hist_data = merged_data[!merged_data$Day == 0,]
rownames(hist_data)=NULL
for(sn in unique(hist_data$Study_Number)){
  if(fluid_df[fluid_df$Study_Number==sn,]$outcome==0){
    sd_day = fluid_df[fluid_df$Study_Number==sn,]$SD_day + 1
  }else{
    sd_day = fluid_df[fluid_df$Study_Number==sn,]$SD_day  
  }
  hist_data = subset(hist_data,!(Study_Number==sn & Day>=sd_day))
  
}
# all hist
hist1 = ggplot(hist_data,aes(x=volume_level)) + 
  geom_bar(stat='count',width = 0.6,fill='steelblue') +
  geom_text(stat='count', aes(label=..count..),
            vjust=-1, size = 6) +
  ggtitle('Histogram of Different Volume Levels') + 
  labs(x='Volume Level',y='Patient-Days') +
  scale_x_continuous(limits=c(-0.6,3.5),breaks=c(0,1,2,3),labels=c('No Fluid','Low','Medium','High')) +
  coord_cartesian(ylim = c(0, 6200))+
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold",margin=margin(0,0,30,0)),
        axis.text=element_text(size = 16,face='bold'),
        axis.title=element_text(size = 18),
        legend.title = element_text(size = 18), #change legend title font size
        legend.text = element_text(size = 18),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.box = 'horizontal')  
hist1
ggsave('Research Materials/fluid/output/coxsummary/histogram1.pdf',hist1,dpi=1000,width=8,height=6)
ggsave('Research Materials/fluid/output/coxsummary/histogram1.png',hist1,dpi=1500,width=8,height=6)
# sub hist
hist2 = ggplot(hist_data,aes(x=volume_level,fill = Outcome)) + 
  geom_bar(stat='count',width = 0.6, position = 'dodge') + 
  ggtitle('Histogram of Different Volume Levels') + 
  labs(x='Volume Level',y='Patient-Days') +
  scale_x_continuous(limits=c(-0.6,3.5),breaks=c(0,1,2,3),labels=c('No Fluid','Low','Medium','High')) + 
  scale_fill_brewer(palette="Dark2") + 
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold",margin=margin(0,0,30,0)),
        axis.text=element_text(size = 16,face='bold'),
        axis.title=element_text(size = 18),
        legend.title = element_text(size = 18), #change legend title font size
        legend.text = element_text(size = 18),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.box = 'horizontal')+
  coord_cartesian(ylim = c(0, 6200))+
  geom_text(stat='count', aes(label=..count.., group = Outcome),positio = position_dodge(width = 0.6),
            vjust=-1, size = 5.5)
hist2
ggsave('Research Materials/fluid/output/coxsummary/histogram2.pdf',hist2,dpi=1000,width=8,height=6)
ggsave('Research Materials/fluid/output/coxsummary/histogram2.png',hist2,dpi=1500,width=8,height=6)

df_time_ind = 
  tmerge(data1 = new_fluid[,c(1,2,3,5,6,7,18,19,20)],
         data2 = new_fluid[,c(1,2,3,5,6,7,18,19,20)],
         id = Study_Number,
         event = event(SD_day,outcome))

tmerged_df = 
  tmerge(data1=df_time_ind,
         data2=merged_data,
         id=Study_Number,
         exposure=tdc(Day,given_fluid),
         Abdo_bf=tdc(Day,Abdo_bf),
         Clin_bf=tdc(Day,Clin_bf),
         Muco_bf=tdc(Day,Muco_bf),
         Letha_bf=tdc(Day,Letha_bf),
         Hepa_bf=tdc(Day,Hepa_bf),
         HCT_bf=tdc(Day,HCT_bf),
         Hypo_bf=tdc(Day,Hypo_bf),
         ws_bf = tdc(Day,ws_bf))

tmerged_df_volume = 
  tmerge(data1=df_time_ind,
         data2=merged_data,
         id=Study_Number,
         exposure=tdc(Day,volume_level),
         Abdo_bf=tdc(Day,Abdo_bf),
         Clin_bf=tdc(Day,Clin_bf),
         Muco_bf=tdc(Day,Muco_bf),
         Letha_bf=tdc(Day,Letha_bf),
         Hepa_bf=tdc(Day,Hepa_bf),
         HCT_bf=tdc(Day,HCT_bf),
         Hypo_bf=tdc(Day,Hypo_bf),
         ws_bf = tdc(Day,ws_bf))


aggregatedf = function(df){
  new_df = df %>%
    group_by(Study_Number) %>%
    # Start a new group every time 'exposure', 'ws', or 'event' changes
    group_by(group = cumsum(exposure != lag(exposure, default = first(exposure))|
                              Abdo_bf != lag(Abdo_bf, default = first(Abdo_bf)) |
                              Clin_bf != lag(Clin_bf, default = first(Clin_bf)) |
                              Muco_bf != lag(Muco_bf, default = first(Muco_bf)) |
                              Letha_bf != lag(Letha_bf, default = first(Letha_bf)) |
                              Hepa_bf != lag(Hepa_bf, default = first(Hepa_bf)) |
                              HCT_bf != lag(HCT_bf, default = first(HCT_bf)) |
                              Hypo_bf != lag(Hypo_bf, default = first(Hypo_bf)) |
                              event == 1), .add = TRUE) %>%
    summarize(
      age = first(age),
      gender = first(gender),
      CCMI = first(CCMI),
      ipw_fluid = first(ipw_fluid),
      ow_fluid = first(ow_fluid),
      tstart = first(tstart),
      tstop = last(tstop),
      event = max(event),
      exposure = first(exposure),
      Abdo_bf = first(Abdo_bf),
      Clin_bf = first(Clin_bf),
      Muco_bf = first(Muco_bf),
      Letha_bf = first(Letha_bf),
      Hepa_bf = first(Hepa_bf),
      HCT_bf = first(HCT_bf),
      Hypo_bf = first(Hypo_bf),
      ws_bf = first(ws_bf)) %>%
    ungroup() %>%
    select(-group) # Removing the temporary 'group' column
  return(new_df)
}
tmerged_df = aggregatedf(tmerged_df)
tmerged_df_volume = aggregatedf(tmerged_df_volume)
write.csv(hosp_df,file='Research Materials/fluid/data/coxdata/hosp_data.csv',row.names=FALSE)
write.csv(hosp_bf,file='Research Materials/fluid/data/coxdata/hosp_bf.csv',row.names=FALSE)
write.csv(fluid_df,file='Research Materials/fluid/data/coxdata/fluid_data.csv',row.names=FALSE)
write.csv(merged_data,file = 'Research Materials/fluid/data/coxdata/merged_data.csv',row.names = FALSE)
write.csv(tmerged_df,file='Research Materials/fluid/data/coxdata/tmerged_df.csv',row.names=FALSE)
write.csv(tmerged_df_volume,file='Research Materials/fluid/data/coxdata/tmerged_df_volume.csv',row.names=FALSE)
write.csv(new_fluid,file='Research Materials/fluid/data/coxdata/new_fluid.csv',row.names=FALSE)
