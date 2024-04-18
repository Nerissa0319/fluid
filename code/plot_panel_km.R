tmerged_df = read.csv('C:/Users/nerissa.xu/Documents/Research Materials/fluid/data/coxdata/tmerged_df.csv',header=TRUE)
tmerged_df_volume = read.csv('C:/Users/nerissa.xu/Documents/Research Materials/fluid/data/coxdata/tmerged_df_volume.csv',header=TRUE)
writesummary = function(modelcox,filename){
  coef_90 = confint(modelcox,level=0.9)
  hr_90 = data.frame(exp(coef_90))
  summ = summary(modelcox)
  call1 = summ$call
  n = summ$n
  nevent = summ$nevent
  newline = data.frame('n',n,'number of events',nevent)
  newline = rbind(newline,rep('',dim(newline)[2]))
  write.table(newline,file=filename,append=FALSE,sep=',',row.names=FALSE,col.names = FALSE)
  coef1 = summ$coefficients
  newdf = data.frame('V'=rownames(coef1),coef1)
  newdf = rbind(newdf,rep('',dim(newdf)[2]))
  write.table(newdf,file=filename,append=TRUE,sep=',',row.names=F,col.names = TRUE)
  confint1 = summ$conf.int
  newdf = data.frame('v'=rownames(confint1),confint1)
  newdf$'Lower90' = hr_90$X5..
  newdf$'Upper90' = hr_90$X95..
  newdf = rbind(newdf,rep('',dim(newdf)[2]))
  write.table(newdf,file=filename,append=TRUE,sep=',',row.names=F,col.names = TRUE)
  concordance1 = summ$concordance
  newline = data.frame('Concordance',concordance1[[1]],'se',concordance1[[2]])
  newline = rbind(newline,rep('',dim(newline)[2]))
  write.table(newline,file=filename,append=TRUE,sep=',',row.names=FALSE,col.names = FALSE)
  llt = summ$logtest
  newline = data.frame('Likelihood ratio test',llt[[1]],'df',llt[[2]],'pvalue',llt[[3]])
  newline = rbind(newline,rep('',dim(newline)[2]))
  write.table(newline,file=filename,append=TRUE,sep=',',row.names=FALSE,col.names = FALSE)
  wt = summ$waldtest
  newline = data.frame('Wald test',wt[[1]],'df',wt[[2]],'pvalue',wt[[3]])
  newline = rbind(newline,rep('',dim(newline)[2]))
  write.table(newline,file=filename,append=TRUE,sep=',',row.names=FALSE,col.names = FALSE)
  st = summ$sctest
  newline = data.frame('Score (logrank) test',st[[1]],'df',st[[2]],'pvalue',st[[3]])
  newline = rbind(newline,rep('',dim(newline)[2]))
  write.table(newline,file=filename,append=TRUE,sep=',',row.names=FALSE,col.names = FALSE)
  rob = summ$robscore
  newline = data.frame('Robust',rob[[1]],'df',rob[[2]],'pvalue',rob[[3]])
  newline = rbind(newline,rep('',dim(newline)[2]))
  write.table(newline,file=filename,append=TRUE,sep=',',row.names=FALSE,col.names = FALSE)
  
}

cox_model = function(df,formula_n,grp,summarytitle){
  cox = coxph(as.formula(formula_n),data=df,weights = ipw_fluid)#weights=ow_fluid)
  legends = c('No Fluid',grp)
  km_fit = survfit(Surv(tstart,tstop,event)~factor(exposure),data=df,conf.int=0.9,weights = ipw_fluid)#conf.int = 0.9,weights=ow_fluid)
  if(grp=='Low'){palettes=c(brewer.pal(6,'Set2')[1],'pink')}
  else if(grp=='Medium'){palettes=c(brewer.pal(6,'Set2')[1],'#CB181C')}
  else if(grp=='High'){palettes=c(brewer.pal(6,'Set2')[1],'#67000D')}
  else {palettes=c(brewer.pal(6,'Set2')[1],'red')}
  p_km = ggsurvplot(km_fit,
                    conf.int=TRUE,
                    conf.int.alpha=0.2,
                    legend.labs=legends,
                    ggtheme=theme_classic(),
                    data=df,
                    palette=palettes)
  p_risk = ggsurvplot(km_fit,
                      conf.int=TRUE,
                      conf.int.alpha=0.2,
                      legend.labs=legends,
                      ggtheme=theme_classic(),
                      data=df,
                      palette=palettes,
                      fun = 'event')
  writesummary(cox,paste('Research Materials/fluid/output/coxsummary/',grp,'_',summarytitle,'.csv',sep=''))
  returned_ls = list('model'=cox,'kmfit'=km_fit,'km_plot'=p_km,'risk_plot'=p_risk)
  return(returned_ls)
}

# subgroup analyisis
subgroup_cox = function(data,group,volumelevel){
  if(grepl('sen',group)){
    formulax = 'Surv(tstart,tstop,event)~factor(exposure)'
  }else if(grepl('ow',group)){
   formulax =  'Surv(tstart,tstop,event)~factor(exposure)'
  }else{
    formulax = "Surv(tstart,tstop,event)~factor(exposure)+age+gender+CCMI+Abdo_bf+
  Clin_bf+Muco_bf+Letha_bf+Hepa_bf+HCT_bf+Hypo_bf"  
  }
  
  if(grepl('age',group)){
    df1 = data[data$age<55,]
    df2 = data[data$age>=55,]
    if(grepl('sen',group)){
      summtitles = list('55minus_sen','55plus_sen')
    }else{
      summtitles = list('55minus','55plus')
    }
    formula1 = gsub("\\+age",'',formulax)
  }else if(grepl('gender',group)){
    df1 = data[data$gender==0,]
    df2 = data[data$gender==1,]
    if(grepl('sen',group)){
      summtitles = list('Female_sen','Male_sen')
    }else{
      summtitles = list('Female','Male')
    }
    formula1 = gsub("\\+gender",'',formulax)
  }else if(grepl('Abdo_bf',group)){
    df1 = data[data$Abdo_bf==0,]
    df2 = data[data$Abdo_bf==1,]
    if(grepl('sen',group)){
      summtitles = list('noAbdominalPain_sen','AbdominalPain_sen')
    }else{
      summtitles = list('noAbdominalPain','AbdominalPain')
    }
    formula1 = gsub("\\+Abdo_bf",'',formulax)
  }else if(grepl('Clin_bf',group)){
    df1 = data[data$Clin_bf==0,]
    df2 = data[data$Clin_bf==1,]
    formula1 = gsub("\\+Clin_bf",'',formulax)
  }else if(grepl('Muco_bf',group)){
    df1 = data[data$Muco_bf==0,]
    df2 = data[data$Muco_bf==1,]
    if(grepl('sen',group)){
      summtitles = list('noMucosalBleed_sen','MucosalBleed_sen')
    }else{
      summtitles = list('noMucosalBleed','MucosalBleed')
    }
    formula1 = gsub("\\+Muco_bf",'',formulax)
  }else if(grepl('Letha_bf',group)){
    df1 = data[data$Letha_bf==0,]
    df2 = data[data$Letha_bf==1,]
    if(grepl('sen',group)){
      summtitles = list('noLethargy_sen','Lethargy_sen')
    }else{
      summtitles = list('noLethargy','Lethargy')
    }
    formula1 = gsub("\\+Letha_bf",'',formulax)
  }else if(grepl('Hepa_bf',group)){
    df1 = data[data$Hepa_bf==0,]
    df2 = data[data$Hepa_bf==1,]
    formula1 = gsub("\\+Hepa_bf",'',formulax)
  }else if(grepl('HCT_bf',group)){
    df1 = data[data$HCT_bf==0,]
    df2 = data[data$HCT_bf==1,]
    formula1 = gsub("\\+HCT_bf",'',formulax)
  }else if(grepl('Hypo_bf',group)){
    df1 = data[data$Hypo_bf==0,]
    df2 = data[data$Hypo_bf==1,]
    formula1 = gsub("\\+Hypo_bf",'',formulax)
  }else if(grepl('ws_bf',group)){
    df1 = data[data$ws_bf==0,]
    df2 = data[data$ws_bf==1,]
    if(grepl('sen',group)){
      summtitles = list('noWarningSigns_sen','WarningSigns_sen')
      formula1 = 'Surv(tstart,tstop,event)~factor(exposure)'
    }else{
      summtitles = list('noWarningSigns','WarningSigns')
      formula1 = "Surv(tstart,tstop,event)~factor(exposure)+age+gender+CCMI"
    }
    
  }
 
  
  kmplots=list()
  result = list()
  count = 0
  for(sub_df in list(df1,df2)){
    count = count+1
    subresult = cox_model(sub_df,formula1,volumelevel,summtitles[[count]])
    kmplots[[count]]=subresult$km_plot
    result[[count]] = subresult
    
  }
  return(result)
}


formulax = "Surv(tstart,tstop,event)~factor(exposure)+age+gender+CCMI+Abdo_bf+
  Clin_bf+Muco_bf+Letha_bf+Hepa_bf+HCT_bf+Hypo_bf"

formula_sen = "Surv(tstart,tstop,event)~factor(exposure)" 
novslow = tmerged_df_volume[tmerged_df_volume$exposure ==0|tmerged_df_volume$exposure==1,]
novsmedium = tmerged_df_volume[tmerged_df_volume$exposure ==0|tmerged_df_volume$exposure==2,]
novshigh = tmerged_df_volume[tmerged_df_volume$exposure ==0|tmerged_df_volume$exposure==3,]

sensitivitylow = cox_model(novslow,formula_sen,'Low','Sensitivity Analysis')
sensitivitymed = cox_model(novsmedium,formula_sen,'Medium','Sensitivity Analysis')
sensitivityhigh = cox_model(novshigh,formula_sen,'High','Sensitivity Analysis')
sensitivityall = cox_model(tmerged_df,formula_sen,'Fluid','Sensitivity Analysis')


sen_all_age = subgroup_cox(tmerged_df,'age_sen','Fluid')
sen_all_gender = subgroup_cox(tmerged_df,'gender_sen','Fluid')
sen_all_abdo = subgroup_cox(tmerged_df,'Abdo_bf_sen','Fluid')
sen_all_muco = subgroup_cox(tmerged_df,'Muco_bf_sen','Fluid')
sen_all_letha = subgroup_cox(tmerged_df,'Letha_bf_sen','Fluid')
sen_all_ws = subgroup_cox(tmerged_df,'ws_bf_sen','Fluid')


sen_low_age = subgroup_cox(novslow,'age_sen','Low')
sen_low_gender = subgroup_cox(novslow,'gender_sen','Low')
sen_low_abdo = subgroup_cox(novslow,'Abdo_bf_sen','Low')
sen_low_muco = subgroup_cox(novslow,'Muco_bf_sen','Low')
sen_low_letha = subgroup_cox(novslow,'Letha_bf_sen','Low')
sen_low_ws = subgroup_cox(novslow,'ws_bf_sen','Low')


sen_medium_age = subgroup_cox(novsmedium,'age_sen','Medium')
sen_medium_gender = subgroup_cox(novsmedium,'gender_sen','Medium')
sen_medium_abdo = subgroup_cox(novsmedium,'Abdo_bf_sen','Medium')
sen_medium_muco = subgroup_cox(novsmedium,'Muco_bf_sen','Medium')
sen_medium_letha = subgroup_cox(novsmedium,'Letha_bf_sen','Medium')
sen_medium_ws = subgroup_cox(novsmedium,'ws_bf_sen','Medium')


sen_high_age = subgroup_cox(novshigh,'age_sen','High')
sen_high_gender = subgroup_cox(novshigh,'gender_sen','High')
sen_high_abdo = subgroup_cox(novshigh,'Abdo_bf_sen','High')
sen_high_muco = subgroup_cox(novshigh,'Muco_bf_sen','High')
sen_high_letha = subgroup_cox(novshigh,'Letha_bf_sen','High')
sen_high_ws = subgroup_cox(novshigh,'ws_bf_sen','High')

mainall = cox_model(tmerged_df,formulax,'Fluid','Main')
ageall = subgroup_cox(tmerged_df,'age','Fluid')
genderall= subgroup_cox(tmerged_df,'gender','Fluid')
abdoall = subgroup_cox(tmerged_df,'Abdo_bf','Fluid')
mucoall = subgroup_cox(tmerged_df,'Muco_bf','Fluid')
Lethaall = subgroup_cox(tmerged_df,'Letha_bf','Fluid')
wsall = subgroup_cox(tmerged_df,'ws_bf','Fluid')


mainlow = cox_model(novslow,formulax,'Low','Main')
agelow = subgroup_cox(novslow,'age','Low')
genderlow= subgroup_cox(novslow,'gender','Low')
abdolow = subgroup_cox(novslow,'Abdo_bf','Low')
mucolow = subgroup_cox(novslow,'Muco_bf','Low')
Lethalow = subgroup_cox(novslow,'Letha_bf','Low')
wslow = subgroup_cox(novslow,'ws_bf','Low')


mainm = cox_model(novsmedium,formulax,'Medium','Main')
agem = subgroup_cox(novsmedium,'age','Medium')
genderm= subgroup_cox(novsmedium,'gender','Medium')
abdom = subgroup_cox(novsmedium,'Abdo_bf','Medium')
mucom = subgroup_cox(novsmedium,'Muco_bf','Medium')
Letham = subgroup_cox(novsmedium,'Letha_bf','Medium')
wsm = subgroup_cox(novsmedium,'ws_bf','Medium')
lsmedium = list(mainm,agem,genderm,abdom,mucom,Letham)

mainh = cox_model(novshigh,formulax,'High','Main')
ageh = subgroup_cox(novshigh,'age','High')
genderh= subgroup_cox(novshigh,'gender','High')
abdoh = subgroup_cox(novshigh,'Abdo_bf','High')
mucoh = subgroup_cox(novshigh,'Muco_bf','High')
Lethah = subgroup_cox(novshigh,'Letha_bf','High')
wsh = subgroup_cox(novshigh,'ws_bf','High')

library(openxlsx)


# Define the base path for the files
base_path <- 'Research Materials/fluid/output/coxsummary/'

# List all CSV files in the specified directory
csv_files <- list.files(pattern = "*.csv", path = base_path)

# Read each CSV file and store in a list of data frames
data_list <- map(csv_files, ~ read.csv(file.path(base_path, .),header=FALSE))

# Get the sheet names by removing the '.csv' extension from the file names
sheet_names <- sub("\\.csv$", "", csv_files)

# Write each data frame to a separate sheet in an Excel workbook
write.xlsx(data_list, 
           file = file.path(base_path, 'Cox_Model_Summary.xlsx'), 
           sheetName = sheet_names)


plotformat = function(result,titles,num,allplots){
  result_plot = list()
  result_plot2 = list()
  for(i in seq(num)){
    if(num==1){
      p = result$km_plot$plot
      p2 = result$risk_plot$plot
    }else{
        p = result[[i]]$km_plot$plot
        p2 = result[[i]]$risk_plot$plot
    }
    p = p + ggtitle(titles[i])+
      theme(plot.title = element_text(size = 13, face = "bold"),
            axis.text=element_text(size = 12),
            axis.title=element_text(size = 12),
            legend.title = element_text(size = 12), #change legend title font size
            legend.text = element_text(size = 12)) + 
      labs(x='Days') +
      scale_y_continuous(labels = scales::percent,limits=c(0.45,1))
    
    p2 = p2 + ggtitle(titles[i])+
      theme(plot.title = element_text(size = 13, face = "bold"),
            axis.text=element_text(size = 12),
            axis.title=element_text(size = 12),
            legend.title = element_text(size = 12), #change legend title font size
            legend.text = element_text(size = 12)) + 
      labs(x='Days') +
      scale_y_continuous(labels = scales::percent,limits=c(0,0.55))
    if(allplots){
      if(grepl('Main',titles[i])|grepl('No Warning Signs',titles[i])|
         grepl('No Lethargy',titles[i])){
        p = p + labs(y='Survival Probability')
        p2 = p2 + labs(y='Risk of Severe Dengue')
      }else{
        p = p + labs(y=NULL)
        p2 = p2 + labs(y=NULL)
      }
    }else{
      if(grepl('Main',titles[i])){
        p = p + labs(y='Survival Probability')
        p2 = p2 + labs(y='Risk of Severe Dengue')
      }else{
        p = p + labs(y=NULL)
        p2 = p2 + labs(y=NULL)
      }
    }
    result_plot[[i]] = p
    result_plot2[[i]] = p2
  }
  results = list('result_plot' = result_plot,'result_plot2' = result_plot2)
  return(results)
  
}

plot1 = function(plts,lvl){
  if(lvl=='Fluid'){
    tt=c('(a1) Main','(a2) No Warning Signs','(a3)Warning Signs',
         '(a4) No Abdominal Pain \n   Tenderness',
         '(a5) Abdominal Pain \n   Tenderness',
         '(a6) No Mucosal Bleed','(a7) Mucosal Bleed',
         '(a8) No Lethargy', '(a9) Lethargy')
  }else if(lvl=='low'){
    tt=c('(b1) Main','(b2) No Warning Signs','(b3)Warning Signs',
         '(b4) No Abdominal Pain \n   Tenderness',
         '(b5) Abdominal Pain \n   Tenderness',
         '(b6) No Mucosal Bleed','(b7) Mucosal Bleed',
         '(b8) No Lethargy', '(b9) Lethargy')
  }else if(lvl=='medium'){
    tt=c('(c1) Main','(c2) No Warning Signs','(c3)Warning Signs',
         '(c4) No Abdominal Pain \n   Tenderness',
         '(c5) Abdominal Pain \n   Tenderness',
         '(c6) No Mucosal Bleed','(c7) Mucosal Bleed',
         '(c8) No Lethargy', '(c9) Lethargy')
  }else{
    tt=c('(d1) Main','(d2) No Warning Signs','(d3)Warning Signs',
         '(d4) No Abdominal Pain \n   Tenderness',
         '(d5) Abdominal Pain \n   Tenderness',
         '(d6) No Mucosal Bleed','(d7) Mucosal Bleed',
         '(d8) No Lethargy', '(d9) Lethargy')
  }
  panelplots = list(
    'main' = plotformat(plts[[1]],tt[1],1,FALSE),
    'ws' = plotformat(plts[[2]],c(tt[2],tt[3]),2,FALSE),
    'abdo'= plotformat(plts[[3]],c(tt[4],tt[5]),2,FALSE),
    'muco' = plotformat(plts[[4]], c(tt[6],tt[7]),2,FALSE),
    'letha' = plotformat(plts[[5]], c(tt[8],tt[9]),2,FALSE)
  )
  return(panelplots)
}
plot2 = function(plts,lvl){
  if(lvl=='Fluid'){
    tt=c('(a1) Main','(a2) Age < 55','(a3) Age >= 55','(a4) Female','(a5) Male')
  }else if(lvl=='low'){
    tt=c('(b1) Main','(b2) Age < 55','(b3) Age >= 55','(b4) Female','(b5) Male')
  }else if(lvl=='medium'){
    tt=c('(c1) Main','(c2) Age < 55','(c3) Age >= 55','(c4) Female','(c5) Male')
  }else{
    tt=c('(d1) Main','(d2) Age < 55','(d3) Age >= 55','(d4) Female','(d5) Male')
    }
  panelplots = list(
    'main' = plotformat(plts[[1]],tt[1],1,FALSE),
    'age'= plotformat(plts[[2]],c(tt[2],tt[3]),2,FALSE),
    'gender' = plotformat(plts[[3]], c(tt[4],tt[5]),2,FALSE)
  )
  return(panelplots)
}

# 
# fluidls1 = plot1(list(mainall,wsall,abdoall,mucoall,Lethaall),'Fluid')
# fluidls2 = plot2(list(mainall,ageall,genderall),'Fluid')
# lowls1 = plot1(list(mainlow,wslow,abdolow,mucolow,Lethalow),'low')
# lowls2 = plot2(list(mainlow,agelow,genderlow),'low')
# mediumls1 = plot1(list(mainm,wsm,abdom,mucom,Letham),'medium')
# mediumls2 = plot2(list(mainm,agem,genderm),'medium')
# highls1 = plot1(list(mainh,wsh,abdoh,mucoh,Lethah),'high')
# highls2 = plot2(list(mainh,ageh,genderh),'high')


fluidls1 = plot1(list(sensitivityall,sen_all_ws,sen_all_abdo,sen_all_muco,sen_all_letha),'Fluid')
fluidls2 = plot2(list(sensitivityall,sen_all_age,sen_all_gender),'Fluid')
lowls1 = plot1(list(sensitivitylow,sen_low_ws,sen_low_abdo,sen_low_muco,sen_low_letha),'low')
lowls2 = plot2(list(sensitivitylow,sen_low_age,sen_low_gender),'low')
mediumls1 = plot1(list(sensitivitymed,sen_medium_ws,sen_medium_abdo,sen_medium_muco,sen_medium_letha),'medium')
mediumls2 = plot2(list(sensitivitymed,sen_medium_age,sen_medium_gender),'medium')
highls1 = plot1(list(sensitivityhigh,sen_high_ws,sen_high_abdo,sen_high_muco,sen_high_letha),'high')
highls2 = plot2(list(sensitivityhigh,sen_high_age,sen_high_gender),'high')


row1 = wrap_elements(panel=textGrob(expression(bold('Fluid')),gp = gpar(col = "black", fontsize = 12,face='bold')))
row2 = wrap_elements(panel=textGrob(expression(bold('Low Volume')),gp = gpar(col = "black", fontsize = 12,face='bold')))
row3 = wrap_elements(panel=textGrob(expression(bold('Medium Volume')),gp = gpar(col = "black", fontsize = 12,face='bold')))
row4 = wrap_elements(panel=textGrob(expression(bold('High Volume')),gp = gpar(col = "black", fontsize = 12,face='bold')))
p1 = (row1 + fluidls1$main$result_plot[[1]] + fluidls1$ws$result_plot[[1]] +
        fluidls1$ws$result_plot[[2]] +
        fluidls1$abdo$result_plot[[1]] +
        fluidls1$abdo$result_plot[[2]] + fluidls1$muco$result_plot[[1]] +
        fluidls1$muco$result_plot[[2]] + fluidls1$letha$result_plot[[1]] + 
        fluidls1$letha$result_plot[[2]] + 
        row2 + lowls1$main$result_plot[[1]] + lowls1$ws$result_plot[[1]] +
        lowls1$ws$result_plot[[2]] +
        lowls1$abdo$result_plot[[1]] +
        lowls1$abdo$result_plot[[2]] + lowls1$muco$result_plot[[1]] +
        lowls1$muco$result_plot[[2]] + lowls1$letha$result_plot[[1]] + 
        lowls1$letha$result_plot[[2]] + 
        row3 + mediumls1$main$result_plot[[1]] + 
        mediumls1$ws$result_plot[[1]] +
        mediumls1$ws$result_plot[[2]] +
        mediumls1$abdo$result_plot[[1]] +
        mediumls1$abdo$result_plot[[2]] + mediumls1$muco$result_plot[[1]] +
        mediumls1$muco$result_plot[[2]] + mediumls1$letha$result_plot[[1]] +
        mediumls1$letha$result_plot[[2]] +
        row4 + highls1$main$result_plot[[1]] + 
        highls1$ws$result_plot[[1]] +
        highls1$ws$result_plot[[2]] +
        highls1$abdo$result_plot[[1]] + 
        highls1$abdo$result_plot[[2]] + highls1$muco$result_plot[[1]] +
        highls1$muco$result_plot[[2]] + highls1$letha$result_plot[[1]] + 
        highls1$letha$result_plot[[2]] + 
        plot_layout(ncol=10,widths=rep(4.5,5,5,5,5,5,5,5,5,5),heights = rep(5,4),guides='collect'))+
  plot_annotation(
    title = element_text('KM Survival Plots (1)',size = 14,face='bold'),
    theme = theme(plot.margin=grid::unit(c(1,0.5,0.5,0.5),'cm')))&
  theme(legend.position='bottom',
        legend.direction = 'horizontal',
        legend.box = 'horizontal')
ggsave('Research Materials/fluid/output/coxsummary/survplot1.pdf',p1,dpi=2000,width=28,height=16)
ggsave('Research Materials/fluid/output/coxsummary/survplot1.png',p1,dpi=1100,width=28,height=16)

p2 = row1 + fluidls2$main$result_plot[[1]] + fluidls2$age$result_plot[[1]] + 
  fluidls2$age$result_plot[[2]] + fluidls2$gender$result_plot[[1]] + 
  fluidls2$gender$result_plot[[2]] +  
  row2 + lowls2$main$result_plot[[1]] + lowls2$age$result_plot[[1]] + 
  lowls2$age$result_plot[[2]] + lowls2$gender$result_plot[[1]] + 
  lowls2$gender$result_plot[[2]] +
  row3 + mediumls2$main$result_plot[[1]] +
  mediumls2$age$result_plot[[1]] + mediumls2$age$result_plot[[2]] + 
  mediumls2$gender$result_plot[[1]] + mediumls2$gender$result_plot[[2]] +
  row4 + highls2$main$result_plot[[1]] + highls2$age$result_plot[[1]] + 
  highls2$age$result_plot[[2]] + highls2$gender$result_plot[[1]] + 
  highls2$gender$result_plot[[2]] + 
  plot_layout(ncol=6,widths=c(4.5,5,5,5,5,5),heights = rep(5,4),guides='collect')+
  plot_annotation(
    title = element_text('KM Survival Plots (2)',size = 14,face='bold'),
    theme = theme(plot.margin=grid::unit(c(1,0.5,0.5,0.5),'cm')))&
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.box = 'horizontal')
ggsave('Research Materials/fluid/output/coxsummary/survplot2.pdf',p2,dpi=600,width=20,height=16)
ggsave('Research Materials/fluid/output/coxsummary/survplot2.png',p2,dpi=600,width=20,height=16)


p3 = (row1 + fluidls1$main$result_plot2[[1]] + fluidls1$ws$result_plot2[[1]] +
        fluidls1$ws$result_plot2[[2]] +
        fluidls1$abdo$result_plot2[[1]] +
        fluidls1$abdo$result_plot2[[2]] + fluidls1$muco$result_plot2[[1]] +
        fluidls1$muco$result_plot2[[2]] + fluidls1$letha$result_plot2[[1]] + 
        fluidls1$letha$result_plot2[[2]] + 
        row2 + lowls1$main$result_plot2[[1]] + lowls1$ws$result_plot2[[1]] +
        lowls1$ws$result_plot2[[2]] +
        lowls1$abdo$result_plot2[[1]] +
        lowls1$abdo$result_plot2[[2]] + lowls1$muco$result_plot2[[1]] +
        lowls1$muco$result_plot2[[2]] + lowls1$letha$result_plot2[[1]] + 
        lowls1$letha$result_plot2[[2]] + 
        row3 + mediumls1$main$result_plot2[[1]] + 
        mediumls1$ws$result_plot2[[1]] +
        mediumls1$ws$result_plot2[[2]] +
        mediumls1$abdo$result_plot2[[1]] +
        mediumls1$abdo$result_plot2[[2]] + mediumls1$muco$result_plot2[[1]] +
        mediumls1$muco$result_plot2[[2]] + mediumls1$letha$result_plot2[[1]] +
        mediumls1$letha$result_plot2[[2]] +
        row4 + highls1$main$result_plot2[[1]] + 
        highls1$ws$result_plot2[[1]] +
        highls1$ws$result_plot2[[2]] +
        highls1$abdo$result_plot2[[1]] + 
        highls1$abdo$result_plot2[[2]] + highls1$muco$result_plot2[[1]] +
        highls1$muco$result_plot2[[2]] + highls1$letha$result_plot2[[1]] + 
        highls1$letha$result_plot2[[2]] + 
        plot_layout(ncol=10,widths=rep(4.5,5,5,5,5,5,5,5,5,5),heights = rep(5,4),guides='collect'))+
  plot_annotation(
    theme = theme(plot.margin=grid::unit(c(1,0.5,0.5,0.5),'cm')))&
  theme(legend.position='bottom',
        legend.direction = 'horizontal',
        legend.box = 'horizontal')
ggsave('Research Materials/fluid/output/coxsummary/riskplot1.pdf',p3,dpi=2000,width=28,height=16)
ggsave('Research Materials/fluid/output/coxsummary/riskplot1.png',p3,dpi=1100,width=28,height=16)

p4 = row1 + fluidls2$main$result_plot2[[1]] + fluidls2$age$result_plot2[[1]] + 
  fluidls2$age$result_plot2[[2]] + fluidls2$gender$result_plot2[[1]] + 
  fluidls2$gender$result_plot2[[2]] +  
  row2 + lowls2$main$result_plot2[[1]] + lowls2$age$result_plot2[[1]] + 
  lowls2$age$result_plot2[[2]] + lowls2$gender$result_plot2[[1]] + 
  lowls2$gender$result_plot2[[2]] +  
  row3 + mediumls2$main$result_plot2[[1]] +
  mediumls2$age$result_plot2[[1]] + mediumls2$age$result_plot2[[2]] + 
  mediumls2$gender$result_plot2[[1]] + mediumls2$gender$result_plot2[[2]] +
  row4 + highls2$main$result_plot2[[1]] + highls2$age$result_plot2[[1]] + 
  highls2$age$result_plot2[[2]] + highls2$gender$result_plot2[[1]] + 
  highls2$gender$result_plot2[[2]] + 
  plot_layout(ncol=6,widths=c(5.5,5,5,5,5,5),heights = rep(5,4),guides='collect')+
  plot_annotation(
    theme = theme(plot.margin=grid::unit(c(1,0.5,0.5,0.5),'cm')))&
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.box = 'horizontal')
ggsave('Research Materials/fluid/output/coxsummary/riskplot2.pdf',p4,dpi=600,width=20,height=16)
ggsave('Research Materials/fluid/output/coxsummary/riskplot2.png',p4,dpi=600,width=20,height=16)



