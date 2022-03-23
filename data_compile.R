rm(list=ls())
library(tidyverse)
dir.input="xxx"
dir.output="xxx"

setwd(dir.input)
files=list.files()

tmp=files[grep("AgeSex", files, value=FALSE)]
age.sex=NULL
for(p in 1:length(tmp)){
  junk=read.csv(tmp[p]);colnames(junk)=tolower(colnames(junk))
  junk[which(junk[,"pts_all"]%in%c(-99,NA,Inf,-999)),"pts_all"]=NA;junk[which(junk[,"pts_ever_severe"]%in%c(-99,NA,Inf,-999)),"pts_ever_severe"]=NA
  junk[,"pts_never_severe"]=junk[,"pts_all"]-junk[,"pts_ever_severe"]
  junk[which((junk[,"pts_never_severe"]%in%c(-99,NA,Inf,-999))|(junk[,"pts_never_severe"]<0)),"pts_never_severe"]=NA
  age.sex=rbind(age.sex,junk)
}
saveRDS(age.sex,paste0(dir.output,"age_sex.RDS"))

tmp=files[grep("ClinicalCourse", files, value=FALSE)]
course=NULL
for(p in 1:length(tmp)){
  junk=read.csv(tmp[p]);colnames(junk)=tolower(colnames(junk))
  for(ll in 4:10){
    junk[which(junk[,ll]%in%c(-99,NA,Inf,-999)),ll]=NA
  }
  junk[,"pts_never_severe_in_hosp"]=junk[,"pts_all_in_hosp"]-junk[,"pts_ever_severe_in_hosp"]
  junk[which(junk[,"pts_never_severe_in_hosp"]<0),"pts_never_severe_in_hosp"]=NA
  course=rbind(course,junk)
  }
saveRDS(course,paste0(dir.output,"clinical_course.RDS"))

tmp=files[grep("DailyCounts", files, value=FALSE)]
dailycount=NULL
for(p in 1:length(tmp)){
  junk=read.csv(tmp[p]);colnames(junk)=tolower(colnames(junk))
  for(ll in 4:13){
    junk[which(junk[,ll]%in%c(-99,NA,Inf,-999)),ll]=NA
  }
  junk[,"cumulative_pts_never_severe"]=junk[,"cumulative_pts_all"]-junk[,"cumulative_pts_severe"]
  junk[which(junk[,"cumulative_pts_never_severe"]<0),"cumulative_pts_never_severe"]=NA
  dailycount=rbind(dailycount,junk)
  }
saveRDS(dailycount,paste0(dir.output,"daily_count.RDS"))

tmp=files[grep("DiagProcMed", files, value=FALSE)]
diagnosis=NULL
for(p in 1:length(tmp)){
  junk=read.csv(tmp[p]);colnames(junk)=tolower(colnames(junk))
  for(ll in 5:24){
    junk[which(junk[,ll]%in%c(-99,NA,Inf,-999)),ll]=NA
  }
  junk[,"pts_never_severe_before_adm"]=junk[,"pts_all_before_adm"]-junk[,"pts_ever_severe_before_adm"]
  junk[,"pts_never_severe_since_adm"]=junk[,"pts_all_since_adm"]-junk[,"pts_ever_severe_since_adm"]
  junk[,"pts_never_severe_dayn14ton1"]=junk[,"pts_all_dayn14ton1"]-junk[,"pts_ever_severe_dayn14ton1"]
  junk[,"pts_never_severe_day0to29"]=junk[,"pts_all_day0to29"]-junk[,"pts_ever_severe_day0to29"]
  junk[,"pts_never_severe_day30to89"]=junk[,"pts_all_day30to89"]-junk[,"pts_ever_severe_day30to89"]
  junk[,"pts_never_severe_day30plus"]=junk[,"pts_all_day30plus"]-junk[,"pts_ever_severe_day30plus"]
  junk[,"pts_never_severe_day90plus"]=junk[,"pts_all_day90plus"]-junk[,"pts_ever_severe_day90plus"]
  junk[,"pts_never_severe_1st_day0to29"]=junk[,"pts_all_1st_day0to29"]-junk[,"pts_ever_severe_1st_day0to29"]
  junk[,"pts_never_severe_1st_day30plus"]=junk[,"pts_all_1st_day30plus"]-junk[,"pts_ever_severe_1st_day30plus"]
  junk[,"pts_never_severe_1st_day90plus"]=junk[,"pts_all_1st_day90plus"]-junk[,"pts_ever_severe_1st_day90plus"]
  junk[,"pts_all_1st_day30to89"]=junk[,"pts_all_1st_day30plus"]-junk[,"pts_all_1st_day90plus"]
  junk[,"pts_ever_severe_1st_day30to89"]=junk[,"pts_ever_severe_1st_day30plus"]-junk[,"pts_ever_severe_1st_day90plus"]
  for(ll in 25:36){
    junk[which(junk[,ll]<0),ll]=NA
  }
  junk[,"pts_never_severe_1st_day30to89"]=junk[,"pts_all_1st_day30to89"]-junk[,"pts_ever_severe_1st_day30to89"]
  junk[which(junk[,"pts_never_severe_1st_day30to89"]<0),"pts_never_severe_1st_day30to89"]=NA
  diagnosis=rbind(diagnosis,junk)
}
saveRDS(diagnosis,paste0(dir.output,"diag_proc_med.RDS"))


tmp=files[grep("RaceBy4CECode", files, value=FALSE)]
race.4ce=NULL
for(p in 1:length(tmp)){
  junk=read.csv(tmp[p]);colnames(junk)=tolower(colnames(junk))
  for(ll in c(4,5)){
    junk[which(junk[,ll]%in%c(-99,NA,Inf,-999)),ll]=NA
  }
  junk[,"pts_never_severe"]=junk[,"pts_all"]-junk[,"pts_ever_severe"]
  junk[which(junk[,"pts_never_severe"]<0),"pts_never_severe"]=NA
  race.4ce=rbind(race.4ce,junk)
  }
saveRDS(race.4ce,paste0(dir.output,"race_4ce.RDS"))


#===============================================================================================================================





