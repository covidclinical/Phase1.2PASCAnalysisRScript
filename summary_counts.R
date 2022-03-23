rm(list=ls())
library(tidyverse);library(gplots);library(metafor);library(PheWAS);library(DescTools);library(epitools)
dir1 = "xxx"
dir.input = "xxx"
dir.share = "xxx"

clinical.course = readRDS(paste0(dir.input,"clinical_course.RDS"))
race.4ce = readRDS(paste0(dir.input,"race_4ce.RDS"))
diag.proc.med = readRDS(paste0(dir.input,"diag_proc_med.RDS"))
daily.count = readRDS(paste0(dir.input,"daily_count.RDS"))
age.sex = readRDS(paste0(dir.input,"age_sex.RDS"))
load(paste0(dir.input,"icd_3digit_to_phecode.Rda"))
load(paste0(dir.input,"icd_3digit_to_phecode_fixed.Rda"))

### read data
site.register=data.frame(apply(read.csv(paste0(dir1, "input_site.csv"))[,c("SiteID","Country", "Obfuscation")], 2, toupper))
y.scale.all=c("original", "log")
is.fakeID=F

sapply(list.files(pattern="[.]R$", 
                  path="xxx", 
                  full.names=TRUE), source)

##########################################################################################################################

###################################################################
##########      Generate Summary Counts Object        #############
###################################################################

site.sel=NULL
country="ALL"
if(is.null(site.sel)){
  if(country=="ALL"){site.sel=site.register$SiteID}else{site.sel=site.register$SiteID[site.register$Country==country]}
} 
dailycount=daily.count
dailycount=dailycount[dailycount[,"siteid"]%in%site.sel,]

count=NULL
for(ss in site.sel){
  junk=dailycount[dailycount$siteid==ss,]
  date.keep = max(as.Date(junk$calendar_date))
  count = rbind.data.frame(count,
                           dailycount[dailycount$siteid==ss&dailycount$calendar_date==date.keep,])
}

count$country=unlist(lapply(count$siteid,FUN=function(ll){
  return(site.register$Country[site.register$SiteID==ll])
}))
count = count[,-which(colnames(count)=="calendar_date")]
write.csv(count,file=paste0(dir.share,"summary_counts.csv"),row.names = FALSE)


###################################################################
##########           Demographics Analysis            #############
###################################################################

cohorts = unique(diag.proc.med$cohort)
cohorts = cohorts[grepl("Pos|Neg",cohorts)]
cohorts = cohorts[!grepl("21Q2|21Q3|21Q4",cohorts)]
severity = c("pts_all","pts_ever_severe","pts_never_severe")
res=NULL
for(cc in cohorts){
  for(ss in severity){
    if(!grepl("PosAdm",cc)){
      site.register1=site.register[-which(site.register$SiteID%in%c("UKFR","ICSM","UMICH","NUH","POLIMI")),]
    }else{site.register1=site.register}
    tmp = demographics_kern(agesex=age.sex,
                                         dailycount=daily.count,
                                         country="ALL",
                                         cohorts=cc,
                                         sev=ss,
                                         site.register=site.register1)
      res=rbind.data.frame(res,tmp)
  }
}
   
for(cc in c("PosAdm","PosNotAdm","NegAdm","NegNotAdm")){
  for(ss in severity){
    if(!grepl("PosAdm",cc)){
      site.register1=site.register[-which(site.register$SiteID%in%c("UKFR","ICSM","UMICH","NUH","POLIMI")),]
    }else{site.register1=site.register}
    tmp = demographics_kern(agesex=age.sex,
                            dailycount=daily.count,
                            country="ALL",
                            cohorts=cohorts[grepl(cc,cohorts)],
                            sev=ss,
                            site.register=site.register1)
    res=rbind.data.frame(res,tmp)
  }
}  

for(cc in cohorts){
  for(ss in severity){
    if(!grepl("PosAdm",cc)){
      site.register1=site.register[-which(site.register$SiteID%in%c("UKFR","ICSM","UMICH","NUH","POLIMI")),]
    }else{site.register1=site.register}
    tmp = race_kern (race4ce=race.4ce,
                            dailycount=daily.count,
                            country="USA",
                            cohorts=cc,
                            sev=ss,
                            site.register=site.register1)
    res=rbind.data.frame(res,tmp)
  }
}

for(cc in c("PosAdm","PosNotAdm","NegAdm","NegNotAdm")){
  for(ss in severity){
    if(!grepl("PosAdm",cc)){
      site.register1=site.register[-which(site.register$SiteID%in%c("UKFR","ICSM","UMICH","NUH","POLIMI")),]
    }else{site.register1=site.register}
    tmp = race_kern(race4ce=race.4ce,
                            dailycount=daily.count,
                            country="USA",
                            cohorts=cohorts[grepl(cc,cohorts)],
                            sev=ss,
                            site.register=site.register1)
    res=rbind.data.frame(res,tmp)
  }
}  

write.csv(res,file=paste0(dir.share,"demographic_prev.csv"),row.names = FALSE)









