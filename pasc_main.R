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
phecode.res = readRDS(paste0(dir.input,"phecode.res.RDS"))

### read data
site.register=data.frame(apply(read.csv(paste0(dir1, "input_site.csv"))[,c("SiteID","Country", "Obfuscation")], 2, toupper))
y.scale.all=c("original", "log")
is.fakeID=F

### source functions 
sapply(list.files(pattern="[.]R$", 
                  path="xxx", 
                  full.names=TRUE), source)

se = function(p,n) {return(sqrt((p*(1-p))/n)) }
var = function(p,n) {return((p*(1-p))/n)}
logit.p = function(p){log(p/(1-p))}
logit.p.var = function(p,n){(1/(n*p)+1/(n*(1-p)))}
expit = function(mm){exp(mm)/(exp(mm)+1)}
wald.ci = function(p,se){pp = 1.96*se;return(cbind(p-pp,p+pp))}

metaFUN.randomeffect.HZ=function(st, yt, w0.FE){
  id.keep=which(is.na(yt)!=1 & is.na(st)!=1 & is.na(w0.FE)!=1)
  
  ## fixed effect at day t
  w0.FE=w0.FE[id.keep]
  st=st[id.keep]
  yt=yt[id.keep]
  mut.FE=c(yt%*%w0.FE/sum(w0.FE))
  
  ## random effect weight at day t
  df=length(yt)-1
  C=sum(w0.FE)-sum(w0.FE^2)/sum(w0.FE)
  Q=c(w0.FE%*%(yt-mut.FE)^2)
  v.between=(Q-df)/C
  #if(v.between<0){v.between=0}
  wt.RE=1/(st^2+v.between)
  
  ## random effect mu and se at day t
  mut.RE=c(yt%*%wt.RE/sum(wt.RE))
  vt.RE=1/sum(wt.RE)
  se.RE=sqrt(vt.RE)
  
  return(data.frame(mut.RE=mut.RE, 
                    se.RE=se.RE,
                    ci_95L=wald.ci(mut.RE,se.RE)[1],
                    ci_95U=wald.ci(mut.RE,se.RE)[2]))
}


##########################################################################################################################

          ###################################################################
          ################         Map ICD to PheCodes       ################
          ###################################################################

      phecode.res = icd_to_phecode(diagprocmed = diag.proc.med,
                                   mapping = icd.phecode.map,
                                   site.register = site.register)
      saveRDS(phecode.res,file=paste0(dir.input,"phecode.res.RDS"))
      

      #identify how many unique conditions we capture
      cohorts = unique(diag.proc.med$cohort)
      cohorts = cohorts[!grepl("2021Q4|2021Q3",cohorts)]
      codes=NULL
      for(nn in cohorts){
        tmp=do.call("rbind",phecode.res[[nn]])
        codes=append(codes,unique(tmp$concept_code))
      }
      length(unique(codes))
      
          ###################################################################
          ##############          Prevalence estimates        ###############
          ###################################################################

      prev.res = NULL
      cohorts = unique(diag.proc.med$cohort)
      cohorts = cohorts[!grepl("2021Q4|2021Q3",cohorts)]
      times = colnames(diag.proc.med)[5:37]
      
      for(cc in cohorts){
        print(cc)
        if(!grepl("PosAdm20",cc)){
        site.register1=site.register[-which(site.register$SiteID%in%c("UKFR","ICSM","UMICH","NUH","POLIMI","KUMC")),]
        }else{site.register1=site.register}
        for(tt in times){
                junk=phecode_prevalence(phecode.res,
                               dailycount=daily.count,
                               country="ALL",
                               cohort=cc,
                               time.window=tt,
                               site.sel=NULL,
                               site.register=site.register1,
                               phecode.des=icd.phecode.map)
                  prev.res = rbind.data.frame(prev.res,junk)
        }
      }
      rownames(prev.res)=c()
      write.csv(prev.res,file=paste0(dir.share,"meta_pasc_prevalence.csv"),row.names = FALSE)
      
      # across quarter function
      prev.res = NULL
      cohorts = c("PosAdm","PosNotAdm","NegAdm","NegNotAdm")
      times = colnames(diag.proc.med)[5:37]
      
      for(cc in cohorts){
        print(cc)
        if(!grepl("PosAdm",cc)){
          site.register1=site.register[-which(site.register$SiteID%in%c("UKFR","ICSM","UMICH","NUH","POLIMI","KUMC")),]
        }else{site.register1=site.register}
        for(tt in times){
          junk=phecode_prevalence_meta_quarter(phecode.res,
                                                dailycount=daily.count,
                                               site.sel=NULL,
                                                country="ALL",
                                                cohort=cc,
                                                time.window=tt,
                                                site.register=site.register1,
                                               qt=c("20Q1","20Q2","20Q3","20Q4","21Q1"),
                                                phecode.des=icd.phecode.map)
          prev.res = rbind.data.frame(prev.res,junk)
        }
      }
      rownames(prev.res)=c()
      write.csv(prev.res,file=paste0(dir.share,"meta_pasc_prevalence_all_quarters.csv"),row.names = FALSE)
 
          ###################################################################
          #########          PASC Relative risk b/w cohorts       ###########
          ###################################################################
      
      cohorts=rbind(c("PosAdm","NegAdm"),c("PosNotAdm","NegNotAdm"))
      times = c("pts_all_1st_day30plus",
                "pts_all_1st_day90plus",
                "pts_all_1st_day30to89")
      site.register1=site.register[-which(site.register$SiteID%in%c("UKFR","ICSM","UMICH","NUH","POLIMI")),]
      res=NULL
      for(io in 1:2){
        for(tt in times){
      tmp = phecode_rr_across_cohorts_meta_quarter(phecode.res,
                                                   dailycount=daily.count,
                                                   site.sel=NULL,
                                                   country="ALL",
                                                   qt=c("20Q1","20Q2","20Q3","20Q4","21Q1"),
                                                   cohort1=cohorts[io,1],
                                                   cohort2=cohorts[io,2],
                                                   time.window=tt,
                                                   site.register=site.register1,
                                                   phecode.des=icd.phecode.map)
      
          res=rbind.data.frame(res,tmp)
        }
      }
      write.csv(res,file=paste0(dir.share,"meta_pasc_riskratio_across_cohorts_meta_quarter.csv"),row.names = FALSE)
      
