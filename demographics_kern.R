demographics_kern = function(agesex,
                             dailycount,
                             country="ALL",
                             cohorts,
                             sev="pts_all",
                             site.register){
  
  expit = function(mm){mm=as.numeric(mm);return(exp(mm)/(exp(mm)+1))}
  if(country=="ALL"){site.sel=site.register$SiteID
  } else{site.sel=site.register$SiteID[site.register$Country==country]}
  dailycount=filter(dailycount, siteid %in% site.sel, cohort %in% cohorts)
  count=NULL
  for(ss in site.sel){
    junk=dailycount[dailycount$siteid==ss,]
    date.keep = max(as.Date(junk$calendar_date))
    count = rbind.data.frame(count,
                             dailycount[dailycount$siteid==ss&dailycount$calendar_date==date.keep,])
  }
  
  if(grepl("pts_all",sev,fixed = T)){count=count[,1:4]
  }else if(grepl("pts_ever_severe",sev,fixed = T)){count=count[,c(1:3,7)]
  }else if(grepl("pts_never_severe",sev,fixed = T)){count=count[,c(1:3,14)]}
  count$sc=paste0(count$siteid,count$cohort)
  count=count[order(count$sc),]
  rownames(count)=c()
  
  agesex=filter(agesex,siteid%in%site.sel,cohort %in% cohorts,mean_age>18)
  agesex$sc=paste0(agesex$siteid,agesex$cohort)
  agesex=merge(agesex,count,by="sc",all.x=TRUE)
  age=filter(agesex,sex=="all",age_group!="all")
  sex=filter(agesex,sex!="all",age_group=="all")
  
  ### for age groups first
  res=NULL
  for(aa in unique(age$age_group)){
    tryCatch({
    junk=filter(age,age_group==aa)
    junk.plo=escalc(xi=junk[,sev],
                     ni=junk[,colnames(count)[4]],
                     measure="PLO")
    rma.res = rma(yi=junk.plo$yi,
                  vi=junk.plo$vi,
                  method="DL",
                  drop00=TRUE)
    res=rbind.data.frame(res,
                         cbind.data.frame("country"=country,
                                          "cohort"=paste(cohorts,collapse = ''),
                                          "setting"=sev,
                                          "subgroup"=aa,
                                          "p"=expit(rma.res$b),
                                          "se"=rma.res$se,
                                          "ci.95L"=expit(rma.res$ci.lb),
                                          "ci.95U"=expit(rma.res$ci.ub)))
       },error=function(e){NA})
  }
  
  for(ss in unique(sex$sex)){
    tryCatch({
    junk=filter(sex,sex==ss)
    junk.plo=escalc(xi=junk[,sev],
                    ni=junk[,colnames(count)[4]],
                    measure="PLO")
    rma.res = rma(yi=junk.plo$yi,
                  vi=junk.plo$vi,
                  method="DL",
                  drop00=TRUE)
    res=rbind.data.frame(res,
                         cbind.data.frame("country"=country,
                                          "cohort"=paste(cohorts,collapse = ''),
                                          "setting"=sev,
                                          "subgroup"=ss,
                                          "p"=expit(rma.res$b),
                                          "se"=rma.res$se,
                                          "ci.95L"=expit(rma.res$ci.lb),
                                          "ci.95U"=expit(rma.res$ci.ub)))
  
        },error=function(e){NA})
    }
  
  rownames(res)=c()
  return(res)
  
}