phecode_rr_across_cohorts_meta_quarter = function(phecode.res,
                                                  dailycount,
                                                  site.sel=NULL,
                                                  country,
                                                  qt=,
                                                  cohort1,
                                                  cohort2,
                                                  time.window,
                                                  site.register,
                                                  phecode.des){
  if(is.null(site.sel)){
  if(country=="ALL"){site.sel=site.register$SiteID}else{site.sel=site.register$SiteID[site.register$Country==country]}
  } else{country=site.sel}
  
  dailycount=dailycount[grepl(paste0(cohort1,"|",cohort2),dailycount[,"cohort"])&grepl(paste(qt,collapse = "|"),dailycount[,"cohort"])&dailycount[,"siteid"]%in%site.sel,]
  count=NULL
  for(ss in site.sel){
    junk=dailycount[dailycount$siteid==ss,]
    date.keep = max(as.Date(junk$calendar_date))
    count = rbind.data.frame(count,
                             dailycount[dailycount$siteid==ss&dailycount$calendar_date==date.keep,])
  }
  
  if(grepl("pts_all",time.window,fixed = T)){count=count[,1:4]
  }else if(grepl("pts_ever_severe",time.window,fixed = T)){count=count[,c(1:3,7)]
  }else if(grepl("pts_never_severe",time.window,fixed = T)){count=count[,c(1:3,14)]}
  
  count$sc=paste0(count$siteid,count$cohort)
  ct.keep = unique(count$cohort)
  dat.code=phecode.res[ct.keep]
  
    # random effects meta-analysis 
    # remove all 0's and NA's first! 
    dat.cohort=NULL
    for(cc in 1:length(ct.keep)){
      dat.junk=dat.code[[ct.keep[cc]]]
      dat.junk=dat.junk[site.sel]
      dat.junk=do.call(rbind,dat.junk); rownames(dat.junk)=c()
      dat.cohort=rbind(dat.cohort,dat.junk)
    }
    dat.cohort=dat.cohort[,c("siteid","cohort","concept_code",time.window)]; dat.cohort=dat.cohort[!is.na(dat.cohort[,time.window]),]; dat.cohort=dat.cohort[dat.cohort[,time.window]!=0,]  
    dat.cohort[,time.window]=as.numeric(dat.cohort[,time.window])
    dat.cohort=dat.cohort[!is.na(dat.cohort[,4]),]
    qq=merge(dat.cohort,count,by=c("siteid","cohort"),all=F)
    colnames(qq)[c(4,6)]=c("count","n")
    qq=qq %>%
      filter(grepl("Pos",cohort)) %>%
      group_by(siteid,concept_code) %>%
      summarize("count"=sum(count),"n"=sum(n)) %>%
      mutate("prop"=count/n)
    qq=qq %>%
      group_by(concept_code) %>%
      summarize("mean"=mean(prop))
    if(cohort1=="PosAdm"){
    code.meta=unique(qq$concept_code[qq$mean>0.0075])
    } else if(cohort1=="PosNotAdm"){code.meta=unique(qq$concept_code[qq$mean>0.0015])}
    
    res.se=NULL
    res=NULL
    for(code in code.meta){
      tryCatch({
      junk.cohort=filter(dat.cohort,concept_code==code)
      junk.cohort$sc=paste0(junk.cohort$siteid,junk.cohort$cohort)
      id.rm=NULL
      for(hh in unique(junk.cohort$siteid)){
        for(nn in qt){
          xx = filter(junk.cohort,siteid==hh,grepl(nn,cohort))
          if(nrow(xx)==1){
            id.rm=c(id.rm,which(junk.cohort$siteid==hh&grepl(nn,junk.cohort$cohort)))
          }
        }
      }
      if(!is.null(id.rm)){junk.cohort=junk.cohort[-id.rm,]}
      junk.count = filter(count,sc %in% junk.cohort$sc)
      junk.cohort=junk.cohort[order(junk.cohort$sc),]; junk.count=junk.count[order(junk.count$sc),]

      if(nrow(junk.count)>20){
      ai=junk.cohort %>% filter(grepl(cohort1,sc)) 
      ai=ai[,time.window]
      bi=junk.count %>% filter(grepl(cohort1,sc))
      bi=bi[,colnames(count)[4]]-ai
      ci=junk.cohort %>% filter(grepl(cohort2,sc)) 
      ci=ci[,time.window]      
      di=junk.count %>% filter(grepl(cohort2,sc))
      di=di[,colnames(count)[4]]-ci
      junk.rr = escalc(ai=ai,
                       bi=bi,
                       ci=ci,
                       di=di,
                       measure="RR")
        rma.res = rma(yi=junk.rr$yi,
                      vi=junk.rr$vi,
                      method="DL",
                      drop00=TRUE)
       
        res = rbind.data.frame(res,
                               cbind.data.frame("country"=country,
                                                "cohort1"=paste0(cohort1,"_",paste(qt,collapse='')),
                                                "cohort2.ref"=paste0(cohort2,"_",paste(qt,collapse='')),
                                                "time.window"=time.window,
                                                "phecode"=code,
                                                "rr"=exp(rma.res$b),
                                                "se"=rma.res$se,
                                                "ci.95L"=exp(rma.res$ci.lb),
                                                "ci.95U"=exp(rma.res$ci.ub),
                                                "pval"=rma.res$pval,
                                                "x"=NA,
                                                "n"=NA,
                                                "description"=unique(phecode.des$description[phecode.des$phecode==code])))

      }
      },error=function(e){NA})
    }
    
    rownames(res)=c()
    return(res)
    

}