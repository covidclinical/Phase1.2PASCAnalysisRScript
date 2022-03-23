phecode_prevalence_meta_quarter = function(phecode.res,
                              dailycount,
                              site.sel=NULL,
                              country=NULL,
                              cohort,
                              time.window,
                              qt,
                              site.register,
                              phecode.des){
  
  expit = function(mm){exp(mm)/(exp(mm)+1)}
  
  if(is.null(site.sel)){
    if(country=="ALL"){site.sel=site.register$SiteID}else{site.sel=site.register$SiteID[site.register$Country==country]}
  } else{country=site.sel}
  
  dailycount=dailycount[grepl(paste0(cohort),dailycount[,"cohort"])&grepl(paste(qt,collapse="|"),dailycount[,"cohort"])&dailycount[,"siteid"]%in%site.sel, ]
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
  
  ct.keep = unique(count$cohort)
  ct.keep = ct.keep[order(ct.keep)]
  dat.code=phecode.res[ct.keep]
    
    # random effects meta-analysis 
    # remove all 0's and NA's first! 
    dat=NULL
    for(cc in 1:length(ct.keep)){
      dat.junk=dat.code[[ct.keep[cc]]]
      dat.junk=dat.junk[site.sel]
      dat.junk=do.call(rbind,dat.junk); rownames(dat.junk)=c()
      dat=rbind(dat,dat.junk)
    }
  
    dat=dat[,c("siteid","cohort","concept_code",time.window)]; dat=dat[!is.na(dat[,time.window]),]; dat=dat[dat[,time.window]!=0,]  
    dat[,time.window]=as.numeric(dat[,time.window]); dat=dat[!is.na(dat[,time.window]),]
    
    code.meta=table(as.character(dat$concept_code))
    code.meta=names(code.meta)[code.meta>5]
   
    res=NULL
    for(code in code.meta){
      tryCatch({
        
        junk.cohort=filter(dat,concept_code==code)
        junk.cohort$sc=paste0(junk.cohort$siteid,junk.cohort$cohort)
        junk.count=count; junk.count$sc=paste0(junk.count$siteid,junk.count$cohort)
        junk.count = dplyr::filter(junk.count,sc %in% junk.cohort$sc)
        junk.cohort=junk.cohort[order(junk.cohort$sc),]; junk.count=junk.count[order(junk.count$sc),]
        
        junk.prop = escalc(xi=junk.cohort[,time.window],
                           ni=junk.count[,4],
                           measure="PLO")
        
        rma.junk=rma(yi=junk.prop$yi,
                     vi=junk.prop$vi,
                     method="DL",
                     drop00=TRUE)
        
        res = rbind.data.frame(res,
                               cbind.data.frame("country"=country,
                                                "cohort"=paste0(cohort,"_",paste(qt,collapse="")),
                                                "time.window"=time.window,
                                                "phecode"=code,
                                                "p"=expit(rma.junk$b),
                                                "ci.95L"=expit(rma.junk$ci.lb),
                                                "ci.95U"=expit(rma.junk$ci.ub),
                                                "x"=sum(junk.cohort[,time.window],na.rm=TRUE),
                                                "n"=sum(junk.count[,4],na.rm=TRUE),
                                                "description"=unique(phecode.des$description[phecode.des$phecode==code])))
        
        
      },error=function(e){NA})
  
    rownames(res)=c()
    return(res)
    
    
  }
}