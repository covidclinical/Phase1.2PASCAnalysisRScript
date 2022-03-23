phecode_prevalence = function(phecode.res,
                              dailycount,
                              site.sel=NULL,
                              country=NULL,
                              cohort,
                              time.window,
                              site.register,
                              phecode.des){
  
    expit = function(mm){exp(mm)/(exp(mm)+1)}
  
    if(is.null(site.sel)){
    if(country=="ALL"){site.sel=site.register$SiteID}else{site.sel=site.register$SiteID[site.register$Country==country]}
    } else{country=site.sel}
    
    dailycount=dailycount[dailycount[,"cohort"]==cohort&dailycount[,"siteid"]%in%site.sel,]
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
    rownames(count)=count$siteid
    dat.code=phecode.res[[cohort]]
   # random effects meta-analysis 
     # remove all 0's and NA's first! 
      dat.code=dat.code[site.sel]
      dat.code=do.call(rbind,dat.code); rownames(dat.code)=c()
      dat.code=dat.code[,c("siteid","concept_code",time.window)]; dat.code=dat.code[!is.na(dat.code[,time.window]),]; dat.code=dat.code[dat.code[,time.window]!=0,]  
      dat.code[,time.window]=as.numeric(dat.code[,time.window])
      code.meta=table(as.character(dat.code$concept_code))
      code.meta=names(code.meta)[code.meta>2]
      res=NULL
      for(code in code.meta){
        tryCatch({
        junk = dat.code[dat.code[,"concept_code"]%in%code,]
        junk.count=count; junk.count=junk.count[as.character(junk$siteid),]
        junk.prop = escalc(xi=junk[,time.window],
                           ni=junk.count[,4],
                           measure="PLO")
        rma.junk=rma(yi=junk.prop$yi,
                     vi=junk.prop$vi,
                     method="DL",
                     drop00=TRUE)
        res = rbind.data.frame(res,
                               cbind.data.frame("country"=country,
                                                "cohort"=cohort,
                                                "time.window"=time.window,
                                                "phecode"=code,
                                                "p"=expit(rma.junk$b),
                                                "ci.95L"=expit(rma.junk$ci.lb),
                                                "ci.95U"=expit(rma.junk$ci.ub),
                                                "x"=sum(junk[,time.window],na.rm=TRUE),
                                                "n"=sum(junk.count[,4],na.rm=TRUE),
                                                "description"=unique(phecode.des$description[phecode.des$phecode==code])))
        
        
        },error=function(e){NA})
      
      rownames(res)=c()
      return(res)
      
      
    }
}