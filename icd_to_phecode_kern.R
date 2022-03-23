icd_to_phecode = function(diagprocmed,
                          mapping,
                          site.register){
  
      cohorts = unique(diagprocmed$cohort)
      sites = unique(diagprocmed$siteid)
      res = as.list(cohorts);names(res)=cohorts
      for(cc in cohorts){
        print(cc)
        site.level.ls = as.list(sites); names(site.level.ls) = sites
        
        for(ss in sites){
          
        dat.tmp=diagprocmed[diagprocmed[,"siteid"]==ss&diagprocmed[,"cohort"]==cc&diagprocmed[,"concept_type"]%in%c("DIAG-ICD10","DIAG-ICD9"),]
        dat.tmp.mapped = (apply(dat.tmp,MARGIN=1,FUN=function(mm){
              if(mm[3]=="DIAG-ICD10"){concept.type="DIAG-ICD10"}else{concept.type="DIAG-ICD9"}
              phecode.mapped = unique(mapping[mapping$concept_type==concept.type&mapping$concept_code%in%mm[4],"phecode"])
              phecode.mapped = as.character(phecode.mapped)
              if(length(phecode.mapped)>0){
              tmp=NULL
              for (code in phecode.mapped){
                mm.junk = mm; mm.junk[4] = code; mm.junk[3]="phecode"
                tmp = rbind.data.frame(tmp,
                                       mm.junk)
              }
              colnames(tmp)=colnames(dat.tmp); rownames(tmp)=c()
              return(tmp)
              }
        }))
        if(!is.null(dat.tmp.mapped)&is.list(dat.tmp.mapped)){
        dat = do.call(rbind.data.frame,dat.tmp.mapped); dat = data.frame(dat)
        colnames(dat)=colnames(dat.tmp); rownames(dat)=c()
        for(ll in 5:37){dat[,ll]=as.numeric(dat[,ll])}
        freq.phecode = table(dat[,"concept_code"])
        if(length(names(freq.phecode)[which(freq.phecode>1)])>0){
        for(qq in names(freq.phecode)[which(freq.phecode>1)]){
          tmp = dat[which(dat[,"concept_code"]==qq)[1],]
          tmp[5:37] = colSums(dat[which(dat[,"concept_code"]==qq),5:37],
                              na.rm = TRUE) 
          dat = dat[-which(dat[,"concept_code"]==qq),]
          dat = rbind.data.frame(dat, tmp)
         }
        }
        rownames(dat)=c()
        if(dim(dat)[1]>0){
        site.level.ls[[ss]]=dat
        }
             }
        }
           res[[cc]]=site.level.ls
      }
  
  
      return(res)
  
  
  
  
}