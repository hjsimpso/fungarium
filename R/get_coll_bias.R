#' @import data.table
#'
#helper functions for getting collector bias in enrichment function
bias_inner <- function (rec_v) {
  rec <- rec_v[rec_v!=""]
  b_diff <- length(rec_v)-length(rec) #find the number of records with blank collector info
  rec_numb <- lapply(unique(rec), function(x) which(x==rec)) #get the row numbers that match each unique collector group string
  rec <- list(rec_u=unique(rec),
              rec_numb=rec_numb,
              rec_count=sapply(rec_numb, length))#count the number of matched rows

  if (length(rec$rec_numb)==1){#only one unique collector group string is non-blank
    out <- c(max(b_diff, rec$rec_count)/length(rec_v),ifelse(b_diff>0, 2, 1)) #two groups if there were blanks, one group if there were no blanks
  }else{#more than one unique collector group string is non-blank
    words <- stringr::str_split(rec$rec_u,pattern=" ", simplify = F)#split strings into individual words
    count_assoc <- c()
    groups <- list()
    groups2 <- list()
    for (j in 1:length(words)){#loops for each unique collector string
      k_match <- data.table()
      for (k in 1:length(words[[j]])){#loops for each word in a unique collector string
        match <- sapply(words, function(x) !is.na(chmatch(words[[j]][k],x)), simplify = T)
        k_match <- rbind(k_match,data.table(match=match, rec_u_numb=c(1:length(words))))
      }
      k_match_d <- unique(k_match[k_match$match==T,]) #get unique matches; multiple words from one row might have matched with multiple words from another row; unique function works for data.table
      groups <- append(groups, list(k_match_d$rec_u_numb)) #make list of unique collector groups

    }
    #add secondarily associated collectors to collector groups (i.e. collector 1 is directly associated with collector 2 but not 3; however, collector 2 is directly associated with collector 3)
    groups_u <- list()
    for (m in 1:length(groups)){
      groups2 <- unique(unlist(groups[groups[[m]]]))

      if (isFALSE(identical(groups2, groups[[m]]))){
        ref <- ""
        while (isFALSE(identical(groups2, ref))){
          ref <- groups2
          groups2 <- unique(unlist(groups[groups2]))
        }
      }
      groups_u <- append(groups_u, list(groups2))
    }
    groups_total <- lapply(groups_u, function(x) sort(unlist(rec$rec_numb[x])))
    count_assoc <- sapply(groups_total, function(x) length(x))

    #calculate and output max_bias (0 to 1) and the number of collector groups (coll_groups) for each variable element (e.g. each taxon)
     out <- c(max(c(max(count_assoc), b_diff))/length(rec_v),
              length(unique(sapply(groups_total, paste, collapse=" ")))+ifelse(b_diff>0,1,0))
  }
  return(out)
}


get_coll_bias <- function(enrich_data, records_data,  freq_col, by, cores, status_feed, done){
  enrich_data <- enrich_data[order(enrich_data[[freq_col]]),]
  setkeyv(records_data,by)
  rec_v <- lapply(enrich_data[[by]], function(x){
    records_data[list(x)]$colls_cleaned
  }) #subset records for each unique element
  out <- maxjobs.mclapply(rec_v, FUN=bias_inner, cores=cores, done=done, freq_col=freq_col, status_feed=status_feed)#use multicore processing to process each variable element (e.g. process multiple taxa simultaneously on different cores)

  out <- data.table::rbindlist(lapply(out, as.data.frame.list), use.names = F)
  if(freq_col=="freq"){
    colnames(out) <- c("max_bias", "coll_groups")
  }else{
    colnames(out) <- c("max_bias_t", "coll_groups_t")
  }
  out <- cbind(enrich_data, out)
  setDF(out)
  return(out)
}

maxjobs.mclapply <- function(X, FUN, cores, done, freq_col, status_feed){
  N <- length(X)
  i.list <- parallel::splitIndices(N, N/cores)
  result.list <- list()
  for(i in seq_along(i.list)){
    i.vec <- i.list[[i]]
    result.list[i.vec] <- parallel::mclapply(X[i.vec], FUN, mc.cores=cores)
    #print status message
    if (status_feed){
      cat(paste(ifelse(freq_col=="freq","All records bias analysis. ", "Trait records bias analysis. "),round(((i*cores)+done)/(N+done)*100), "% of elements processed.", sep=""), "\r")
    }
  }
  return(result.list)
}
