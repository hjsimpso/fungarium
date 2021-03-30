#' @import data.table
#'
#helper functions for getting collector bias in enrichment function
bias_inner <- function (rec_v, freq_type, by) {
  if(length(rec_v)==0){#check if taxon has any records in reference dataset ("data_type") supplied; most useful when checking for records in the trait dataset, which will not necessarily include all taxa in input "data"
    out_loop <- c(0,0)
  }else if(length(rec_v)==1){#one record in reference dataset
    out_loop <- c(1,1)
  }else{ #more than one record in the reference dataset
    rec <- rec_v[rec_v!=""]
    b_diff <- length(rec_v)-length(rec) #find the number of records with blank collector info
    rec_numb <- lapply(unique(rec), function(x) which(x==rec)) #get the row numbers that match each unique collector group string
    rec <- list(rec_u=unique(rec),
                rec_numb=rec_numb,
                rec_count=sapply(rec_numb, length))#count the number of matched rows

    if (length(rec$rec_numb)==0){#no records left after removing records with blank collector info
      out_loop <- c(1,1)#all "blanks": treated as one group
    }else if (length(rec$rec_numb)==1){#only one unique collector group string is non-blank
      out_loop <- c(max(b_diff, rec$rec_count)/length(rec_v),ifelse(b_diff>0, 2, 1)) #two groups if there were blanks, one group if there were no blanks
    }else{
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
       out_loop <- c(max(c(max(count_assoc), b_diff))/length(rec_v),
                      length(unique(sapply(groups_total, paste, collapse=" ")))+ifelse(b_diff>0,1,0))
    }
  }
  return(out_loop)
}


bias_outer <- function(data, data_type, freq_type,by, cores, low_elem){
  setkeyv(data_type,by)
  rec_v <- lapply(data[[by]], function(x){
    rec_df <- data_type[list(x)];
    rec_v <- str_clean(paste(rec_df$recordedBy, rec_df$associatedCollectors, sep = " "),periods=" ", letter_thresh = 2);
    }) #subset records for taxon
  out_loop <- maxjobs.mclapply(rec_v, function(x){#use multicore processing to process each variable element (e.g. process multiple taxa simultaneously on different cores)
    bias_inner(rec_v=x, freq_type=freq_type,by=by)
    }, cores = cores, low_elem=low_elem, freq_type = freq_type)

  out_loop <- data.table::rbindlist(lapply(out_loop, as.data.frame.list), use.names = F)#add out
  return(out_loop)
}

get_coll_bias <- function(data, all_rec, trait_rec, by="new_full_name", cores){
  #assign bias metrics using all records dataset
  data <- data[order(data[["freq"]]),]#order by descending freq; helps speed up processing if using parallel computing
  out1.1 <- data.frame(x=rep(1, nrow(data[data$freq==1,])), y=rep(1, nrow(data[data$freq==1,])))#assign bias values for elements with one record; easy assignment: 1,1
  low_elem <- nrow(out1.1)#total number of variable elements with just 1 trait record
  out1.2 <- bias_outer(data[data$freq!=1], data_type = all_rec, freq_type = "freq", by=by, cores=cores, low_elem=low_elem)#assign bias values by analyzing collectors in records
  if(nrow(out1.2)>0){
    colnames(out1.2) <- c("x","y")
    out1 <- rbind(out1.1, out1.2)#combine outputs
  }else{
    out1 <- out1.1
  }
  data <- cbind(data, out1) #append bias values to input counts data

  #assign bias metrics using trait records dataset
  data <- data[order(data$trait_freq),]#order by descending fire freq; helps speed up processing if using parallel computing
  out2.0 <- data.frame(x=rep(0, nrow(data[data$trait_freq==0,])), y=rep(0, nrow(data[data$trait_freq==0,])))#assign bias values for elements with zero trait records; easy assignment: 0,0
  out2.1 <- data.frame(x=rep(1, nrow(data[data$trait_freq==1,])), y=rep(1, nrow(data[data$trait_freq==1,])))#assign bias values for elements with one trait record; easy assignment: 1,1
  out2 <- rbind(out2.0, out2.1)#combine outputs
  low_elem <- nrow(out2) #total number of variable elements with 0 or 1 trait records
  out2.2 <- bias_outer(data[!data$trait_freq%in%c(0,1),], data_type = trait_rec, freq_type = "trait_freq", by=by, cores=cores, low_elem = low_elem)#assign bias values by analyzing collectors in trait records
  if(nrow(out2.2)>0){
    colnames(out2.2) <- c("x","y")
    out2 <- rbind(out2, out2.2)#combine outputs
  }else{
    out2 <- out2
  }
  data <- cbind(data, out2)#append trait bias values to input counts data
  colnames(data)[(length(data[1,])-3):length(data[1,])] <- c("max_bias", "coll_groups", "max_bias_t", "coll_groups_t")
  data <- data[order(data[[by]]),]
  return(data)
}

maxjobs.mclapply <- function(X, FUN, cores, low_elem, freq_type){
  N <- length(X)
  i.list <- parallel::splitIndices(N, N/cores)
  result.list <- list()
  for(i in seq_along(i.list)){
    i.vec <- i.list[[i]]
    result.list[i.vec] <- parallel::mclapply(X[i.vec], FUN, mc.cores=cores)
    #print status message
    cat(paste(ifelse(freq_type=="freq","All records bias analysis. ", "Trait records bias analysis. "),round(((i*cores)+low_elem)/(N+low_elem)*100), "% of elements processed.", sep=""), "\r")
  }
  return(result.list)
}
