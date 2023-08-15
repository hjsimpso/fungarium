#' Visualize trait enrichment factors within a cladogram based on taxonomic classification
#'
#' Taxonomic classification data is used to create a cladogram, which is then annotated with
#' trait enrichment factors.
#'
#'
#' @param data           Enrichment table from \code{fungarium::enrichment} (i.e., data.frame containing taxonomic classification variables and enrichment values).
#' @param formula        Formula describing how the taxonomic variables (e.g. kingdom, phylum, class, etc.) in your enrichment table should be nested (e.g ~V1/V2/.../Vn). Default: ~new_kingdom/new_phylum/new_class/new_order/new_family/new_genus/new_species.
#' @param ladderize      Logical. Should the cladogram be ladderized? Default is TRUE. See \code{ape::ladderize}.
#' @param continuous     Logical. Type of node annotation? Default is "color". See \code{ggtree::ggtree}.
#' @param layout         Character string specifying the type of tree layout. Default is "circular". See \code{ggtree::ggtree}.
#' @param filter         Character vector specifying data set filtering parameters. Default is NULL. To select taxa with the highest or lowest values for a certain variable (e.g., trait_ratio, trait_freq, freq) use "high-100-trait_ratio", "high-150-trait_freq", "low-200-freq", etc. To set a value threshold filter (i.e., filter out taxa with less than or more than a certain value for trait_ratio, trait_freq, etc.) use "trait_freq>=5", "freq>20", etc. Full vector example: c("freq>=10","high-150-trait_ratio"). This example will filter out all taxa with less than 10 trait_freq and then select the top 150 taxa with the highest trait_ratio values. Note that inequality filters are used prior to selecting "high" or "low" taxa.
#' @param node_color     Logical. If TRUE (the default), nodes are colored based on the values in trait_ratio.
#' @param node_all       Logical. If TRUE (the default), all taxa in the original input data (before filtering via the \code{filtering} parameter) are used for calculating node enrichment values. This becomes important if \code{filter} is non NULL.
#' @param node_calc      Character string defining method for calculating trait enrichment for nodes. If "mean" (the default) is selected, nodes values are calculated based on the mean trait enrichment value for all of the lowest level taxa in that group. If "add" (the default) is selected, nodes values are calculated by dividing the sum of all trait-relevant records by the sum of all records for the lowest level taxa in that group.
#' @param ...            Additional args passed to ggtree. See \code{ggtree::ggtree}.
#' @return           Returns a ggtree object.
#'
#' @details Cladogram produced using \code{ggtree::ggtree};
#' thus, all \code{ggtree} arguments are accepted (e.g., \code{geom_tiplab}, \code{geom_tippoint}, etc.).
#' Additional ggplot layers (e.g., \code{geom_bar}, \code{geom_point}, etc.) can be added to the tree as if it were a ggplot object.
#' @note Choose \code{filter} and \code{node_all} values carefully. These values control how enrichment
#' values are displayed for higher level taxa (i.e., nodes). If a \code{filter} condition is used and \code{node_all}
#' is set to TRUE, node enrichment factors will be calculated using all lowest rank taxa in the input data set, prior to filtering
#' (i.e., sum of trait records/sum of total records for all lowest rank taxa in these higher level groups).
#' If \code{node_all} is FALSE, only the lowest rank taxa that remain after filtering (which are the taxa that will be plotted as tree tips)
#' will be used to calculate node enrichment factors. The former method is likely to give a more accurate representation
#' of trait enrichment for higher level taxa, especially if your filtering parameters are removing a significant amount of lowest rank taxa.
#' Note that it may useful to do some data set filtering prior to calling this function. For example, it may be useful to
#' filter out taxa with high collector bias (i.e. high values for \code{max_bias} and \code{max_bias_t}; see \code{fungarium::enrichment}) metrics,
#' prior to using \code{trait_clado}, so that these biased taxa are not used in the calculations of enrichment for higher level taxa.
#' @references \enumerate{
#' \item Hunter J. Simpson & Jonathan S. Schilling (2021) Using aggregated field
#' collection data and the novel r package fungarium to investigate fungal fire
#' association, Mycologia, 113:4, 842-855, DOI: 10.1080/00275514.2021.1884816
#' }
#' @export
#'
#' @examples
#' library(fungarium)
#' library(ggtree)
#' library(ggplot2)
#'
#' #load sample enrichment data set
#' data(agaricales_enrich)
#'
#' #filter out taxa with high collector bias (optional)
#' agaricales_enrich <- agaricales_enrich[agaricales_enrich$max_bias<=0.75,]
#' agaricales_enrich <- agaricales_enrich[agaricales_enrich$max_bias_t<=0.75,]
#'
#' #filter out taxa with low total records ("freq")
#' agaricales_enrich <- agaricales_enrich[agaricales_enrich$freq>=3,]
#'
#' #make cladogram
#' trait_clado(data=agaricales_enrich, continuous="color",
#'             ladderize=TRUE, layout="circular", size=0.8,
#'             formula = ~new_order/new_family/new_genus/new_species,
#'             filter="high-300-trait_ratio", node_all = TRUE)+
#'   geom_tiplab2(color = "black", hjust = 0, offset = 0.1,
#'                size = 1.4, fontface = "italic") + #add species labels
#'   geom_tippoint(shape=20,
#'                 aes(color=trait_ratio, size=trait_freq),
#'                 alpha=0.75)+#add tree tips
#'   scale_color_gradientn(colours= c("cyan", "blue", "purple", "red", "orange"),
#'                         name = "Fire-associated records enrichment",
#'                         limits = c(0, round(max(agaricales_enrich$trait_ratio),2)),
#'                         guide = guide_colourbar(label.vjust = 0.6,
#'                                                 label.theme = element_text(size = 10,
#'                                                                            colour = "black",
#'                                                                            angle = 0),
#'                                                 title.position = "top",
#'                                                 nbin=100,
#'                                                 draw.ulim = FALSE,
#'                                                 draw.llim = FALSE,
#'                                                 barwidth = 15,
#'                                                 barheight = 0.5))+
#'   scale_size(name = "Fire-associated records",
#'              guide = guide_legend(keywidth = 2,
#'                                   keyheight = 1,
#'                                   label.position = "bottom",
#'                                   label.vjust = 0.6,
#'                                   label.theme = element_text(size = 10,
#'                                                              colour = "black",
#'                                                              angle = 0),
#'                                   title.position = "top")) +
#'   theme(plot.margin=margin(0,0,0,0),
#'         legend.title = element_text(size = 10, margin = margin(0,0,0,0)),
#'         legend.title.align = 0.5,
#'         legend.position = "bottom",
#'         legend.justification = "center",
#'         legend.margin = margin(0,0,0,0),
#'         plot.title = element_text(hjust = 0.5, margin=margin(0,0,0,0)))+
#'   xlim(c(-1, 4))

trait_clado <- function(data, formula=~new_kingdom/new_phylum/new_class/new_order/new_family/new_genus/new_species,
                        node_color=TRUE, filter=NULL, node_all=TRUE,
                        ladderize=TRUE, continuous="color", layout="circular", node_calc = "mean",
                        ...){
  #check for deprecated arguments
  if(exists("show", inherits = FALSE)){
    stop('"show" argument is deprecated. Please use "filter".')
  }
  if(exists("trait_col", inherits = FALSE)){
    warning('"trait_col" no longer needs to be specified. The standard field for enrichment values is now "trait_ratio"')
  }
  #check for dependencies
  if (!requireNamespace("ggtree", quietly = TRUE)) {
    stop("Please install the \"ggtree\" package.",
         call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install the \"ggplot2\" package.",
         call. = FALSE)
  }
  #check that input is enrichment table
  if (T%in%(!c("freq", "trait_freq", "trait_ratio")%in%colnames(data))){
    stop('Input data must be an enrichment table containing the fields "freq", "trait_freq", "trait_ratio". See fungarium::enrichment')
  }
  #check node_calc
  if (!node_calc %in% c("mean", "sum")){
    stop("Invalid value for 'node_calc'")
  }
  #convert trait numeric data from characters to numeric
  data$freq <- as.numeric(data$freq)
  data$trait_freq <- as.numeric(data$trait_freq)
  data$trait_ratio <- as.numeric(data$trait_ratio)

  #get taxonomic variables
  vars <- all.vars(formula)
  vars_check <- F%in%(vars%in%colnames(data))
  if (vars_check){
    stop("Invalid taxonomic variables supplied. Please list variables present in data.")
  }

  #coerce data if not data.table
  if (!is.data.frame(data)){#check if data.frame/data.table
    stop("Input data must be a data.frame or data.table.")
  }
  if (!data.table::is.data.table(data)){#check specifically if input is data.table
    #message("Coercing input data to data.table.")
    data <- data.table::setDT(data)
  }

  #add underscores to blanks in names
  data <- data.table::data.table(sapply(data, function(x) gsub(" ", "_",x), simplify=T))

  #insert hypothetical taxonomic groups when group is absent
  #(i.e. hasn't been decided yet by scientific community)
  for (i in 1:nrow(data)){
    for (j in 2:(length(vars)-1)){#assign hypothetical labels for unassigned taxa; first and last taxonomic variables must not be blank
      if (data[[vars[j]]][i]==""){
        data[[vars[j]]][i] <- paste("h", data[[vars[j-1]]][i], data[[vars[j+1]]][i], sep="_")
      }
    }
  }

  #convert all taxonomic variables to factors
  for (j in 1:length(vars)){
    data[[vars[j]]] <- as.factor(data[[vars[j]]])
  }

  #make phylo object
  if (is.null(filter)){
    tree <- as.phylo.formula2(formula, data, root = F)
  }else{
    data_filt <- data
    inequality <- filter[grep("<|>", filter)]
    if(length(inequality)>0){#check for inequalities in filter
      for (k in 1:length(inequality)){#loop through multiple inequalities
        data_filt[[gsub("<.+|>.+","",inequality[k])]]<- as.numeric(data_filt[[gsub("<.+|>.+","",inequality[k])]])#convert variable used in inequality to numeric
        data_filt <- data_filt[eval(parse(text=paste0("data_filt$", inequality[k]))),]
      }
    }
    highlow <- filter[grep("high|low", filter)]
    if (length(highlow)>0){#select taxa with highest or lowest values
      if (length(highlow)>1){#check if multiple high or low filters used
        stop("Multiple 'high' or 'low' values not allowed. Please adjust the 'filter' parameter.")
      }else{
        highlow_var <- gsub("^.+-.+-","",highlow)
        data_filt[[highlow_var]] <- as.numeric(data_filt[[highlow_var]])#convert filter variable to numeric
        data_filt <- data_filt[order(data_filt[[gsub("(^.+-.+-)(.+$)", "\\2", highlow)]], decreasing=ifelse(length(grep("low", highlow)==1),FALSE,TRUE)),][1:abs(as.integer(gsub("(^.+-)(.+)(-.+$)", "\\2", highlow))),]
      }
    }
    tree <- as.phylo.formula2(formula, data_filt, root = F)
  }

  #filter data based on "filter" and "node_all"
  if (!is.null(filter)&node_all==FALSE){
    data <- data_filt
  }

  #Calculating trait_ratio for higher taxa
  label_data <- data
  names(label_data)[names(label_data)==vars[length(vars)]] <- "label"
  label_data$label <- as.character(label_data$label)
  label_list <- data.frame(label=append(tree$tip.label, tree$node.label))
  label_data <- merge(label_data, label_list, by = "label", all.y = T, all.x=T, sort = FALSE)
  label_data$tax_level <- ""

  for (i in 1:nrow(label_data)){
    if (is.na(label_data[["trait_ratio"]][i])){
      j <- 1
      matched <- F
      while (matched==F & j < length(vars)){
        hi_tax_rows <- label_data[as.character(label_data[[vars[j]]])==label_data$label[i],]
        if (nrow(hi_tax_rows)>0){
          matched <- T
          label_data$tax_level[i] <- vars[j]
        }
        j <- j+1
      }
      if (node_calc == "mean") { # trait ratio calculation for nodes based on mean
        label_data$trait_freq[i] <- as.character(sum(as.numeric(hi_tax_rows$trait_freq)))
        label_data$freq[i] <- as.character(sum(as.numeric(hi_tax_rows$freq)))
        label_data$trait_ratio[i] <- as.character(mean(as.numeric(hi_tax_rows$trait_ratio)))
      }else{ # trait ratio calculation for nodes based on sums
        label_data$trait_ratio[i] <- as.character(sum(as.numeric(hi_tax_rows$trait_freq))/sum(as.numeric(hi_tax_rows$freq)))
        label_data$trait_freq[i] <- as.character(sum(as.numeric(hi_tax_rows$trait_freq)))
        label_data$freq[i] <- as.character(sum(as.numeric(hi_tax_rows$freq)))
      }

    }else{
      label_data$tax_level[i] <- vars[length(vars)]
    }
  }

  label_data <- as.data.frame(as.matrix(label_data))
  label_data[is.na(label_data)] <- 0
  label_data$trait_ratio <- as.numeric(label_data$trait_ratio)
  label_data$trait_freq <- as.numeric(label_data$trait_freq)
  label_data$freq <- as.numeric(label_data$freq)

  #filter tree tips based on trait_col
  if (!is.null(filter)&node_all){
    label_data <- label_data[label_data$label%in%label_list$label,]#removes any taxa that are not to be included in final tree
  }

  #match label order
  label_data <- label_data[match(label_list$label, label_data$label), ]

  #color nodes based on trait_col
  if(node_color){#color
    tree2 <- ggtree::ggtree(tree, ladderize = ladderize, continuous=continuous, layout=layout,
                            ggplot2::aes(color = label_data[["trait_ratio"]]), ...)
  }else{#no color
    tree2 <- ggtree::ggtree(tree, ladderize = ladderize, continuous=continuous, layout=layout, ...)
  }

  #make ggtree object
  if (layout=="circular"){suppressMessages(tree2 <- tree2 +ggplot2::ylim(0.5,(length(tree$tip.label)+0.5)))}#fixes gap in circular plots

  #add additional data to ggtree object - can be used later for annotations
  tree2$data$tax_level <- label_data[match(label_data$label, tree2$data$label),]$tax_level
  tree2$data$trait_ratio <- label_data[match(label_data$label, tree2$data$label),]$trait_ratio
  tree2$data$trait_freq <- label_data[match(label_data$label, tree2$data$label),]$trait_freq
  tree2$data$freq <- label_data[match(label_data$label, tree2$data$label),]$freq
  tree2$data$label <- gsub("_", " ", tree2$data$label) #fix tip labels

  return(tree2)
}




