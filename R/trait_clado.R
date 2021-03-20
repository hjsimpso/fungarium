#' Plot enrichment factors within a cladogram based on taxonomic classification
#'
#' Taxonomic classification data is used to create a cladogram,
#' with node colors corresponding to trait enrichment factors. Useful for displaying
#' the distribution of a trait among different taxonomic groups.
#'
#'
#' @param data           Data.frame/data.table containing taxonomic classification variables (e.g. kingdom, phylum, class, etc), and enrichment values.
#' @param formula        Formula describing how the taxonomic variables in your data.frame should be nested (e.g ~V1/V2/.../Vn). Taxonomic variables must be factors. See \code{ape::as.phylo.formula}.
#' @param trait_col      Character string specifying the name of the column containing enrichment factors (must be numeric). Default is: "trait_ratio".
#' @param ladderize      Logical. Should the cladogram be ladderized? Default is TRUE. See \code{ape::ladderize}.
#' @param continuous     Logical. Should tree coloring be continuous between nodes? Default is TRUE. See \code{ggtree::ggtree}.
#' @param layout         Character string specifying the type of tree layout. Default is "circular". See \code{ggtree::ggtree}.
#' @param ...            Additional args passed to ggtree. See \code{ggtree::ggtree}.
#' @return           Returns a ggtree object.
#'
#' @details Cladogram produced using \code{ggtree::ggtree};
#' thus, all \code{ggtree} arguments are accepted. Additional
#' ggplot layers can be added to the tree as if it were a ggplot object.\cr
#' \cr
#' Please cite: \code{ggtree}. To get citation info use: \code{citation(package = 'ggtree')}.\cr
#' \cr
#' Cladogram in Simpson and Schilling (2021) created using \code{trait_clado}.
#' @references \enumerate{
#' \item Simpson, H.J., Schilling, J.S. 2021. Using aggregated field collections data
#' and the novel R package fungarium to investigate fungal fire association. \emph{Mycologia}. \bold{IN PRESS}
#' }
#' @export
#'
#' @examples
#' library(fungarium)
#' data(strophariaceae) #import sample dataset
#' MP_data <- taxon_update(strophariaceae) #update taxon names
#'
#' #Finds fire-associated records
#' string1 <- "(?i)charred|burn(t|ed)|scorched|fire.?(killed|damaged|scarred)|killed.by.fire"
#'
#' #Removes records falsely identfied as fire-associated
#' string2 <- "(?i)un.?burn(t|ed)"
#'
#' #filter out records that do not contain any environmental metadata (optional)
#' MP_data <- MP_data[MP_data$occurrenceRemarks!=""|MP_data$host!=""|MP_data$habitat!=""|MP_data$substrate!="",]
#'
#' #find trait-relevant records
#' trait_data <- find_trait(MP_data, pos_string=string1, neg_string=string2)
#'
#' #get trait enrichment
#' trait_enrichment <- enrichment(all_rec=MP_data, trait_rec=trait_data)
#'
#' #filter taxa based on total number of records (optional)
#' trait_enrichment <- trait_enrichment[trait_enrichment$freq>=5,]
#'
#' #make cladogram
#' library(ggtree)
#' library(ggplot2)
#'
#' tree <- trait_clado(data=trait_enrichment, trait_col="trait_ratio", continuous=T,
#'                     ladderize=T, layout="circular", size=1,
#'                     formula = ~new_order/new_family/new_genus/new_species)+
#'   geom_tiplab2(color = "black", hjust = 0, offset = 0.1, size = 1.5, fontface = "italic") + #add species labels
#'   geom_tippoint(shape=20, aes(color=trait_ratio, size=trait_freq))+ #add tree tips; size corresponds to the number of fire-associated records
#'   ggtitle("Strophariaceae (US records): fire-association")+
#'   scale_color_gradientn(colours= c("cyan", "blue", "purple", "red", "orange"),
#'                         name = "Fire-associated records enrichment",
#'                         limits = c(0, max(trait_enrichment$trait_ratio)+(0.01*max(trait_enrichment$trait_ratio))),
#'                         guide = guide_colourbar(label.vjust = 0.6, label.theme = element_text(size = 10, colour = "black", angle = 0),
#'                                                 title.position = "top", nbin=100, draw.ulim = FALSE, draw.llim = FALSE, barwidth = 15, barheight = 0.5)
#'   )+
#'   scale_size(name = "Fire-associated records",
#'              guide = guide_legend(keywidth = 2, keyheight = 1,
#'                                   label.position = "bottom", label.vjust = 0.6,
#'                                   label.theme = element_text(size = 10, colour = "black", angle = 0),
#'                                   title.position = "top")) +
#'   theme(plot.margin=margin(0,0,0,0),
#'         legend.title = element_text(size = 10, margin = margin(0,0,0,0)),
#'         legend.title.align = 0.5,
#'         legend.position = "bottom",
#'         legend.justification = "center",
#'         legend.margin = margin(0,0,0,0),
#'         plot.title = element_text(hjust = 0.5, margin=margin(0,0,0,0)))+
#'   xlim(-1, 3.6)#move root away from center; can help improve appearance of circular plot if root has more than two branches
#'
#' tree
#'
trait_clado <- function(data, formula=~new_kingdom/new_phylum/new_class/new_order/new_family/new_genus/new_name,
                        trait_col="trait_ratio",
                        ladderize=TRUE, continuous=TRUE, layout="circular",
                        ...){
  #check for dependencies
  if (!requireNamespace("ggtree", quietly = TRUE)) {
    stop("Please install the \"ggtree\" package.",
         call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install the \"ggplot2\" package.",
         call. = FALSE)
  }
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
    message("Coercing input data to data.table.")
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
  tree <- as.phylo.formula2(formula, data, root = F)

  #Calculating trait_ratio for higher taxa
  label_data <- data
  names(label_data)[names(label_data)==vars[length(vars)]] <- "label"
  label_data$label <- as.character(label_data$label)
  label_list <- data.frame(label=append(tree$tip.label, tree$node.label))
  label_data <- merge(label_data, label_list, by = "label", all.y = T, all.x=T, sort = FALSE)
  label_data <- label_data[match(label_list$label, label_data$label), ]
  label_data$tax_level <- ""

  for (i in 1:nrow(label_data)){
    if (is.na(label_data[[trait_col]][i])){
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
      label_data$trait_ratio[i] <- as.character(sum(as.numeric(hi_tax_rows$trait_freq))/sum(as.numeric(hi_tax_rows$freq)))
      label_data$trait_freq[i] <- as.character(sum(as.numeric(hi_tax_rows$trait_freq)))
      label_data$freq[i] <- as.character(sum(as.numeric(hi_tax_rows$freq)))
    }else{
      label_data$tax_level[i] <- vars[length(vars)]
    }
  }

  label_data <- as.data.frame(as.matrix(label_data))
  label_data[is.na(label_data)] <- 0
  label_data$trait_ratio <- as.numeric(label_data$trait_ratio)
  label_data$trait_freq <- as.numeric(label_data$trait_freq)
  label_data$freq <- as.numeric(label_data$freq)

  tree2 <- ggtree::ggtree(tree, ladderize = ladderize, continuous=continuous, layout=layout,
                                           ggplot2::aes(color = label_data[[trait_col]]), ...)

  if (layout=="circular"){suppressMessages(tree2 <- tree2 +ggplot2::ylim(0.5,(length(tree$tip.label)+0.5)))}#fixes gap in circular plots
  #add addiotional data to ggtree object
  tree2$data$tax_level <- label_data[match(label_data$label, tree2$data$label),]$tax_level
  tree2$data$trait_ratio <- label_data[match(label_data$label, tree2$data$label),]$trait_ratio
  tree2$data$trait_freq <- label_data[match(label_data$label, tree2$data$label),]$trait_freq
  tree2$data$freq <- label_data[match(label_data$label, tree2$data$label),]$freq
  tree2$data$label <- gsub("_", " ", tree2$data$label) #fix tip labels

  return(tree2)
}




