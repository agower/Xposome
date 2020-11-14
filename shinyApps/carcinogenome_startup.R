# Defaults for gene expression subtab
defaults <- list(
  landmark_de = FALSE,
  summarizefunc_de = "median",
  filterbyinput_de = c("score", "number"),
  range_de = c(-2, 2),
  numberthresleft_de = 10,
  numberthresright_de = 10
)

# Create a list of gene enrichment methods
dsmap_method <- list("gsva"="gsva")

# The gsva helptext method
helptext_method <- paste(
  "gsva, ssgea, zscore: from R Bioconductor package GSVA",
  sep = "<br>"
)

# Create classes for connectivity mapping
connmap <- list("Perturbagen Classes" = "pcl", "Perturbagens" = "pert")

# Link gene expression to genecards.org
get_genecard_link <- function (genesymbol)
{
  paste0(
    '<a href="http://www.genecards.org/cgi-bin/carddisp.pl?gene=', genesymbol,
    '&keywords=', genesymbol,
    '" target="_blank" style="text-decoration:none;">', genesymbol, '</a>'
  )
}

# Get the gene set enrichment hyperlink
get_geneset_link <- function (geneset)
{
  label <- gsub("_", " ", geneset)
  geneset <- gsub(" ", "_", geneset)
  paste0(
    '<a href="http://software.broadinstitute.org/gsea/msigdb/cards/', geneset,
    ' target="_blank" style="text-decoration:none;">', label, '</a>'
  )
}

# Get the 25th percentile
Q1 <- function (x) quantile(x, 0.25, na.rm = TRUE)
# Get the 75th percentile
Q3 <- function (x) quantile(x, 0.75, na.rm = TRUE)

# Get the chemical name
get_chem_description <- function (chemical_dat, chem, chemical_id=FALSE)
{
  pos <- unlist(
    lapply(
      c("Chemical_Name", "BUID", "CAS"),
      function (x) {
        w <- which(chemical_dat[,x] == chem)
        if (length(w)) return(w)
      }
    )
  )
  if (chemical_id) {
    return(unique(chemical_dat$Chemical_Id[pos]))
  } else {
    return(pos)
  }
}

# Function to round the data table values ####
data.table.round <- function (dt, digits = 3) {
  for (i in which(sapply(dt, is.numeric))) {
    dt[[i]] <- round(dt[[i]], digits)
  }
  data.table(dt)
}

# Get gene expression
get_de <- function (
  chem, annot_var,
  profile_dat, chemical_dat, expression_dat,
  header = "ModZScore",
  summarize.func = "mean",
  landmark = TRUE,
  do.nmarkers = TRUE, nmarkers = c(100, 100),
  do.scorecutoff = TRUE, scorecutoff = c(-2, 2)) {
  # Get chemical BUID
  chemical_id <- get_chem_description(
    chemical_dat=chemical_dat, chem=chem, chemical_id=TRUE
  )
  # Get signature id
  profile_dat <- profile_dat[
    match(colnames(expression_dat), profile_dat[, annot_var]),
  ]
  exposure <- sort(
    unique(
      profile_dat$unique_ID_by_chem[
        which(profile_dat$Chemical_Id == chemical_id)
      ]
    )
  )
  # Getting expression data
  eset <- expression_dat
  # Getting feature data
  fdat <- fData(eset)
  if (nrow(fdat)) {
    if (!"Gene" %in% colnames(fdat)) {
      fdat <- data.frame(Gene=rownames(eset), fdat)
    }
  } else {
    fdat <- data.frame(Gene=rownames(eset))
  }
  # check if landmark is selected
  if (landmark) {
    if ("Landmark_Gene" %in% colnames(fdat)) {
      eset <- eset[which(toupper(fdat[,"Landmark_Gene"]) %in% "YES"), ]
    }
  }
  # Summarize the ExpressionSet
  eSet <- exprs(eset)
  mat <- matrix(
    NA, nrow=nrow(eSet), ncol=length(exposure), byrow=TRUE,
    dimnames=list(rownames(eSet), paste0(header, " ", exposure))
  )
  for (i in seq_along(exposure)) {
    sig <- unique(
      profile_dat[
        which(profile_dat$unique_ID_by_chem %in% exposure[i]), annot_var
      ]
    )
    if (length(sig) > 1) {
      mat[,i] <- rowMeans(eSet[,as.character(sig)], na.rm=TRUE)
    } else {
      mat[,i] <- eSet[,as.character(sig)]
    }
  }
  x <- apply(mat, 1, match.fun(summarize.func))
  x <- as.numeric(x)
  n <- length(x)
  if (do.nmarkers) {
    ind0 <- sum(x > 0)
    n1 <- min(nmarkers[1], ind0)
    n2 <- min(nmarkers[2], n-ind0)
    ord <- order(x, decreasing = TRUE)
    n2ind <- n-n2+1
    if (n1 == 0 & n2 == 0) {
      x.ind.nmarkers <- NULL
    } else if (n2 == 0) {
      x.ind.nmarkers <- ord[1:n1]
    } else {
      x.ind.nmarkers <- c(ord[1:n1], ord[n2ind:n])
    }
  } else {
    x.ind.nmarkers <- 1:n
  }
  if (do.scorecutoff) {
    #TODO: rank by score here too
    x.ind.scorecutoff <- which(x > scorecutoff[2] | x < scorecutoff[1])
  } else {
    x.ind.scorecutoff <- 1:n
  }
  inds <- intersect(x.ind.nmarkers, x.ind.scorecutoff)
  inds <- inds[order(x[inds], decreasing = TRUE)]
  # Get expression results
  res.ind <- inds
  res.scores <- x[inds]
  # Determine whether gene is up or down regulated
  direction <- ifelse(res.scores > 0, "Up", "Down")
  # Create the summary table
  table <- cbind(
    fdat[res.ind,, drop = FALSE],
    Direction = direction, SummaryScore=res.scores, mat[res.ind,, drop = FALSE]
  )
  colnames(table)[colnames(table) == "SummaryScore"] <- "Summary Score"
  # Return hyperlink from genecard.org
  table$Gene <- sapply(as.character(table$Gene), get_genecard_link)
  return(table)
}

# Summarize gene set enrichment
get_gsenrichment <- function (
  chem, annot_var, profile_dat, chemical_dat, expression_dat,
  gsname = "Hallmark", header = "GS Score", summarize.func = "mean"
) {
  # Get chemical BUID
  chemical_id <- get_chem_description(
    chemical_dat=chemical_dat, chem=chem, chemical_id=TRUE
  )
  # Get signature id
  profile_dat <- profile_dat[
    match(colnames(expression_dat), profile_dat[, annot_var]),
  ]
  exposure <- sort(
    unique(
      profile_dat$unique_ID_by_chem[
        which(profile_dat$Chemical_Id == chemical_id)
      ]
    )
  )
  # Get expression data
  eset <- expression_dat
  # Get feature data
  fdat <- fData(eset)
  if (nrow(fdat) > 0) {
    if (!"Geneset" %in% colnames(fdat)) {
      fdat <- data.frame(Geneset=rownames(eset), fdat)
    }
  } else {
    fdat <- data.frame(Geneset=rownames(eset))
  }
  # Summarize the ExpressionSet
  eSet <- exprs(eset)
  mat <- matrix(
    NA, nrow=nrow(eSet), ncol=length(exposure), byrow=TRUE,
    dimnames=list(rownames(eSet), paste0(header, " ", exposure))
  )
  for (i in seq_along(exposure)) {
    sig <- unique(
      profile_dat[
        which(profile_dat$unique_ID_by_chem == exposure[i]), annot_var
      ]
    )
    if (length(sig) > 1) {
      mat[,i] <- rowMeans(eSet[, as.character(sig)], na.rm=TRUE)
    } else {
      mat[,i] <- eSet[, as.character(sig)]
    }
  }
  res <- apply(mat, 1, match.fun(summarize.func))
  res <- as.numeric(res)
  res <- cbind(fdat, score = res, mat)
  res <- res[order(res$score, decreasing = TRUE),, drop = FALSE]
  colnames(res)[colnames(res) == "score"] <- "Summary Score"
  # Return hyperlink to MSigDB genesets
  if (gsname %in% c("Hallmark", "C2")) {
    res$Geneset <- sapply(as.character(res$Geneset), get_geneset_link)
  }
  return(res)
}

# Summarize connectivity map
get_connectivity <- function (
  chem, annot_var, profile_dat, chemical_dat, expression_dat,
  header = "Connectivity Score", summarize.func = "mean"
) {
  # Get chemical BUID
  chemical_id <- get_chem_description(
    chemical_dat=chemical_dat, chem=chem, chemical_id=TRUE
  )
  # Get signature id
  profile_dat <- profile_dat[
    match(colnames(expression_dat), profile_dat[, annot_var]),
  ]
  exposure <- sort(
    unique(
      profile_dat$unique_ID_by_chem[
        which(profile_dat$Chemical_Id %in% chemical_id)
      ]
    )
  )
  # Get expression data
  eset <- expression_dat
  # Get feature data
  fdat <- fData(eset)
  if (nrow(fdat)) {
    if (!"Connectivity_Id" %in% colnames(fdat)) {
      fdat <- data.frame(Connectivity_Id=rownames(eset), fdat)
    }
  } else {
    fdat <- data.frame(Connectivity_Id=rownames(eset))
  }
  # Summarize the ExpressionSet
  eSet <- exprs(eset)
  mat <- matrix(
    NA, nrow=nrow(eSet), ncol=length(exposure), byrow=TRUE,
    dimnames=list(rownames(eSet), paste0(header, " ", exposure))
  )
  for (i in seq_along(exposure)) {
    sig <- unique(
      profile_dat[
        which(profile_dat$unique_ID_by_chem %in% exposure[i]), annot_var
      ]
    )
    if (length(sig) > 1) {
      mat[,i] <- rowMeans(eSet[,as.character(sig)], na.rm=TRUE)
    } else {
      mat[,i] <- eSet[,as.character(sig)]
    }
  }
  res <- apply(mat, 1, match.fun(summarize.func))
  res <- as.numeric(res)
  res <- cbind(fdat, score = res, mat)
  res <- res[order(res$score, decreasing = TRUE),, drop = FALSE]
  colnames(res)[colnames(res) == "score"] <- "Summary Score"
  return(res)
}

plot_wrapper <- function (df, view = "Density", ...)
{
  if (view == "Density") {
    res <-
      ggplot(data=df, aes_string(x = "x", fill = "cols")) +
      geom_density(position = "identity", alpha = 0.5)
  }else if (view == "Boxplot") {
    res <-
      ggplot(data=df, aes_string(x = "cols", y= "x", fill = "cols")) +
      geom_boxplot(
        position = "identity", width = 0.2, alpha = 0.5,
        outlier.fill = NULL, outlier.alpha = NULL
      )
  }
  return(res)
}

# Function to create density and boxplot for marker explorer
get_marker_plot <- function (
  expression_dat,
  profile_dat,
  annot_var = "Sig_Id",
  marker_id,
  col_id = "Carcinogenicity",
  col_names = c("N/A", "-", "+"),
  col_colors = c("grey", "green", "orange"),
  header = "Mod Z-scores",
  tas = NULL,
  view = "Density") {
  if (is.null(tas)) {
    profile_ann <- profile_dat
  } else {
    profile_ann <- profile_dat %>%
      filter(TAS >= (min(tas)-0.01) & TAS <= (max(tas)+0.01))
  }
  if (is.na(col_id)) {
    profile_ann <- profile_ann %>%
      transmute(Id=(!!!syms(annot_var)), cols="query") %>%
      distinct(Id, .keep_all=TRUE)
  } else {
    profile_ann <- profile_ann %>%
      transmute(Id=(!!!syms(annot_var)), cols=(!!!syms(col_id))) %>%
      distinct(Id, .keep_all=TRUE)
  }
  rowid <- which(rownames(expression_dat) %in% marker_id)
  eset <- exprs(expression_dat)[rowid,]
  query <- profile_ann %>%
    mutate(Id=as.character(Id)) %>%
    left_join(data.frame(Id=as.character(names(eset)), x=as.numeric(eset))) %>%
    select(x, cols)
  if (is.na(col_id)) {
    background <- data.frame(
      x=as.numeric(exprs(expression_dat)), cols="background"
    )
    df <- query %>% rbind(background)
    df$cols <- factor(
      df$cols, levels = c("background", "query"),
      ordered=is.ordered(c("background", "query"))
    )
    cols_match <- c("grey", "red")
    names(cols_match) <- c("background", "query")
    p.title <- paste(
      "Distribution of", header, "Across Profiles\n",
      "for", marker_id, "(Overall)\n"
    )
  } else {
    df <- query
    cols_match <- col_colors
    names(cols_match) <- col_names
    df$cols <- factor(
      df$cols, levels = col_names, ordered=is.ordered(col_names)
    )
    p.title <- paste(
      "Distribution of", header, "across profiles\n",
      "for", marker_id, "(by ", col_id, ")\n",
    )
  }
  p <- plot_wrapper(df=df, view=view) +
    scale_fill_manual(
      name = NULL,
      values = cols_match,
      breaks = names(cols_match),
      labels = names(cols_match)
    ) +
    xlab(header) +
    ylab("Density") +
    ggtitle(p.title) +
    theme_bw() +
    theme(
      plot.margin = margin(5, 5, 0, 0),
      plot.title = element_text(hjust = 0.5)
    )
  return(p)
}

# Get the table profile ranked by mod Z-scores for marker explorer
get_de_by_gene_table <- function (
  marker_id, expression_dat, profile_dat, annot_var = "Sig_Id",
  header = "Mod Z-scores", tas = NULL
) {
  profile_dat <- profile_dat[
    which(profile_dat[, annot_var] %in% colnames(expression_dat)),
  ]
  if (is.null(tas)) {
    sig <- unique(profile_dat[, annot_var])
  } else {
    sig <- unique(
      profile_dat[
        which(
          profile_dat$TAS >= (min(tas)-0.01) &
          profile_dat$TAS <= (max(tas)+0.01)
        ),
        annot_var
      ]
    )
  }
  eset <- expression_dat[, as.character(sig)]
  rowid <- which(rownames(eset) %in% marker_id)
  x <- as.numeric(exprs(eset)[rowid,])
  pdat <- profile_dat[
    which(
      !duplicated(
        profile_dat[
          which(profile_dat[,annot_var] %in% as.character(sig)), annot_var
        ]
      )
    ),
  ]
  df <- cbind(value = x, pdat)
  df <- df[order(x, decreasing = TRUE),]
  colnames(df)[colnames(df) %in% "value"] <- header
  return(df)
}
