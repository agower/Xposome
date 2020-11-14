# Define the meta variable test for K2Taxonomer
meta_variable_test <- data.frame(
  statistical_test = c(
    "1-sided Fisher test", "2-sided Fisher test", "1-sided Wilcox RS test",
    "2-sided Wilcox RS test", "1-sided t test", "2-sided t test"
  ),
  method = c("factor1", "factor", "numeric1", "numeric", "normal1", "normal"),
  stringsAsFactors = FALSE
)

phenotype_test <- function (pro_ann, varlist) {
  test <- NULL
  for (var in varlist) {
    checkvar <- pro_ann[, var]
    if (any(!is.na(checkvar))) {
      if (all(is.na(as.numeric(checkvar)))) {
        if (length(unique(checkvar)) > 1) {
          if (length(unique(checkvar)) <= 3 & all(toupper(c("Yes", "No")) %in% toupper(checkvar))) {
            test <- c(
              test,
              SelectInputFunction(
                id=var, label=NULL, choices=c("1-sided Fisher test"),
                selected="1-sided Fisher test"
              )
            )
          } else {
            test <- c(
              test,
              SelectInputFunction(
                id=var, label=NULL, choices=c("2-sided Fisher test"),
                selected="2-sided Fisher test"
              )
            )
          }
        } else {
          test <- c(
            test,
            paste0(
              "<p style='color:red;'>", var, " only has one factor level. Please choose another variable that has more than one factor/value.</p>"
            )
          )
        }
      } else {
        if (length(unique(checkvar)) > 1) {
          if (length(unique(checkvar)) == 2) {
            test <- c(
              test,
              SelectInputFunction(
                id=var, label=NULL, choices=c("1-sided Fisher test"),
                selected="1-sided Fisher test"
              )
            )
          } else if (length(unique(checkvar)) %in% 3:10) {
            test <- c(
              test,
              SelectInputFunction(
                id=var, label=NULL, choices=c("2-sided Fisher test"),
                selected="2-sided Fisher test"
              )
            )
          } else {
            test <- c(
              test,
              SelectInputFunction(
                id=var, label=NULL,
                choices=c(
                  "1-sided Wilcox RS test", "2-sided Wilcox RS test",
                  "1-sided t test", "2-sided t test"
                ),
                selected=NULL
              )
            )
          }
        } else {
          test <- c(
            test,
            paste0(
              "<p style='color:red;'>", var, " only has one factor level. Please choose another variable that has more than one factor/value.</p>"
            )
          )
        }
      }
    } else {
      test <- c(
        test,
        paste0(
          "<p style='color:red;'>", var, " variable is empty. Please choose another variable that has more than one factor/value.</p>"
        )
      )
    }
  }
  return(test)
}

# Add links to gene sets
get_enrTablelink <- function (geneset) {
  geneset <- gsub(" ", "_", geneset)
  paste0(
    '<a href="http://software.broadinstitute.org/gsea/msigdb/cards/', geneset,
    '" style="text-decoration:none;" target="_blank">&#128269;</a>'
  )
}

# Add links to genes
get_dgeTable_link <- function (genesymbol) {
  paste0(
    '<a href="http://www.genecards.org/cgi-bin/carddisp.pl?',
    'gene=', genesymbol, '&keywords=', genesymbol,
    '" style="text-decoration: none;" target="_blank">&#128269;</a>'
  )
}
