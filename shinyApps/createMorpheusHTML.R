createMorpheusHTML <- function (
  eset, genesetname, filename, cluster=FALSE, max.rowLabels=3, max.colLabels=20)
{
  require(rjson)

  json <- list(
    rows=unname(nrow(eset)), columns=unname(ncol(eset)),
    seriesArrays=list(apply(unname(exprs(eset)), 1, as.list)),
    seriesDataTypes=list("Float32"),
    seriesNames=list(genesetname),
    rowMetadataModel=list(
      vectors=lapply(
        seq_along(fData(eset)),
        function (j) list(name=fvarLabels(eset)[j], array=fData(eset)[[j]])
      )
    ),
    columnMetadataModel=list(
      vectors=lapply(
        seq_along(pData(eset)),
        function (j) list(name=varLabels(eset)[j], array=pData(eset)[[j]])
      )
    )
  )

  rows <- lapply(
    seq(min(ncol(fData(eset)), max.rowLabels)),
    function (j) list(field=fvarLabels(eset)[j], display=list("text"))
  )
  columns <- lapply(
    seq(min(ncol(pData(eset)), max.colLabels)),
    function (j) list(field=varLabels(eset)[j], display=list("color"))
  )
  columnFilter <- list(
    filters=list(
      list(field="TAS", type="range", min=0, max=1)
    )
  )
  tools <- list(
    list(
      name="Hierarchical Clustering", params=c(cluster="Rows and columns")
    )
  )

  cat(
    '<!DOCTYPE html>',
    '<html>',
    '<head>',
    '  <meta http-equiv="Content-Type" content="text/html;charset=utf-8">',
    '  <meta http-equiv="X-UA-Compatible" content="IE=edge">',
    '  <meta name="viewport" content="user-scalable=no, width=device-width, initial-scale=1, maximum-scale=1">',
    '  <title>Morpheus</title>',
    '  <link rel="stylesheet" href="https://software.broadinstitute.org/morpheus/css/morpheus-latest.min.css">',
    '  <link rel="shortcut icon" href="https://software.broadinstitute.org/morpheus/favicon.ico" type="image/x-icon">',
    '  <script type="text/javascript" src="https://software.broadinstitute.org/morpheus/js/morpheus-external-latest.min.js"></script>',
    '  <script src="https://software.broadinstitute.org/morpheus/js/morpheus-latest.min.js"></script>',
    '</head>',
    '<body>',
    '<noscript>',
    '<p>Please enable JavaScript</p>',
    '</noscript>',
    '<div style="width: 100%; padding: 20px 20px 20px 20px;" id="vis"></div>',
    '<script type="text/javascript">',
    paste0('  var json = ', toJSON(json), ';'),
    '  new morpheus.HeatMap({',
    '    el: $("#vis"),',
    '    dataset: morpheus.Dataset.fromJSON(json),',
    paste0('    rows: ', toJSON(rows), ','),
    paste0('    columns: ', toJSON(columns), ','),
    paste0('    columnFilter: ', toJSON(columnFilter), ','),
    if (cluster) paste0('    tools: ', toJSON(tools)),
    '  });',
    '</script>',
    '</body>',
    '</html>',
    sep="\n", file=filename
  )
}
