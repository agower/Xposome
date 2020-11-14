sendTemporaryPassword <- function (
  from_sender="montilab@bu.edu", to_recipient="montilab@bu.edu",
  recipient_first="Montilab", recipient_last="Montilab",
  recipient_account="Montilab", tmp_pwd
)
{
  recipient <- paste(recipient_first, recipient_last)
  msg <- mime_part(
    paste0(
      '<!DOCTYPE>',
      '<html>',
      '<head>',
      '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>',
      '<title>HTML MESSAGE</title>',
      '<style type="text/css">',
      '</style>',
      '</head>',
      '<body>',
      p("Hi", strong(recipient_first)),
      p("The password for your Xposome account has changed."),
      p(),
      p("Username:", strong(recipient_account)),
      p("Temporary password:", strong(tmp_pwd)),
      '<br>',
      p(
        "Log back in? Follow this link,",
        strong("http://155.41.202.164/Xposome/?sign_in")
      ),
      '<br>',
      p("Best,"),
      p("Montilab Team"),
      '</body>',
      '</html>'
    )
  )
  # Override content type
  msg[["headers"]][["Content-Type"]] <- "text/html"
  from <- paste0(dQuote("Montilab Team"), "<", from_sender, ">")
  to <- paste0(dQuote(recipient), "<", to_recipient, ">", collapse="")
  subject <- "Temporary password for Xposome"
  body <- list(msg)
  sendmail(
    from, to, subject, body,
    control=list(smtpServer="smtp.bu.edu", smtpPort="25")
  )
}
