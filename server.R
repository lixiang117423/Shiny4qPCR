shinyServer(function(input, output, session){
  source('./main/qpcr_server.R', local = TRUE, encoding = 'UTF-8')
})