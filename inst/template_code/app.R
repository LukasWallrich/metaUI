
    shinyApp(onLaunch = function() {
      f <- '.'

      if(!(file.exists(file.path(f, 'server.R')))) {
        message('server.R not found in current working directory. Trying to automatically ',
                'identify location of this script - in case of errors, please set the ',
                'working directory.')


        # With thanks to https://stackoverflow.com/a/55322344/10581449
        getCurrentFileLocation <-  function()
        {
          this_file <- commandArgs() %>%
            tibble::enframe(name = NULL) %>%
            tidyr::separate(col=value, into=c('key', 'value'), sep='=', fill='right') %>%
            dplyr::filter(key == '--file') %>%
            dplyr::pull(value)
          if (length(this_file)==0)
          {
            this_file <- rstudioapi::getSourceEditorContext()$path
          }
          return(dirname(this_file))
        }

        f <- getCurrentFileLocation()
      }
      source(file.path(f, "global.R"))
    }, ui = ui, server = server)
