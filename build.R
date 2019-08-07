bookdown::render_book("index.Rmd", "bookdown::gitbook", config_file = "_bookdown.yml", clean = TRUE, 
                      output_dir = "G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/State_briefing/Test")

bookdown::render_book("index.Rmd", "bookdown::pdf_book", config_file = "_bookdown.yml", clean = TRUE, 
                      output_dir = "G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/State_briefing/Test")

bookdown::serve_book(dir = ".", output_dir = "_book", preview = TRUE, 
           in_session = TRUE, quiet = FALSE)
