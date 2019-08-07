
State_curr="UK"

bookdown::render_book("index.Rmd", "bookdown::gitbook", config_file = "_bookdown.yml", clean = TRUE, 
                      output_dir = "G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/State_briefing/Test")

bookdown::render_book("State_briefing_pdf.Rmd", "bookdown::pdf_book", config_file = "_bookdown.yml", clean = TRUE, 
                      output_dir = "G:/HQ/dgof-pru/Project/Vertical_flight_efficiency/2015/State_briefing/Test")


rmarkdown::render('State_briefing_pdf_test.Rmd',
                  output_file = paste0('Test/State briefing ', State_curr, '.pdf'))
