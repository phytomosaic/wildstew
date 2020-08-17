######################################################################
#
#  Wilderness Stewardship Performance:
#      Shiny app for automagically generated WSP reports
#
#    Rob Smith, phytomosaic@gmail.com, 24 Jun 2020
#
##      GNU General Public License, Version 3.0    ###################

# This project was supported in part by an appointment to the Research
# Participation Program at the United States Forest Service, United
# States Department of Agriculture, administered by the Oak Ridge
# Institute for Science and Education through an interagency agreement
# between the U.S. Department of Energy and USFS.

require(shiny)
require(rmarkdown)
require(knitr)
require(rgdal)
require(raster)
require(rgeos)
require(viridis)
load('d.rda', verbose=T)  # data in same directory as app.R
load('a.rda', verbose=T)  # data in same directory as app.R
load('s.rda', verbose=T)  # data in same directory as app.R
### UPDATE: newest shapefile as of 25 Jun 2020:
###    http://www.wilderness.net/GIS/Wilderness_Areas.zip
w       <- readOGR(dsn='.', 'wilderness_2019') # shapefiles
dem     <- raster('dem_usa_unclipped.tif') # DEM raster
choices <- sort(unique(as.character(a$warea)))

###################################################################
ui <- fluidPage(
        title = 'Wilderness Stewardship Performance',
        titlePanel('Wilderness Stewardship Performance'),
        br(),br(),
        sidebarLayout(
                ### sidebar
                sidebarPanel(
                        selectizeInput(
                                'x',
                                'Choose your Wilderness area',
                                choices = choices,
                                options=list(
                                        placeholder='Begin typing',
                                        onInitialize = I('function() {
                                        this.setValue(""); }')
                                )
                        ),
                        radioButtons('format', 'Document format',
                                     c('PDF'),
                                     # c('PDF', 'HTML', 'Word'),
                                     inline = TRUE),
                        downloadButton('downloadreport')
                ),
                ### main panel
                mainPanel(
                        ### text for selected wilderness area
                        h2(textOutput('selected_wa')),
                        ### footer
                        hr(),
                        div(class='footer',
                            p('Warning! This app is in beta stage for trial only!'),
                            p('Data are provisional and subject to change --'),
                            p('Questions? Comments? Contact: ',
                              a('robert.smith3@usda.gov',
                                href='robert.smith3@usda.gov',
                                target='_blank')
                            ),
                            div(style='height:50px')
                        )
                )
        )
)
###################################################################


###################################################################
server <- function(input, output) {

        ### reactive handler to capture wilderness name
        rx <- reactive({
                req(input$x) # require it not to be empty
                as.character(input$x)
        })

        ### render text
        output$selected_wa <- renderText({
                paste(input$x)
        })

        ### download handler
        output$downloadreport <- downloadHandler(
                filename = function() {
                        paste('wsp_report', sep = '.', switch(
                                input$format,
                                PDF  = 'pdf',
                                HTML = 'html',
                                Word = 'docx'
                        ))
                },
                content = function(file) {
                        src       <- normalizePath('report.Rmd')
                        src_png_a <- normalizePath('usfs.png')
                        src_png_b <- normalizePath('bothlichens.png')
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
                        file.copy(src, 'report.Rmd',
                                  overwrite=TRUE)
                        file.copy(src_png_a, 'usfs.png',
                                  overwrite=TRUE)
                        file.copy(src_png_b, 'bothlichens.png',
                                  overwrite=TRUE)
                        out <- rmarkdown::render(
                                'report.Rmd',
                                switch(
                                        input$format,
                                        PDF  = pdf_document(
                                                toc = T,
                                                toc_depth = 3,
                                                number_sections = T
                                        ),
                                        # HTML  = pdf_document(
                                        #         toc = T,
                                        #         toc_depth = 3,
                                        #         number_sections = T
                                        # ),
                                        # Word  = pdf_document(
                                        #         toc = T,
                                        #         toc_depth = 3,
                                        #         number_sections = T
                                        # )
                                        HTML = html_document(),
                                        Word = word_document()
                                ))
                        file.rename(out, file)
                }
        )
}
###################################################################

### build it
shinyApp(ui=ui, server=server)
