######################################################################
#
#  Wilderness Stewardship Performance:
#      Shiny app for automagically generated WSP reports
#
#    Rob Smith, phytomosaic@gmail.com, 16 Apr 2020
#
##      GNU General Public License, Version 3.0    ###################

require(shiny)
require(rmarkdown)
require(knitr)
load('d.rda', verbose=T)  # data in same directory as app.R
load('a.rda', verbose=T)  # data in same directory as app.R
load('s.rda', verbose=T)  # data in same directory as app.R
choices <- sort(unique(as.character(a$warea)))

### functions setup
`get_years` <- function (pick, ...) {
        rng <- range(d[d$wa==pick, 'year'], na.rm=TRUE)
        if (any(!is.finite(rng))) c(NA,NA) else rng
}

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
                                     c('PDF', 'HTML', 'Word'),
                                     inline = TRUE),
                        downloadButton('downloadreport')
                ),
                ### main panel
                mainPanel(

                        ### text for selected wilderness area
                        h2(textOutput('selected_wa')),

                        ### text for temporal coverage
                        h3(textOutput('rangeofyears')),

                        # ### trends plot
                        # br(),br(),
                        # plotOutput('trendplot'),

                        # ### table output
                        # br(),
                        # tableOutput('fulltable'),

                        ### footer
                        hr(),
                        div(class='footer',
                            p('Questions? Comments? Contact: ',
                              a('phytomosaic@gmail.com',
                                href='phytomosaic@gmail.com',
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

        ### render text
        output$rangeofyears <- renderText({
                req(input$x)
                rng <- get_years(pick = rx())
                paste0('Years: ', rng[1], ' - ', rng[2])
        })

        # ### render plot of wilderness trends
        # output$trendplot <- renderPlot({
        #         par(tcl=-0.2, mgp=c(1.8,0.4,0), mar=c(4,4,0.5,0.5),
        #             oma=c(0,0,0,0), pty = 's', bty = 'L', las = 1,
        #             cex.axis = 0.85)
        #         plot_trend(pick = rx())
        #         # set_par(4)
        #         # plot_trend(pick = rx(), yvar = 'n_airscore')
        #         # plot_trend(pick = rx(), yvar = 'n_ratio',
        #         #            ylab = 'N eutroph ratio')
        #         # plot_trend(pick = rx(), yvar = 's_scr',
        #         #            ylab = expression(S~air~score~(kg~S~ha^'-1'~y^'-1')))
        #         # plot_trend(pick = rx(), yvar = 's_ratio',
        #         #            ylab = 'S eutroph ratio')
        # })

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
                        src <- normalizePath('report.Rmd')
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
                        file.copy(src, 'report.Rmd', overwrite=TRUE)
                        out <- rmarkdown::render(
                                'report.Rmd',
                                switch(
                                        input$format,
                                        PDF  = pdf_document(
                                                toc = T,
                                                toc_depth = 3,
                                                number_sections = T
                                        ),
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
