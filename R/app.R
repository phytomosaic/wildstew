

### app for automagically generated WSP reports
# see: https://shiny.rstudio.com/gallery/download-knitr-reports.html
library(shiny)
library(rmarkdown)
# source('./_makefiles/00_data-processing.R')
wnames <- sort(unique(as.character(a$warea)))

### function to plot trends of each WA
`plot_trend` <- function (ii, iy, addtitle=TRUE, ...) {
        if (missing(iy)) iy <- 'n_scr'
        y    <- a[a$warea==ii,paste0(iy, '_mean')]
        ysd  <- a[a$warea==ii,paste0(iy, '_sd')]
        yn   <- a[a$warea==ii,paste0(iy, '_n')]
        x    <- a[a$warea==ii,'year']
        ylm  <- c(min(c(y, y - ysd), na.rm=T) - 0.4,
                  max(c(y, y + ysd), na.rm=T) + 0.1)
        if (all(is.infinite(ylm))) return(NULL)
        if (anyNA(ylm)) { ylm[which(is.na(ylm))] <- NULL }
        ylb <- expression(Lichen~N~score~(kg~N~ha^'-1'~y^'-1'))
        plot(x, y, pch=16, ylab=ylb, xlab='Year', ylim=ylm, ...)
        `se` <- function(x, yval, se, wiskwidth=0.4) {
                w <- wiskwidth/2
                segments(x0=x, x1=x, y0=yval-se, y1=yval+se, lwd=2)
                segments(x0=x-w, x1=x+w, y0=yval+se, y1=yval+se,lwd=2)
                segments(x0=x-w, x1=x+w, y0=yval-se, y1=yval-se,lwd=2)
        }
        se(x, y, ysd)
        text(x, min(c(ylm,y))+0.1, yn, cex=0.7, col=2)
        if (addtitle) title(ii)
}
`get_years` <- function (ii, ...) {
        rng <- range(a[a$warea==ii,'year'], na.rm=TRUE)
        if (any(!is.finite(rng))) c(NA,NA) else rng
}
`get_table` <- function (ii, ...) {
        # require(kableExtra, quietly = TRUE)
        tab <- a[a$warea==ii,
                 !(colnames(a) %in% c('warea','lat','lon'))]
        colnames(tab) <- c('Year',rep(c('Mean','SD','n'), 4))
        options(knitr.kable.NA = '')
        knitr::kable(tab, row.names = F, longtable = T, booktabs = T,
                     caption = NULL, escape = F) %>%
                add_header_above(c('',
                                   'Nitrogen air scores' = 3,
                                   'Nitrogen trophic scores' = 3,
                                   'Sulfur air scores' = 3,
                                   'Sulfur trophic scores' = 3
                ),
                escape = FALSE)%>%
                kable_styling(latex_options = 'repeat_header') %>%
                column_spec(2:13, width = "1cm")
}

###################################################################
ui <- fluidPage(
        title = 'Wilderness Stewardship Performance',
        sidebarLayout(
                ### sidebar
                sidebarPanel(
                        selectizeInput(
                                'x',
                                'Choose your Wilderness area',
                                choices = wnames,
                                options=list(
                                        placeholder='Begin typing',
                                        onInitialize = I('function() {
                                        this.setValue(""); }')
                                )
                        ),
                        radioButtons('format', 'Document format',
                                     c('PDF', 'HTML', 'Word'),
                                     inline = TRUE),
                        downloadButton('downloadReport')
                ),
                ### main panel
                mainPanel(

                        ### main title
                        br(),br(),
                        h2('Wilderness Stewardship Performance'),

                        ### text for selected wilderness area
                        h3(textOutput('selected_wa')),

                        ### text for temporal coverage
                        h4(textOutput('rangeofyears')),

                        ### trends plot
                        br(),br(),
                        plotOutput('trendplot'),

                        ### table output
                        br(),
                        tableOutput('fulltable'),

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
        r_wname <- reactive({
                as.character(input$x)
        })

        ### render text
        output$selected_wa <- renderText({
                paste(input$x)
        })

        ### render text
        output$rangeofyears <- renderText({
                rng <- get_years(ii = r_wname())
                paste0('Years: ', rng[1], ' - ', rng[2])
        })

        ### render plot of wilderness trends
        output$trendplot <- renderPlot({
                set_par(1)
                plot_trend(ii = r_wname())
        })

        ### table output
        output$fulltable <- function() {
                get_table(ii = r_wname())
                # req(input$mpg)
                # mtcars %>%
                #         mutate(car = rownames(.)) %>%
                #         select(car, everything()) %>%
                #         filter(mpg <= input$mpg) %>%
                #         knitr::kable("html") %>%
                #         kable_styling("striped", full_width = F) %>%
                #         add_header_above(c(" ", "Group 1" = 5, "Group 2" = 6))
        }


        ### download handler
        output$downloadReport <- downloadHandler(
                filename = function() {
                        paste('wsp_report', sep = '.', switch(
                                input$format,
                                PDF = 'pdf',
                                HTML = 'html',
                                Word = 'docx'
                        ))
                },
                content = function(file) {
                        src <- normalizePath('report.Rmd')
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
                        file.copy(src, 'report.Rmd', overwrite = TRUE)
                        out <- rmarkdown::render('report.Rmd', switch(
                                input$format,
                                PDF  = pdf_document(),
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