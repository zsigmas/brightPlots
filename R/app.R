
#' Volcano and upset app
#'
#' Runs a Shiny app for gene upsetting results exploration.
#'
#' @param dataset A named list of datasets as output by maUEB::dea_toptable1()
#' @export
app <- function(dataset) {
    if (missing(dataset)) {
        dataset <- brightPlots::toptable
    }

    dataset <- format_vol_data(dataset)

    ui <- function(req) {
        menu_elements <- menu_UI("menu", unique(dataset[["facet"]]))
        volcano_elements <- volcano_UI("volcano")

        alpha_column <-
            shiny::div(
                class = "col-auto",
                shiny::div(
                    class = "card",
                    shiny::div(class = "card-header p-1", shiny::tags[["p"]](htmltools::HTML("&alpha;"), class = "mb-1 pe-1")),
                    shiny::div(
                        class = "card-body p-1",
                        shiny::fluidRow(
                            class = "g-1",
                            shiny::div(
                                class = "col col-auto",
                                menu_elements[["alpha"]][["p_val"]]
                            ),
                            shiny::div(
                                class = "col col-auto",
                                menu_elements[["alpha"]][["adj"]]
                            )
                        ),
                        shiny::div(menu_elements[["comp"]][["comp"]])
                    )
                )
            )

        logfc_column <-
            shiny::div(
                class = "col-auto",
                shiny::div(
                    class = "card h-100",
                    shiny::div(
                        class = "card-header p-1",
                        shiny::tags[["p"]]("Log FC", class = "mb-1 pe-1 ps-1")
                    ),
                    shiny::div(
                        class = "card-body p-1",
                        shiny::fluidRow(
                            class = "g-0",
                            shiny::div(
                                class = "col col-auto",
                                menu_elements[["b"]][["lt_enab"]]
                            ),
                            shiny::div(
                                class = "col col-auto",
                                menu_elements[["b"]][["lt"]]
                            )
                        ),
                        shiny::fluidRow(
                            class = "g-0",
                            shiny::div(
                                class = "col col-auto",
                                menu_elements[["b"]][["gt_enab"]]
                            ),
                            shiny::div(
                                class = "col col-auto",
                                menu_elements[["b"]][["gt"]]
                            )
                        )
                    )
                )
            )

        selected_column <-
            shiny::div(
                class = "col-auto",
                shiny::div(
                    class = "card h-100",
                    shiny::div(
                        class = "card-header p-1",
                        shiny::tags[["p"]]("Selected entries", class = "mb-1 pe-1 ps-1")
                    ),
                    shiny::div(
                        class = "card-body p-1",
                        shiny::fluidRow(
                            class = "g-0",
                            shiny::div(
                                class = "col col-auto",
                                shiny::downloadButton("download_selected", shiny::tagList("Download ", shiny::textOutput("n_selected", inline = TRUE), "entries"), class = "btn-sm m-1de ")
                            )
                        ),
                        shiny::fluidRow(
                            class = "g-0 mt-2  justify-content-end",
                            shiny::div(
                                class = "col col-auto",
                            )
                        )
                    )
                )
            )

        volcano_column <-
            shiny::column( # chart column
                width = 9,
                shiny::div(
                    class = "card",
                    shiny::div(
                        class = "card-header p-1 text-muted",
                        "Volcano"
                    ),
                    shiny::div(
                        class = "card-body p-1 overflow-auto",
                        volcano_elements[["chart"]]
                    )
                )
            )


        info_column <- shiny::column( # info column
            width = 3,
            shiny::div(
                class = "card h-100",
                shiny::div(
                    class = "card-header p-1",
                    shiny::textOutput("hovered_name") %>% htmltools::tagAppendAttributes(class = "fw-bold")
                ),
                shiny::div(
                    class = "card-body p-1 overflow-auto",
                    shiny::uiOutput("hovered_body")
                )
            )
        )

        upset_column <-
            shiny::column( # chart column
                width = 12,
                shiny::div(
                    class = "card",
                    shiny::div(class = "card-header p-1 text-muted", "Upset"),
                    shiny::div(
                        class = "card-body p-1",
                        upset_UI("upset")
                    )
                )
            )


        shiny::tagList(
            shiny::fluidPage(
                shiny::fluidRow(
                    class = "m-1",
                    alpha_column,
                    logfc_column,
                    selected_column
                ),
                shiny::fluidRow(
                    class = "m-1",
                    volcano_column,
                    info_column
                ),
                shiny::fluidRow(
                    class = "m-1",
                    upset_column
                ),
                theme = bslib::bs_theme(version = 5)
            )
        )
    }

    server <- function(input, output, session) {
        sel <- menu_server("menu")

        shiny::observe({
            shiny::reactiveValuesToList(input)
            session$doBookmark()
        })
        # Update the query string
        shiny::onBookmarked(shiny::updateQueryString)

        sel_info <- shiny::reactiveVal(NULL)

        shiny::observe({
            d <- try(vol_r[["hovered"]](), silent = TRUE)
            shiny::req(!inherits(d, "try-error"))
            sel_info(d)
        })

        shiny::observe({
            purrr::iwalk(sel, ~ rlang::inform(paste(.y, .x())))
        })
        vol_r <- volcano_server("volcano", dataset, p = sel[["p"]], adj_p = sel[["adj"]], comp = sel[["comp"]], log_fc_range = sel[["log_fc_range"]])

        output[["hovered_name"]] <- shiny::renderText({
            shiny::validate(
                shiny::need(
                    !is.null(sel_info()) && nrow(sel_info()) > 0,
                    "Hover over a point"
                )
            )
            sel_info()[["Gene.Symbol"]]
        })

        output[["hovered_body"]] <- shiny::renderUI({
            shiny::req(sel_info())

            gene_dataset <- dataset %>%
                dplyr::filter(.data[["Gene.Symbol"]] == sel_info()[["Gene.Symbol"]])

            build_comp_table <- function(x) {
                t <- shiny::tags

                comp_table <- purrr::pmap(.l = x, function(...) {
                    arg <- list(...)
                    t$tr(
                        t$th(scope = "row", arg[["facet"]]),
                        t$td(signif(arg[["log_fc"]], digits = 2)),
                        t$td(signif(arg[["adj_p"]], digits = 2)),
                        t$td(signif(arg[["unadj_p"]], digits = 2))
                    )
                })

                t$table(
                    class = "table",
                    t$thead(
                        t$tr(
                            t$th(scope = "col", ""),
                            t$th(scope = "col", "LogFC"),
                            t$th(scope = "col", "Adj p"),
                            t$th(scope = "col", "p")
                        )
                    ),
                    t$tbody(
                        comp_table
                    )
                )
            }

            build_meta_table <- function(x) {
                t <- shiny::tags


                t$table(
                    class = "table",
                    t$tbody(
                        t$tr(
                            t$td("EntrezID"),
                            t$td(t$a(x[["EntrezID"]], href = paste0("https://www.ncbi.nlm.nih.gov/gene/", x[["EntrezID"]]), target = "_blank"))
                        ),
                        t$tr(
                            t$td(
                                "Probe"
                            ),
                            t$td(
                                x[["Probe"]]
                            )
                        )
                    )
                )
            }

            shiny::tagList(
                build_meta_table(sel_info()),
                build_comp_table(gene_dataset)
            )
        })

        output[["n_selected"]] <- shiny::renderText({
            nrow(vol_r[["selected"]]())
        })

        output[["download_selected"]] <- shiny::downloadHandler(
            filename = function() {
                paste0("Selected_", format(Sys.time(), "%Y-%m-%d-%H.%M.%S"), ".csv")
            },
            content = function(file) {
                utils::write.csv(vol_r[["selected"]](), file, row.names = FALSE)
            }
        )

        upset_server("upset", dataset, p = sel[["p"]], adj_p = sel[["adj"]], comp = sel[["comp"]], log_fc_range = sel[["log_fc_range"]])
    }

    shiny::shinyApp(
        ui = ui,
        server = server,
        enableBookmarking = "url"
    )
}

#' Volcano and upset app
#'
#' Runs a Shiny app for gene upsetting results exploration.
#'
#' Allows file uploading.
#'
#' @export
app_upload <- function() {
    ui <- shiny::fluidPage(
        shiny::div(id = "upload_container", shiny::fileInput("upload", "Upload an rds file from maUEB::toptable1", accept = ".rds")),
        shiny::uiOutput("app"),
        theme = bslib::bs_theme(version = 5)
    )

    server <- function(input, output, session) {
        output$app <- shiny::renderUI({
            shiny::req(input$upload)
            dataset <- readRDS(input$upload$datapath) %>% format_vol_data()

            ui_after_upload <- function() {
                menu_elements <- menu_UI("menu", unique(dataset[["facet"]]))
                volcano_elements <- volcano_UI("volcano")

                alpha_column <-
                    shiny::div(
                        class = "col-auto",
                        shiny::div(
                            class = "card",
                            shiny::div(class = "card-header p-1", shiny::tags[["p"]](htmltools::HTML("&alpha;"), class = "mb-1 pe-1")),
                            shiny::div(
                                class = "card-body p-1",
                                shiny::fluidRow(
                                    class = "g-1",
                                    shiny::div(
                                        class = "col col-auto",
                                        menu_elements[["alpha"]][["p_val"]]
                                    ),
                                    shiny::div(
                                        class = "col col-auto",
                                        menu_elements[["alpha"]][["adj"]]
                                    )
                                ),
                                shiny::div(menu_elements[["comp"]][["comp"]])
                            )
                        )
                    )

                logfc_column <-
                    shiny::div(
                        class = "col-auto",
                        shiny::div(
                            class = "card h-100",
                            shiny::div(
                                class = "card-header p-1",
                                shiny::tags[["p"]]("Log FC", class = "mb-1 pe-1 ps-1")
                            ),
                            shiny::div(
                                class = "card-body p-1",
                                shiny::fluidRow(
                                    class = "g-0",
                                    shiny::div(
                                        class = "col col-auto",
                                        menu_elements[["b"]][["lt_enab"]]
                                    ),
                                    shiny::div(
                                        class = "col col-auto",
                                        menu_elements[["b"]][["lt"]]
                                    )
                                ),
                                shiny::fluidRow(
                                    class = "g-0",
                                    shiny::div(
                                        class = "col col-auto",
                                        menu_elements[["b"]][["gt_enab"]]
                                    ),
                                    shiny::div(
                                        class = "col col-auto",
                                        menu_elements[["b"]][["gt"]]
                                    )
                                )
                            )
                        )
                    )

                selected_column <-
                    shiny::div(
                        class = "col-auto",
                        shiny::div(
                            class = "card h-100",
                            shiny::div(
                                class = "card-header p-1",
                                shiny::tags[["p"]]("Selected entries", class = "mb-1 pe-1 ps-1")
                            ),
                            shiny::div(
                                class = "card-body p-1",
                                shiny::fluidRow(
                                    class = "g-0",
                                    shiny::div(
                                        class = "col col-auto",
                                        shiny::downloadButton("download_selected", shiny::tagList("Download ", shiny::textOutput("n_selected", inline = TRUE), "entries"), class = "btn-sm m-1de ")
                                    )
                                ),
                                shiny::fluidRow(
                                    class = "g-0 mt-2  justify-content-end",
                                    shiny::div(
                                        class = "col col-auto",
                                    )
                                )
                            )
                        )
                    )

                volcano_column <-
                    shiny::column( # chart column
                        width = 9,
                        shiny::div(
                            class = "card",
                            shiny::div(
                                class = "card-header p-1 text-muted",
                                "Volcano"
                            ),
                            shiny::div(
                                class = "card-body p-1 overflow-auto",
                                volcano_elements[["chart"]]
                            )
                        )
                    )


                info_column <- shiny::column( # info column
                    width = 3,
                    shiny::div(
                        class = "card h-100",
                        shiny::div(
                            class = "card-header p-1",
                            shiny::textOutput("hovered_name") %>% htmltools::tagAppendAttributes(class = "fw-bold")
                        ),
                        shiny::div(
                            class = "card-body p-1 overflow-auto",
                            shiny::uiOutput("hovered_body")
                        )
                    )
                )

                upset_column <-
                    shiny::column( # chart column
                        width = 12,
                        shiny::div(
                            class = "card",
                            shiny::div(class = "card-header p-1 text-muted", "Upset"),
                            shiny::div(
                                class = "card-body p-1",
                                upset_UI("upset")
                            )
                        )
                    )


                shiny::tagList(
                    shiny::fluidRow(
                        class = "m-1",
                        alpha_column,
                        logfc_column,
                        selected_column
                    ),
                    shiny::fluidRow(
                        class = "m-1",
                        volcano_column,
                        info_column
                    ),
                    shiny::fluidRow(
                        class = "m-1",
                        upset_column
                    )
                )
            }

            server_after_upload <- function(input, output, session) {
                sel <- menu_server("menu")

                shiny::observe({
                    shiny::reactiveValuesToList(input)
                    session$doBookmark()
                })
                # Update the query string
                shiny::onBookmarked(shiny::updateQueryString)

                sel_info <- shiny::reactiveVal(NULL)

                shiny::observe({
                    d <- try(vol_r[["hovered"]](), silent = TRUE)
                    shiny::req(!inherits(d, "try-error"))
                    sel_info(d)
                })

                shiny::observe({
                    purrr::iwalk(sel, ~ rlang::inform(paste(.y, .x())))
                })
                vol_r <- volcano_server("volcano", dataset, p = sel[["p"]], adj_p = sel[["adj"]], comp = sel[["comp"]], log_fc_range = sel[["log_fc_range"]])

                output[["hovered_name"]] <- shiny::renderText({
                    shiny::validate(
                        shiny::need(
                            !is.null(sel_info()) && nrow(sel_info()) > 0,
                            "Hover over a point"
                        )
                    )
                    sel_info()[["Gene.Symbol"]]
                })

                output[["hovered_body"]] <- shiny::renderUI({
                    shiny::req(sel_info())

                    gene_dataset <- dataset %>%
                        dplyr::filter(.data[["Gene.Symbol"]] == sel_info()[["Gene.Symbol"]])

                    build_comp_table <- function(x) {
                        t <- shiny::tags

                        comp_table <- purrr::pmap(.l = x, function(...) {
                            arg <- list(...)
                            t$tr(
                                t$th(scope = "row", arg[["facet"]]),
                                t$td(signif(arg[["log_fc"]], digits = 2)),
                                t$td(signif(arg[["adj_p"]], digits = 2)),
                                t$td(signif(arg[["unadj_p"]], digits = 2))
                            )
                        })

                        t$table(
                            class = "table",
                            t$thead(
                                t$tr(
                                    t$th(scope = "col", ""),
                                    t$th(scope = "col", "LogFC"),
                                    t$th(scope = "col", "Adj p"),
                                    t$th(scope = "col", "p")
                                )
                            ),
                            t$tbody(
                                comp_table
                            )
                        )
                    }

                    build_meta_table <- function(x) {
                        t <- shiny::tags


                        t$table(
                            class = "table",
                            t$tbody(
                                t$tr(
                                    t$td("EntrezID"),
                                    t$td(t$a(x[["EntrezID"]], href = paste0("https://www.ncbi.nlm.nih.gov/gene/", x[["EntrezID"]]), target = "_blank"))
                                ),
                                t$tr(
                                    t$td(
                                        "Probe"
                                    ),
                                    t$td(
                                        x[["Probe"]]
                                    )
                                )
                            )
                        )
                    }

                    shiny::tagList(
                        build_meta_table(sel_info()),
                        build_comp_table(gene_dataset)
                    )
                })

                output[["n_selected"]] <- shiny::renderText({
                    nrow(vol_r[["selected"]]())
                })

                output[["download_selected"]] <- shiny::downloadHandler(
                    filename = function() {
                        paste0("Selected_", format(Sys.time(), "%Y-%m-%d-%H.%M.%S"), ".csv")
                    },
                    content = function(file) {
                        utils::write.csv(vol_r[["selected"]](), file, row.names = FALSE)
                    }
                )

                upset_server("upset", dataset, p = sel[["p"]], adj_p = sel[["adj"]], comp = sel[["comp"]], log_fc_range = sel[["log_fc_range"]])
            }
            shiny::removeUI("#upload_container")
            server_after_upload(input, output, session)
            ui_after_upload()
        })
        shiny::uiOutput("app")
    }



    shiny::shinyApp(
        ui = ui,
        server = server,
        enableBookmarking = "url"
    )
}
