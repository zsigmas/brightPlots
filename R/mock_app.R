#' @export
mock_app <- function(dataset) {
    options("..__db_mode" = TRUE)

    if (missing(dataset)) {
        dataset <- format_vol_data(toptable)
    }

    menu_elements <- menu_UI("menu", unique(dataset[["facet"]]), as_tag_list = FALSE)
    volcano_elements <- volcano_UI("volcano", as_tag_list = FALSE)

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

    b_column <-
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
                            class = "col col-5",
                            menu_elements[["b"]][["lt_enab"]]
                        ),
                        shiny::div(
                            class = "col col-5",
                            menu_elements[["b"]][["lt"]]
                        )
                    ),
                    shiny::fluidRow(
                        class = "g-0",
                        shiny::div(
                            class = "col col-5",
                            menu_elements[["b"]][["gt_enab"]]
                        ),
                        shiny::div(
                            class = "col col-5",
                            menu_elements[["b"]][["gt"]]
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

    ui <- shiny::tagList(
        shiny::fluidPage(
            ..__db_input_UI("dev"),
            shiny::fluidRow(
                class = "m-1",
                alpha_column,
                b_column
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

    # )
    server <- function(input, output, session) {
        ..__db_input_server("dev")
        sel <- menu_server("menu")

        is_clicked <- shiny::reactiveVal(FALSE)
        sel_info <- shiny::reactiveVal(NULL)

        shiny::observe({
            d <- try(vol_r[["hovered"]](), silent = TRUE)
            shiny::req(!inherits(d, "try-error"))
            shiny::req(!is_clicked())
            sel_info(d)
        })

        shiny::observe({
            d <- try(vol_r[["clicked"]](), silent = TRUE)
            shiny::req(!inherits(d, "try-error"))
            is_clicked(TRUE)
            sel_info(d)
        })

        shiny::observe({
            purrr::iwalk(sel, ~ rlang::inform(paste(.y, .x())))
        })
        vol_r <- volcano_server("volcano", dataset, p = sel[["p"]], adj_p = sel[["adj"]], comp = sel[["comp"]], b_range = sel[["b_range"]])

        output[["hovered_name"]] <- shiny::renderText({
            shiny::validate(
                shiny::need(
                    !is.null(sel_info()) && nrow(sel_info()) > 0,
                    "Hover over a point or click to select"
                )
            )
            sel_info()[["Gene.Symbol"]]
        })

        output[["hovered_body"]] <- shiny::renderUI({
            shiny::req(sel_info())
            gene_dataset <- dataset %>%
                dplyr::filter(.data[["Gene.Symbol"]] == sel_info()[["Gene.Symbol"]])

            build_comp_table <- function(x) {
                shiny::withTags({
                    comp_table <- purrr::pmap(.l = x, function(...) {
                        arg <- list(...)
                        tr(
                            th(scope = "row", arg[["facet"]]),
                            td(signif(arg[["log_fc"]], digits = 2)),
                            td(signif(arg[["adj_p"]], digits = 2)),
                            td(signif(arg[["unadj_p"]], digits = 2))
                        )
                    })

                    table(
                        class = "table",
                        thead(
                            tr(
                                th(scope = "col", ""),
                                th(scope = "col", "LogFC"),
                                th(scope = "col", "Adj p"),
                                th(scope = "col", "p")
                            )
                        ),
                        tbody(
                            comp_table
                        )
                    )
                })
            }

            build_meta_table <- function(x) {
                shiny::withTags({
                    table(
                        class = "table",
                        tbody(
                            tr(
                                td("EntrezID"),
                                td(a(x[["EntrezID"]], href = paste0("https://www.ncbi.nlm.nih.gov/gene/", x[["EntrezID"]]), target = "_blank"))
                            ),
                            tr(
                                td(
                                    "Probe"
                                ),
                                td(
                                    x[["Probe"]]
                                )
                            )
                        )
                    )
                })
            }

            shiny::tagList(
                build_meta_table(sel_info()),
                build_comp_table(gene_dataset)
            )
        })

        upset_server("upset", dataset, p = sel[["p"]], adj_p = sel[["adj"]], comp = sel[["comp"]])
    }

    shiny::shinyApp(
        ui = ui,
        server = server
    )
}
