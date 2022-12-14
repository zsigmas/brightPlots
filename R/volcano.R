DS_COL <- poc()

#' Reformat volcano dataset for plotting
#' Expects as input the output of maUEB::dea_toptable1()
#'
#' @param dataset_list A named list of datasets where each entry contains the result of the comparison and is named after it.
#' @param log_fc_col The name of the column that contains the log fold value
#' @param unadj_p_col The name of the column that contains the unadjusted p value
#' @param adj_p_col The name of the column that contains the adjusted p value
#' @param tooltip_cols A character vector indicating which fields will be included in addition to the previous columns, normally used as tooltips in the graph
#'
#' @export
format_vol_data <- function(dataset_list, log_fc_col = "logFC", unadj_p_col = "P.Value", adj_p_col = "adj.P.Val", extra_cols = c("Gene.Symbol", "EntrezID")) {
    dataset <- purrr::imap_dfr(dataset_list, ~ {
        .x %>%
            tibble::as_tibble(rownames = "Probe") %>%
            dplyr::select(
                dplyr::all_of(c("Probe", log_fc = log_fc_col, unadj_p = unadj_p_col, adj_p = adj_p_col, extra_cols))
            ) %>%
            dplyr::mutate(
                facet = .y,
                unadj_p = -log10(unadj_p),
                adj_p = -log10(adj_p)
            )
    })

    attr(dataset[["log_fc"]], "label") <- log_fc_col
    attr(dataset[["unadj_p"]], "label") <- paste("-log10(", unadj_p_col, ")")
    attr(dataset[["adj_p"]], "label") <- paste("-log10(", adj_p_col, ")")
    dataset
}

#' Reformat volcano dataset for plotting
#' Expects as input the output of maUEB::dea_toptable1()
#'
#' @param dataset a dataset from format_vol_vd
#' @param y the name of the column to use for y values
#' @param threshold value for the y axis thresholding. If used in a shiny app use the default value
#'
#' @export
# The threshold value is random to force vegawidget to interpret it as a signal
get_vol_spec <- function(dataset, y = "adj_p", threshold = stats::rnorm(1)) {
    dataset <- dplyr::mutate(dataset, y = .data[[y]])
    encoding <- list(
        x = list(
            field = "log_fc",
            type = "quantitative",
            title = attr(dataset[["log_fc"]], "label") %||% "log_fc"
        ),
        y = list(
            field = "y",
            title = attr(dataset[["y"]], "label") %||% y,
            type = "quantitative"
        ),
        color = list(
            condition = list(
                test = "datum.y>p_val",
                value = "red"
            ),
            value = "grey"
        )
        # ,
        # opacity = list(
        #     condition = list(
        #         test = "datum.Probe == highlight.Probe",
        #         empty = FALSE,
        #         value = 1
        #     ),
        #     value = .2
        # )
    )

    layer <- list(
        # list(mark = list(type = "line")),
        list(mark = list(type = "point", tooltip = list(
            "content" = "data"
        )))
    )

    spec <- list(
        `$schema` = vegawidget::vega_schema(), # specifies Vega-Lite
        usermeta = list(embedOptions = vegawidget::vega_embed(renderer = "canvas", actions = list(editor = TRUE))),
        description = "Volcano Plot",
        data = list(values = dataset),
        params = list(
            list(name = "p_val", value = 0), # set different to input to force signal
            list(name = "highlight", select = list(type = "point", on = "mouseover"))
        )
    )

    spec[["spec"]] <- list(
        encoding = encoding,
        layer = layer,
        width = 200,
        height = 200
    )

    spec[["facet"]] <- list(
        column = list(field = "facet", title = "Comparison")
    )

    vegawidget::as_vegaspec(spec)
}

#' @export
volcano_UI <- function(id, dataset, as_tag_list = TRUE) {
    ns <- shiny::NS(id)
    comparisons <- unique(dataset[["facet"]])

    menu <- list(
        adj = shiny::checkboxInput(ns("use_adj"), label = "Adjusted p?", value = TRUE),
        comparisons = shiny::selectInput(ns("facets"), label = "Select comparisons", choices = comparisons, multiple = TRUE, selected = comparisons),
        p_val = shiny::sliderInput(ns("p_val"), label = "Select pval", min = 0, max = 1, step = .01, value = .05)
    )

    chart <- list(
        chart = vegawidget::vegawidgetOutput(ns("chart")),
        text = shiny::textOutput(ns("clicked"))
    )

    ui <- list(
        menu = menu,
        chart = chart
    )

    if (as_tag_list) {
        t <- shiny::tags
        menu <- t$ol(
            class = "list-group",
            t$li(
                class = "list-group-item d-flex justify-content-between align-items-start",
                t$div(
                    class = "ms-2 me-auto",
                    t$div(
                        class = "fw-bold",
                        "p-value"
                    ),
                    t$div(
                        ui[["menu"]][["adj"]],
                        ui[["menu"]][["p_val"]]
                    )
                )
            ),
            t$li(
                class = "list-group-item",
                t$div(
                    class = "ms-2 me-auto",
                    t$div(
                        class = "fw-bold",
                        "Comparisons"
                    ),
                    t$div(
                        ui[["menu"]][["comparisons"]]
                    )
                )
            )
        )

        shiny::tagList(
            menu,
            shiny::tagList(ui[["chart"]])
        )
    } else {
        ui
    }
}

#' @export
volcano_server <- function(id, dataset) {
    mod <- function(input, output, session) {
        ns <- session[["ns"]]
        y_col <- shiny::reactive({
            if (input[["use_adj"]]) "adj_p" else "unadj_p"
        })

        p_val_db <- shiny::reactive({
            -log10(shiny::req(input[["p_val"]]))
        }) %>%
            shiny::bindEvent( # We invalidate in more cases than expected otherwise the initial signal is not sent to the vegawidget
                input[["p_val"]],
                input[["use_adj"]],
                input[["facets"]]
            ) %>%
            shiny::debounce(500)

        facets_db <- shiny::reactive(input[["facets"]]) %>%
            shiny::debounce(500)

        click <- vegawidget::vw_shiny_get_event(ns("chart"), event = "click", body_value = "datum")
        vegawidget::vw_shiny_set_signal(ns("chart"), name = "p_val", value = p_val_db())


        output[["chart"]] <- vegawidget::renderVegawidget({
            dplyr::filter(dataset, facet %in% facets_db()) %>%
                get_vol_spec(y = y_col())
        })

        output[["clicked"]] <- shiny::renderText({
            click()[["facet"]]
        })
    }
    shiny::moduleServer(id, mod)
}

#' @export
mock_volcano <- function(dataset) {
    if (missing(dataset)) {
        dataset <- format_vol_data(toptable)
    }
    ui <- shiny::fluidPage(
        volcano_UI("volcano", dataset),
        theme = bslib::bs_theme(version = 5)
    )
    server <- function(input, output, session) {
        volcano_server("volcano", dataset)
    }

    shiny::shinyApp(
        ui = ui,
        server = server
    )
}
