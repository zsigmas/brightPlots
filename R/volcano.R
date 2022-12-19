VOL_ID <- poc(
    CHART = "chart",
    TEXT = "text"
)

SIGNALS <- poc(
    B_VAL = "B_val"
)

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
                mlog10_unadj_p = -log10(unadj_p),
                mlog10_adj_p = -log10(adj_p)
            )
    })

    attr(dataset[["log_fc"]], "label") <- log_fc_col
    attr(dataset[["mlog10_unadj_p"]], "label") <- paste0("-log10(", unadj_p_col, ")")
    attr(dataset[["mlog10_adj_p"]], "label") <- paste0("-log10(", adj_p_col, ")")
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
get_vol_spec <- function(dataset, y = "adj_p", threshold = 0) {
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
                test = paste0("datum.y>", SIGNALS$B_VAL),
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
            list(name = SIGNALS$B_VAL, value = 0), # set different to input to force signal
            list(name = "highlight", select = list(type = "point", on = "mouseover"))
        ),
        columns = 2
    )

    spec[["facet"]] <- list(field = "facet")
    spec[["columns"]] <- 2

    spec[["spec"]] <- list(
        encoding = encoding,
        layer = layer,
        width = 150,
        height = 150
    )

    # spec[["facet"]] <- list(
    #     column = list(field = "facet", title = "Comparison")
    # )

    vegawidget::as_vegaspec(spec)
}

#' @export
volcano_UI <- function(id, as_tag_list = TRUE) {
    ns <- shiny::NS(id)

    ui <- list(
        chart = vegawidget::vegawidgetOutput(ns(VOL_ID$CHART), width = "auto"),
        text = shiny::textOutput(ns(VOL_ID$TEXT))
    )
    if (as_tag_list) {
        shiny::tagList(ui)
    } else {
        ui
    }
}

#' @export
volcano_server <- function(id, dataset, p, adj_p, comp) {
    mod <- function(input, output, session) {
        ns <- session[["ns"]]

        click <- shiny::reactiveVal(NULL)

        b_val <- shiny::reactive(
            {
                -log10(shiny::req(p()))
            },
            label = ns(" b")
        ) %>%
            shiny::bindEvent(
                comp(),
                p()
            )

        click <- vegawidget::vw_shiny_get_event(ns(VOL_ID$CHART), event = "click", body_value = "datum")
        vegawidget::vw_shiny_set_signal(ns(VOL_ID$CHART), name = SIGNALS$B_VAL, value = b_val())

        output[[VOL_ID$CHART]] <- vegawidget::renderVegawidget({            
            y_col <- if (adj_p()) "mlog10_adj_p" else "mlog10_unadj_p"
            dplyr::filter(dataset, .data[["facet"]] %in% comp()) %>%
                get_vol_spec(y = y_col, threshold = 0)
        }) %>% shiny::bindEvent(
            adj_p(),
            comp()
        )

        output[[VOL_ID$TEXT]] <- shiny::renderText({
            click()[["facet"]]
        })
    }
    shiny::moduleServer(id, mod)
}

