VOL_ID <- poc(
    CHART = "chart",
    HOVERED = "hovered",
    TABLE = "table"
)

#' Reformat volcano dataset for plotting
#'
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

#' @export
volcano_UI <- function(id, as_tag_list = TRUE) {
    ns <- shiny::NS(id)

    ui <- list(
        chart = shiny::plotOutput(ns(VOL_ID$CHART), click = ns("plot_click"), hover = shiny::hoverOpts(id = ns("plot_hover"), nullOutside = TRUE, delay = 100), width = "100%"),
        hovered = shiny::uiOutput(ns(VOL_ID$HOVERED))
    )
    if (as_tag_list) {
        shiny::tagList(ui)
    } else {
        ui
    }
}

#' @export
volcano_server <- function(id, dataset, p, adj_p, comp, log_fc_range) {
    mod <- function(input, output, session) {
        ns <- session[["ns"]]

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

        d <- shiny::reactive({
            y_col <- if (adj_p()) "mlog10_adj_p" else "mlog10_unadj_p"
            d <- dplyr::mutate(dataset, y = .data[[y_col]])
            dplyr::filter(
                d,
                .data[["facet"]] %in% comp()
            )
        })

        selected <- shiny::reactive({             
                    dplyr::filter(d(),
                        .data[["y"]] >= b_val(),
                    log_fc_range()[["lt"]] > .data[["log_fc"]] | .data[["log_fc"]] > log_fc_range()[["gt"]]
                    )            
        })

        output[[VOL_ID$CHART]] <- shiny::renderPlot(
            {                
                c_d <- d() %>%
                    dplyr::mutate(
                        Sig = dplyr::if_else(
                            (.data[["y"]] >= b_val()) &
                                (log_fc_range()[["lt"]] > .data[["log_fc"]] | .data[["log_fc"]] > log_fc_range()[["gt"]]),
                            "Yes", "No"
                        ),
                        alpha = dplyr::if_else(
                            (.data[["y"]] >= b_val()) &
                                (log_fc_range()[["lt"]] > .data[["log_fc"]] | .data[["log_fc"]] > log_fc_range()[["gt"]]),
                            1, .2
                        )
                    )

                p <- ggplot2::ggplot(c_d, mapping = ggplot2::aes(x = .data[["log_fc"]], y = .data[["y"]], colour = .data[["Sig"]], alpha = .data[["alpha"]])) +
                    ggplot2::geom_point(size = 3) +
                    ggplot2::facet_wrap(~facet, ncol = 3) +
                    ggthemes::theme_tufte(base_size = 20) +
                    ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA, color = "black"), legend.position = "None") +
                    ggplot2::labs(y = attr(d()[["y"]], "label") %||% "y", x = attr(d()[["log_fc"]], "label") %||% "x") +
                    ggplot2::theme(aspect.ratio = 1)

                # Draw reference lines

                if (isTRUE(is.numeric(log_fc_range()[["gt"]]))) {
                    p <- p + ggplot2::geom_vline(xintercept = log_fc_range()[["gt"]], linetype = 2)
                }

                if (isTRUE(is.numeric(log_fc_range()[["lt"]]))) {
                    p <- p + ggplot2::geom_vline(xintercept = log_fc_range()[["lt"]], linetype = 2)
                }

                if (isTRUE(is.numeric(b_val()))) {
                    p <- p + ggplot2::geom_hline(yintercept = b_val(), linetype = 2)
                }

                # Return the csv

                # Return a box with selected values

                p
            }
        ) %>% shiny::bindEvent(
            d(),
            b_val(),
            log_fc_range()
        )

        hovered <- shiny::reactive({
            np <- shiny::nearPoints(d(), input[["plot_hover"]], xvar = "log_fc", yvar = "y", threshold = 10)
            shiny::validate(
                shiny::need(nrow(np) > 0, "Hover over a point. Click on it to lock.")
            )            
            np[1, ]
        })

        output[[VOL_ID$HOVERED]] <- shiny::renderText({
            hovered()[["facet"]]
        })

        return(
            list(hovered = hovered, selected = selected)
        )
    }
    shiny::moduleServer(id, mod)
}
