# # Uses upsetjs
UPSET_ID <- poc(
  CHART = "CHART"
)

#' Upset module
#'
#' Shows an upset chart
#'
#' @name upset_module
NULL

#' @describeIn upset_module UI
#'
#' @param id A Shiny ID
#'
#' @export
upset_UI <- function(id) {
  ns <- shiny::NS(id)
  upsetjs::upsetjsOutput(outputId = ns(UPSET_ID$CHART))
}

#' @describeIn upset_module server
#'
#' @param id Shiny ID
#' @param dataset Data as output by `format_vol_data`
#' @param p A reactive numerical indicating the p value limit
#' @param adj_p A reactive boolean indicating if adjusted p value should be used
#' @param log_fc_range A reactive numerical vector with two entries `lt` and `gt` indicating the limits for the Log FC
#'
#' @export
upset_server <- function(id, dataset, p, adj_p, comp, log_fc_range) {
  mod <- function(input, output, session) {
    ns <- session[["ns"]]
    output[[UPSET_ID$CHART]] <- upsetjs::renderUpsetjs({
      shiny::req(!is.null(adj_p()))
      y_col <- if (adj_p()) "adj_p" else "unadj_p"
      df <- dplyr::filter(
        dataset,
        .data[["facet"]] %in% comp(),
        .data[[y_col]] < p(),
        log_fc_range()[["lt"]] > .data[["log_fc"]] | .data[["log_fc"]] > log_fc_range()[["gt"]]
      ) %>%
        dplyr::select(
          .data[["Probe"]],
          .data[["facet"]]
        ) %>%
        dplyr::group_by(.data[["facet"]]) %>%
        dplyr::summarise(set = list(.data[["Probe"]])) %>%
        as.list()

      set_list <- purrr::list_merge(
        stats::setNames(vector("list", length(comp())), comp()),
        !!!stats::setNames(df[["set"]], df[["facet"]])
      )

      upsetjs::upsetjs() %>%
        upsetjs::fromList(set_list) %>%
        upsetjs::interactiveChart()
    }) %>%
      shiny::bindEvent(
        p(),
        adj_p(),
        comp(),
        log_fc_range()
      )
  }
  shiny::moduleServer(id, mod)
}
