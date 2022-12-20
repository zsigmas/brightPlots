# # Uses upsetjs
UPSET_ID <- poc(
    CHART = "CHART"
)
# listInput <- list(one = c(1, 2, 3, 5, 7, 8, 11, 12, 13), two = c(1, 2, 4, 5, 10), three = c(1, 5, 6, 7, 8, 9, 10, 12, 13))
# upsetjs() %>% fromList(listInput) %>% interactiveChart()
# upsetjs::ren
#' @export
upset_UI <- function(id){
    ns <- shiny::NS(id)
    upsetjs::upsetjsOutput(outputId = ns(UPSET_ID$CHART))
}
#' @export
upset_server <- function(id, dataset, p, adj_p, comp){
    mod <- function(input, output, session) {
        ns <- session[["ns"]]
        output[[UPSET_ID$CHART]] <- upsetjs::renderUpsetjs({            
            shiny::req(!is.null(adj_p()))
            y_col <- if (adj_p()) "adj_p" else "unadj_p"
            df <- dplyr::filter(
                dataset,
                .data[["facet"]] %in% comp(),
                .data[[y_col]] < p()
                ) %>%
            dplyr::select(
                Probe,
                facet
            ) %>% 
            dplyr::group_by(facet) %>% 
            dplyr::summarise(set = list(Probe)) %>%
            as.list()

            set_list <- purrr::list_merge(
                stats::setNames(vector("list", length(comp())), comp()),
                !!!stats::setNames(df$set, df$facet)
                )

            upsetjs::upsetjs() %>%
            upsetjs::fromList(set_list) %>%
            upsetjs::interactiveChart()
        }) %>%
        shiny::bindEvent(
            p(),
            adj_p(),
            comp()
        )
    }
    shiny::moduleServer(id, mod)
}