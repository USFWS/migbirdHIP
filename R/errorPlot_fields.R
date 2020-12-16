#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tibble
#' @import ggplot2

errorPlot_fields <-
  # x = proofed data tibble
  # loc = "all" or specific state abbreviation
  function(x, loc = "all"){

    # Plot errors by field name
    fields_plot <-
      suppressWarnings(
        if(loc == "all"){
          x %>%
            select(errors) %>%
            mutate(temp_key = row_number()) %>%
            separate(errors, into = as.character(c(1:25)), sep = "-") %>%
            pivot_longer(1:25, names_to = "name") %>%
            select(-name) %>%
            rename(errors = value) %>%
            filter(!is.na(errors)) %>%
            ggplot() +
            geom_bar(aes(x = errors), stat = "count") +
            geom_text(
              stat = "count", aes(x = errors, label = stat(count), angle = 90),
              vjust = 0.2,
              hjust = -0.2
            ) +
            labs(x = "Field", y = "Error Count", title = "Errors per field") +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
        }
        else{
          x %>%
            filter(state == loc) %>%
            select(errors) %>%
            mutate(temp_key = row_number()) %>%
            separate(errors, into = as.character(c(1:25)), sep = "-") %>%
            pivot_longer(1:25, names_to = "name") %>%
            select(-name) %>%
            rename(errors = value) %>%
            filter(!is.na(errors)) %>%
            ggplot() +
            geom_bar(aes(x = errors), stat = "count") +
            geom_text(
              stat = "count", aes(x = errors, label = stat(count), angle = 90),
              vjust = 0.2,
              hjust = -0.2
            ) +
            labs(x = "Field", y = "Error Count",
                 title = paste0("Errors per field", " in ", loc)) +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
        }
      )

    return(fields_plot)

  }
