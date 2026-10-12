save_plots_safely <- function(plot_object, base_name, out_dir) {
  if (inherits(plot_object, "ggplot")) {
    # It's a single plot, save normally
    ggplot2::ggsave(filename = file.path(out_dir, paste0(base_name, ".png")), 
                    plot = plot_object, width = 8, height = 6, dpi = 300)
  } else if (is.list(plot_object)) {
    # It's a list of multiple plots, loop through them
    plot_names <- names(plot_object)
    for (i in seq_along(plot_object)) {
      # Determine a distinct name suffix for each plot
      suffix <- if (!is.null(plot_names) && plot_names[i] != "") plot_names[i] else i
      file_name <- paste0(base_name, "_", suffix, ".png")
      
      ggplot2::ggsave(filename = file.path(out_dir, file_name), 
                      plot = plot_object[[i]], width = 8, height = 6, dpi = 300)
    }
  }
}