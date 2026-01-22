# Export to docs/ folder to deploy to Github Pages
req_packages <- c("shiny", "rjson", "ComplexHeatmap", "circlize", "dplyr", "htmltools", "tidyr", "shinyWidgets")
total_packages <- NULL
for (k in 1:length(req_packages)) {
  pkg <- tools::package_dependencies(req_packages[[k]], recursive = TRUE, db = available.packages())
  pkg2 <- pkg[[req_packages[[k]]]]
  total_packages <- c(total_packages, pkg2)
}
total_packages <- unique(total_packages)
total_packages <- c(total_packages, req_packages)
export_output <- capture.output({shinylive::export(appdir = "apps/brdv", destdir = "docs", packages = total_packages)})
cat(export_output, sep = "\n")
index_file <- "docs/index.html"
html_lines <- readLines(index_file)
html_lines <- gsub("<title>.*</title>", "<title>Bus Route Demand Visualiser 1.3.3</title>", html_lines)
cache_control1 <- '<meta http-equiv="Cache-Control" content="no-cache, no-store, must-revalidate">'
cache_control2 <- '<meta http-equiv="Pragma" content="no-cache">'
cache_control3 <- '<meta http-equiv="Expires" content="0">'
favicon_tag <- '<link rel="icon" type="image/png" href="www/stc-icon.png">'
head_close_index <- grep("</head>", html_lines)[1]
insertion <- c(cache_control1, cache_control2, cache_control3, favicon_tag)
html_lines <- append(html_lines, insertion, after = head_close_index - 1)
writeLines(html_lines, index_file)