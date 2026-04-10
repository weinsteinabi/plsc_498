# Run this script once to create the RDS data files for Week 12
# Requires: ggplot2, gapminder packages

library(ggplot2)
library(gapminder)

# Use script's own directory for output
out_dir <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
if (!nzchar(out_dir) || is.null(out_dir)) {
  out_dir <- "C:/Users/Jared_Edgerton/Dropbox/teaching_material/PSU/plsc_498/12_week/data"
}

# Economics data (from ggplot2)
econ <- ggplot2::economics
saveRDS(econ, file.path(out_dir, "economics.rds"))
write.csv(econ, file.path(out_dir, "economics.csv"), row.names = FALSE)

# Presidential terms data (from ggplot2)
pres <- ggplot2::presidential
saveRDS(pres, file.path(out_dir, "presidential.rds"))
write.csv(pres, file.path(out_dir, "presidential.csv"), row.names = FALSE)

# Gapminder data
gap <- gapminder::gapminder
saveRDS(gap, file.path(out_dir, "gapminder.rds"))
write.csv(gap, file.path(out_dir, "gapminder.csv"), row.names = FALSE)

cat("Week 12 data files created in:", out_dir, "\n")
