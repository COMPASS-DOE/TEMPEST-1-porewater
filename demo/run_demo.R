# Run with Rscript; all file locations are relative to this script.
args <- commandArgs(trailingOnly = FALSE)
script_arg <- args[startsWith(args, "--file=")]
if (length(script_arg) != 1L) stop("Run this example with Rscript demo/run_demo.R")
demo_dir <- dirname(normalizePath(sub("^--file=", "", script_arg)))
started <- proc.time()[["elapsed"]]

dat <- read.csv(file.path(demo_dir, "demo_doc.csv"), stringsAsFactors = FALSE)
stopifnot(all(is.finite(dat$doc_mg_l)), all(dat$doc_mg_l > 0))
groups <- split(dat, interaction(dat$date, dat$plot, drop = TRUE))
result <- do.call(rbind, lapply(groups, function(x) {
  stopifnot(nrow(x) >= 2)
  data.frame(date = x$date[1], plot = x$plot[1], n = nrow(x),
             mean_doc_mg_l = mean(x$doc_mg_l),
             sd_doc_mg_l = sd(x$doc_mg_l),
             se_doc_mg_l = sd(x$doc_mg_l) / sqrt(nrow(x)))
}))
result <- result[order(result$date, result$plot), ]
rownames(result) <- NULL
control <- result[result$plot == "Control", ]
control_mean <- control$mean_doc_mg_l[match(result$date, control$date)]
stopifnot(all(is.finite(control_mean)), all(control_mean > 0))
result$effect_percent <- 100 * (result$mean_doc_mg_l - control_mean) / control_mean

# The supplied reference was calculated independently from the CSV using Python.
expected <- read.csv(file.path(demo_dir, "expected_output.csv"), stringsAsFactors = FALSE)
comparison <- all.equal(result, expected, tolerance = 1e-10, check.attributes = FALSE)
if (!isTRUE(comparison)) stop("Expected-output check failed: ", paste(comparison, collapse = "; "))

output_dir <- file.path(demo_dir, "output")
dir.create(output_dir, showWarnings = FALSE)
write.csv(result, file.path(output_dir, "doc_summary.csv"), row.names = FALSE)

pdf(file.path(output_dir, "doc_demo.pdf"), width = 8, height = 4)
par(mfrow = c(1, 2), mar = c(5, 4, 3, 1))
colors <- c(Control = "#444444", Freshwater = "#0072B2", Saltwater = "#D55E00")
dates <- sort(unique(as.Date(result$date)))
plot(range(dates), c(0, max(result$mean_doc_mg_l + result$sd_doc_mg_l) * 1.1),
     type = "n", xlab = "Collection date", ylab = "DOC (mg C/L)", main = "Mean DOC +/- SD")
for (p in names(colors)) {
  x <- result[result$plot == p, ]; d <- as.Date(x$date)
  lines(d, x$mean_doc_mg_l, type = "b", pch = 16, col = colors[p])
  arrows(as.numeric(d), pmax(0, x$mean_doc_mg_l - x$sd_doc_mg_l),
         as.numeric(d), x$mean_doc_mg_l + x$sd_doc_mg_l,
         angle = 90, code = 3, length = 0.04, col = colors[p])
}
legend("topleft", legend = names(colors), col = colors, lty = 1, pch = 16, bty = "n", cex = 0.8)
plot(range(dates), range(c(0, result$effect_percent)), type = "n",
     xlab = "Collection date", ylab = "Difference from Control (%)", main = "Relative DOC difference")
abline(h = 0, lty = 2, col = "grey")
for (p in c("Freshwater", "Saltwater")) {
  x <- result[result$plot == p, ]
  lines(as.Date(x$date), x$effect_percent, type = "b", pch = 16, col = colors[p])
}
invisible(dev.off())
capture.output(sessionInfo(), file = file.path(output_dir, "session_info.txt"))
print(result, row.names = FALSE)
cat("\nPASS: all nine summary rows match expected_output.csv.\n")
cat("Outputs:", output_dir, "\nElapsed seconds:", proc.time()[["elapsed"]] - started, "\n")
