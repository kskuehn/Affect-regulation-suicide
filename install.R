# Install the packages the pipeline needs. Stan requires a C++ toolchain:
# macOS: Xcode command-line tools; Windows: Rtools; see https://mc-stan.org/rstan/
pkgs <- c("brms", "rstan", "posterior", "mclust", "dplyr", "tidyr", "readr", "ggplot2", "lavaan")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
# Optional: pin versions for full reproducibility
#   install.packages("renv"); renv::init(); renv::snapshot()

# If Stan compilation is killed for lack of memory (common on 8 GB machines), drop debug
# symbols and optimisation level in ~/.R/Makevars:
#   CXX17FLAGS = -O1 -g0 -fPIC
