# Setup script to configure R to use Python from climate4R conda environment

# Get the conda environment path
conda_env_path <- "/mnt/CORDEX_CMIP6_tmp/user_tmp/jbediajimenez/conda_envs/climate4R"
python_path <- file.path(conda_env_path, "bin", "python")

# set RETICULATE_PYTHON before loading reticulate
Sys.setenv(RETICULATE_PYTHON = python_path)

# Add conda lib a .libPaths()
r_lib_path <- file.path(conda_env_path, "lib", "R", "library")
.libPaths(c(r_lib_path, .libPaths()))

if (!require("reticulate", quietly = TRUE)) {
  install.packages("reticulate",
                   repos = "https://cran.r-project.org/",
                   lib = r_lib_path)
  library(reticulate)
}

# Use python from conda environment
reticulate::use_python(python_path, required = TRUE)
# reticulate::py_config()
