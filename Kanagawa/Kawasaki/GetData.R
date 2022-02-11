##https://rstudio.github.io/reticulate/index.html

library(reticulate)
use_python("/usr/local/bin/python3")
use_virtualenv("/Users/momma/labs/pylab/Pandas/")
py_run_file("/Users/momma/labs/pylab/Pandas/a.py")
