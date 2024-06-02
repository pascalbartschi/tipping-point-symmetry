
# About ![Badge](https://mfr.osf.io/render?url=https://osf.io/download/hr67w/?direct%26mode=render)


This repository holds the scripts used to run experiments, the resulting data  and complementary analysis of asymmetric result to understand and
reproduce the research presented in the paper: "[Reflecting on the symmetry of ecosystem tipping points. The influence of trait dissimilarity and environmental driver dynamics in a simple ecosystem model.](link)". 

# Documentation

This [user guide](https://uzh-peg.r-universe.dev/articles/microxanox/SymSys-User-guide.html) explains functionalities to simulate, analyse and visulualize experiments for this project. 


# Installation

The latter funcionalities have been implemented in the [microxanox package](https://github.com/UZH-PEG/microxanox) and are available in package version [0.9.3](https://zenodo.org/records/10119025) or newer.

It is best installed through the r-unviverse:

```r
install.packages('microxanox', repos = c('https://uzh-peg.r-universe.dev', 'https://cloud.r-project.org'))
```

# Contents

### [analysis](analysis/)

 Scripts to analyze the result, as well as the code used for the plotting:
  - *asymmetric_response_analysis.R* and *asymmetric_response_plotting* contain the code to apply certain measurements on a folder of .RDS data, and to plot the correlation of these simulation data
  - *publication_figures.rmd* is the markdown file used to produce the figures of the publication

### [data](data/)

.RDS files of the simulation visualized in the publication

  - [asymmetric-sim](data/asymmetric-sim/): contains to daughter directories *trait_asym* and *stressor_asym*, holding the responses to trait asymmetry, and asymmetry in driver patterns respectively.
  - [bush-sim](data/bush-sim/) contains one file, the reproduced simulation of [Bush et al, 2017](https://www.nature.com/articles/s41467-017-00912-x)
  - [symmetric-sim](data/symmetric-sim): contains two files, one resulting from the temporal method and the other one from running dynamics traditionally (time_dynamics)

### [experiments](experiments)

Scripts to reproduce the data stored in [data](data/), i.e. the experiments performed.

  - *run_all.R* runs all simulations used for the publication after creating the necessary directory structure in *data*. Please pay attention to the `wait_time` (`wait_time` * `steps` = `sim_duration`) parameter that is set in the setup scripts, as it decides the time it takes to run particular simulations. For instance, a wait_time of 1e3 on my Windows OS (32 GB) machine using the temporal method with 300 steps takes about 15 seconds. However, the time complexity of the algorithm is somewhere between $O(n^2)$ and $O(n^3)$.
  - *setup* scripts document the construction of a parameter set, while *run* scripts hold the function calls to run the respective simulations.
