# About

Scripts to reproduce the data stored in [data](data/), i.e. the experiments performed.

# Contents

  - *run_all.R* runs all simulations used for the publication after creating the necessary directory structure in *data*. Please pay attention to the `wait_time` (`wait_time` * `steps` = `sim_duration`) parameter that is set in the setup scripts, as it decides the time it takes to run particular simulations. For instance, a wait_time of 1e3 on my Windows OS (32 GB) machine using the temporal method with 300 steps takes about 15 seconds. However, the time complexity of the algorithm is somewhere between $O(n^2)$ and $O(n^3)$.
  - *setup* scripts document the construction of a parameter set, while *run* scripts hold the function calls to run the respective simulations.
