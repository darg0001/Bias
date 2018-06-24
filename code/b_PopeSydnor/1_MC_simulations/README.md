# MC Simulations
The set of code 'a_*' corresponds with different simulations settings we consider, which we then plot figures for in 'b_simulation_figs.R'. The script 'c_general_MCvizFig3.R' corresponds with Figure 3 in the paper. For the simulation scripts, we briefly describe the variables used for setting the different parameters:
* N: the number of observations
* beta_1_SAP_array, beta_2_CP_array, and beta_3_SUP_array: varies $\beta_1$, $\beta_2$, and $\beta_3$
* displacement_array: corresponds with $\eta$ in the main paper's equation 3
* shift_sd_array: corresponds with $\sigma$ in the main paper's equation 3
* delta_0: varies relative class proportions
* delta_CP: varies amount of correlation between CP and SUP
* n_rep: controls how many iterations of random 80-20 splits to implement. (Note for our analysis, we split this file into smaller files instead of running one script of 100 iterations.)



