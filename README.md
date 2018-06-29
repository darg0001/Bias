## Replication code for ``When Algorithms Import Private Bias into Public Enforcement: The Promise and Limitations of Statistical De-Biasing Solutions''

### Documentation
This repository contains all the corresponding code to replicate tables and figures in ``When Algorithms Import Private Bias into Public Enforcement: The Promise and Limitations of Statistical De-Biasing Solutions''.

### Directions
1. Save the raw Yelp data called 'instances_mergerd_seattle.csv' from Kang et al. 2013 <a href="http://www3.cs.stonybrook.edu/~junkang/hygiene/">[link]</a> to the data folder in this repo.

2. Code organization assumes the working directory is the same as a specific code's folder location.

3. Run code which is briefly described below:
    * a_EthnicBias/ - includes all relevant code for first part of the paper showing consumers are more likely to issue complaints of (or use terms indicating) food poisoning for Asian establishments, holding constant violation scores assigned by professional food safety inspectors.
    * b_PopeSydnor/ -  includes code for the second half of the paper which via Monte Carlo simulation to examine a de-biasing solution offered by Pope and Sydnor (2011) and then applied the P&S approach to 311 Call Complaint Data and Yelp Reviews from Kang et al.

All code was written and tested for Python 2.7.12
    

### Authors
* Kristen M. Altenburger, kaltenb@stanford.edu
* Daniel E. Ho*, dho@law.stanford.edu

[ * corresponding author]
