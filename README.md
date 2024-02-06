# Off-policy Evaluation in Doubly Inhomogeneous Environments (JASA2023)

Article: Off-policy Evaluation in Doubly Inhomogeneous Environments.

The README.Rmd provides instructions on how to implement the code.

The enclosed R.file demonstrates the implementation of the proposed method for TWDIDP.

The Sensitivity folder contains codes of how to generate the four synthetic environments, each representing a different degree of deviation from the additive assumption, corresponds to the Scenarios 1, 2, 3, and 4 in the paper.

The D4RL folder contains a guidance of how to install the D4RL environment.

The TWDIDP.R files implements the proposed two-way doubly inhomogeneous decision process; the target.R and the MCMC.R were used to calculate the true value function; the mixdata.R was used to generate the observed data. The mimic.R file contains code for doing analysis in the paper and the pre-processed MIMIC datasets. The fitted-q.R file is the competing method for the fitted-q evaluation. The cartpole.ipynb file illustrate how to generate data from the OpenAI Gym environment Cartpole. 
