# Off-policy Evaluation in Doubly Inhomogeneous Environments (JASA2023)

Article: Off-policy Evaluation in Doubly Inhomogeneous Environments.

Author: Zeyu Bian, Chengchun Shi, Zhengling Qi and Lan Wang

Email: zeyu.bian@miami.edu

The enclosed R.file demonstrates the implementation of the proposed method for TWDIDP.

The README.Rmd provides instructions on how to implement the code. The TWDIDP.R files implements the proposed two-way doubly inhomogeneous decision process; the target.R and the MCMC.R were used to calculate the true value function; the mixdata.R was used to generate the observed data. The mimic.R file contains code for doing analysis in the paper and the pre-processed MIMIC datasets. The fitted-q.R file is the competing method for the fitted-q evaluation. Finally, the cartpole.ipynb file illustrate how to generate data from the OpenAI Gym environment Cartpole.
