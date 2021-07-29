# A systematic comparison of methods for the validation of binary classification models

This repo contains scripts that were used for my thesis. All the scripts are written in R, and all the code is reproducible with the given random seeds. 

The ```Test``` folder contains mainly test files and the code for simulations in Chapter 3: ```3_External_effect.Rmd```.

The code for producing the results of Chapter 4 and 5 can be found in the ```Simulation``` folder as well as scripts to evaluate the results. 


### Structure overview

The ```Simulation``` folder contains all the scripts for the main experiments. In the parent folder, necessary scripts can be found for the simulations. The code is easily expandable for more experiments, it is convinient to add extra performance measures or classification methods.

The ```Simulation\running_simulations``` folder contains code for producing results. It contains all the scripts for simulation **4.A**, **4.B**, **4.C**, **5.A**, and **5.B**. It also contains slurm files, which were used for running the code on clusters. However, it is possible to run the code on personal computers as well, it will produce the same results. The results of the simulations are saved in ```Simulation\data``` folder as ```.Rdata``` files.

The ```Simulation\evaluation``` folder contains scripts to evaluate and produce plots from the results. ```evaluating_results.R``` script contains general functions to draw plots. ```evaluate_sim4.R``` contains code for Chapter 4 while ```evaluate_sim5.R``` contains code for Chapter 5.

Short description of the scripts for the simulations

- ```performance_measures.R``` contains a function that creates a list which can calculate all the performance measures with given outcomes y and prediction p. 
- ```predictive_models.R``` encodes classification rules as lists which have a fit and a predict object.
- ```data_generation.R``` contains functions to generate different kinds of data.
- ```validation_techniques.R``` contains functions to run the 7 different validation techniques, namely CV, RCV, PCV, PRCV, OB, 0.632 and 0.632+. For efficiency the bootstrap methods are calculated at once, as well as the pooled and unplooled CV methods.
- ```validation_simulation.R``` the highest level script. It combines all the previous code. This script is loaded for the simulations. 