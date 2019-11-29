# Autistic traits influence the strategic diversity of information sampling

To replicate the results and figures of the article, try to run

1. first `main.R`,
2. then `figures.R` and `figures_sup.R`

R (>= 3.5.3) are required; R packages mentioned in the codes also have to be installed first. Running codes in RStudio is optional.

## Data

In `Data` folder, there are four `.rds` files, including anonymous subject information and choice behavior results:

- `subject_data.rds`: Basic participant information
- `choice_data.rds`: Participants' choices and experiment conditions of each trial
- `rt_data.rds`: Participants' decision times of each key press and corresponding stimuli (blue or pink bead)
- `in_trial_sim.rds`: Pre-generated stimuli that a participant would encounter during the experiment, which was used to do simulation

Other files and sub-folders are for choice models and corresponding RT<sup>[1](#footnote1)</sup> models and for Bayesian model comparisons.

- `choice_rt_combined_model_(aicc|bic)(_output).csv`: Choice + RT models' model comparisons and outputs
- `choice_model` folder: Estimations of choice models' parameters and log-likehood
- `rt_model` folder:
    - `(ts|os)_.+_rt.csv`: RT-related data calculated by choice models (i.e., RT and probability), which was used to estimate RT models
    - `twelve_ts_plus_four_os` foler: Modeling results (`mr_`) for corresponding RT models

## Codes

Contained in `Codes` folder are mostly helper functions or codes for simulation used in `main.R`, which are either less necessary or too much too long to be written in the main code file.

Several code files were written in MATLAB; thus they need to be run in MATLAB environment:

- `group_level_MBS_choice_rt_combined.m`: Group-level Bayesian model comparison for Choice + RT models
- `choice_model` folder: Codes that fit various choice models
- `rt_model` folder: Codes that fit different RT models


<a name="footnote1">1</a>: "RT" here and in codes and data refers to decision time (DT) that appears in the main text.