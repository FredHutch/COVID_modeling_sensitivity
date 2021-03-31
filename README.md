# COVID_modeling_sensitivity

This repository contains version 1.5 of a  mathematical model of COVID-19 transmission in King County.
It is has been used to test the effectiveness of different test-and-treat interventions, social distancing and vaccination policies.

This particular branch of the project supports the following papers:

Reeves DB, Bracis C, Swan DA, Moore M, Dimitrov D, Schiffer JT. Rapid vaccination and early reactive partial lockdown will minimize deaths from emerging highly contagious SARS-CoV-2 variants. medRxiv (2021).

Swan DA, Goyal A, Bracis C, Moore M, Krantz E, Brown E, Cardozo-Ojeda, Reeves DB, Gao F, Gilbert PB, Corey L, Cohen MS, Janes H, Dimitrov D, Schiffer JT. Vaccines that prevent SARS-CoV-2 transmission may prevent or dampen a spring wave of COVID-19 cases and deaths in 2021. medRxiv (2020).

Swan DA, Bracis C, Janes H, Moore M, Matrajt L, Reeves DB, Burns E, Donnell D, Cohen MS, Schiffer JT, Dimitrov D. COVID-19 vaccines that reduce symptoms but do not block infection need higher coverage and faster rollout to achieve population impact. medRxiv (2020).

It also supports the shiny-app representation of the model which can be viewed at https://covidmodeling.fredhutch.org.

The sub-directories are organized as follows:

R-proj - contains the following sub-directories for the R code

    functions - R files that implement various model functions

    scenarios - contains various local sensitivity scenarios and variant scenarios for the shiny app.
	cd to this directory to run those scenarios.

    sensitivity - contains files used in the global sensitivity analysis for the papers

    calibration - contains scripts to support compiling parameters to fit KC epidemic through 12/31/2020.

    shiny_out - plots to show what is in the rds files for the shiny app

    papers_data - rds files for the paper plots

    papers_out - pdf files for the paper plots

data - KC input data files used in calibration

shiny_data - the rds files for the shiny app

refs - references used when creating and parameterizing the model
