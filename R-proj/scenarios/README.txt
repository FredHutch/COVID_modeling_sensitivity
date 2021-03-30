# The scenarios directory contains R files used to run various scenarios either for generating
# data for the Shiny App (https://covidmodeling.fredhutch.org) or for various diagrams in papers.
#
# The files can be grouped into 3 sets: 
# 1. Shiny App "worlds" and their support files namely
#    b117.R, b1351.R, p1.R and no_variant.R 
#    with support files: create_world_scenarios_new_rate.R and print_world_scenarios_new_rate.R
#    make_csv_worlds.sh and csv_from_scenarios.R to get CSV files for all scenarios in an rds file
#    
# 2. Sensitivity plots (updated to run with Dec calibration) namely
#    RATE_VEP_50_sens.R,RATE_VEP_90_sens.R,RATE_VES_50_sens.R for vaccine efficacy
#    TRIG_MAX.R  and TRIG_MIN_sens.R for social distancing triggers
#    SEVERITY_sens.R for b117 disease severity increases	
#
# 3. Plot files for papers (not updated to new calibration, email if you need to run these
#    For the Swan/Schiffer paper:
#       JS_figs_23_data.R JS_figs_23_table.R JS_paper_fig1_irresp.R JS_paper_fig1.R
#       JS_paper_fig5.R JS_paper_fig_new.R JS_paper_figs_2_3.R JS_paper_reff_fig.R JS_paper_suppl3.R
#       run_JS_fig1.sh run_JS_figs_23.sh run_JS_figs_345.sh
#    For the Reeves/Schiffer paper:
#       DR_paper_fig1b.R DR_paper_fig1.R DR_paper_fig2.R DR_paper_hms.R DR_paper_strains_0.9.R
#
#
To run a scenario...
1. cd to this directory
2. run "R --vanilla < [scenario file]
3. Look for rds file in ../../shiny_data or ../../data
4. Look for plots in ../shiny_out or ../out
