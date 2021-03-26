#!/bin/tcsh
date
setenv dist "seniors"
setenv age_code 4
setenv trig_min $1
setenv trig_max $2
setenv trig_perc 0
setenv strain_inc $3
setenv cover $4
setenv imports $5

rm -f mut_sens_$trig_min\_$trig_max\_$trig_perc\_$strain_inc\_$cover\_$imports\.csv

foreach vac_rate (2000 5000 8000 11000)
#foreach vac_rate (5000)
    foreach max_sd (0.4 0.5 0.6)
    #foreach max_sd (0.6)
	foreach min_sd (0.2 0.1 0)
	#foreach min_sd (0.2)
	    foreach vei (0.1 0.5 0.9)
	    #foreach vei (0.1)
    		setenv rds_file "../sens_data/$dist\_vei_$vei\_sdmin_$min_sd\_sdmax_$max_sd\_rate_$vac_rate\_mut_$strain_inc\_trigmin_$trig_min\_trigmax_$trig_max\_trigperc_0_cover_$cover\_import_$imports\.rds"
		echo "Filename is $rds_file"
		if (! -e $rds_file ) then
		    echo "Running cmd: Rscript full_sensitivity.R $dist $age_code $vei $vac_rate $strain_inc $min_sd $max_sd $trig_min $trig_max $trig_perc $cover $imports"
		    Rscript full_sensitivity.R $dist $age_code $vei $vac_rate $strain_inc $min_sd $max_sd $trig_min $trig_max $trig_perc $cover $imports
		else
		    echo "Skipping full_sensitivity.R"
		endif
		echo "Running cmd: Rscript sens_table_w_infs.R $dist $vei $vac_rate $strain_inc $min_sd $max_sd $trig_min $trig_max $trig_perc $cover $imports"
		Rscript sens_table_w_infs.R $dist $vei $vac_rate $strain_inc $min_sd $max_sd $trig_min $trig_max $trig_perc $cover $imports
		cat sens_out/$dist\_vei_$vei\_sdmin_$min_sd\_sdmax_$max_sd\_rate_$vac_rate\_mut_$strain_inc\_trigmin_$trig_min\_trigmax_$trig_max\_trigperc_0_cover_$cover\_import_$imports\.csv | grep -v "scenario" | grep -v "epidemic" >> mut_sens_$trig_min\_$trig_max\_$trig_perc\_$strain_inc\_$cover\_$imports\.csv
	    end
	end
    end
end
