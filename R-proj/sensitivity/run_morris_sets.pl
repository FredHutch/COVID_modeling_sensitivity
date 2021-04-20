#!/usr/bin/perl
# Perl script that takes a morris file , sets the parameters and runs the sims
#
#VEsusc: 10%, 100%
#VEinf: 10%, 100%
#VEsymp: 10%, 100%
#Vaccination rate: 2000, 15000 
#Release case threshold: 20, 200 
#Lockdown case threshold: 200, 800 
#Minimal social distancing proportion: 0, 30% (+ 20% for seniors)
#Maximum social distancing proportion: 50%, 70% (+ 20% for seniors)
#Coverage before switching age-group priority: 60%,100%
#External exposure rate (variant infection imports): 0.05/day - 1.25/day


my $inpfile;

if ( -f $ARGV[0]) {
    $inpfile=$ARGV[0]
} else {
    die "1st arg is the sobol file (8 comma separated scale factors per line)\n";
    exit (1);
}
open(INP,"<$inpfile");

my $dist="seniors";
my $age_code=4;

my $vac_rate_min= 2000;
my $vac_rate_max= 15000;
my $vac_rate_range= $vac_rate_max - $vac_rate_min;

my $vac_eff_p_min= 0.1;
my $vac_eff_p_max= 1.0;
my $vac_eff_p_range= $vac_eff_p_max - $vac_eff_p_min;

my $vac_eff_i_min= 0.1;
my $vac_eff_i_max= 1.0;
my $vac_eff_i_range= $vac_eff_i_max - $vac_eff_i_min;

my $vac_eff_s_min= 0.1;
my $vac_eff_s_max= 1.0;
my $vac_eff_s_range= $vac_eff_s_max - $vac_eff_s_min;

my $sd_lower_min= 0;
my $sd_lower_max= 0.3;
my $sd_lower_range=$sd_lower_max - $sd_lower_min;

my $sd_upper_min= 0.5;
my $sd_upper_max= 0.7;
my $sd_upper_range=$sd_upper_max - $sd_upper_min;

my $sd_loosen_min= 20;
my $sd_loosen_max= 200;
my $sd_loosen_range= $sd_loosen_max - $sd_loosen_min;

my $sd_tighten_min= 200;
my $sd_tighten_max= 800;
my $sd_tighten_range= $sd_tighten_max - $sd_tighten_min;

my $mut_inf_min= 1;
my $mut_inf_max= 1.75;
my $mut_inf_range= $mut_inf_max - $mut_inf_min;

my $coverage_min= 0.6;
my $coverage_max= 1;
my $coverage_range= $coverage_max - $coverage_min;

my $imports_min= 0.05;
my $imports_max= 1.25;
my $imports_range= $imports_max - $imports_min;

system("rm -f sens_$inpfile");

while( $line=<INP>) {
    chomp($line);              # remove the newline from $line.
    my @pieces = split(',',$line);
    my $vac_rate = int(1000 * ($vac_rate_range * $pieces[0] + $vac_rate_min)+0.5)/1000.0;
    my $vac_eff_p = int(1000 * ($vac_eff_p_range * $pieces[1] + $vac_eff_p_min)+0.5)/1000.0;
    my $vac_eff_i = int(1000 * ($vac_eff_i_range * $pieces[2] + $vac_eff_i_min)+0.5)/1000.0;
    my $vac_eff_s = int(1000 * ($vac_eff_s_range * $pieces[3] + $vac_eff_s_min)+0.5)/1000.0;
    my $sd_lower = int(1000 * ($sd_lower_range * $pieces[4] + $sd_lower_min)+0.5)/1000.0;
    my $sd_upper = int(1000 * ($sd_upper_range * $pieces[5] + $sd_upper_min)+0.5)/1000.0;
    my $sd_loosen = int(1000 * ($sd_loosen_range * $pieces[6] + $sd_loosen_min)+0.5)/1000.0;
    my $sd_tighten = int(1000 * ($sd_tighten_range * $pieces[7] + $sd_tighten_min)+0.5)/1000.0;
    my $mut_inf = int(1000 * ($mut_inf_range * $pieces[8] + $mut_inf_min)+0.5)/1000.0;
    my $coverage = int(1000 * ($coverage_range * $pieces[9] + $coverage_min)+0.5)/1000.0;
    my $imports = int(1000 * ($imports_range * $pieces[10] + $imports_min)+0.5)/1000.0;

    my $rds_file="../sens_data/$dist\_vei_$vac_eff_i\_ves_$vac_eff_s\_vep_$vac_eff_p\_sdmin_$sd_lower\_sdmax_$sd_upper\_rate_$vac_rate\_mut_$mut_inf\_trigmin_$sd_loosen\_trigmax_$sd_tighten\_cover_$coverage\_import_$imports\.rds";
    print("Filename: $rds_file\n");

    if (! -e $rds_file) {
	print("Running: Rscript one_run.R $dist $age_code $vac_eff_i $vac_eff_s $vac_eff_p $vac_rate $mut_inf $sd_lower $sd_upper $sd_loosen $sd_tighten $coverage $imports\n");
	system("Rscript one_run.R $dist $age_code $vac_eff_i $vac_eff_s $vac_eff_p $vac_rate $mut_inf $sd_lower $sd_upper $sd_loosen $sd_tighten $coverage $imports");
    } else {
	print "Skipped creation of $rds_file\n";
    }
    system("Rscript one_run_table.R $dist $vac_eff_i $vac_eff_s $vac_eff_p $vac_rate $mut_inf $sd_lower $sd_upper $sd_loosen $sd_tighten $coverage $imports");
    system("tail -n 1 ../sens_out/$dist\_vei_$vac_eff_i\_ves_$vac_eff_s\_vep_$vac_eff_p\_sdmin_$sd_lower\_sdmax_$sd_upper\_rate_$vac_rate\_mut_$mut_inf\_trigmin_$sd_loosen\_trigmax_$sd_tighten\_cover_$coverage\_import_$imports\.csv >> sens_$inpfile");
}
close(INP);
