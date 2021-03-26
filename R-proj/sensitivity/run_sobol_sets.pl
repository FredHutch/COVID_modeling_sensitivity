#!/usr/bin/perl
# Perl script that takes a sobol file , sets the parameters and runs the sims
#
#VEsusc: 10%, 90%
#VEinf: 10%, 90%
#VEsymp: 10%, 90%
#Vaccination rate: 2000, 8000 
#Lockdown cast threshold: 200, 800 
#Minimal social distancing proportion: 0, 20%

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
my $trig_min=100;

my $vac_rate_min= 2000;
my $vac_rate_max= 8000;
my $vac_rate_range= $vac_rate_max - $vac_rate_min;

my $vac_eff_p_min= 0.1;
my $vac_eff_p_max= 0.9;
my $vac_eff_p_range= $vac_eff_p_max - my $vac_eff_p_min;

my $vac_eff_i_min= 0.1;
my $vac_eff_i_max= 0.9;
my $vac_eff_i_range= $vac_eff_i_max - my $vac_eff_i_min;

my $vac_eff_s_min= 0.1;
my $vac_eff_s_max= 0.9;
my $vac_eff_s_range= $vac_eff_s_max - my $vac_eff_s_min;

my $sd_lower_min= 0;
my $sd_lower_max= 0.2;
my $sd_lower_range=$sd_lower_max - $sd_lower_min;

my $sd_tighten_min= 200;
my $sd_tighten_max= 800;
my $sd_tighten_range= $sd_tighten_max - $sd_tighten_min;

system("rm -f sobol_sens_$inpfile");

while( $line=<INP>) {
    chomp($line);              # remove the newline from $line.
    my @pieces = split(',',$line);
    my $vac_rate = $vac_rate_range * $pieces[0] + $vac_rate_min;
    my $vac_eff_p = $vac_eff_p_range * $pieces[1] + $vac_eff_p_min;
    my $vac_eff_i = $vac_eff_i_range * $pieces[2] + $vac_eff_i_min;
    my $vac_eff_s = $vac_eff_s_range * $pieces[3] + $vac_eff_s_min;
    my $sd_lower = $sd_lower_range * $pieces[4] + $sd_lower_min;
    my $sd_tighten = $sd_tighten_range * $pieces[5] + $sd_tighten_min;

    my $rds_file="../data/$dist\_vei_$vac_eff_i\_sdmin_$sd_lower\_rate_$vac_rate\_trigs_$trig_min\_$sd_tighten\.rds";

    
    print "Running $dist sets with start=$start vei=$vac_eff_i vac rate=$vac_rate sd_min=$sd_lower trig_min=$trig_min trig_max=$sd_tighten\n";
    if (! -e $rds_file) {
	system("Rscript sobol_sensitivity.R $dist $age_code $vac_eff_i $vac_rate $sd_lower $trig_min $sd_tighten $vac_eff_p $vac_eff_s");
    } else {
	print "Skipped creation of $rds_file\n";
    }
    system("Rscript sens_table_sobol.R $dist $start $vac_eff_i $vac_rate $sd_lower $trig_min $sd_tighten $vac_eff_p $vac_eff_s");
    system("tail -n 1 out/$dist\_vei_$vac_eff_i\_sdmin_$sd_lower\_rate_$vac_rate\_trigs_$trig_min\_$sd_tighten\.csv >> sobol_sens_$inpfile");
}
close(INP);
