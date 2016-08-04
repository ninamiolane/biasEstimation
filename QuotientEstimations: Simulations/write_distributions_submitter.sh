#!/bin/bash

echo ""

	declare -a arr=( "mle_SF" "mle_SS") # "mapFF" "mapFS" "mapSF" "mapSS")
	declare -i arrn=(10 20 50 100 200)
declare -a arrxi=(0.2 0.5 1 2)
	base_job_name="distributions_jobfile"
	jobfile="write_distributions_jobfile.sh"
	mkdir -p output

	for estname in "${arr[@]}"; do
	for n in "${arrn[@]}"; do
	for xi in "${arrxi[@]}"; do
	jobname=$base_job_name-${estname}-${n}-${xi}
	echo "Submitting start estimator ${estname} and ${n} and xi= ${xi}  observations"

	outfile=output/output-${estname}-${n}-${xi}
	export estname
	export n
	export xi
	sbatch -J $jobname -o $outfile $jobfile
	done;
	done;
	done;

	echo "Done submitting jobs"
