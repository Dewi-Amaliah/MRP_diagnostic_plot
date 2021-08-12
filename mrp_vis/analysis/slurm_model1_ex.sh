#!/bin/env bash
#SBATCH --job-name=mrp_vis_model1
#SBATCH --time=12:00:00
#SBATCH --mem=4000
#SBATCH --ntasks=4
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=dama0007@student.monash.edu
module load R
R --vanilla < Mona0070/dama0007/mrp_vis/hpc_code_model1_ex.R