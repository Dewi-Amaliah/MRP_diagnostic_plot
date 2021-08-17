#!/bin/env bash
#SBATCH --job-name=mrp_vis_model4a
#SBATCH --time=12:00:00
#SBATCH --mem=4000
#SBATCH --ntasks=4
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=dama0007@student.monash.edu
module load R
R --vanilla < Mona0070/dama0007/mrp_vis/hcp_code_model4a.R