#!/bin/bash
#SBATCH --partition=shas
#SBATCH --qos condo
#SBATCH -A csu-summit-sta
#SBATCH --nodes=64
#SBATCH --output=logs/%x_%a_%j.out
#SBATCH --error=logs/%x_%a_%j.err
#SBATCH --mail-type=end
#SBATCH --mail-user=schafecs@colostate.edu
#SBATCH --job-name=2014
#SBATCH --time=168:00:00
#SBATCH --mem=10000

#
# modules
#
module purge
module load jdk

#
# spark configuration
#
export SPARK_HOME=/projects/schafecs\@colostate.edu/spark-2.3.1-bin-hadoop2.7/
export SPARK_LOCAL_DIRS=/scratch/summit/schafecs@colostate.edu/tmp/

#
# run script
#
$SPARK_HOME/bin/spark-submit \
--master "local[*]" \
--conf "spark.driver.extraJavaOptions=-Dlog4j.configuration=log4j.properties" \
--conf "spark.driver.extraClassPath=/projects/schafecs\@colostate.edu/dblink-assembly-0.1.jar" \
/projects/schafecs\@colostate.edu/dblink-assembly-0.1.jar \
2014.conf
