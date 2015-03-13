#!/bin/sh

#   3:40 GMT  ->  22:40 Heure local     -> 00:00 
#  15:40 GMT  ->  10:40 heure local      ->12:00
#
#  Check date: 2004-12-13 10:42 (15:42 GMT)
#     
#   CMC_glb_00/ ->  13-Dec-2004 03:43  
#	CMC_glb_12/ ->  12-Dec-2004 15:41
#
#  Check date:  2004-12-13 12:03 (17:03 GMT)
#   
#   CMC_glb_00/ ->  13-Dec-2004 03:43  
#	CMC_glb_12/ ->  13-Dec-2004 15:41
#
#  Check date:  2004-12-14 11:20 (16:20 GMT)
#   
#   CMC_glb_00/ ->  14-Dec-2004 03:43  
#	CMC_glb_12/ ->  14-Dec-2004 15:41
#
#  Il peut avoir un délai dans le transfert (minimum 40 minutes)
#
#  Temps de traitment:  10 min.

################################################################################
# Functions
################################################################################
. /opt/lib/bash/functions

_fileList() {
	################################################################################
	#  File names follow the following convention:
	#    Center_model_parameter_level-type_level_grid-qualifier_yyyymmddhh_Phour.grib 
	#
	#  http://weatheroffice.ec.gc.ca/grib/High-resolution_GRIB_e.html
	################################################################################
	Center=CMC
	model=glb
	grid_qualifier=latlon1x1
	yyyymmddhh=$1$2$3$4
	
	file_list=~/tmp/file_list
	
	# Delete the previous file list
	rm -f ${file_list}
	
	for parameter in HGT TMP DEPR UGRD VGRD
	do
		for level_type in ISBL
		do
			for level in 50 100 150 200 250 300 400 500 700 850 925 1000
			do			
				for Phour in P000 P006 P012 P018
				do	
					echo ${Center}_${model}_${parameter}_${level_type}_${level}_${grid_qualifier}_${yyyymmddhh}_${Phour}.grib >> ${file_list}
				done
			done			
		done
	done		
	
	for parameter in APCP
	do
		for level_type in SFC
		do
			for level in 0
			do			
				for Phour in P006 P012 P018
				do	
					echo ${Center}_${model}_${parameter}_${level_type}_${level}_${grid_qualifier}_${yyyymmddhh}_${Phour}.grib >> ${file_list}
				done
			done			
		done
	done
}

################################################################################
# Default values
################################################################################
hh=00
outputDir=$HOME/data/meteo
execute1=1
execute2=1

################################################################################
# Read command-line options
################################################################################
while getopts h:o:xy name
do
    case $name in
		h)    hh="$OPTARG";;
		o)    outputDir="$OPTARG";;
		x)    execute1=0;;
		y)    execute2=0;;
		[?])   printf "Usage: %s:  [-h hour] [-o outputDir] [-x] [-y]\n" $0
			exit 2;;
    esac
done

################################################################################
# Set year, month, day variables
################################################################################
set -- `date -u +"%Y %m %d %k%M"`
year=$1
month=$2
day=$3
hhmm=$4

if [ $hh -eq "00" ]
then
	if [ $hhmm -lt 340 ]; then
		set -- `perl -w -e 'use Date::Calc qw(Add_Delta_Days Today);printf("%04d %02d %02d\n", (Add_Delta_Days(Today(1), -1)));'`		
		year=$1
		month=$2
		day=$3	
	fi
fi

if [ $hh -eq "12" ]
then
	if [ $hhmm -lt 1540 ]; then
		set -- `perl -w -e 'use Date::Calc qw(Add_Delta_Days Today);printf("%04d %02d %02d\n", (Add_Delta_Days(Today(1), -1)));'`
		year=$1
		month=$2
		day=$3	
	fi
fi

echo $year-$month-$day $hhmm

#################0###############################################################
# Set log file
################################################################################
logDir=$outputDir/log/$year.$month.$day

if [ ! -e $logDir ]; then
	mkdir -p $logDir
fi

logfile=$logDir/$year$month$day$hh.log
date +"== %F %T ==" >> $logfile

################################################################################
# Retrieving fields from a locally generated list of file names
#   (http://weatheroffice.ec.gc.ca/grib/usage_tips_e.html)
################################################################################
user=wxo_colsher
passwd=r6w2s7a
base_URL=http://dd.weatheroffice.ec.gc.ca/grib/public/hires/CMC_glb_${hh}/
outputDirGRIB=$outputDir/grib/$year.$month.$day/${hh}
nbGRIB1=0

if [ ! -e $outputDirGRIB ]; then
	mkdir -p $outputDirGRIB
fi

if [ $execute1 -gt 0 ]; then
	nbGRIB1=`ls $outputDirGRIB/*.grib | wc -l`

	if [ $nbGRIB1 -lt 243 ]; then			
		rm -f $outputDirGRIB/*.grib	
		nbGRIB1=0
		
		_fileList $year $month $day $hh
		wget --http-user=$user --http-passwd=$passwd -nd -nc -i ${file_list} -P ${outputDirGRIB} -B ${base_URL} -nv -a $logfile
		
		#if [ $? -eq 0 ]; then		
		#	ssh mnnguyen@callisto.si.usherb.ca "touch \$HOME/data/meteo/grib/$year.$month.$day/${hh}.done"
		#fi			
		
		# Check the exit status of the last command executed	
		_mailError $? $year.$month.$day.$hh:wget 		
	fi		
fi

nbGRIB2=`ls $outputDirGRIB/*.grib | wc -l`
nbGRIB=`expr $nbGRIB2 - $nbGRIB1`

if [ $nbGRIB -eq 243 ]; then
	# Flag to callisto to not download data
	ssh mnnguyen@callisto.si.usherb.ca "touch \$HOME/data/meteo/grib/$year.$month.$day.${hh}.done"
fi

################################################################################
# Convert grib to ascii format
################################################################################
outputDirASC=$outputDir/asc/$year.$month.$day/${hh}
nbASC1=0

if [ 0 = 1 ]; then
	if [ ! -e $outputDirASC ]; then
		mkdir -p $outputDirASC
	fi
	
	if [ $execute2 -gt 0 ]; then
		nbASC1=`ls $outputDirASC/*.* | wc -l`
		
		if [ $nbGRIB -ge 243 ] && [ $nbASC1 -lt 7 ]; then
			rm -f $outputDirASC/*.*
			nbASC1=0
			
			~/bin/CMC2asc.pl \
				--inputDir=$outputDirGRIB  \
				--outputDir=$outputDirASC  \
				2>&1 >> $logfile
			
			# Check the exit status of the last command executed	
			_mailError $? $year.$month.$day.$hh:CMC2asc 
			
			# Compress output files
			#bzip2 $outputDirASC/*.asc
		fi
	fi
	
	nbASC2=`ls $outputDirASC/*.* | wc -l`
	nbASC=`expr $nbASC2 - $nbASC1`
fi

################################################################################
# Verify files creation
################################################################################
if [ 0 = 1 ]; then
	if [ $nbGRIB -gt 0 ] && [ $nbASC -eq 0 ]; then
		_mailError 1 $year.$month.$day:noASC
	fi
fi

################################################################################
# Statistics
################################################################################
date +"== %F %T ($year-$month-$day $hh) $nbGRIB grib/$nbASC asc ==" >> $logfile
