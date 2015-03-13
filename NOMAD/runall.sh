#!/bin/sh

################################################################################
# Functions
################################################################################
. /opt/lib/bash/functions

################################################################################
# Default values
################################################################################
aodsemDir=/home3/level3
logDir=/home/level3/log
predir=`pwd`
year=0
month=0
retrystage=0
################################################################################
# Read command-line options
################################################################################
while getopts y:m: name
do
    case $name in
		y)    year="$OPTARG";;
		m)    month="$OPTARG";;
		[?])   printf "Usage: %s: [-y year] [-m month]\n" $0
			exit 2;;
    esac	
done

# Bzip the previous month
#bzip2 /home/level3/data/meteo/asc/$year.$month.??/*/*.asc

if [ $year = 0 ]; then
	set -- `perl -w -e 'use Date::Calc qw(Add_Delta_YM Today);printf("%04d %02d\n", (Add_Delta_YM(Today(1), 0, -1)));'`
	year=$1
	month=$2
fi
#
# definir les mois d execution et repertoires associes
#
actmonth=${month/#0/}
let 'prevmonth=actmonth-1'
prevyear=$year
let "nextmonth=actmonth+1"

nextyear=$year
if [ $actmonth -eq 1 ]; then
	prevmonth=12
	let "prevyear=year-1"
fi
if [ $actmonth -eq 12 ]; then
	nextmonth=1
	let "nextyear=year+1"
fi
if [ $prevmonth -lt 10 ]; then
	pmonth="0"$prevmonth
else 	pmonth=$prevmonth
fi
if [ $nextmonth -lt 10 ]; then
        nmonth="0"$nextmonth
else 	nmonth=$nextmonth
fi
#
#  verifier si une instance conflictuelle est en cours d'execution
#
timedate=`date`
if [ -f $aodsemDir/$pmonth$prevyear.lock ]
then echo "Conflict with another instance of aodsem!"
     echo "Wait for end of previous runall.sh instance or "
     echo "remove .lock files in " $aodsemDir
     echo $timedate "killing runall.sh -y " $year "-m " $month >> $aodsemDir/runallerror.log
     exit 3
fi
if [ -f $aodsemDir/$month$year.lock ]
then echo "Conflict with another instance of aodsem!"
	     echo "Wait for end of previous runall.sh instance or "
	     echo "remove .lock files in " $aodsemDir
	     echo $timedate "killing runall.sh -y " $year "-m " $month >> $aodsemDir/runallerror.log
     exit 3
fi
if [ -f $aodsemDir/$nmonth$nextyear.lock ]
then echo "Conflict with another instance of aodsem!"
	     echo "Wait for end of previous runall.sh instance or "
	     echo "remove .lock files in " $aodsemDir
	     echo $timedate "killing runall.sh -y " $year "-m " $month >> $aodsemDir/runallerror.log
     exit 3
fi
     
#
#  marquer une trace des mois d execution en cours
#
echo "" > $aodsemDir/$pmonth$prevyear.lock
echo "" > $aodsemDir/$month$year.lock
echo "" > $aodsemDir/$nmonth$nextyear.lock

if [ $retrystage -eq 0 ] 
then	
	################################################################################
	# Produce pgm files for AODSEM
	################################################################################
	echo $year $month

	for deltaMonth in -1 0 1
	do
		################################################################################
		# Set year, month
		################################################################################
		aaaa=`expr $month + 0`
		set -- `perl -w -e 'use Date::Calc qw(Add_Delta_YM Today);printf("%04d %02d\n", (Add_Delta_YM('$year','$aaaa', 1, 0, '$deltaMonth')));'`
		yearTmp=$1
		monthTmp=$2
	
		echo Processing $yearTmp $monthTmp
	
		logFile=$logDir/$yearTmp.$monthTmp
	
		if [ ! -e $logDir ]; then
			mkdir -p $logDir
		fi
	
		date > $logFile
			
		################################################################################
		# Create files link for AODSEM
		################################################################################
		~/bin/data2aodsem.sh -y $yearTmp -m $monthTmp -o $aodsemDir
		echo $aodsemDir
	done
	################################################################################
	# AODSEM
	################################################################################
	# converting meteo files

	cd $aodsemDir/GEM/$prevyear/$pmonth
	echo "Converting meteo files for month:" $pmonth
	rm -f *.rhu
	rm -f *.wsp
	rm -f *.tt
	rm -f *.PR
	ln -s ../$month/$year$month"0100_000_ana.asc" .
	gemin
	gem2n
	listem=`ls -l *asc | grep lrw | gawk '{ print $11 }'`
 	for fiasc in $listem
   		do rm -f $fiasc
    	done
	#  enlever les liens brises restants ou les fichier asc locaux
   	 rm -f *asc
	cd $aodsemDir
	cd $aodsemDir/GEM/$year/$month
	echo "Converting meteo files for month:" $month
	rm -f *.rhu
	rm -f *.wsp
	rm -f *.tt
	rm -f *.PR
	ln -s ../$nmonth/$nextyear$nmonth"0100_000_ana.asc" .
	gemin
	gem2n
	listem=`ls -l *asc | grep lrw | gawk '{ print $11 }'`
        for fiasc in $listem
        	do rm -f $fiasc
    	done
   	#  enlever les liens brises restants ou les fichier asc locaux
    	rm -f *asc
				
	cd $aodsemDir
	cd $aodsemDir/GEM/$nextyear/$nmonth
	echo "Converting meteo files for month:" $nmonth
	rm -f *.rhu
	rm -f *.wsp
	rm -f *.tt
	rm -f *.PR
	gemin
	gem2n
	listem=`ls -l *asc | grep lrw | gawk '{ print $11 }'`
    	for fiasc in $listem
        	do rm -f $fiasc
    	done
    	#  enlever les liens brises restants ou les fichier asc locaux
    	rm -f *asc
	cd $aodsemDir
	rm -fr $aodsemDir/results/$year/$month
fi
###########################################################################
# AODSEM start here        
###################################################################
cd $aodsemDir

auto-nomad $actmonth $year $retrystage
# cleaning dat files for the previous month
cd $aodsemDir
cd results/$prevyear/$pmonth/coarse
rm -fr *.dat.gz
cd ../fine
rm -fr *.dat.gz
cd ../aeronet
rm -fr *.dat.gz
cd ../modis
rm -fr *.dat.gz
cd ../modis+aeronet
rm -fr *.dat.gz
cd ..
rm -fr buffer
cd $predir

# Check the exit status of the last command executed	
_mailError $? $year.$month:autonomad
		
################################################################################
# AODSEM PGM results file to NetCDF format
################################################################################
~/bin/aodsem2nc.sh -y $year -m $month -i $aodsemDir/results

# Check the exit status of the last command executed	
_mailError $? $year.$month:aodsem2nc

date >> $logFile
rm -f  $aodsemDir/$pmonth$prevyear.lock
rm -f  $aodsemDir/$month$year.lock
rm -f  $aodsemDir/$nmonth$nextyear.lock
