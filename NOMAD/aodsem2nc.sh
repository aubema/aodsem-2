#!/bin/sh

################################################################################
# Functions
################################################################################
_pgm2nc() {
	for pgmFile in $2????2130_01.pgm	
	do
		# Check pgm date 
		keep=`head -n 4 $pgmFile | grep " $5 $4" | wc -l`	
		
		if [ $keep -ne 0 ]; then										
			filePrefix=`dirname $pgmFile`/`basename $pgmFile 2130_01.pgm`
			
			if [ ! -e $outputDir ]; then
				mkdir -p $outputDir
			fi

			# Process for each wavelength
			for wl in 340 380 440 470 500 550 670 860 870 940 1020 1240 1640 2130
			do
				varname=`printf "aod%04d" $wl`
				longname=`printf "Total Extinction Aerosol Optical Depth (%4d nm)" $wl`
				
				for src in $filePrefix${wl}_01.pgm
				do
					~/bin/pgm2nc.pl $src --name=$varname --longname="$longname" --outputDir=$3 --product=$1
				done
			done			
		fi
	done
}

################################################################################
# Default values
################################################################################
year=2005
month=04
inputDirRoot=/home3/level3/results    
outputDirRoot=/home/level3/public_html   

################################################################################
# Read command-line options
################################################################################
while getopts y:m:i:o: name
do
    case $name in
		y)    year="$OPTARG";;
		m)    month="$OPTARG";;
		i)    inputDirRoot="$OPTARG";;		
		o)    outputDirRoot="$OPTARG";;		
		[?])   printf "Usage: %s:  [-y year] [-m month] [-i inputDirRoot] [-o outputDirRoot]\n" $0
			exit 2;;
    esac
done

month2=`expr $month + 0`    # get 2 instead of 02 for february

################################################################################
# Remove old data
################################################################################
rm -fr $outputDirRoot/a1/$year.$month.*
rm -fr $outputDirRoot/a2/$year.$month.*
rm -fr $outputDirRoot/b1/$year.$month.*
rm -fr $outputDirRoot/b2/$year.$month.*
rm -fr $outputDirRoot/b3/$year.$month.*

###############################################################################
# AOD forecast mode, Coarse resolution
###############################################################################
product=L3A1
outputDir=$outputDirRoot/a1
inputFile=$inputDirRoot/$year/$month/coarse/bgf-

_pgm2nc $product $inputFile $outputDir $year $month2
	
###############################################################################
# AOD forecast mode, Fine resolution
###############################################################################
product=L3A2
outputDir=$outputDirRoot/a2
inputFile=$inputDirRoot/$year/$month/fine/bgf-

_pgm2nc $product $inputFile $outputDir $year $month2

###############################################################################
# AOD from assimilation mode, MODIS L2 data
###############################################################################
product=L3B1
outputDir=$outputDirRoot/b1
inputFile=$inputDirRoot/$year/$month/aeronet/ana-

_pgm2nc $product $inputFile $outputDir $year $month2

###############################################################################
# AOD from assimilation mode, AERONET level 1.5
###############################################################################
product=L3B2
outputDir=$outputDirRoot/b2
inputFile=$inputDirRoot/$year/$month/modis/ana-

_pgm2nc $product $inputFile $outputDir $year $month2

###############################################################################
# AOD from assimilation mode, MODIS L2 and AERONET level 1.5
###############################################################################
product=L3B3
outputDir=$outputDirRoot/b3
inputFile=$inputDirRoot/$year/$month/modis+aeronet/ana-

_pgm2nc $product $inputFile $outputDir $year $month2

###############################################################################
# Update LAS config src and server
###############################################################################
/opt/bin/updateLAS.sh
	
