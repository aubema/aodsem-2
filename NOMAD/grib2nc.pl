#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use Pod::Usage;
use PDL;
use PDL::IO::Grib;
use File::Basename;

use lib("/home/level3/bin");
use MeteoNetCDF;

main();

###############################################################################
#  Main
#
#    Parameters:
#      None
#
#    Returns:
#      None
################################################################################
sub main {
	my $help  = 0;
	my @inDir = ();
	my @outDir = ();
	
	GetOptions('help|?'   => \$help,
			'inputDir=s'  => \@inDir,
			'outputDir=s' => \@outDir)
			or pod2usage(1);
	
	pod2usage(1) if $help;
	
	# Set undefined values
	@inDir  = (".") unless (scalar(@inDir)  > 0);		   
	@outDir = (".") unless (scalar(@outDir) > 0);		   

	# Create output directory if necessary			
	mkdir($outDir[0]) unless (-e $outDir[0]);
		
	# Get the date and hour
	my @vecFile = `ls "$inDir[0]"/CMC_glb_UGRD_ISBL_1000_latlon1x1_*_P000.grib`;
		
	foreach my $file (@vecFile) {
		if (basename($file) =~ /CMC_glb_UGRD_ISBL_1000_latlon1x1_(\d\d\d\d\d\d\d\d)(\d\d)_P000.grib/) {		
			print "Find $1 $2\n";
#			main2($inDir[0], $outDir[0], $1, $2); 
			sdsad($inDir[0], $outDir[0], $1, $2); 
		}
	}	
}

sub sdsad {
	my ($inputDir, $outputDir, $yyyymmdd, $hh) = @_;

	my @lonInfo = (-180, 1, 361);
	my @latInfo = ( -85, 1, 171);
	
	my @dataDate = ();
	
	if ($yyyymmdd =~ /(\d\d\d\d)(\d\d)(\d\d)/) {			
		@dataDate = ($1, $2, $3, $hh, 0);
	}
	
###################################################################################################			
		
	my $level_type   = "ISBL";
	my @vecLevel     = (1000, 925, 850, 700, 500, 400, 300, 250, 200, 150, 100, 50);
	my @vecParameter = ("UGRD", "VGRD", "HGT", "TMP", "DEPR");
	my @vecPhour     = ("P000", "P006", "P012", "P018");	

	my $ncobj = new MeteoNetCDF(\@lonInfo, \@latInfo);
	$ncobj->setVecTime(pdl([0 ,6, 12, 18]), sprintf("hours since %4d-%02d-%02d %02d:%02d", @dataDate));
	$ncobj->setVecLevel(pdl([1000, 925, 850, 700, 500, 400, 300, 250, 200, 150, 100, 50]), "hPa");	
	
	writeNc($inputDir, $yyyymmdd, $hh,
	        $outputDir, 
	    	$ncobj, $level_type,
		    \@vecLevel, \@vecParameter, \@vecPhour); 		  		  	

###################################################################################################			
			
	$level_type   = "SFC";
	@vecLevel     = (0);
	@vecParameter = ("APCP");
	@vecPhour     = ("P006", "P012", "P018");	
	
	writeNc($inputDir, $yyyymmdd, $hh,
	        $outputDir, 
	    	$ncobj, $level_type,
		    \@vecLevel, \@vecParameter, \@vecPhour); 		  		  	
}

sub writeNc {
	my ($inputDir, $yyyymmdd, $hh, $outputDir, $ncobj, $level_type, $refVecLevel, $refVecParameter, $refVecPhour) = @_;

	my $Center         = "CMC";
	my $model          = "glb";
	my $grid_qualifier = "latlon1x1";	
	my $fillValue      = -9999;
					
	foreach my $parameter (@$refVecParameter) {		
		my $data = float(zeroes(361, 171, scalar(@$refVecLevel), scalar(@$refVecPhour)));
		my $longName = "n/a";
		my $units = "n/a";
		my $hourInd = 0;
		
		if (scalar(@$refVecLevel) == 1) {
			$data = zeroes(float, 361, 171, scalar(@$refVecPhour) + 1);
			$hourInd = 1;
		}
				
		foreach my $Phour (@$refVecPhour) {		
			my $levelInd = 0;	
			
			foreach my $level (@$refVecLevel) {
				my $gribFile = "${Center}_${model}_${parameter}_${level_type}_${level}_${grid_qualifier}_${yyyymmdd}${hh}_${Phour}.grib";				
				
				if (-e "$inputDir/$gribFile" . "aa") {
					my $gh = new PDL::IO::Grib("$inputDir/$gribFile");
					my $f = $gh->anyfield();														
					my $val = $f->pds_attribute(9);
					
					if ($val eq  7) { $longName = "Geopotential height" ; $units = "Gpm"; }		# GZ  GEOPOTENTIEL
					if ($val eq 11) { $longName = "Temperature"         ; $units = "K"; }		# TT  TEMPERATURE
					if ($val eq 18) { $longName = "Dew point depression"; $units = "K"; }		# ES  ECART DU POINT DE ROSEE
					if ($val eq 33) { $longName = "u-component of wind" ; $units = "m/s"; }		# UU  COMPOSANTE U DU VENT (SELON L AXE DES X)
					if ($val eq 34) { $longName = "v-component of wind" ; $units = "m/s"; }		# VV  COMPOSANTE V DU VENT (SELON L AXE DES Y)
					if ($val eq 61) { $longName = "Total precipitation" ; $units = "kg/m2"; }	# PR  PRECIP
					
					if (scalar(@$refVecLevel) == 1) {	
						$data->slice(":,:,$hourInd") .= $f->read_data($gh->{_FILEHANDLE});
					}
					else {
						$data->slice(":,:,$levelInd,$hourInd") .= $f->read_data($gh->{_FILEHANDLE});
					}
				}
				else {
					print "$inputDir/$gribFile unavailable\n";
					
					if (scalar(@$refVecLevel) == 1) {	
						$data->slice(":,:,$hourInd") .= ones(float, 361, 171) * $fillValue;
					}
					else {
						$data->slice(":,:,$levelInd,$hourInd") .= ones(float, 361, 171) * $fillValue;					
					}
				}				
				
				$levelInd++;				
			}
			
			$hourInd++;
		}		
		
		my $varName  = lc($parameter);
		
		print "Writing parameter: $varName\n";		
		$ncobj->write("$outputDir/${Center}_${model}.${yyyymmdd}${hh}.nc", $varName, $longName, $units, $data, float([$fillValue]));
	}
}

=head1 NAME

grib2nc.pl - 

=head1 SYNOPSIS

grib2nc.pl [OPTIONS]

=head2 Options

Recognized options include:

=over 4

=item -h, --help

Display this help screen.

=item -inputDir=INPUTDIR

Set the input directory (default: .)

=item -outputDir=OUTPUTDIR

Set the output directory (default: .)

