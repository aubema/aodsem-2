#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use Pod::Usage;
use Date::Calc;

use lib("/home/level3/bin");
use Meteo;

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
	my @latitude  = ();
	my @longitude = ();
	my @dinf   = ();
	my @dsup   = ();
	my @inDir  = ();
	my @outDir = ();
	
	GetOptions('help|?'   => \$help,
			'lat=s'       => \@latitude,
			'lon=s'       => \@longitude,
			'dinf=s'      => \@dinf,
			'dsup=s'      => \@dsup,
			'inputDir=s'  => \@inDir,
			'outputDir=s' => \@outDir)
			or pod2usage(1);
	
	pod2usage(1) if $help;
	
	# Set undefined values
	$longitude[0] = "195:1:120" if (scalar(@longitude) == 0);  # 180:1:361
	$latitude[0]  = " 25:1:60"  if (scalar(@latitude)  == 0);  # -85:1:171
	my @date1 = ((scalar(@dinf) > 0 ? split(/[ :\,-]/, $dinf[0]) : Date::Calc::Add_Delta_Days(Date::Calc::Today(1), -1)), 0 ,0 ,0);
	my @date2 = ((scalar(@dsup) > 0 ? split(/[ :\,-]/, $dsup[0]) : Date::Calc::Add_Delta_Days(Date::Calc::Today(1), -1)), 0, 0, 0);
	@inDir  = (".") unless (scalar(@inDir)  > 0);		   
	@outDir = (".") unless (scalar(@outDir) > 0);		   

	my @lonInfo = split(/:/, $longitude[0]);
	my @latInfo = split(/:/, $latitude[0]);
	
	# Create output directory if necessary			
	mkdir($outDir[0]) unless (-e $outDir[0]);
					
	process(\@lonInfo, \@latInfo, \@date1, \@date2, $inDir[0], $outDir[0]); 
}

###############################################################################
#  Process
#
#    Parameters:
#     $inputDir
#     $outputDir
#     $refDate1
#     $refDate2
#
#    Returns:
#      None
################################################################################
sub process {
	my ($refLonInfo, $refLatInfo, $refDate1, $refDate2, $inputDir, $outpuDir) = @_;	

    my $dd = (Date::Calc::Delta_DHMS(@$refDate1, @$refDate2))[0];					 
	my $dt = 6 * 3600;	# 6 hours * 60 mins/hour * 60 secs/min		
	
	my $met1 = undef;
	my $met2 = undef;
			
	for (my $dh = 0; $dh < $dd * 2; $dh++) {
		my @date = Date::Calc::Add_Delta_DHMS(@$refDate1, 0, $dh * 12, 0, 0);
	
		foreach my $hour (0, 6) {											
			$met2 = new Meteo($refLonInfo, $refLatInfo, $inputDir, \@date, $hour);
			#$met2->calculatePAnddP_dz();
						
			if (defined($met1)) {					
	
				print $met1->getDate() . "\t" . $met2->getDate() . "\n";					
							
				#$met1->getVZ($met2->getP(), $dt);	
				#$met1->getRH();						
				#$met1->getWet();		  # A Verifier car pas de donnee a 0h				
			}
				
			$met1 = $met2;
		}				
	}		
}
	
=head1 NAME

grib2aodsem.pl - 

=head1 SYNOPSIS

grib2aodsem.pl [OPTIONS] 

=head2 Options

Recognized options include:

=over 4

=item -h, --help

Display this help screen.

=item -inputDir=INPUTDIR

Set the input directory (default: .)

=item -outputDir=OUTPUTDIR

Set the output directory (default: .)

=item -dinf=DATE

Set the GMT date lower limit (ex.: "2002-12-31") (default: Today - 1 day)

=item -dsup=DATE

Set the GMT date upper limit (ex.: "2002-12-31") (default: Today - 1 day)

