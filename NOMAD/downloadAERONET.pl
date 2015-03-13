#!/usr/bin/perl -w

################################################################################
# This script is use to query a limited set of AERONET data.  It is a
# replacement for the AERONET Data Download Tool web interface
# (http://aeronet.gsfc.nasa.gov).
################################################################################
use strict;
use Getopt::Long;
use Pod::Usage;
use Date::Calc;
use LWP::Simple;
use LWP::UserAgent;
use HTTP::Request::Common;
use File::Basename;

my $aeronetURL = "http://aeronet.gsfc.nasa.gov";

my @vecSite = (
	"Barrow",
	"Bermuda",
	"Bonanza_Creek", 
	"BONDVILLE", 
	"Bratts_Lake", 
	"Brookhaven", 
	"CARTEL", 
	"Cart_Site", 
	"Chequamegon", 
	"Egbert", 
	"GSFC", 
	"Harvard_Forest", 
	"HJAndrews", 
	"Honolulu", 
	"Howland", 
	"Kejimkujik", 
	"La_Jolla", 
	"Lunar_Lake", 
	"MISR-JPL", 
	"Niabrara", 
	"Oyster", 
	"Pullman", 
	"Rogers_Lake", 
	"San_Nicolas", 
	"Saturn_Island", 
	"SERC", 
	"Sevilleta", 
	"Sioux_Falls", 
	"Sterling", 
	"TABLE_MOUNTAIN_CA", 
	"Table_Mountain", 
	"Thompson", 
	"Tucson", 
	"USDA");

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
	my $lev10 = 0;
	my $lev15 = 0;
	my $lev20 = 0;
	my $alm   = 0;
	my $ppl   = 0;
	my $avg   = 10; # Set to All Points
	my @outDir = ();
	my @dinf   = ();
	my @dsup   = ();
	
	GetOptions('help|?'   => \$help,
			'lev10'       => \$lev10,          
			'lev15'       => \$lev15,          
			'lev20'       => \$lev20,          
			'alm'         => \$alm,
			'ppl'         => \$ppl,          
			'outputDir=s' => \@outDir,
			'dinf=s'      => \@dinf,
			'dsup=s'      => \@dsup)
			or pod2usage(1);
	
	pod2usage(1) if $help;
	
	# Set undefined values
	@outDir = (".") unless (scalar(@outDir) > 0);		   
	
	# Read $HOME/.aeronetsites if available
	if (-e "$ENV{HOME}/.aeronetsites") {
		@vecSite = ();
	
		open(FILE, "< $ENV{HOME}/.aeronetsites");
	
		my $line = <FILE>;
		$line = <FILE>;
		
		while ($line = <FILE>) {
			chomp($line);
		
			my ($siteName,$longitude,$latitude,$elevation) = split("[\t,\,]", $line);
					
			next if ($longitude >  -50.0);
			next if ($longitude < -160.0);
			next if ($latitude > 80.0);
			next if ($latitude < 30.0);
			
			print "Adding $siteName\t$longitude\t$latitude\n";				
			push(@vecSite, $siteName);
		}	
		
		close (FILE);		
	}
		
	my $refVecSite = (scalar(@ARGV) > 0 ? \@ARGV                     : \@vecSite);
	#my @date1      = (scalar(@dinf) > 0 ? split(/[ :\,-]/, $dinf[0]) : (1990, 1, 1));
	my @date1      = (scalar(@dinf) > 0 ? split(/[ :\,-]/, $dinf[0]) : Date::Calc::Add_Delta_Days(Date::Calc::Today(1), -1));
	my @date2      = (scalar(@dsup) > 0 ? split(/[ :\,-]/, $dsup[0]) : Date::Calc::Add_Delta_Days(Date::Calc::Today(1), -1));
	
	my $select = $lev10 + $lev15 + $lev20 + $alm + $ppl;
	if ($select == 0) {
		$lev10 = 1;
		$lev15 = 1;
	}

	# Create output directory if necessary			
	mkdir($outDir[0]) unless (-e $outDir[0]);
	
	# Process					
	foreach my $site (@$refVecSite) {	
		print "Query AERONET site: $site (@date1-@date2)\n";
		
		my $outputFilenameTmp = sprintf("$outDir[0]/%02d%02d%02d_%02d%02d%02d_${site}.zip", 
									$date1[0] - ($date1[0] < 2000 ? 1900 : 2000), $date1[1], $date1[2],
									$date2[0] - ($date2[0] < 2000 ? 1900 : 2000), $date2[1], $date2[2]); 
		
		if (-e $outputFilenameTmp) {		
			print "\t$outputFilenameTmp already exist\n";
			next;
		}
											
		my $url = submit($site, \@date1, \@date2, $lev10, $lev15, $lev20, $alm, $ppl, $avg);
				
		if (defined($url)) {			
			my $outputFilename = "$outDir[0]/" . basename($url);
		
			print "\tWriting $outputFilename\n";
			getstore("$aeronetURL$url",  $outputFilename);
		}	
		else {
			print "\tCannot get zip url\n";
		}
	}
}

###############################################################################
#  Send query to AERONET Data Download Tool
#
#    Parameters:
#      $site		AERONET site
#      $refDate1	Reference to a vector of start date info
#      $refDate2	Reference to a vector of stop date info
#      $lev10		Flag for AOT/Level 1.0 (Raw)
#      $lev15    	Flag for AOT/Level 1.5 (Cloud Screened)
#      $lev20		Flag for AOT/Level 2.0 (Quality Assured)
#      $alm			Flag for Raw Data/Almucantars
#      $ppl			Flag for Raw Data/Principal Planes
#      $avg         Data Format (10-All Points, 20-Daily Averages, 30-Monthly Averages)
#
#    Returns:
#      Partial URL of the data zip file
################################################################################
sub submit {
	my ($site, $refDate1, $refDate2, $lev10, $lev15, $lev20, $alm, $ppl, $avg) = @_;

	my $ua = LWP::UserAgent->new();
	
	my $response = $ua->request(POST "$aeronetURL/cgi-bin/print_warning_new3", [
				site 	=> $site,
				day 	=> int($$refDate1[2]),
				month 	=> int($$refDate1[1]),
				year 	=> int($$refDate1[0]) - 1900,
				day2 	=> int($$refDate2[2]),
				month2	=> int($$refDate2[1]),
				year2	=> int($$refDate2[0]) - 1900,
				LEV10	=> $lev10,
				LEV15	=> $lev15,
				LEV20	=> $lev20,
				ALM     => $alm,
				PPL     => $ppl,
				TYPE	=> 21,
				MODE	=> 40,
				AVG		=> $avg,        
				Submit	=> 'Download']);

	if ($response->is_success) {
		if ($response->content =~ m/href=\"(.+\.zip)\"/) {		
			return $1;
		}	
	}

	return undef;
}

=head1 NAME

downloadAERONET.pl - 

=head1 SYNOPSIS

downloadAERONET.pl [OPTIONS] sites  

=head2 Options

Recognized options include:

=over 4

=item -h, --help

Display this help screen.

=item -outputDir=OUTPUTDIR

Set the output directory (default: .)

=item -dinf=DATE

Set the GMT date lower limit (ex.: "2002-12-31") (default: Today - 1 day)

=item -dsup=DATE

Set the GMT date upper limit (ex.: "2002-12-31") (default: Today - 1 day)

=item -lev10, -lev15, -lev20, -alm, -ppl

Select the parameters to download (lev10 = Level 1.0 (Raw), lev15 = Level 1.5 (Cloud Screened), lev20 = Level 2.0 (Quality Assured),
alm = Almucantars, ppl = Principal Planes)