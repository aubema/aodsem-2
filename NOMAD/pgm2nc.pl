#!/usr/bin/perl -w

# Fichier PGM
# 3(a)  65 x 135     (20N, 175W)  1x1
# 3(a)  125 x 275    (30N, 160W)  0.4x0.4
# 3(b)  125 x 275    (30N, 160W)  0.4x0.4
# test  501 x 1101   (30N, 160W)  0.1x0.1
#     1       1101
#   1 x x x x x
#     x x x x x
#     x x x x x
# 501 55 x x x x
#
#    où 55 est le premier élément qui apparait lorsqu on fait un ncdump
#
#
# DODS Query
#  def.nc.asc?aod[0][0][0]
#
#  Dataset: def.nc
#  aod.lon, 200
#  aod.aod[aod.time=24][aod.lat=30], 55

use strict;
use Sys::Syslog;
use File::Basename;
use PDL;
use Getopt::Long;
use Pod::Usage;
use Date::Calc;
use POSIX qw(strftime);
use PDL::IO::Pnm;

use lib("/opt/lib/perl5");
use NOMADNetCDF;

main();

sub main {
	openlog(basename($0), 'perror', 'local3');

	eval {
		syslog('info', "Running " . basename($0));
		pgm2nc();
	};
	# Exception handler
	if ($@) {
		print "$@\n";
    	syslog('err', $@);
	}

	closelog();
}

sub pgm2nc {
	# Process command line arguments and options
	# ===========================================
	my $help    = 0;
	my $man     = 0;
	my @outDir  = ();
	my @product = ();
	my $varName  = "aod";
	my $longName = "Total Extinction Aerosol Optical Depth";

	GetOptions('help|?'  => \$help,
			'man'        => \$man,
			'outputDir=s' => \@outDir,
			'product=s'   => \@product,
			'name=s'     => \$varName,
			'longname=s' => \$longName)
			or pod2usage(2);

	pod2usage(1) if $help;
	pod2usage(-verbose => 2) if $man;

	# Set the default values
	@outDir = (".")   unless (scalar(@outDir) > 0);		   
	@product = ("L3") unless (scalar(@product) > 0);		   

	proceed(\@ARGV, uc($product[0]), $outDir[0], $varName, $longName);
}
	
sub proceed {
	my ($refVecPgmFilename, $product, $outDir, $varName, $longName) = @_;	
	my $ncObj = undef;
	my $ncFilename = undef;
	my $once = 1;
			
	foreach my $pgmFilename (@$refVecPgmFilename) {
		my $matAOD = rpnm $pgmFilename;
				
		# Unsigned short [0, 65535] to signed short [-32768, 32767]
		$matAOD = float($matAOD) - 32768;
					
		# Change value of 32766 and 32767 to 32767 
		$matAOD = ($matAOD == 32766) + $matAOD; 

		# Special case, add an extra line and column
		$matAOD = ins(ones($matAOD->getdim(0) + 1, $matAOD->getdim(1) + 1) * 32767, $matAOD, 0, 0) if ($matAOD->getdim(0) == 275);
														
		if ($once) {		
			# Get size
			# =============================================
			my $xsize = $matAOD->getdim(0);
			my $ysize = $matAOD->getdim(1);
			my $tsize = 1;
					
			# Generate the longitude and latitude info vector
			# ===============================================
			my ($lonMin, $latMin, $step);
			($lonMin, $latMin, $step) = (200.0, 30.0, 0.1) if ($xsize == 1101); #
			($lonMin, $latMin, $step) = (195.0, 25.0, 1.0) if ($xsize ==  120); # Level 3a1
			($lonMin, $latMin, $step) = (200.0, 30.0, 0.4) if ($xsize ==  276); # Level 3a2 3b1 3b2
			die("Unknown xsize $xsize") if !defined($lonMin);
	
			my @lonInfo = ($lonMin, $step, $xsize);
			my @latInfo = ($latMin, $step, $ysize);
						
			# Write AOD to NetCDF file 
			# ========================
			$ncObj = new NOMADNetCDF(\@lonInfo, \@latInfo);	
						
			# Set the time vector
			# ==============================================			
			my @refDate  = (2000, 1, 1, 0, 0, 0);
			my @dataDate = getDataDate($pgmFilename);
				
			my $vecTime  = zeroes($tsize);						
			set($vecTime, 0, getDeltaDays(@refDate, @dataDate) * 24 * 60);
			$ncObj->setVecTime($vecTime, "minutes since " . sprintf("%04d-%02d-%02d %02d:%02d", @refDate));
			$ncFilename = "$outDir/" . sprintf("%04d.%02d.%02d", @dataDate) . "/NOMAD_$product." . sprintf("%04d%02d%02d%02d%02d.nc", @dataDate);
			
			# Set attributes
			# ================
			$ncObj->setUnits("Unitless");
			$ncObj->setScaleFactor(pdl([0.001]));
			$ncObj->setAddOffset(pdl([32.768]));				
			$ncObj->setFillValue(short([32767]));
			
			# Reset once						
			$once = 0;
		}
					
		# Write data to NetCDF file
		# ================================
		#my $wl = undef;
		
		
		#if ($pgmFilename =~ /^0(\d+)\//) {		
		#	$wl = $1;
		#}
		#elsif ($pgmFilename =~ /^(\d+)\//) {			
		#	$wl = $1;
		#} 
		#else {
		#	die("Cannot extract wavelength from $pgmFilename");
		#}
					
		#my $varName = "aod" . ($wl < 1000 ? "0$wl" : "$wl");
		#my $longName = "Total Extinction Aerosol Optical Depth (" . ($wl < 1000 ? " $wl" : "$wl")  . " nm)";				
		my $datAOD = zeroes($matAOD->getdim(0), $matAOD->getdim(1), 1);				
		$datAOD->slice(':,:,(0)') .= $matAOD;			
		
		my $dirname = dirname($ncFilename);
		mkdir($dirname) unless (-e $dirname);
		
		print "Writing $pgmFilename ($varName) to $ncFilename\n";	
		$ncObj->write($ncFilename, $varName, $longName, short($datAOD));	
	}
}

sub getDataDate {
	my ($filename) = @_;

	my @dataDate;
	
	open(FILE, "<$filename") || die ("$filename: $!\n");

	while (my $line = <FILE>) {
		chomp($line);

		$line =~ s/^\s*//;
		$line =~ s/\s*$//;

		next unless $line;          	# ignorer les lignes vides
		next unless $line =~ m/^#/;     # Garder les commentaires

		if ($line =~ m/^#\s+date\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)/) {
			@dataDate = ($6, $5, $4, $1, $2, $3);

			last;
			}
		}

	close(FILE);
	
	return @dataDate;
}


sub getDeltaDays {
	my (@refDate, @dataDate) = @_;
    my ($Dd,$Dh,$Dm,$Ds) = Date::Calc::Delta_DHMS(@refDate, @dataDate);

	return $Dd + $Dh/24.0 + $Dm/(24.0*60.0) + $Ds/(24.0*60.0*60.0);
}

__END__

=head1 NAME
pgm2nc.pl - Convert level 3 pgm file to a netCDF file.

=head1 SYNOPSIS

pgm2nc.pl [OPTION] pgmFile ...

=head1 DESCRIPTION

Convert level 3 pgm file to a netCDF file with COARDS conventions.

=head2 Options

=over 4

=item --name=LABEL

Change the data variable name

=item --longname=LABEL

Change the data variable long name

=item -outputDir=OUTPUTDIR

Set the output directory (default: .)

=item -product=PRODUCT

Set the product string (default: L3)

=back

=head1 AUTHOR

Written by Minh-Nghia Nguyen.

=head1 BUGS

Report bugs to <Minh-Nghia.Nguyen@USherbrooke.ca>.

=head1 SEE ALSO

bqsubmit(1)

=head1 COPYRIGHT

Copyright(C) 2003 CARTEL.
This is free software; see the source for copying conditions.  There is
NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.

=cut
