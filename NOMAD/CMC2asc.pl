#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use Pod::Usage;
use PDL;
use PDL::IO::Grib;
use File::Basename;
use PerlIO::via::Bzip2;

my $templateFile = "/home/level3/data/meteo/template.txt";

#mainold("/home/level3/PDL-IO-Grib-2/CMC_glb_VGRD_ISBL_925_latlon1x1_2004102700_P018.grib", "/home/level3/cmc/template.txt", "/home/level3/tmp/test.txt");

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
			main2($inDir[0], $outDir[0], $1, $2); 
		}
	}
	

}

sub main2 {
	my ($inputDir, $outputDir, $yyyymmdd, $hh) = @_;

	my $Center         = "CMC";
	my $model          = "glb";
	my $grid_qualifier = "latlon1x1";	
	my @vecLevel       = (1000, 925, 850, 700, 500, 400, 300, 250, 200, 150, 100, 50);
	
	my @template = readTemplateFile($templateFile);
		
	foreach my $Phour ("P000", "P006", "P012", "P018") {		
		my $outputFile = "${yyyymmdd}${hh}_$Phour.ana.asc";	

                if ( ! -e "$outputDir/$outputFile") {
		unlink("$outputDir/$outputFile");
	
		print "Writing $outputDir/$outputFile\n";

		foreach my $level_type ("ISBL") {
			foreach my $parameter ("UGRD", "VGRD") {
				foreach my $level (@vecLevel) {
					my $gribFile = "${Center}_${model}_${parameter}_${level_type}_${level}_${grid_qualifier}_${yyyymmdd}${hh}_${Phour}.grib";
					
 					process(\@template, "$inputDir/$gribFile", "$outputDir/$outputFile", $yyyymmdd, $hh, $Phour);
				}
			}
			
			foreach my $level (@vecLevel) {
				foreach my $parameter ("HGT", "TMP", "DEPR") {					
					my $gribFile = "${Center}_${model}_${parameter}_${level_type}_${level}_${grid_qualifier}_${yyyymmdd}${hh}_${Phour}.grib";
					
					process(\@template, "$inputDir/$gribFile", "$outputDir/$outputFile", $yyyymmdd, $hh, $Phour);
				}
			}
		}
		}
	}

	foreach my $Phour ("P006", "P012", "P018") {
		my $outputFile = "${yyyymmdd}${hh}_$Phour.pcp.asc";	
		if ( ! -e "$outputDir/$outputFile") {
                unlink("$outputDir/$outputFile");
		print "Writing $outputDir/$outputFile\n";
		
		foreach my $level_type ("SFC") {
			foreach my $level (0) {
				foreach my $parameter ("APCP") {					
					my $gribFile = "${Center}_${model}_${parameter}_${level_type}_${level}_${grid_qualifier}_${yyyymmdd}${hh}_${Phour}.grib";
				
					process(\@template, "$inputDir/$gribFile", "$outputDir/$outputFile", $yyyymmdd, $hh, $Phour);
				}
			}
		}
		}
	}
}

sub readTemplateFile {
	my ($templateFile) = @_;
	my @templateArray = ();

	open(FILE, "< $templateFile") or die("input $templateFile: $!");

	while(my $line = <FILE>) {						
		push(@templateArray, $line);
	}

	close(FILE);

	return @templateArray
}

sub process {
	my ($refTemplate, $gribFile, $outputFile, $yyyymmdd, $hh, $Phour) = @_;

	if (! -e $gribFile) {	
		print "$gribFile missing\n";
		return;
	}
		
	my $gh = new PDL::IO::Grib($gribFile);
	my $f = $gh->anyfield();
						
	open(DEST, ">> $outputFile") or die("output $outputFile: $!");
#	open(DEST, ">:via(Bzip2)", "$outputFile") or die("output $outputFile: $!");
	my $lineNumber = 0;
	
	foreach my $line (@$refTemplate) {
		$lineNumber++;
		
		$_ = $lineNumber . "A";		
			
		SWITCH: {	
			/^3A/ && do {	# 3e ligne: variable = ... "UU "VV ou autre			
				# HGT  007 = GZ
				# TMP  011 = TT
				# DEPR 018 = ES
				# UGRD 033 = UU
				# VGRD 034 = VV
				# APCP 061 = PR
			
				my $val = $f->pds_attribute(9);				
				my $variable;
				
				$variable = "GZ  GEOPOTENTIEL"                             if ($val eq  7);
				$variable = "TT  TEMPERATURE"                              if ($val eq 11);
				$variable = "ES  ECART DU POINT DE ROSEE"                  if ($val eq 18);
				$variable = "UU  COMPOSANTE U DU VENT (SELON L AXE DES X)" if ($val eq 33);
				$variable = "VV  COMPOSANTE V DU VENT (SELON L AXE DES Y)" if ($val eq 34);
				$variable = "PR  PRECIP"                                   if ($val eq 61);
				
				print DEST " variable = \"$variable\",\n";
				last SWITCH;
			};
		
			/^4A/ && do {	# 4e ligne: 
				my $variable = (($Phour eq "P000") ? "A ANALYSE" : "P PREVISION"); 				
				
				print DEST " nature = \"$variable\",\n";
				last SWITCH;
			};
			
			/^6A/ && do {	# 6e ligne: units = .... 
				my $val = $f->pds_attribute(9);
				my $units;
				
				$units = "Gpm"   if ($val eq  7);
				$units = "K"     if ($val eq 11);
				$units = "K"     if ($val eq 18);
				$units = "m/s"   if ($val eq 33);
				$units = "m/s"   if ($val eq 34);
				$units = "kg/m2" if ($val eq 61);

				print DEST " units = \"$units\",\n";
				last SWITCH;
			};
			
			/^7A/ && do {	# 7e ligne: 
				#my @val = ($f->gds_attribute(13),$f->gds_attribute(14),$f->gds_attribute(15),
				#           $f->gds_attribute(16),$f->gds_attribute(17));
				#my $variable = "20020601.000000";
				
				print DEST " date = \"$yyyymmdd.$hh\",\n";
				last SWITCH;
			};
			
			/^12A/ && do {	# 12e ligne: level = .... (pression)
				my $val = $f->pds_attribute(11);

				printf DEST " level =    %f    ,\n", $val;
				last SWITCH;
			};
		
			/^15A/ && do {	# 15e ligne: ni = ...  (il s'agit du nombre de cellules en longitude)
				my $val = $f->gds_attribute(7);				
				print DEST " ni =          $val,\n";				
				last SWITCH;
			};

			/^16A/ && do {	# 16e ligne: nj = ...  (idem en latitude)
				my $val = $f->gds_attribute(9);				
				print DEST " nj =          $val,\n";
				last SWITCH;
			};

			/^18A/ && do {	# 18e ligne: 
				my $val = $f->gds_attribute(6);								
				my $variable = "L (LATLON)";

				die("GDS(6) != 0  not supported") unless ($val == 0);				
				print DEST " mapproj = \"$variable\",\n";
				last SWITCH;
			};
			
			/^23A/ && do {	# 23e ligne: meshlat = ... (taille du pixel)
				my $first = $f->gds_attribute(11);
				my $last  = $f->gds_attribute(18);
				my $inc   = $f->gds_attribute(9);				
				my $val = ($last - $first) / ($inc - 1); 
								
				#print "lat $first $last $inc\n";
				print DEST " meshlat =    $val    ,\n";
				last SWITCH;
			};

			/^24A/ && do {	# 24e ligne: meshlon= ... (taille du pixel)
				my $first = $f->gds_attribute(14);
				my $last  = $f->gds_attribute(21);
				my $inc   = $f->gds_attribute(7);								
				$last = $last + 360 if ($first == $last);				
				my $val = ($last - $first) / ($inc - 1); 
				
				#print "lon $first $last $inc\n";				
				print DEST " meshlon =    $val    ,\n";
				last SWITCH;
			};

			/^25A/ && do {	# 25e ligne: swlat = ... (latitude sud ouest)
				my $val = $f->gds_attribute(11);
				printf DEST " swlat =   %f    ,\n", $val;	
				last SWITCH;
			};
															
			/^26A/ && do {	# 26e ligne: swlon = ... (longitude sud ouest)
				my $val = $f->gds_attribute(14);
				printf DEST " swlon =   %f,\n", $val;
				last SWITCH;
			};
			
			/^43A/ && do {	# STOP_DATA label
				my $data = $f->read_data($gh->{_FILEHANDLE});
				
				my $count = 0;
				for (my $j = 0; $j < $data->getdim(1); $j++) {
					for (my $i = 0; $i < $data->getdim(0); $i++) {				
						#print DEST $data->at($i, $j) . "\t";				
						
						printf(DEST "%14.6f ",$data->at($i, $j));
						$count++;
						print DEST "\n" if ($count % 4 == 0);
					}
				}
				
				print DEST "\n" if ($count %4 != 0);
				print DEST "$line\n";
				last SWITCH;
			};
			
			print DEST "$line";
		}  
	}

	close(DEST);
}


sub mainold {
	my ($gribFile, $source, $dest) = @_;

	my $gh = new PDL::IO::Grib($gribFile);
	my $f = $gh->anyfield();

	print "QQ1 " . $f->gds_attribute(24) . "\n";
	print "QQ2 " . $f->gds_attribute(26) . "\n";

					
	
	open(SOURCE, "< $source") or die("input $source : $!");
	open(DEST,   "> $dest")   or die("output $dest : $!");
	my $lineNumber = 0;
	
	while(my $line = <SOURCE>) {						
		$lineNumber++;
		
		$_ = $lineNumber . "A";		
		#print $_ . " ";
			
		SWITCH: {	
			/^3A/ && do {	# 3e ligne: variable = ... "UU "VV ou autre			
				# HGT  007 = GZ
				# TMP  011 = TT
				# DEPR 018 = ES
				# UGRD 033 = UU
				# VGRD 034 = VV
				# APCP 061 = PR
			
				my $val = $f->pds_attribute(9);				
				my $variable;
				
				$variable = "GZ  GEOPOTENTIEL" if ($val eq  7);
				$variable = "TT  TEMPERATURE" if ($val eq 11);
				$variable = "ES  ECART DU POINT DE ROSEE" if ($val eq 18);
				$variable = "UU  COMPOSANTE U DU VENT (SELON L AXE DES X)" if ($val eq 33);
				$variable = "VV  COMPOSANTE V DU VENT (SELON L AXE DES Y)" if ($val eq 34);
				$variable = "PR  PRECIP" if ($val eq 61);
				
				print DEST " variable = \"$variable\",\n";
				last SWITCH;
			};
		
			/^12A/ && do {	# 12e ligne: level = .... (pression)
				my $val = $f->pds_attribute(11);

				printf DEST " level =    %f    ,\n", $val;
				last SWITCH;
			};
		
			/^15A/ && do {	# 15e ligne: ni = ...  (il s'agit du nombre de cellules en longitude)
				my $val = $f->gds_attribute(7);				
				print DEST " ni =          $val,\n";				
				last SWITCH;
			};

			/^16A/ && do {	# 16e ligne: nj = ...  (idem en latitude)
				my $val = $f->gds_attribute(9);				
				print DEST " nj =          $val,\n";
				last SWITCH;
			};

			/^23A/ && do {	# 23e ligne: meshlat = ... (taille du pixel)
				my $first = $f->gds_attribute(11);
				my $last  = $f->gds_attribute(18);
				my $inc   = $f->gds_attribute(9);				
				my $val = ($last - $first) / ($inc - 1); 
								
				print "AA $last $first $inc \n";
				print DEST " meshlat =    $val    ,\n";
				last SWITCH;
			};

			/^24A/ && do {	# 24e ligne: meshlon= ... (taille du pixel)
				my $first = $f->gds_attribute(14);
				my $last  = $f->gds_attribute(21);
				my $inc   = $f->gds_attribute(7);								
				$last = $last + 360 if ($first == $last);				
				my $val = ($last - $first) / ($inc - 1); 
				
				print "BB $last $first $inc \n";
				print DEST " meshlon =    $val    ,\n";
				last SWITCH;
			};

			/^25A/ && do {	# 25e ligne: swlat = ... (latitude sud ouest)
				my $val = $f->gds_attribute(11);
				printf DEST " swlat =   %f    ,\n", $val;	
				last SWITCH;
			};
															
			/^26A/ && do {	# 26e ligne: swlon = ... (longitude sud ouest)
				my $val = $f->gds_attribute(14);
				printf DEST " swlon =   %f,\n", $val;
				last SWITCH;
			};
			
			/^43A/ && do {	# STOP_DATA label
				my $data = $f->read_data($gh->{_FILEHANDLE});
				
				my $count = 0;
				for (my $j = 0; $j < $data->getdim(1); $j++) {
					for (my $i = 0; $i < $data->getdim(0); $i++) {				
						#print DEST $data->at($i, $j) . "\t";				
						
						printf(DEST "%14.6f ",$data->at($i, $j));
						$count++;
						print DEST "\n" if ($count % 4 == 0);
					}
				}
				
				print DEST "\n" if ($count %4 != 0);
				print DEST "$line\n";
				last SWITCH;
			};
			
			print DEST "$line";
		}  
	}

	close(DEST);
	close(SOURCE);
}

=head1 NAME

CMC2asc.pl - 

=head1 SYNOPSIS

downloadAERONET.pl [OPTIONS]

=head2 Options

Recognized options include:

=over 4

=item -h, --help

Display this help screen.

=item -inputDir=INPUTDIR

Set the input directory (default: .)

=item -outputDir=OUTPUTDIR

Set the output directory (default: .)

