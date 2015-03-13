#!/usr/bin/perl -w

package Meteo;
################################################################################
# Class use for pressure data
#
################################################################################
use PDL;
use PDL::IO::Grib;
use Date::Calc;

use lib("/home/level3/bin");
use MeteoNetCDF;

my $section = undef;
my $pi = acos(-1.0);

################################################################################
# Class constructor - Set the attributes of the object.
#
#   Parameters:
#     None
#
#   Returns:
#     Class reference
################################################################################
sub new {
	my ($proto, $refLonInfo, $refLatInfo, $inputDir, $refDate, $hour, $outputDir) = @_;
	my $class = ref($proto) || $proto;
	my $self  = {};

	# Initialize class attributes
	$self->{lonInfo}  = $refLonInfo;
	$self->{latInfo}  = $refLatInfo;
	$self->{inputDir} = $inputDir;
	$self->{refDate}  = $refDate;
	$self->{hour}     = $hour;
							
	$self->{ni}    = $refLonInfo->[2];
	$self->{nj}    = $refLatInfo->[2];
	$self->{nk}    = 10;
	$self->{xRes}  = $refLonInfo->[1];;
	$self->{yRes}  = $refLatInfo->[1];;
					
	# Output
	$self->{level}  = pdl([1000, 925, 850, 700, 500, 400, 300, 250, 200, 150, 100, 50]);
	$self->{hcellz} = pdl([82.5, 255, 443.5, 651, 883, 1146, 1448.5, 1805.5, 7000, 21000]);
	$self->{lcellz} = pdl([165, 180, 197, 218, 246, 280, 325, 389, 10000, 18000]);
	$self->{P}      = undef;
	$self->{dP_dz}  = undef;
		
	if (defined($outputDir)) {
		$self->{ncobjOut} = new MeteoNetCDF($refLonInfo, $refLatInfo);		
		$self->{ncobjOut}->setVecTime(pdl([$hour]), sprintf("hours since %4d-%02d-%02d %02d:%02d", @$refDate));				
		$self->{ncobjOut}->setVecLevel($self->{lcellz}, "none");	
		
		my $dir        = sprintf("%04d.%02d.%02d", @$refDate);
		my $ncFilename = sprintf("meteo.%04d%02d%02d%02d.nc", @$refDate);
				
		$self->{filename} = "$outputDir/$dir/$ncFilename";		
	}
			
	bless ($self, $class);
	return $self;
}

################################################################################
# Class destructor
#
#   Parameters:
#     None
#
#   Returns:
#     None
################################################################################
sub DESTROY {
	my $self = shift;	
	
	$self->{ncobjOut}->close() if defined($self->{ncobjOut})
}

################################################################################
# Get date
#
#   Parameters:
#     None
#
#   Returns:
#     Date
################################################################################
sub getDate {
	my $self = shift;	

	return "@{$self->{refDate}}, $self->{hour}";
}

################################################################################
# Get a variable for the NetCDF file.
#
#   Parameters:
#     $varName	Variable name
#
#   Returns:
#     PDL object
################################################################################
sub get {
	my ($self, $varName) = @_;		
	
	print "varname $varName\n";
	
	my $data = readGrib($varName, $self->{inputDir}, $self->{refDate}, $self->{hour}, $self->{lonInfo}, $self->{latInfo});
	
	return $data;	
}

sub readGrib {
	my ($parameter, $inputDir, $refDate, $hour, $refLonInfo, $refLatInfo) = @_;

	die("@$refLonInfo: lonStep not supported") if ($refLonInfo->[1] != 1);
	die("@$refLatInfo: latStep not supported") if ($refLatInfo->[1] != 1);
	
	my $Center         = "CMC";
	my $model          = "glb";
	my $grid_qualifier = "latlon1x1";	
	
	my $date1    = sprintf("%04d%02d%02d%02d", @$refDate);
	my $Phour1   = sprintf("P%03d", $hour);
	my @vecDate2 = Date::Calc::Add_Delta_DHMS(@$refDate, 0, -12, 0, 0);
	my $date2    = sprintf("%04d%02d%02d%02d", @vecDate2);
	my $Phour2   = sprintf("P%03d", $hour + 12);
					
	my $level_type = ($parameter eq "APCP" ? "SFC" : "ISBL");	
	my @vecLevel   = ($parameter eq "APCP" ? (0) : (1000, 925, 850, 700, 500, 400, 300, 250, 200, 150, 100, 50));
	
	my $data     = zeroes(float, $refLonInfo->[2], $refLatInfo->[2], scalar(@vecLevel));
	my $levelInd = 0;					
	
	my $i1 = $refLonInfo->[0] + ($refLonInfo->[0] >= 180 ? -180 : 180); 
	my $i2 = $i1 + $refLonInfo->[2] - 1;	
	my $j1 = $refLatInfo->[0] + 85; 
	my $j2 = $j1 + $refLatInfo->[2] - 1;

	die("i2 out of lon bounds: $i2 > 360") if ($i2 > 360);
	die("j2 out of lat bounds: $j2 > 170") if ($j2 > 170);
			
	foreach my $level (@vecLevel) {
		my $gribFile = "${Center}_${model}_${parameter}_${level_type}_${level}_${grid_qualifier}_${date1}_${Phour1}.grib";				
		$gribFile = sprintf("%04d.%02d.%02d/%02d/$gribFile", @$refDate);
		
		if (! -e "$inputDir/$gribFile") {				
			$gribFile = "${Center}_${model}_${parameter}_${level_type}_${level}_${grid_qualifier}_${date2}_${Phour2}.grib";
			$gribFile = sprintf("%04d.%02d.%02d/%02d/$gribFile", @vecDate2);
		}
						
		if (-e "$inputDir/$gribFile") {
			my $gh = new PDL::IO::Grib("$inputDir/$gribFile");
			my $f  = $gh->anyfield();														
									
			$data->slice(":,:,$levelInd") .= ($f->read_data($gh->{_FILEHANDLE}))->slice("$i1:$i2,$j1:$j2");
		}
		else {		
			die("$inputDir/$gribFile unavailable");
		}				
		
		$levelInd++;				
	}
					
	return $data;
}

################################################################################
# TBD
#
#   Parameters:
#     None
#
#   Returns:
#     None
################################################################################
sub getP {
	my ($self) = @_;		
		
	return $self->{P};
}

################################################################################
# TBD
#
#   Parameters:
#     None
#
#   Returns:
#     None
################################################################################
sub calculatePAnddP_dz {	
	my ($self) = @_;		

	my $hgt    = $self->get("HGT");
	my $plevel = $self->{level};	
	my $hcellz = $self->{hcellz};
	
	$self->{P}     = zeroes($self->{ni}, $self->{nj}, $self->{nk});
	$self->{dP_dz} = zeroes($self->{ni}, $self->{nj}, $self->{nk});  		   
					
	for (my $nl = 0; $nl < 12; $nl++) {		
		my $ind1 = ($nl < 11 ? $nl     : $nl - 1);
		my $ind2 = ($nl < 11 ? $nl + 1 : $nl    );					
		my $hgt1 = $hgt->slice(":,:,$ind1");		
		my $hgt2 = $hgt->slice(":,:,$ind2");
		
		my $h0 = ($hgt2 - $hgt1) / log($plevel->at($ind1) / $plevel->at($ind2)); 			
					
		my $P0 = undef;
		$P0 = $plevel->at($ind2) / exp(-$hgt2 / $h0) if ($nl == 11);;
		$P0 = $plevel->at($ind1) / exp(-$hgt1 / $h0) unless defined($P0);
			
		for (my $k = 0; $k < $self->{nk}; $k++) {		
			my $tmp = $P0 * exp(-$hcellz->at($k) / $h0);			
			
			my $filter = (($hgt1 < $hcellz->at($k)) * ($hgt2 >= $hcellz->at($k)));		
			$filter = $filter + ($hgt1 > $hcellz->at($k)) if ($nl == 0);
			$filter = $filter + ($hgt2 < $hcellz->at($k)) if ($nl == 11);
			$filter = ($filter > 0);
			
			$self->{P}->slice(":,:,$k")     .= $self->{P}->slice(":,:,$k")     * ($filter == 0) +  $tmp * $filter;
			$self->{dP_dz}->slice(":,:,$k") .= $self->{dP_dz}->slice(":,:,$k") * ($filter == 0) + -$tmp * $filter / $h0;
		}				
	}	
	
	if (sum($self->{dP_dz} == 0) > 0) {
		die("@$self->{refDate}, $self->{hour} :dP_dz == 0");
	}
}   	

################################################################################
# TBD
#
#   Parameters:
#     None
#
#   Returns:
#     None
################################################################################
sub getdP_dxdy {
	my ($self) = @_;		       	
	
	my $lat       = pdl([1..$self->{latInfo}->[2]]) + $self->{latInfo}->[0] - 1;	
	my ($dx, $dy) = echelle($lat, $self->{xRes}, $self->{yRes});	
	
	my $dP_dx = zeroes($self->{ni}, $self->{nj}, $self->{nk});
	my $dP_dy = zeroes($self->{ni}, $self->{nj}, $self->{nk});
	
	$dx->reshape(1, $dx->nelem());
	for (my $i = 0; $i < $self->{ni}; $i++) {	 
		my $ind1 = ($i > 0               ? $i - 1 : $i);
		my $ind2 = ($i < $self->{ni} - 1 ? $i + 1 : $i);
		
		my $p1 = $self->{P}->slice("$ind1,:,:");  # 171x10		 
		my $p2 = $self->{P}->slice("$ind2,:,:");		
				
		$dP_dx->slice("$i,:,:") .= ($p2 - $p1) / $dx / ($ind2 - $ind1);						
	}
	
	for (my $j = 0; $j < $self->{nj}; $j++) {	 
		my $ind1 = ($j > 0               ? $j - 1 : $j);
		my $ind2 = ($j < $self->{nj} - 1 ? $j + 1 : $j);
		
		my $p1 = $self->{P}->slice(":,$ind1,:"); # 361x10		
		my $p2 = $self->{P}->slice(":,$ind2,:");		
				
		$dP_dy->slice(":,$j,:") .= ($p2 - $p1) / $dy->at($j) / ($ind2 - $ind1);						
	}		
	
	return ($dP_dx, $dP_dy);						
}

################################################################################
# TBD
#
#   Parameters:
#     None
#
#   Returns:
#     None
################################################################################
sub getdP_dt {
	my ($self, $P, $dt) = @_;		

	return ($P - $self->{P}) / $dt;
}

################################################################################
# Set the vertical wind speed.
#
#   Parameters:
#     None
#
#   Returns:
#     None
################################################################################
sub getVZ {
	my ($self, $P, $dt) = @_;		
		
	my $dP_dt            = $self->getdP_dt($P, $dt);
	my ($dP_dx, $dP_dy)	 = $self->getdP_dxdy();
	
	my $filter = ($self->{dP_dz} != 0);
	
	my $vv = projection($self->{P}, $self->{level}, $self->get("VGRD"));			
	my $uu = projection($self->{P}, $self->{level}, $self->get("UGRD"));
	
	my $vz = (-$dP_dt - $vv * $dP_dy - $uu * $dP_dx) / $self->{dP_dz} * $filter;	
	
	if (defined($self->{ncobjOut})) {	
		$self->{ncobjOut}->write($self->{filename}, "VV", "Speed", "TBD", float($vv));
		$self->{ncobjOut}->write($self->{filename}, "UU", "Speed", "TBD", float($uu));
		$self->{ncobjOut}->write($self->{filename}, "VZ", "Speed", "TBD", float($vz));
	}
	
	if (defined($section)) {
		print $self->{P}->slice($section);
		print $self->{dP_dz}->slice($section);
		print $dP_dx->slice($section);
		print $dP_dy->slice($section);
		print $dP_dt->slice($section);
		print $vv->slice($section);
		print $uu->slice($section);	
		print $vz->slice($section);
	}
	
	return $vz;
}

################################################################################
# Get the relative humidity.
#
#   Parameters:
#     None
#
#   Returns:
#     None
################################################################################
sub getRH {
	my ($self) = @_;		

	my $tt = projection($self->{P}, $self->{level}, $self->get("TMP")) - 273.15;
	my $es = projection($self->{P}, $self->{level}, $self->get("DEPR"));
	
	if (sum($es < 0) > 0) {
		die("@$self->{refDate}, $self->{hour} :es < 0");	
	}
	
    my $ea  = 6.1070 * (1 + sqrt(2) * sin($pi * ($tt - $es) / (3 * 180)))**8.827;
    my $eta = 6.1070 * (1 + sqrt(2) * sin($pi *     $tt     / (3 * 180)))**8.827;
         	
	my $rh = rint(100 * $ea / $eta);
	
	if (defined($self->{ncobjOut})) {	
		$self->{ncobjOut}->write($self->{filename}, "TMP", "Temperature"      , "C", float($tt));
		$self->{ncobjOut}->write($self->{filename}, "RH" , "Relative humidity", "%", byte($rh));
	}
		
	if (defined($section)) {
		print $tt->slice($section);	
		print $es->slice($section);
		print $rh->slice($section);
	}	
	
	return $rh;			
}

################################################################################
# Get the precipitation
#
#   Parameters:
#     None
#
#   Returns:
#     None
################################################################################
sub getWet {
	my ($self) = @_;		

	my $apcp = $self->get("APCP");
	
	$apcp->reshape($self->{ni}, $self->{nj});
	
	if (defined($self->{ncobjOut})) {	
		$self->{ncobjOut}->write($self->{filename}, "APCP", "Total precipitation", "kg/m2", float($apcp));
	}
	
	return $apcp;			
}
################################################################################
# TBD
#
#   Parameters:
#     None
#
#   Returns:
#     None
################################################################################
sub projection {
	my ($P, $plevel, $data) = @_;		
	
	my $newData = zeroes($data->dims());
	my $filter;
		
	$filter  = ($P > $plevel->at(0));			
	$newData = $newData * ($filter == 0) + $data->slice(":,:,0") * $filter;
			
	$filter  = ($P < $plevel->at(11));		
	$newData = $newData * ($filter == 0) + $data->slice(":,:,11") * $filter;
								
	for (my $k = 0; $k < $data->getdim(2); $k++) {		
		for (my $nl = 0; $nl < 11; $nl++) {		
			my $ind1 = $nl;
			my $ind2 = $nl + 1;
						
			my $data1 = $data->slice(":,:,$ind1");
			my $data2 = $data->slice(":,:,$ind2");
						
			my $m = ($data2 - $data1) / ($plevel->at($ind2) - $plevel->at($ind1));
			my $b = $data1 - $m * $plevel->at($ind1);
			
			$filter  = ($P <= $plevel->at($ind1)) * ($P >= $plevel->at($ind2));					
			$newData = $newData * ($filter == 0) + ($m * $P + $b) * $filter;			
		}					
	}				
	
	return $newData;
}

################################################################################
# TBD
#
#   Parameters:
#     None
#
#   Returns:
#     None
################################################################################
# Note:
#
#   routine permettant le calcul de l echelle d une cellule centree
#   a latt et avec une largeur angulaire dang pour une terre 
#   ellipsoide.
#
#   epsilon = difference d angle entre l horizon sur une sphere et
#             l horizon sur un ellipsoide
#   dangx =  taille de la cellule en degres
#   dangy =  taille de la cellule en degres
#   dx = taille de la cellule dans la direction s-n [m]
#   dy = taille de la cellule dans la direction w-e [m]
#   r = distance du point geographique par rapport au centre de la terre
#   a = demi grand axe (centre a l equateur)
#   b = demi petit axe (centre au pole)
#   he = angle d horizon de l ellipsoide
#   hs = angle d horizon de la sphere
#   x = coord de l ellipsoide
#   y=  coord de l ellipsoide
################################################################################
sub echelle {	  
	my ($latt, $dangx, $dangy) = @_;

   	$latt = abs($latt);      	  
#	$latt = 0.01 if ($latt < 0.01);

	my $filter = ($latt < 0.01);	
	$latt = $latt * ($filter == 0) + $filter * 0.01;	
	
    my $b  = 6357000.;
    my $a  = 6378000.;
			
    my $x = (($b**2.) / ((tan($pi * $latt / 180.))**2. + ($b / $a)**2.))**0.5;
    my $y = ($b**2. - $b**2. * $x**2. / $a**2.)**0.5;
    my $r = ($x**2. + $y**2.)**.5;
    
	my $hs = 90. - $latt;
    my $he = 180. * atan((($b**2.) / ($a**2.)) * $x / $y) / $pi;
    my $epsilon = $hs - $he;
    
    my $dx = $x * (tan($dangy * $pi / 180.));
	my $dy = $r * (tan($dangx * $pi / 180.)) / cos($pi * $epsilon / 180.);
      	  
	return ($dx, $dy);
}

