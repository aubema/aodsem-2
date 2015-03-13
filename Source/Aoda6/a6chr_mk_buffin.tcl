#Ce script fait la liste de tous les les fichiers a utiliser dans la climatologie
#Il y ajoute la date associee a chaque fichier et le nombre de fichiers en premiere ligne

cd $sa6_dir_trav

set buffer_ind [open "buffer.index" {RDWR CREAT TRUNC}]

puts $buffer_ind [llength $sa6_clim_list]
foreach file $sa6_clim_list {
	set file_name [file tail $file]
	
	#On trouve la date dans le fichier
	set file_dat [open $file_name {RDONLY}]
	gets $file_dat
	gets $file_dat
	set date_file [concat [gets $file_dat]]
	puts $buffer_ind [concat [lindex $date_file 0] [lindex $date_file 1] [lindex $date_file 2] \
	[lindex $date_file 3] [lindex $date_file 4] [lindex $date_file 5] $file_name]

	close $file_dat
}

close $buffer_ind
