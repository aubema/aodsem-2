#!/usr/bin/tixwish

###########################################################################
#Execution chronologique d'aodsem
#
#Jean-Denis Giguere 2003
#
#Ce script lit le fichier de configuration produit par aoda6 en mode
#chronologique puis execute les differents scripts d'aodsem
#
###########################################################################

###########################################################################
#Variables
set AODSEM_HOME [ file nativename ~ ]
set AODA6_ARTS [file join $AODSEM_HOME AODSEM Nesfiles Aoda6-arts]
#Variables
###########################################################################

###########################################################################
#Source a inclure
source [ file join $AODSEM_HOME AODSEM Source Aoda6 aoda6_proc.tcl ]
#
image create photo aide-logo -file $AODA6_ARTS/aide.gif
###########################################################################

#Nettoyage du reperoire
$AODSEM_HOME/AODSEM/Scripts/rmdln

open_sess $argc $argv


set sa6_exec_chra6 "0"
save

set sa6_etape .chr_exec
cd $sa6_dir_trav
#Nettoyage du reperoire
$AODSEM_HOME/AODSEM/Scripts/rmdln


set out_f [open "output.pa6" {RDWR CREAT TRUNC}]
puts $out_f "Debut de l'execution de a6chr.tcl"
close $out_f

	#Copie des donnees meteos
	if {$sa6_dat_meteo != "NULL"} then {
		foreach f_meteo $sa6_dat_meteo {
			exec /bin/ln -s -f $f_meteo $sa6_dir_trav
		}
	}

	#Conversion de fichier GEM vers les formats AODSEM
	switch $sa6_gem_conv {
		"NULL" {}

		0 {}

		1 {
			cd $sa6_dir_trav
			#Nettoyage du reperoire
			$AODSEM_HOME/AODSEM/Scripts/rmdln

			#creation des listes chronologiques des fichiers GEM
			set l_gem_ana [open listegem-ana.pa6 {RDWR CREAT TRUNC} ]
			set l_gem_pcp [open listegem-pcp.pa6 {RDWR CREAT TRUNC} ]
			lsort $sa6_dat_meteo
			foreach fich $sa6_dat_meteo {
				if {[regexp ana $fich] == 1} then {
				puts $l_gem_ana [lindex [file split $fich] end]
			} elseif {[regexp pcp $fich] == 1} then {
				puts $l_gem_pcp [lindex [file split $fich] end]
			}

			}
			close $l_gem_ana
			close $l_gem_pcp

			#gemin
			set gemin_pa6 [open gemin.pa6 {RDWR CREAT TRUNC} ]
			puts $gemin_pa6 "listegem-pcp.pa6"
			puts $gemin_pa6 "listegem-ana.pa6"
			close $gemin_pa6
			exec /usr/bin/nohup gemin < gemin.pa6 >>& "output.pa6"

			#gem22
			set gem22_pa6 [open gem22.pa6 {RDWR CREAT TRUNC} ]
			#sa6_gem_step ne peut prendre qu'une seule valeur
			#dans gem22
			#puts $gem22_pa6 $sa6_gem_step
			#close $gem22_pa6
			#exec /usr/bin/nohup gem22 < gem22.pa6 >>& "output.pa6"
			exec /usr/bin/nohup gem22 >>& "output.pa6"

		}

		default {}
	}

		#Creation des index meteo

		if {$sa6_cr_ind_wsp == "1"} then {
			exec /usr/bin/nohup wspin >>& "output.pa6"
		}
			
		if {$sa6_cr_ind_tt == "1" } then {
			exec /usr/bin/nohup ttind >>& "output.pa6"
		}
		
		if {$sa6_cr_ind_rhu == "1" } then {
			exec /usr/bin/nohup rhuin >>& "output.pa6"
		}
		
		if {$sa6_cr_ind_pr == "1" } then {
			exec /usr/bin/nohup prind >>& "output.pa6"
		}

		#Fichier *.bmi
		
		foreach fich $sa6_bmi_fich {
				exec /bin/ln -fs $fich $sa6_dir_trav
		}
		set bmi_fich [lindex $sa6_bmi_fich 0]
		set bmi_root_name [lindex [file split $bmi_fich] end]
		set bmi_root_name [file rootname $bmi_root_name]
		set bmi_root_name [file rootname $bmi_root_name]
		set bmi_res_ok "[file isfile $bmi_root_name.res.bmi]" 
		set bmi_pfc_ok "[file isfile $bmi_root_name.pfc.bmi]"
		set bmi_bns_ok "[file isfile $bmi_root_name.bns.bmi]"
		set bmi_ref_ok "[file isfile $bmi_root_name.ref.bmi]"

		set bmi_f_ok "[expr $bmi_res_ok + 0]"
		set bmi_i_ok "[expr $bmi_bns_ok + $bmi_ref_ok + $bmi_pfc_ok]"
		
		if {$bmi_f_ok == 1} then {
			#Rien a faire	
		} elseif {$bmi_i_ok != 3} then {
			set out_f [open "output.pa6" {RDWR APPEND}]
			puts $out_f "IMPOSSIBLE DE CREER LES .BMI NECESSAIRE"
			close $out_f
		} else {
			set bmies_pa6 [open "bmies.pa6" {RDWR CREAT TRUNC}]
			puts $bmies_pa6 $bmi_root_name
			close $bmies_pa6
		
			exec /usr/bin/nohup bmies < "bmies.pa6" >>& "output.pa6"
		}

		if {$sa6_dom_pix != "NULL"} then {
			file delete -force "domain.par"
			set domain_pa6 [open "domain.pa6" {RDWR CREAT TRUNC}]
			puts $domain_pa6 $sa6_dom_pix
			puts $domain_pa6 $sa6_dom_nlat
			puts $domain_pa6 $sa6_dom_nlong
			puts $domain_pa6 $sa6_dom_lat0
			puts $domain_pa6 $sa6_dom_long0
			close $domain_pa6
			exec domai < "domain.pa6" >>& "output.pa6"
		}

		#set-size
		set set_size_pa6 [open "set_size.pa6" {RDWR CREAT TRUNC}]
		puts $set_size_pa6 $sa6_dom_nlat
		puts $set_size_pa6 $sa6_dom_nlong
		close $set_size_pa6
		exec /usr/bin/nohup $AODSEM_HOME/AODSEM/Scripts/set-size  < "set_size.pa6" >>& "output.pa6"

		#mkcli
		if {$sa6_clim_get == 1} then {
			foreach file $sa6_clim_list {
				exec ln -sf $file $sa6_dir_trav
				#creer le fichier buffer.index
				if {$sa6_clim_new != 1} then {
					source $AODSEM_HOME/AODSEM/Source/Aoda6/a6chr_mk_buffin.tcl
				}
			}
		} elseif {$sa6_clim_new == 1} then {
			set mkcli_pa6 [open "mkcli.pa6" {RDWR CREAT TRUNC}]
			puts $mkcli_pa6 $sa6_clim_sais
			puts $mkcli_pa6 $sa6_clim_wl
			puts $mkcli_pa6 $bmi_root_name
			close $mkcli_pa6

			exec /usr/bin/nohup mkcli < "mkcli.pa6" >>& "output.pa6"
		}

		#mksrc

		if {$sa6_src_get == 1} then {
			foreach file3ds $sa6_src_list {
				exec /bin/ln -sf $file3ds $sa6_dir_trav
			}
			file delete -force "makesource.par"
			set mksrc_pa6 [open "mksrc.pa6" {RDWR CREAT TRUNC}]
			puts $mksrc_pa6 $sa6_start
			puts $mksrc_pa6 $sa6_end
			puts $mksrc_pa6 [llength $sa6_src_list]
			foreach srcf $sa6_src_list {
				puts $mksrc_pa6 [lindex [file split [file rootname $srcf]] end]
			}
			puts $mksrc_pa6 $sa6_src_ssalt
			close $mksrc_pa6

			exec /usr/bin/nohup mksrc < "mksrc.pa6" >>& "output.pa6"
		}

		#freem

		if { $sa6_freem_nassim == 0 } then {
			set freem_pa6 [open "freem.pa6" {RDWR CREAT TRUNC}]
			puts $freem_pa6 $sa6_freem_wet
			puts $freem_pa6 $sa6_freem_start
			puts $freem_pa6 $sa6_freem_end
			puts $freem_pa6 $sa6_freem_step
			puts $freem_pa6 $sa6_freem_buff
			puts $freem_pa6 $sa6_freem_src
			puts $freem_pa6 [lindex $sa6_freem_wl 0]
			puts $freem_pa6 $sa6_freem_smooth
			puts $freem_pa6 $bmi_root_name
			puts $freem_pa6 $sa6_freem_ityp
			if {$sa6_freem_ityp != 2 } then  {
				puts $freem_pa6 $sa6_freem_if
			}
			puts $freem_pa6 $sa6_freem_fn
			puts $freem_pa6 $sa6_freem_dat
			close $freem_pa6

			exec /usr/bin/nohup freem < "freem.pa6" >>& "output.pa6"
		} elseif { $sa6_freem_nassim > 0} then {
			set rdata_pa6 [open "rdata.pa6" {RDWR CREAT TRUNC}]
			puts $rdata_pa6 $sa6_freem_mod
			puts $rdata_pa6 $sa6_freem_ityp
			if {$sa6_freem_ityp != 2} then {
				puts $rdata_pa6 $sa6_freem_if
			}
			puts $rdata_pa6 $sa6_freem_step
			puts $rdata_pa6 $sa6_freem_buff
			puts $rdata_pa6 $sa6_freem_src
			puts $rdata_pa6 $sa6_freem_wet
			puts $rdata_pa6 [concat [lindex $sa6_freem_wl 0] [lindex $sa6_freem_wl 1]]
			puts $rdata_pa6 $sa6_freem_smooth
			puts $rdata_pa6 $bmi_root_name
			puts $rdata_pa6 $sa6_freem_fn
			puts $rdata_pa6 $sa6_freem_nassim
			puts $rdata_pa6 $sa6_freem_start
			#Ajout de la liste de fichiers et de dates
			for {set iter 0} {$iter < $sa6_freem_nassim} {set iter [expr $iter + 1 ] } {
				#Pointeur dans la liste
				set p_c [expr $iter * 7]
				exec /bin/ln -sf [lindex $sa6_freem_lassim $p_c] .
				set p_f [file rootname [file tail [lindex $sa6_freem_lassim $p_c]]]
				puts $rdata_pa6 $p_f
				
				#Verifie si le pgm contient la date
				set rdata_tmp1 [open $p_f.pgm {RDONLY}]
				gets $rdata_tmp1
				gets $rdata_tmp1
				set rdata_tmp1_u [concat [gets $rdata_tmp1]]
				set rdata_tmp1_v [lindex $rdata_tmp1_u 1]

				set f_cont_date [expr {"date" == $rdata_tmp1_v} ]
				
				if { $f_cont_date == 0} then {
					#Ajoute la date si elle n'est pas contenu dans le fichier
					puts $rdata_pa6 [concat [lindex $sa6_freem_lassim [expr $p_c + 1]] \
					[lindex $sa6_freem_lassim [expr $p_c + 2]] \
					[lindex $sa6_freem_lassim [expr $p_c +3]] \
					[lindex $sa6_freem_lassim [expr $p_c + 4]] \
					[lindex $sa6_freem_lassim [expr $p_c + 5]] \
					[lindex $sa6_freem_lassim [expr $p_c + 6]]]
				}
			}
			close $rdata_pa6
			exec /bin/rm -f repdatas.par
			exec /usr/bin/nohup rdata < "rdata.pa6" >>& "output.pa6"
			
		}
		
		
	
set sa6_exec_fin 1
save

message .bravo -width 400 -text {
L'execution d'AODSEM est maintenant completee. Vous pouvez consultez le fichier output.pa6 pour connaitre l'historique de l'execution.
}

button .b_ok -text "OK" -command {exit}
button .b_help -image aide-logo -command {pls_help "www.tcl.tk"}

grid .bravo
grid .b_ok -pady 10
grid .b_help -sticky {s e} -padx 3 -pady 3

wm title . "Fin d'AODSEM"
