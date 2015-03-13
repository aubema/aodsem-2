##########################################################################
#aoda6_proc.tcl
#
#Fichier contenant tous les programmes appeles par aoda6.tcl
#
#Jean-Denis Giguere
#
#Creation : 2003-07-25
#
###########################################################################
source $AODSEM_HOME/AODSEM/Source/Aoda6/aoda6_sa6.tcl

##########################################################################
#Procedure vide
proc ::vide {argc argv} {}
#Fin de la procedure vide
###########################################################################

###########################################################################
## Procedure:  getAvOut

proc ::getAvOut {lesdispos} {
	
	array set dispo $lesdispos
	global fr_out_meteo

###########################################################################
	####Liste des programmes et de leurs entrees

	#### Categorie meteo 
	###gem21

	#initialisation
	set gem21 0

	set canGem21 $dispo(GEMv1)
	if {$canGem21 == 1} {
		set gem21 1
	}

	###gem22

	#initialisation
	set gem22 0

	set canGem22 $dispo(GEMv2)
	if {$canGem22 == 1} {
		set gem22 1
	}

	###prind

	#initialisation
	set prind 0
	set canPrind $dispo(pr)
	if { $canPrind == 1 } {
		set prind 1
	}


	###rhuin

	#initialisation
	set rhuin 0
	set canRhuin $dispo(rhu)
	if { $canRhuin == 1 } {
		set rhuin 1
	}

	###ttind

	#initialisation
	set ttind 0
	set canTtind $dispo(tt)
	if { $canTtind == 1 } {
		set ttind 1
	}


	###wspin

	#initialisation
	set wspin 0
	set canWspin $dispo(wsp)
	if { $canWspin == 1 } {
		set wspin 1
	}


	#### Fin de la categorie meteo


	###################################################################
	####Liste des sorties
	####Les entrees sont aussi consideres comme des sorties potentielles

	####Categorie meteo

	#GEMv1
	set avalaible(GEMv1) $dispo(GEMv1)
	#aucun programme ne retourne ce type de fichiers

	#GEMv2
	set avalaible(GEMv2) $dispo(GEMv2)

	#PR
	set avalaible(pr) $dispo(pr)
	$fr_out_meteo.pr configure -state disabled
	if { $gem22 == 1 || $gem21 == 1 } {
		set avalaible(pr) 1
		$fr_out_meteo.pr configure -state active
	}

	###rhu
	set avalaible(rhu) $dispo(rhu)
	$fr_out_meteo.rhu configure -state disabled
	if { $gem22 == 1 || $gem21 == 1 } {
		set avalaible(rhu) 1
		$fr_out_meteo.rhu configure -state active
	}

	###tt
	set avalaible(tt) $dispo(tt)
	$fr_out_meteo.tt configure -state disabled

	if { $gem22 == 1 || $gem21 == 1 } {
		set avalaible(tt) 1
		$fr_out_meteo.tt configure -state active
	}


	###wsp
	set avalaible(wsp) $dispo(wsp)
	$fr_out_meteo.wsp configure -state disabled
	if { $gem22 == 1 || $gem21 == 1 } {
		set avalaible(wsp) 1
		$fr_out_meteo.wsp configure -state active
	}

	##Index PR
	set avalaible(ipr) $dispo(ipr)
	$fr_out_meteo.ipr configure -state disabled
	if { $prind == 1 } {
		set avalaible(ipr) 1
		$fr_out_meteo.ipr configure -state active
	}


	##Index rhu
	set avalaible(irhu) $dispo(irhu)
	$fr_out_meteo.irhu configure -state disabled
	if { $rhuin == 1 } {
		set avalaible(irhu) 1
		$fr_out_meteo.irhu configure -state active
	}


	#Index tt
	set avalaible(itt) $dispo(itt)
        $fr_out_meteo.itt configure -state disabled
	if { $ttind == 1 } {
		set avalaible(itt) 1
		$fr_out_meteo.itt configure -state active
	}


	#Index wsp
	set avalaible(iwsp) $dispo(iwsp)
	$fr_out_meteo.iwsp configure -state disabled
	if { $wspin == 1 } {
		set avalaible(iwsp) 1
		$fr_out_meteo.iwsp configure -state active
	}

	####Fin de la section meteo

	return [array get avalaible]
}

###########################################################################
## Procedure:  setInDispo

proc ::setInDispo {} {

	global inGEMv1
	global inGEMv2
	global inpr
	global inrhu
	global intt
	global inwsp
	global inipr
	global inirhu
	global initt
	global iniwsp

	####Appel des valeurs locales
	##Fichiers de meteo
	set lGEMv2 $inGEMv2 
        set lGEMv1 $inGEMv1
	set lpr $inpr
	set lrhu $inrhu 
	set ltt $intt
	set lwsp $inwsp 
	set lipr $inipr
	set lirhu $inirhu
	set litt $initt
	set liwsp $iniwsp


	####Construction de la matrice de fichiers disponbles
	set inDispo(GEMv2) $lGEMv2
	set inDispo(GEMv1) $lGEMv1
	set inDispo(pr) $lpr
	set inDispo(rhu) $lrhu
	set inDispo(tt) $ltt
	set inDispo(wsp) $lwsp
	set inDispo(ipr) $lipr
	set inDispo(irhu) $lirhu
	set inDispo(itt) $litt
	set inDispo(iwsp) $liwsp

	####Retour de la matrice de fichiers disponibles
	return [array get inDispo]
}

##########################################################################
proc ::show_toplevel_io {status} {
	switch $status {
		0  {wm state .tl_in-out withdrawn}
		1  {wm state .tl_in-out normal}
		default	 {wm state .tl_in-out withdrawn}
	}
}

###########################################################################

###########################################################################
proc ::show_toplevel_navig {status} {
	switch $status {
		0  {wm state .tl_navig withdrawn}
		1  {wm state .tl_navig normal}
		default  {wm state .tl_navig withdrawn}
	}
}
###########################################################################
proc ::close_chr_acc {} {
	wm state .chr_acc withdrawn
}
###########################################################################

###########################################################################
proc ::close_chr_dom {} {
	        wm state .chr_dom withdrawn
	}
###########################################################################

###########################################################################
proc ::close_chr_debfin {} {
               wm state .chr_debfin withdrawn
}
###########################################################################

###########################################################################
proc ::close_chr_clim {} {
     wm state .chr_clim withdrawn
}
###########################################################################
	       
					
###########################################################################
proc ::close_confirm {} {
	        wm state .confirm withdrawn
	}
###########################################################################

###########################################################################
proc ::close_chr_exec {} {
	        wm state .chr_exec  withdrawn
}
###########################################################################

###########################################################################
proc ::close_chr_meteo {} {
	wm state .chr_meteo  withdrawn
}
###########################################################################

###########################################################################
proc ::close_chr_meteo_ind {} {
	        wm state .chr_meteo_ind  withdrawn
	}
###########################################################################
	
###########################################################################
proc ::close_chr_bmi {} {
	        wm state .chr_bmi  withdrawn
}
###########################################################################

###########################################################################
proc ::close_chr_src {} {
	                wm state .chr_src  withdrawn
}
###########################################################################

###########################################################################
proc ::close_chr_freem {} {
                       wm state .chr_freem  withdrawn
}
###########################################################################

###########################################################################
proc ::close_chr_rdata {} {
	                       wm state .chr_rdata  withdrawn
}
###########################################################################

		       
##########################################################################
proc ::close_freem_debfin {} {
                       wm state .freem_debfin  withdrawn
	       }
###########################################################################

##########################################################################
proc ::close_rdata_debfin {} {
		wm state .freem_debfin  withdrawn
	}
##########################################################################
	
##########################################################################
proc ::close_rdata_rec {} {
		wm state .freem_rdata_rec  withdrawn
	}
##########################################################################

###########################################################################
proc ::open_any_tl {a_tl} {
	wm state $a_tl normal
}
###########################################################################
	

###########################################################################
#Procedure de selection du fichier initial de freem
proc ::proc_freem_ff {cas} {
	switch $cas {
		0
			{ 	.chr_freem.prfich.n.l_name configure -state disabled
				.chr_freem.prfich.n.e_name configure -state disabled
				.chr_freem.prfich.n.b_name configure -state disabled
			}
		default {
				.chr_freem.prfich.n.l_name configure -state normal
                                .chr_freem.prfich.n.e_name configure -state normal
                                .chr_freem.prfich.n.b_name configure -state normal
                        }
	}
}
#Fin de la procedure de selection du fichier initial de freem
###########################################################################

###########################################################################
#Procedure de selection du fichier initial
proc ::freem_pif {argc argv} {
	global sa6_freem_if
	global selectNavig
	set sa6_freem_if $selectNavig
}
#Procedure de selection du fichier initial
###########################################################################

###########################################################################
#Procedure  de selection des fichiers d'assimilation

proc ::lassim {arc argv} {

	global selectNavig

	#verifie qu'un seul fichier a ete selecitonnne
	if { [llength $selectNavig] != 1 } then {
		wm state .erreur normal
		global msgERR1
		set msgERR1 "Mauvais nombre de fichiers, recommencez"
	} else {
		global recurs_press
		set base_name "sa6_rdata_r_f_"
		set valentree [append base_name $recurs_press]
		global $valentree
		set $valentree $selectNavig

		#ajout de la date si disponible
		set rdata_tmp1 [open $selectNavig {RDONLY}]
		gets $rdata_tmp1
		gets $rdata_tmp1
		set rdata_tmp1_date [concat [gets $rdata_tmp1]]
	
		if { [lindex $rdata_tmp1_date 1] == "date" } then {
			set p_hh ""
			set p_hh [append p_hh  sa6_rdata_r_HH_ $recurs_press]
			global $p_hh
			set $p_hh [lindex $rdata_tmp1_date 2]
			set p_mi ""
			set p_mi [append p_mi  sa6_rdata_r_MI_ $recurs_press]
			global $p_mi
			set $p_mi [lindex $rdata_tmp1_date 3]
			set p_ss ""
			set p_ss [append p_ss  sa6_rdata_r_SS_ $recurs_press]
			global $p_ss
			set $p_ss [lindex $rdata_tmp1_date 4]
			set p_jj ""
                        set p_jj [append p_jj  sa6_rdata_r_JJ_ $recurs_press]
                        global $p_jj
                        set $p_jj [lindex $rdata_tmp1_date 5]
			set p_mo ""
                        set p_mo [append p_mo  sa6_rdata_r_MO_ $recurs_press]
                        global $p_mo
                        set $p_mo [lindex $rdata_tmp1_date 6]
			set p_aaaa ""
                        set p_aaaa [append p_aaaa  sa6_rdata_r_AAAA_ $recurs_press]
                        global $p_aaaa
                        set $p_aaaa [lindex $rdata_tmp1_date 7]
		}
	}
			
}
#
###########################################################################

###########################################################################
proc ::save {} {
	global sa6_sess_typ
	global sa6_etape
	global sa6_dir_trav 
	global sa6_gem_conv 
	global sa6_dat_meteo 
	global sa6_cr_ind_wsp 
	global sa6_cr_ind_tt 
	global sa6_cr_ind_rhu 
	global sa6_cr_ind_pr
	global sa6_bmi_fich
	global sa6_dom_pix
        global sa6_dom_nlat
        global sa6_dom_nlong
        global sa6_dom_lat0
        global sa6_dom_long0
        global sa6_start
        global sa6_end
	global sa6_clim_new
	global sa6_clim_get
	global sa6_clim_list
	global sa6_clim_sais
	global sa6_clim_wl
	global sa6_src_no
	global sa6_src_get
	global sa6_src_list
	global sa6_src_ssalt
	global sa6_freem_start
	global sa6_freem_end
	global sa6_freem_step
	global sa6_freem_buff
	global sa6_freem_smooth
	global sa6_freem_nassim
	global sa6_freem_lassim
	global sa6_freem_wl
	global sa6_freem_src
	global sa6_freem_wet
	global sa6_freem_fn
	global sa6_freem_dat
	global sa6_freem_ityp
	global sa6_freem_if
	global sa6_freem_mod
	global sa6_exec_chra6
	global sa6_exec_fin
	
	set filesess [file join $sa6_dir_trav "session.pa6" ]
	set sessfid [open  $filesess {RDWR CREAT TRUNC}]

	puts $sessfid "sa6_sess_typ"
	puts $sessfid $sa6_sess_typ
	puts $sessfid "sa6_etape"
	puts $sessfid $sa6_etape
	puts $sessfid "sa6_dir_trav"
	puts $sessfid $sa6_dir_trav
	puts $sessfid "sa6_gem_conv" 
	puts $sessfid $sa6_gem_conv	
	puts $sessfid "sa6_dat_meteo"
	puts $sessfid $sa6_dat_meteo
	puts $sessfid "sa6_cr_ind_wsp"
	puts $sessfid $sa6_cr_ind_wsp
	puts $sessfid "sa6_cr_ind_tt"
	puts $sessfid $sa6_cr_ind_tt
	puts $sessfid "sa6_cr_ind_rhu"
	puts $sessfid $sa6_cr_ind_rhu
	puts $sessfid "sa6_cr_ind_pr"
	puts $sessfid $sa6_cr_ind_pr
	puts $sessfid "sa6_bmi_fich"
	puts $sessfid $sa6_bmi_fich
	puts $sessfid "sa6_dom_pix"
	puts $sessfid $sa6_dom_pix
	puts $sessfid "sa6_dom_nlat"
	puts $sessfid $sa6_dom_nlat
	puts $sessfid "sa6_dom_nlong"
	puts $sessfid $sa6_dom_nlong
	puts $sessfid "sa6_dom_lat0" 
	puts $sessfid $sa6_dom_lat0
	puts $sessfid "sa6_dom_long0" 
	puts $sessfid $sa6_dom_long0
	puts $sessfid "sa6_start" 
	puts $sessfid $sa6_start
	puts $sessfid "sa6_end" 
	puts $sessfid $sa6_end
        puts $sessfid "sa6_clim_new"
	puts $sessfid $sa6_clim_new
        puts $sessfid "sa6_clim_get"
        puts $sessfid $sa6_clim_get
        puts $sessfid "sa6_clim_list"
        puts $sessfid $sa6_clim_list
        puts $sessfid "sa6_clim_sais"
        puts $sessfid $sa6_clim_sais
        puts $sessfid "sa6_clim_wl"
        puts $sessfid $sa6_clim_wl
        puts $sessfid "sa6_src_no"
        puts $sessfid $sa6_src_no
        puts $sessfid "sa6_src_get"
        puts $sessfid $sa6_src_get
        puts $sessfid "sa6_src_list"
        puts $sessfid $sa6_src_list
	puts $sessfid "sa6_src_ssalt"
	puts $sessfid $sa6_src_ssalt
	puts $sessfid "sa6_freem_start"
	puts $sessfid $sa6_freem_start
	puts $sessfid "sa6_freem_end"
	puts $sessfid $sa6_freem_end
	puts $sessfid "sa6_freem_step"
	puts $sessfid $sa6_freem_step
	puts $sessfid "sa6_freem_buf"
	puts $sessfid $sa6_freem_buff
	puts $sessfid "sa6_freem_smooth"
	puts $sessfid $sa6_freem_smooth
	puts $sessfid "sa6_freem_nassim"
        puts $sessfid $sa6_freem_nassim
	puts $sessfid "sa6_freem_lassim"
        puts $sessfid $sa6_freem_lassim
	puts $sessfid "sa6_freem_wl"
	puts $sessfid $sa6_freem_wl
	puts $sessfid "sa6_freem_src"
	puts $sessfid $sa6_freem_src
	puts $sessfid "sa6_freem_wet"
	puts $sessfid $sa6_freem_wet
	puts $sessfid "sa6_freem_fn"
	puts $sessfid $sa6_freem_fn
	puts $sessfid "sa6_freem_dat"
	puts $sessfid $sa6_freem_dat
	puts $sessfid "sa6_freem_ityp"
	puts $sessfid $sa6_freem_ityp
	puts $sessfid "sa6_freem_if"
	puts $sessfid $sa6_freem_if
	puts $sessfid "sa6_freem_mod"
	puts $sessfid $sa6_freem_mod
	puts $sessfid "sa6_exec_chra6"
	puts $sessfid $sa6_exec_chra6
	puts $sessfid "sa6_exec_fin"
	puts $sessfid $sa6_exec_fin

	close $sessfid
}
	
#Fin de la proc save
###########################################################################

###########################################################################
#Proc glob_open
proc ::glob_open {} {
	global navig_msg1
	global proc_fin_navig
	        set navig_msg1 "Selectionnez le fichier contenant les informations sur la session a ouvrir (Fichier session.pa6)"
	        wm state .tl_navig normal
	        set proc_fin_navig open_sess
}
			

#Proc glob_open
###########################################################################

###########################################################################
#Proc glob_canc
proc ::glob_canc {to_close} {
	
	global msgCONF1
	global si_confirm_oui
	global si_confirm_non
	
	set msgCONF1 "Etes-vous certain de vouloir annuler ?"
	set si_confirm_oui "$to_close"
	set si_confirm_non {close_confirm}
	wm state .confirm normal
}

#Proc glob_canc
###########################################################################

###########################################################################
#Proc nav_refresh
proc ::nav_refresh {} {

	global rtc
	global laListeFich
	global laListeDir
	
	set rtc [pwd]
	#####Affichage des fichiers
	.tl_navig.fr_listbox.file delete 0 end
	set listTmp [glob -nocomplain -type {f } -directory $rtc *]
	set listTmp2 [glob -nocomplain -type {f hidden} -directory $rtc *]
	set listTmp3 [glob -nocomplain -type {l r} -directory $rtc *]
	foreach i $listTmp {
	        set ajout [file tail $i]
       		 lappend laListeFich $ajout
	}
	foreach i $listTmp2 {
	        set ajout [file tail $i]
	        lappend laListeFich $ajout
	}
	foreach i $listTmp3 {
		if {[file isfile $i] == "1"} then {
                	set ajout [file tail $i]
                	lappend laListeFich $ajout
		}
        }
	set laListeFich [lsort -increasing -dictionary -unique $laListeFich]

	#####Affichage des repertoire
	.tl_navig.fr_listbox.dir delete 0 end
	set listTmp [glob -nocomplain -type {d hidden } -directory $rtc *]
	set listTmp2 [glob -nocomplain -type {d} -directory $rtc *]
	set listTmp3 [glob -nocomplain -type {l r} -directory $rtc *]
	foreach i $listTmp {
	        set ajout [lindex [file split $i] end]
	        lappend laListeDir $ajout
	}
	foreach i $listTmp2 {
	        set ajout [lindex [file split $i] end]
	        lappend laListeDir $ajout
	}

	foreach i $listTmp3 {
		if { [file isdirectory $i] == "1" } then {
                	set ajout [lindex [file split $i] end]
                	lappend laListeDir $ajout
		}
         }
	set laListeDir [lsort -increasing -dictionary -unique $laListeDir]
}
#Fin de proc nav_refresh
###########################################################################

###########################################################################
#Proc pls_help
proc ::pls_help {about} {
	exec mozilla $about
}
#Fin de proc pls_help
###########################################################################

###########################################################################
#Proc open_sess
proc ::open_sess {argc argv} {
	global sa6_sess_typ
        global sa6_etape
        global sa6_dir_trav
        global sa6_gem_conv
        global sa6_dat_meteo
        global sa6_cr_ind_wsp
        global sa6_cr_ind_tt
        global sa6_cr_ind_rhu
        global sa6_cr_ind_pr
	global sa6_bmi_fich
	global sa6_dom_pix
        global sa6_dom_nlat
        global sa6_dom_nlong
        global sa6_dom_lat0
        global sa6_dom_long0
        global sa6_start
        global sa6_end
	global sa6_clim_new
        global sa6_clim_get
        global sa6_clim_list
        global sa6_clim_sais
        global sa6_clim_wl
        global sa6_src_no
        global sa6_src_get
        global sa6_src_list
	global sa6_src_ssalt
	global sa6_freem_start
        global sa6_freem_end
        global sa6_freem_step
        global sa6_freem_buff
        global sa6_freem_smooth
	global sa6_freem_nassim
	global sa6_freem_lassim
        global sa6_freem_wl
        global sa6_freem_src
        global sa6_freem_wet
	global sa6_freem_fn
        global sa6_freem_dat
        global sa6_freem_ityp
        global sa6_freem_if
	global sa6_freem_mod
	global sa6_exec_chra6
	global sa6_exec_fin

	set f_open [open [lindex $argv 0] {RDONLY} ]
	gets $f_open
	set sa6_sess_type [gets $f_open]
	gets $f_open
	set sa6_etape [gets $f_open]
	gets $f_open
	set sa6_dir_trav [gets $f_open]
	gets $f_open
	set sa6_gem_conv [gets $f_open]
	gets $f_open
	set sa6_dat_meteo [gets $f_open]
	gets $f_open
	set sa6_cr_ind_wsp [gets $f_open]
	gets $f_open
	set sa6_cr_ind_tt [gets $f_open]
	gets $f_open
	set sa6_cr_ind_rhu [gets $f_open]
	gets $f_open
	set sa6_cr_ind_pr [gets $f_open]
	gets $f_open
	set sa6_bmi_fich [gets $f_open]
	gets $f_open
	set sa6_dom_pix [gets $f_open]
	gets $f_open
	set sa6_dom_nlat [gets $f_open]
        gets $f_open
	set sa6_dom_nlong [gets $f_open]
        gets $f_open
	set sa6_dom_lat0 [gets $f_open]
	gets $f_open
	set sa6_dom_long0 [gets $f_open]
	gets $f_open
	set sa6_start [gets $f_open]
	gets $f_open
	set sa6_end [gets $f_open]
	gets $f_open
	set sa6_clim_new [gets $f_open]
	gets $f_open
	set sa6_clim_get [gets $f_open]
	gets $f_open
	set sa6_clim_list [gets $f_open]
	gets $f_open
	set sa6_clim_sais [gets $f_open]
	gets $f_open
	set sa6_clim_wl [gets $f_open]
	gets $f_open
	set sa6_src_no [gets $f_open]
	gets $f_open
	set sa6_src_get [gets $f_open]
	gets $f_open
	set sa6_src_list [gets $f_open]
	gets $f_open
	set sa6_src_ssalt [gets $f_open]
	gets $f_open
	set sa6_freem_start [gets $f_open]
	gets $f_open
	set sa6_freem_end [gets $f_open]
	gets $f_open
	set sa6_freem_step [gets $f_open]
	gets $f_open
	set sa6_freem_buff [gets $f_open]
	gets $f_open
	set sa6_freem_smooth [gets $f_open]
	gets $f_open
	set sa6_freem_nassim [gets $f_open]
	gets $f_open
	set sa6_freem_lassim [concat [gets $f_open]]
        gets $f_open
	set sa6_freem_wl [concat [gets $f_open]]
	gets $f_open
	set sa6_freem_src [gets $f_open]
	gets $f_open
	set sa6_freem_wet [gets $f_open]
	gets $f_open
	set sa6_freem_fn [gets $f_open]
	gets $f_open
	set sa6_freem_dat [gets $f_open]
	gets $f_open
	set sa6_freem_ityp [gets $f_open]
	gets $f_open
	set sa6_freem_if [gets $f_open]
	gets $f_open
	set sa6_freem_mod [gets $f_open]
	gets $f_open
	set sa6_exec_chra6 [gets $f_open]
	gets $f_open
	set sa6_exec_fin [gets $f_open]

	######################
	#Variables temporaires reconstituees du fichier de sauvegarde
	######################

	#Reconstitution des longueurs d'onde
		for {set iter 1} {$iter <=14} {set iter [expr $iter +1]} {
			global sa6_freem_wl_$iter
			set sa6_freem_wl_$iter [lindex $sa6_freem_wl [expr $iter -1 ]]
		}

	#Reconstitution des informations sur les pgm de rdatas
	
	#####Ouverture de la fenetre appropriee
	if {$sa6_exec_chra6 != 1} then {
		if {$sa6_etape != "NULL"} then {
			wm state $sa6_etape normal
		} else {
			set msgERR1 {
			Impossible de retracer votre position dans le processus
			}
		}
	}
}
	
#Fin de la proc open_sess
###########################################################################
