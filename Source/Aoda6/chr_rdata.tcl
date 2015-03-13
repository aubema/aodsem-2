#Ce script contient le code du toplevel .rdata_rec	

##########################################################################
	#conversion
	set sa6_rdata_nassim $sa6_freem_nassim

	#construction du toplevel rdata_recurs
	toplevel .rdata_rec
	wm title .rdata_rec "Recursivite" 
	wm state .rdata_rec normal

	#Commandes pour faire l'association fichier - numros d'assimilation
	 bind .rdata_rec <ButtonPress> {set t_recurs_press %W}
		
	frame .rdata_rec.rec
	frame .rdata_rec.but
	frame .rdata_rec.tempo

	grid .rdata_rec.rec
	grid .rdata_rec.but
	grid .rdata_rec.tempo
	label .rdata_rec.tempo.msg -textvar msgtmp
	grid .rdata_rec.tempo.msg

	label .rdata_rec.rec.l_rec -text  "Recursion"
	label .rdata_rec.rec.l_fich -text "Fichier"
	label .rdata_rec.rec.l_HH -text "Heure"
	label .rdata_rec.rec.l_MI -text "Minute"
	label .rdata_rec.rec.l_SS -text "Seconde"
	label .rdata_rec.rec.l_JJ -text "Jour"
	label .rdata_rec.rec.l_MO -text "Mois"
	label .rdata_rec.rec.l_AAAA -text "Annee"
	
	grid .rdata_rec.rec.l_rec -row 0 -column 0
	grid .rdata_rec.rec.l_fich -row 0 -column 1 -columnspan 2
	grid .rdata_rec.rec.l_HH -row 0 -column 3
	grid .rdata_rec.rec.l_MI -row 0 -column 4
	grid .rdata_rec.rec.l_SS -row 0 -column 5
	grid .rdata_rec.rec.l_JJ -row 0 -column 6
	grid .rdata_rec.rec.l_MO -row 0 -column 7
	grid .rdata_rec.rec.l_AAAA -row 0 -column 8

	#Initialisation  des donnees 
	#for {set recurs 1} {$recurs <= 100} {set recurs [expr $recurs + 1]} {
	#	if {$recurs <= $sa6_rdata_nassim} then {
	#		 set sa6_rdata_r
	# ...
	#}
	#}
	
	#Creation de la liste des cases
	for {set recurs 1} {$recurs <= $sa6_rdata_nassim} {set recurs [expr $recurs + 1]} {
		set name_l_r ".rdata_rec.rec.l_r"
		set name_e_f ".rdata_rec.rec.e_f"
		set name_b_f ".rdata_rec.rec.b_f"
		set name_e_HH ".rdata_rec.rec.e_HH"
		set name_e_MI ".rdata_rec.rec.e_MI"
		set name_e_SS ".rdata_rec.rec.e_SS"
		set name_e_JJ ".rdata_rec.rec.e_JJ"
		set name_e_MO ".rdata_rec.rec.e_MO"
		set name_e_AAAA ".rdata_rec.rec.e_AAAA"
	
		append name_l_r $recurs
		append name_e_f $recurs
		append name_b_f $recurs
		append name_e_HH $recurs
		append name_e_MI $recurs
		append name_e_SS $recurs
		append name_e_JJ $recurs
		append name_e_MO $recurs
		append name_e_AAAA $recurs
		
		set base_n_rdata "sa6_rdata_r"
		entry $name_e_f -textvar [append base_n_rdata "_f_" $recurs] -width 60
		set base_n_rdata "sa6_rdata_r"
		entry $name_e_HH -textvar [append base_n_rdata "_HH_" $recurs] -width 2
		set base_n_rdata "sa6_rdata_r"
		entry $name_e_MI -textvar [append base_n_rdata "_MI_" $recurs] -width 2
		set base_n_rdata "sa6_rdata_r"
		entry $name_e_SS -textvar [append base_n_rdata "_SS_" $recurs] -width 2
		set base_n_rdata "sa6_rdata_r"
		entry $name_e_JJ -textvar [append base_n_rdata "_JJ_" $recurs] -width 2
		set base_n_rdata "sa6_rdata_r"
		entry $name_e_MO -textvar [append base_n_rdata "_MO_" $recurs] -width 2
		set base_n_rdata "sa6_rdata_r"
		entry $name_e_AAAA -textvar [append base_n_rdata "_AAAA_" $recurs] -width 4
		
		button $name_b_f -text "..." -command { 
	                wm state .tl_navig normal
			set recurs_press [ lindex [split $t_recurs_press {}] end]
			set laSelect {} 
			set navig_msg1 "Selectionnez le fichier d'assimilation "
			set proc_fin_navig "lassim" 
			
	        }
													
		label $name_l_r -text $recurs 

		grid $name_l_r -row $recurs -column 0
		grid $name_e_f -row $recurs -column 1
		grid $name_e_HH -row $recurs -column 3
		grid $name_e_MI -row $recurs -column 4
		grid $name_e_SS -row $recurs -column 5
		grid $name_e_JJ -row $recurs -column 6
		grid $name_e_MO -row $recurs -column 7
		grid $name_e_AAAA -row $recurs -column 8
		grid $name_b_f -row $recurs -column 2
	} 
	#Fermeture de for {set recurs 1} {$recurs <= $sa6_rdata_nassim} {set recurs [expr $recurs + 1]}


	###Les boutons de .rdata_rec

	button .rdata_rec.but.b_ok -text "Continuer" -command {

		###Ecrire de la variable contenant  les recursions

		#Initialisation de  la liste
		set sa6_freem_lassim ""

		#Remplissage des champs vide


		for {set recursi 1} {$recursi <= $sa6_rdata_nassim} {set recursi [expr $recursi + 1]} {
			set base_n_rdata "sa6_rdata_r"
			set rdat_f [append base_n_rdata "_f_" $recursi]
			set drdat_f {$rdat_f}
			set base_n_rdata "sa6_rdata_r"
			set rdat_hh [append base_n_rdata "_HH_" $recursi]
			set drdat_hh {$rdat_hh}
			set base_n_rdata "sa6_rdata_r"
			set rdat_mi [append base_n_rdata "_MI_" $recursi]
			set drdat_mi {$rdat_mi}
			set base_n_rdata "sa6_rdata_r"
			set rdat_ss [append base_n_rdata "_SS_" $recursi]
			set drdat_ss {$rdat_ss}
			set base_n_rdata "sa6_rdata_r"
			set rdat_jj [append base_n_rdata "_JJ_" $recursi]
			set drdat_jj {$rdat_jj}
			set base_n_rdata "sa6_rdata_r"
			set rdat_mo [append base_n_rdata "_MO_" $recursi]
			set drdat_mo {$rdat_mo}
			set base_n_rdata "sa6_rdata_r"
			set  rdat_aaaa [append base_n_rdata "_AAAA_" $recursi]
			set drdat_aaaa {$rdat_aaaa}
			eval "lappend sa6_freem_lassim_tmp $drdat_f  $drdat_hh $drdat_mi $drdat_ss $drdat_jj"
			eval "lappend sa6_freem_lassim_tmp $drdat_mo $drdat_aaaa"
		}
#fermeture de for {set recursi 1} {$recursi <= $sa6_rdata_nassim} {set recursi [expr $recursi + 1]}
		
		foreach nom $sa6_freem_lassim_tmp {
			set temp [set tempo {$}][set tempor $nom]
			set temp [eval "set tempo $temp"]
			lappend sa6_freem_lassim $temp
		}
		
		
	
		wm state .rdata_rec withdrawn
		wm state .chr_exec normal
               	set sa6_etape .chr_exec
	} 
	#fermture  de button  .rdat_rec.but.b_ok ...

	button .rdata_rec.but.b_canc -text "Annuler" -command {
                    glob_canc "close_rdata_rec"
        }
	button .rdata_rec.b_help -image aide-logo -command {
                    pls_help "http://www.w3.org/"
              }

	grid .rdata_rec.but.b_ok  -row 0 -column 0 -pady 20 -padx 30
	grid .rdata_rec.but.b_canc -row 0 -column 1 -pady 20 -padx 30
	grid .rdata_rec.b_help -sticky {s e} -padx 2 -pady 3
