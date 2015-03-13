##########################################################################
#Toplevel chr_acc
toplevel .chr_acc 
wm title .chr_acc "Assistant chronologique"
wm state .chr_acc withdrawn

##################################################
#Initialisation
set longNameNFolder "NULL"
#Fin de l'initialisation
##################################################
	
label .chr_acc.lab_welc -text "Bienvenue dans l'assistant chronologique d'AODA6"
message .chr_acc.msg_intr -justify left -width 400 -text {
L'assitant chronologique vous permettra de naviguer a travers toutes les fonctionnalite d'AODSEM. Pour commencer, vous devez creer un nouveau repertoire qui contiendra tous les fichiers qui seront crees.
}
message .chr_acc.msg_save -justify left -width 400 -text {
A tout moment, apres avoir cree votre repertoire de travail, vous pourrez enregistrer votre session en pressant <Ctrl-S> ou en vous rendant dans le menu Fichier puis Sauvegarder.
}
button .chr_acc.b_nfold -text "Creer un nouveau dossier" -command {
	wm state .tl_navig normal
	set laSelect {}
	set proc_fin_navig "vide"
	.chr_acc.b_cont configure -state normal
	set navig_msg1 "Pour creer un nouveau dossier, cliquez sur l'icone representant un dossier avec une etoile. Une fois le dossier cree, cliquez sur OK"
}
button .chr_acc.b_cont -text "Continuer" -state disabled -command {
	if {$longNameNFolder == "NULL"} then {
		wm state .erreur normal
		set msgERR1 "Vous devez tout d'abord creer un nouveau dossier"
	} else {
			set sa6_dir_trav $longNameNFolder
			set sa6_etape ".chr_meteo"
			wm state .chr_acc withdrawn
			 wm state .chr_meteo normal
		}
	}
	button .chr_acc.b_canc -text "Annuler" -command {
		glob_canc "close_chr_acc"
		#set msgCONF1 "Etes-vous certain de vouloir annuler ?"
		#set si_confirm_oui {close_chr_acc}
		#set si_confirm_non {close_confirm}
		#wm state .confirm normal
	}
	button .chr_acc.b_help -image aide-logo -command {pls_help "www.kernel.org"}

	grid .chr_acc.lab_welc -row 0 -column 0 -columnspan 3
	grid .chr_acc.msg_intr -row 1 -column 0 -columnspan 3
	grid .chr_acc.msg_save -row 2 -column 0 -columnspan 3
	grid .chr_acc.b_nfold -row 3 -column 0
	grid .chr_acc.b_cont -row 3 -column 1
	grid .chr_acc.b_canc -row 3 -column 2
	grid .chr_acc.b_help -columnspan 3 -sticky {s e} -padx 3 -pady 3
	#Fin du toplevel chr_acc
	##########################################################################

	##########################################################################
	#Toplevel chr_meteo
	toplevel .chr_meteo
	wm title .chr_meteo "Donnees meteo"
	wm state .chr_meteo withdrawn

	frame .chr_meteo.but

	message .chr_meteo.info_gem -width 400 -text {
	AODSEM utilise frequemment des donnees meteo. Il est necessaire de traiter ces donnees avant de pouvoir les utiliser. Cette section vous permet de traiter des donnees meteo brutes. 
	Si vos donnees meteo ne se terminent pas par les extensions .tt, .PR, .rhu, .wsp , vous devez les traiter.
	}
	message .chr_meteo.info_next -width 400 -text {
	Si vos donnees meteo sont deja converties, cliquez sur le bouton "Donnees meteo converties" pour selectionner vos donnees.
	}
	message .chr_meteo.info_a_conv -width 400 -text {
	Si vous devez convertir vos donnees, choissez tout d'abord le type de donnees a convertir entre GEMv1 et GEMv2. Puis, cliquez sur le bouton "Donnees a convertir" pour selectionner les donnees.
	}

	radiobutton .chr_meteo.gemv1 -text "GEMv1" -variable which_gem -value 1 -state disabled
	radiobutton .chr_meteo.gemv2 -text "GEMv2" -variable which_gem -value 2

	button .chr_meteo.but.b_aodsem -text "Donnees converties" -command {
		wm state .tl_navig normal
		set laSelect {}
		set navig_msg1 "Selectionner tous les fichiers meteo que vous souhaitez utiliser"
		.chr_meteo.but.b_ok configure -state active
		set sa6_gem_conv 0
	}
		
	button .chr_meteo.but.b_a_conv -text "Donnees a convertir" -command {
		set navig_msg1 "Selectionner tous les fihciers meteo que vous souhaitez convertir"
		set laSelect {}
		wm state .tl_navig normal
		.chr_meteo.but.b_ok configure -state active
		set sa6_gem_conv 1

	}
	button .chr_meteo.but.b_ok -text "Continuer" -state disabled -command {
		if {$sa6_gem_conv == 1} {
			if { $which_gem == 1 || $which_gem == 2 } then {
			set sa6_dat_meteo $selectNavig
			set sa6_which_gem $which_gem
			wm state .chr_meteo withdrawn
			wm state .chr_meteo_ind normal
			set sa6_etape ".chr_meteo_ind"
			} else {
				set msgERR1 "Selectionnez le type de donnees GEM"
				wm state .erreur normal
			}
		} else {
			set sa6_dat_meteo $selectNavig
			wm state .chr_meteo withdrawn
			wm state .chr_meteo_ind normal
			set sa6_etape ".chr_meteo_ind"
		}
	}

	button .chr_meteo.but.b_canc -text "Annuler" -command {
		glob_canc "close_chr_meteo"
	}
	button .chr_meteo.b_help -image aide-logo -command {pls_help "www.thinkgeek.com"}
					
	grid .chr_meteo.info_gem -pady 0
	grid .chr_meteo.info_next -pady 0
	grid .chr_meteo.info_a_conv -pady 0
	grid .chr_meteo.gemv1 -pady 2
	grid .chr_meteo.gemv2 -pady 2
	grid .chr_meteo.but -pady 5
	grid .chr_meteo.but.b_aodsem -column 0 -row 0 -padx 2
	grid .chr_meteo.but.b_a_conv -column 1 -row 0 -padx 2
	grid .chr_meteo.but.b_ok -column 2 -row 0 -padx 2
	grid .chr_meteo.but.b_canc -column 3 -row 0 -padx 2
	grid .chr_meteo.b_help -columnspan 4 -sticky {s e} -padx 3 -pady 3
	#Fin du toplevel chr_meteo
	###########################################################################

	###########################################################################
	#Toplevel chr_meteo_ind
	toplevel .chr_meteo_ind
	wm title .chr_meteo_ind "Index des fichiers meteo"
	wm state .chr_meteo_ind withdrawn
	frame .chr_meteo_ind.but

	message .chr_meteo_ind.msg_index -width 400 -text {
	AODSEM a besoin d'index contenant les noms de tous les fichiers meteos pouvant etre utilises. 
	Il est conseille de creer tous les index.
	}
	checkbutton .chr_meteo_ind.ipr -text "Index des precipitations" -variable sa6_cr_ind_pr
	checkbutton .chr_meteo_ind.itt -text "Index des temperature" -variable sa6_cr_ind_tt
	checkbutton .chr_meteo_ind.irhu -text "Index des humidite relatives" -variable sa6_cr_ind_rhu
	checkbutton .chr_meteo_ind.iwsp -text "Index des vitesses du vent" -variable sa6_cr_ind_wsp

	button .chr_meteo_ind.but.b_ok -text "Creer les index" -command {
		wm state .chr_meteo_ind withdrawn
		wm state .chr_bmi normal
		set sa6_etape ".chr_bmi"
	}
	button .chr_meteo_ind.but.b_canc -text "Annuler" -command {
		glob_canc "close_chr_meteo_ind"
	}
	button .chr_meteo_ind.b_help -image aide-logo -command {pls_help "www.granddictionnaire.com"}

	grid .chr_meteo_ind.msg_index
	grid .chr_meteo_ind.irhu
	grid .chr_meteo_ind.ipr
	grid .chr_meteo_ind.itt
	grid .chr_meteo_ind.iwsp
	grid .chr_meteo_ind.but -pady 10
	grid .chr_meteo_ind.but.b_ok -row 0 -column 0 -padx 2
	grid .chr_meteo_ind.but.b_canc -row 0 -column 1 -padx 2
	grid .chr_meteo_ind.b_help -columnspan 2 -sticky {s e} -padx 3 -pady 3

	###########################################################
	#Bindings chr_meteo_ind
	bind .chr_meteo_ind <FocusIn> {
		.chr_meteo_ind.ipr select
		.chr_meteo_ind.itt select
		.chr_meteo_ind.iwsp select
		.chr_meteo_ind.irhu select
	}

	#Fin des bindings chr_meteo_ind
	###########################################################

	#Fin du toplevel chr_meteo_ind
	###########################################################################

	###########################################################################
	#Toplevel chr_bmi
	toplevel .chr_bmi
	wm title .chr_bmi "Calcul de Mie"
	wm state .chr_bmi withdrawn

	frame .chr_bmi.but
	message .chr_bmi.info -width 400 -text {
	Il faut maintenant creer les fichiers qui serviront au calcul de Mie. Il est possible que ces fichiers soient deja disponibles. Selectionnez les fichiers *.bmi que vous desirez utiliser.
	}
	button .chr_bmi.but.b_sel -text "Selectionner les *.bmi" -command {
		  wm state .tl_navig normal
		  set laSelect {}
		  set proc_fin_navig "vide"
		  set navig_msg1 "Selectionner les fichiers *.bmi que vous souhaitez utiliser"
		  .chr_bmi.but.b_ok configure -state active
	}
				  
	button .chr_bmi.but.b_ok -text "Continuer" -state disabled -command {
		set sa6_bmi_fich $selectNavig
		wm state .chr_bmi withdrawn 
		wm state .chr_dom normal 
		set sa6_etape ".chr_dom"
	}
	button .chr_bmi.but.b_canc -text "Annuler" -command {
		glob_canc "close_chr_bmi"
	}

	button .chr_bmi.b_help -image aide-logo -command {
		pls_help "www.sourceforge.net"
	}

	grid .chr_bmi.info
	grid .chr_bmi.but
	grid .chr_bmi.but.b_sel -row 0 -column 0 -padx 2 -pady 3
	grid .chr_bmi.but.b_ok -row 0 -column 1 -padx 2 -pady 3
	grid .chr_bmi.but.b_canc -row 0 -column 2 -padx 2 -pady 3
	grid .chr_bmi.b_help -padx 3 -pady 3 -sticky {s e}
	#Fin du toplevel chr_bmi
	###########################################################################

	###########################################################################
	#Toplevel chr_dom
	toplevel .chr_dom
	wm title .chr_dom "Domaine de la modelisation"
	wm state .chr_dom withdrawn


	frame .chr_dom.fr_pix
	frame .chr_dom.but

	canvas .chr_dom.ima -width 325
	.chr_dom.ima create image 0 0 -image domaine -anchor nw 
	message .chr_dom.info -width 500 -text {
	Le domaine d'AODSEM est la region geographique pour laquelle sera fait la modelisation. Elle est definie par 5 parametres:  (Voir le schema plus haut)
	1. La taille d'un pixel (en 100e de degre)
	2. Le nombre de pixels en latitude
	3. Le nombre de pixels en longitude
	4. La latitude du centre du pixel sud-ouest (en degres)
	5. La longitude du centre du pixel sud-ouest (en degres)
	}
	label .chr_dom.fr_pix.l_size -text "Taille d'un pixel (100e de degres)"
	entry .chr_dom.fr_pix.e_size -width 7 -text {}


	label .chr_dom.fr_pix.l_nlat -text "Nombre de pixels en latitude"
	entry .chr_dom.fr_pix.e_nlat -textvar dom_nlat -width 7 -text {} 
	label .chr_dom.fr_pix.l_nlong -text "Nombre de pixels en longitude"
	entry .chr_dom.fr_pix.e_nlong -textvar dom_nlong -width 7  -text {}

	label .chr_dom.fr_pix.l_lat0 -text "Latitude du pixel sud-ouest (degres)"
	entry .chr_dom.fr_pix.e_lat0 -textvariable dom_lat0 -width 7  -text {}
	label .chr_dom.fr_pix.l_long0 -text "Longitude du pixel sud-ouest (degres)"
	entry .chr_dom.fr_pix.e_long0 -textvar dom_long0 -width 7  -text {}
	label .chr_dom.bid -textvar dom_pix
	button .chr_dom.but.b_ok -text "Continuer" -command {
		set sa6_dom_pix [.chr_dom.fr_pix.e_size get]
		set sa6_dom_nlat [.chr_dom.fr_pix.e_nlat get]
		set sa6_dom_nlong [.chr_dom.fr_pix.e_nlong get]
		set sa6_dom_lat0 [.chr_dom.fr_pix.e_lat0 get]
		set sa6_dom_long0 [.chr_dom.fr_pix.e_long0 get]
		if {$sa6_dom_pix>0 && $sa6_dom_pix<36000 } then {set pixOK 1} else {set pixOK 0}
		if {$sa6_dom_nlat>0 && $sa6_dom_nlat<50000} then {set nlatOK 1} else {set nlatOK 0}
		if {$sa6_dom_nlong>0 && $sa6_dom_nlong<50000} then {set nlongOK 1} else {set nlongOK 0}
		if { $sa6_dom_lat0>-360 && $sa6_dom_lat0 <360} then {set lat0OK 1} else {set lat0OK 0}
		if { $sa6_dom_long0>-360 && $sa6_dom_long0<360} then {set long0OK 1} else {set long0OK 0}
		set domOK [expr $pixOK+$nlatOK+$nlongOK+$lat0OK+$long0OK]
		if {$domOK !=5 } then {
			 wm state .erreur normal
			 set msgERR1 "Domaine non valide, verifier les valeurs"
		 } else {
			 wm state .chr_dom withdrawn
			 wm state .chr_debfin normal
			 set sa6_etape ".chr_debfin"
		 }
	 }

	button .chr_dom.but.b_canc -text "Annuler" -command {
			glob_canc "close_chr_dom"
		}
		
	button .chr_dom.b_help -image aide-logo -command {
			pls_help "www.usherbrooke.ca"
		}
		
	grid .chr_dom.bid
	grid .chr_dom.ima
	grid .chr_dom.info
	grid .chr_dom.fr_pix -sticky w
	grid .chr_dom.fr_pix.l_size -row 0 -column 0 -sticky w
	grid .chr_dom.fr_pix.e_size -row 0 -column 1 -sticky w
	grid .chr_dom.fr_pix.l_nlat -row 1 -column 0 -sticky w
	grid .chr_dom.fr_pix.e_nlat -row 1 -column 1 -sticky w
	grid .chr_dom.fr_pix.l_nlong -row 1 -column 2 -sticky e
	grid .chr_dom.fr_pix.e_nlong -row 1 -column 3 -sticky e
	grid .chr_dom.fr_pix.l_lat0 -row 2 -column 0 -sticky w
	grid .chr_dom.fr_pix.e_lat0 -row 2 -column 1 -sticky w
	grid .chr_dom.fr_pix.l_long0 -row 2 -column 2 -sticky e
	grid .chr_dom.fr_pix.e_long0 -row 2 -column 3 -sticky e
	grid .chr_dom.but
	grid .chr_dom.but.b_ok -row 0 -column 0 -pady 30 -padx 20
	grid .chr_dom.but.b_canc -row 0 -column 1 -pady 30 -padx 20
	grid .chr_dom.b_help -sticky {s e} -padx 3 -pady 3
	#Fin du toplevel chr_dom
	###########################################################################

	###########################################################################
	#Toplevel chr_debfin

	toplevel .chr_debfin
	wm title .chr_debfin "Periode de la modelisation"
	wm state .chr_debfin withdrawn

	frame .chr_debfin.fr_l
	frame .chr_debfin.but

	message .chr_debfin.info -width 400 -text {
	Entrez ici la date a laquelle vous souhaitez debuter la modelisation ainsi que la date a laquelle vous desirez terminer. Ces valeurs serviront de valeurs par defaut plus tard dans l'assitant.
	}

	label .chr_debfin.fr_l.deb -text "Debut"
	label .chr_debfin.fr_l.fin -text "Fin"
	label .chr_debfin.fr_l.l_HH -text "Heure"
	label .chr_debfin.fr_l.l_MI -text "Minute"
	label .chr_debfin.fr_l.l_SS -text "Seconde"
	label .chr_debfin.fr_l.l_JJ -text "Jour"   
	label .chr_debfin.fr_l.l_MO -text "Mois"   
	label .chr_debfin.fr_l.l_AA -text "Annee"  
	entry .chr_debfin.fr_l.d_HH -width 2 -textvar dHH
	entry .chr_debfin.fr_l.d_MI -width 2 -textvar dMI
	entry .chr_debfin.fr_l.d_SS -width 2 -textvar dSS
	entry .chr_debfin.fr_l.d_JJ -width 2 -textvar dJJ
	entry .chr_debfin.fr_l.d_MO -width 2 -textvar dMO
	entry .chr_debfin.fr_l.d_AA -width 4 -textvar dAA
	entry .chr_debfin.fr_l.f_HH -width 2 -textvar fHH
	entry .chr_debfin.fr_l.f_MI -width 2 -textvar fMI
	entry .chr_debfin.fr_l.f_SS -width 2 -textvar fSS
	entry .chr_debfin.fr_l.f_JJ -width 2 -textvar fJJ
	entry .chr_debfin.fr_l.f_MO -width 2 -textvar fMO
	entry .chr_debfin.fr_l.f_AA -width 4 -textvar fAA
	button .chr_debfin.but.b_ok -text "Continuer" -command {
		if {$dHH >= 0 && $dHH < 24} then {set dHHOK 1} else {set dHHOK 0}
		if {$dMI >= 0 && $dMI < 60} then {set dMIOK 1} else {set dMIOK 0}
		if {$dSS >= 0 && $dSS < 60} then {set dSSOK 1} else {set dSSOK 0}
		if {$dJJ > 0 && $dJJ < 32} then {set dJJOK 1} else {set dJJOK 0}
		if {$dMO > 0 && $dMO < 13} then {set dMOOK 1} else {set dMOOK 0}
		if {$dAA > 0 && $dAA < 3000} then {set dAAOK 1} else {set dAAOK 0}
		if {$fHH >= 0 && $fHH < 24} then {set fHHOK 1} else {set fHHOK 0}
		if {$fMI >= 0 && $fMI < 60} then {set fMIOK 1} else {set fMIOK 0}
		if {$fSS >= 0 && $fSS < 60} then {set fSSOK 1} else {set fSSOK 0}
		if {$fJJ > 0 && $fJJ < 32} then {set fJJOK 1} else {set fJJOK 0}
		if {$fMO > 0 && $fMO < 13} then {set fMOOK 1} else {set fMOOK 0}
		if {$fAA > 0 && $fAA < 3000} then {set fAAOK 1} else {set fAAOK 0}
		set f_time [expr 31*$fMO + $fJJ + $fHH/24 + $fMI/(24*60) + $fSS/(24*60*60)]
		set d_time [expr 31*$dMO + $dJJ + $dHH/24 + $dMI/(24*60) + $dSS/(24*60*60)]
			if {$fAA == $dAA && $f_time > $d_time} then { set chrOK 1} else {set chrOK 0}
			
		set debfinOK [expr $dHHOK+$dMIOK+$dSSOK+$dJJOK+$dMOOK+$dAAOK+$fHHOK+$fMIOK+$fSSOK+ \
		$fJJOK+$fMOOK+$fAAOK+$chrOK]
		
		if {$debfinOK != 13} then {	
			wm state .erreur normal
			set msgERR1 "Date invalide"
		} else {
			wm state .chr_debfin withdrawn
			set sa6_start [list $dHH $dMI $dSS $dJJ $dMO $dAA]
			set sa6_end [list $fHH $fMI $fSS $fJJ $fMO $fAA]
			wm state .chr_clim normal
			set sa6_etape ".chr_clim"
		}
	}
	button .chr_debfin.but.b_canc -text "Annuler" -command {
				glob_canc "close_chr_debfin"
					}
	button .chr_debfin.b_help -image aide-logo -command {
		      pls_help "http://www.edulinux.org/spip/"
	}

	grid .chr_debfin.info
	grid .chr_debfin.fr_l
	grid .chr_debfin.but
	grid .chr_debfin.fr_l.deb -row 1 -column 0
	grid .chr_debfin.fr_l.fin -row 2 -column 0
	grid .chr_debfin.fr_l.l_HH -row 0 -column 1
	grid .chr_debfin.fr_l.l_MI -row 0 -column 2
	grid .chr_debfin.fr_l.l_SS -row 0 -column 3
	grid .chr_debfin.fr_l.l_JJ -row 0 -column 4
	grid .chr_debfin.fr_l.l_MO -row 0 -column 5
	grid .chr_debfin.fr_l.l_AA -row 0 -column 6
	grid .chr_debfin.fr_l.d_HH -row 1 -column 1
	grid .chr_debfin.fr_l.d_MI -row 1 -column 2
	grid .chr_debfin.fr_l.d_SS -row 1 -column 3
	grid .chr_debfin.fr_l.d_JJ -row 1 -column 4
	grid .chr_debfin.fr_l.d_MO -row 1 -column 5
	grid .chr_debfin.fr_l.d_AA -row 1 -column 6
	grid .chr_debfin.fr_l.f_HH -row 2 -column 1
	grid .chr_debfin.fr_l.f_MI -row 2 -column 2
	grid .chr_debfin.fr_l.f_SS -row 2 -column 3
	grid .chr_debfin.fr_l.f_JJ -row 2 -column 4
	grid .chr_debfin.fr_l.f_MO -row 2 -column 5
	grid .chr_debfin.fr_l.f_AA -row 2 -column 6

	grid .chr_debfin.but.b_ok -row 0 -column 0 -padx 20 -pady 30
	grid .chr_debfin.but.b_canc -row 0 -column 1 -padx 20 -pady 30
	grid .chr_debfin.b_help -sticky {s e} -padx 3 -pady 3

	grid columnconfigure .chr_debfin.fr_l {1 2 3 4 5 6} -minsize 60
	#Fin du toplevel chr_debfin
	###########################################################################

	###########################################################################
	#Toplevel chr_clim
	toplevel .chr_clim
	wm title .chr_clim "Climatologie"
	wm state .chr_clim withdrawn

	frame .chr_clim.sais
	frame .chr_clim.wlen
	frame .chr_clim.but

	message .chr_clim.info -width 400 -text {
	Il faut maintenant realise la climatologie associe au modele. Vous pouvez soit selectionner une climatologie existante ou en creer une nouvelle.
	Pour selectionner une climatologie, cliquez sur "Selection de la climatologie".
	Pour en creer une nouvelle, selectionnez une saison et une longueur d'ondes puis, cochez la case creation d'une nouvelle climatologie.
	}

	label .chr_clim.sais.sais -text "Saison :"
	radiobutton .chr_clim.sais.hiver -text "Hiver" -variable clim_s -value 1
	radiobutton .chr_clim.sais.ete -text "Ete" -variable clim_s -value 2

	label .chr_clim.wlen.wlen -text "Longueur d'onde (en nanometre) :"
	radiobutton .chr_clim.wlen.l340 -text "340" -variable clim_wl -value 1
	radiobutton .chr_clim.wlen.l380 -text "380" -variable clim_wl -value 2
	radiobutton .chr_clim.wlen.l440 -text "440" -variable clim_wl -value 3
	radiobutton .chr_clim.wlen.l470 -text "470" -variable clim_wl -value 4
	radiobutton .chr_clim.wlen.l500 -text "500" -variable clim_wl -value 5
	radiobutton .chr_clim.wlen.l550 -text "550" -variable clim_wl -value 6
	radiobutton .chr_clim.wlen.l670 -text "670" -variable clim_wl -value 7
	radiobutton .chr_clim.wlen.l860 -text "860" -variable clim_wl -value 8
	radiobutton .chr_clim.wlen.l870 -text "870" -variable clim_wl -value 9
	radiobutton .chr_clim.wlen.l940 -text "940" -variable clim_wl -value 10
	radiobutton .chr_clim.wlen.l1020 -text "1020" -variable clim_wl -value 11
	radiobutton .chr_clim.wlen.l1240 -text "1240" -variable clim_wl -value 12
	radiobutton .chr_clim.wlen.l1640 -text "1640" -variable clim_wl -value 13
	radiobutton .chr_clim.wlen.l2130 -text "2130" -variable clim_wl -value 14

	checkbutton .chr_clim.creat -text "Creer une nouvelle climatologie" -variable sa6_clim_new

	button .chr_clim.but.b_sel -text "Selectionner une climatologie" -command {
		set sa6_clim_get 1
		wm state .tl_navig normal
		set laSelect {}
		set navig_msg1 "Selectionnez la climatologie (un ou plusieurs .dat)  que desirez utiliser"
					      
	} 
	button .chr_clim.but.b_ok -text "Continuer" -command {
		set sa6_clim_sais $clim_s
		set sa6_clim_wl $clim_wl
		if { $sa6_clim_new != 1 && $sa6_clim_get != 1 } then {
			wm state .erreur normal
			set msgERR1 "Vous devez soit selectionnez une climatologie ou en creer une nouvelle"
		} else {
			if {$sa6_clim_new == 1} then {
				set sa6_clim_get 0
			} else {
				set sa6_clim_list $selectNavig
			}
			if {$sa6_clim_sais == "NULL" && $sa6_clim_wl == "NULL"} then {
				 wm state .erreur normal
				 set msgERR1 "Selectionner une longueur d'onde et un saison"		
			}
			wm state .chr_clim withdrawn
			wm state .chr_src normal
			set sa6_etape ".chr_src"
		}

	}
		
	button .chr_clim.but.b_canc -text "Annuler" -command {
		glob_canc "close_chr_clim"
	}
	button .chr_clim.b_help -image aide-logo -command {
		     pls_help "http://counter.li.org/"
	}
	grid .chr_clim.info -pady 10
	grid .chr_clim.sais -pady 0
	grid .chr_clim.wlen -pady 20
	grid .chr_clim.creat 
	grid .chr_clim.but -pady 30
	grid .chr_clim.sais.sais -row 0 -column 0 -sticky {w} -columnspan 2
	grid .chr_clim.sais.hiver -row 1 -column 0 
	grid .chr_clim.sais.ete  -row 1 -column 1
	grid .chr_clim.wlen.wlen -row 0 -column 0 -sticky {w} -columnspan 2
	grid .chr_clim.wlen.l340 -row 1 -column 0
	grid .chr_clim.wlen.l380 -row 2 -column 0
	grid .chr_clim.wlen.l440 -row 3 -column 0
	grid .chr_clim.wlen.l470 -row 4 -column 0
	grid .chr_clim.wlen.l500 -row 5 -column 0
	grid .chr_clim.wlen.l550 -row 6 -column 0
	grid .chr_clim.wlen.l670 -row 7 -column 0
	grid .chr_clim.wlen.l860 -row 1 -column 1
	grid .chr_clim.wlen.l870 -row 2 -column 1
	grid .chr_clim.wlen.l940 -row 3 -column 1
	grid .chr_clim.wlen.l1020 -row 4 -column 1 
	grid .chr_clim.wlen.l1240 -row 5 -column 1 
	grid .chr_clim.wlen.l1640 -row 6 -column 1 
	grid .chr_clim.wlen.l2130 -row 7 -column 1 
	grid .chr_clim.creat
	grid .chr_clim.but.b_sel -row 0 -column 0 -padx 10
	grid .chr_clim.but.b_ok  -row 0 -column 1 -padx 10
	grid .chr_clim.but.b_canc -row 0 -column 2 -padx 10
	grid .chr_clim.b_help -sticky {s e} -padx 3 -pady 3                 

	#Fin du toplevel chr_clim
	###########################################################################

	###########################################################################
	#Toplevel chr_src
	toplevel .chr_src
	wm title .chr_src "Sources d'emissions"
	wm state .chr_src withdrawn

frame .chr_src.but

message .chr_src.info -width 400 -text {
Les sources d'emissions sont stockees dans des fichiers .3ds. Certains contiennent des moyennes d'emission tandis que d'autres sont crees pour representer un situation specifique. 
Les fichier dont le nom commence par geia sont des moyennes saissonnieres. Il est conseille de les selectionner.
Pour savoir comment creer vos propres sources d'emission consultez le fichier d'aide.

Si vous souhaitez utilisez aucune sources d'emission, cochez "Aucune source". Sinon, cliquez sur "Selection des .3ds" pour choisir les fichiers a utilisez.

Vous devez aussi specifier si vous desirez tenir compte des emissions de sel marin.
}

checkbutton .chr_src.no -text "Aucune source" -variable sa6_src_no 
checkbutton .chr_src.ssalt -text "Tenir compte des emissions de sel marin" -variable sa6_src_ssalt

button .chr_src.but.b_sel -text "Selection des .3ds" -command {
#####Initialisaton
       if {[file isfile geia_antbc.3ds] != 1} then {
	       exec ln -sf $AODSEM_HOME/AODSEM/Nesfiles/geia_antbc.3ds $sa6_dir_trav
	}
	if {[file isfile geia_biobc.3ds] != 1} then {
	        exec ln -sf $AODSEM_HOME/AODSEM/Nesfiles/geia_biobc.3ds $sa6_dir_trav
	}
	if {[file isfile geia_lowsu.3ds] != 1} then {
	        exec ln -sf $AODSEM_HOME/AODSEM/Nesfiles/geia_lowsu.3ds $sa6_dir_trav
	}
	if {[file isfile geia_higsu.3ds] != 1} then {
	        exec ln -sf $AODSEM_HOME/AODSEM/Nesfiles/geia_higsu.3ds $sa6_dir_trav
	}
	if {[file isfile Lavoue_et_al_bb+ff_bc.3ds] != 1} then {
	        exec ln -sf $AODSEM_HOME/AODSEM/Nesfiles/Lavoue_et_al_bb+ff_bc.3ds $sa6_dir_trav
	}
	if {[file isfile Lavoue_et_al_bb+ff_oc.3ds] != 1} then {
	        exec ln -sf $AODSEM_HOME/AODSEM/Nesfiles/Lavoue_et_al_bb+ff_oc.3ds $sa6_dir_trav
	}
###################							
        set sa6_src_get 1
        wm state .tl_navig normal
        set laSelect {}
        set navig_msg1 "Selectionnez les fichiers .3ds que desirez utiliser"
	set proc_fin_navig "vide"
}
						
button .chr_src.but.b_ok -text "Continuer" -command {
	if {$sa6_src_get == 1} then {
		set sa6_src_list $selectNavig
		 wm state .chr_src withdrawn
	                 wm state .chr_freem normal
	                 set sa6_etape .chr_freem
			 ###Initialisation de variables
			 set sa6_freem_start $sa6_start                                  
			 set sa6_freem_end $sa6_end
	} elseif { $sa6_src_no == 1} then {
		wm state .chr_src withdrawn
		wm state .chr_freem normal
		set sa6_etape .chr_freem
		###Initialisation de variables
                set sa6_freem_start $sa6_start
                set sa6_freem_end $sa6_end

	} else {
		 wm state .erreur normal
                 set msgERR1 "Selection non valide"
	 }
}

button .chr_src.but.b_canc -text "Annuler" -command {
	        glob_canc "close_chr_src"
	}
button .chr_src.b_help -image aide-logo -command {
	        pls_help "www.nature.com"
	}
	
grid .chr_src.info -pady 10
grid .chr_src.no -pady 5
grid .chr_src.ssalt -pady 5
grid .chr_src.but -pady 20
grid .chr_src.but.b_sel -padx 10 -row 0 -column 0
grid .chr_src.but.b_ok -padx 10 -row 0 -column 1
grid .chr_src.but.b_canc -padx 10 -row 0 -column 3
grid .chr_src.b_help -padx 3 -pady 3 -sticky {s e}
																																					       
				
#Fin du toplevel chr_src
###########################################################################

###########################################################################
#Toplevel chr_freem
toplevel .chr_freem

wm title .chr_freem "Assimilation"
wm state .chr_freem withdrawn

frame .chr_freem.inf
frame .chr_freem.temps 
frame .chr_freem.temps.date
frame .chr_freem.temps.space -width 50
frame .chr_freem.size
frame .chr_freem.size.space -width 50
frame .chr_freem.assim -relief groove -bd 4
frame .chr_freem.assim.nassim
frame .chr_freem.assim.mod -relief groove -bd 2
frame .chr_freem.assim.mod.b
frame .chr_freem.wlen -relief groove -bd 4
frame .chr_freem.wlen.wl
frame .chr_freem.check
frame .chr_freem.check.space -width 50
frame .chr_freem.prfich -relief groove -bd 4
frame .chr_freem.prfich.n
frame .chr_freem.but

grid propagate .chr_freem.temps.space 0
grid propagate .chr_freem.size.space 0
grid propagate .chr_freem.check.space 0

grid .chr_freem.inf -row 0 -column 0
grid .chr_freem.temps -row 1 -column 0 -sticky {}
grid .chr_freem.temps.date -row 0 -column 0 -sticky {w}
grid .chr_freem.temps.space -row 0 -column 2 
grid .chr_freem.size -row 2 -column 0 -pady 10 -sticky {}
grid .chr_freem.size.space -row 0 -column 2
grid .chr_freem.assim -row 3 -column 0 -pady 10
grid .chr_freem.assim.nassim -row 0 -column 0 -padx 25
grid .chr_freem.assim.mod -row 0 -column 1 -padx 25 -pady 5
grid .chr_freem.assim.mod.b -row 1 -column 0
grid .chr_freem.wlen -row 5 -column 0 -ipadx 5 -ipady 5 -padx 10
grid .chr_freem.wlen.wl -row 1 -column 0 -columnspan 15 -sticky {w}
grid .chr_freem.check -pady 10 -row 6 -column 0
grid .chr_freem.check.space -row 0 -column 2
grid .chr_freem.prfich -row 7 -column 0
grid .chr_freem.prfich.n -row 1 -column 0 -columnspan 3
grid .chr_freem.but 

message .chr_freem.inf.info -width 600 -text {
La modelisation libre permet d'obtenir les premieres cartes d'epaisseur optique. C'est sur celles-ci que s'effectueront les assimilitions. Cette etape demande un grand temps de calcul. Apportez une attention particuliere a la selection des differents parametres.
}

grid .chr_freem.inf.info

###Initialisation de variables
set sa6_freem_step ""
set sa6_freem_buff ""
set sa6_freem_smooth ""
set sa6_freem_nassim ""
set sa6_freem_wl ""
set sa6_freem_src ""
set sa6_freem_wet ""
set sa6_freem_fn ""
set sa6_freem_dat ""
set sa6_freem_ityp ""
set sa6_freem_if ""
set sa6_freem_mod ""
for {set iter 1} {$iter <= 14} {set iter [expr $iter + 1]} {
	set sa6_freem_wl_$iter 0
}

label .chr_freem.temps.date.deb -text "Debut : "
label .chr_freem.temps.date.fi -text "Fin : "
label .chr_freem.temps.date.debut -textvar sa6_freem_start
label .chr_freem.temps.date.fin -textvar sa6_freem_end
button .chr_freem.temps.chdate -text "Changer les dates" -command {
	wm state .freem_debfin normal
}
	
label .chr_freem.temps.dystep -text "Pas de calcul (en minutes): "
entry .chr_freem.temps.dynstep -textvar sa6_freem_step -width 4

grid .chr_freem.temps.date.deb -row 0 -column 0
grid .chr_freem.temps.date.fi -row 1 -column 0
grid .chr_freem.temps.date.debut -row 0 -column 1
grid .chr_freem.temps.date.fin -row 1 -column 1 
grid .chr_freem.temps.chdate -row 0  -column 1 -sticky {w}
grid .chr_freem.temps.dystep -row 0 -column 3 -sticky {e}
grid .chr_freem.temps.dynstep -row 0 -column 4 -sticky {e}

label .chr_freem.size.l_buffer -text "Taille du tampon (en pixels) : "
entry .chr_freem.size.e_buffer -textvar sa6_freem_buff -width 5
button .chr_freem.size.b_buffer -text "Auto" -command {
	set t_larg_cell [expr ((2*3.14159265*6370000.*($sa6_dom_pix/100.))/360.)]
	set sa6_freem_buff [expr (((7.5*$sa6_freem_step*60.)/$t_larg_cell)+1.)]
	set  sa6_freem_buff [expr int($sa6_freem_buff)]
}
			

label .chr_freem.size.l_smooth -text "Taille de la cellule de lissage 
pour les images de sortie (en degres)"
entry .chr_freem.size.e_smooth -textvar sa6_freem_smooth -width 5
button .chr_freem.size.b_smooth -text "Auto" -command {
	set sa6_freem_smooth [expr (2.*$sa6_dom_pix/100.)]
}


grid .chr_freem.size.l_buffer -row 0 -column 0 -sticky w
grid .chr_freem.size.e_buffer -row 0 -column 1 -sticky w
grid .chr_freem.size.b_buffer -row 0 -column 2 -sticky w
grid .chr_freem.size.l_smooth -row 0 -column 3 -sticky e
grid .chr_freem.size.e_smooth -row 0 -column 4 -sticky e
grid .chr_freem.size.b_smooth -row 0 -column 5 -sticky e

label .chr_freem.assim.nassim.l -text  "Nombre d'assimilation"
entry .chr_freem.assim.nassim.e -textvar sa6_freem_nassim -width 5

label .chr_freem.assim.mod.l -text "Mode d'assimilation"
radiobutton .chr_freem.assim.mod.b.geo -text "Geographique" -variable sa6_freem_mod -value 0
radiobutton .chr_freem.assim.mod.b.dyn -text "Dynamique" -variable sa6_freem_mod -value 1

grid .chr_freem.assim.nassim.l -row 0 -column 0 -padx 10
grid .chr_freem.assim.nassim.e -row 0 -column 1
grid .chr_freem.assim.mod.l -row 0 -column 0
grid .chr_freem.assim.mod.b.geo -row 0 -column 1 -padx 25
grid .chr_freem.assim.mod.b.dyn -row 1 -column 1 -padx 25

label .chr_freem.wlen.wlen -text "Longueur d'onde (en nanometre) : "

button .chr_freem.wlen.wl.ll -text "Reset" -command {
	for {set iter 1} {$iter <= 14} {set iter [expr $iter + 1]} {
	        set sa6_freem_wl_$iter 0
	}
}
label .chr_freem.wlen.wl.l340 -text 340 -width 4
label .chr_freem.wlen.wl.l380 -text 380 -width 4
label .chr_freem.wlen.wl.l440 -text 440 -width 4
label .chr_freem.wlen.wl.l470 -text 470 -width 4
label .chr_freem.wlen.wl.l500 -text 500 -width 4
label .chr_freem.wlen.wl.l550 -text 550 -width 4
label .chr_freem.wlen.wl.l670 -text 670 -width 4
label .chr_freem.wlen.wl.l860 -text 860 -width 4
label .chr_freem.wlen.wl.l870 -text 870 -width 4
label .chr_freem.wlen.wl.l940 -text 940 -width 4
label .chr_freem.wlen.wl.l1020 -text 1020 -width 4
label .chr_freem.wlen.wl.l1240 -text 1240 -width 4 
label .chr_freem.wlen.wl.l1640 -text 1640 -width 4
label .chr_freem.wlen.wl.l2130 -text 2130 -width 4



tixScrolledWindow .chr_freem.wlen.box -scrollbar auto -height 75
set wlbox [.chr_freem.wlen.box subwidget window]


#Construction de la variable sa6_freem wl

grid columnconfigure .chr_freem.wlen.wl {1 2 3 4 5 6 7 8 9 10 11 12 13 14} -minsize 40
grid columnconfigure .chr_freem.wlen 0 -minsize 50
grid columnconfigure $wlbox {1 2 3 4 5 6 7 8 9 10 11 12 13 14} -minsize 40
grid columnconfigure $wlbox 0 -minsize 50
grid columnconfigure $wlbox 15 -minsize 10


grid .chr_freem.wlen.wlen -row 0 -column 0 -columnspan 15 
grid .chr_freem.wlen.box -row 2 -column 0 -columnspan 15

grid .chr_freem.wlen.wl.ll -row 1 -column 0  -sticky {w}
grid .chr_freem.wlen.wl.l340 -row 1 -column 1 -sticky {w}
grid .chr_freem.wlen.wl.l380 -row 1 -column 2 -sticky {w}
grid .chr_freem.wlen.wl.l440 -row 1 -column 3 -sticky {w}
grid .chr_freem.wlen.wl.l470 -row 1 -column 4 -sticky {w}
grid .chr_freem.wlen.wl.l500 -row 1 -column 5 -sticky {w}
grid .chr_freem.wlen.wl.l550 -row 1 -column 6 -sticky {w}
grid .chr_freem.wlen.wl.l670 -row 1 -column 7 -sticky {w}
grid .chr_freem.wlen.wl.l860 -row 1 -column 8  -sticky {w} 
grid .chr_freem.wlen.wl.l870 -row 1 -column 9 -sticky {w}
grid .chr_freem.wlen.wl.l940 -row 1 -column 10 -sticky {w}
grid .chr_freem.wlen.wl.l1020 -row 1 -column 11 -sticky {w}
grid .chr_freem.wlen.wl.l1240 -row 1 -column 12 -sticky {w}
grid .chr_freem.wlen.wl.l1640 -row 1 -column 13 -sticky {w}
grid .chr_freem.wlen.wl.l2130 -row 1 -column 14 -sticky {w}

for {set iter 1} {$iter <= 14} {set iter [expr $iter+1]} {
	#Creer les 14 etiquettes
	label $wlbox.l$iter -text "lambda $iter" -width 10
	grid $wlbox.l$iter -row $iter -column 0  -sticky {w}

	#Creer les 196 boutons et les placer dans la page
	for {set iter2 1} {$iter2 <= 14} {set iter2 [expr $iter2+1]} {
		radiobutton [append temp0$iter-$iter2 $wlbox . l $iter ll $iter2] -variable sa6_freem_wl_$iter -value $iter2 
	}
	for {set iter2 1} {$iter2 <= 14} {set iter2 [expr $iter2+1]} {
	grid [append itero$iter-$iter2 $wlbox . l $iter ll $iter2]  -row $iter -column $iter2  -sticky {w}
}
}

checkbutton .chr_freem.check.src -text "Utilisation d'un inventaire de source ?" -variable sa6_freem_src
checkbutton .chr_freem.check.wet -text "Tenir compte du lessivage ?" -variable sa6_freem_wet
label .chr_freem.check.l_first -text "Numero du premier fichier de sortie"
entry .chr_freem.check.e_first -textvar sa6_freem_fn -width 5
checkbutton .chr_freem.check.rdat -text "Supprimer periodiquement les .dat" -variable sa6_freem_dat
grid .chr_freem.check.src -row 0 -column 0 -columnspan 2 -sticky w
grid .chr_freem.check.wet -row 0 -column 3 -sticky w
grid .chr_freem.check.l_first -row 1 -column 0 -sticky w
grid .chr_freem.check.e_first -row 1 -column 1 -sticky w
grid .chr_freem.check.rdat -row 1 -column 3 -sticky w

radiobutton .chr_freem.prfich.empty -text "Carte d'aod vide" -variable sa6_freem_ityp -value 2 -command {
		proc_freem_ff 0
		set sa6_freem_if "NULL"
	}
radiobutton .chr_freem.prfich.pgm -text "Carte d'aod existante (.pgm)" -variable sa6_freem_ityp -value 0 -command {proc_freem_ff 1}
radiobutton .chr_freem.prfich.dat -text "Matrice existante (.dat)" -variable sa6_freem_ityp -value 1 -command {proc_freem_ff 2}
label .chr_freem.prfich.n.l_name -text "Nom du fichier : "
entry .chr_freem.prfich.n.e_name -textvar sa6_freem_if -width 50
button .chr_freem.prfich.n.b_name -text "..." -command {
	 wm state .tl_navig normal
         set laSelect {}
         set navig_msg1 "Selectionnez la carte ou la matrice initiale"
         set proc_fin_navig "freem_pif"
 }
				 

grid .chr_freem.prfich.empty -row 0 -column 0 -padx 10
grid .chr_freem.prfich.pgm -row 0 -column 1 -padx 10
grid .chr_freem.prfich.dat -row 0 -column 2 -padx 10
grid .chr_freem.prfich.n.l_name -row 1 -column 0
grid .chr_freem.prfich.n.e_name -row 1 -column 1
grid .chr_freem.prfich.n.b_name -row 1 -column 2

button .chr_freem.but.b_ok -text "Continuer" -command {

	#Creation de la variable sa6_freem_wl
	for {set iter 1} {$iter <= 14} {set iter [expr $iter + 1]} {
		set tmpb [join sa6_freem_wl_$iter]
		if {[expr { $tmpb == ""}] == 1} then {
	                set sa6_freem_wl_$iter 0
		}
        }
					
	set sa6_freem_wl [concat $sa6_freem_wl_1 $sa6_freem_wl_2 $sa6_freem_wl_3 $sa6_freem_wl_4 \
	$sa6_freem_wl_5 $sa6_freem_wl_6 $sa6_freem_wl_7 $sa6_freem_wl_8 $sa6_freem_wl_9 $sa6_freem_wl_10 \
	$sa6_freem_wl_11 $sa6_freem_wl_12 $sa6_freem_wl_13 $sa6_freem_wl_14]
	#Fin de la cration de la variable sa6_freem_wl

	#Verification des resultats fournis pour freem
	source $AODSEM_HOME/AODSEM/Source/Aoda6/chr_vrf_freem.tcl
	
if {$sa6_freem_nassim != 0 } then {

#creation et ouverture du toplevel .rdata_rec
source $AODSEM_HOME/AODSEM/Source/Aoda6/chr_rdata.tcl

} else {

		if {$chr_freem_vrf_r == 0} then {
			wm state .chr_freem withdrawn
                	wm state .chr_exec normal
                	set sa6_etape .chr_exec
		}
	}
}

button .chr_freem.but.b_canc -text "Annuler" -command {
                    glob_canc "close_chr_freem"
              }
button .chr_freem.b_help -image aide-logo -command {
                    pls_help "http://www.w3.org/"
              }

grid .chr_freem.but.b_ok  -row 0 -column 0 -pady 20 -padx 30
grid .chr_freem.but.b_canc -row 0 -column 2 -pady 20 -padx 30
grid .chr_freem.b_help -sticky {s e} -padx 3 -pady 3

	
#Fin du toplevel chr_freem
###########################################################################

	###########################################################################
	#Toplevel freem_debfin

	toplevel .freem_debfin
	wm title .freem_debfin "Periode de la modelisation"
	wm state .freem_debfin withdrawn

	frame .freem_debfin.fr_l
	frame .freem_debfin.but

	message .freem_debfin.info -width 400 -text {
	Entrez ici la date a laquelle vous souhaitez debuter la modelisation ainsi que la date a laquelle vous desirez terminer. Ces valeurs serviront de valeurs par defaut plus tard dans l'assistant.
	}

	label .freem_debfin.fr_l.deb -text "Debut"
	label .freem_debfin.fr_l.fin -text "Fin"
	label .freem_debfin.fr_l.l_HH -text "Heure"
	label .freem_debfin.fr_l.l_MI -text "Minute"
	label .freem_debfin.fr_l.l_SS -text "Seconde"
	label .freem_debfin.fr_l.l_JJ -text "Jour"   
	label .freem_debfin.fr_l.l_MO -text "Mois"   
	label .freem_debfin.fr_l.l_AA -text "Annee"  
	entry .freem_debfin.fr_l.d_HH -width 2 -textvar dHH
	entry .freem_debfin.fr_l.d_MI -width 2 -textvar dMI
	entry .freem_debfin.fr_l.d_SS -width 2 -textvar dSS
	entry .freem_debfin.fr_l.d_JJ -width 2 -textvar dJJ
	entry .freem_debfin.fr_l.d_MO -width 2 -textvar dMO
	entry .freem_debfin.fr_l.d_AA -width 4 -textvar dAA
	entry .freem_debfin.fr_l.f_HH -width 2 -textvar fHH
	entry .freem_debfin.fr_l.f_MI -width 2 -textvar fMI
	entry .freem_debfin.fr_l.f_SS -width 2 -textvar fSS
	entry .freem_debfin.fr_l.f_JJ -width 2 -textvar fJJ
	entry .freem_debfin.fr_l.f_MO -width 2 -textvar fMO
	entry .freem_debfin.fr_l.f_AA -width 4 -textvar fAA
	button .freem_debfin.but.b_ok -text "Continuer" -command {
		if {$dHH >= 0 && $dHH < 24} then {set dHHOK 1} else {set dHHOK 0}
		if {$dMI >= 0 && $dMI < 60} then {set dMIOK 1} else {set dMIOK 0}
		if {$dSS >= 0 && $dSS < 60} then {set dSSOK 1} else {set dSSOK 0}
		if {$dJJ > 0 && $dJJ < 32} then {set dJJOK 1} else {set dJJOK 0}
		if {$dMO > 0 && $dMO < 13} then {set dMOOK 1} else {set dMOOK 0}
		if {$dAA > 0 && $dAA < 3000} then {set dAAOK 1} else {set dAAOK 0}
		if {$fHH >= 0 && $fHH < 24} then {set fHHOK 1} else {set fHHOK 0}
		if {$fMI >= 0 && $fMI < 60} then {set fMIOK 1} else {set fMIOK 0}
		if {$fSS >= 0 && $fSS < 60} then {set fSSOK 1} else {set fSSOK 0}
		if {$fJJ > 0 && $fJJ < 32} then {set fJJOK 1} else {set fJJOK 0}
		if {$fMO > 0 && $fMO < 13} then {set fMOOK 1} else {set fMOOK 0}
		if {$fAA > 0 && $fAA < 3000} then {set fAAOK 1} else {set fAAOK 0}
		set debfinOK [expr $dHHOK+$dMIOK+$dSSOK+$dJJOK+$dMOOK+$dAAOK+$fHHOK+$fMIOK+$fSSOK+$fJJOK+$fMOOK+$fAAOK]
		
		if {$debfinOK != 12} then {	
			wm state .erreur normal
			set msgERR1 "Date invalide"
		} else {
			wm state .freem_debfin withdrawn
			set sa6_freem_start [list $dHH $dMI $dSS $dJJ $dMO $dAA]
			set sa6_freem_end [list $fHH $fMI $fSS $fJJ $fMO $fAA]
			set sa6_etape ".chr_freem"
		}
	}
	button .freem_debfin.but.b_canc -text "Annuler" -command {
				glob_canc "close_freem_debfin"
					}
	button .freem_debfin.b_help -image aide-logo -command {
		      pls_help "http://www.edulinux.org/spip/"
	}

	grid .freem_debfin.info
	grid .freem_debfin.fr_l
	grid .freem_debfin.but
	grid .freem_debfin.fr_l.deb -row 1 -column 0
	grid .freem_debfin.fr_l.fin -row 2 -column 0
	grid .freem_debfin.fr_l.l_HH -row 0 -column 1
	grid .freem_debfin.fr_l.l_MI -row 0 -column 2
	grid .freem_debfin.fr_l.l_SS -row 0 -column 3
	grid .freem_debfin.fr_l.l_JJ -row 0 -column 4
	grid .freem_debfin.fr_l.l_MO -row 0 -column 5
	grid .freem_debfin.fr_l.l_AA -row 0 -column 6
	grid .freem_debfin.fr_l.d_HH -row 1 -column 1
	grid .freem_debfin.fr_l.d_MI -row 1 -column 2
	grid .freem_debfin.fr_l.d_SS -row 1 -column 3
	grid .freem_debfin.fr_l.d_JJ -row 1 -column 4
	grid .freem_debfin.fr_l.d_MO -row 1 -column 5
	grid .freem_debfin.fr_l.d_AA -row 1 -column 6
	grid .freem_debfin.fr_l.f_HH -row 2 -column 1
	grid .freem_debfin.fr_l.f_MI -row 2 -column 2
	grid .freem_debfin.fr_l.f_SS -row 2 -column 3
	grid .freem_debfin.fr_l.f_JJ -row 2 -column 4
	grid .freem_debfin.fr_l.f_MO -row 2 -column 5
	grid .freem_debfin.fr_l.f_AA -row 2 -column 6

	grid .freem_debfin.but.b_ok -row 0 -column 0 -padx 20 -pady 30
	grid .freem_debfin.but.b_canc -row 0 -column 1 -padx 20 -pady 30
	grid .freem_debfin.b_help -sticky {s e} -padx 3 -pady 3

	grid columnconfigure .freem_debfin.fr_l {1 2 3 4 5 6} -minsize 60
	#Fin du toplevel freem_debfin
	###########################################################################

###########################################################################
#Toplevel rdata_rec
	
###########################################################################
#Toplevel chr_exec
toplevel .chr_exec
wm title .chr_exec "Execution d'AODSEM"
wm state .chr_exec withdrawn

frame .chr_exec.but
message .chr_exec.info -width 400 -text {
Vous etes maintenant pret a executer AODSEM.
Pour plus de securite, sauvegarder votre session avant de lancer l'execution.
}
button .chr_exec.but.b_ok -text "Lancer l'execution" -command {
	set sa6_exec_chra6 "1"
	save
	exec a6chr.tcl "$sa6_dir_trav/session.pa6" &
	wm state .chr_exec withdrawn
	set sa6_etape .chr_fin
	wm state .chr_fin normal
		
}
button .chr_exec.but.b_canc -text "Annuler" -command {
	glob_canc "close_chr_exec"
}
button .chr_exec.b_help -image aide-logo -command {pls_help "www.google.ca"}


grid .chr_exec.info
grid .chr_exec.but -pady 10
grid .chr_exec.but.b_ok -column 0 -row 0 -padx 2
grid .chr_exec.but.b_canc -column 1 -row 0 -padx 2
grid .chr_exec.b_help -sticky {s e} -padx 3 -pady 3

#Fin du toplevel chr_exec
###########################################################################

###########################################################################
#Toplevel fin

toplevel .chr_fin
wm title .chr_fin "Execution en cours"
wm state .chr_fin withdrawn

frame .chr_fin.but
grid .chr_fin.but -column 0 -row 1

message .chr_fin.msg -width 400 -text {
	L'execution d'AODSEM est maintenant en cours. Pressez annuler pour arreter.
	Vous pouvez quittez aoda6 en toute securite en pressant sur OK.
}
grid .chr_fin.msg -column 0 -row 0 -padx 10 -pady 20

button .chr_fin.but.b_ok -text "OK" -command {
	exit
}

button .chr_fin.but.b_canc -text "Annuler" -command {
	exec killall a6chr.tcl
}

button .chr_fin.b_help -image aide-logo -command {pls_help "http://grass.itc.it/"}

grid .chr_fin.but.b_ok -row 0 -column 0 -padx 10 -pady 10
grid .chr_fin.but.b_canc -row 0 -column 1 -padx 10 -pady 10
grid .chr_fin.b_help -sticky {s e} -padx 3 -pady 3
###########################################################################
#Toplevel chr_standby
#toplevel .chr_standby
#wm title .chr_standby "Execution en cours"
#wm state .chr_standby withdrawn
#
#frame .chr_standby.but
#
#message .chr_standby.msg1 -width 400 -text {
#L'execution d'AODSEM est maintenant en cours. Pressez annuler pour arreter
#}
#tixMeter .chr_standby.meter1
#button .chr_standby.but.b_canc -text "Annuler" -command {exec killall a6chr.tcl}
#button .chr_standby.b_help -image aide-logo -command {pls_help "http://scienceworld.wolfram.com/"}
#
#
#
#grid .chr_standby.msg1 -pady 5
#grid .chr_standby.meter1 -padx 2
#grid .chr_standby.but -pady 10 
#grid .chr_standby.but.b_canc 
#grid .chr_standby.b_help -sticky {s e} -padx 3 -pady 3
#Fin du toplevel chr_exec
###########################################################################
