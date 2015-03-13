#!/usr/bin/tixwish

###########################################################################
#Declaration des variables

set AODSEM_HOME [ file nativename ~ ] 
set AODA6_ARTS [file join $AODSEM_HOME AODSEM Nesfiles Aoda6-arts]
#Fin de la declaration des variables
###########################################################################

###########################################################################
#Nettoyage du reperoire
$AODSEM_HOME/AODSEM/Scripts/rmdln
###########################################################################

###########################################################################
#Creation des images

image create photo newfolder -file $AODA6_ARTS/newfolder.gif
image create photo io-logo -file $AODA6_ARTS/in-out.gif
image create photo chrono-logo -file $AODA6_ARTS/chrono.gif
image create photo open-logo -file $AODA6_ARTS/open-a6.gif
image create photo aide-logo -file $AODA6_ARTS/aide.gif
image create photo refresh-logo -file $AODA6_ARTS/refresh.gif
image create photo domaine -file $AODA6_ARTS/domaine.gif
#Fin de la creation des images
###########################################################################

###########################################################################
#Fichiers a inclure
source $AODSEM_HOME/AODSEM/Source/Aoda6/aoda6_proc.tcl
#Fin des fichiers a inclure
###########################################################################

###########################################################################
#Bindings globaux
bind all <Control-s> {
	save
}
bind all <Control-q> {
	exit
}

bind all <Control-o> {
	glob_open
}
#Fin des bindings globaux
###########################################################################

###########################################################################
#Fenetre principale
wm title . "L'assistant d'AODSEM"

. configure -menu .m_princ

frame .fr_princ
frame .fr_princ.fr_txt_chro
frame .fr_princ.fr_txt_io
frame .fr_princ.fr_txt_open
button .fr_princ.b_chrono -image chrono-logo -command {
	wm state .chr_acc normal
	set sa6_sess_typ chronologique
	set sa6_etape ".chr_acc"
}

button .fr_princ.b_io -image io-logo
button .fr_princ.b_open -image open-logo -command {glob_open}
label .fr_princ.fr_txt_chro.titre -text "Nouvelle session en mode chronologique"
label .fr_princ.fr_txt_io.titre -text "Nouvelle session en mode entrees et sorties"
label .fr_princ.fr_txt_open.titre -text "Ouvrir une session existante"
message .fr_princ.fr_txt_chro.info -width 400 -text {
	La session en mode chronologique permet de parcourir, etape par etape, les differentes fonctionnalitees d'AODSEM. Ce mode est suggere aux nouveaux utilisateurs.
}
message .fr_princ.fr_txt_io.info -width 400 -text {
	La session en mode d'entrees et sorties permet d'obtenir rapidement un produit d'AODSEM. Ce mode est suggere aux utilisateurs qui desirent effectuer une tache specifique avec AODSEM.
}

message .fr_princ.fr_txt_open.info -width 400 -text {
	Pour continuer une session precedemment enregistree.
}

grid .fr_princ
grid .fr_princ.fr_txt_chro -column 1 -row 0
grid .fr_princ.fr_txt_io -column 1 -row 1
grid .fr_princ.fr_txt_open -column 1 -row 2
grid .fr_princ.b_chrono -column 0 -row 0 -pady 10 -padx 5
grid .fr_princ.b_io -column 0 -row 1 -pady 10 -padx 5
grid .fr_princ.b_open -column 0 -row 2 -pady 10 -padx 5
grid .fr_princ.fr_txt_chro.titre -sticky {n w}
grid .fr_princ.fr_txt_chro.info -sticky {}
grid .fr_princ.fr_txt_io.titre -sticky {n w}
grid .fr_princ.fr_txt_io.info -sticky {}
grid .fr_princ.fr_txt_open.titre -sticky {n w}
grid .fr_princ.fr_txt_open.info -stick {}

#Fin de la fenetre principale
##########################################################################

##########################################################################
#Fenetre in-out

toplevel .tl_in-out -height 500 -width 600
wm title .tl_in-out "Les entrees et les sorties d'AODSEM"
wm withdraw .tl_in-out

frame .tl_in-out.f_in-out -class "frame" -height 400 -width 550
grid .tl_in-out.f_in-out

frame .tl_in-out.f_conf -class "frame" -height 50 -width 400
grid .tl_in-out.f_conf

set up2date_io {getAvOut [ getAvOut [ getAvOut [ setInDispo ] ] ]}

############################################################
#Frame in-out
tixNoteBook .tl_in-out.f_in-out.nb_in-out -ipadx 10 -ipady 10
#Alias
set nbook_inout .tl_in-out.f_in-out.nb_in-out

$nbook_inout add meteo -label "Meteo"
$nbook_inout add bmies -label "Particule"
$nbook_inout add src -label "Source d'emission"
$nbook_inout add map2d -label "Carte 2D"
$nbook_inout add mat3d -label "Matrice 3D"
$nbook_inout add geom -label "Geomatique"

grid $nbook_inout


#################################################
#Page meteo
set io_nbpage_meteo [$nbook_inout subwidget meteo]

frame  $io_nbpage_meteo.fr_main
tixLabelFrame  $io_nbpage_meteo.fr_main.fr_in  -label "Entrees"

tixLabelFrame  $io_nbpage_meteo.fr_main.fr_out -label "Sorties"

set fr_in_meteo [$io_nbpage_meteo.fr_main.fr_in subwidget frame]
set fr_out_meteo [$io_nbpage_meteo.fr_main.fr_out subwidget frame]

grid  $io_nbpage_meteo.fr_main
grid  $io_nbpage_meteo.fr_main.fr_in -padx 20 -column 0 -row 0
grid  $io_nbpage_meteo.fr_main.fr_out -padx 20 -column 1 -row 0

########################################
#Frame in-meteo

label $fr_in_meteo.gem -text "Donnees meteo GEM"
label $fr_in_meteo.maodsem -text "Donnees meteo AODSEM"
label $fr_in_meteo.imaodsem -text "Index des donnees AODSEM"

grid $fr_in_meteo.gem -padx 5 -pady 5 -row 0
grid $fr_in_meteo.maodsem -padx 5 -pady 5 -row 3
grid $fr_in_meteo.imaodsem -padx 5 -pady 5 -row 8

#####Les Donnees GEM
checkbutton $fr_in_meteo.gemv1 -text "GEMv1" -variable inGEMv1 \
-command "$up2date_io"
checkbutton $fr_in_meteo.gemv2 -text "GEMv2" -variable inGEMv2 \
-command "$up2date_io"

grid $fr_in_meteo.gemv1 -padx 2 -pady 2 -row 1
grid $fr_in_meteo.gemv2 -padx 2 -pady 2 -row 2

#####Les Donnees AODSEM
checkbutton  $fr_in_meteo.rhu -text {Humidite relative} -variable inrhu -command "$up2date_io"
checkbutton  $fr_in_meteo.pr -text "Precipitation" -variable inpr -command "$up2date_io"
checkbutton  $fr_in_meteo.tt -text "Temperature" -variable intt -command "$up2date_io"
checkbutton  $fr_in_meteo.wsp -text "Vitesse du vent" -variable inwsp -command "$up2date_io"

grid $fr_in_meteo.rhu -padx 2 -pady 2 -row 4
grid $fr_in_meteo.pr -padx 2 -pady 2 -row 5
grid $fr_in_meteo.tt -padx 2 -pady 2 -row 6
grid $fr_in_meteo.wsp -padx 2 -pady 2 -row 7

######Les Index AODSEM
checkbutton  $fr_in_meteo.irhu -text "Index des humidites" -variable inirhu -command "$up2date_io"
checkbutton  $fr_in_meteo.ipr -text "Index des precipitations" -variable inipr -command "$up2date_io"
checkbutton  $fr_in_meteo.itt -text "Index des temperatures" -variable initt -command "$up2date_io"
checkbutton  $fr_in_meteo.iwsp -text "Index des vitesses du vent" -variable iniwsp -command "$up2date_io"

grid $fr_in_meteo.irhu -padx 2 -pady 2 -row 9
grid $fr_in_meteo.ipr -padx 2 -pady 2 -row 10
grid $fr_in_meteo.itt -padx 2 -pady 2 -row 11
grid $fr_in_meteo.iwsp -padx 2 -pady 2 -row 12


#Fin du frame in-meteo
########################################

########################################
#Frame out-meteo


label $fr_out_meteo.gem -text "Donnees meteo GEM"
label $fr_out_meteo.maodsem -text "Donnees meteo AODSEM"
label $fr_out_meteo.imaodsem -text "Index des donnees AODSEM"

grid $fr_out_meteo.gem -padx 5 -pady 5 -row 0
grid $fr_out_meteo.maodsem -padx 5 -pady 5 -row 3
grid $fr_out_meteo.imaodsem -padx 5 -pady 5 -row 8

#####Les Donnees GEM
checkbutton $fr_out_meteo.gemv1 -text "GEMv1" -variable outGEMv1 -state disabled
checkbutton $fr_out_meteo.gemv2 -text "GEMv2" -variable outGEMv2 -state disabled

grid $fr_out_meteo.gemv1 -padx 2 -pady 2 -row 1 
grid $fr_out_meteo.gemv2 -padx 2 -pady 2 -row 2

#####Les Donnees AODSEM
checkbutton  $fr_out_meteo.rhu -text "Humidite relative" -variable outrhu -state disabled 
checkbutton  $fr_out_meteo.pr -text "Precipitation" -variable outpr -state disabled 
checkbutton  $fr_out_meteo.tt -text "Temperature" -variable outtt -state disabled 
checkbutton  $fr_out_meteo.wsp -text "Vitesse du vent" -variable -outwsp -state disabled 

grid $fr_out_meteo.rhu -padx 2 -pady 2 -row 4
grid $fr_out_meteo.pr -padx 2 -pady 2 -row 5
grid $fr_out_meteo.tt -padx 2 -pady 2 -row 6
grid $fr_out_meteo.wsp -padx 2 -pady 2 -row 7

######Les Index AODSEM
checkbutton  $fr_out_meteo.irhu -text "Index des humidites" -variable outirhu -state disabled 
checkbutton  $fr_out_meteo.ipr -text "Index des precipitations" -variable outipr -state disabled 
checkbutton  $fr_out_meteo.itt -text "Index des temperatures" -variable outitt -state disabled 
checkbutton  $fr_out_meteo.iwsp -text "Index des vitesses du vent" -variable -outiwsp -state disabled 

grid $fr_out_meteo.irhu -padx 2 -pady 2 -row 9
grid $fr_out_meteo.ipr -padx 2 -pady 2 -row 10
grid $fr_out_meteo.itt -padx 2 -pady 2 -row 11
grid $fr_out_meteo.iwsp -padx 2 -pady 2 -row 12


#Fin du frame out-meteo
########################################
#Fin de la page meteo
#################################################


#Fin du frame in-out
############################################################

############################################################
#Frame de confirmation

button .tl_in-out.f_conf.b_ok -text "Ok"  
grid .tl_in-out.f_conf.b_ok -column 0 -row 0 -padx 5

button .tl_in-out.f_conf.b_cancel -text "Annuler"  
.tl_in-out.f_conf.b_cancel  configure -command {
    wm withdraw .tl_in-out
}

grid .tl_in-out.f_conf.b_cancel -column 1 -row 0 -padx 5
#Fin du frame de confirmation
############################################################

##########################################################################
#Toplevel navigateur

toplevel .tl_navig
wm title .tl_navig "Navigateur AODA6"
wm state .tl_navig withdrawn

frame .tl_navig.fr_msg
frame .tl_navig.fr_filtre
frame .tl_navig.fr_label
frame .tl_navig.fr_listbox
frame .tl_navig.fr_inf_sel
frame .tl_navig.fr_sel
frame .tl_navig.fr_button
frame .tl_navig.fr_confirm
button .tl_navig.b_aide -image aide-logo -command {pls_help "www.distrowatch.com"}

grid .tl_navig.fr_msg -row 0 -columnspan 2 -pady 5
grid .tl_navig.fr_filtre -row 1 -columnspan 2 -pady 5
grid .tl_navig.fr_label -row 2 -columnspan 2
grid .tl_navig.fr_listbox -row 3 -columnspan 2
grid .tl_navig.fr_inf_sel -row 4 -columnspan 2 -pady 0
grid .tl_navig.fr_sel -row 5 -column 0 -pady 2
grid .tl_navig.fr_button -row 5 -column 1 -padx 5
grid .tl_navig.fr_confirm -row 6 -columnspan 2 -pady 10
grid .tl_navig.b_aide -sticky {s e} -padx 3 -pady 3 -columnspan 2

#################################################
#Frame fr_msg

set navig_msg1 "Navigateur"
message .tl_navig.fr_msg.msg1 -width 400 -textvar navig_msg1
grid .tl_navig.fr_msg.msg1

#Fin du frame fr_msg
################################################
#################################################
#Frame fr_filtre
label .tl_navig.fr_filtre.lab1 -text "Filtre : "
entry .tl_navig.fr_filtre.filt -textvar leFiltre
button .tl_navig.fr_filtre.butok -text "Appliquer" -command {
	.tl_navig.fr_listbox.file delete 0 end
	set listTmp [glob -nocomplain -type {f} -directory $rtc $leFiltre]
	set listTmp2 [glob -nocomplain -type {f hidden} -directory $rtc $leFiltre]
	set listTmp3 [glob -nocomplain -type {l} -directory $rtc $leFiltre]
	
	foreach i $listTmp {                 
			set ajout [file tail $i]
			lappend laListeFich $ajout
		}

	foreach i $listTmp2 {
		set ajout [file tail $i]
		lappend laListeFich $ajout
	}

	foreach i $listTmp3 {
		if { [file isfile $i] == 1 } then {
			set ajout [file tail $i]
			lappend laListeFich $ajout
		}
	}

	set laListeFich [lsort -increasing -dictionary $laListeFich]	
}
		 
button .tl_navig.fr_filtre.buthome -text "home" -command {
	cd ~
	#Nettoyage du reperoire
	$AODSEM_HOME/AODSEM/Scripts/rmdln

	nav_refresh
 }
						  
button .tl_navig.fr_filtre.ndir -image newfolder -command {
	wm stat .tl_newfolder normal
}

button .tl_navig.fr_filtre.refresh -image refresh-logo -command {
	nav_refresh
}

grid .tl_navig.fr_filtre.lab1 -row 0 -column 0 -padx 1
grid .tl_navig.fr_filtre.filt -row 0 -column 1 -padx 0
grid .tl_navig.fr_filtre.butok -row 0 -column 2 -padx 0
grid .tl_navig.fr_filtre.buthome -row 0 -column 3 -padx 9
grid .tl_navig.fr_filtre.ndir -row 0 -column 4 -padx 3
grid .tl_navig.fr_filtre.refresh -row 0 -column 5 -padx 3
#Fin du frame filtre
##################################################

##################################################
#Frame label
label .tl_navig.fr_label.wdir -textvar rtc
grid .tl_navig.fr_label.wdir -pady 5
#Fin du frame label
##################################################

##################################################
#Frame listbox
listbox .tl_navig.fr_listbox.dir -listvar laListeDir -xscrollcommand  ".tl_navig.fr_listbox.sb_h_dir set" -yscrollcommand ".tl_navig.fr_listbox.sb_v_dir set" -height 15 -width 25
listbox .tl_navig.fr_listbox.file -listvar laListeFich  -selectmode extended -xscrollcommand  ".tl_navig.fr_listbox.sb_h_file set" -yscrollcommand ".tl_navig.fr_listbox.sb_v_file set" -height 15 -width 25

frame .tl_navig.fr_listbox.space 
scrollbar .tl_navig.fr_listbox.sb_h_dir -orient horizontal -width 10 -command ".tl_navig.fr_listbox.dir xview" 
scrollbar .tl_navig.fr_listbox.sb_v_dir -orient vertical -width 10 -command ".tl_navig.fr_listbox.dir yview"
scrollbar .tl_navig.fr_listbox.sb_h_file -orient horizontal -width 10 -command  ".tl_navig.fr_listbox.file xview"
scrollbar .tl_navig.fr_listbox.sb_v_file -orient vertical -width 10 -command ".tl_navig.fr_listbox.file yview"

grid .tl_navig.fr_listbox.dir -row 1 -column 0 -padx 0
grid .tl_navig.fr_listbox.file -row 1 -column 3  -padx 0 
grid .tl_navig.fr_listbox.sb_h_dir -row 2 -column 0 -sticky {e w}
grid .tl_navig.fr_listbox.sb_v_dir -row 1 -column 1 -sticky {s n}
grid .tl_navig.fr_listbox.sb_h_file -row 2 -column 3 -sticky {e w}
grid .tl_navig.fr_listbox.sb_v_file -row 1 -column 4 -sticky {s n}
grid .tl_navig.fr_listbox.space -row 1 -column 2 -padx 10
#Fin du frame listbox
##################################################
##################################################
#Frame inf_sel

label .tl_navig.fr_inf_sel.lab1 -text "Votre selection correspond aux fichiers ci-dessous : "

grid .tl_navig.fr_inf_sel.lab1 -row 0 -column 0 -padx 3 -pady 2 -sticky {w}

#Fin du frame inf_sel
##################################################

##################################################
#Frame selection
listbox .tl_navig.fr_sel.laSel -listvar laSelect -selectmode extended  -xscrollcommand ".tl_navig.fr_sel.sb_h_sel set" -yscrollcommand ".tl_navig.fr_sel.sb_v_sel set" -width 60 -height 5
scrollbar .tl_navig.fr_sel.sb_h_sel -orient horizontal -width 10 -command ".tl_navig.fr_sel.laSel xview"
scrollbar .tl_navig.fr_sel.sb_v_sel -orient vertical -width 10 -command ".tl_navig.fr_sel.laSel yview"

grid .tl_navig.fr_sel.laSel -row 0 -column 0
grid .tl_navig.fr_sel.sb_h_sel -row 1 -column 0 -sticky {e w}
grid .tl_navig.fr_sel.sb_v_sel -row 0 -column 1 -sticky {n s}

set nb_in_sel [llength $laSelect]

#Fin du frame selection
##################################################

##################################################
#Frame boutons
button .tl_navig.fr_button.ajout -text "Ajouter" -command {
	set rtc [pwd]
	
	foreach fil2add [.tl_navig.fr_listbox.file curselection] {
		.tl_navig.fr_sel.laSel insert end $rtc/[lindex $laListeFich $fil2add]
	}
}
									
button .tl_navig.fr_button.enlever -text "Enlever" -command {

	foreach i [ .tl_navig.fr_sel.laSel curselection ] {
		lappend supprimera $i
	}
	lsort -integer -increasing $supprimera
	.tl_navig.fr_sel.laSel delete [lindex $supprimera 0] [lindex $supprimera end]
	set supprimera [list ]
}
								

grid .tl_navig.fr_button.ajout -pady 3
grid .tl_navig.fr_button.enlever -pady 3
#Fin du frame boutons
##################################################

##################################################
#Frame confirm

#########Initialisation
set proc_fin_navig vide
set proc_navig_argc 0
set proc_navig_argv {}
######################

button .tl_navig.fr_confirm.ok -text "OK" -command {
	set selectNavig $laSelect
	set navig_msg1 ""
	wm state .tl_navig withdrawn
	$proc_fin_navig [llength $selectNavig] $selectNavig
}
button .tl_navig.fr_confirm.canc -text "Annuler" -command { 
	set navig_msg1 ""
	wm state .tl_navig withdrawn
}

grid .tl_navig.fr_confirm.ok -column 0 -row 0 -padx 20
grid .tl_navig.fr_confirm.canc -column 1 -row 0 -padx 20

#Fin du frame confirm
##################################################

##################################################
#Bindings du toplevel navigateur

bind .tl_navig <Control-a> {
	.tl_navig.fr_listbox.file selection set 0 end
}
##################################################
#Bindings de l'"entry" filtre

bind .tl_navig.fr_filtre.filt <KeyRelease-Return> {
	  .tl_navig.fr_listbox.file delete 0 end
	 set listTmp [glob -nocomplain -type {f} -directory $rtc $leFiltre]
	 set listTmp2 [glob -nocomplain -type {f hidden} -directory $rtc $leFiltre]
	 set listTmp3 [glob -nocomplain -type {l} -directory $rtc $leFiltre]
	 foreach i $listTmp {
		 set ajout [file tail $i]
		 lappend laListeFich $ajout
	 }

	 foreach i $listTmp2 {
		 set ajout [file tail $i]
		 lappend laListeFich $ajout
	 }

	 foreach i $listTmp3 {
                 if { [file isfile $i] == 1 } then {
                         set ajout [file tail $i]
                         lappend laListeFich $ajout
                 }
        }


	set laListeFich [lsort -increasing -dictionary $laListeFich]

 }

#Fin des bindings de l'"entry" filtre
##################################################

##################################################
#Bindings de la listbox dir

bind .tl_navig.fr_listbox.dir <Double-Button-1> {

#####Changement de repertoire
	set presRoot [ pwd ]
	set select [.tl_navig.fr_listbox.dir get [.tl_navig.fr_listbox.dir curselection]]
	if { $select == "./" 
	} then {
		  cd .
		#Nettoyage du reperoire
		$AODSEM_HOME/AODSEM/Scripts/rmdln

	  } elseif { 
		  $select == "../" 
	  } then {
		    cd ..
		#Nettoyage du reperoire
		$AODSEM_HOME/AODSEM/Scripts/rmdln
		    
	    } else { 
		      cd $select 
		#Nettoyage du reperoire
		$AODSEM_HOME/AODSEM/Scripts/rmdln
		      
	      }

	      set rtc [pwd]

	      nav_refresh
######Affichage des nouveaux fichiers
#	      .tl_navig.fr_listbox.file delete 0 end
#	      set listTmp [glob -nocomplain -type {f} -directory $rtc *]
#              set listTmp2 [glob -nocomplain -type {f hidden} -directory $rtc *]
#		      
#	     foreach i $listTmp {
#	          set ajout [file tail $i]
#                  lappend laListeFich $ajout
#              }
#
#	     foreach i $listTmp2 {
#		     set ajout [file tail $i]
#		     lappend laListeFich $ajout
#	     }
#
#            set laListeFich [lsort -increasing -dictionary $laListeFich]
#	    
######Affichage des nouveaux repertoire
#									     
#	     .tl_navig.fr_listbox.dir delete 0 end
#             set listTmp [glob -nocomplain -type {d hidden } -directory $rtc *]
#	     set listTmp2 [glob -nocomplain -type {d} -directory $rtc *]
#
#             foreach i $listTmp {
#             set ajout [lindex [file split $i] end]
#             lappend laListeDir $ajout
#             }
#
#	      foreach i $listTmp2 {
#                set ajout [lindex [file split $i] end]
#                lappend laListeDir $ajout
#             }
#							     
#             set laListeDir [lsort -increasing -dictionary $laListeDir]
}
 
#Fin des bindings du listbox dir
##################################################

##################################################
#Bindings du listbox file
bind .tl_navig.fr_listbox.file <Button-3> {
	set rtc [pwd]

        foreach fil2add [.tl_navig.fr_listbox.file curselection] {
	.tl_navig.fr_sel.laSel insert end $rtc/[lindex $laListeFich $fil2add]
        }
}
				

#Fin des bindings du listbox file
##################################################

##################################################
#Bindings du listbox selection

bind .tl_navig.fr_sel.laSel <Button-3> {
	foreach i [ .tl_navig.fr_sel.laSel curselection ] {
		lappend supprimera $i
	}

	lsort -integer -increasing $supprimera

	.tl_navig.fr_sel.laSel delete [lindex $supprimera 0] [lindex $supprimera end]

	set supprimera [list ] 
	
}

#Fin des bindings du listbox selection
##################################################
#Fin des bindings du toplevel navigateur
############################################################

############################################################
#Toplevel nouveau dossier

toplevel .tl_newfolder 
wm title .tl_newfolder "Creer un nouveau repertoire"
wm state .tl_newfolder withdrawn

frame .tl_newfolder.rep
frame .tl_newfolder.but
label .tl_newfolder.rep.msg1 -text "Repertoire courant :"
label .tl_newfolder.rep.rtc -textvar rtc
entry .tl_newfolder.nom -textvar nameNewFolder -width 35
button .tl_newfolder.but.b_ok -text "Creer" -command {\
	file mkdir $nameNewFolder
	set longNameNFolder [file join [pwd] $nameNewFolder ]
	wm state .tl_newfolder withdrawn
	nav_refresh
	
}
button .tl_newfolder.but.b_canc -text "Annuler" -command {
	wm state .tl_newfolder withdrawn
}

button .tl_newfolder.b_help -image aide-logo -command {pls_help "www.slashdot.org"}

grid .tl_newfolder.rep -row 0 -column 0 -pady 5 -padx 2 -sticky {w}
grid .tl_newfolder.but -row 2 -column 0
grid .tl_newfolder.rep.msg1 -row 0 -column 0 -sticky {w}
grid .tl_newfolder.rep.rtc -row 1 -column 0 -sticky {w}
grid .tl_newfolder.nom -row 1 -column 0 -padx 25
grid .tl_newfolder.but.b_ok -row 0 -column 0 -padx 10 -pady 5
grid .tl_newfolder.but.b_canc -row 0 -column 1 -padx 10 -pady 5
grid .tl_newfolder.b_help -sticky {s e} -padx 3 -pady 3
#Fin du toplevel noveau dossier
############################################################

############################################################
#Script a executer apres la creation du toplevel navigateur
nav_refresh
#Fin du script
############################################################

#Fin du toplevel navigateur
###########################################################################

###########################################################################
#Toplevel Erreur
toplevel .erreur
wm title .erreur "Erreur !"
wm state .erreur withdrawn

message .erreur.msg1 -width 200 -textvar msgERR1
button .erreur.b_ok -text "OK" -command {wm state .erreur withdrawn}
button .erreur.b_help -image aide-logo -command {pls_help "www.gnu.org"}

grid .erreur.msg1 -padx 10 -pady 5
grid .erreur.b_ok -padx 20 -pady 5
grid .erreur.b_help -sticky {s e} -padx 3 -pady 3

#Fin du toplevel erreur
###########################################################################

###########################################################################
#Toplevel Confirm

toplevel .confirm
wm title .confirm "Attention !"
wm state .confirm withdrawn

frame .confirm.but
message .confirm.msg1 -width 400 -textvar msgCONF1
button .confirm.but.b_oui -text "Oui" -command {
	$si_confirm_oui
	wm state .confirm withdrawn
	set confirm_rep 1
}
button .confirm.but.b_non -text "Non" -command {
	$si_confirm_non
	wm state .confirm withdrawn
	set confirm_rep 0
}
button .confirm.b_help -image aide-logo -command {pls_help "www.tldp.org"}

grid .confirm.msg1 -row 0 -column 0 -pady 20
grid .confirm.but -row 1 -column 0
grid .confirm.but.b_oui -row 0 -column 0 -padx 2 
grid .confirm.but.b_non -row 0 -column 1 -padx 2
grid .confirm.b_help -columnspan 2 -sticky {s e} -padx 3 -pady 3
#Fin du toplevel confirm
###########################################################################
#Les menus

###########################################################
#Menu de la fenetre principal
menu .m_princ
.m_princ add cascade -menu .m_princ.m_Fich -label "Fichier"
.m_princ add cascade -menu .m_princ.m_Aff -label "Afficher"
.m_princ add cascade -menu .m_princ.m_Aide -label "Aide"

menu .m_princ.m_Fich
.m_princ.m_Fich add command -label "Ouvrir          <Ctrl-o>" -command {glob_open}
.m_princ.m_Fich add command -label "Enregistrer     <Ctrl-s>" -command {save}
.m_princ.m_Fich add command -label "Quitter         <Ctrl-q>"  -command {exit}
menu .m_princ.m_Aff
.m_princ.m_Aff add checkbutton -label "Entrees et sorties" -variable show_tl_io -onvalue 1 -offvalue 0 -command {show_toplevel_io $show_tl_io}
.m_princ.m_Aff add checkbutton -label "Navigateur" -variable show_tl_navig -onvalue 1 -offvalue 0 -command {show_toplevel_navig $show_tl_navig}

menu .m_princ.m_Aide
#Fin du menu de la fenetre principal
###########################################################

#Fin des menus
##########################################################################

###########################################################################
#Message de debuggage
message .debug -textvariable msgdebug
grid .debug

#Fin du message de debuggage
###########################################################################
source $AODSEM_HOME/AODSEM/Source/Aoda6/aoda6_chrono.tcl
###########################################################################
###########################################################################
#FIN
###########################################################################
###########################################################################
