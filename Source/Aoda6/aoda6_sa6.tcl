##########################################################################
#Variable		Definition			Valeur possible
#sa6_sess_typ		Type de session			{chrono, e-s}
#sa6_etape		Derniere etape de travail	{ }
#				effectue
#sa6_dir_trav	Repertoire de travail			Nom d'un repertoire
#sa6_gem_conv	Des donnees GEM ont-elles		{0, 1}
#		ete converties
#sa6_dat_meteo	Donnees meteos disponibles		Liste de fichiers
#						  	 .tt .rhu .wsp .PR 
#sa6_cr_ind_wsp	Creation de l'index des vents		{0, 1}
#sa6_cr_ind_tt	Creation de l'index de temperature	{0, 1}
#sa6_cr_ind_rhu Creation de l'index des humidites rel.	{0, 1}
#sa6_cr_ind_pr	Creation de l'index des precipitation 	{0, 1}
#sa6_bmi_fich	Fichiers pour le calcul de Mie		*.bmi
#sa6_dom_pix	Taille du pixel en 100e de degre	entier
#sa6_dom_nlat	Nombre de pixels en latitude		entier
#sa6_dom_nlong	Nombre de pixels en longitude		entier
#sa6_dom_lat0 	Latitude du pixel sud-ouest		reel
#sa6_dom_long0 	Longitude du pixel sud-ouest		reel
#sa6_start 	Debut de la periode de modelisation	HH MM SS JJ MM AAAA
#sa6_end 	Fin de la periode de modelisation	HH MM SS JJ MM AAAA
#sa6_clim_new	Creation d'une nouvelle climato		{0, 1}
#sa6_clim_get	Utilisation du climato existante	{0, 1}
#sa6_clim_list	Fichiers de la climatologie existante	{0, 1}
#sa6_clim_sais	Saison pour la climatologie		{1, 2}
#sa6_clim_wl	Longueur d'onde pour la climatologie	{1 - 14}
#sa6_src_no	Pas de sources d'emission		{0, 1}
#sa6_src_get	Utilisation de sources d'emission	{0, 1}
#sa6_src_list	Liste des fichiers de sources d'emissionListe de fichers .3ds
#sa6_freem_startDebut du la modelisation libre		HH MM SS JJ MM AAAA
#sa6_freem_end  Fin de la periode de mod. libre		HH MM SS JJ MM AAAA
#sa6_freem_step	Pas de calcul pour la mod. libre	entier
#sa6_freem_buff	Taillon du tempon en pixel mod. libre	entier
#sa6_freem_smooth Taille de la cellule de lissage (deg) reel
#sa6_freem_nassim Nombre de fichier d'assimillation	entier
#sa6_freem_lassim Listes des ficiers d'assimilation     Liste de fichiers .pgm
#sa6_freem_wl	Longueur d'onde pour la mod. libre	{1 - 14}
#sa6_freem_src	Utilisation d'un inventaire  de src	{0, 1}
#sa6_freem_wet	Utilisation du lessivage		{0, 1}
#sa6_freem_fn	# du premier fichier de sortie		entier
#sa6_freem_dat  Supression periodique des dat		{0, 1}
#sa6_freem_ityp Type des donnees initial de freem	{0, 1, 2}
#sa6_freem_if	Nom du fichier initial			fichier
#sa6_freem_mod	Mode d'analyse de data			{0, 1}
#sa6_exec_chra6	Utilisation du scripts chra6.tcl	{0, 1}
#sa6_exec_fin	Fin de l'execution d'AODSEM		{0, 1}
##########################################################################
set sa6_sess_typ "NULL"
set sa6_etape "NULL"
set sa6_dir_trav "NULL"
set sa6_gem_conv "NULL"
set sa6_dat_meteo "NULL"
set sa6_cr_ind_wsp "NULL"
set sa6_cr_ind_tt "NULL"
set sa6_cr_ind_rhu "NULL"
set sa6_cr_ind_pr "NULL"
set sa6_bmi_fich "NULL"
set sa6_dom_pix "NULL"
set sa6_dom_nlat "NULL"
set sa6_dom_nlong "NULL"
set sa6_dom_lat0 "NULL"
set sa6_dom_long0 "NULL"
set sa6_start "NULL"
set sa6_end "NULL"
set sa6_clim_new "NULL"
set sa6_clim_get "NULL"
set sa6_clim_list "NULL"
set sa6_clim_sais "NULL"
set sa6_clim_wl "NULL"
set sa6_src_no "NULL"
set sa6_src_get "NULL"
set sa6_src_list "NULL"
set sa6_src_ssalt "NULL"
set sa6_freem_start "NULL"
set sa6_freem_end "NULL"
set sa6_freem_step "NULL"
set sa6_freem_buff "NULL"
set sa6_freem_smooth "NULL"
set sa6_freem_nassim "NULL"
set sa6_freem_lassim "NULL"
set sa6_freem_wl "0 0 0 0 0 0 0 0 0 0 0 0 0 0"
set sa6_freem_src "NULL"
set sa6_freem_wet "NULL"
set sa6_freem_fn "NULL"
set sa6_freem_dat "NULL"
set sa6_freem_ityp "NULL"
set sa6_freem_if "NULL"
set sa6_freem_mod "NULL"
set sa6_exec_chra6 "NULL"
set sa6_exec_fin "NULL"
