#chr_vrf_freem.tcl
#Ce fichier contient une serie de test pour valider les entrees faites dans aoda6 concerant freem.

if {$sa6_freem_buff < 0 } then {
	wm state .erreur normal
	set msgERR1 "Mauvaise taille du tampon"
	set chr_freem_vrf_r 1
} elseif {[expr $sa6_freem_step % 1] != 0} then {
	wm state .erreur normal
	set msgERR1 "Le pas de calcul doit etre entier"
	set chr_freem_vrf_r 2
} elseif {[lindex $sa6_freem_wl 0] > 14 || [lindex $sa6_freem_wl 0] < 1} then {
	wm state .erreur normal
	set msgERR1 "Mauvaise longueur d'onde"
	set chr_freem_vrf_r 3
} elseif {$sa6_freem_fn < 0 || $sa6_freem_fn > 9999 } then {
	wm state.erreur normal
	set msgERR1 "Numeros du premier fichier de sortie non valable"
	set chr_freem_vrf_r 4
} elseif { $sa6_freem_nassim >= 1 } then {
	if { $sa6_freem_mod == "" || $sa6_freem_mod == "NULL"} then {
		wm state .erreur normal
		set msgERR1 "Choisissez un mode d'assimilation"
		set chr_freem_vrf_r 5
	}
} else {set chr_freem_vrf_r 0}
