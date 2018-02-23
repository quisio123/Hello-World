;trasladar una forma a fv_dest
;"|FV|TRASLADAR"
(defun |FV|TRASLADAR (fv_lptf fv_dest / fv_dx fv_dy)
	(setq fv_dx (- (car fv_dest) (caar fv_lptf)) fv_dy (- (cadr fv_dest) (cadar fv_lptf)))
	(mapcar '(lambda (el) (list (+ (car el) fv_dx) (+ (cadr el) fv_dy))) fv_lptf) 
)

;escalar una forma en x e y
;"|FV|ESCXY"
(defun |FV|ESCXY (fv_lptf fv_fx fv_fy)
	(cons (car fv_lptf) (mapcar '(lambda (el) (list (+ (* (- (car el) (caar fv_lptf)) fv_fx) (caar fv_lptf)) (+ (* (- (cadr el) (cadar fv_lptf)) fv_fy) (cadar fv_lptf)))) (cdr fv_lptf)))
)

;reflejar una forma
;"|FV|REFLEJAR"
(defun |FV|REFLEJAR (fv_lptf fv_ls)
	(cond
		((null fv_lptf) nil)
		((null (car fv_lptf)) (|FV|REFLEJAR (cdr fv_lptf) fv_ls))
		(T (cons (list (* (caar fv_lptf) (car fv_ls)) (* (cadar fv_lptf) (cadr fv_ls))) (|FV|REFLEJAR (cdr fv_lptf) fv_ls)))
	)
)

;espejar horizontalmente
;"|FV|ESPEJARH"
(defun |FV|ESPEJARH (fv_lptf)
	(|FV|TRASLADAR (|FV|REFLEJAR fv_lptf '(-1 1)) (car fv_lptf))
)

;espejar verticalmente
;"|FV|ESPEJARV"
(defun |FV|ESPEJARV (fv_lptf)
	(|FV|TRASLADAR (|FV|REFLEJAR fv_lptf '(1 -1)) (car fv_lptf))
)

;espejar vertical y horizontalmente
;"|FV|ESPEJAR"
(defun |FV|ESPEJAR (fv_lptf / fv_temp)
	(setq fv_temp (|FV|TRASLADAR (|FV|REFLEJAR fv_lptf '(1 -1)) (car fv_lptf)))
	(|FV|TRASLADAR (|FV|REFLEJAR fv_temp '(-1 1)) (car fv_temp))
)

;estirar X
;"|FV|ESTIRARX"
(defun |FV|ESTIRARX (fv_lpts fv_dx fv_pa)
	(cond
		((null fv_lpts) nil)
		((null fv_pa) (fv_estirarx (cdr fv_lpts) fv_dx (car fv_lpts)))
		((null (car fv_lpts)) (|FV|ESTIRARX (cdr fv_lpts) fv_dx fv_pa))
		(T
			(setq fv_ang (|K|ANG fv_pa (car fv_lpts)))
			(cond
				((or (= fv_ang (/ PI 2)) (= fv_ang (* PI 1.5)))
					(cons (car fv_lpts) (|FV|ESTIRARX (cdr fv_lpts) fv_dx (car fv_lpts)))
				)
				(T
					(setq fv_sents (|N|SENTS fv_ang) fv_ps (|P|C_I fv_pa (car fv_lpts)) fv_sp (|K|PTP (car fv_lpts) (* fv_dx (car fv_sents)) fv_ps))
					(cons fv_sp (|FV|ESTIRARX (cdr (|FV|TRASLADAR fv_lpts fv_sp)) fv_dx fv_sp))
				)
			)
		)
	)
)

;estirar Y
;"|FV|ESTIRARY"
(defun |FV|ESTIRARY (fv_lpts fv_dy fv_pa)
	(cond
		((null fv_lpts) nil)
		((null fv_pa) (|FV|ESTIRARY (cdr fv_lpts) fv_dy (car fv_lpts)))
		((null (car fv_lpts)) (|FV|ESTIRARY (cdr fv_lpts) fv_dy fv_pa))
		(T
			(setq fv_ang (|K|ANG fv_pa (car fv_lpts)))
			(cond
				((or (= fv_ang 0.0) (= fv_ang PI))
					(cons (car fv_lpts) (|FV|ESTIRARY (cdr fv_lpts) fv_dy (car fv_lpts)))
				)
				((= fv_ang (/ PI 2))
					(setq fv_sp (|K|Y+ (car fv_lpts) fv_dy))
					(cons fv_sp (|FV|ESTIRARY (cdr (|FV|TRASLADAR fv_lpts fv_sp)) fv_dy fv_sp))
				)
				((= fv_ang (* PI 1.5))
					(setq fv_sp (|K|Y+ (car fv_lpts) (- fv_dy)))
					(cons fv_sp (|FV|ESTIRARY (cdr (|FV|TRASLADAR fv_lpts fv_sp)) fv_dy fv_sp))
				)
				(T
					(setq fv_sents (|N|SENTS fv_ang) fv_ps (|P|C_I fv_pa (car fv_lpts)) fv_dh (* fv_dy fv_ps) fv_sp (|K|PTP (car fv_lpts) (* fv_dh (cadr fv_sents)) fv_ps))
					(cons fv_sp (|FV|ESTIRARY (cdr (|FV|TRASLADAR fv_lpts fv_sp)) fv_dy fv_sp))
				)
			)
		 )
	)
)

;estirar
;"|FV|ESTIRAR"
(defun |FV|ESTIRAR (fv_lpts fv_dx fv_dy)
	(cond
		((null fv_lpts) nil)
		((null fv_dx)
			(cons (car fv_lpts) (|FV|ESTIRARY (cdr fv_lpts) fv_dy (car fv_lpts)))
		)
		((null fv_dy)
			(cons (car fv_lpts) (|FV|ESTIRARX (cdr fv_lpts) fv_dx (car fv_lpts)))
		)
		(T
			(cons (car fv_lpts) (|FV|ESTIRARY (|FV|ESTIRARX (cdr fv_lpts) fv_dx (car fv_lpts)) fv_dy (car fv_lpts)))
		)
	)
)

;estirar horizontalmente
;"|FV|ESTIRARH"
(defun |FV|ESTIRARH (fv_lptf fv_eh)
	(defun |FV|ESTH (fv_lp fv_pp fv_eh)
		(cond
			((null fv_lp) nil)
			(T
				(setq fv_ang (|K|ANG fv_pp (car fv_lp)) fv_signo (|N|SIGNO (- (caar fv_lp) (car fv_pp))))
				(cond
					((or (= fv_ang 0.0) (equal fv_ang PI 0.001))
						(setq fv_lp (mapcar '(lambda (el) (|K|X+ el (* fv_eh fv_signo))) fv_lp))
						(cons fv_pp (|FV|ESTH (cdr fv_lp) (car fv_lp) fv_eh))
					)
					(T
						(cons fv_pp (|FV|ESTH (cdr fv_lp) (car fv_lp) fv_eh))
					)
				)
			)
		)
	)
	(cons (car fv_lptf) (|FV|ESTH (cdr fv_lptf) (car fv_lptf) fv_eh))
)

;estirar verticalmente
;"|FV|ESTIRARV"
(defun |FV|ESTIRARV (fv_lptf fv_ev)
	(defun |FV|ESTV (fv_lp fv_pp fv_ev)
		(cond
			((null fv_lp) nil)
			(T
				(setq fv_ang (|K|ANG fv_pp (car fv_lp)) fv_signo (|N|SIGNO (- (cadr fv_pp) (cadr (car fv_lp)))))
				(cond
					((or (= fv_ang (/ PI 2)) (= fv_ang (* PI 1.5)))
						(setq fv_lp (mapcar '(lambda (el) (|K|Y+ el (* fv_ev fv_signo))) fv_lp))
						(cons fv_pp (|FV|ESTV (cdr fv_lp) (car fv_lp) fv_ev))
					)
					(T
						(cons fv_pp (|FV|ESTV (cdr fv_lp) (car fv_lp) fv_ev))
					)
				)
			)
		)
	)
	(cons (car fv_lptf) (|FV|ESTV (cdr fv_lptf) (car fv_lptf) fv_ev))
)

;Bandera
;"|FV|BANDERA"
(defun |FV|BANDERA (fv_pib)
	(setq 
	      fv_ppb (|GC|PAD fv_pib (* Pi 1.25) 1.37) fv_spb (|GC|PAD fv_pib (* Pi 0.25) 1.37) fv_tpb (|GC|PAD (|GC|PAD fv_pib (* Pi 0.25) 1.12) (* Pi 1.75) 1.0) fv_cpb (|GC|PAD fv_pib (* Pi 0.25) 0.87)
	)
	(list fv_pib fv_ppb fv_spb fv_tpb fv_cpb)
)

;dibujar una forma en una capa y lugar determinado con deformación vertical
;"|FV|DIBFORM"
(defun |FV|DIBFORM (fv_lispt fv_capa fv_nc fv_desp fv_pc fv_ex fv_ey)
	(setq fv_lisptd (mapcar '(lambda (el) (list (+ (* (car el) fv_ex) (car fv_desp)) (+ (* (- (cadr el) fv_pc) fv_ey) (cadr fv_desp)))) fv_lispt))
	(if (> (length fv_lisptd) 2)
		(|O|LWPOLY fv_lisptd fv_capa fv_nc)
		(|O|LINE (car fv_lisptd) (cadr fv_lisptd) fv_capa)
	)
	fv_lisptd
)
;(|FV|DIBFORM (|FV|NJERSEY (getpoint) 0.3) "0" 1 '(0 0) 0.0 1 1)

;"|FV|LINE"
(defun |FV|LINE (fv_po fv_dx fv_pend)
	(list fv_po (|K|PTP fv_po fv_dx fv_pend))
)

;"|FV|DESP"
(defun |FV|DESP (fv_po fv_desp fv_pc fv_dv fv_dh)
	(list (+ (* (car fv_po) fv_dh) (car fv_desp)) (+ (* (- (cadr fv_po) fv_pc) fv_dv) (cadr fv_desp)))
)

;"|FV|ORIGIN"
(defun |FV|ORIGIN (fv_listpt fv_orig fv_pc fv_dv)
	(mapcar '(lambda (el) (list (- (car el) (car fv_orig)) (+ (/ (- (cadr el) (cadr fv_orig)) fv_dv) fv_pc))) fv_listpt)
)

;Talud
;"|FV|TALUD"
(defun |FV|TALUD (fv_po fv_long fv_dx fv_dy)
	(list fv_po (|GC|PAD fv_po (atan fv_dy fv_dx) fv_long))
)

;Cordon Normal
;"|FV|CORDNOR"
(defun |FV|CORDNOR (fv_po fv_hc fv_acun)
	(if fv_acun
		(setq fv_cun (|FV|CUNETA fv_po fv_acun -0.04) fv_poc (last fv_cun))
		(setq fv_poc fv_po fv_cun (list fv_poc))
	)
	(setq
		fv_psi (|K|Y+ fv_poc 0.15) fv_pse (|K|X+ fv_psi 0.15)
		fv_pie (|K|Y- fv_pse 0.30)
	)
	(append fv_cun (list fv_psi fv_pse fv_pie))
)

;Cordon Montable
;"|FV|CORDMON"
(defun |FV|CORDMON (fv_po fv_hc fv_acun)
	(if fv_acun
		(setq fv_cun (|FV|CUNETA fv_po fv_acun (- fv_hc 0.15)) fv_poc (last fv_cun))
		(setq fv_poc fv_po fv_cun (list fv_poc))
	)
	(setq
		fv_psi (|K|Y+ fv_poc 0.05) fv_pse (|K|Y+ (|K|X+ fv_psi 0.15) 0.05)
		fv_pie (|K|Y- fv_pse 0.30)
	)
	(append fv_cun (list fv_psi fv_pse fv_pie))
)

;Cuneta
;"|FV|CUNETA"
(defun |FV|CUNETA (fv_po fv_ac fv_pend)
	(setq fv_psi (|K|PTP (|K|Y- fv_po 0.05) -0.05 -0.02)
	      fv_pse (|K|PTP fv_psi fv_ac fv_pend)
	      fv_pii (|K|Y- fv_psi 0.15)
	      fv_pie (|K|PTP fv_pii fv_ac fv_pend)
	)
	(list fv_pie fv_pii fv_psi fv_pse)
)

;Cordon Integrado
;"|FV|CORDINT"
(defun |FV|CORDINT (fv_po fv_hc fv_flecha)
	(setq
		fv_pie fv_po fv_pii (|K|PTP fv_pie -0.15 (- fv_flecha))
		fv_pse (|K|Y+ fv_pie 0.15) fv_psi (|K|X- fv_pse 0.15)
	)
	(list fv_pie fv_pii fv_psi fv_pse)
)

;Paquete estructural
;"|FV|PAQEST"
(defun |FV|PAQEST (fv_po fv_ap fv_hp fv_pend)
	(setq
		fv_psi fv_po fv_pse (|K|PTP fv_psi fv_ap fv_pend)
		fv_pii (|K|Y+ fv_po (- fv_hp)) fv_pie (|K|PTP fv_pii fv_ap fv_pend)
	)
	(list fv_psi fv_pii fv_pie fv_pse)
)

;Barrera New Jersey
;"|FV|NJERSEY"
(defun |FV|NJERSEY (fv_po fv_ab)
	(setq
		fv_pa (|K|PTP fv_po fv_ab 0.0)
		fv_pb (|K|Y+ fv_pa 0.3)
		fv_pc (|K|Y+ (|K|PTP fv_pb -0.13 0.0) 0.3)
		fv_pd (|K|Y+ (|K|PTP fv_pc -0.02 0.0) 0.6)
		fv_pe (|K|Y+ fv_po 1.22)
	)
	(list fv_po fv_pa fv_pb fv_pc fv_pd fv_pe)
)

;Cuneta Revestida
;"|FV|CUNREV"
(defun |FV|CUNREV (fv_po fv_al fv_pp fv_hc fv_as fv_ep)
	(setq fv_lf (cons fv_po nil) fv_xep (|N|CDX fv_ep fv_pp) fv_yep (|N|CDY fv_ep fv_pp) fv_pendp (|N|FTOR fv_pp))
	(if (or (not fv_al) (= fv_al 0))
		(if fv_pp
			(setq fv_al (+ fv_ep fv_xep))
			(setq fv_al fv_ep)
		)
	)
	(if (not fv_as) (setq fv_as 0.0))
	(setq fv_lscr (list fv_po (|K|X+ fv_po fv_al))
	      fv_lscr (append fv_lscr (list (|K|PTP (last fv_lscr) (/ fv_hc fv_pendp) (- fv_pendp))))
	      fv_lscr (append fv_lscr (list (|K|X+ (last fv_lscr) fv_as)))
	      fv_lscr (append fv_lscr (list (|K|PTP (last fv_lscr) (/ fv_hc fv_pendp) fv_pendp)))
	      fv_lscr (append fv_lscr (list (|K|X+ (last fv_lscr) fv_al)))
	)
	(setq fv_licr (list (|K|Y- (last fv_lscr) fv_ep))
	      fv_licr (append fv_licr (list (|K|X- (last fv_licr) (- fv_al fv_xep))))
	      fv_licr (append fv_licr (list (|K|PTP (last fv_licr) (- (/ fv_hc fv_pendp)) (- fv_pendp))))
	      fv_licr (append fv_licr (list (|K|X- (last fv_licr) (+ fv_as (* fv_xep 2)))))
	      fv_licr (append fv_licr (list (|K|PTP (last fv_licr) (- (/ fv_hc fv_pendp)) fv_pendp)))
	      fv_licr (append fv_licr (list (|K|X- (last fv_licr) (- fv_al fv_xep))))
	)
	(append fv_lscr fv_licr)
)

;Muro de Sostenimiento
;"|FV|MSOST"
(defun |FV|MSOST (fv_po fv_hl)
	(cond
		((<= fv_hl 1.0)
			(setq fv_ab 1.05 fv_hb 0.2 fv_he 0.3 fv_hm 1.2 fv_hs (+ fv_hl 0.2) fv_as 0.18 fv_ai 0.18 fv_am 0.18 fv_db 0.47) 
		)
		((<= fv_hl 1.5)
			(setq fv_ab 1.40 fv_hb 0.2 fv_he 0.3 fv_hm 0.75 fv_hs (+ fv_hl 0.2) fv_as 0.18 fv_ai 0.24 fv_am 0.18 fv_db 0.66) 
		)
		((<= fv_hl 2.4)
			(setq fv_ab 1.95 fv_hb 0.25 fv_he 0.4 fv_hm 0.95 fv_hs (+ fv_hl 0.2) fv_as 0.25 fv_ai 0.6 fv_am 0.35 fv_db 0.55) 
		)
		((<= fv_hl 3.25)
			(setq fv_ab 2.60 fv_hb 0.25 fv_he 0.55 fv_hm 1.8 fv_hs (+ fv_hl 0.2) fv_as 0.25 fv_ai 0.75 fv_am 0.4 fv_db 0.75) 
		)
		((<= fv_hl 4.15)
			(setq fv_ab 3.50 fv_hb 0.25 fv_he 0.65 fv_hm 2.70 fv_hs (+ fv_hl 0.2) fv_as 0.25 fv_ai 0.95 fv_am 0.4 fv_db 1.4) 
		)
		(T
			(setq fv_ab 4.20 fv_hb 0.25 fv_he 0.70 fv_hm 3.50 fv_hs (+ fv_hl 0.2) fv_as 0.30 fv_ai 1.00 fv_am 0.45 fv_db 1.8) 
		)
	)
 	(setq fv_lmuro (list fv_po (|K|Y+ fv_po fv_hs))
	      fv_lmuro (append fv_lmuro (list (|K|X+ (last fv_lmuro) fv_as)))
	      fv_lmuro (append fv_lmuro (list (|K|X- (last fv_lmuro) (- fv_hs fv_hm))))
	      fv_lmuro (append fv_lmuro (list (|K|X+ (last fv_lmuro) (- fv_am fv_as))))
	      fv_lmuro (append fv_lmuro (list (|K|Y- (|K|X+ (last fv_lmuro) (- fv_ai fv_am)) fv_hm)))
	      fv_lmuro (append fv_lmuro (list (|K|Y- (|K|X+ (last fv_lmuro) fv_db) (- fv_he fv_hb))))
	      fv_lmuro (append fv_lmuro (list (|K|Y- (last fv_lmuro) fv_hb)))
	      fv_lmuro (append fv_lmuro (list (|K|X- (last fv_lmuro) fv_ab)))
	      fv_lmuro (append fv_lmuro (list (|K|Y+ (last fv_lmuro) fv_hb)))
	)
	(l_purgar fv_lmuro)
)
	      
;Dibujar la grilla
;"|FV|GRILLA"
(defun |FV|GRILLA (fv_lon fv_alt fv_pi fv_tit fv_ht fv_defv fv_pc fv_paso fv_st)
	(defun |FV|ROWS (fv_cr fv_p1 fv_ti fv_esp)
		(cond
			((zerop fv_cr) nil)
			(T
				(if fv_esp
					(setq fv_pf (|K|X- fv_p1 fv_lon) fv_pt (|K|X- fv_pf (/ fv_ht 3)) fv_al 2)
					(setq fv_pf (|K|X+ fv_p1 fv_lon) fv_pt (|K|X+ fv_pf (/ fv_ht 3)) fv_al 0)
				)
				(|O|LINE fv_p1 fv_pf "G_Grilla")
				(|O|TEXT fv_pt fv_pt fv_ht (rtos (+ fv_pc fv_cr) 2 1) 0 0.8 0 "ROMANS" 0 fv_al 0 "G_Cotas")
				(if fv_ti (|O|TEXT fv_p1 fv_p1 fv_ht (rtos (+ fv_pc fv_cr) 2 1) 0 0.8 0 "ROMANS" 0 2 0 "G_Cotas"))
				(|FV|ROWS (1- fv_cr) (|K|Y- fv_p1 fv_defv) fv_ti fv_esp)
			)
		)
	)

	(defun |FV|COLS (fv_cc fv_p2 fv_esp)
		(cond
			((zerop fv_cc) nil)
			(T
				(setq fv_pf (|K|Y+ fv_p2 (* fv_alt fv_defv)) fv_pt (|K|Y- fv_p2 (* fv_ht 1.6))
				      fv_signo (if fv_esp "-" "+")
				)
				(|O|LINE fv_p2 fv_pf "G_Grilla")
				(|O|TEXT fv_pt fv_pt fv_ht (strcat fv_signo (rtos (* fv_cc fv_paso) 2 0)) 0 0.8 0 "ROMANS" 0 1 1 "G_Textos")
			  	(|FV|COLS (1- fv_cc) (if fv_esp (|K|X+ fv_p2 fv_paso) (|K|X- fv_p2 fv_paso)) fv_esp)
			)
		)
	)

	(setq fv_lon (|N|SIGPAS fv_lon fv_paso) fv_pid (|K|X+ fv_pi fv_lon) fv_psd (|K|Y+ fv_pid (* fv_alt fv_defv))
	      fv_pii fv_pi fv_psi (|K|Y+ fv_pii (* fv_alt fv_defv)) fv_rows (- (fix fv_alt) 1) fv_cols (- (fix (/ fv_lon fv_paso)) 1) fv_tc 0
	      fv_cerr (if fv_st 128 129) fv_pib (|K|Y+ fv_pi (+ (* fv_alt fv_defv) 1.44))
	)
	(|O|LWPOLY (list fv_pii fv_pid fv_psd fv_psi) "G_Marco" fv_cerr)
	(|FV|ROWS fv_rows (|K|Y- fv_psi fv_defv) (not fv_st) nil)
	(|FV|COLS fv_cols (|K|X- fv_pid fv_paso) nil)
	(if fv_st
		(progn
			(setq fv_pt (|K|Y- fv_pi (* fv_ht 1.6)))
			(|O|TEXT fv_pt fv_pt fv_ht "0" 0 1.0 0 "ROMANS" 0 1 1 "G_Textos")
			(|O|LINE fv_pi (|K|Y+ fv_pi (+ (* fv_alt fv_defv) 2.5)) "G_Eje")
			(|O|LWPOLY (|FV|BANDERA (|K|Y+ fv_pi (+ (* fv_alt fv_defv) 1.44))) "G_Eje" 0)
			(|O|LWPOLY (|FV|ESPEJARH (|FV|BANDERA (|K|Y+ fv_pi (+ (* fv_alt fv_defv) 1.44)))) "G_Eje" 0)
			(|O|LWPOLY (|FV|ESPEJARH (list fv_pii fv_pid fv_psd fv_psi)) "G_Marco" fv_cerr)
			(|FV|ROWS fv_rows (|K|Y- fv_psi fv_defv) (not fv_st) fv_st)
			(|FV|COLS fv_cols (|K|X- fv_pi (- fv_lon fv_paso)) T)
		)
		(|O|TEXT fv_pi fv_pi fv_ht (strcat "P. C.: "(rtos fv_pc 2 0)) 0 0.8 0 "ROMANS" 0 2 0 "G_Cotas")
	)
	(setq fv_ht (if fv_st (* fv_ht 2) (/ (* fv_ht 5.0) 3.0)) fv_pit (|K|Y+ fv_pi (+ (* fv_alt fv_defv) 1 (* fv_ht 1.3))))
	(|O|TEXT fv_pit fv_pit (/ (* fv_ht 5.0) 3.0) fv_tit 0 1.0 0 "ROMANS" 0 1 1 "G_Titulo")
)

;dibujar un acceso
;"|FV|ACC"
(defun |FV|ACC (fv_pt fv_ac fv_lacc fv_lalc fv_idp fv_dpt)
	(setq fv_pr (car fv_idp) fv_di (cadr fv_idp) fv_lado (caddr fv_idp) fv_pe (cadddr fv_idp) fv_rumbo (last fv_idp)
	      fv_scc (cadr fv_dpt) fv_aca (caddr fv_dpt) fv_tipp (substr (last fv_dpt) 3 1) fv_dbc (+ fv_scc fv_aca)
	      fv_racc (+ fv_rumbo (|N|ANGLADO fv_lado))
	      fv_bc (|K|POLAR fv_pe fv_racc fv_dbc) fv_longa (- fv_di fv_dbc) fv_ac (/ fv_ac 2)
	)
	(if (eq fv_tipp "R")
	  (progn
		(setq fv_rc (if (< fv_ac 7.0) 9.0 12.0) fv_palc (|K|POLAR fv_bc fv_racc fv_rc) fv_jota (fix (+ (* fv_ac 2.0) 1.0)))
		(|B|INSERT "ALCA" fv_palc fv_lalc fv_rumbo (list fv_jota 0.8 1.0) nil)
		(|FV|MUROS_ALA (entlast))
	  )
		(setq fv_rc (if (< fv_ac 7.0) 6.0 12.0))
	)
	(setq fv_bc1 (|K|POLAR fv_bc (+ fv_rumbo PI) fv_ac) fv_bc2 (|K|POLAR fv_bc fv_rumbo fv_ac)
	      fv_pc1 (|K|POLAR fv_bc1 (+ fv_rumbo PI) fv_rc) fv_pc2 (|K|POLAR fv_bc2 fv_rumbo fv_rc)
	      fv_cc1 (|K|POLAR fv_pc1 fv_racc fv_rc) fv_cc2 (|K|POLAR fv_pc2 fv_racc fv_rc)
	      fv_fc1 (|K|POLAR fv_bc1 fv_racc fv_rc) fv_fc2 (|K|POLAR fv_bc2 fv_racc fv_rc)
	      fv_dr (- fv_longa fv_rc)
	      fv_mb1 (|N|MINBAR fv_cc1 fv_pc1 fv_fc1) fv_mb2 (|N|MINBAR fv_cc2 fv_pc2 fv_fc2)
	)
	(|O|ARC fv_cc1 fv_rc (car fv_mb1) (cadr fv_mb1) fv_lacc)
	(|O|ARC fv_cc2 fv_rc (car fv_mb2) (cadr fv_mb2) fv_lacc)
	(if (> fv_dr 0.0)
	  (progn
	 	(|O|LINE fv_fc1 (|K|POLAR fv_fc1 fv_racc fv_dr) fv_lacc)
	 	(|O|LINE fv_fc2 (|K|POLAR fv_fc2 fv_racc fv_dr) fv_lacc)
	  )
	)
)

;"|FV|MUROS_ALA"
(defun |FV|MUROS_ALA (fv_nent / fv_ent fv_pi fv_tip fv_lay fv_jota fv_luz fv_ang fv_dl fv_dt fv_v1 fv_v2 fv_v3 fv_v4 fv_f1 fv_f2 fv_f3 fv_f4)
	(setq fv_ent (entget fv_nent) fv_pi (|O|ITEM 10 fv_ent) fv_tip (|O|ITEM 2 fv_ent) fv_lay (|O|ITEM 8 fv_ent)
	      fv_jota (|O|ITEM 41 fv_ent) fv_luz (|O|ITEM 42 fv_ent) fv_an (|O|ITEM 50 fv_ent)
	)
	(if (eq fv_tip "SUMID")
	  (progn
		(setq fv_dl fv_jota fv_dt (/ fv_luz 2)
		      fv_v1 (|K|POLAR (|K|POLAR fv_pi fv_an fv_dl) (+ fv_an (/ PI 2)) fv_dt) fv_f1 (|K|POLAR fv_v1 (+ fv_an (/ PI 4)) fv_luz)
		      fv_v2 (|K|POLAR (|K|POLAR fv_pi fv_an fv_dl) (- fv_an (/ PI 2)) fv_dt) fv_f2 (|K|POLAR fv_v2 (- fv_an (/ PI 4)) fv_luz)
		)
		(|O|LINE fv_v1 fv_f1 fv_lay)
		(|O|LINE fv_v2 fv_f2 fv_lay)
	  )
	  (progn
		(setq fv_dl (/ fv_jota 2) fv_dt (/ fv_luz 2)
		      fv_v1 (|K|POLAR (|K|POLAR fv_pi fv_an fv_dl) (+ fv_an (/ PI 2)) fv_dt) fv_f1 (|K|POLAR fv_v1 (+ fv_an (/ PI 4)) fv_luz)
		      fv_v2 (|K|POLAR (|K|POLAR fv_pi fv_an fv_dl) (- fv_an (/ PI 2)) fv_dt) fv_f2 (|K|POLAR fv_v2 (- fv_an (/ PI 4)) fv_luz)
		      fv_v3 (|K|POLAR (|K|POLAR fv_pi (+ fv_an PI) fv_dl) (+ fv_an (/ PI 2)) fv_dt) fv_f3 (|K|POLAR fv_v3 (- fv_an PI (/ PI 4)) fv_luz)
		      fv_v4 (|K|POLAR (|K|POLAR fv_pi (+ fv_an PI) fv_dl) (- fv_an (/ PI 2)) fv_dt) fv_f4 (|K|POLAR fv_v4 (+ (+ fv_an PI) (/ PI 4)) fv_luz)
		)
		(|O|LINE fv_v1 fv_f1 fv_lay)
		(|O|LINE fv_v2 fv_f2 fv_lay)
		(|O|LINE fv_v3 fv_f3 fv_lay)
		(|O|LINE fv_v4 fv_f4 fv_lay)
	  )
	)
)

;Dibujar la grilla para las láminas
;"|FV|GRILLAL"
(defun |FV|GRILLAL (fv_lon fv_alt fv_pi fv_defv fv_paso)
	(defun |FV|ROWS (fv_cr fv_p1)
		(cond
			((zerop fv_cr) nil)
			(T
				(|O|LINE fv_p1 (|K|X+ fv_p1 fv_lon) "G_Grilla")
				(|FV|ROWS (1- fv_cr) (|K|Y- fv_p1 (/ fv_defv g_eschi)))
			)
		)
	)

	(defun |FV|COLS (fv_cc fv_p2)
		(cond
			((zerop fv_cc) nil)
			(T
				(|O|LINE fv_p2 (|K|Y+ fv_p2 (/ (* fv_alt fv_defv) g_eschi)) "G_Grilla")
			  	(|FV|COLS (1- fv_cc) (|K|X- fv_p2 fv_paso))
			)
		)
	)

	(setq fv_lon (|N|SIGPAS fv_lon fv_paso) fv_pid (|K|X+ fv_pi fv_lon) fv_psd (|K|Y+ fv_pid (/ (* fv_alt fv_defv) g_eschi))
	      fv_pii fv_pi fv_psi (|K|Y+ fv_pii (/ (* fv_alt fv_defv) g_eschi)) fv_rows (- (fix fv_alt) 1) fv_cols (- (fix (/ fv_lon fv_paso)) 1) fv_tc 0
	)
	(|O|LWPOLY (list fv_pii fv_psi fv_psd fv_pid) "G_Marco" 0)
	(|FV|ROWS fv_rows (|K|Y- fv_psi (/ fv_defv g_eschi)))
	(|FV|COLS fv_cols (|K|X- fv_pid fv_paso))
	(setq fv_pc (|K|Y- (|K|X- fv_pi 27.25) 33.7) fv_pp (|K|Y- (|K|X- fv_pi 17.5) 9.35) fv_pt (|K|Y- (|K|X- fv_pi 14.75) 22.45)
	      fv_pn (|K|Y- (|K|X- fv_pi 14.75) 29.95) fv_pr (|K|Y- (|K|X- fv_pi 14.75) 41.2) fv_pd (|K|Y- (|K|X- fv_pi 17.5) 57.35)
	)
	(|O|LWPOLY (list (|K|Y+ fv_pi 180.0) (|K|Y+ fv_pi 200.0) (|K|X+ (|K|Y+ fv_pi 200.0) fv_lon) (|K|X+ (|K|Y+ fv_pi 180.0) fv_lon)) "G_Marco" 1)
	(|O|LWPOLY (list (|K|X- fv_pi 30.0) (|K|X+ fv_pi (+ fv_lon 5)) (|K|Y- (|K|X+ fv_pi (+ fv_lon 5)) 65.5) (|K|Y- (|K|X- fv_pi 30.0) 65.5)) "G_Marco" 1)
	(|O|LINE (|K|Y- (|K|X- fv_pi 30.0) 18.7) (|K|Y- (|K|X+ fv_pi (+ fv_lon 5)) 18.7) "G_Marco")
	(|O|LINE (|K|X- fv_pi 5.0) (|K|Y- (|K|X- fv_pi 5) 65.5) "G_Marco")
	(|O|LINE (|K|Y- (|K|X- fv_pi 24.5) 33.7) (|K|Y- (|K|X+ fv_pi (+ fv_lon 5)) 33.7) "G_Marco")
	(|O|LINE (|K|Y- (|K|X- fv_pi 30.0) 49.2) (|K|Y- (|K|X+ fv_pi (+ fv_lon 5)) 49.2) "G_Marco")
	(|O|LINE (|K|Y- (|K|X- fv_pi 24.5) 18.7) (|K|Y- (|K|X- fv_pi 24.5) 49.2) "G_Marco")
	(|O|TEXT fv_pc fv_pc 3.0 "COTAS" (/ PI 2) 0.75 0 "ROMANS" 0 1 2 "G_Textos")
	(|O|TEXT fv_pp fv_pp 3.0 "PROGRESIVA" 0 0.75 0 "ROMANS" 0 1 2 "G_Textos")
	(|O|TEXT fv_pt fv_pt 3.0 "TERRENO" 0 0.75 0 "ROMANS" 0 1 2 "G_Textos")
	(|O|TEXT fv_pn fv_pn 3.0 "NATURAL" 0 0.75 0 "ROMANS" 0 1 2 "G_Textos")
	(|O|TEXT fv_pr fv_pr 3.0 "RASANTE" 0 0.75 0 "ROMANS" 0 1 2 "G_Textos")
	(|O|TEXT fv_pd fv_pd 3.0 "DIFERENCIA" 0 0.75 0 "ROMANS" 0 1 2 "G_Textos")
)
