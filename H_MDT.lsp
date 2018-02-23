;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;									P	U	N	T	O	S
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;"|H|OBTLARG"
(defun |H|OBTLARG ()
	(if g_ltri
	 	(append g_lar (apply 'append g_lenv))
		(if g_lcn
		  (progn
		  	(setq h_laux '())
		  	(foreach h_cn g_lcn
				(setq h_ccn (car h_cn) h_lacn '())
				(foreach h_ar (cadr h_cn)
					(setq h_pp (|K|Z+ (car h_ar) h_ccn) h_sp (|K|Z+ (cadr h_ar) h_ccn)
					      h_lacn (append h_lacn (list (list h_pp h_sp)))
					)
				)
				(setq h_laux (append h_laux h_lacn))
			)
		  	(|H|ELARNUL h_laux)
		  )
			nil
		)
	)
)

(defun |H|ELARNUL (h_lar)
	(cond
		((null h_lar) nil)
		((> (length h_lar) 10000)
			(setq h_divl (|L|DIVM h_lar))
			(append (|H|ELARNUL (car h_divl)) (|H|ELARNUL (cadr h_divl)))
		)
		((equal (|L|BULT (caar h_lar)) (|L|BULT (cadar h_lar)) g_tol)
			(|H|ELARNUL (cdr h_lar))
		)
		(T
			(cons (car h_lar) (|H|ELARNUL (cdr h_lar)))
		)
	)
)

;CORTRI: corregir triángulos según un punto
;"|H|CORTRI"
(defun |H|CORTRI (h_npt h_pbus h_ltri)
	(cond
		((null h_ltri) nil)
		((member h_pbus (car h_ltri))
			(cons (|L|REEMP h_pbus h_npt (car h_ltri)) (|H|CORTRI h_npt h_pbus (cdr h_ltri)))
		)
		(T
			(cons (car h_ltri) (|H|CORTRI h_npt h_pbus (cdr h_ltri)))
		)
	)
)

;aplicar el criterio de delaunay
;"|H|DELAUNAY"
(defun |H|DELAUNAY (h_cir h_nube h_ptsn)
	(cond ((null h_nube) T)
	      ((null h_cir) T)
		((member (car h_nube) h_ptsn) (|H|DELAUNAY h_cir (cdr h_nube) h_ptsn))
		((|K|ENCIRC (car h_nube) h_cir) nil)
		(T (|H|DELAUNAY h_cir (cdr h_nube) h_ptsn))
	)
)

;hallar un nuevo punto válido para la triangulación
;"|H|BUSCPT"
(defun |H|BUSCPT (h_lpts h_1p h_2p h_ptn)
	(cond
		((null h_lpts) nil)
		((or (> (|K|DISTH h_1p (car h_lpts)) 250.0) (> (|K|DISTH h_2p (car h_lpts)) 250.0))
			(|H|BUSCPT (cdr h_lpts) h_1p h_2p h_ptn)
		)
;;;		((or (> (|K|DISTH h_1p (car h_lpts)) 1000.0) (> (|K|DISTH h_2p (car h_lpts)) 1000.0))
;;;			(|H|BUSCPT (cdr h_lpts) h_1p h_2p h_ptn)
;;;		)
		((equal (car h_lpts) h_ptn 0.001)
			(|H|BUSCPT (cdr h_lpts) h_1p h_2p h_ptn)
		)
		((|GC|COLINEAL (|K|XY h_1p) (|K|XY h_2p) (|K|XY (car h_lpts)) 0.01)
			(|H|BUSCPT (cdr h_lpts) h_1p h_2p h_ptn)
		)
		((|H|DELAUNAY (|K|CIRCULO h_1p h_2p (car h_lpts)) g_lptsm (list h_1p h_2p (car h_lpts)))
			(car h_lpts)
		)
		(T
			(|H|BUSCPT (cdr h_lpts) h_1p h_2p h_ptn)
		)
	)
)

;buscar dentro de que triángulo cae un punto
;"|H|BUSCTRI"
(defun |H|BUSCTRI (h_ltri h_pt)
	(cond
		((null h_ltri) nil)
		((|K|ENTRI h_pt (car h_ltri)) (car h_ltri))
		(T (|H|BUSCTRI (cdr h_ltri) h_pt))
	)
)

;FINDTRI: encontrar los triángulos que comparten una arista
;"|H|FINDTRI"
(defun |H|FINDTRI (h_ltri h_ari)
	(cond
		((null h_ltri) nil)
		((and (member (car h_ari) (car h_ltri)) (member (cadr h_ari) (car h_ltri)))
			(cons (car h_ltri) (|H|FINDTRI (cdr h_ltri) h_ari))
		)
		(T
			(|H|FINDTRI (cdr h_ltri) h_ari)
		)
	)
)

;CONTTRI:devolver el número de triángulos que comparten una arista
;"|H|CONTTRI"
(defun |H|CONTTRI (h_ltri h_ari)
	(cond
		((null h_ltri) 0)
		((and (member (car h_ari) (car h_ltri)) (member (cadr h_ari) (car h_ltri)))
			(+ 1 (|H|CONTTRI (cdr h_ltri) h_ari))
		)
		(T
			(|H|CONTTRI (cdr h_ltri) h_ari)
		)
	)
)

;DEF: deformar verticalmente la red de triángulos
;"|H|DEF"
(defun |H|DEF (h_ltri h_fdv)
	(cond
		((null h_ltri) nil)
		(T
			(setq h_tri (car h_ltri)
			      h_pp (car h_tri) h_sp (cadr h_tri) h_tp (caddr h_tri)
			      h_pp (list (car h_pp) (cadr h_pp) (* (caddr h_pp) h_fdv))
			      h_sp (list (car h_sp) (cadr h_sp) (* (caddr h_sp) h_fdv))
			      h_tp (list (car h_tp) (cadr h_tp) (* (caddr h_tp) h_fdv))
			)
			(|O|3DFACE h_pp h_sp h_tp h_pp "T_TIND")
			(|H|DEF (cdr h_ltri) h_fdv)
		)
	)
)

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;								Q	U	I	E	B	R	E	S	
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;obtener la lista de líneas de ruptura
;"|H|CLLR"
(defun |H|CLLR ()
	(defun |H|NOENV (h_lq)
		(cond
			((null h_lq) nil)
			((or (null (car h_lq)) (null (caar h_lq)) (null (cadar h_lq))) (|H|NOENV (cdr h_lq)))
			((or (member (car h_lq) g_lenv) (member (reverse (car h_lq)) g_lenv)) (|H|NOENV (cdr h_lq)))
			(T (cons (car h_lq) (|H|NOENV (cdr h_lq))))
		)
	)
	(defun |H|LISTLR (h_lelr / h_lr h_tip h_lay)
		(cond
			((null h_lelr) nil)
			(T
				(setq h_lr (entget (car h_lelr)) h_tip (|O|ITEM 0 h_lr) h_lay (|O|ITEM 8 h_lr))
				(cond
					((member h_lay h_lno)
						(|H|LISTLR (cdr h_lelr))
					)
					((eq h_tip "LWPOLYLINE")
						(append (|A|LISSEG (|A|LVPOLI (car h_lelr))) (|H|LISTLR (cdr h_lelr)))
					)
					((eq h_tip "LINE")
						(append (list (list (|K|XY (|O|ITEM 10 h_lr)) (|K|XY (|O|ITEM 11 h_lr)))) (|H|LISTLR (cdr h_lelr)))
					)
					(T
						(|H|LISTLR (cdr h_lelr))
					)
				)
			)
		)
	)
	(defun |H|VALLR (h_llq)
		(defun |H|OBTCOTA (h_lpts h_pt)
			(cond
				((null h_lpts) nil)
				((equal (|K|XY (car h_lpts)) h_pt g_tol) (car h_lpts))
				(T (|H|OBTCOTA (cdr h_lpts) h_pt))
			)
		)
		(cond
			((null h_llq) nil)
			((and (setq h_pi (|H|OBTCOTA g_lptsm (caar h_llq))) (setq h_pf (|H|OBTCOTA g_lptsm (cadar h_llq))))
				(cons (list h_pi h_pf) (|H|VALLR (cdr h_llq)))
			)
			(T
				(|H|VALLR (cdr h_llq))
			)
		)
	)
	(setq h_sslr (|L|SSL (ssget "X" '((8 . "R_*")))) h_lno (list "R_LAT" "R_LBT" "R_LMT" "R_Agua Cruda" "R_Agua Potable" "R_FFCC" "R_Sangria"))
	(|H|NOENV (|H|VALLR (|L|PURGAR (|H|LISTLR h_sslr)))) 
)

;buscar la intersección de una linea de ruptura con las aristas
;"|H|BUSCINT"
(defun |H|BUSCINT (h_lar h_lr)
	(defun |H|BINT (h_lari) 
		(cond
			((null h_lari) nil)
			((setq h_pint (inters (|K|XY (car h_lr)) (|K|XY (cadr h_lr)) (|K|XY (caar h_lari)) (|K|XY (cadar h_lari))))
;;;				(if (or (equal (|K|XY h_pint) (|K|XY (car h_lr)) g_tol) (equal (|K|XY h_pint) (|K|XY (cadr h_lr)) g_tol))
;;;					(|H|BINT (cdr h_lari))
;;;					(cons (cons (|K|DISTH h_pint (car h_lr)) (car h_lari)) (|H|BINT (cdr h_lari)))
;;;				)
				(if (|N|ENTREE (|GC|PARAM (list (|K|XY (caar h_lari)) (|K|XY (cadar h_lari))) (|K|XY h_pint)) '(0 1))
					(cons (cons (|K|DISTH h_pint (car h_lr)) (car h_lari)) (|H|BINT (cdr h_lari)))
					(|H|BINT (cdr h_lari))
				)
			)
			(T
				(|H|BINT (cdr h_lari))
			)
		)
	)
	(|L|CDRSL (|L|ORDENAR (|H|BINT h_lar)))
)



;determinar si una linea de ruptura es una arista
;"|H|ESARI"
(defun |H|ESARI (h_lar h_lr)
	(cond
		((null h_lar) nil)
		((and (member (car h_lr) (car h_lar)) (member (cadr h_lr) (car h_lar))) T)
		(T
			(|H|ESARI (cdr h_lar) h_lr)
		)
	)
)

;determinar si una linea de ruptura es una arista
;"|H|MEZCLA"
(defun |H|MEZCLA (h_lar1 h_lar2)
	(cond
		((null h_lar2) nil)
		(T
			(append (list (car h_lar1) (car h_lar2)) (|H|MEZCLA (cdr h_lar1) (cdr h_lar2)))
		)
	)
)

;;;(setq lista (list 1 2 3 4 5 6) llist (length lista) mlist (fix (/ llist 2)) plist (|l|subl lista mlist) slist (|l|resl lista mlist))
;determinar si una linea de ruptura es una arista
;"|H|RESCONF"
(defun |H|RESCONF (h_lar)
	(defun |H|FLAR (h_lari)
		(cond
			((null h_lari) nil)
			(T
				(setq h_ar (car h_lari)
				      h_ltri (|H|FINDTRI g_ltri h_ar) h_ptri (car h_ltri) h_stri (cadr h_ltri)
				      h_pop1 (car (|L|LIBRES h_ptri h_ar nil)) h_pop2 (car (|L|LIBRES h_stri h_ar nil))
				)
				(if (inters (|K|XY (car h_ar)) (|K|XY (cadr h_ar)) (|K|XY h_pop1) (|K|XY h_pop2))
				  (progn
					(setq h_nar (list h_pop1 h_pop2)
					      h_ntr1 (list h_pop1 h_pop2 (car h_ar)) h_ntr2 (list h_pop1 h_pop2 (cadr h_ar))
					      g_ltri (|L|REEMP h_ptri h_ntr1 g_ltri) g_ltri (|L|REEMP h_stri h_ntr2 g_ltri)
					      g_lar (|L|REEMP h_ar h_nar g_lar)
					)
				   )
				)
				(|H|FLAR (cdr h_lari))
			)
		)
	)
	(setq h_divl (|L|DIVM h_lar) h_lar1 (car h_divl) h_lar2 (cadr h_divl))
	(|H|FLAR (|L|PURGAR (|H|MEZCLA (reverse h_lar1) h_lar2)))
)

;hacer flip en las aristas
;"|H|FLIPAR"
(defun |H|FLIPAR (h_llr h_cont)
	(cond
		((null h_llr) nil)
		((setq h_larc (|H|BUSCINT g_lar (car h_llr)))
			(cond
				((= (length h_larc) 1)
					(setq h_ar (car h_larc)
					      h_ltri (|H|FINDTRI g_ltri h_ar) h_ptri (car h_ltri) h_stri (cadr h_ltri)
					      h_pop1 (car (|L|LIBRES h_ptri h_ar nil)) h_pop2 (car (|L|LIBRES h_stri h_ar nil))
					      h_ntr1 (list h_pop1 h_pop2 (car h_ar)) h_ntr2 (list h_pop1 h_pop2 (cadr h_ar))
					      g_ltri (|L|REEMP h_ptri h_ntr1 g_ltri) g_ltri (|L|REEMP h_stri h_ntr2 g_ltri)
					      g_lar (|L|BORRA h_ar g_lar)
					)
					(princ (strcat "\n" (itoa h_cont) " líneas de ruptura procesadas"))
					(cons (car h_llr) (|H|FLIPAR (cdr h_llr) (1+ h_cont)))
				)
				(T
					(|H|RESCONF h_larc)
					(princ (strcat "\n" (itoa h_cont) " líneas de ruptura procesadas"))
					(|H|FLIPAR (cdr h_llr) (1+ h_cont))
				)
			)
		)
		(T
			(cons (car h_llr) (|H|FLIPAR (cdr h_llr) h_cont))
		)
	)
)

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;									A	R	I	S	T	A	S
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;GETAR: obtener la lista completa de aristas a partir de la red de triángulos
;"|H|GETAR"
(defun |H|GETAR (h_ltri)
	(cond
		((null h_ltri) nil)
		(T
			(append (list (list (caar h_ltri) (cadar h_ltri)) (list (cadar h_ltri) (caddar h_ltri)) (list (caddar h_ltri) (caar h_ltri))) (|H|GETAR (cdr h_ltri)))
		)
	)
)

;GETARS: obtener la lista de aristas
;"|H|GETARS"
(defun |H|GETARS ()
	(defun |H|SEPENV (h_lar / h_lart h_lenv h_ar)
		(setq h_lart '() h_lenv '())
		(while h_lar
			(setq h_ar (car h_lar) h_lar (cdr h_lar))
			(cond
				((member h_ar h_lar) (setq h_lart (cons h_ar h_lart) h_lar (|L|BORRA h_ar h_lar)))
				((member (reverse h_ar) h_lar) (setq h_lart (cons h_ar h_lart) h_lar (|L|BORRA (reverse h_ar) h_lar)))
				(T (setq h_lenv (cons h_ar h_lenv)))
			)
		)
		(list h_lart (|L|PURGAR (|A|ORDENASEG h_lenv)))
	)
	(if g_ltri (|H|SEPENV (|H|GETAR g_ltri)) (list nil nil))
)

(defun |H|SEPLQ (h_lar h_lq)
	(cond
		((null h_lar) nil)
		((or (null (car h_lar)) (null (caar h_lar)) (null (cadar h_lar))) (|H|SEPLQ (cdr h_lar) h_lq))
		((or (member (car h_lar) h_lq) (member (reverse (car h_lar)) h_lq)) (|H|SEPLQ (cdr h_lar) h_lq))
		(T (cons (car h_lar) (|H|SEPLQ (cdr h_lar) h_lq)))
	)
)

;DIBAR: dibujar las aristas
;"|H|DIBAR"
(defun |H|DIBAR (h_laris)
	(cond
		((null h_laris) nil)
		(T
			(|O|LINE (caar h_laris) (cadar h_laris) "T_Aristas")
			(|H|DIBAR (cdr h_laris))
		)
	)
)

;DELAR: borrar las aristas
;"|H|DELAR"
(defun |H|DELAR ()
	(|O|DELENTS (|L|SSL (ssget "X" '((8 . "T_Aristas")))))
)

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;								E	N	V	O	L	V	E	N	T	E
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;DIBENV: dibujar la envolvente convexa
;"|H|DIBENV"
(defun |H|DIBENV ()
	(|H|DELENV)
	(foreach h_env g_lenv
		(if h_env (|O|LWPOLY (|A|LISTAV h_env) "T_Envolvente" 0))
	)
	(redraw)
)

;DELENV: borrar la envolvente convexa
;"|H|DELENV"
(defun |H|DELENV ()
	(|O|DELENTS (|L|SSL (ssget "X" '((8 . "T_Envolvente")))))
)

;obtener los puntos de la envolvente
;"|H|OBTPENV"
(defun |H|OBTPENV (h_lenv)
	(|L|ELIMDUP (append (mapcar 'car h_lenv) (mapcar 'cadr h_lenv)))
)

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;										M		D		T
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;GUARDA: guardar la red de triángulos
;"|H|STLISTT"
(defun |H|STLISTT (h_let / h_e)
	(cond
		((null h_let) nil)
		(T
			(setq h_e (entget (car h_let)) h_pp (|O|ITEM 10 h_e) h_sp (|O|ITEM 11 h_e) h_tp (|O|ITEM 12 h_e))
			(cons
				(list
				       (strcat "(list (list " (rtos (car h_pp) 2 4) " " (rtos (cadr h_pp) 2 4) " " (rtos (caddr h_pp) 2 3) ") "
			      			"(list " (rtos (car h_sp) 2 4) " " (rtos (cadr h_sp) 2 4) " " (rtos (caddr h_sp) 2 3) ") "
			      			"(list " (rtos (car h_tp) 2 4) " " (rtos (cadr h_tp) 2 4) " " (rtos (caddr h_tp) 2 3) ")) "
					)
				)
				(|H|STLISTT (cdr h_let))
			)
		)
	)
)

;"|H|GUARDA"
(defun |H|GUARDA()
	(princ "\n obteniendo la lista de triangulos")
	(setq h_lss (|L|SSL (ssget "X" '((8 . "T_TIN"))))
	      h_lini (list "(setq g_ltri (list ") h_linf (list ")) ")
	)
	(|F|GUARDAR g_datos "TIN" "lsp" nil (append (list h_lini) (|H|STLISTT h_lss) (list h_linf)) "" T)
)

;CARGA: cargar los datos de la red de triángulos
(defun |H|LISTT (h_let / h_e)
	(cond
		((null h_let) nil)
		(T
			(setq h_e (entget (car h_let)) h_pp (|O|ITEM 10 h_e) h_sp (|O|ITEM 11 h_e) h_tp (|O|ITEM 12 h_e))
			(cons (list h_pp h_sp h_tp) (|H|LISTT (cdr h_let)))
		)
	)
)

;ELTRI: eliminar triángulos de la lista de triángulos
(defun |H|ELTRI (h_lte)
	(cond
		((null h_lte) nil)
		(T
			(setq g_ltri (|L|BORRA (car h_lte) g_ltri))
			(|H|ELTRI (cdr h_lte))
		)
	)
)

;eliminar triángulos
;"|H|DELTRI"
(defun |H|DELTRI ()
	(setq h_ltel (|L|SSl (ssget '((8 . "T_TIN")))))
	(|H|ELTRI (|H|LISTT h_ltel))
	(|O|DELENTS h_ltel)
	(princ "\n calculando envolvente y lista de aristas")
	(setq h_laryen (|H|GETARS g_ltri) g_lar (car h_laryen) g_lenv (cadr h_laryen))
	(|H|GUARDA) (|H|DIBENV)
)

;"|H|CARGA"
(defun |H|CARGA ()
	(setq h_lss (|L|SSL (ssget "X" '((8 . "T_TIN")))))
	(if h_lss
		(setq g_ltri (|H|LISTT h_lss))
		(load (strcat g_datos "TIN.lsp") "")
	)
	(princ (strcat (itoa (length g_ltri)) " triángulos cargados\n"))
)

;"|H|SELECT"
(defun |H|SELECT (h_pi kdt_pf)
	(setq h_lss (|L|SSL (ssget "F" (list h_pi h_pf) '((8 . "T_TIN")))))
	(if h_lss
		(setq g_ltri (|H|LISTT h_lss))
		(load (strcat g_datos "TIN.lsp") "")
	)
)

;BORRA: borrar la red de triángulos
;"|H|BORRA"
(defun |H|BORRA ()
	(|O|DELENTS (|L|SSL (ssget "X" '((8 . "T_TIN")))))
)

;DIB: dibujar la red de triángulos
;"|H|DIB"
(defun |H|DIB (h_ltri)
	(cond
		((null h_ltri) nil)
		(T
			(|O|3DFACE (caar h_ltri) (cadar h_ltri) (caddar h_ltri) (caar h_ltri) "T_TIN")
			(|H|DIB (cdr h_ltri))
		)
	)
)

;DIB: deformar la red de triángulos
;"|H|DEF"
(defun |H|DEF (h_ltri h_dv)
	(cond
		((null h_ltri) nil)
		(T
			(setq h_tri (car h_ltri) h_pp (|K|ZxD (car h_tri) h_dv) h_sp (|K|ZxD (cadr h_tri) h_dv) h_tp (|K|ZxD (caddr h_tri) h_dv))
			(|O|3DFACE h_pp h_sp h_tp h_pp "T_TIND")
			(|H|DEF (cdr h_ltri) h_dv)
		)
	)
)

;eliminar los puntos que quedan dentro de la envolvente de la lista de puntos a procesar
;"|H|ELPTS"
(defun |H|ELPTS (h_lpts h_lpel)
	(cond
		((null h_lpts) nil)
		((null h_lpel) h_lpts)
		((member (car h_lpts) h_lpel)
			(|H|ELPTS (cdr h_lpts) h_lpel)
		)
		(T
			(cons (car h_lpts) (|H|ELPTS (cdr h_lpts) h_lpel))
		)
	)
)

;eliminar las aristas duplicadas de la lista de aristas
;"|H|PURGAAR"
(defun |H|PURGAAR (h_lars h_larc)
	(cond
		((null h_lars) nil)
		((member (car h_lars) (cdr h_lars))
			(setq h_pos (|L|POS (car h_lars) (cdr h_lars))
			      h_lars (|L|BORRA h_pos (cdr h_lars))
			      h_larc (|L|BORRA h_pos (cdr h_larc))
			)
			(|H|PURGAAR h_lars h_larc)
		)
		((member (reverse (car h_lars)) (cdr h_lars))
			(setq h_pos (|L|POS (reverse (car h_lars)) (cdr h_lars))
			      h_lars (|L|BORRA h_pos (cdr h_lars))
			      h_larc (|L|BORRA h_pos (cdr h_larc))
			)
			(|H|PURGAAR h_lars h_larc)
		)
		(T
			(cons (car h_larc) (|H|PURGAAR (cdr h_lars) (cdr h_larc)))
		)
	)
)

;procesar la lista de aristas para encontrar nuevos triángulos
;"|H|PLISAR"
(defun |H|PLISAR (h_larp h_cnp h_dcen h_lpl h_nla h_lanp h_ct)
	(redraw)
	(cond
		((null h_larp)
			(append (|H|PURGAAR (mapcar '(lambda (el) (list (car el) (cadr el))) h_nla) h_nla) h_lanp)
		)
		(T
			(cond
				((> (|K|DISTH (|K|PMED (caar h_larp) (cadar h_larp)) h_cnp) h_dcen)
					(|H|PLISAR (cdr h_larp) h_cnp h_dcen h_lpl h_nla (cons (car h_larp) h_lanp) h_ct)
				)
				((> (|H|CONTTRI g_ltri (car h_larp)) 1)
					(|H|PLISAR (cdr h_larp) h_cnp h_dcen h_lpl h_nla h_lanp h_ct)
				)
				((setq h_tp (|H|BUSCPT h_lpl (caar h_larp) (cadar h_larp) (caddar h_larp)))
					(setq g_ltri (cons (list (caar h_larp) (cadar h_larp) h_tp) g_ltri)
					      h_nar1 (list (caar h_larp) h_tp (cadar h_larp))
					      h_nar2 (list h_tp (cadar h_larp) (caar h_larp))
					      h_ct (1+ h_ct)
					)
					(princ (strcat "\n" (itoa h_ct) " triangulos encontrados"))
					(|O|3DFACE (caar h_larp) (cadar h_larp) h_tp (caar h_larp) "T_TIN")
					(|H|PLISAR (cdr h_larp) h_cnp h_dcen h_lpl (append h_nla (list h_nar1 h_nar2)) h_lanp h_ct)
				)
				(T
					(|H|PLISAR (cdr h_larp) h_cnp h_dcen h_lpl h_nla h_lanp h_ct)
				)
			)
		)
	)
)

;"|H|CREA"
(defun |H|CREA ()
	(|H|BORRA) (|H|DELENV) (redraw)
	(setq h_tini (getvar "DATE") h_sini (* 86400.0 (- h_tini (fix h_tini))))
	(if (not g_lptsm) (setq g_lptsm (|L|DEPURAR (|L|ELIMDUP (|K|OBTPTS g_lpts)))))
	(setq g_ltri '() h_mm (|L|M&M g_lptsm) h_xmm (car h_mm) h_ymm (cadr h_mm) h_dx (- (car h_xmm) (cadr h_xmm)) h_dy (- (car h_ymm) (cadr h_ymm)))
	(princ "\n procesando los datos")
	(setq g_lptsm (|K|ORDDC g_lptsm (|K|CENTRO g_lptsm)) h_lpts g_lptsm h_tp (|H|BUSCPT h_lpts (car g_lptsm) (cadr g_lptsm) (car g_lptsm))
	      g_ltri (cons (list (car g_lptsm) (cadr g_lptsm) h_tp) g_ltri)
	      h_lar (list (list (car g_lptsm) (cadr g_lptsm) h_tp) (list (cadr g_lptsm) h_tp (car g_lptsm)) (list h_tp (car g_lptsm) (cadr g_lptsm)))
	      h_cnp (car (|K|CIRCULO (car g_lptsm) (cadr g_lptsm) h_tp)) g_lptsm (|K|ORDDC g_lptsm h_cnp) h_lpts g_lptsm
	      h_penv (list (car g_lptsm) (cadr g_lptsm) h_tp)
	)
	(|O|3DFACE (car g_lptsm) (cadr g_lptsm) h_tp (car g_lptsm) "T_TIN")(redraw)
	(while (and h_lpts (> (length h_lar) 2))
		(setq h_lar (|H|PLISAR h_lar h_cnp (max (|H|OBTDMED h_lar h_cnp) 300.0) h_lpts nil nil (length g_ltri))
		      h_npenv (|H|OBTPENV h_lar) h_cpa (length h_lpts)
		      h_lpts (|H|ELPTS h_lpts (|L|LIBRES h_penv h_npenv nil)) h_penv h_npenv
		      h_desp (- h_cpa (length h_lpts)) h_divl (|L|DIVL g_lptsm h_desp)
		      g_lptsm (append (cadr h_divl) (car h_divl))
		)(redraw)
	) (redraw)
	(setq h_tfin (getvar "DATE") h_sfin (* 86400.0 (- h_tfin (fix h_tfin))) h_tiempo (- h_sfin h_sini)
	      h_horas (fix (/ h_tiempo 3600)) h_min (fix (/ (- h_tiempo (* h_horas 3600)) 60)) h_seg (- h_tiempo (* h_horas 3600) (* h_min 60))
	)
	(princ (strcat "\n" (itoa (length g_ltri)) " triángulos procesados en " (itoa h_horas) "h" (itoa h_min) "m" (rtos h_seg 2 2) "s"))
	(princ "\n calculando envolvente y guardando la lista de aristas")
	(setq h_laryen (|H|GETARS) g_lar (car h_laryen) g_lenv (cadr h_laryen) h_sini h_sfin
	      h_tfin (getvar "DATE") h_sfin (* 86400.0 (- h_tfin (fix h_tfin))) h_tiempo (- h_sfin h_sini)
	      h_horas (fix (/ h_tiempo 3600)) h_min (fix (/ (- h_tiempo (* h_horas 3600)) 60)) h_seg (- h_tiempo (* h_horas 3600) (* h_min 60))
	)
	(|H|DIBENV)
	(princ (strcat "\nenvolvente y arístas calculadas en " (itoa h_horas) "h" (itoa h_min) "m" (rtos h_seg 2 2) "s"))
)

;obtener la distancia media entre las aristas a procesar y el centro de la nube de puntos
;"|H|OBTDMED"
(defun |H|OBTDMED (h_laris h_cnp)
	(setq h_ldis '())
	(foreach h_ari h_laris
		(setq h_ptm (|K|POLAR (car h_ari) (|K|ANG (car h_ari) (cadr h_ari)) (/ (|K|DISTH (car h_ari) (cadr h_ari)) 2))
		      h_ldis (cons (|K|DISTH h_ptm h_cnp) h_ldis)
		)
	)
	(setq h_mm (|L|M&M h_ldis))
	(/ (+ (car h_mm) (cadr h_mm)) 2)
)

;COLI: si el punto pertenece a una arita existente
;"|H|COLI"
(defun |H|COLI (h_ari h_ptp)
	(setq h_ltriar (|H|FINDTRI g_ltri h_ari) h_ptri (car h_ltriar) h_stri (cadr h_ltriar))
	(cond
		(h_stri
			(setq h_pop1 (car (|L|LIBRES h_ptri h_ari nil)) h_pop2 (car (|L|LIBRES h_stri h_ari nil))
			      h_nla (list (list h_ptp (car h_ari)) (list h_ptp (cadr h_ari)) (list h_ptp h_pop1) (list h_ptp h_pop2))
			      h_nlt (list (list h_ptp (car h_ari) h_pop1)
					    (list h_ptp (cadr h_ari) h_pop1)
					    (list h_ptp (car h_ari) h_pop2)
					    (list h_ptp (cadr h_ari) h_pop2)
				      )
			      g_lar (append (|L|BORRA h_ari (|L|BORRA (reverse h_ari) g_lar)) h_nla)
			      g_ltri (append (|L|BORRA h_ptri (|L|BORRA h_stri g_ltri)) h_nlt)
			)
		)
		(T
			(setq h_pop1 (car (|L|LIBRES h_ptri h_ari nil))
			      h_nla (list (list h_ptp (car h_ari)) (list h_ptp (cadr h_ari)))
			      h_nlt (list (list h_ptp (car h_ari) h_pop1)
					    (list h_ptp (cadr h_ari) h_pop1)
				      )
			      g_lar (cons (list h_ptp h_pop1) g_lar)
			      g_lenv (car (|A|ORDENASEG (append (|L|BORRA h_ari (|L|BORRA (reverse h_ari) g_lenv)) h_nla)))
			      g_ltri (append (|L|BORRA h_ptri g_ltri) h_nlt)
			)
		)
	)
)

;AGPT: agregar un punto dentro del MDT
;"|H|AGPTDE"
(defun |H|AGPTDE (h_npt h_tri / h_ar1 h_ar2 h_ar3)
	(setq h_ar1 (list (car h_tri) (cadr h_tri)) h_ar2 (list (cadr h_tri) (caddr h_tri)) h_ar3 (list (caddr h_tri) (car h_tri)))
	(cond
		((|K|COLINEAL (|K|XY h_npt) (|K|XY (car h_ar1)) (|K|XY (cadr h_ar1)))
			(|H|COLI h_ar1 h_npt)
	 	)
		((|K|COLINEAL (|K|XY h_npt) (|K|XY (car h_ar2)) (|K|XY (cadr h_ar2)))
			(|H|COLI h_ar2 h_npt)
		)
		((|K|COLINEAL (|K|XY h_npt) (|K|XY (car h_ar3)) (|K|XY (cadr h_ar3)))
			(|H|COLI h_ar3 h_npt)
		)
		(T
			(setq h_nlar (list (list h_npt (car h_tri)) (list h_npt (cadr h_tri)) (list h_npt (caddr h_tri)))
			      g_lar (append g_lar (|L|LIBRES h_nlar g_lenv T))
			      g_ltri (append (|L|BORRA h_tri g_ltri) (list (cons h_npt h_ar1) (cons h_npt h_ar2) (cons h_npt h_ar3)))
			)
		)
	)
)

;"|H|AGPTFE"
(defun |H|AGPTFE (h_lenv h_npt h_pos)
	(defun |H|TRIVAL (h_are h_ptn)
		(cond
			((or (> (|K|DISTH (car h_are) h_ptn) 250.0) (> (|K|DISTH (cadr h_are) h_ptn) 250.0))
				nil
			)
			((|K|COLINEAL h_ptn (car h_are) (cadr h_are))
				nil
			)
			((|H|DELAUNAY (|K|CIRCULO h_ptn (car h_are) (cadr h_are)) g_lptsm (list h_ptn (car h_are) (cadr h_are)))
				T
			)
			(T nil)
		)
	)

	(cond
		((null h_lenv)
			nil
		)
		((|H|TRIVAL (car h_lenv) h_npt)
			(cons (car h_lenv) (|H|AGPTFE (cdr h_lenv) h_npt (1+ h_pos)))
		)
		(T
			(|H|AGPTFE (cdr h_lenv) h_npt (1+ h_pos))
		)
	)
)

;"|H|FORZARTRI"
(defun |H|FORZARTRI (h_lenvo h_npt / h_ldpm h_ar h_pos)
	(defun |H|DPM (h_lenv h_pt)
		(cond
			((null h_lenv) nil)
			(T
				(cons (cons (|K|DISTH (|GC|PTM (car h_lenv)) h_pt) (car h_lenv)) (|H|DPM (cdr h_lenv) h_pt))
			)
		)
	)

	(setq h_ldpm (|L|ORDENAR (|H|DPM h_lenvo h_npt)) h_ar (cdar h_ldpm))
	(setq h_pos (|L|POS h_ar (car g_lenv))
	      g_lar (cons h_ar g_lar) h_divl (|L|DIVL g_lenv h_pos)
	      g_lenv (append (car h_divl) (list (list (car h_ar) h_npt) (list h_npt (cadr h_ar))) (cdadr h_divl))
	)
	(list (append h_ar (list h_npt)))  
)

(defun |H|VERIFT (h_ntri h_pn)
	(setq h_ptr (car h_ntri) h_utr (last h_ntri) 
	      h_nenv (list (reverse (|L|BULT h_ptr)) (list (car h_utr) (caddr h_utr)))
	      g_lenv (car (|A|ORDENASEG (append g_lenv h_nenv)))
	)
	(foreach h_ar h_pme
		(setq h_tra (cons h_pn h_ar))
		(if (not (member h_tra h_ntri))
			(setq h_ntri (cons h_tra h_ntri))
		)
	)
	(setq g_ltri (append g_ltri h_ntri) g_lar (append g_lar (|L|LIBRES (car (|H|GETARS h_ntri)) h_nenv T)))
)

;"|H|NEWPT"
(defun |H|NEWPT (h_pt / h_trib h_nltri)
	(cond
		((null h_pt) nil)
		((setq h_trib (|H|BUSCTRI g_ltri h_pt))
			(|H|AGPTDE h_pt h_trib)
		)
		(T
			(setq h_nltri (|H|AGPTFE (car g_lenv) h_pt 0))
			(if h_nltri
				(|H|VERIFT h_nltri h_pt)
				(setq g_ltri (append g_ltri (|H|FORZARTRI (car g_lenv) h_pt)))
			)
		)
	)
)

;FLIP: cambiar aristas
;"|H|FLIP"
(defun |H|FLIP ()
	(defun |H|SELAR ()
		(setq h_resp (|S|SELAR "\nseleccione la arista a cambiar"))
		(cond
			((eq (type h_resp) 'ENAME)
				(setq h_ear (entget h_resp) h_lay (|O|ITEM 8 h_ear))
				(if (eq h_lay "T_Aristas")
					h_resp
				  (progn
					(princ "\n no es una arista")
				  	(|H|SELAR)
				  )
				)
			)
			((eq (type h_resp) 'INT)
				(if (eq (strcase (chr h_resp)) "U")
				  (progn
					(entdel (entlast)) (entdel h_near) (redraw)
					(setq g_ltri (|L|REEMP h_ntr2 h_tr2 (|L|REEMP h_ntr1 h_tr1 g_ltri))
				  	      g_lar (|L|REEMP (list h_pn1 h_pn2) h_ar g_lar)
					)
				  	(|H|SELAR)
				  )
					nil
				)
			)
			(T nil)
		)
	)
	(|H|BORRA)
	(if (not g_lar) (setq g_laryenv (|H|GETARS g_ltri) g_lar (car g_laryenv) g_lenv (cadr g_laryenv)))
  	(setq h_nlar '())
	(|H|DIBAR g_lar)
	(while (setq h_near (|H|SELAR))
		(setq h_ear (entget h_near))
		(setq h_ar (list (|O|ITEM 10 h_ear) (|O|ITEM 11 h_ear))
		      h_trip (|H|FINDTRI g_ltri h_ar) h_tr1 (car h_trip) h_tr2 (cadr h_trip)
		      h_pn1 (car (|L|LIBRES h_tr1 h_ar nil)) h_pn2 (car (|L|LIBRES h_tr2 h_ar nil))
		)
		(if (inters (|K|XY (car h_ar)) (|K|XY (cadr h_ar)) (|K|XY h_pn1) (|K|XY h_pn2))
		  (progn
			(entdel h_near) (|O|LINE h_pn1 h_pn2  "T_Aristas") (redraw)
			(setq h_ntr1 (list h_pn1 h_pn2 (car h_ar)) h_ntr2 (list h_pn2 (cadr h_ar) h_pn1)
			      g_ltri (|L|REEMP h_tr2 h_ntr2 (|L|REEMP h_tr1 h_ntr1 g_ltri))
		  	      g_lar (|L|REEMP h_ar (list h_pn1 h_pn2) g_lar)
			)
		  )
			(princ "\n no se puede cambiar esta arista por el momento")
		)
	)
;;;	(foreach h_dar h_nlar
;;;		(setq h_nar (car h_dar) h_var (cadr h_dar)
;;;		      h_trip (|H|FINDTRI g_ltri h_var) h_tr1 (car h_trip) h_tr2 (cadr h_trip)
;;;		      h_pn1 (car (|L|LIBRES h_tr1 h_var nil)) h_pn2 (car (|L|LIBRES h_tr2 h_var nil))
;;;		      h_ntr1 (list h_pn1 h_pn2 (car h_nar)) h_ntr2 (list h_pn2 (cadr h_nar) h_pn1)
;;;		      g_ltri (|L|REEMP h_tr2 h_ntr2 (|L|REEMP h_tr1 h_ntr1 g_ltri))
;;;		      g_lar (|L|REEMP h_var h_nar g_lar)
;;;		)
;;;	)
	(|H|DELAR)
	(|H|DIB g_ltri) (|H|GUARDA)
)

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;				C	U	R	V	A	S		D	E		N	I	V	E	L
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(setq cn_lordg nil cn_lseglg nil)
;"|CN|ELARDUP"
(defun |CN|ELARDUP (cn_ari cn_laris)
	(cond
		((null cn_laris) (list cn_ari))
		((equal (car cn_ari) (cadr cn_ari) 0.001)
			(princ "\n arista nula")
			(|CN|ELARDUP (car cn_laris) (cdr cn_laris))
		)
      		((apply 'or (mapcar '(lambda (cn_el) (and (member (car cn_el) cn_ari) (member (cadr cn_el) cn_ari))) cn_laris))
			(princ "\n arista duplicada")
			(|CN|ELARDUP (car cn_laris) (cdr cn_laris))
		)
		(T (cons cn_ari (|CN|ELARDUP (car cn_laris) (cdr cn_laris))))
	)
)

;"|CN|GENPTS"
(defun |CN|GENPTS (cn_lcn)
	(defun |CN|OBTPTS (cn_lpts cn_el cn_pa)
		(cond
			((null cn_lpts) nil)
			((null cn_pa) (cons (|K|Z+ (car cn_lpts) cn_el) (|CN|OBTPTS (cdr cn_lpts) cn_el (car cn_lpts))))
			((= (length cn_lpts) 1) (cons (|K|Z+ (car cn_lpts) cn_el) (|CN|OBTPTS (cdr cn_lpts) cn_el (car cn_lpts)))) 
			(T
				(setq cn_pt (car cn_lpts) cn_pp (cadr cn_lpts) cn_an (|N|A3P cn_pt cn_pa cn_pp)
				      cn_dia (|K|DISTH cn_pa cn_pt) cn_dip (|K|DISTH cn_pt cn_pp)
				)
				(cond
					((> cn_dia 1.0) (cons (|K|Z+ cn_pt cn_el) (|CN|OBTPTS (cdr cn_lpts) cn_el cn_pt)))
;;;					((< cn_an (|N|GAR 120)) (cons (|K|Z+ cn_pt cn_el) (|CN|OBTPTS (cdr cn_lpts) cn_el cn_pt)))
					(T (|CN|OBTPTS (cdr cn_lpts) cn_el cn_pa))
				)
			)
		)
	)

	(cond
		((null cn_lcn) nil)
		(T
			(setq cn_dcn (car cn_lcn) cn_elev (car cn_dcn) cn_lseg (cadr cn_dcn))
			(cond
				((> (length cn_lseg) 2)
					(append (|CN|OBTPTS (|A|LISTAV cn_lseg) cn_elev nil) (|CN|GENPTS (cdr cn_lcn)))
				)
				(T
					(append (list (|K|Z+ (caar cn_lseg) cn_elev) (|K|Z+ (cadar cn_lseg) cn_elev) (|K|Z+ (cadadr cn_lseg) cn_elev))
						(|CN|GENPTS (cdr cn_lcn))
					)
				)
			)
		)
	)
)

;"|CN|PROCTRI"
(defun |CN|PROCTRI (cn_ltri cn_plh cn_cota)
	(cond
		((null cn_ltri) nil)
		(T
			(setq cn_tri (car cn_ltri) cn_v1 (car cn_tri) cn_v2 (cadr cn_tri) cn_v3 (caddr cn_tri))
			(cond
				((< cn_cota (min (caddr cn_v1) (caddr cn_v2) (caddr cn_v3)))
					(|CN|PROCTRI (cdr cn_ltri) cn_plh cn_cota)
				)
				((> cn_cota (max (caddr cn_v1) (caddr cn_v2) (caddr cn_v3)))
					(|CN|PROCTRI (cdr cn_ltri) cn_plh cn_cota)
				)
				(T
					(setq cn_p1 (|GC|INTRP (list cn_v1 cn_v2) cn_plh)
					      cn_p2 (|GC|INTRP (list cn_v2 cn_v3) cn_plh)
					      cn_p3 (|GC|INTRP (list cn_v3 cn_v1) cn_plh)
					)
					(cond
						((and (|GC|ENSR cn_p1 (list cn_v1 cn_v2)) (|GC|ENSR cn_p2 (list cn_v2 cn_v3)) (not (equal cn_p1 cn_p2 gc_tol)))
							(cons (list cn_p1 cn_p2) (|CN|PROCTRI (cdr cn_ltri) cn_plh cn_cota))
						)
						((and (|GC|ENSR cn_p1 (list cn_v1 cn_v2)) (|GC|ENSR cn_p3 (list cn_v3 cn_v1)) (not (equal cn_p1 cn_p3 gc_tol)))
							(cons (list cn_p1 cn_p3) (|CN|PROCTRI (cdr cn_ltri) cn_plh cn_cota))
						)
						((and (|GC|ENSR cn_p2 (list cn_v2 cn_v3)) (|GC|ENSR cn_p3 (list cn_v3 cn_v1)) (not (equal cn_p2 cn_p3 gc_tol)))
							(cons (list cn_p2 cn_p3) (|CN|PROCTRI (cdr cn_ltri) cn_plh cn_cota))
						)
						(T
							(|CN|PROCTRI (cdr cn_ltri) cn_plh cn_cota)
						)
					)
				)
			)
		)
	)
)

;"|CN|DIBCN"
(defun |CN|DIBCN (cn_lar cn_lay cn_cota)
	(cond
		((null cn_lar) nil)
		(T
			(setq cn_llcn (|A|ORDENASEG cn_lar) cn_pol (|L|PURGAR (|A|LISTAV (car cn_llcn))) cn_lar (cadr cn_llcn)
			      cn_ani (|K|ANG (car cn_pol) (cadr cn_pol)) cn_anf (|K|ANG (last (|L|BULT cn_pol)) (last cn_pol))
			      cn_lays (strcat cn_lay "S") cn_layt (strcat cn_lay "T")
			)
			(if (|N|ENTRE cn_ani (list (/ PI 2) (* pi 1.5))) (setq cn_ani (- cn_ani PI) cn_alhi 0) (setq cn_alhi 2))
			(if (|N|ENTRE cn_anf (list (/ PI 2) (* pi 1.5))) (setq cn_anf (- cn_anf PI) cn_alhf 2) (setq cn_alhf 0))
			(cond
				((equal (car cn_pol) (last cn_pol) 0.001)
					(setq cn_tc 129 cn_pol (|L|BULT cn_pol) cn_pm (|K|PMED (car cn_pol) (cadr cn_pol)))
					(|O|CNIVELS cn_pol cn_lays cn_tc cn_cota) (redraw)
					(|O|CNIVEL cn_pol cn_lay cn_tc cn_cota) (redraw)
					(|O|TEXT cn_pm cn_pm 0.5 (rtos cn_cota 2 2) cn_ani 0.8 0 "ROMANS" 0 1 2 cn_layt)
				)
				(T
					(setq cn_tc 128)
					(|O|CNIVELS cn_pol cn_lays cn_tc cn_cota) (redraw)
					(|O|CNIVEL cn_pol cn_lay cn_tc cn_cota) (redraw)
					(|O|TEXT (car cn_pol) (car cn_pol) 0.5 (rtos cn_cota 2 2) cn_ani 0.8 0 "ROMANS" 0 cn_alhi 0 cn_layt)
					(|O|TEXT (last cn_pol) (last cn_pol) 0.5 (rtos cn_cota 2 2) cn_anf 0.8 0 "ROMANS" 0 cn_alhf 0 cn_layt)
				)
			)
			(|CN|DIBCN cn_lar cn_lay cn_cota)
		)
	)
)

;"|CN|BUSCARCN"
(defun |CN|BUSCARCN (cn_cpri cn_ci cn_cf cn_deltap cn_deltas)
	(cond
		((> cn_ci cn_cf) nil)
		(T
			(princ (strcat "\n calculando curvas en cota " (rtos cn_ci 2 2)))
			(setq cn_pla (list (list 0 0 cn_ci) (list 1 0 cn_ci) (list 0 1 cn_ci))
			      cn_larcn (|CN|PROCTRI g_ltri cn_pla cn_ci)
			      cn_larcn (|CN|ELARDUP (car cn_larcn) (cdr cn_larcn))
			)
			(if (equal cn_cpri cn_ci 0.001)
				(setq cn_lay "CN_Primarias" cn_cpri (+ cn_cpri cn_deltap))
				(setq cn_lay "CN_Secundarias")
			)
			(|CN|DIBCN cn_larcn cn_lay cn_ci)
			(|CN|BUSCARCN cn_cpri (+ cn_ci cn_deltas) cn_cf cn_deltap cn_deltas)
		)
	)
)

;"|CN|BORRA"
(defun |CN|BORRA ()
	(|O|DELENTS (|L|SSL (ssget "X" '((-4 . "<XOR") (8 . "CN_Primarias") (8 . "CN_Secundarias") (-4 . "XOR>")))))
	(|O|DELENTS (|L|SSL (ssget "X" '((-4 . "<XOR") (8 . "CN_PrimariasS") (8 . "CN_SecundariasS") (-4 . "XOR>")))))
	(|O|DELENTS (|L|SSL (ssget "X" '((-4 . "<XOR") (8 . "CN_PrimariasT") (8 . "CN_SecundariasT") (-4 . "XOR>")))))
)

;"|CN|CARGA"
(defun |CN|CARGA ()
	(setq cn_lnecn (|L|SSL (ssget "X" '((-4 . "<XOR") (8 . "CN_Primarias") (8 . "CN_Secundarias") (-4 . "XOR>"))))
	      cn_lcn '() cn_lpcn '() cn_lpcnc '() cn_cpt 10000
	)
	(foreach cn_necn cn_lnecn
		(setq cn_cn (entget cn_necn) cn_elev (|O|ITEM 38 cn_cn) cn_plcn (|A|LISTAPOL cn_necn)
		      cn_lcn (cons (list cn_elev cn_plcn) cn_lcn)
		)
		(foreach cn_pt (|A|LISTAV cn_plcn)
			(setq cn_lpcnc (cons (list cn_cpt (|K|Z+ (|K|PTATOP cn_pt) cn_elev) "CN") cn_lpcnc))
			(setq cn_cpt (1+ cn_cpt))
		)
	)
	(|F|GUARDACSV g_path "Puntos Curvas de Nivel" nil (reverse cn_lpcnc) T)
	(princ (strcat (itoa (length cn_lcn)) " curvas de nivel cargadas\n"))
	cn_lcn
)

;"|CN|DIB"
(defun |CN|DIB (cn_lcn cn_dcp)
	(cond
		((null cn_lcn) nil)
		(T
			(setq cn_cota (caar cn_lcn) cn_cn (|A|LISTAV (cadar cn_lcn)))
			(if (= (rem cn_cota cn_dcp) 0)
				(setq cn_lay "CN_Primarias")
				(setq cn_lay "CN_Secundarias")
			)
			(|O|CNIVELS cn_cn cn_lay 0 cn_cota) (redraw)
			(|O|TEXT (car cn_cn) (car cn_cn) 0.5 (rtos cn_cota 2 2) 0.0 0.8 0 "ROMANS" 0 0 0 (strcat cn_lay "T"))
			(|O|TEXT (last cn_cn) (last cn_cn) 0.5 (rtos cn_cota 2 2) 0.0 0.8 0 "ROMANS" 0 0 0 (strcat cn_lay "T"))
			(|CN|DIB (cdr cn_lcn) cn_dcp)
		)
	)
)

(defun |CN|SELECT (cn_pi cn_pf)
	(setq cn_lnecn (|L|SSL (ssget "F" (List cn_pi cn_pf) '((-4 . "<XOR") (8 . "CN_Primarias") (8 . "CN_Secundarias") (-4 . "XOR>"))))
	      cn_lcn '()
	)
	(foreach cn_necn cn_lnecn
		(setq cn_cn (entget cn_necn) cn_elev (|O|ITEM 38 cn_cn) cn_plcn (|A|LISTAPOL cn_necn)
		      cn_lcn (cons (list cn_elev cn_plcn) cn_lcn)
		)
	)
	cn_lcn
)

(defun |CN|CCN()
	(setq cn_echo (getvar "cmdecho") cn_omod (getvar "osmode") cn_bm (getvar "blipmode"))  
	(setvar "cmdecho" 0) (setvar "osmode" 0) (setvar "blipmode" 0)
	(setq cn_sscn (ssget "X" '((-4 . "<AND") (0 . "LWPOLYLINE") (-4 . "<XOR") (8 . "CN_Primarias") (8 . "CN_Secundarias") (-4 . "XOR>") (-4 . "AND>"))) cn_i 0)
	(while (setq cn_cnn (ssname cn_sscn (- (setq cn_i (1+ cn_i)) 1)))
		(setq cn_cn (entget cn_cnn) cn_cota (|O|ITEM 38 cn_cn) cn_lcn (|A|LISTAPOL cn_cnn) cn_ppar (car cn_lcn) cn_upar (last cn_lcn)
			cn_ani (angle (car cn_ppar) (cadr cn_ppar)) cn_anf (angle (car cn_upar) (cadr cn_upar)) 
		)
		(if (and (> cn_ani (/ PI 2)) (< cn_ani (* pi 1.5))) (setq cn_ani (- cn_ani PI) cn_alhi 0) (setq cn_alhi 2))
		(if (and (> cn_anf (/ PI 2)) (< cn_anf (* pi 1.5))) (setq cn_anf (- cn_anf PI) cn_alhf 2) (setq cn_alhf 0))
		(if (eq (|O|ITEM 70 cn_cn) 129)
		  (progn
		  	(setq cn_pm (polar (car cn_ppar) (angle (car cn_ppar) (cadr cn_ppar)) (/ (distance (car cn_ppar) (cadr cn_ppar)) 2)))
			(|O|TEXT cn_pm cn_pm 0.5 (rtos cn_cota 2 2) cn_ani 0.8 0 "ROMANS" 0 1 2 (strcat (|O|ITEM 8 cn_cn) "T"))
		  )
		  (progn
			(|O|TEXT (car cn_ppar) (car cn_ppar) 0.5 (rtos cn_cota 2 2) cn_ani 0.8 0 "ROMANS" 0 cn_alhi 0 (strcat (|O|ITEM 8 cn_cn) "T"))
			(|O|TEXT (cadr cn_upar) (cadr cn_upar) 0.5 (rtos cn_cota 2 2) cn_anf 0.8 0 "ROMANS" 0 cn_alhf 0 (strcat (|O|ITEM 8 cn_cn) "T"))
		  )
		)
	)
	(setvar "cmdecho" cn_echo) (setvar "osmode" cn_omod) (setvar "blipmode" cn_bm)
)
;g_lcn lterr