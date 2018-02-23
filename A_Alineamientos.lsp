;---------------------------------------------------------------------------------------------------------------------------------------------------------
;							L	W	P	O	L	Y	L	I	N	E	S
;---------------------------------------------------------------------------------------------------------------------------------------------------------
; Devolver una lista con los vértices de una LWPOLYLINE
;"|A|LISVER"
(defun |A|LISVER (a_lent)
	(cond
		((null a_lent) nil)
		((not (assoc 10 a_lent)) nil)
		(T (cons (|O|ITEM 10 a_lent) (|A|LISVER (cdr (member (assoc 10 a_lent) a_lent)))))
	)
)

;"|A|LVPOLI"
(defun |A|LVPOLI (a_npol)
	(cond
		((null a_npol) nil)
		(T (|A|LISVER (entget a_npol)))
	)
)

;devolver una lista de segmentos de una poligonal
;"|A|LISSEG"
(defun |A|LISSEG (a_lpol)
	(cond
		((null a_lpol) nil)
		((= (length a_lpol) 1) nil)
		((equal (car a_lpol) (cadr a_lpol) g_tol) (|A|LISSEG (cdr a_lpol)))
		(T (cons (list (car a_lpol) (cadr a_lpol)) (|A|LISSEG (cdr a_lpol))))
	)
)

; Crear la lista con los segmentos de una poligonal cualquiera
;"|A|LISTAPOL"
(defun |A|LISTAPOL (a_npol)
	(cond
		((null a_npol) nil)
;;;		(T (|A|LISSEG (|L|BULT (|A|LISVER (entget a_npol)))))
		(T (|A|LISSEG (|A|LISVER (entget a_npol))))
	)
)

; Devolver la lista con los vértices de una lista de segmentos
;"|A|LISTAV"
(defun |A|LISTAV (a_lseg)
	(cond ((< (length a_lseg) 2) (car a_lseg))
		(t
			(cons (caar a_lseg) (|A|LISTAV (cdr a_lseg)))
		)
	)
)

; Devolver los puntos de una poligonal que caen entre dos puntos dados 
;"|A|LPINT"
(defun |A|LPINT (a_lseg a_pt1 a_pt2 / a_ppi a_upi a_posi a_posf)
;;;	(cond
;;;		((null a_lseg) nil)
;;;		((null a_pt1) (|A|LPINT a_lseg (caar a_lseg) a_pt2))
;;;		((null a_pt2) (|A|LPINT a_lseg a_pt1 (cadr (last a_lseg))))
;;;		
;;;	)
	(if (> (car a_pt1) (car a_pt2)) (setq a_pau a_pt1 a_pt1 a_pt2 a_pt2 a_pau))
	(setq a_lseg1 (|A|PRALIN a_lseg 0.0) a_pri (car (|A|IDP a_pt1 a_lseg1 nil)) a_prf (car (|A|IDP a_pt2 a_lseg1 nil))
	      a_lsegi (|L|BULTSL (|L|BULTSL (|A|SEGINT a_lseg1 a_pri a_prf)))
	)
	(cdr (|A|LISTAV a_lsegi))
)

;---------------------------------------------------------------------------------------------------------------------------------------------------------

;								S	E	G	M	E	N	T	O	S

;---------------------------------------------------------------------------------------------------------------------------------------------------------
; Devolver el segmento en que cae un punto en un alineamiento
;"|A|SEGP"
(defun |A|SEGP (a_pt a_pol)
	(cond
		((null a_pol) nil)
		((and (|GC|COLINEAL (caar a_pol) (cadar a_pol) a_pt 0.01) (|N|ENTRE (|GC|PARAM (car a_pol) a_pt) (list 0 1))) (car a_pol))
		(T (|A|SEGP a_pt (cdr a_pol)))
	)
)

; Devolver el segmento en que cae una progresiva
;"|A|SEG"
(defun |A|SEG (a_pr a_alin)
	(cond
		((null a_alin) nil)
		((>= a_pr (caddr (last a_alin))) (last a_alin))
		((<= a_pr (caddar a_alin)) (car a_alin))
		(T (|A|SEG a_pr (cdr a_alin)))
	)
)

; Devolver el segmento que comienza en el punto dado
;"|A|PSEG"
(defun |A|PSEG (a_pto a_alin)
	(nth (|L|POSSL a_pto a_alin 0) a_alin)
)

; Devolver el segmento que termina en el punto dado
;"|A|ASEG"
(defun |A|ASEG (a_pto a_alin / a_c a_laux a_laux1)
	(nth (|L|POSSL a_pto a_alin 1) a_alin)
)

;invertir la direccción de un segmento
;"|A|INVSEG"
(defun |A|INVSEG (a_seg)
	(cond
		((eq (last a_seg) "A")
			(list (cadr a_seg) (car a_seg) (caddr a_seg) (- (cadddr a_seg)) (car (cddddr a_seg)) "A")
		)
		((eq (last a_seg) "R")
			(list (cadr a_seg) (car a_seg) nil "R"))
		(T (list (cadr a_seg) (car a_seg) nil nil))
	)
)

;unir segmentos según sus puntos extremos
;"|A|UNESEG"
(defun |A|UNESEG (a_lord a_lsegm a_lsegl a_inv)
	(cond
		((null a_lsegm)
			(if a_inv
				(list (|A|CDIRALI a_lord) a_lsegl)
				(list a_lord a_lsegl)
			)
		)
		((equal (caar a_lord) (cadr (last a_lord)) 0.01)
			(list a_lord (append a_lsegm a_lsegl))
		)
		((equal (cadr (last a_lord)) (caar a_lsegm) 0.01)
			(|A|UNESEG (append a_lord (list (car a_lsegm))) (cdr a_lsegm) a_lsegl a_inv)
		)
		((equal (caar a_lord) (caar a_lsegm) 0.01)
			(|A|UNESEG (append (list (|A|INVSEG (car a_lsegm))) a_lord) (cdr a_lsegm) a_lsegl T)
		)
		((equal (cadr (last a_lord)) (cadar a_lsegm) 0.01)
			(|A|UNESEG (append a_lord (list (|A|INVSEG (car a_lsegm)))) (cdr a_lsegm) a_lsegl a_inv)
		)
		((equal (caar a_lord) (cadar a_lsegm) 0.01)
			(|A|UNESEG (append (list (car a_lsegm)) a_lord) (cdr a_lsegm) a_lsegl T)
		)
		(T
			(|A|UNESEG a_lord (cdr a_lsegm) (append a_lsegl (list (car a_lsegm))) a_inv)
		)
	)
)

;"|A|ORDSEG"
(defun |A|ORDSEG (a_lord a_lseg)
	(setq a_lres (|A|UNESEG a_lord a_lseg nil nil) a_lord (car a_lres) a_lseg (cadr a_lres))
	(if a_lseg
		(progn
			(setq a_pi (caar a_lord) a_pf (cadr (last a_lord)))
			(if (not (equal a_pi a_pf g_tol))
				(if (apply 'or (mapcar '(lambda (a_el)
							  (or (equal a_pi (car a_el) 0.01)
							      (equal a_pi (cadr a_el) 0.01)
							      (equal a_pf (car a_el) 0.01)
							      (equal a_pf (cadr a_el) 0.01)
							   )
							)
						       a_lseg)
					)
					(|A|ORDSEG a_lord a_lseg)
					(list a_lord a_lseg)
				)
				(list a_lord a_lseg)
			)
		)
		(list a_lord nil)
	)
)

;ordenar una lista de segmentos
;"|A|ORDENASEG"
(defun |A|ORDENASEG (a_lseg)
	(cond
		((null a_lseg) nil)
		((= (length a_lseg) 1) (list a_lseg))
		(T 
			(|A|ORDSEG (list (car a_lseg)) (cdr a_lseg))
		)
	)
)

; devolver una lista para un rumbo
;"|A|LISREC"
(defun |A|LISREC (a_pp a_sp)
	(list a_pp a_sp -1)
)

; devolver una lista para una curva circular
;"|A|LISCUR"
(defun |A|LISCUR (a_pp a_sp a_delta a_rc / a_pm a_cg)
;;;	(cond
;;;		((> (abs a_delta) PI)
;;;			(setq a_cg (|C|CENTRO a_delta a_rc a_pp a_sp) a_delta (/ a_delta 2)
;;;				a_pm (polar a_cg (- (angle a_cg a_pp) a_delta) a_rc)
;;;			)
;;;			(list (list a_pp a_pm nil a_delta a_rc "A") (list a_pm a_sp a_delta a_rc "A"))
;;;		)
;;;		(T (list a_pp a_sp -1 a_delta a_rc "A"))
;;;	)
	(list (list a_pp a_sp -1 a_delta a_rc "A"))
)

;convertir una entidad arco en un segmento de curva
;"|A|ARC2SEG"
(defun |A|ARC2SEG (a_earc)
	(setq a_cen (|O|ITEM 10 a_earc) a_rc (|O|ITEM 40 a_earc) a_ai (|O|ITEM 50 a_earc) a_af (|O|ITEM 51 a_earc))
	(setq a_delta (|C|DELTA nil a_ai a_af))
	(list (|L|BULT (|K|POLAR a_cen a_ai a_rc)) (|L|BULT (|K|POLAR a_cen a_af a_rc)) nil a_delta a_rc "A")
)

; devolver una lista para una curva espiral
;"|A|LISESE"
(defun |A|LISESE (a_pp a_sp a_le a_rc a_delta)
	nil
)

;devolver la fracción de un alineamiento entre dos progresivas
;"|A|SUBAL"
(defun |A|SEGINT (a_lalin a_pri a_prf)
	(cond
		((null a_lalin) nil)
		((> (caddr (car a_lalin)) a_prf) nil)
		((< (caddr (car a_lalin)) a_pri) (|A|SEGINT (cdr a_lalin) a_pri a_prf))
		(T (cons (car a_lalin) (|A|SEGINT (cdr a_lalin) a_pri a_prf)))
	)
)
(defun |A|SUBAL (a_alin a_pri a_prf)
	(append (list (|A|SEG a_pri a_alin)) (|A|SEGINT a_alin a_pri a_prf) (list a_ults (|A|SEG a_prf a_alin)))
)


;---------------------------------------------------------------------------------------------------------------------------------------------------------

;						A	L	I	N	E	A	M	I	E	N	T	O	S

;---------------------------------------------------------------------------------------------------------------------------------------------------------
;							L	W	P	O	L	Y	L	I	N	E	S
;---------------------------------------------------------------------------------------------------------------------------------------------------------
;devolver un alineamiento a partir de una polilinea
;"|A|LISTALPL"
(defun |A|LISTALPL (a_nalin)
	(cond
		((null a_nalin) nil)
		(T (|A|PRALIN (|S|LISSEG (|L|BULT (|S|LISVER (entget a_nalin)))) 0.0))
	)
)

;---------------------------------------------------------------------------------------------------------------------------------------------------------
;						A	L	I	N	E	A	M	I	E	N	T	O	S
;---------------------------------------------------------------------------------------------------------------------------------------------------------
;seleccionar un alineamiento de una lista de alineamientos
;"|A|SELALI"
(defun |A|SELALI ()
	(if g_ldal
		(progn
			(textscr)
			(foreach a_dal g_ldal
				(princ (strcat "\n" (itoa (|O|ITEM 0 a_dal)) " - " (|O|ITEM 1 a_dal)))
			)
			(setq a_numa (|N|GETINT (|O|ITEM 0 g_alact) "\n número de alineamiento:"))
			(graphscr)
			(|G|GETAL a_numa g_ldal)
		)
		nil
	)
)

;poner progresivas finales a los segmentos de un alineamiento
;"|A|PRALIN"
(defun |A|PRALIN (a_lalin a_pri)
	(cond
		((null a_lalin) nil)
		((null a_pri) (|A|PRALIN a_lalin 0.0))
		(T (setq a_pseg (car a_lalin) a_pp (car a_pseg) a_sp (cadr a_pseg) a_tip (last a_pseg))
			(cond
				((eq a_tip "A")
					(setq a_delta (cadddr a_pseg) a_prfs (|C|PRFC a_delta a_pp a_sp a_pri) a_rc (car (cddddr a_pseg)))
					(cons (list a_pp a_sp a_prfs a_delta a_rc a_tip) (|A|PRALIN (cdr a_lalin) a_prfs))
				)
				((or (eq a_tip "E") (eq a_tip "S"))
					(setq a_le (cadddr a_pseg) a_tita (nth 4 a_pseg) a_dirt (nth 5 a_pseg) a_prfs (+ a_pri a_le))
					(cons (list a_pp a_sp a_prfs a_le a_tita a_dirt a_tip) (|A|PRALIN (cdr a_lalin) a_prfs))
				)
				(T
					(setq a_prfs (|C|PRFR a_pp a_sp a_pri))
					(cons (list a_pp a_sp a_prfs "R") (|A|PRALIN (cdr a_lalin) a_prfs))
				)
			)
		)
	)
)

;invertir la dirección de un alineamiento
;"|A|CDIRALI"
(defun |A|CDIRALI (a_alin)
	(cond
		((null a_alin) nil)
		((= (length a_alin) 1)
			(list (|A|INVSEG (car a_alin)))
		)
		(T (append (|A|CDIRALI (cdr a_alin)) (list (|A|INVSEG (car a_alin)))))
	)
)

; Crear la lista con los datos del alineamiento
;"|A|CREA"
(defun |A|CREA (a_nomal / a_lalin)
	(princ "\nSeleccione los elementos para el alineamiento: ")
	(setq a_limpan (|S|LIMPAN))
	(if (setq a_prient (|S|SELENT "seleccione una entidad"))
		(progn
;;;			(setq a_prient (car a_lpriseg) a_pref (trans (cadr a_lpriseg) 1 0) a_di 10000.0 a_lnent (list a_prient))
			(redraw a_prient 3)
			(setq a_liscons (car (|A|ORDENASEG (|S|PROCEL (setq a_lnent (|S|RESEL (list a_prient)))))))
			(foreach a_ent a_lnent
				(redraw a_ent 4)
			)
			(setq a_ppt (caar a_liscons) a_upt (cadr (last a_liscons)) a_lnent nil)
;;;			(if (< (distance a_upta a_pref) (distance a_ppta a_pref)) (setq a_ppt a_upta a_liscons (|A|CDIRALI a_liscons)) (setq a_ppt a_ppta))
			(if (|S|DENTRO a_ppt a_limpan) (|K|MARCA a_ppt) (|K|MARCA (car (|K|BPTPS a_ppt (cdr (vports))))))
			(if (eq (strcase (getstring "\nPunto de Inicio: (confirma Si/No)")) "N") (setq a_liscons (|A|CDIRALI a_liscons)))
			(redraw)
			(setq a_lalin (|A|PRALIN a_liscons 0.0))
			(if (not (eq a_nomal "")) (|A|GUARDA a_nomal (|A|POLIGONAL a_lalin)))
			a_lalin
		)
		nil
	)
)

;Guardar los datos de un alineamiento y actualizar la lista de alineamientos
;°|A|GUARDA"
(defun |A|GUARDA (a_nomal a_polal)
	(|F|GUARDACSVA a_nomal "Poligonal" (list (list "PUNTO" "ALFA" "DISTANCIA" "X" "Y" "RADIO" "LEE" "LES")) a_polal T)
	(setq a_path (strcat g_alipath a_nomal "\\") a_lna (mapcar '(lambda (x) (|O|ITEM 1 x)) g_ldal))
  	(if (not (findfile (strcat g_alipath a_nomal "\\" "ExcelVial.xls")))
		(startapp (strcat "cmd /Q /C CD " a_path " & COPY G:\\AcadVial\\ExcelVial.xls"))
	)
	(cond
		((null g_ldal)
			(setq g_ldal (cons (list (cons 0 0) (cons 1 a_nomal) (cons 2 a_polal) (cons 3 (|A|LISTA a_polal nil 0))) g_ldal))
		)
		((member a_nomal a_lna)
			(setq a_pos (|L|POS a_nomal a_lna)
			      a_dal (|O|REEMP 2 a_polal (|G|GETAL a_pos g_ldal))
			      g_ldal (|L|REEMPOS a_pos a_dal g_ldal)
			)
		)
		(T
			(setq g_ldal (append g_ldal (list (list (cons 0 (length g_ldal)) (cons 1 a_nomal) (cons 2 a_polal) (cons 3 (|A|LISTA a_polal nil 0)))))
			      a_lna (append a_lna (list a_nomal))
			)
		)
	)
)

;Crear un alineamiento a partir de su poligonal
;"|A|EJE"
(defun |A|EJE (a_lpol a_pa a_lay)
	(defun |A|CURVA (a_ldc)
		(command "_zoom" "c" a_pto g_ezc)
		(princ "\nProcesando Vértice ") (prin1 a_np) (princ " - Delta = ") (prin1 a_deltas)
		(if a_ldc
			(setq a_rc (car a_ldc) a_lee (cadr a_ldc) a_les (caddr a_ldc))
			(if (setq a_rc (getreal "\nRadio: "))
				(setq a_lee (|N|GETREAL 0.0 "\nLongitud espiral de entrada:") a_les (|N|GETREAL 0.0 "\nLongitud espiral de salida:"))
			  (progn
				(|O|LINE a_pa a_pto a_lay) (redraw)
				(if (not (getstring "\nconfirma?: "))
					(append (list (append (car a_lpol) (list 0 0 0)) (cadr a_lpol)) (|A|EJE (cddr a_lpol) a_pto a_lay))
				  (progn
				  	(entdel (entlast))
					(|A|CURVA nil)
				  )
				)
			  )
			)
		)
		(setq a_lpif (|E|PTIF a_pto a_rc a_lee a_les a_delta (|K|ANG a_pto a_pa) (|K|ANG a_pto a_ppt))
		      a_te (car a_lpif) a_et (cadr a_lpif) a_titae (/ a_lee (* a_rc 2)) a_titas (/ a_les (* a_rc 2))
		      a_deltac ((|N|SIGNO a_delta) (- a_deltaa (+ a_titae a_titas))) a_lent '()
		)
		(|O|LINE a_pa a_te a_lay) (redraw)
		(setq a_lent (cons (entlast) a_lent))
		(if (> a_lee 0)
		  (progn
			(setq a_ee (|E|REPPE a_te a_rc a_lee (|K|ANG a_pa a_pto) (|N|SIGNO a_delta) 100) a_ec (last a_ee))
		  	(|O|LWPOLY a_ee a_lay 128) (redraw)
			(setq a_lent (cons (entlast) a_lent))
		  )
			(setq a_ec a_te)
		)
		(if (> a_les 0)
		  (progn
			(setq a_es (|E|REPPE a_et a_rc a_les (|K|ANG a_ppt a_pto) (|N|-SIGNO a_delta) 100) a_ce (last a_es))
		  	(|O|LWPOLY a_es a_lay 128) (redraw)
			(setq a_lent (cons (entlast) a_lent))
		  )
			(setq a_ce a_et)
		)
		(setq a_cgc (|C|CENTRO a_deltac a_rc a_ec a_ce) a_ai (|K|ANG a_cgc a_ec) a_af (|K|ANG a_cgc a_ce))
		(if (minusp a_delta)
			(|O|ARC a_cgc a_rc a_ai a_af a_lay)
			(|O|ARC a_cgc a_rc a_af a_ai a_lay)
		) (redraw)
		(setq a_lent (cons (entlast) a_lent))
		(if (not (getstring "\nconfirma?: "))
			(append (|L|SUBL (car a_lpol) 5) (list a_rc a_lee a_les))
		  (progn
		  	(|O|DELENTS a_lent)
			(|A|CURVA nil)
		  )
		)
	)

	(cond
		((null a_lpol) nil)
		((null a_pa)
			(setq a_dver (car a_lpol) a_pto (|K|PTATOP (cadddr a_dver))) ;a_xp (cadddr a_dver) a_yp (nth 4 a_dver) a_pto (list a_yp a_xp))
			(|A|EJE (cddr a_lpol) a_pto a_pra)
		)
		(T
			(setq a_dver (car a_lpol) a_nv (car a_dver) a_alfa (cadr a_dver) a_pto (|K|PTATOP (cadddr a_dver))) ;a_xp (cadddr a_dver) a_yp (nth 4 a_dver)
			(cond
				((eq a_nv "PLFIN")
					(|O|LINE a_pa a_pto a_lay)
					(list (car a_lpol))
				)
				((eq (substr a_nv 1 2) "PL")
					(|O|LINE a_pa a_pto a_lay)
					(append (list (car a_lpol) (cadr a_lpol)) (|A|EJE (cddr a_lpol) a_pto a_lay))
				)
				((member (substr a_nv 1 1) (list "A" "R"))
					(|O|LINE a_pa a_pto a_lay)
					(append (list (car a_lpol) (cadr a_lpol)) (|A|EJE (cddr a_lpol) a_pto a_lay))
				)
				(T
					(setq a_dppt (caddr a_lpol) a_ppt (|K|PTATOP (cadddr a_dppt))
					      a_delta (- a_alfa PI) a_deltaa (abs a_delta) a_deltas (angtos a_deltaa 1 3)
					)
					(append (list (|A|CURVA (|L|RESL a_dver 4)) (cadr a_lpol)) (|A|EJE (cddr a_lpol) a_pto a_lay))
				)
			)
		)
	)
)

; Listar un alineamiento a partir de su poligonal
;"|A|LISTA"
(defun |A|LISTA (a_lpol a_pa a_pra)
	(defun |A|DCURVA (a_ldc)
		(if (and (car a_ldc) (/= (car a_ldc) 0))
		  (progn
			(setq a_rc (|N|IAR (car a_ldc)) a_lee (|N|IAR (cadr a_ldc)) a_les (|N|IAR (caddr a_ldc))
			      a_lpif (|E|PTIF a_pto a_rc a_lee a_les a_delta (|K|ANG a_pto a_pa) (|K|ANG a_pto a_ppt))
			      a_te (car a_lpif) a_et (cadr a_lpif) a_prte (+ a_pra (|K|DISTH a_pa a_te))
			      a_dra (list (list a_pa a_te a_prte))
			      a_titae (/ a_lee (* a_rc 2)) a_titas (/ a_les (* a_rc 2))
			      a_deltac ((|N|SIGNO a_delta) (- (abs a_delta) (+ a_titae a_titas))) a_lent '()
			)
			(if (> a_lee 0)
				(setq a_ee (|E|REPPE a_te a_rc a_lee (|K|ANG a_pa a_pto) (|N|-SIGNO a_delta) 100)
				      a_ec (last a_ee) a_prec (+ a_prte a_lee)
				      a_dae (list (list a_te a_ec a_prec a_lee ((|N|SIGNO a_delta) a_titae) (|K|ANG a_te a_pto) "E"))
				)
				(setq a_ec a_te a_prec a_prte a_dae nil)
			)
			(setq a_prce (+ a_prec (* (abs a_deltac) a_rc)))
			(if (> a_les 0)
				(setq a_es (|E|REPPE a_et a_rc a_les (|K|ANG a_ppt a_pto) (|N|SIGNO a_delta) 100)
				      a_ce (last a_es) a_pret (+ a_prce a_les)
				      a_das (list (list a_ce a_et a_pret a_les ((|N|SIGNO a_delta) a_titas) (|K|ANG a_et a_pto) "S"))
				)
				(setq a_ce a_et a_pret a_prce a_das nil)
			)
			(setq a_cgc (|C|CENTRO a_deltac a_rc a_ec a_ce) a_dac (list (list a_ec a_ce a_prce a_deltac a_rc "A")))
			(append a_dra a_dae a_dac a_das)
		  )
		  (progn
		  	(setq a_et a_pto a_pret (+ a_pra (|K|DISTH a_pa a_pto)))
			(list (list a_pa a_pto a_pret))
		  )
		)
	)

	(cond
		((null a_lpol) nil)
		((null a_pa)
			(setq a_dver (car a_lpol) a_pto (|K|PTATOP (cadddr a_dver))) ;a_xp (cadddr a_dver) a_yp (nth 4 a_dver) a_pto (list a_yp a_xp))
			(|A|LISTA (cddr a_lpol) a_pto a_pra)
		)
		(T
			(setq a_dver (car a_lpol) a_nv (car a_dver) a_alfa (cadr a_dver) a_pto (|K|PTATOP (cadddr a_dver))) ;a_xp (cadddr a_dver) a_yp (nth 4 a_dver)
			(cond
				((eq a_nv "PLFIN")
					(if (not (equal a_pa a_pto g_tol))
						(list (list a_pa a_pto (+ a_pra (|K|DISTH a_pa a_pto))))
						nil
					)
				)
				((eq (substr a_nv 1 2) "PL")
					(setq a_prfs (+ a_pra (|K|DISTH a_pa a_pto)))
					(cons (list a_pa a_pto a_prfs) (|A|LISTA (cddr a_lpol) a_pto a_prfs)) 
				)
				((member (substr a_nv 1 1) (list "A" "R"))
					(cons (list a_pa a_pto a_pra) (|A|LISTA (cddr a_lpol) a_pto a_pra)) 
				)
				(T
					(setq a_dppt (caddr a_lpol) a_ppt (|K|PTATOP (cadddr a_dppt))
					      a_delta (- a_alfa PI)
					)
					(append (|A|DCURVA (|L|RESL a_dver 4)) (|A|LISTA (cddr a_lpol) a_et a_pret))
				)
			)
		)
	)
)

;crear una poligonal a partir de un alineamiento
(defun |A|POLIGONAL (a_lsal)
	(defun |A|PROCC (a_lcc a_lsal)
		(cond
			((null a_lcc) nil)
			(T
				(setq a_seg (car a_lcc) a_pc (car a_seg) a_fc (cadr a_seg) a_delta (cadddr a_seg) a_rc (nth 4 a_seg)
					a_aseg (|A|ASEG a_pc a_lsal) a_pseg (|A|PSEG a_fc a_lsal) a_tsa (last a_aseg) a_tsp (last a_pseg)
				)
				(cond
					((and (eq a_tsa "E") (eq a_tsp "S"))
						(setq a_te (car a_aseg) a_lee (cadddr a_aseg) a_titae (nth 4 a_aseg) a_dirte (nth 5 a_aseg)
						      a_les (cadddr a_pseg) a_titas (nth 4 a_pseg) a_dirts (nth 5 a_pseg)
						      a_ltgs (|E|LTANG a_rc a_lee a_les (+ (abs a_delta) (abs a_titae) (abs a_titas)))
						)
						(cons (list (polar a_te a_dirte (car a_ltgs)) a_rc a_lee a_les) (|A|PROCC (cdr a_lcc) a_lsal))
					)
					((eq a_tsa "E")
						(setq a_te (car a_aseg) a_lee (cadddr a_aseg) a_titae (nth 4 a_aseg) a_dirte (nth 5 a_aseg)
							a_ltgs (|E|LTANG a_rc a_lee 0.0 (+ (abs a_delta) (abs a_titae)))
						)
						(cons (list (polar a_te a_dirte (car a_ltgs)) a_rc a_lee 0.0) (|A|PROCC (cdr a_lcc) a_lsal))
					)
					((eq a_tsp "S")
						(setq a_et (cadr a_pseg) a_les (cadddr a_pseg) a_titas (nth 4 a_pseg) a_dirts (nth 5 a_pseg)
							a_ltgs (|E|LTANG a_rc 0.0 a_les (+ (abs a_delta) (abs a_titas)))
						)
						(cons (list (polar a_et a_dirts (cadr a_ltgs)) a_rc 0.0 a_les) (|A|PROCC (cdr a_lcc) a_lsal))
					)
					(T
						(cons (list (|C|VERT a_delta a_rc a_pc a_fc) a_rc 0.0 0.0) (|A|PROCC (cdr a_lcc) a_lsal))
					)
				)
			)
		)
	)

	(defun |A|PROCAL (a_lalin a_lvc a_sega a_va a_rini a_nv a_npl)
		(cond
			((null a_lalin) nil)
			((= (length a_lalin) 1)
;;;				(setq a_seg (car a_lalin) a_ts (last a_seg))
;;;				(list (list nil nil (distance a_va (cadr a_seg))) (append (list "PLFIN" PI nil) (reverse (cadr a_seg))))
				(setq a_seg (car a_lalin) a_up (cadr a_seg) a_ts (last a_seg) a_v (caar a_lvc))
				(if (not (member (last a_seg) (list "E" "A" "S")))
					(list (list nil nil (distance a_va (cadr a_seg)))
					      (list "PLFIN" PI nil (cadr a_up) (car a_up))
				        )
					(list
						(list nil nil (distance a_va a_v))
						(list (strcat "V" (itoa a_nv)) (|N|CADATOP (angle a_v a_up) a_rini) nil (cadr a_v) (car a_v)) 
						(list nil nil (distance a_v a_up))
						(list "PLFIN" PI nil (cadr a_up) (car a_up))
					)
				)
			)
			((null a_sega)
				(setq a_seg (car a_lalin) a_ts (last a_seg))
				(if (not (member (last a_seg) (list "E" "A" "S")))
					(cons
						(append (list "PL0" (|N|CADATOP (angle (caar a_lalin) (cadar a_lalin)) nil) nil) (reverse (caar a_lalin)))
						(|A|PROCAL (cdr a_lalin) a_lvc (car a_lalin) (caar a_lalin) (angle (caar a_lalin) (cadar a_lalin)) a_nv a_npl)
					)
					(cons
						(append (list "PL0" (|N|CADATOP (angle (caar a_lalin) (caar a_lvc)) nil) nil) (reverse (caar a_lalin)))
						(|A|PROCAL a_lalin a_lvc (car a_lalin) (caar a_lalin) (angle (caar a_lalin) (caar a_lvc)) a_nv a_npl)
					)
				)
			)
			(T
				(setq a_seg (car a_lalin) a_ts (last a_seg) a_tsa (last a_sega))
				(cond
					((eq a_ts "A")
						(setq a_delta (cadddr a_seg) a_rc (nth 4 a_seg))
						(if (setq a_pseg (cadr a_lalin))
							(if (eq (last a_pseg) "S")
								(setq a_pa (cadr a_pseg))
								(setq a_pa (car a_pseg))
							)
							(setq a_pa (cadr a_seg))
						)
						(append
						  (list
							(list nil nil (distance a_va (caar a_lvc)))
							(append (list (strcat "V" (itoa a_nv)) (|N|CADATOP (angle (caar a_lvc) a_pa) a_rini) nil) (reverse (caar a_lvc)) (cdar a_lvc))
						  )
							(|A|PROCAL (cdr a_lalin) (cdr a_lvc) a_seg (caar a_lvc) (angle (caar a_lvc) a_pa) (1+ a_nv) a_npl)
						)
					)
					((or (eq a_ts "E") (eq a_ts "S"))
						(|A|PROCAL (cdr a_lalin) a_lvc a_seg a_va a_rini a_nv a_npl)
					)
					(T
						(if (member (last a_sega) (list "A" "E" "S"))
							(|A|PROCAL (cdr a_lalin) a_lvc a_seg a_va a_rini a_nv a_npl)
						  (progn
							(setq a_rfin (angle (car a_seg) (cadr a_seg)) a_alfa (|N|CADATOP a_rfin a_rini))
							(if (equal a_alfa PI 0.00005)
								(setq a_npt (strcat "PL" (itoa a_npl)) a_npl (1+ a_npl))
								(setq a_npt (strcat "V" (itoa a_nv)) a_nv (1+ a_nv))
							)
							(append
							  (list
								(list nil nil (distance a_va (car a_seg)))
								(append (list a_npt a_alfa nil) (reverse (car a_seg)))
							  )
								(|A|PROCAL (cdr a_lalin) a_lvc a_seg (car a_seg) (angle (car a_seg) a_va) a_nv a_npl)
							)
						  )
						)
					)
				)
			)
		)
	)

	(setq a_lcurvas (|A|SEGCC a_lsal) a_lvcc (|A|PROCC a_lcurvas a_lsal))
	(if (> (length a_lsal) 1)
		(|A|PROCAL a_lsal a_lvcc nil nil nil 1 1)
	  (progn
	  	(setq a_seg (car a_lsal) a_pp (car a_seg) a_up (cadr a_seg) a_tip (last a_seg) a_v (caar a_lvcc))
		(if (eq (last (car a_lsal)) "A")
		  (list
			(list "PL0" (|N|CADATOP (angle a_pp (caar a_lvcc)) nil) nil (cadr a_pp) (car a_pp))
			(list nil nil (|K|DISTH a_pp a_v))
			(list "V1" (|N|CADATOP (angle (caar a_lvcc) a_up) (angle a_pp (caar a_lvcc))) nil (cadr a_v) (car a_v))
			(list nil nil (|K|DISTH a_v a_up))
			(list "PLFIN" PI nil (cadr a_up) (car a_up))
		  )
		  (list
			(list "PL0" (|N|CADATOP (angle a_pp a_up) nil) nil (cadr a_pp) (car a_pp))
			(list nil nil (|K|DISTH a_pp a_up))
			(list "PLFIN" PI nil (cadr a_up) (car a_up))
		  )
		)
	  )
	)
)

;devolver los segmentos rectos de un alineamiento
;"|A|SEGR"
(defun |A|SEGR (a_alin)
	(cond ((null a_alin) nil)
		(T (setq a_seg (car a_alin) a_alin (cdr a_alin))
			(cond
				((not (member (last a_seg) (list "E" "A" "S"))) 
					(cons a_seg (|A|SEGR a_alin))
				)
				(T
					(|A|SEGR a_alin)
				)
			)
		)
	)
)

;crear una lista con las curvas circulares de un alineamiento |A|SEGCC
;"|A|SEGCC"
(defun |A|SEGCC (a_alin)
	(cond ((null a_alin) nil)
		(T (setq a_seg (car a_alin) a_alin (cdr a_alin))
			(if (eq (last a_seg) "A")
				(cons a_seg (|A|SEGCC a_alin))
				(|A|SEGCC a_alin)
			)
		)
	)
)

;dibujar un alineamiento
;"|A|DIBAL"
(defun |A|DIBAL (a_lpol a_pa a_pra a_lay)
	(defun |A|DIBCURV (a_ldc)
		(setq a_rc (|N|IAR (car a_ldc)) a_lee (|N|IAR (cadr a_ldc)) a_les (|N|IAR (caddr a_ldc)))
		(if (and a_rc (/= a_rc 0))
		  (progn
			(setq a_lpif (|E|PTIF a_pto a_rc a_lee a_les a_delta (|K|ANG a_pto a_pa) (|K|ANG a_pto a_ppt))
			      a_te (car a_lpif) a_et (cadr a_lpif) a_prte (+ a_pra (|K|DISTH a_pa a_te))
			      a_dra (list (list a_pa a_te a_prte))
			      a_titae (/ a_lee (* a_rc 2)) a_titas (/ a_les (* a_rc 2))
			      a_deltac ((|N|SIGNO a_delta) (- a_deltaa (+ a_titae a_titas))) a_lent '()
			)
			(|O|LINE a_pa a_te a_lay)
			(if (> a_lee 0)
			  (progn
				(setq a_ee (|E|REPPE a_te a_rc a_lee (|K|ANG a_pa a_pto) (|N|-SIGNO a_delta) 100)
				      a_ec (last a_ee) a_prec (+ a_prte a_lee)
				)
			  	(|O|LWPOLY a_ee a_lay 128)
			  )
				(setq a_ec a_te a_prec a_prte)
			)
			(setq a_prce (+ a_prec (* (abs a_deltac) a_rc)))
			(if (> a_les 0)
			  (progn
				(setq a_es (|E|REPPE a_et a_rc a_les (|K|ANG a_ppt a_pto) (|N|SIGNO a_delta) 100)
				      a_ce (last a_es) a_pret (- a_prce a_les)
				)
			  	(|O|LWPOLY a_es a_lay 128)
			  )
				(setq a_ce a_et a_pret a_prce)
			)
			(setq a_cgc (|C|CENTRO a_deltac a_rc a_ec a_ce) a_ai (|K|ANG a_cgc a_ec) a_af (|K|ANG a_cgc a_ce)
			      a_dac (list (list a_ec a_ce a_prce a_delta a_rc "A"))
			)
			(if (minusp a_delta)
				(|O|ARC a_cgc a_rc a_ai a_af a_lay)
				(|O|ARC a_cgc a_rc a_af a_ai a_lay)
			)
		  )
		  (progn
			(setq a_et a_pto a_pret (+ a_pra (|K|DISTH a_pa a_et)))
			(|O|LINE a_pa a_pto a_lay)
		  )
		)
	)

	(cond
		((null a_lpol) nil)
		((null a_pa)
			(setq a_dver (car a_lpol) a_pto (|K|PTATOP (cadddr a_dver))) ;a_xp (cadddr a_dver) a_yp (nth 4 a_dver) a_pto (list a_yp a_xp))
			(|A|DIBAL (cddr a_lpol) a_pto a_pra a_lay)
		)
		(T
			(setq a_dver (car a_lpol) a_nv (car a_dver) a_alfa (cadr a_dver) a_pto (|K|PTATOP (cadddr a_dver))) ;a_xp (cadddr a_dver) a_yp (nth 4 a_dver)
			(cond
				((eq a_nv "PLFIN")
					(|O|LINE a_pa a_pto a_lay)
				)
				((eq (substr a_nv 1 2) "PL")
					(|O|LINE a_pa a_pto a_lay)
					(setq a_prfs (+ a_pra (|K|DISTH a_pa a_pto)))
					(|A|DIBAL (cddr a_lpol) a_pto a_prfs a_lay) 
				)
				((member (substr a_nv 1 1) (list "A" "R"))
					(|O|LINE a_pa a_pto a_lay)
					(|A|DIBAL (cddr a_lpol) a_pto a_pra a_lay) 
				)
				(T
					(setq a_dppt (caddr a_lpol) a_ppt (|K|PTATOP (cadddr a_dppt))
					      a_delta (- a_alfa PI) a_deltaa (abs a_delta) a_deltas (angtos a_deltaa 1 3)
					)
					(|A|DIBCURV (|L|RESL a_dver 4))
					(|A|DIBAL (cddr a_lpol) a_et a_pret a_lay)
				)
			)
		)
	)
)

;dibujar los puntos de la poligonal g_polal
;"|A|PPTPOL"
(defun |A|PPTPOL (a_lpol a_pa a_lay)
	(cond
		((null a_lpol) nil)
		((= (length a_lpol) 1)
			(setq a_dpt (car a_lpol) a_np (car a_dpt) a_alfa (cadr a_dpt) a_pin (|K|PTATOP (cadddr a_dpt)))
			(|B|INSERT "PL" a_pin a_lay (|K|ANG a_pa a_pin) g_eschi (list a_np))
			(list a_pin)
		)
		(T
			(setq a_dpt (car a_lpol) a_np (car a_dpt) a_alfa (cadr a_dpt) a_pin (|K|PTATOP (cadddr a_dpt)))
			(if a_pa (setq a_anga (|K|ANG a_pa a_pin)) (setq a_anga nil))
			(setq a_angi (|N|TOPACAD a_alfa a_anga))
			(if (eq (substr a_np 1 1) "P")
			 	(setq a_nb "PL")
				(if (> a_alfa PI)
					(setq a_nb "VERTD" a_angi (|N|ANGC a_anga a_angi))
					(setq a_nb "VERTI" a_angi (|N|ANGC a_anga a_angi))
				)
			)
			(|B|INSERT a_nb a_pin a_lay a_angi g_eschi (list a_np))
			(cons a_pin (|A|PPTPOL (cddr a_lpol) a_pin a_lay))
		)
	)
)


;---------------------------------------------------------------------------------------------------------------------------------------------------------

;			P	U	N	T	O	S		E	N		A	L	I	N	E	A	M	I	E	N	T	O	S

;---------------------------------------------------------------------------------------------------------------------------------------------------------
; Cálculo del punto de inserción y el ángulo para una progresiva dada
;"|A|PINS"
(defun |A|PINS (a_pr a_al)
	(cond
		((null a_al) nil)
		((minusp a_pr) nil)
		((= (fix a_pr) 0) (list (caar a_al) (angle (caar a_al) (cadar a_al))))
		((> a_pr (caddr (last a_al))) (list (cadr (last a_al)) (angle (car (last a_al)) (cadr (last a_al)))))
		(T 
			(setq a_seg (|A|SEG a_pr a_al) a_pti (car a_seg) a_ptf (cadr a_seg) a_prfin (caddr a_seg) a_tipo (last a_seg))
			(cond
				((eq (last a_seg) "A")
					(|C|PYAC a_pr a_seg)
				)
				((eq (last a_seg) "E")
					(|E|PYAE a_pr a_seg)
				)
				((eq (last a_seg) "S")
					(|E|PYAE a_pr a_seg)
				)
				(T
					(if (equal a_pr a_prfin 0.005)
						(list a_ptf (angle a_pti a_ptf))
						(list (polar a_ptf (angle a_ptf a_pti) (- a_prfin a_pr)) (angle a_pti a_ptf))
					)
				)
			)
		)
	)
)

; Cálculo de un punto para una progresiva dada y a una distancia a_dist del eje
;"|A|PINL"
(defun |A|PINL (a_pr a_dist a_lado a_alin)
	(setq a_pia (|A|PINS a_pr a_alin) a_ptal (car a_pia) a_ang (cadr a_pia))
	(cond
		((equal a_dist 0 g_tol) a_ptal)
		(T
			(|K|POLAR a_ptal (+ a_ang (|N|ANGLADO a_lado)) a_dist)
		)
	)
)

;"|A|REPC"
(defun |A|REPC (a_segc a_pri)
	(setq a_pc (car a_segc) a_fc (cadr a_segc) a_delta (cadddr a_segc) a_rc (car (cddddr a_segc)) a_des (* a_rc (abs a_delta))
	      a_cg (|C|CENTRO a_delta a_rc a_pc a_fc) a_ai (angle a_cg a_pc) a_af (angle a_cg a_fc)
	)
	(cond
		((<= a_rc 100.0)
			(setq a_paso 5.0)
		)
		((<= a_rc 500.0)
			(setq a_paso 10.0)
		)
		(T
			(setq a_paso 20.0)
		)
	)
	(|C|REPCD a_cg a_rc a_delta a_pc a_fc a_ai a_af 0.0 a_paso a_pri)
)

;"|A|REPC"
(defun |A|REPCP (a_segc a_pri a_nc)
	(defun |A|REPPC (a_pr a_np)
		(cond
			((>= (- a_pr a_pri) a_des) nil)
			(T
				(setq a_ps (|K|XY (|K|POLAR a_cg ((|N|-SIGNO a_delta) a_ai (/ (- a_pr a_pri) a_rc)) a_rc)) a_ac (|K|ANG a_ps a_cg))
				(cons (list (strcat a_nc (|T|ITOC a_np 2)) a_pr a_ps a_ac) (|A|REPPC (+ a_pr a_paso) (1+ a_np)))
			)
		)
	)

	(setq a_pc (car a_segc) a_fc (cadr a_segc) a_prf (caddr a_segc) a_delta (cadddr a_segc) a_rc (car (cddddr a_segc)) a_des (* a_rc (abs a_delta))
	      a_cg (|C|CENTRO a_delta a_rc a_pc a_fc) a_ai (angle a_cg a_pc) a_af (angle a_cg a_fc)
	)
	(cond
		((<= a_rc 100.0)
			(setq a_paso 5.0)
		)
		((<= a_rc 500.0)
			(setq a_paso 10.0)
		)
		(T
			(setq a_paso 20.0)
		)
	)
	(if (not a_pri) (setq a_pri (- a_prf (* (abs a_delta) a_rc))))
	(setq a_prp (|N|SIGPAS a_pri a_paso))
	(if (= a_prp a_pri) (setq a_prp (+ a_prp a_paso)))
	(cons (list (strcat "PC" a_nc) a_pri a_pc (|K|ANG a_pc a_cg))
	      (append (|A|REPPC a_prp 1) (list (list (strcat "FC" a_nc) a_prf a_fc (|K|ANG a_fc a_cg))))
	)
)

;"|A|REPEE"
(defun |A|REPEE (a_sege a_prec)
	(setq a_te (car a_sege) a_prf (caddr a_sege) a_le (cadddr a_sege) a_tita (car (cddddr a_sege)) a_dirt (cadr (cddddr a_sege)) a_rc (/ a_le 2 (abs a_tita))
	      a_pri (- a_prf a_le)
	)
	(|E|REPPE a_te a_rc a_le a_dirt (|N|-SIGNO a_tita) a_prec)
)

;"|A|REPES"
(defun |A|REPES (a_sege a_prec)
	(setq a_et (cadr a_sege) a_prf (caddr a_sege) a_le (cadddr a_sege) a_tita (car (cddddr a_sege)) a_dirt (cadr (cddddr a_sege)) a_rc (/ a_le 2 (abs a_tita))
	      a_pri (- a_prf a_le)
	)
	(reverse (|E|REPPE a_et a_rc a_le a_dirt (|N|SIGNO a_tita) a_prec))
)

; Devolver el segmento en cuyo ámbito cae un punto en un alineamiento
;"|A|IDPL"
(defun |A|IDPL (a_pto a_alin a_lidp)
	(cond
		((null a_alin) a_lidp)
		(T
			(setq a_seg (car a_alin) a_pps (car a_seg) a_sps (cadr a_seg) a_prfs (caddr a_seg) a_tip (last a_seg))
			(cond
				((eq a_tip "A")
					(setq a_delta (cadddr a_seg) a_radio (car (cddddr a_seg)))
					(if (|C|ENCUR a_pto a_delta a_radio a_pps a_sps)
						(setq a_lidp (cons (|C|IDP a_pto a_seg) a_lidp))
					)
					(|A|IDPL a_pto (cdr a_alin) a_lidp)
				)
				((eq a_tip "E")
					(if (|E|ENESP a_pto a_seg)
						(setq a_lidp (cons (|E|IDP a_pto a_seg) a_lidp))
					)
					(|A|IDPL a_pto (cdr a_alin) a_lidp)
				)
				((eq a_tip "S")
					(if (|E|ENESP a_pto a_seg)
						(setq a_lidp (cons (|E|IDP a_pto a_seg) a_lidp))
					)
					(|A|IDPL a_pto (cdr a_alin) a_lidp)
				)
				(T
					(if (|C|ENRECTA a_pto a_seg)
						(setq a_lidp (cons (|C|IDPR a_pto a_seg) a_lidp))
					)
					(|A|IDPL a_pto (cdr a_alin) a_lidp)
				)
			)
		)
	)
)

;"|A|IDP"
(defun |A|IDP (a_pto a_alin a_forzar)
	(defun |A|MENL (a_lista a_min)
		(cond
			((null a_lista) a_min)
			(T
				(setq a_nmin (cadr a_min) a_el (car a_lista) a_ncomp (cadr a_el))
				(if (< a_ncomp a_nmin)
					(|A|MENL (cdr a_lista) a_el)
					(|A|MENL (cdr a_lista) a_min)
				)
			)
		)
	)
	(defun |A|FORZAR (a_pb)
		(setq a_polal (|A|POLIGONAL a_alin)
		      a_pp (list (nth 4 (car a_polal)) (nth 3 (car a_polal)))
		      a_sp (list (nth 4 (caddr a_polal)) (nth 3 (caddr a_polal))) a_diri (|K|ANG a_sp a_pp)
		      a_polal (reverse a_polal)
		      a_up (list (nth 4 (car a_polal)) (nth 3 (car a_polal)))
		      a_ap (list (nth 4 (caddr a_polal)) (nth 3 (caddr a_polal))) a_dirf (|K|ANG a_ap a_up)
		      a_segi (list (|K|POLAR a_pp a_diri 1000.0) a_pp 0.0)
		      a_segf (list a_up (|K|POLAR a_up a_dirf 1000.0) (+ (caddr (last a_alin)) 1000.0))
		)
		(if (|C|ENRECTA a_pb a_segi)
			(|C|IDPR a_pb a_segi)
			(|C|IDPR a_pb a_segf)
		)
	)

	(setq a_lidps (|A|IDPL a_pto a_alin nil))
	(cond
		((null a_lidps)
			(if a_forzar (|A|FORZAR a_pto) nil)
		)
		((= (length a_lidps) 1) (car a_lidps))
		(T
			(|A|MENL (cdr a_lidps) (car a_lidps))	
		)
	)
)

; crea una lista con los puntos singulares de un alineamiento
;"|A|WAYPOINTS"
(defun |A|WAYPOINTS (a_alin / a_wp)
	(defun |A|OBTLPT (a_lpa a_nc)
		(cond
			((null a_lpa) nil)
			(T
				(setq a_seg (car a_lpa) a_tip (last a_seg) a_pt (cadr a_seg) a_prfs (caddr a_seg))
			 	(if (cdr a_lpa)
				  (progn
					(setq a_pseg (|A|PSEG a_pt a_lpa) a_tips (last a_pseg))
				 	(cond
				 		  ((eq a_tip "E")
							(cond ((eq a_tips "A")
							       		(setq a_np (strcat "EC" (itoa a_nc)))
									(cons (list a_np a_prfs (|K|PTATOP a_pt) ((|N|SIGNO (cadddr a_pseg)) 1)) (|A|OBTLPT (cdr a_lpa) a_nc))
							      )
								((eq a_tips "S")
							       		(setq a_np (strcat "EE" (itoa a_nc)))
									(cons (list a_np a_prfs (|K|PTATOP a_pt) ((|N|SIGNO (nth 4 a_pseg)) 1)) (|A|OBTLPT (cdr a_lpa) a_nc))
							      )
							)
						  )
				 		  ((eq a_tip "A")
						   	(setq a_cc (|C|PM (cadddr a_seg) (nth 4 a_seg) (car a_seg) a_pt) a_sg (|N|SIGNO (cadddr a_seg))
							      a_npm (strcat "CC" (itoa a_nc)) a_prpm (- a_prfs (/ (|C|ARCO (cadddr a_seg) (car a_seg) a_pt (car (cddddr a_seg))) 2))
							)
							(cond
								((eq a_tips "S")
							       		(setq a_np (strcat "CE" (itoa a_nc)))
									(append (list (list a_npm a_prpm (|K|PTATOP a_cc) (a_sg 1)) (list a_np a_prfs (|K|PTATOP a_pt) (a_sg 1))) (|A|OBTLPT (cdr a_lpa) a_nc))
								)
								((eq a_tips "A")
							       		(setq a_np (strcat "FC" (itoa a_nc) "-PC" (itoa (1+ a_nc))))
									(append (list (list a_npm a_prpm (|K|PTATOP a_cc) (a_sg 1)) (list a_np a_prfs (|K|PTATOP a_pt) (a_sg 1))) (|A|OBTLPT (cdr a_lpa) (1+ a_nc)))
								)
								((eq a_tips "E")
							       		(setq a_np (strcat "FC" (itoa a_nc) "-TE" (itoa (1+ a_nc))))
									(append (list (list a_npm a_prpm (|K|PTATOP a_cc) (a_sg 1)) (list a_np a_prfs (|K|PTATOP a_pt) (a_sg 1))) (|A|OBTLPT (cdr a_lpa) (1+ a_nc)))
								)
								(T
							       		(setq a_np (strcat "FC" (itoa a_nc)))
									(append (list (list a_npm a_prpm (|K|PTATOP a_cc) (a_sg 1)) (list a_np a_prfs (|K|PTATOP a_pt) (a_sg 1))) (|A|OBTLPT (cdr a_lpa) (1+ a_nc)))
								)
							)
					      	  )
				 		  ((eq a_tip "S")
							(cond
								((eq a_tips "E")
							       		(setq a_np (strcat "ET" (itoa a_nc) "-TE" (itoa (1+ a_nc))))
									(cons (list a_np a_prfs (|K|PTATOP a_pt) ((|N|SIGNO (nth 4 a_pseg)) 1)) (|A|OBTLPT (cdr a_lpa) (1+ a_nc)))
								)
								((eq a_tips "A")
							       		(setq a_np (strcat "ET" (itoa a_nc) "-PC" (itoa (1+ a_nc))))
									(cons (list a_np a_prfs (|K|PTATOP a_pt) ((|N|SIGNO (cadddr a_pseg)) 1)) (|A|OBTLPT (cdr a_lpa) (1+ a_nc)))
								)
								(T
							       		(setq a_np (strcat "ET" (itoa a_nc)))
									(cons (list a_np a_prfs (|K|PTATOP a_pt) ((|N|SIGNO (nth 4 a_seg)) 1)) (|A|OBTLPT (cdr a_lpa) (1+ a_nc)))
								)
							)
						 )
						(T
							(cond
								((eq a_tips "E")
							       		(setq a_np (strcat "TE" (itoa a_nc)) )
									(cons (list a_np a_prfs (|K|PTATOP a_pt) ((|N|SIGNO (nth 4 a_pseg)) 1)) (|A|OBTLPT (cdr a_lpa) a_nc))
							 	)
								((eq a_tips "A")
							       		(setq a_np (strcat "PC" (itoa a_nc)))
									(cons (list a_np a_prfs (|K|PTATOP a_pt) ((|N|SIGNO (cadddr a_pseg)) 1)) (|A|OBTLPT (cdr a_lpa) a_nc))
								)
								(T
							       		(setq a_np (strcat "V" (itoa a_nc)))
									(cons (list a_np a_prfs (|K|PTATOP a_pt) 1) (|A|OBTLPT (cdr a_lpa) (1+ a_nc)))
								)
							)
						  )
					)
				  )
				  (progn
				 	(cond
				 		((eq a_tip "A")
						   	(setq a_cc (|C|PM (cadddr a_seg) (car (cddddr a_seg)) (car a_seg) a_pt) a_sg (|N|SIGNO (cadddr a_seg))
							      a_npm (strcat "CC" (itoa a_nc)) a_prpm (- a_prfs (/ (|C|ARCO (cadddr a_seg) (car a_seg) a_pt (car (cddddr a_seg))) 2))
							      a_np (strcat "FC" (itoa a_nc))
							)
						 	(list (list a_npm a_prpm (|K|PTATOP a_cc) (a_sg 1)) (list a_np a_prfs (|K|PTATOP a_pt) (a_sg 1)))
					      	)
				 		((eq a_tip "S")
							(setq a_np (strcat "ET" (itoa a_nc)))
							(list (list a_np a_prfs (|K|PTATOP a_pt) ((|N|SIGNO (nth 4 a_pseg)) 1)))
						)
						(T
							(list (list "PLFIN" a_prfs (|K|PTATOP a_pt) 1))
						)
					)
				  )
				)
			)
		)
	)

	(cond
		((eq (last (car a_alin)) "A")
			(cons (list "PC1" 0.0 (|K|PTATOP (caar a_alin)) 1) (|A|OBTLPT a_alin 1))
		)
		((eq (last (car a_alin)) "E")
			(cons (list "TE1" 0.0 (|K|PTATOP (caar a_alin)) 1) (|A|OBTLPT a_alin 1))
		)
		(T
			(cons (list "PL0" 0.0 (|K|PTATOP (caar a_alin)) 1) (|A|OBTLPT a_alin 1))
		)
	)
)

;progresivar un alineamiento
;"|A|PROGRESIVAR"
(defun |A|PROGRESIVAR (a_alin a_pri a_fesc / a_dist a_upr a_pr a_lpos a_pt a_an)
	(if (not a_pri) (setq a_pri (car (|A|IDP (caar a_alin) a_alin))))
	(setq a_dist (GetReal "\n Ingrese distancia para el progesivado"))
	(|O|CREALAY "Lp_Progresivas" 3 "continuous" nil)
	(setq a_upr (caddr (last a_alin)) a_pr (|N|SIGPAS a_pri a_dist))
	(while (< a_pr a_upr)
		(setq a_lpos (|A|PINS a_pr a_alin) a_pt (car a_lpos) a_an (cadr a_lpos))
		(|B|INSERT "PR" a_pt "Lp_Progresivas" a_an a_fesc (list (rtos a_pr 2 0)))
		(setq a_pr (+ a_pr a_dist))
	)
)

;progresivar puntos singulares
;"|A|PROGPS"
(defun |A|PROGPS (a_wpoints a_transf)
	(cond
		((null a_wpoints) nil)
		(T
			(setq a_wpt (car a_wpoints) a_npt (car a_wpt) a_pr (cadr a_wpt) a_pt (|K|PTATOP (caddr a_wpt)) a_sg (|N|-SIGNO (last a_wpt))
;;;			      a_dir (a_sg (cadr (|A|PINS a_pr g_alin)) (* Pi 0.5))
			      a_dir (a_sg (last (|A|IDP a_pt g_alin)) (* Pi 0.5))
			)
			(if a_transf (setq a_dps (|K|BPTPS a_pt (cdr (vports))) a_pt (car a_dps) a_dir (+ a_dir (cadr a_dps))))
			(|B|INSERT "PRGE" a_pt "Lp_Progresivas" a_dir g_eschi (list (strcat a_npt ": Pr. " (rtos a_pr 2 2))))
			(|A|PROGPS (cdr a_wpoints) a_transf)
		)
	)
)

;"|A|IDP2A"
(defun |A|IDP2A (a_pro a_ejeo a_ejed)
	(setq u_lpp (|A|PINS u_pro u_ejeo) u_pp (car u_lpp) u_dir (cadr u_lpp)
	      u_pted (car (|L|PURGAR (i_sa (list (polar u_pp (+ u_dir (n_gar 90)) 100.0) (polar u_pp (- u_dir (n_gar 90)) 100.0)) u_ejed)))
	      u_prd (car (|A|IDP u_pted u_ejed nil)) u_idpo (|A|IDP u_pted u_ejeo nil) u_di (cadr u_idpo) u_lado (caddr u_idpo)
	)
	(list u_prd u_di u_lado)
)

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;			V	A	R	I	O	S		E	N		A	L	I	N	E	A	M	I	E	N	T	O	S

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;dividir láminas
;"|A|LAMINAS"
(defun |A|LAMINAS (a_plEje a_pri / a_upr a_pr a_lpos a_longl a_pt a_an)
	(|O|CREALAY "F_láminas" 255 "continuous" nil)
	(setq a_upr (caddr (last a_plEje)) a_longl (* g_mxl g_eschi) a_pr (|N|SIGPAS a_pri a_longl))
	(while (< a_pr a_upr)
		(setq a_lpos (|A|PINS a_pr a_plEje) a_pt (car a_lpos) a_an (cadr a_lpos)
			a_pr (+ a_pr a_longl)
			a_pa1 (polar a_pt (+ a_an (/ PI 2)) 200)
			a_pa2 (polar a_pt (- a_an (/ PI 2)) 200)
		)
		(|O|LINE a_pa1 a_pa2 "F_láminas")
	)
)

;armar un string de datos de puntos singulares
;"|A|DPS"
(defun |A|DPS (a_tps a_nps a_prps)
	(list (strcat a_tps a_nps ": Pr. " (rtos a_prps 2 2)))
)

(defun |A|PRI (a_alin)
	(setq a_seg (car a_alin) a_pp (car a_seg) a_sp (cadr a_seg) a_prfs (caddr a_seg))
	(cond
		((eq (last a_seg) "A")
			(setq a_delta (cadddr a_seg) a_rc (nth 4 a_seg))
			(|N|ROUND (- a_prfs (* (abs a_delta) a_rc)) 2)
		)
		((or (eq (last a_seg) "E") (eq (last a_seg) "S"))
			(|N|ROUND (- a_prfs (cadddr a_seg)) 2)
		)
		(T (|N|ROUND (- a_prfs (|K|DISTH a_pp a_sp)) 2))
	)
)

(defun |A|PRF (a_alin)
	(caddr (last a_alin))
)

;crear una lista con los peraltes de un alineamiento
;"|A|LISTPERSA"
(defun |A|LISTPERSA (a_nomal a_alin a_lperft)
	(defun |A|CPERSA (a_curvas a_lper)
		(cond
			((null a_curvas) NIL)
			(T
				(setq a_seg (car a_curvas) a_rc (nth 4 a_seg) a_delta (cadddr a_seg) a_prfs (caddr a_seg) a_pris (- a_prfs (* a_rc (abs a_delta)))
				      a_prms (|L|PROM (list a_prfs a_pris)) a_dirc (|C|DIRC a_delta) a_aseg (|A|ASEG (car a_seg) a_alin) a_pseg (|A|PSEG (cadr a_seg) a_alin)
				      a_ptent (|A|CALCVAL a_lperft a_pris) a_ptsal (|A|CALCVAL a_lperft a_prfs)
				      a_vde (car a_ptent) a_ace (caddr a_ptent) a_pne (- (cadddr a_ptent)) a_adpe (nth 4 a_ptent) a_adse (nth 5 a_ptent)
				      a_vds (car a_ptsal) a_acs (caddr a_ptsal) a_pns (- (cadddr a_ptsal)) a_adps (nth 4 a_ptsal) a_adss (nth 5 a_ptsal)
				      a_adp (or (|T|STOL a_adpe) (|T|STOL a_adps)) a_ads (or (|T|STOL a_adse) (|T|STOL a_adss))
				      a_per (if a_lper (cadar a_lper) (|C|PER a_rc (max a_vde a_vds) g_perm (min a_pne a_pns)))
				)
				(if (eq (last a_aseg) "E") (setq a_lee (cadddr a_aseg)) (setq a_lee 0.0))
				(if (eq (last a_pseg) "S") (setq a_les (cadddr a_pseg)) (setq a_les 0.0))
				(setq a_transe (|C|TRPSA a_per a_rc a_delta a_pris a_lee - + a_vde a_ace nil a_pne a_adp a_ads)
				      a_transs (reverse (|C|TRPSA a_per a_rc a_delta a_prfs a_les + - a_vds a_acs nil a_pns a_adp a_ads))
				)
				(if (eq a_dirc "D") (setq a_transe (|L|SLCPOS a_transe (list '(1 2) '(3 4))) a_transs (|L|SLCPOS a_transs (list '(1 2) '(3 4)))))
			 	(append a_transe a_transs (|A|CPERSA (cdr a_curvas) (cdr a_lper)))
			 )
		)
	)

	(setq a_lperc (cadr (|F|LEECSVA g_nomal "Peraltes" 1)))
	(setq a_pri (|A|PRI a_alin) a_prf (caddr (last a_alin)) a_lcurvas (|A|SEGCC a_alin)
	      a_pti (|A|CALCVAL a_lperft a_pri) a_ptf (|A|CALCVAL a_lperft a_prf)
	      a_pni (- (cadddr a_pti)) a_pnf (- (cadddr a_ptf))
	)
	(append (list (list a_pri a_pni a_pni 0.0 0.0)) (|A|CPERSA a_lcurvas a_lperc) (list (list a_prf a_pnf a_pnf 0.0 0.0)))
)

;calcular valores para una progresiva determinada
;"|A|CALCVAL"
(defun |A|CALCVAL (a_lv a_st)
	(cdar (|L|OBTSLV a_lv a_st a_st))
)

;LISTPR: crear una lista de puntos de un alineamiento incluyendo todos los puntos singulares y puntos a una equidistancia determinada
;"|A|LISTPR"
(defun |A|LISTPR (a_dal a_psa a_psr a_psp a_pifr a_pifp a_pifv a_dist / a_alin a_ldist)
	(defun |A|COMBINAR (a_lpr a_pra a_tip)
		(cond
			((null a_lpr) (list (list a_pra a_tip)))
			((null a_pra) (|A|COMBINAR (cdr a_lpr) (caar a_lpr) (cadar a_lpr)))
			((equal (caar a_lpr) a_pra 0.01)
				(|A|COMBINAR (cdr a_lpr) a_pra (|T|SEP- a_tip (cadar a_lpr)))
			)
			(T
				(cons (list a_pra a_tip) (|A|COMBINAR (cdr a_lpr) (caar a_lpr) (cadar a_lpr)))
			)
		)
	)

	(setq a_alin (|O|ITEM 3 a_dal))
	(if a_psa (setq a_psa (mapcar '(lambda (el) (list (cadr el) (car el))) (|A|WAYPOINTS a_alin))))
	(if a_psr (setq a_psr (|L|BULT (cdr (|P|LPSR (|O|ITEM 5 a_dal) nil)))))
	(if a_psp (setq a_psp (mapcar '(lambda (el) (list (car el) "VP")) (|L|BULT (cdr (|A|LISTPERSA (|O|ITEM 1 a_dal) a_alin (|O|ITEM 7 a_dal)))))))
	(if a_pifr (setq a_pifr (apply 'append (mapcar '(lambda (el) (list (list (car el) "IR") (list (cadr el) "FR"))) (cadr (|F|LEECSVA (|O|ITEM 1 a_dal) "retornos" 1))))))
	(if a_pifp (setq a_pifp (apply 'append (mapcar '(lambda (el) (list (list (car el) "IP") (list (cadr el) "FP"))) (cadr (|F|LEECSVA (|O|ITEM 1 a_dal) "puentes" 1))))))
	(if a_pifv (setq a_pifv (apply 'append (mapcar '(lambda (el) (list (list (car el) "IV") (list (cadr el) "FV"))) (cadr (|F|LEECSVA (|O|ITEM 1 a_dal) "vados" 1))))))
	(if a_dist (setq a_ldist (|N|SEQNUM 0.0 a_dist (caddr (last a_alin)))))
	(|A|COMBINAR
		(|L|INTERC a_ldist
			(|L|INTERC a_pifv
				(|L|INTERC a_pifp
					(|L|INTERC a_pifr
						(|L|INTERC a_psp
							(|L|INTERC a_psa a_psr)
						)
					)
				)
			)
		)
		0.0 ""
	)
)

;devolver los puntos singulares de un alineamiento entre dos progresivas
;"|A|PTINTER"
(defun |A|PTINTER (a_alin a_pri a_prf)
	(cond
		((null a_alin) nil)
		(T
			(setq a_seg (car a_alin) a_prfs (caddr a_seg))
			(if (< a_prfs a_prf)
				(if (> a_prfs a_pri)
					(cons (cadr a_seg) (|A|PTINTER (cdr a_alin) a_pri a_prf))
					(|A|PTINTER (cdr a_alin) a_pri a_prf)
				)
				nil
			)
		)
	)
)

;"|A|ODV"
(defun |A|ODV (a_nv a_pr a_dal)
	(setq a_dp (|L|BUSCAR a_nv (|O|ITEM 2 a_dal) 0) a_alfa (cadr a_dp) a_coords (|K|PTATOP (cadddr a_dp)) a_delta (abs (- a_alfa PI)) a_dcc (|L|RESL a_dp 4))
	(if (and (car a_dcc) (/= (car a_dcc) 0))
		(setq a_rc (car a_dcc) a_lee (cadr a_dcc) a_les (caddr a_dcc) a_ltgs (|E|LTANG a_rc a_lee a_les a_delta) a_ee (|E|EXTER a_rc (list a_lee a_les) a_delta)
		      a_des (|E|DES a_rc (list a_lee a_les) a_delta) a_ptipo (|A|CALCVAL (|O|ITEM 7 a_dal) a_pr) a_vd (car a_ptipo) a_pn (/ (cadddr a_ptipo) 100)
		      a_per (if (eq (nth 4 a_ptipo) "S") (abs (* (|C|PER a_rc a_vd nil a_pn) 100)) 0.0)
		      a_sa (if (eq (nth 5 a_ptipo) "S") (|C|SANCHO a_rc a_vd) 0.0)
		)
		(setq a_rc 0.0 a_lee 0.0 a_les 0.0 a_te 0.0 a_ee 0.0 a_des 0.0 a_per 0.0 a_sa 0.0)
	)
	(if (= a_lee a_les)
		(list (list "<" a_alfa) (list "<" a_delta) a_rc a_lee (car a_ltgs) a_ee a_des a_per a_sa) 
		(list (list "<" a_alfa) (list "<" a_delta) a_rc (list "-" a_lee a_les) (cons "-" a_ltgs) a_ee a_des a_per a_sa)
	)
)

;"|A|CREATGV"
(defun |A|CREATGV (a_dal)
	(defun |A|DPTSP (a_lpol a_dpa)
		(cond
			((null a_lpol) nil)
			((= (length a_lpol) 1)
				(setq a_dp (car a_lpol) a_npt (car a_dp) a_alfa (cadr a_dp) a_coords (cadddr a_dp) a_delta (abs (- a_alfa PI))
				      a_pr (car (|A|IDP (|K|PTATOP a_coords) a_alin nil))
				)
				(|B|INSERT "TVERTGF" a_pin a_lay 0 1 (|T|LISATT (append (list a_npt a_coords a_dpa) (|A|ODV a_npt a_pr a_dal)) nil)) (|B|EXPLOT (entlast))
			)
			(T
				(setq a_dp (car a_lpol) a_npt (car a_dp) a_alfa (cadr a_dp) a_coords (cadddr a_dp) a_delta (abs (- a_alfa PI))
				      a_pr (car (|A|IDP (|K|PTATOP a_coords) a_alin nil))
				)
				(|B|INSERT "TVERTG" a_pin a_lay 0 1 (|T|LISATT (append (list a_npt a_coords a_dpa) (|A|ODV a_npt a_pr a_dal)) nil)) (|B|EXPLOT (entlast))
				(setq a_pin (|K|Y- a_pin 6))
				(|A|DPTSP (cddr a_lpol) (caddr (cadr a_lpol)))
			)
		)
	)
	(setq a_pin (getpoint "\nmarque el punto de inserción de la tabla") a_cp 0 a_alin (|O|ITEM 3 a_dal) a_lay (strcat (|T|INICIALES (|O|ITEM 1 a_dal) nil "") "_Tablas"))
	(|B|INSERT "TTGV" a_pin a_lay 0 1 nil) (|B|EXPLOT (entlast))
	(setq a_pin (|K|Y- a_pin 10))
	(|A|DPTSP (|O|ITEM 2 a_dal) "")
)

;LISTINT: Listar las intersecciones del alineamiento actual con otros alineamientos
;"|A|LISTINT"
(defun |A|LISTINT (a_ldal a_nomal a_alin)
	(defun |A|PROCINTS (a_lpts)
		(cond
			((null a_lpts) nil)
			((null (car a_lpts))
				(|A|PROCINTS (cdr a_lpts))
			)
			(T
				(setq a_pint (car a_lpts) a_idp (|A|IDP a_pint a_alin nil) a_idpe (|A|IDP a_pint a_eje)
				      a_pr (car a_idp) a_dir (last a_idp) a_pre (car a_idpe) a_dire (last a_idpe)
				      a_ani (angtos (|N|CADATOP a_dire (|N|DOSPI (+ a_dir PI))) 1 6)
				)
				(cons (list a_pr a_nomb a_pre a_pint a_ani) (|A|PROCINTS (cdr a_lpts))) 
			)
		)
	)
	(cond
		((null a_ldal) nil)
		((eq (|O|ITEM 1 (car a_ldal)) a_nomal)
			(|A|LISTINT (cdr a_ldal) a_nomal a_alin)
		)
		(T
			(setq a_dal (car a_ldal) a_nomb (|O|ITEM 1 a_dal) a_eje (|O|ITEM 3 a_dal) a_lpint (|I|INT2A a_alin a_eje))
			(if a_lpint
				(append (|A|PROCINTS a_lpint) (|A|LISTINT (cdr a_ldal) a_nomal a_alin)) 
				(|A|LISTINT (cdr a_ldal) a_nomal a_alin)
			)
		)
	)
)

;OFFSET: hacer un offset de un alineamiento
;"|A|OFFSET"
(defun |A|OFFSET (a_alin a_dist a_lado a_lay)
	(cond
		((null a_alin) nil)
		(T
			(setq a_seg (car a_alin) a_pp (car a_seg) a_sp (cadr a_seg) a_ts (last a_seg))
			(cond
				((eq a_ts "A")
					(setq a_delta (cadddr a_seg) a_rc (nth 4 a_seg) a_cg (|C|CENTRO a_delta a_rc a_pp a_sp)
					      a_dil (|N|DLADO a_lado a_dist) a_nrc ((|N|-SIGNO a_delta) a_rc a_dil)
					      a_ai (|K|ANG a_cg a_pp) a_af (|K|ANG a_cg a_sp)
					)
					(if (minusp a_delta)
						(|O|ARC a_cg a_nrc a_ai a_af a_lay)
						(|O|ARC a_cg a_nrc a_af a_ai a_lay)
					)
				)
;;;				((or (eq a_ts "E") (eq a_ts "S"))
;;;					(setq a_lpb '())
;;;					(foreach a_pe (|A|REPEE a_seg 100)
;;;						(setq a_pr (car (|A|IDP a_pe g_alin nil)) a_pb (|A|PINL a_pr a_dist a_lado g_alin) a_lpb (cons a_pb a_lpb))
;;;					)
;;;					(|O|LWPOLY a_lpb a_lay 128)
;;;				)
				(T
					(setq a_rumbo (|K|ANG a_pp a_sp)
					      a_pb1 (|K|POLAR a_pp (+ a_rumbo (|N|ANGLADO a_lado)) a_dist)
					      a_pb2 (|K|POLAR a_sp (+ a_rumbo (|N|ANGLADO a_lado)) a_dist)
					)
					(|O|LINE a_pb1 a_pb2 a_lay)
				)
			)
			(|A|OFFSET (cdr a_alin) a_dist a_lado a_lay)
		)
	)
)

;ALAMBRADOS: crear la lista con los alambrados de proyecto
;"|A|ALAMBRADOS"
(defun |A|ALAMBRADOS ()
	(defun |A|CLALAM (a_lenal a_lali a_lald / a_laux a_lado)
		(cond
			((null a_lenal) (list a_lali a_lald))
			(T
				(setq a_ental (entget (car a_lenal)) a_tips (|O|ITEM 0 a_ental) a_lay (|O|ITEM 8 a_ental) a_lado nil a_ns 0 a_handle (|O|ITEM 5 a_ental))
				(if (eq (substr a_lay 1 1) "R") (setq a_nuevo nil) (setq a_nuevo T))
				(cond
					((eq a_tips "LINE")
						(setq a_laux (list (|O|ITEM 10 a_ental) (|O|ITEM 11 a_ental)))
					)
					((eq a_tips "ARC")
						(setq a_cen (|O|ITEM 10 a_ental) a_rc (|O|ITEM 40 a_ental)
						      a_ai (|O|ITEM 50 a_ental) a_af (|O|ITEM 51 a_ental)
						)
						(if (> a_ai a_af) (setq a_af (+ a_af (* 2 PI))))
						(setq a_delta (|C|DELTA nil a_ai a_af)
						      a_laux (mapcar 'car 
								     (|A|REPC (car (|A|LISCUR (|K|XY (|K|POLAR a_cen a_ai a_rc)) (|K|XY (|K|POLAR a_cen a_af a_rc)) a_rc a_delta)) 0.0)
							     )
						)
					)
					((eq a_tips "LWPOLYLINE")
						(setq a_laux (|A|LISVER a_ental)) 
					)
				)
				(while (not a_lado)
					(setq a_pt (nth a_ns a_laux) a_lado (caddr (|A|IDP a_pt g_alin nil)) a_ns (1+ a_ns))
				)
				(if (eq a_lado "I")
					(|A|CLALAM (cdr a_lenal) (cons (list a_nuevo (|A|LISSEG a_laux) a_handle) a_lali) a_lald)
					(|A|CLALAM (cdr a_lenal) a_lali (cons (list a_nuevo (|A|LISSEG a_laux) a_handle) a_lald))
				)
			)
		)
	)
	(|A|CLALAM (|L|SSL (ssget "X" '((8 . "*_Alambrados")))) nil nil)
)

;REPLANTEO: replantear un alineamiento
;"|A|REPLANTEO"
(defun |A|REPLANTEO (a_lprg a_dal)
	(defun |A|OBTAC (a_nbpt)
		(setq a_lisent (cadr (|B|LISTENT a_nbpt)) a_aci nil a_acd nil)
		(foreach a_lent a_lisent
			(setq a_tent (|O|ITEM 8 a_lent))
			(if (wcmatch a_tent "*Calzada")
			  (progn
				(setq a_lv (|A|LISVER a_lent) a_pp (car a_lv) a_up (last a_lv) a_dx (|K|DX a_pp a_up))
				(if (minusp a_dx) (setq a_aci (abs a_dx)) (setq a_acd a_dx))
			  )
			)
		)
		(list a_aci a_acd)
	)
	(defun |A|TITG ()
		(list
			(list (strcase a_nomal))
			(list "")
			(list "PLANILLA DE REPLANTEO DE LA TRAZA")
			(list "")
		)
	)
	(defun |A|TITCS ()
		(list
			(list "")
			(list "Calzada")
			(list nil "Borde Izquierdo" "" "" "Eje" "" "" "Borde Derecho")
			(list "Progresiva" "Norte" "Este" "Elev" "Norte" "Este" "Elev" "Norte" "Este" "Elev" "Características")
		)
	)
	(defun |A|TITCD ()
		(list
			(list "")
			(list nil "Calzada Izquierda" "" "" "" "" "" "Calzada Derecha")
			(list nil "Borde Externo" "" "" "Borde Interno" "" "" "Borde Interno" "" "" "Borde Externo")
			(list "Progresiva" "Norte" "Este" "Elev" "Norte" "Este" "Elev" "Norte" "Este" "Elev" "Norte" "Este" "Elev" "Características")
		)
	)
	(defun |A|CARACT ()
		(list
			(list "")
			(list "CODIGOS DE CARACTERISTICAS")
			(list "PL*" "Punto de Línea")
			(list "TE*" "Tangente Espiral")
			(list "EC*" "Espiral Circular")
			(list "CC*" "Punto Medio Curva Circular")
			(list "CE*" "Circular Espiral")
			(list "ET*" "Espiral Tangente")
			(list "PC*" "Principo de Curva")
			(list "FC*" "Fin de Curva")
			(list "V*" "Vértice Planimétrico")
			(list "PIV" "Vértice Altimétrico")
			(list "PCV" "Principio de Curva Vertical")
			(list "FCV" "Fin de Curva Vertical")
			(list "VP" "Punto de variación de Peralte")
			(list "I+" "Inicio del Evento")
			(list "F+" "Fin del Evento")
			(list "PT" "Perfil Tipo")
		)
	)
	(defun |A|REPLANT (a_lpr a_ta)
		(defun |A|ENNOREP (a_pr a_lprnorep)
			(cond
				((null a_lprnorep) nil)
				((|N|ENTREE a_pr (|L|SUBL (car a_lprnorep) 2))
					T
				)
				(T (|A|ENNOREP a_pr (cdr a_lprnorep)))
			)
		)
		(defun |A|ENVADO (a_pr a_lprv)
			(cond
				((null a_lprv) nil)
				((|N|ENTRE a_pr (|L|SUBL (car a_lprv) 2))
					(car a_lprv)
				)
				(T (|A|ENVADO a_pr (cdr a_lprv)))
			)
		)
		(defun |A|PROCVADO (a_dpr a_dvado)
			(setq a_pr (car a_dpr) a_tip (cadr a_dpr) a_pritp (car a_dvado) a_prftp (cadr a_dvado) a_prmv (caddr a_dvado)
			      a_lvh (cadddr a_dvado) a_sesc (nth 4 a_dvado) a_tipv (last a_dvado)
			      a_lpe (|A|PINS a_pr a_alin) a_pe (car a_lpe) a_dir (cadr a_lpe) a_cras (|P|ELEV a_pr a_ras)
			      a_pt (cdr (|A|CALCVAL a_lperft a_pr)) a_sc (car a_pt) a_ac (cadr a_pt) a_flecha (/ (caddr a_pt) -100) a_nbl (last a_pt)
			      a_lac (|A|OBTAC a_nbl) a_aci (car a_lac) a_acd (cadr a_lac)
			      a_lpsa (|A|CALCVAL a_lpersa a_pr) a_sai (caddr a_lpsa) a_sad (last a_lpsa)
			      a_lpsai (|A|CALCVAL a_lpersa a_pritp) a_perii (/ (car a_lpsai) 100) a_perdi (/ (cadr a_lpsai) 100)
			      a_saii (caddr a_lpsai) a_sadi (last a_lpsai) a_priv (- a_prmv (/ a_lvh 2)) a_prfv (+ a_prmv (/ a_lvh 2))
			      a_lpsaf (|A|CALCVAL a_lpersa a_prftp) a_perif (/ (car a_lpsaf) 100) a_perdf (/ (cadr a_lpsaf) 100)
			      a_saif (caddr a_lpsaf) a_sadf (last a_lpsaf) 
			)
			(if (eq a_sesc "I") (setq a_perim -0.01 a_perdm 0.01) (setq a_perim 0.01 a_perdm -0.01))
			(cond
				((|N|ENTRE a_pr (list a_pritp a_priv))
					(setq a_peri (cadr (|L|INTERP (list a_pritp a_perii) (list a_priv a_perim) a_pr a_pritp a_priv))
					      a_perd (cadr (|L|INTERP (list a_pritp a_perdi) (list a_priv a_perdm) a_pr a_pritp a_priv))
					)
				)
				((|N|ENTRE a_pr (list a_prfv a_prftp))
					(setq a_peri (cadr (|L|INTERP (list a_prfv a_perim) (list a_prftp a_perif) a_pr a_prfv a_prftp))
					      a_perd (cadr (|L|INTERP (list a_prfv a_perim) (list a_prftp a_perdf) a_pr a_prfv a_prftp))
					)
				)
				(T
					(setq a_peri a_perim a_perd a_perdm a_cras (- a_cras 0.02))
					(if (eq a_tipv "V") (setq a_sai 3.0 a_sad 3.0) (setq a_sai 0.0 a_sad 0.0))
				)
			)
			(|A|REPS a_pr a_cras a_pe a_dir a_aci a_acd a_sai a_sad a_peri a_perd a_tip)
		)
		(defun |A|PPVADO (a_lprv)
			(cond
				((null a_lprv) nil)
				(T
					(setq a_dvado (car a_lprv) a_prmv (caddr a_dvado) a_lvh (cadddr a_dvado) a_sesc (nth 4 a_dvado) a_tipv (last a_dvado)
					      a_pt (cdr (|A|CALCVAL a_lperft a_prmv)) a_sc (car a_pt) a_ac (cadr a_pt) a_flecha (/ (caddr a_pt) -100) a_nbl (last a_pt)
					      a_lac (|A|OBTAC a_nbl) a_aci (car a_lac) a_acd (cadr a_lac)
					      a_priv (- a_prmv (/ a_lvh 2)) a_peri (if (eq a_sesc "I") -0.01 0.01) a_perd (if (eq a_sesc "I") 0.01 -0.01)
					      a_sa (if (eq a_tipv "V") 3.0 0.0) a_sai a_sa a_sad a_sa
					      a_cras (|P|ELEV a_priv a_ras) a_lpe (|A|PINS a_priv a_alin) a_pe (car a_lpe) a_dir (cadr a_lpe)
					      a_lptriv (|A|REPS a_priv (- a_cras 0.02) a_pe a_dir a_aci a_acd a_sai a_sad a_peri a_perd (strcat "I" a_tipv))
					      a_prfp (+ a_priv 0.05) a_cras (|P|ELEV a_prfp a_ras) a_lpe (|A|PINS a_prfp a_alin) a_pe (car a_lpe) a_dir (cadr a_lpe)
					      a_lptrfp (|A|REPS a_prfp a_cras a_pe a_dir a_aci a_acd 0.0 0.0 a_peri a_perd "F+Pav")
					      a_lpe (|A|PINS a_prmv a_alin) a_pe (car a_lpe) a_dir (cadr a_lpe) a_cras (|P|ELEV a_prmv a_ras)
					      a_lptrmv (|A|REPS a_prmv (- a_cras 0.02) a_pe a_dir a_aci a_acd a_sai a_sad a_peri a_perd "")
					      a_prfv (+ a_prmv (/ a_lvh 2)) a_cras (|P|ELEV a_prfv a_ras) a_lpe (|A|PINS a_prfv a_alin) a_pe (car a_lpe) a_dir (cadr a_lpe)
					      a_lptrfv (|A|REPS a_prfv (- a_cras 0.02) a_pe a_dir a_aci a_acd a_sai a_sad a_peri a_perd (strcat "F" a_tipv))
					      a_prip (- a_prfv 0.05) a_cras (|P|ELEV a_prip a_ras) a_lpe (|A|PINS a_prip a_alin) a_pe (car a_lpe) a_dir (cadr a_lpe)
					      a_lptrip (|A|REPS a_prip a_cras a_pe a_dir a_aci a_acd 0.0 0.0 a_peri a_perd "I+Pav")
					)
					(append (list a_lptriv a_lptrfp a_lptrmv a_lptrip a_lptrfv) (|A|PPVADO (cdr a_lprv)))
		  		)
			)
		)
		(defun |A|PPR (a_pto a_prg a_cota a_cod)
			(|B|INSERT "PTO" a_pto "R_Replanteo" a_dir 0.25 (list (rtos a_prg 2 2) (rtos a_cota 2 2) a_cod))
		)
		(defun |A|REPS (a_pr a_cras a_pe a_dir a_aci a_acd a_sai a_sad a_peri a_perde a_tip)
			(setq a_acis (+ a_aci a_sai) a_pbi (|K|POLAR a_pe (+ a_dir (/ PI 2)) a_acis) a_cbi (+ a_cras (* a_acis a_peri))
			      a_acds (+ a_acd a_sad) a_pbd (|K|POLAR a_pe (- a_dir (/ PI 2)) a_acds) a_cbd (+ a_cras (* a_acds a_perd))
			)
			(|A|PPR a_pe a_pr a_cras "EJE")
			(|A|PPR a_pbi a_pr a_cbi "BI")
			(|A|PPR a_pbd a_pr a_cbd "BD")
			(list a_pr (|K|PTATOP (|K|Z+ a_pbi a_cbi)) (|K|PTATOP (|K|Z+ a_pe a_cras)) (|K|PTATOP (|K|Z+ a_pbd a_cbd)) a_tip)
		)
		(defun |A|REPD ()
			(setq a_pbii (|K|POLAR a_pe (+ a_dir (/ PI 2)) a_sc) a_aci (+ a_ac a_sai)
			      a_pbei (|K|POLAR a_pbii (+ a_dir (/ PI 2)) a_aci) a_cbei (+ a_cras (* a_aci a_peri))
			      a_pbid (|K|POLAR a_pe (- a_dir (/ PI 2)) a_sc) a_acd (+ a_ac a_sad)
			      a_pbed (|K|POLAR a_pbid (- a_dir (/ PI 2)) a_acd) a_cbed (+ a_cras (* a_acd a_perd))
			)
			(|A|PPR a_pbii a_pr a_cras "BII") (|A|PPR a_pbei a_pr a_cbei "BEI")
			(|A|PPR a_pbid a_pr a_cras "BID") (|A|PPR a_pbed a_pr a_cbed "BED")
			(list a_pr (|K|PTATOP (|K|Z+ a_pbei a_cbei)) (|K|PTATOP (|K|Z+ a_pbii a_cras)) (|K|PTATOP (|K|Z+ a_pbid a_cras)) (|K|PTATOP (|K|Z+ a_pbed a_cbed)) a_tip)
		)
		(cond
			((null a_lpr) nil)
			((|A|ENNOREP (caar a_lpr) a_lprnorep)
				(|A|REPLANT (cdr a_lpr) a_tc)
			)
			((setq a_dvado (|A|ENVADO (caar a_lpr) a_lprvados))
				(cons (|A|PROCVADO (car a_lpr) a_dvado) (|A|REPLANT (cdr a_lpr) a_tc))
			)
			(T
				(setq a_dpr (car a_lpr) a_pr (car a_dpr) a_tip (cadr a_dpr) a_cras (|P|ELEV a_pr a_ras)
				      a_lpe (|A|PINS a_pr a_alin) a_pe (car a_lpe) a_dir (cadr a_lpe)
				      a_pt (cdr (|A|CALCVAL a_lperft a_pr)) a_sc (car a_pt) a_ac (cadr a_pt) a_flecha (/ (caddr a_pt) -100)
				      a_adp (cadddr a_pt) a_adsa (nth 4 a_pt) a_dali (nth 5 a_pt) a_dald (nth 6 a_pt) a_nbl (last a_pt)
				      a_lac (|A|OBTAC a_nbl) a_aci (car a_lac) a_acd (cadr a_lac)
				      a_lpsa (|A|CALCVAL a_lpersa a_pr) a_peri (/ (car a_lpsa) 100) a_perd (/ (cadr a_lpsa) 100)
				      a_sai (caddr a_lpsa) a_sad (last a_lpsa)
				)
;;;				(if (eq a_adp "N") (setq a_peri a_flecha a_perd a_flecha))
				(if (eq a_adsa "N") (setq a_sai 0.0 a_sad 0.0))
				(if (= a_sc 0)
					(setq a_tc "S" a_ltit (|A|TITCS) a_lptr (|A|REPS a_pr a_cras a_pe a_dir a_aci a_acd a_sai a_sad a_peri a_perd a_tip))
					(setq a_tc "D" a_ltit (|A|TITCD) a_lptr (|A|REPD))
				)
				(if (not (eq a_ta a_tc))
					(append a_ltit (list a_lptr) (|A|REPLANT (cdr a_lpr) a_tc))
					(cons a_lptr (|A|REPLANT (cdr a_lpr) a_tc))
				)
			)
		)
	)

	(setq a_nomal (|O|ITEM 1 a_dal) a_alin (|O|ITEM 3 a_dal) a_ras (|O|ITEM 5 a_dal) a_lperft (|O|ITEM 7 a_dal)
	      a_lpersa (|A|LISTPERSA a_nomal a_alin a_lperft) a_lprvados (cadr (|F|LEECSVA a_nomal "vados" 1))
	      a_lprpuentes (cadr (|F|LEECSVA a_nomal "puentes" 1)) a_lprI&R (cadr (|F|LEECSVA a_nomal "retornos" 1))
	      a_lprnorep (|L|INTERC a_lprI&R a_lprpuentes)
	)
	(setq a_lprtraza (|A|REPLANT a_lprg "M") a_lrpvados (|A|PPVADO a_lprvados)
	)
	(append (|A|TITG) (|L|SUBL a_lptraza 4) (|L|INTERC (|L|RESL a_lprtraza 4) a_lrpvados) (|A|CARACT))
)

;;;;BORDES: dibujar los bordes de un alineamiento
;;;;"|A|BORDES"
;;;(defun |A|BORDES (a_lprg)
;;;	(defun |A|REPLANT (a_lpr a_ta)
;;;		(defun |A|PPR (a_pto a_prg a_cota a_cod)
;;;			(|B|INSERT "PTO" a_pto "R_Replanteo" a_dir 0.25 (list (rtos a_prg 2 2) (rtos a_cota 2 2) a_cod))
;;;		)
;;;		(defun |A|REPS ()
;;;			(setq a_aci (+ a_ac a_sai) a_pbi (|K|POLAR a_pe (+ a_dir (/ PI 2)) a_aci) a_cbi (+ a_cras (* a_aci a_peri))
;;;			      a_acd (+ a_ac a_sad) a_pbd (|K|POLAR a_pe (- a_dir (/ PI 2)) a_acd) a_cbd (+ a_cras (* a_acd a_perd))
;;;			)
;;;			(|A|PPR a_pe a_pr a_cras "EJE")
;;;			(|A|PPR a_pbi a_pr a_cbi "BI")
;;;			(|A|PPR a_pbd a_pr a_cbd "BD")
;;;			(list a_pr (|K|PTATOP (|K|Z+ a_pbi a_cbi)) (|K|PTATOP (|K|Z+ a_pe a_cras)) (|K|PTATOP (|K|Z+ a_pbd a_cbd)) a_tip)
;;;		)
;;;		(defun |A|REPD ()
;;;			(setq a_pbii (|K|POLAR a_pe (+ a_dir (/ PI 2)) a_sc) a_aci (+ a_ac a_sai)
;;;			      a_pbei (|K|POLAR a_pbii (+ a_dir (/ PI 2)) a_aci) a_cbei (+ a_cras (* a_aci a_peri))
;;;			      a_pbid (|K|POLAR a_pe (- a_dir (/ PI 2)) a_sc) a_acd (+ a_ac a_sad)
;;;			      a_pbed (|K|POLAR a_pbid (- a_dir (/ PI 2)) a_acd) a_cbed (+ a_cras (* a_acd a_perd))
;;;			)
;;;			(|A|PPR a_pbii a_pr a_cras "BII") (|A|PPR a_pbei a_pr a_cbei "BEI")
;;;			(|A|PPR a_pbid a_pr a_cras "BID") (|A|PPR a_pbed a_pr a_cbed "BED")
;;;			(list a_pr (|K|PTATOP (|K|Z+ a_pbei a_cbei)) (|K|PTATOP (|K|Z+ a_pbii a_cras)) (|K|PTATOP (|K|Z+ a_pbid a_cras)) (|K|PTATOP (|K|Z+ a_pbed a_cbed)) a_tip)
;;;		)
;;;		(cond
;;;			((null a_lpr) nil)
;;;			(T
;;;				(setq a_dpr (car a_lpr) a_pr (car a_dpr) a_tip (cadr a_dpr) a_cras (|P|ELEV a_pr g_ras)
;;;				      a_lpe (|A|PINS a_pr g_alin) a_pe (car a_lpe) a_dir (cadr a_lpe)
;;;				      a_pt (cdr (|A|CALCVAL g_lperft a_pr)) a_sc (car a_pt) a_ac (cadr a_pt) a_flecha (caddr a_pt)
;;;				      a_adp (cadddr a_pt) a_adsa (nth 4 a_pt) a_dali (nth 5 a_pt) a_dald (last a_pt)
;;;				      a_lpsa (|A|CALCVAL g_lpersa a_pr) a_peri (car a_lpsa) a_perd (cadr a_lpsa)
;;;				      a_sai (caddr a_lpsa) a_sad (last a_lpsa)
;;;				)
;;;				(if (eq a_adp "N") (setq a_peri a_flecha a_perd a_flecha))
;;;				(if (eq a_adsa "N") (setq a_sai 0.0 a_sad 0.0))
;;;				(if (= a_sc 0)
;;;					(setq a_tc "S" a_ltit (|A|TITCS) a_lptr (|A|REPS))
;;;					(setq a_tc "D" a_ltit (|A|TITCD) a_lptr (|A|REPS))
;;;				)
;;;				(if (not (eq a_ta a_tc))
;;;					(append a_ltit (list a_lptr) (|A|REPLANT (cdr a_lpr) a_tc))
;;;					(cons a_lptr (|A|REPLANT (cdr a_lpr) a_tc))
;;;				)
;;;			)
;;;		)
;;;	)
;;;
;;;	(append (|A|TITG) (|A|REPLANT a_lprg "M") (|A|CARACT))
;;;)
;;;(defun a_prgee (a_pto a_seg)
;;;	(setq a_te (car a_seg) a_ec (cadr a_seg) a_prf (caddr a_seg) a_seg (cdddr a_seg) a_le (car a_seg) a_tita (cadr a_seg) a_dirt (caddr a_seg) a_rc (/ a_le (abs a_tita)))
;;;	(- a_prf (c_dist_c a_tita a_rc a_te a_ec a_pto))
;;;	(if (minusp a_tita) (setq a_sg (- (/ pi 2)) a_sg2 1) (setq a_sg (/ pi 2) a_sg2 -1))
;;;	(setq a_pa1 (polar a_te (+ a_dirt a_sg) 1000.0) a_pa2 (polar a_ec (+ a_dirt a_sg (- a_tita)) 1000.0) a_pip (inters a_te a_pa1 a_ec a_pa2 nil)
;;;	      a_titap (- (angle a_pip a_pat) (angle a_pip a_te)) a_rc (/ a_le 2 (abs a_tita)) a_k (a_c_k a_rc a_le) a_lx (* a_k (sqrt (* (abs a_titap) 2.0))) 
;;;		a_pie (a_c_ptt (abs a_titap) a_te a_k a_dirt a_sg2)
;;;	)
;;;	(o_lwpoly (list a_pa1 a_pip a_pa2) "0" 0)
;;;	(o_lwpoly (list a_pto a_pip a_pie) "0" 0)
;;;	(+ (- a_prf a_le) a_lx)
;;;)
;;;
;;;(defun a_prges (a_pto a_seg)
;;;	(setq a_ce (car a_seg) a_et (cadr a_seg) a_prf (caddr a_seg) a_seg (cdddr a_seg) a_le (car a_seg) a_tita (cadr a_seg) a_dirt (caddr a_seg) a_rc (/ a_le (abs a_tita)))
;;;	(- a_prf (c_dist_c a_tita a_rc a_ce a_et a_pto))
;;;	(if (minusp a_tita) (setq a_sg (/ pi 2) a_sg2 -1) (setq a_sg (- (/ pi 2)) a_sg2 1))
;;;	(setq a_pa1 (polar a_et (+ a_dirt a_sg) 1000.0) a_pa2 (polar a_ce (+ a_dirt a_sg a_tita) 1000.0) a_pip (inters a_et a_pa1 a_ce a_pa2 nil)
;;;	      a_pat (inters a_pto (polar a_pto (+ (angle a_et a_ce) (/ PI 2)) 1000.0) a_et a_ce nil)
;;;	      a_titap (- (angle a_pip a_pat) (angle a_pip a_et)) a_rc (/ a_le 2 (abs a_tita)) a_k (a_c_k a_rc a_le) a_lx (* a_k (sqrt (* (abs (/ a_titap a_tita)) 2)))
;;;		a_pie (a_c_ptt (abs a_titap) a_et a_k a_dirt a_sg2)
;;;	)
;;;	(o_lwpoly (list a_pa1 a_pip a_pa2) "0" 0)
;;;	(o_lwpoly (list a_pto a_pip a_pie) "0" 0)
;;;	(- a_prf a_lx)
;;;)
;;;

