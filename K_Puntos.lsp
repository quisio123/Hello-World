;"|K|MARCA"
(defun |K|MARCA (k_pt / k_fz)
	(defun |K|LPS (k_po k_an)
		(cond
			((>= k_an (* PI 2)) nil)
			(T (append (list k_po (|K|POLAR k_po k_an k_fz)) (|K|LPS k_po (+ k_an (* PI 0.25)))))
		)
	)
	(redraw)
	(setq k_fz (/ (getvar "VIEWSIZE") 20.0))
	(if k_pt (grvecs (cons 1 (|K|LPS k_pt 0))))
)

;"|K|GETPT"
(defun |K|GETPT (k_lnb k_losn k_msg k_sb / k_resp)
	(princ (strcat "\n" k_msg))
	(while (= (car (setq k_lcod (grread T 1 2))) 5)
		(redraw)
		(if (not k_sb)
		  (progn
			(if k_losn
			  (progn
				(setq k_resp (osnap (cadr k_lcod) k_losn))
				(|K|MARCA k_resp)
			  )
				(setq k_resp (cadr k_lcod))
			)
		  )
		)
		(if k_lnb
			(if (setq k_ss (ssget (cadr k_lcod) '((0 . "INSERT"))))
			  (progn
				(setq k_obn (ssname k_ss 0))
			 	(if (member (|O|ITEM 2 (entget k_obn)) k_lnb)
				  (progn
					(redraw)
					(|K|MARCA (|O|ITEM 10 (entget k_obn)))
					(setq k_resp k_obn)
				  )
				)
			  )
			)
		)
	)
	(redraw)
	(if (= (car k_lcod) 3)
		(if (= (TYPE k_resp) 'ENAME)
			(list (|K|XYZ k_resp) k_resp)
			(if (and k_resp (not k_sb)) (list (|K|XYZ k_resp) nil) nil)
		)
		nil
	)
)

;Listar puntos
;"|K|LISTP"
(defun |K|LISTP (k_lent / k_pt)
	(cond
		((null k_lent) nil)
		(T
			(setq k_ent (car k_lent) k_pt (|O|ITEM 10 (entget k_ent)) k_att2 (|B|ATTVAL k_ent 2))
			(if (eq "~" (substr k_att2 1 1)) (setq k_att2 (substr k_att2 2)))
			(cons (list (atoi (|B|ATTVAL k_ent 1)) (cadr k_pt) (car k_pt) (atof k_att2) (|B|ATTVAL k_ent 3)) (|K|LISTP (cdr k_lent)))
		)
	)
)

;crear la variable lsp
;"|K|CREALSP"
(defun |K|CREALSP (k_lpts)
	(cond
		((null k_lpts) (cons (list ")) ") nil))
		(T
			(cons (list (strcat "(list " (|T|CONCATCH (|L|BULT (car k_lpts)) " ")  " \"" (last (car k_lpts)) "\"" ") ")) (|K|CREALSP (cdr k_lpts)))
		)
	)
)

;Guardar el listado de puntos 
;"|K|GUARDA"
(defun |K|GUARDA ()
 	(if (null g_lpts) (|K|CARGA))
	(|F|GUARDAR g_datos "PUNTOS" "lsp" nil (cons (list "(setq g_lpts (list ") (|K|CREALSP g_lpts)) "" T)
)

;Guardar el listado de puntos
;"|K|EXPORT"
(defun |K|EXPORT ()
 	(if (null g_lpts) (|K|CARGA))
	(|F|GUARDACSV g_datos "PUNTOS" (list (list "ID" "Norte" "Este" "Cota" "Código")) g_lpts T)
)

;Cargar el listado de puntos
;"|K|CARGA"
(defun |K|CARGA ()
	(setq g_lpts (|L|ORDENAR (|K|LISTP (|L|SSL (ssget "X" '((-4 . "<AND") (0 . "INSERT") (2 . "PTO") (-4 . "AND>")))))))
	(if (null g_lpts)
		(load (strcat g_datos "PUNTOS.lsp") "")
	)
	(princ (strcat (itoa (length g_lpts)) " puntos cargados\n"))
)

;renumerar una lista de puntos cambiando su ID
;"|K|RENUM"
(defun |K|RENUM (k_lpts k_nu)
	(mapcar '(lambda (el) (cons k_nu (cdr el)) (setq k_nu (1+ k_nu))) k_lpts)
)

;verificar si un punto es 3d
;"|K|3D"
(defun |K|3D (k_pt)
	(caddr k_pt)
)

; Inrementa la coordenada X de un punto
;"|K|X+"
(defun |K|X+ (k_p k_d)
	(|L|INCN k_p 0 k_d)
)

; Incrementa la coordenada Y de un punto
;"|K|Y+"
(defun |K|Y+ (k_p k_d)
	(|L|INCN k_p 1 k_d)
)

; Incrementa la coordenada Z de un punto
;"|K|Z+"
(defun |K|Z+ (k_p k_d)
	(|L|INCN k_p 2 k_d)
)

; Incrementa la coordenada Z de un punto por un factor de escala
;"|K|ZxD"
(defun |K|ZxD (k_p k_d)
	(list (car k_p) (cadr k_p) (* (caddr k_p) k_d))
)

; Decrementa la coordenada X de un punto
;"|K|X-"
(defun |K|X- (k_p k_d)
	(|L|INCN k_p 0 (- k_d))
)

; Decrementa la coordenada Y de un punto
;"|K|Y-"
(defun |K|Y- (k_p k_d)
	(|L|INCN k_p 1 (- k_d))
)

; Decrementa la coordenada Z de un punto
;"|K|Z-"
(defun |K|Z- (k_p k_d)
	(|L|INCN k_p 2 (- k_d))
)

;calcular un punto a una distancia horizontal de otro en pendiente
;"|K|PTP"
(defun |K|PTP (k_po k_dh k_pend)
	(|K|Y+ (|K|X+ k_po k_dh) (* (abs k_dh) k_pend))
)

;calcular un punto por ángulo y distancia desde un punto dado (2D)
;"|K|POLAR"
(defun |K|POLAR (k_po k_an k_dist)
	(cond
		((|K|3D k_po)
			(list (+ (car k_po) (* k_dist (cos k_an))) (+ (cadr k_po)  (* k_dist (sin k_an))) (caddr k_po))
		)
		(T
			(list (+ (car k_po) (* k_dist (cos k_an))) (+ (cadr k_po)  (* k_dist (sin k_an))))
		)
	)
)

;calcular un punto por ángulo horizontal, pendiente y distancia desde un punto dado (3D)
;"|K|POL3D"
(defun |K|POL3D (k_po k_an k_pend k_dist)
	(setq k_po (|K|XYZ k_po))
	(list (+ (car k_po) (* k_dist (cos k_an))) (+ (cadr k_po)  (* k_dist (sin k_an))) (+ (caddr k_po)  (* k_dist k_pend)))
)

;"|K|PTATOP"
(defun |K|PTATOP (k_p)
	(cond
		((null k_p) nil)
		((|K|3D k_p)
			(list (cadr k_p) (car k_p) (caddr k_p))
		)
		(T
			(list (cadr k_p) (car k_p))
		)
	)
)

;Invertir las coordenadas X e Y de un listado de puntos
;"|K|LPATOP"
(defun |K|LPATOP (k_lpts)
	(cond
		((null k_lpts) nil)
		(T
			(cons (|K|PTATOP (car k_lpts)) (|K|LPATOP (cdr k_lpts))) 
		)
	)
)

;Obtener las coordenadas x e y de un punto
;"|K|XY"
(defun |K|XY (k_pt)
	(cond
		((null k_pt) nil)
		((listp k_pt)
			(list (car k_pt) (cadr k_pt))
		)
		(T
			(|K|XY (|O|ITEM 10 (entget k_pt)))
		)
	)
)

;Obtener las coordenadas x,y,z de un punto
;"|K|XYZ"
(defun |K|XYZ (k_pt)
	(cond
		((null k_pt) nil)
		((listp k_pt)
			(|K|Z+ k_pt 0.0)
		)
		(T
			(append (|K|XY (|O|ITEM 10 (entget k_pt))) (list (|K|COTA k_pt)))
		)
	)
)

;obtener la cota de un punto
;"|K|COTA"
(defun |K|COTA (k_enom / k_ent k_tipo k_txt k_list k_str1 k_str2 k_nbloq)
	(setq k_ent (entget k_enom) k_tipo (|O|ITEM 0 k_ent))
	(cond
		((eq k_tipo "TEXT")
			(atof (|O|ITEM 1 k_ent))
		)
		((eq k_tipo "MTEXT")
			(setq k_txt (|O|ITEM 1 k_ent)
				k_list (|T|SUBCH k_txt ";") k_str1 (car k_list) k_str2 (cadr k_list)
			)
			(if (> (strlen k_str2) 0) (atof k_str2) (atof k_str1))
		)
		((eq k_tipo "INSERT")
			(setq k_nbloq (|O|ITEM 2 k_ent))
			(if (wcmatch k_nbloq "|") (setq k_nbloq (cadr (|T|SUBCH k_nbloq "|"))))
			(cond ((or (eq k_nbloq "PTO") (eq k_nbloq "pto"))
					(atof (|B|ATTVAL k_enom 2))
				)
				((or (eq k_nbloq "P2") (eq k_nbloq "p2"))
					(atof (|B|ATTVAL k_enom 2))
				)
				((or (eq k_nbloq "CBE") (eq k_nbloq "CBI") (eq k_nbloq "CBES") (eq k_nbloq "CBIS"))
					(atof (|B|ATTVAL k_enom 1))
				)
				(T
					(|K|COTA (car (nentsel "\n seleccione la cota del punto ")))
				)
			)
		)
		(T
			(|K|COTA (car (entsel "\n seleccione la cota del punto ")))
		)
	)
)

;obtener la cota de un punto
;"|K|OBTC"
(defun |K|OBTC (k_pt)
	(if g_ltri
		(|K|OBTCT g_ltri k_pt)
		(if g_larg (|K|OBTCCN k_pt) (|K|Z+ k_pt 0.0))
	)
)

(defun |K|OBTCT (k_ltri k_pt)
	(cond
		((null k_ltri) (|K|Z+ k_pt 0.0))
		((|K|ENTRI k_pt (car k_ltri))
			(|GC|INTRP (list (|K|Z+ (|K|XY k_pt) 0.0) (|K|Z+ (|K|XY k_pt) 10000.0)) (car k_ltri))
		)
		(T
			(|K|OBTCT (cdr k_ltri) k_pt)
		)
	)
)

(defun |K|OBTCCN (k_pt / k_di k_lpp)
	(setq k_di 0.0 k_lpp '())
	(repeat 11
		(setq k_lst (|ST|OBTST k_pt (|K|XY (|K|POLAR k_pt k_di 200.0)) (|K|XY (|K|POLAR k_pt (+ k_di PI) 200.0)))
		      k_lprox (|L|OBTPROX k_lst 0.0) k_pp (car k_lprox) k_sp (cadr k_lprox) k_i (|P|I k_pp k_sp)
		)
		(if k_i
			(if (minusp k_i)
				(setq k_lpp (cons (list (abs k_i) k_sp) k_lpp))
				(setq k_lpp (cons (list k_i k_pp) k_lpp))
			)
		)
		(setq k_di (+ k_di (/ PI 12)))
	)
	(if k_lpp
	  (progn
		(setq k_maxp (car (|L|M&M (mapcar 'car k_lpp)))
		      k_lau (assoc k_maxp k_lpp) k_i (car k_lau) k_pa (cadr k_lau)
		)
		(|K|Z+ k_pt (+ (cadr k_pa) (* (abs (car k_pa)) k_i)))
	  )
	)
)

;obtener la cota de un punto por intersección de una recta con un triángulo
;"|K|OBTINT"
(defun |K|OBTINT (k_ltri k_rect / k_pa)
	(cond
		((null k_ltri) nil)
		(T
			(setq k_pa (|GC|INTRP k_rect (car k_ltri)))
			(if (and (|K|ENTRI k_pa (car k_ltri)) (|GC|ENSR k_pa k_rect))
				k_pa
				(|K|OBTINT (cdr k_ltri) k_rect)
			)
		)
	)
)

;Diferencia en X entre dos puntos
;"|K|DX"
(defun |K|DX (k_p1 k_p2)
	(- (car k_p2) (car k_p1))
)

;Diferencia en Y entre dos puntos
;"|K|DY"
(defun |K|DY (k_p1 k_p2)
	(- (cadr k_p2) (cadr k_p1))
)

;Diferencia en Z entre dos puntos
;"|K|DZ"
(defun |K|DZ (k_p1 k_p2)
	(setq k_p1 (|K|XYZ k_p1) k_p2 (|K|XYZ k_p2))
	(- (caddr k_p2) (caddr k_p1))
)

;ángulo entre dos puntos
;"|K|ANG"
(defun |K|ANG (k_p1 k_p2 / k_dx k_dy)
	(cond
		((or (null k_p1) (null k_p2)) nil)
		(T
			(setq k_dx (|K|DX k_p1 k_p2) k_dy (|K|DY k_p1 k_p2))
			(cond
				((and (= k_dx 0) (= k_dy 0)) 0)
				((= k_dx 0)
					(if (> k_dy 0) (/ PI 2) (* PI 1.5))
				)
				((= k_dy 0)
					(if (> k_dx 0) 0.0 PI)
				)
				((< k_dy 0)
					(+ (* PI 2) (atan k_dy k_dx))
				)
				(T (atan k_dy k_dx))
			)
		)
	)
)

;Distancia entre dos puntos
;"|K|DIST"
(defun |K|DIST (k_p1 k_p2)
	(setq k_p1 (|K|XYZ k_p1) K_p2 (|K|XYZ k_p2))
	(sqrt (+ (expt (|K|DX k_p1 k_p2) 2) (expt (|K|DY k_p1 k_p2) 2) (expt (|K|DZ k_p1 k_p2) 2)))
)

;Distancia horizontal entre dos puntos
;"|K|DISTH"
(defun |K|DISTH (k_p1 k_p2)
	(sqrt (+ (expt (|K|DX k_p1 k_p2) 2) (expt (|K|DY k_p1 k_p2) 2)))
)

;Distancia vertical entre dos puntos
;"|K|DISTV"
(defun |K|DISTV (k_p1 k_p2)
	(setq k_p1 (|K|XYZ k_p1) k_p2 (|K|XYZ k_p2))
	(abs (|K|DZ k_p1 k_p2))
)

;obtener el punto medio entre dos puntos
;"|K|PMED"
(defun |K|PMED (k_p1 k_p2)
	(cond
		((|L|3D (list k_p1 k_p2))
			(list (/ (+ (car k_p1) (car k_p2)) 2) (/ (+ (cadr k_p1) (cadr k_p2)) 2) (/ (+ (caddr k_p1) (caddr k_p2)) 2))
		)
		(T
			(list (/ (+ (car k_p1) (car k_p2)) 2) (/ (+ (cadr k_p1) (cadr k_p2)) 2))
		)
	)
)

;Pendiente entre dos puntos
;"|K|PEND"
(defun |K|PEND (k_p1 k_p2)
;;;	(/ (|K|DZ k_p1 k_p2) (|K|DISTH k_p2 k_p1))
	(cond
		((|L|3D (list k_p1 k_p2))
			(/ (|K|DISTV k_p2 k_p1) (|K|DISTH k_p2 k_p1))
		)
		(T
			(/ (|K|DY k_p2 k_p1) (|K|DX k_p2 k_p1))
		)
	)
)

;hallar un circulo dados tres puntos
;"|K|CIRCULO"
(defun |K|CIRCULO (k_pp k_sp k_tp / k_m12 k_n12 k_m13 k_n13 k_c)
	(setq k_m12 (|K|XY (|K|PMED k_pp k_sp)) k_m13 (|K|XY (|K|PMED k_pp k_tp))
	      k_n12 (|K|XY (|K|POLAR k_m12 (+ (/ pi 2) (|K|ANG k_pp k_sp)) 1000.0)) k_n13 (|K|XY (|K|POLAR k_m13 (+ (/ pi 2) (|K|ANG k_pp k_tp)) 1000.0))
	      k_c (inters k_m12 k_n12 k_m13 k_n13 nil)
	)
	(list k_c (|K|DIST k_c (|K|XY k_pp)))
)

;calcular el área de un triángulo
;"|K|AREAT"
(defun |K|AREAT (k_a k_b k_c)
	(abs (/ (+ (- (* (car k_a) (cadr k_b)) (* (cadr k_a) (car k_b)))
		   (- (* (car k_b) (cadr k_c)) (* (cadr k_b) (car k_c)))
		   (- (* (car k_c) (cadr k_a)) (* (cadr k_c) (car k_a)))
		)
		2
	     )
	)
)

;verificar si tres puntos están alineados
;"|K|COLINEAL"
(defun |K|COLINEAL (k_a k_b k_c)
	(equal (|K|AREAT k_a k_b k_c) 0 g_tol)
)

;verificar si un punto cae dentro de un triángulo
;"|K|ENTRI"
(defun |K|ENTRI (k_pt k_tri)
;;;	(|S|DENTRO (|K|XY k_pt) (mapcar '|K|XY (cons (last k_tri) k_tri)))
	(equal (|K|AREAT (car k_tri) (cadr k_tri) (caddr k_tri))
	       (+ (|K|AREAT k_pt (car k_tri) (cadr k_tri)) (|K|AREAT k_pt (cadr k_tri) (caddr k_tri)) (|K|AREAT k_pt (caddr k_tri) (car k_tri)))
	       0.01
	)
)

;devolver el punto con mayor ángulo a un punto dado
;"|K|MAYANG"
(defun |K|>ANG (k_po k_p1 k_p2)
	(cond
		((equal k_po k_p1 g_tol) k_p2)
		((equal k_po k_p2 g_tol) k_p1)
		((equal k_p1 k_p2) k_p1)
		((> (|K|ANG k_po k_p1) (|K|ANG k_po k_p2))
			k_p1
		)
		(T k_p2)
	)
)

;devolver el punto con menor ángulo a un punto dado
;"|K|MINANG"
(defun |K|<ANG (k_po k_p1 k_p2)
	(cond
		((equal k_po k_p1 g_tol) k_p2)
		((equal k_po k_p2 g_tol) k_p1)
		((equal k_p1 k_p2) k_p1)
		((< (|K|ANG k_po k_p1) (|K|ANG k_po k_p2))
			k_p1
		)
		(T k_p2)
	)
)

;"|K|INTERPOLAR"
(defun |K|INTERPOLAR (k_p1 k_p2 k_pi k_tol)
	(cond
		((or (null k_p1) (null k_p2) (null k_pi)) nil)
		((|GC|COLINEAL (|K|XY k_p1) (|K|XY k_p2) (|K|XY k_pi) k_tol)
			(|GC|CPRT (list k_p1 k_p2) (|GC|PARAM (list k_p1 k_p2) (|K|XY k_pi)))
		)
		(T nil)
	)
)

;"|K|ELIMPAR"
(defun |K|ELIMPAR (k_lpts)
	(cond
		((< (length k_lpts) 2) k_lpts)
		((apply 'or (mapcar '(lambda (el) (and (< (|K|DISTH (car k_lpts) el) 0.05) (< (|K|DISTV (car k_lpts) el) 0.02))) (cdr k_lpts)))
			(|K|ELIMPAR (cdr k_lpts))
		)
		(T
			(cons (car k_lpts) (|K|ELIMPAR (cdr k_lpts)))
		)
	)
)

;crear la lista de puntos de un relevamiento
;"|K|OBTPTS"
(defun |K|OBTPTS (k_lpts)
	(cond
		((null k_lpts) nil)
		(T
			(cons (|K|PTATOP (|L|BULT (cdr (car k_lpts)))) (|K|OBTPTS (cdr k_lpts)))
		)
	)
)

;crear la lista de puntos dentro de los límites dados por k_limites
(defun |K|SELPTS (k_lps k_limites)
	(cond
		((null k_lps) nil)
		((|S|DENTRO (|K|XY (car k_lps)) k_limites)
			(cons (car k_lps) (|K|SELPTS (cdr k_lps) k_limites))
		)
		(T
			(|K|SELPTS (cdr k_lps) k_limites)
		)
	)
)

;borrar puntos con z=0
;"|K|BORRAZ0"
(defun |K|BORRAZ0()
	(setq k_lss (|L|SSL (ssget "X" '((-4 . "<AND") (0 . "INSERT") (2 . "PTO") (-4 . "AND>")))))
	(|O|DELENTS (mapcar '(lambda (el) (zerop (|K|COTA el))) k_lss))
	(|K|CARGA)
	(setq g_lptsm (|L|DEPURAR (|K|OBTPTS g_lpts)))
)

;eliminar puntos duplicados
;"|K|ELPTOS"
(defun |K|ELPTOS ()
	(setq k_lss (|L|SSL (ssget "X" '((-4 . "<AND") (0 . "INSERT") (2 . "PTO") (-4 . "AND>")))) K_lpts '())
	(foreach k_obn k_lss
		(setq k_pto (|K|XYZ k_obn))
		(if (member k_pto k_lpts)
			(entdel k_obn)
			(setq k_lpts (cons k_pto k_lpts))
		)
	)
	(|K|CARGA)
	(setq g_lptsm (|L|DEPURAR (|K|OBTPTS g_lpts)))
)

;representar los puntos
;"|K|REPPTS"
(defun |K|REPPTS (k_lptos k_r3)
	(cond
		((null k_lptos) nil)
		(T
			(setq k_pto (car k_lptos) k_id (itoa (car k_pto)) k_este (caddr k_pto) k_norte (cadr k_pto) k_elev (cadddr k_pto) k_cod (car (cddddr k_pto))
			      k_pt (if (= k_r3 3) (list k_este k_norte k_elev) (list k_este k_norte)) k_elev (rtos k_elev 2 3)
			)
			(|B|INSERT "PTO" k_pt "Defpoints" 0.0 0.25 (list k_id k_elev k_cod))
			(|K|REPPTS (cdr k_lptos) k_r3)
		)
	)
)

;"|K|REPPDL"
(defun |K|REPPDL (k_lptos k_r3)
	(cond
		((null k_lptos) nil)
		(T
			(setq k_pto (car k_lptos) k_id (car k_pto) k_pr (caddr k_pto) k_elev (cadr k_pto) k_dis (cadddr k_pto) k_lado (car (cddddr k_pto)) k_cod (last k_pto)
			      k_pt (|A|PINL k_pr k_dis k_lado g_alin) k_cota (rtos k_elev 2 3)
			)
			(if k_r3 (setq k_pti (append k_pt (list k_elev))) (setq k_pti k_pt))
			(|B|INSERT "PTO" k_pt "Defpoints" k_dir 0.25 (list k_id k_cota k_cod))
			(cons (list k_id (|k|PTATOP k_pt) k_cota k_cod) (|K|REPPDL (cdr k_lptos) k_r3))
		)
	)
)

;"|K|REPPP"
(defun |K|REPPP (k_lptos k_po k_ro k_r3)
	(cond
		((null k_lptos) nil)
		((null k_ro) (|K|REPPP k_lptos k_po (/ PI 2) k_r3))
		(T
			(setq k_pto (car k_lptos) k_id (car k_pto) k_a (cadr k_pto) k_dis (caddr k_pto) k_elev (cadddr k_pto) k_cod (las k_pto)
			      k_pt (|K|POLAR (|K|XY k_po) (|N|TOPACAD k_a k_ro) k_dis) k_lado g_alin)
			)
			(if k_elev (setq k_cota (rtos k_elev 2 3)) (setq k_cota "--" k_elev 0.0)
			(if k_r3 (setq k_pt (|K|Z+ k_pt k_elev)) (setq k_pt k_pt))
			(|B|INSERT "PTO" k_pt "Defpoints" 0 0.25 (list k_id k_cota k_cod))
			(cons (list k_id (|k|PTATOP k_pt) k_cota k_cod) (|K|REPPP (cdr k_lptos) k_po k_ro k_r3))
		)
	)
)

;representar los Puntos Fijos
;"|K|PTFC"
(defun |K|PTFC (k_lptf)
	(cond
		((null k_lptf) nil)
		(T
			(setq k_dpt (car k_lptf) k_nom (car k_dpt) k_cx (cadr k_dpt) k_cy (caddr k_dpt) k_cota (cadddr k_dpt) k_mat (last k_dpt) k_pin (list k_cy k_cx)
			      k_idp (|A|IDP k_pin g_alin nil)
			)
			(if k_idp
				(setq k_pr (rtos (car k_idp) 2 2) k_dist (rtos (cadr k_idp) 2 2) k_lado (caddr k_idp) k_nptf (strcat "PFijo" k_lado))
				(setq k_pr "N/A" k_dist "N/A" k_lado "N/A" k_nptf "PFijoI")
			)
			(setq k_latt (list k_nom (strcat "Pr.:" k_pr "-" k_dist "-" k_lado) (strcat "N:" (rtos k_cx 2 2)) (strcat "E:" (rtos k_cy 2 2)) (strcat "Elev:" (rtos k_cota 2 2))))
			(|B|INSERT k_nptf k_pin "Lp_Ptos_Fijos" 0 g_eschi k_latt) ;(|B|EXPLOT (entlast))
			(cons (list k_nom k_cx k_cy k_cota k_pr k_dist k_lado) (|K|PTFC (cdr k_lptf)))
		)
	)
)

;"|K|PTFPDL"
(defun |K|PTFPDL (k_lptf)
	(cond
		((null k_lptf) nil)
		(T
			(setq k_dpt (car k_lptf) k_nom (car k_dpt) k_pr (cadr k_dpt) k_cota (caddr k_dpt) k_lado (cadddr k_dpt) k_dist (nth 4 k_dpt) k_mat (last k_dpt)
			      k_pin (|A|PINL k_pr k_dist k_lado g_alin) k_cx (cadr k_pin) k_cy (car k_pin) k_nptf (strcat "PFijo" k_lado)
			)
			(setq k_latt (list k_nom (strcat "Pr.:" k_pr "-" k_dist "-" k_lado) (strcat "N:" (rtos k_cx 2 2)) (strcat "E:" (rtos k_cy 2 2)) (strcat "Elev:" (rtos k_cota 2 2))))
			(|B|INSERT k_nptf k_pin "Lp_Ptos_Fijos" 0 g_eschi k_latt) (|B|EXPLOT (entlast))
			(cons (list k_nom k_cx k_cy k_cota k_pr k_dist k_lado) (|K|PTFPDL (cdr k_lptf)))
		)
	)
)

;ordenar una lista de puntos por su distancia a un punto dado
;"|K|ORDDC"
(defun |K|ORDDC (k_lp k_ce)
	(defun |K|LDIS (k_lpt k_cn)
		(cond
			((null k_lpt) nil)
			((> (length k_lp) 10000)
				(setq k_divm (|L|DIVM k_lp))
				(append (|K|LDIS (car k_divm) k_cn) (|K|LDIS (cadr k_divm) k_cn))
			)
			(T (cons (append (list (|K|DISTH (car k_lpt) k_cn)) (car k_lpt)) (|K|LDIS (cdr k_lpt) k_cn)))
		)
	)

	(|L|CDRSL (|L|ORDENAR (|K|LDIS k_lp k_ce)))
)

;ordenar una lista de puntos por su progresiva en el alineamiento activo
;"|K|ORDPR"
(defun |K|ORDPR (k_lp)
	(defun |K|LPR (k_lpt k_cn)
		(cond
			((null k_lpt) nil)
			(T (cons (cons (car (|A|IDP (car k_lpt) g_alin nil)) (car k_lpt)) (|K|LPR (cdr k_lpt))))
		)
	)

	(|L|CDRSL (|L|ORDENAR (|K|LPR k_lp)))
)

;calcular el centro de una nube de puntos
;"|K|CENTRO"
(defun |K|CENTRO (k_lv)
	(setq k_mm (|L|M&M k_lv) k_lx (car k_mm) k_ly (cadr k_mm) k_maxx (car k_lx) k_minx (cadr k_lx) k_maxy (car k_ly) k_miny (cadr k_ly))
	(list (+ (/ (- k_maxx k_minx) 2.0) k_minx) (+ (/ (- k_maxy k_miny) 2.0) k_miny))
)

;verificar si un punto cae dentro de un círculo
;"|K|ENCIRC"
(defun |K|ENCIRC (k_pt k_cir)
	(< (|K|DISTH k_pt (car k_cir)) (cadr k_cir))
)

;hallar un punto
;"|K|BUSCA"
(defun |K|BUSCA (k_ptb k_lpts)
	(cond
		((null k_lpts) nil)
		(T
			(setq k_dpt (car k_lpts) k_pt (cdr (|L|BULT k_dpt)))
			(if (equal k_ptb k_pt g_tol)
				k_dpt
				(|K|BUSCA k_ptb (cdr k_lpts))
			)
		)
	)
)

;borrar los puntos del dibujo
;"|K|BORRA"
(defun |K|BORRA ()
	(|O|DELENTS (|L|SSL (ssget "X" '((-4 . "<AND") (0 . "INSERT") (2 . "PTO") (-4 . "AND>")))))
)

;borrar puntos
;"|K|DELPTS"
(defun |K|DELPTS (k_lnpt / k_ept)
	(cond
		((null k_lnpt) nil)
		(T
			(setq k_ept (|K|XYZ (car k_lnpt)) g_lptsm (|L|BORRA k_ept g_lptsm) k_pt (|K|BUSCA (|K|PTATOP k_ept) g_lpts))
			(entdel (car k_lnpt))
			(cons k_pt (|K|DELPTS (cdr k_lnpt))) 
		)
	)
)

;mostrar los puntos en el dibujo
;"|K|VERPTS"
(defun |K|VERPTS (k_lpts)
	(|K|REPPTS k_lpts nil)
)

(defun |K|LENTP (k_lent / k_pt)
	(cond
		((null k_lent) nil)
		(T
			(setq k_ent (car k_lent) k_pt (|O|ITEM 10 (entget k_ent)))
			(cons (list k_ent (list (atoi (|B|ATTVAL k_ent 1)) (cadr k_pt) (car k_pt) (atof (|A|ATTVAL k_ent 2)) (|A|ATTVAL k_ent 3))) (|K|LENTP (cdr k_lent)))
		)
	)
)

;"|K|BPTS"
(defun |K|BPTS (k_lpts)
	(defun |K|BENTP (k_lent)
		(cond
			((null k_lent) nil)
			((member (cadar k_lent) k_lpts)
				(entdel (caar k_lent))
				(|K|BENTP (cdr k_lent))
			)
			(T
				(|K|BENTP (cdr k_lent))
			)
		)
	)
	(|K|BENTP (|K|LENTP (|L|SSL (ssget "X" '((-4 . "<AND") (0 . "INSERT") (2 . "PTO") (-4 . "AND>"))))))
)

;crear grupos de puntos según su código
;"|K|GRUPO"
(defun |K|GRUPO (k_lpts k_strc)
	(cond
		((null k_lpts) nil)
		((wcmatch (last (car k_lpts)) k_strc)
			(cons (car k_lpts) (|K|GRUPO (cdr k_lpts) k_strc))
		)
		(T
			(|K|GRUPO (cdr k_lpts) k_strc)
		)
	)
)

;"|K|GRUPOS"
(defun |K|GRUPOS ()
	(setq g_grupts
	       (list
		  (list "Apoyo" (|K|GRUPO g_lpts "PF*,AUX*,BASE*"))
		  (list "Accesos" (|K|GRUPO g_lpts "*ACC*,*TRQ*,*TRAN*,*POR*,*PUER*,*GAR*,*UMB*"))
		  (list "LMT" (|K|GRUPO g_lpts "*LMT*"))
		  (list "LBT" (|K|GRUPO g_lpts "*LBT*"))
		  (list "LAT" (|K|GRUPO g_lpts "*LAT*"))
		  (list "FO" (|K|GRUPO g_lpts "FO,M*FO"))
		  (list "Desagües" (|K|GRUPO g_lpts "*ALC*,*CDE*,*CDS*,*CU*,*DES*,*CANAL*,P-A,F*R"))
		  (list "Alambrados" (|K|GRUPO g_lpts "*AL,*AL-*,*ALAM*,*ALM*,*ESQ*,*AO*,*TEJID*"))
		  (list "Ejes" (|K|GRUPO g_lpts "*EJE*,*E*PAV*,*E*-C*"))
		  (list "FFCC" (|K|GRUPO g_lpts "*FFCC*,*VIAS*,*RIEL*"))
		  (list "Taludes" (|K|GRUPO g_lpts "*BB*,*BT*,*PT*"))
		  (list "Pavimentos" (|K|GRUPO g_lpts "*BPAV*,*BCAL*,*B-PAV*,*B-CAL*"))
		  (list "Caminos" (|K|GRUPO g_lpts "*B*CAM*"))
		  (list "Cordones" (|K|GRUPO g_lpts "*CC*,*CM*"))
		  (list "Puentes" (|K|GRUPO g_lpts "*PTE*,*EST*,*LO?A*"))
		  (list "Terreno" (|K|GRUPO g_lpts "*TN*"))
		  (list "Edificaciones" (|K|GRUPO g_lpts "*ED*,*LDE*,*MUR*,*CASA*,*ESCU*,*LM*,*GRU*,*MON*"))
		  (list "Vegetación" (|K|GRUPO g_lpts "ARBOL"))
		)
	      g_grupts (append g_grupts (list (list "Otros" (|L|LIBRES g_lpts (apply 'append (mapcar 'car (|L|CDRSL g_grupts))) nil))))
	)
)

(DEFUN |K|SELG ()
	(setq k_lgrups (|L|NUMERAR (mapcar 'car g_grupts) nil 1))
	(textscr)
	(foreach k_ngr k_lgrups
		(princ (strcat "\n" (itoa (car k_ngr)) " - " (cdr k_ngr)))
	)
	(setq k_numg (getint "\n número de grupo: "))
	(graphscr)
	(|O|ITEM (assoc k_numg k_lgrups) g_grupts)
)

(defun |K|OBTPTSM ()
	(setq k_lss (|L|SSL (ssget "X" '((0 . "POINT")))) g_lptsm '() k_an 0.0)
	(foreach k_ept k_lss
		(setq k_dpt (entget k_ept) k_pt (|O|ITEM 10 k_dpt) g_lptsm (cons (|K|POLAR k_pt k_an 0.01) g_lptsm) k_an (+ k_an 0.01))
	)
)

;transformación de coordenadas
(defun |K|TRANS (k_p k_id)
	(setq k_layout (getvar "CTAB"))
	(cond
		((null k_p) nil)
		((and k_id (> k_id 1))
			(setq k_nvp (|O|GETVPN k_id k_layout) k_vp (entget k_nvp) k_cps (|O|ITEM 10 k_vp) k_cvp (|O|ITEM 12 k_vp) k_target (|O|ITEM 17 k_vp)
			      k_fesc (/ (|O|ITEM 41 k_vp) (|O|ITEM 45 k_vp)) k_vtwist (|O|ITEM 51 k_vp) k_ucso (|O|ITEM 110 k_vp)
			      k_cms (|K|POLAR (|K|XY k_target) (- (|K|ANG k_ucso k_cvp) k_vtwist) (|K|DIST k_ucso k_cvp))
			)
			(list (|K|POLAR k_cps (+ (|K|ANG k_cms k_p) k_vtwist) (* (|K|DISTH k_p k_cms) k_fesc)) k_nvp)
		)
		((eq k_layout "Model") (list k_p nil))
		(T
			(setq k_nvp (|O|GETVP k_p k_layout) k_vp (entget k_nvp) k_cps (|O|ITEM 10 k_vp) k_cvp (|O|ITEM 12 k_vp) k_target (|O|ITEM 17 k_vp)
			      k_fesc (/ (|O|ITEM 41 k_vp) (|O|ITEM 45 k_vp)) k_vtwist (|O|ITEM 51 k_vp) k_ucso (|O|ITEM 110 k_vp)
			      k_cms (|K|POLAR (|K|XY k_target) (- (|K|ANG k_ucso k_cvp) k_vtwist) (|K|DIST k_ucso k_cvp))
			)
			(list (|K|POLAR k_cms (- (|K|ANG k_cps k_p) k_vtwist) (/ (|K|DISTH k_p k_cps) k_fesc)) k_nvp)
		)
	)
)

(defun |K|BPTPS (k_pms k_lvp)
	(cond
		((null k_lvp) nil)
		(T
			(setq k_avp (car k_lvp) k_id (car k_avp) k_eii (cadr k_avp) k_esd (caddr k_avp) k_esi (list (car k_eii) (cadr k_esd)) k_eid (list (car k_esd) (cadr k_eii))
			      k_pa (car (|K|TRANS k_pms (car k_avp))) k_layout (getvar "CTAB")
			)
			(if (and (>= (car k_pa) (car k_eii)) (>= (cadr k_pa) (cadr k_eii)) (<= (car k_pa) (car k_esd)) (<= (cadr k_pa) (cadr k_esd)))
;;;			(if (|S|DENTRO k_pa (list k_eii k_esi k_esd k_eid))
			  (progn
			  	(setq k_nvp (|O|GETVPN k_id k_layout) k_vtwist (|O|ITEM 51 (entget k_nvp)))
				(list k_pa k_vtwist)
			  )
				(|K|BPTPS k_pms (cdr k_lvp))
			)
		)
	)
)