;; CeoCal.LSP Copyright 2000 Víctor Arinci.
;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;;    Author: Víctor Daniel Arinci,
;;           
;;            victorarinci@yahoo.com.ar
;;
;	Rutinas para Geometría de Coordenadas.
;	
;
; Víctor Arinci
(setq gc_tol 0.00001)
;---------------------------------------------------------------------------------------------------------------------------------------------------------
;  					V	E	C	T	O	R	E	S
;---------------------------------------------------------------------------------------------------------------------------------------------------------
;igualar cantidad de componentes de vectores
(defun |GC|=NCOMP (gc_vl gc_vc)
	(cond
		((null gc_vl) nil)
		((null gc_vc) (cons 0.0 (|GC|=NCOMP (cdr gc_vl) nil)))
		(T (cons (car gc_vc) (|GC|=NCOMP (cdr gc_vl) (cdr gc_vc))))
	)
)

;sumar vectores
;"|GC|SUMV"
(defun |GC|SUMV (gc_u gc_v)
	(cond
		((null gc_u) nil)
		((< (length gc_v) (length gc_u)) (|GC|SUMV gc_u (|GC|=NCOMP gc_u gc_v)))
		((> (length gc_v) (length gc_u)) (|GC|SUMV (|GC|=NCOMP gc_v gc_u) gc_v))
		(T
			(cons (+ (car gc_u) (car gc_v)) (|GC|SUMV (cdr gc_u) (cdr gc_v)))
		)
	)
)

;restar vectores
;"|GC|RESV"
(defun |GC|RESV (gc_u gc_v)
	(cond
		((null gc_u) nil)
		((< (length gc_v) (length gc_u)) (|GC|RESV gc_u (|GC|=NCOMP gc_u gc_v)))
		((> (length gc_v) (length gc_u)) (|GC|RESV (|GC|=NCOMP gc_v gc_u) gc_v))
		(T
			(cons (- (car gc_u) (car gc_v)) (|GC|RESV (cdr gc_u) (cdr gc_v)))
		)
	)
)

;calcular el módulo de un vector
;"|GC|MODV" = raiz(u1^2+u2^2+u3^2)
(defun |GC|MOD (gc_v)
	(cond
		((null gc_v) nil)
		((|K|3D gc_v)
			(sqrt (+ (expt (car gc_v) 2) (expt (cadr gc_v) 2) (expt (caddr gc_v) 2)))
		)
		(T (|GC|MOD (|K|Z+ gc_v 0.0)))
	)
)

;calcular el producto de un escalar por un vector
;"|GC|E*V"
(defun |GC|E*V (gc_esc gc_vec)
	(mapcar '(lambda (el) (* gc_esc el)) gc_vec)
)

;calcular el producto escalar de dos vectores
;"|GC|V.V" u . v = |u| . |v| . (cos u,v)
(defun |GC|V.V (gc_u gc_v)
	(cond
		((null gc_u) 0)
		((< (length gc_v) (length gc_u)) (|GC|V.V gc_u (|GC|=NCOMP gc_u gc_v)))
		((> (length gc_v) (length gc_u)) (|GC|V.V (|GC|=NCOMP gc_v gc_u) gc_v))
		(T
			(+ (* (car gc_u) (car gc_v)) (|GC|V.V (cdr gc_u) (cdr gc_v)))
		)
	)
)

;calcular el producto vectorial de dos vectores
;"|GC|V*V" 
(defun |GC|V*V (gc_u gc_v)
	(setq gc_u (|K|Z+ gc_u 0.0) gc_v (|K|Z+ gc_v 0.0))
	(list (- (* (cadr gc_u) (caddr gc_v)) (* (caddr gc_u) (cadr gc_v))) (- (* (caddr gc_u) (car gc_v)) (* (car gc_u) (caddr gc_v))) (- (* (car gc_u) (cadr gc_v)) (* (cadr gc_u) (car gc_v))))
)

;calcular el vector de dirección de una recta
;"|GC|VECT"
(defun |GC|VECT (gc_recta)
	(|GC|RESV (cadr gc_recta) (car gc_recta))
)

;calcular el vector de dirección perpendicular a un plano
;"|GC|VECTN"
(defun |GC|VECTN (gc_plano)
	(|GC|V*V (|GC|VECT (|L|BULT gc_plano)) (|GC|VECT (cdr gc_plano)))
)

;calcular el versor correspondiente a un vector (módulo del versor = 1)
;"|GC|VERS"
(defun |GC|VERS (gc_v)
	(setq gc_v (|K|Z+ gc_v 0.0))
	(if (= (setq gc_mod (|GC|MOD gc_v)) 0) nil
		(list (/ (car gc_v) gc_mod) (/ (cadr gc_v) gc_mod) (/ (caddr gc_v) gc_mod))
	)
)

;calcular el versor inverso correspondiente a un vector (módulo del versor = 1)
;"|GC|VINV"
(defun |GC|VINV (gc_vs)
	(list (- (car gc_vs)) (- (cadr gc_vs)) (- (caddr gc_vs)))
)

;calcular un versor dado un punto y un ángulo en el plano XY
;"|GC|VPA"
(defun |GC|VPA (gc_po gc_an)
	(setq gc_po (|K|Z+ gc_po 0.0))
	(list gc_po (list (+ (car gc_po) (* 1 (cos gc_an))) (+ (cadr gc_po)  (* 1 (sin gc_an))) (caddr gc_po)))
)

;calcular el ángulo en el plano XY de un vector
;"|GC|ANGV"
(defun |GC|ANGV (gc_v)
	(cond
		((equal gc_v '(0 0) gc_tol) 0)
		((= (car gc_v) 0)
			(if (> (cadr gc_v) 0) (/ PI 2) (* PI 1.5))
		)
		((= (cadr gc_v) 0)
			(if (> (car gc_v) 0) 0 PI)
		)
		(T (|N|DOSPI (atan (cadr gc_v) (car gc_v))))
	)
)

;---------------------------------------------------------------------------------------------------------------------------------------------------------
;  O   P   E   R   A   C   I   O   N   E   S      C   O   N      P   U   N   T   O   S   ,      L   I   N   E   A   S      Y      P   L   A   N   O   S
;---------------------------------------------------------------------------------------------------------------------------------------------------------
;calcular el parámetro desde el primer punto de definición de la recta hasta un punto
;"|GC|PARAM"
(defun |GC|PARAM (gc_recta gc_pt)
	(if (= (car (|GC|VECT gc_recta)) 0.0) 0.0
		(/ (- (car gc_pt) (caar gc_recta)) (car (|GC|VECT gc_recta)))
	)
)

;calcular un punto en una recta desde el primer punto de definición de la recta dada una distancia
;"|GC|CPRD"
(defun |GC|CPRD (gc_recta gc_dist)
	(|GC|SUMV (car gc_recta) (|GC|E*V gc_dist (|GC|VERS (|GC|VECT gc_recta))))
)

;calcular un punto en una recta desde el primer punto de definición de la recta dado el parámetro
;"|GC|CPRT"
(defun |GC|CPRT (gc_recta gc_t)
	(|GC|SUMV (car gc_recta) (|GC|E*V gc_t (|GC|VECT gc_recta)))
)

;calcular el punto medio de un segmento
;"|GC|PTM"
(defun |GC|PTM (gc_seg)
	(|GC|CPRT gc_seg 0.5)
)

;calcular un punto a partir de otro dados un ángulo y una distancia "el punto devuelto tendrá la misma coordenada z que el punto de origen"
;"|GC|PAD"
(defun |GC|PAD (gc_or gc_an gc_di)
	(|GC|CPRT (|GC|VPA gc_or gc_an) gc_di)	
)

;calcular un punto a partir de otro dados un ángulo horizontal, un ángulo vertical y una distancia horizontal
;"|GC|PADH"
(defun |GC|3DPADH (gc_or gc_anh gc_anv gc_dh / gc_dv)
	(setq gc_dv (/ gc_dh (cos gc_anv)))
	(|K|Z+ (|GC|PAD gc_or gc_anh gc_dh) (* gc_dv (sin gc_anv)))	
)

;calcular un punto a partir de otro dados un ángulo horizontal, un ángulo vertical y una distancia
;"|GC|PADV"
(defun |GC|3DPADV (gc_or gc_anh gc_anv gc_dv / gc_dv)
	(setq gc_dh (* gc_dv (cos gc_anv)))
	(|K|Z+ (|GC|PAD gc_or gc_anh gc_dh) (* gc_dv (sin gc_anv)))	
)

;calcular el ángulo entre dos rectas
;"|GC|ANG2R"
(defun |GC|ANG2R (gc_r1 gc_r2 / gc_u gc_v)
	(setq gc_u (|GC|VECT gc_r1) gc_v (|GC|VECT gc_r2))
	(|N|ACOS (/ (|GC|V.V gc_u gc_v) (* (|GC|MOD gc_u) (|GC|MOD gc_v))))
)

;calcular el ángulo entre una recta y un plano
;"|GC|ANGRP"
(defun |GC|ANGRP (gc_r gc_p / gc_u gc_v)
	(setq gc_u (|GC|VECT gc_r) gc_v (|GC|VECTN gc_p))
	(|N|ACOS (/ (|GC|V.V gc_u gc_v) (* (|GC|MOD gc_u) (|GC|MOD gc_v))))
)

;calcular el ángulo entre dos planos
;"|GC|ANG2P"
(defun |GC|ANG2P (gc_p1 gc_p2 / gc_u gc_v)
	(setq gc_u (|GC|VECTN gc_p1) gc_v (|GC|VECTN gc_p2))
	(|N|ACOS (/ (|GC|V.V gc_u gc_v) (* (|GC|MOD gc_u) (|GC|MOD gc_v))))
)

;calcular la distancia de un punto a una recta
;"|GC|DISTPR"
(defun |GC|DISTPR (gc_pt gc_r / gc_d gc_vpr)
	(setq gc_d (|GC|VECT gc_r) gc_vpr (|GC|VECT (list gc_pt (car gc_r))))
	(abs (/ (|GC|MOD (|GC|V*V gc_vpr gc_d)) (|GC|MOD gc_d)))
)

;calcular la proyección de un punto en una recta
;"|GC|PROYPR"
(defun |GC|PROYPR (gc_pt gc_r / gc_vr gc_vpr)
	(setq gc_vr (|GC|VECT gc_r) gc_vpr (|GC|VECT (list gc_pt (car gc_r))))
	(|GC|CPRT gc_r (- (/ (|GC|V.V gc_vpr gc_vr) (expt (|GC|MOD gc_vr) 2))))
)

;calcular la distancia de un punto a un plano.
;"|GC|DISTPP"
(defun |GC|DISTPP (gc_pt gc_p / gc_vp gc_vpt)
	(setq gc_vp (|GC|VECTn gc_p) gc_vpt (|GC|VECT (list (car gc_p) gc_pt)))
	(abs (/ (|GC|V.V gc_vp gc_vpt) (|GC|MOD gc_vp)))
)

;calcular la poyección de un punto en un plano
;"|GC|PROYPP"
(defun |GC|PROYPP (gc_pt gc_p / gc_vp gc_vpt gc_pa)
	(setq gc_vp (|GC|VECTn gc_plano) gc_vpt (|GC|SUMV gc_pt gc_vp)
	      gc_pa (|GC|CPRD (list gc_pt gc_vpt) (|GC|DISTPP gc_pt gc_p))
	)
	(if (= (|GC|DISTPP gc_pa gc_p) 0.0)
		gc_pa
		(|GC|CPRD (list gc_pt gc_vpt) (- (|GC|DISTPP gc_pt gc_p)))
	)
)

;calcular la distancia entre dos rectas
;"|GC|DIST2R"
(defun |GC|DIST2R (gc_r1 gc_r2 / gc_da gc_uv gc_uvm)
	(if (equal (|GC|ANG2R gc_r1 gc_r2) 0 gc_tol)
		(|GC|DISTPR (car gc_r1) gc_r2)
		(progn
			(setq gc_uv (|GC|V*V (|GC|VECT gc_r1) (|GC|VECT gc_r2)) gc_uvm (|GC|MOD gc_uv)
			      gc_da (|GC|V.V (|GC|VECT (list (car gc_r1) (car gc_r2))) gc_uv)
			)
			(abs (/ gc_da gc_uvm))
		)
	)
)

;calcular la distancia de una recta a un plano
;"|GC|DISTRP"
(defun |GC|DISTRP (gc_r gc_p)
	(if (= (|GC|ANGRP gc_r gc_p) 0) (|GC|DISTPP (car gc_r) gc_p) 0)
)

;calcular la distancia entre dos planos
;"|GC|DIST2P"
(defun |GC|DIST2P (gc_p1 gc_p2)
	(if (= (|GC|ANG2P gc_p1 gc_p2) 1) (|GC|DISTPP (car gc_p1) gc_p2) 0)
)

;determinar si tres puntos están alineados
;"|GC|COLINEAL"
(defun |GC|COLINEAL (gc_p1 gc_p2 gc_p3 gc_tol)
	(equal (|GC|DISTPR gc_p3 (list gc_p2 gc_p1)) 0.0 gc_tol)
)

;calcular la intersección de dos rectas
;"|GC|INT2R"
(defun |GC|INT2R (gc_r1 gc_r2 / gc_p1)
	(cond
		((or (null gc_r1) (null gc_r2)) nil)
		((equal (|GC|ANG2R gc_r1 gc_r2) 0 g_tol) nil)
		((equal (|GC|DIST2R gc_r1 gc_r2) 0 gc_tol)
			(cond
				((and (|GC|COLINEAL (car gc_r1) (cadr gc_r1) (car gc_r2) 0.01) (|GC|COLINEAL (car gc_r1) (cadr gc_r1) (cadr gc_r2) 0.01)) nil)
				(T
					(setq gc_p1 (|GC|CPRD gc_r1 (/ (|GC|DISTPR (car gc_r1) gc_r2) (sin (|GC|ANG2R gc_r1 gc_r2)))))
				  	(if (equal (|GC|DISTPR gc_p1 gc_r2) 0.0 gc_tol) gc_p1
						(|GC|CPRD gc_r1 (- (/ (|GC|DISTPR (car gc_r1) gc_r2) (sin (|GC|ANG2R gc_r1 gc_r2)))))
					)
				)
			)
		)
		(T nil)
	)
)

;calcular la intersección de dos segmentos de recta
;"|GC|ENSR"
(defun |GC|ENSR (gc_pt gc_seg)
	(if gc_pt
		(and (|GC|COLINEAL (car gc_seg) (cadr gc_seg) gc_pt 0.01) (|N|ENTRE (|GC|PARAM gc_seg gc_pt) '(0.0 1.0)))
		nil
	)
)

;"|GC|INTSS"
(defun |GC|INTSS (gc_s1 gc_s2 / gc_pi)
	(setq gc_pi (|GC|INT2R gc_s1 gc_s2))
	(if (and gc_pi (|GC|ENSR gc_pi gc_s1) (|GC|ENSR gc_pi gc_s2)) (list gc_pi) nil)
)

;Intersección entre recta y plano
;"|GC|INTRP"
(defun |GC|INTRP (gc_r gc_pl / gc_vr gc_vp gc_d)
	(setq gc_d1 (|GC|DISTPP (car gc_r) gc_pl) gc_d2 (|GC|DISTPP (cadr gc_r) gc_pl))
	(cond
		((= gc_d1 gc_d2) nil)
		((= gc_d1 0) (car gc_r))
		((= gc_d2 0) (cadr gc_r))
		(T
			(setq gc_vr (|GC|VECT gc_r) gc_vp (|GC|VECTN gc_pl) gc_d (- (|GC|V.V gc_vp (car gc_pl)))
			      gc_t (- (/ (+ (|GC|V.V gc_vp (car gc_r)) gc_d) (|GC|V.V gc_vp gc_vr)))
			)
			(|GC|CPRT gc_r gc_t)
		)
	)
)

;Intersección entre dos planos
;"|GC|INTPP"
(defun |GC|INTPP (gc_p1 gc_p2)
	(|L|SUBL
		(|L|PURGAR
			(list
				(|GC|INTRP (|L|BULT gc_p1) gc_p2)
				(|GC|INTRP (cdr gc_p1) gc_p2)
				(|GC|INTRP (list (car gc_p1) (last gc_p1)) gc_p2)
				(|GC|INTRP (|L|BULT gc_p2) gc_p1)
				(|GC|INTRP (cdr gc_p2) gc_p1)
				(|GC|INTRP (list (car gc_p2) (last gc_p2)) gc_p1)
			)
		)
		2
	)
)

;Baricentro entre 3 puntos
;"|GC|BARIC"
(defun |GC|BARIC (gc_p1 gc_p2 gc_p3)
	(if (not (|GC|COLINEAL gc_p1 gc_p2 gc_p3 0.01))
		(|GC|E*V (/ 1.0 3.0) (|GC|SUMV (|GC|SUMV gc_p1 gc_p2) gc_p3))
		nil
	)
)

;Interseccines entre recta y circulo
;"|GC|INTRC"
(defun |GC|INTRC (gc_r gc_circ / gc_dcr gc_pai)
	(setq gc_dcr (|GC|DISTPR (car gc_circ) gc_r) gc_pai (|GC|PROYPR (car gc_circ) gc_r))
	(cond
		((< gc_dcr (cadr gc_circ))
	     		(list (|GC|CPRD (list gc_pai (car gc_r)) (sqrt (- (expt (cadr gc_circ) 2) (expt gc_dcr 2))))
			      (|GC|CPRD (list gc_pai (car gc_r)) (- (sqrt (- (expt (cadr gc_circ) 2) (expt gc_dcr 2)))))
			)
		)
		((equal gc_dcr (cadr gc_circ) gc_tol)
			(list gc_pai)
		)
		(T nil)
	)
)

;intersecciones entre dos círculos
;"|GC|INT2C"
(defun |GC|INT2C (gc_c1 gc_c2 / gc_r1 gc_r2 gc_cg1 gc_cg2 gc_bt gc_ac gc_dc)
	(cond
		((= (|K|DIST (car gc_c1) (car gc_c2)) 0) nil)
		((> (|K|DIST (car gc_c1) (car gc_c2)) (+ (cadr gc_c1) (cadr gc_c2))) nil)
		((= (|K|DIST (car gc_c1) (car gc_c2)) (+ (cadr gc_c1) (cadr gc_c2))) (|GC|CALCPRD (list (car gc_c1) (car gc_c2)) (cadr gc_c1)))
		(T
			(setq gc_cg1 (car gc_c1) gc_r1 (cadr gc_c1) gc_cg2 (car gc_c2) gc_r2 (cadr gc_c2) gc_dc (|K|DIST gc_cg1 gc_cg2)
			      gc_supr (- (+ gc_r1 gc_r2) gc_dc)
;;;			      gc_bt (|N|ACOS (/ (- (+ (expt gc_r1 2) (expt gc_dc 2)) (expt gc_r2 2)) (* 2 gc_r1 gc_dc)))
			)
			(cond
				((> gc_r1 gc_2)
					(setq gc_ac (|K|ANG gc_cg1 gc_cg2) gc_di1 (- gc_r1 gc_supr) gc_pa (|K|POLAR gc_cg1 gc_ac gc_di1) gc_di2 (- (expt gc_r1 2) (expt gc_di1 2)))
					(list
						(|K|POLAR gc_pa (+ gc_ac (/ PI 2)) gc_di2)
						(|K|POLAR gc_pa (+ gc_ac (* PI 1.5)) gc_di2)
					)
				)
				(T
					(setq gc_ac (|K|ANG gc_cg2 gc_cg1) gc_di1 (- gc_r2 gc_supr) gc_pa (|K|POLAR gc_cg2 gc_ac gc_di1) gc_di2 (- (expt gc_r2 2) (expt gc_di1 2)))
					(list
						(|K|POLAR gc_pa (+ gc_ac (/ PI 2)) gc_di2)
						(|K|POLAR gc_pa (+ gc_ac (* PI 1.5)) gc_di2)
					)
				)
			)
;;;			(list
;;;				(list (+ (car gc_cg1) (* gc_r1 (cos (- gc_ac gc_bt)))) (+ (cadr gc_cg1) (* gc_r1 (sin (- gc_ac gc_bt)))))
;;;				(list (+ (car gc_cg1) (* gc_r1 (cos (+ gc_ac gc_bt)))) (+ (cadr gc_cg1) (* gc_r1 (sin (+ gc_ac gc_bt)))))
;;;			)
		)
	)
)

;encontrar las intersecciones de un segmento con un arco
;"|GC|ENARC"
(defun |GC|ENARC (gc_pt gc_arc / gc_pi gc_pf gc_delta gc_ra gc_cg gc_ani gc anf)
	(setq gc_pi (car gc_arc) gc_pf (cadr gc_arc) gc_delta (cadddr gc_arc) gc_ra (nth 4 gc_arc)
	      gc_cg (|C|CENTRO gc_delta gc_ra gc_pi gc_pf) gc_ani (|K|ANG gc_cg gc_pi) gc_anf (|K|ANG gc_cg gc_pf)
	)
	(and (= (|K|DISTH gc_cg gc_pt) gc_ra) (|N|ENTRE (|K|ANG gc_cg gc_pt (list gc_ani gc_anf))))
)

;"|GC|INTSA"
(defun |GC|INTSA (gc_seg gc_arc / cg_pp gc_upa gc_d gc_ra gc_cg gc_res)
	(setq gc_cg (|C|CENTRO (cadddr gc_arc) (car (cddddr gc_arc)) (car gc_arc) (cadr gc_arc))
	      gc_lpi (|GC|INTRC gc_seg (list gc_cg (car (cddddr gc_arc)))) gc_res '()
	)
 	(foreach gc_pt gc_lpi
		(if (and (|GC|ENSR gc_pt gc_seg) (|GC|ENARC gc_pt gc_arc))
			(setq gc_res (cons gc_pt gc_res))
		)
	)
	gc_res
)

;encontrar las intersecciones de dos arcos
;"|GC|INTAA"
(defun |GC|INTAA (gc_a1 gc_a2)
	(setq gc_cg1 (|C|CENTRO (cadddr gc_a1) (car (cddddr gc_a1)) (car gc_a1) (cadr gc_a1))
	      gc_cg2 (|C|CENTRO (cadddr gc_a2) (car (cddddr gc_a2)) (car gc_a2) (cadr gc_a2))
	      gc_lpi (|GC|INT2C (list gc_cg1 (car (cddddr gc_a1))) (list gc_cg2 (car (cddddr gc_a2)))) gc_res '()
	)
 	(foreach gc_pt gc_lpi
		(if (and (|GC|ENARC gc_pt gc_a1) (|GC|ENARC gc_pt gc_a2))
			(setq gc_res (cons gc_pt gc_res))
		)
	)
	gc_res
)

;encontrar las intersecciones de un arco y un circulo
;"|GC|INTAC"
(defun |GC|INTAC (gc_a gc_c)
	(setq gc_cga (|C|CENTRO (cadddr gc_a) (car (cddddr gc_a)) (car gc_a) (cadr gc_a))
	      gc_lpi (|GC|INT2C (list gc_cg1 (car (cddddr gc_a1))) gc_c) gc_res '()
	)
 	(foreach gc_pt gc_lpi
		(if (|GC|ENARC gc_pt gc_a)
			(setq gc_res (cons gc_pt gc_res))
		)
	)
	gc_res
)


;Calcular áreas por el Método de Gauss
;"|GC|GAUSS"
(defun |GC|GAUSS (gc_pol / gc_pp gc_poli)
	(defun |GC|SUMA (gc_list gc_xp gc_ypp)
		(cond
			((null gc_list) (* gc_xp gc_ypp))
			((null gc_xp) (|GC|SUMA (cdr gc_list) (caar gc_list) (cadar gc_list)))
			(T
				(+ (* gc_xp (cadar gc_list)) (|GC|SUMA (cdr gc_list) (caar gc_list) gc_ypp))
			)
		)
	)
	(if (equal (car gc_pol) (last gc_pol) gc_tol) (setq gc_pol (cdr gc_pol)))
	(setq gc_pp (car gc_pol) gc_poli (cons (list 0 0) (mapcar '(lambda (el) (list (- (car el) (car gc_pp)) (- (cadr el) (cadr gc_pp)))) (cdr gc_pol))))
  	(abs (/ (- (|GC|SUMA gc_poli nil nil) (|GC|SUMA (mapcar '(lambda (el) (list (cadr el) (car el))) gc_poli) nil nil)) 2.0))
)

;verificar si un punto está en un segmento
;"|GC|ENSEG"
(defun |GC|ENSEG (gc_pt gc_seg)
	(cond
		((null gc_pt) nil)
		((null gc_seg) nil)
		((eq (last cg_seg) "A") (|GC|ENARC gc_pt gc_seg))
		((eq (last cg_seg) "C") (= (cadr gc_seg) (|K|DISTH (car gc_seg) gc_pt)))
		(T (|GC|ENSR gc_pt gc_seg))
	)
)

