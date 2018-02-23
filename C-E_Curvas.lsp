;---------------------------------------------------------------------------------------------------------------------------------------------------------
;				C	U	R	V	A	S		C	I	R	C	U	L	A	R	E	S
;---------------------------------------------------------------------------------------------------------------------------------------------------------
; Cálculo del ángulo alfa
;"|C|ALFA"
(defun |C|ALFA (c_beta)
	(if (> c_beta pi) (setq c_beta (- (* 2 pi) c_beta)))
	(+ pi c_beta)
)

; Cálculo del ángulo de desarrollo (Delta) c_fcur puede ser el factor de curvatura, o una lista con el los ángulos inicial y final de la curva
;"|C|DELTA"
(defun |C|DELTA (c_fcur c_ai c_af)
	(cond 
		(c_fcur (* (atan c_fcur) -4))
		((> c_ai c_af)
			(- (- c_af (- c_ai (* PI 2))))
		)
		(T
			(- (- c_af c_ai))
		)
	)
)

; Cálculo del factor de curvatura (no se usa)
;"|C|FCUR"
(defun |C|FCUR (c_delta c_ai c_af)
	(cond 
		(c_delta (/ (|N|TAN c_delta) -4))
		(T (|C|FCUR (|C|DELTA nil c_ai c_af) nil nil))
	)
)

; Cálculo del radio
;"|C|RADIO"
(defun |C|RADIO (c_delta c_pc c_fc c_arc)
	(cond
		((null c_delta)
			nil
		)
		((and c_pc c_fc)
			(abs (/ (|K|DISTH c_pc c_fc) (* (sin (/ (abs c_delta) 2)) 2)))
		)
		(c_arc
			(* c_delta c_arc)
		)
		(T nil)
	)
)

; Cálculo del centro de giro
;"|C|CENTRO"
(defun |C|CENTRO (c_delta c_rc c_pc c_fc)
	(cond 
		((minusp c_delta)
			(|K|XY (|K|POLAR c_pc (+ (|K|ANG c_pc c_fc) (/ (+ pi c_delta) 2)) c_rc))
		)
		(T
			(|K|XY (|K|POLAR c_pc (+ (|K|ANG c_pc c_fc) (/ (- pi c_delta) -2)) c_rc))
		)
	)
)

; Cálculo del arco circular
;"|C|ARCO"
(defun |C|ARCO (c_delta c_pc c_fc c_ra)
	(cond 
		((null c_delta)
			nil
		)
		(c_ra (abs (* c_ra c_delta)))
		((and c_pc c_fc)
			(abs (* (|C|RADIO c_delta c_pc c_fc nil) c_delta))
		)
	)
)

; Cálculo de la cuerda (no se usa)
;"|C|CUERDA"
(defun |C|CUERDA (c_delta c_ra)
	(* (* c_ra 2) (sin (/ (abs c_delta) 2)))
)

; Cálculo de la longitud de la tangente
;"|C|LTANG"
(defun |C|LTANG (c_delta c_ra)
	(abs (* c_ra (|N|TAN (/ c_delta 2))))
)

; Cálculo del vértice
;"|C|VERT"
(defun |C|VERT (c_delta c_rc c_pc c_fc)
	(cond
		((null c_delta)
			nil
		)
		(T (setq c_cg (|C|CENTRO c_delta c_rc c_pc c_fc) c_apc (|K|ANG c_cg c_pc) c_tg (|C|LTANG c_delta c_rc))
			(|K|XY (|K|POLAR c_pc (- c_apc ((|N|SIGNO c_delta) (* Pi 0.5))) c_tg))
		)
	)
)

;cálculo del punto medio de una curva circular
;"|C|PM"
(defun |C|PM (c_delta c_rc c_pc c_fc)
	(cond
		((null c_delta)
			nil
		)
		(T (setq c_cg (|C|CENTRO c_delta c_rc c_pc c_fc) c_am (|N|ANGM c_cg c_pc c_fc))
			(|K|POLAR c_cg c_am c_rc)
		)
	)
)

; Cálculo de la Progresiva final de una curva circular
;"|C|PRFC"
(defun |C|PRFC (c_delta c_pc c_fc c_ppc)
	(+ c_ppc (|C|ARCO c_delta c_pc c_fc nil))
)

; Cálculo de un punto a una distancia x del fin de curva en el arco
;"|C|PTOC"
(defun |C|PTOC (c_delta c_ra c_pc c_fc c_dist)
	(cond
		((= c_dist 0) c_fc)
		((= c_dist (abs (* c_ra c_delta))) c_pc)
		(T
			(setq c_cg (|C|CENTRO c_delta c_ra c_pc c_fc) c_af (|K|ANG c_cg c_fc)
			      c_ax (+ c_af ((|N|SIGNO c_delta) (/ c_dist c_ra)))
			)
			(|K|XY (|K|POLAR c_cg c_ax c_ra))
		)
	)
)

;devolver la dirección de una curva
;"|C|DIRC"
(defun |C|DIRC (c_delta)
	(if (minusp c_delta) "I" "D")
)

;determinar si un punto cae en el ámbito de una curva circular
;"|C|ENCUR"
(defun |C|ENCUR (c_pto c_delta c_rc c_pc c_fc / c_cg)
	(setq c_cg (|C|CENTRO c_delta c_rc c_pc c_fc))
	(or (equal (angle c_cg c_pto) (angle c_cg c_pc) 0.001)
	    (equal (+ (|N|A3P c_cg c_pc c_pto) (|N|A3P c_cg c_pto c_fc)) (|N|A3P c_cg c_pc c_fc) 0.001)
	    (equal (angle c_cg c_pto) (angle c_cg c_fc) 0.001)
	)
)

;cálculo de la distancia de la proyeccción de un punto en el arco circular al final del arco
;"|C|DISTC"
(defun |C|DISTC (c_delta c_rc c_pc c_fc c_pto)
	(setq c_cg (|C|CENTRO c_delta c_rc c_pc c_fc))
	(* c_rc (|N|A3P c_cg c_pto c_fc))
)

;cálcular a que lado de la curva cae un punto
;"|C|LADO"
(defun |C|LADO (c_pt c_di c_rc c_delta)
	(cond
		((equal c_di c_rc 0.005) "E")
		((< c_di c_rc) (|C|DIRC c_delta))
		(T (|C|DIRC (- c_delta)))
	)
)

;cálculo de la distancia y lado a que cae un punto respecto de una curva circular y ängulo de la tangente
;"|C|DLPC"
(defun |C|DLPC (c_delta c_rc c_pc c_fc c_pt / c_cg c_pej)
	(setq c_cg (|C|CENTRO c_delta c_rc c_pc c_fc) c_pej (|K|XY (|K|POLAR c_cg (|K|ANG c_cg c_pt) c_rc)))
	(list (|K|DISTH c_pt c_pej) (|C|LADO c_pt (|K|DISTH c_pt c_cg) c_rc c_delta) c_pej (+ (|K|ANG c_cg c_pej) ((|N|-SIGNO c_delta) (/ PI 2))))
)

;ID de un punto en una curva circular
;"|C|IDP"
(defun |C|IDP (c_pt c_seg)
	(append (list (|N|ROUND (- (caddr c_seg) (|C|DISTC (cadddr c_seg) (nth 4 c_seg) (car c_seg) (cadr c_seg) c_pt)) 2)) (|C|DLPC (nth 3 c_seg) (nth 4 c_seg) (car c_seg) (cadr c_seg) c_pt))
)

; Cálculo del punto de inserción y el ángulo en un segmento de curva circular
;"|C|PYAC"
(defun |C|PYAC (c_pr c_seg)
	(setq c_pt (|C|PTOC (cadddr a_seg) (nth 4 a_seg) (car a_seg) (cadr a_seg) (- (caddr a_seg) a_pr)) c_sg (|N|-SIGNO (cadddr c_seg))
	      c_cg (|C|CENTRO (cadddr c_seg) (nth 4 c_seg) (car c_seg) (cadr c_seg))
	)
	(list c_pt (c_sg (angle c_cg c_pt) (/ PI 2)))
)

;"|C|REPCD"
(defun |C|REPCD (c_cg c_rc c_delta c_pi c_pf c_ai c_af c_pr c_dist c_pri)
	(cond
		((>= c_pr (abs (* c_rc c_delta))) (list (list (|K|XY (|K|POLAR c_cg c_af c_rc)) (+ c_pri (abs (* c_rc c_delta))))))
		(T
			(cons (list (|K|XY (|K|POLAR c_cg ((|N|-SIGNO c_delta) c_ai (/ c_pr c_rc)) c_rc)) (+ c_pri c_pr))
			      (|C|REPCD c_cg c_rc c_delta c_pi c_pf c_ai c_af (+ c_pr c_dist) c_dist c_pri)
			)
		)
	)
)

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;						C	U	R	V	A	S		E	S	P	I	R	A	L	E	S
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;calcular el parámetro K
;"|E|K"
(defun |E|K (e_rc e_le)
	(cond
		((= e_le 0) 0)
		(T (sqrt (* e_rc e_le)))
	)
)

;calcular el valor de Tita
;"|E|TITA"
(defun |E|TITA (e_rc e_le)
	(cond
		((= e_le 0) 0)
		(T (/ e_Le (* e_rc 2)))
	)
)

;calcular el radio de la curva circular
;"|E|RCC"
(defun |E|RCC (e_tita e_le)
	(/ e_le (* (abs e_tita) 2))
)

;calcular el del Le
;"|E|LE"
(defun |E|LE (e_rc e_tita)
	(cond
		((= e_tita 0) 0)
		(T (* e_rc 2 (abs e_tita)))
	)
)

;calcular el valor de tita para un punto en la espiral
;"|E|TITAP"
(defun |E|TITAP (e_tita e_lp e_le)
	(* (abs e_tita) (expt (/ e_lp e_le) 2))
)

;calcular el valor del radio de la curva circular
;"|E|RC"
(defun |E|RC (e_le e_tita)
	(cond
		((= e_le 0) nil)
		(T (/ e_le (* (abs e_tita) 2)))
	)
)

;calcular el cociente de tita para una potencia determinada
;"|E|CTF"
(defun |E|CTF (e_t e_exp e_div e_sig)
	(* (/ (expt e_t e_exp) (* e_div (|N|FACT e_exp))) e_sig)
)

;calcular el valor de x a una distancia k del inicio de la espiral
;"|E|CX"
(defun |E|CX (e_t e_k)
	(* e_k (sqrt (* 2 e_t)) (+ 1 (|E|CTF e_t 2 5 -1) (|E|CTF e_t 4 9 1) (|E|CTF e_t 6 13 -1) (|E|CTF e_t 8 17 1) (|E|CTF e_t 10 21 -1) (|E|CTF e_t 12 25 1) (|E|CTF e_t 14 29 -1) (|E|CTF e_t 16 33 1)))
)

;calcular el valor de y a una distancia k del inicio de la espiral
;"|E|CY"
(defun |E|CY (e_t e_k)
	(* e_k (sqrt (* 2 e_t)) (+ (/ e_t 3) (|E|CTF e_t 3 7 -1) (|E|CTF e_t 5 11 1) (|E|CTF e_t 7 15 -1) (|E|CTF e_t 9 19 1) (|E|CTF e_t 11 23 -1) (|E|CTF e_t 13 27 1) (|E|CTF e_t 15 31 -1) (|E|CTF e_t 17 35 1)))
)

;calcular un punto en una espiral a una distancia determinada del TE o ET
;"|E|CPT"
(defun |E|CPT (e_dist e_po e_k e_dir e_sg)
	(cond
		((null e_dist) nil)
		((= e_dist 0.0) e_po)
		(T
			(setq e_osp (list 0 0) e_titp (/ (expt e_dist 2) (* 2 (expt e_k 2)))
				e_cp0 (list (|E|CX e_titp e_k) (e_sg (|E|CY e_titp e_k)))
				e_a0 (|K|ANG e_osp e_cp0) e_d0 (|K|DISTH e_osp e_cp0)
			)
			(|K|XY (|K|POLAR e_po (+ e_dir e_a0) e_d0))
		)
	)
)

;calcular el valor de Ye
;"|E|YE"
(defun |E|YE (e_rc e_le) ;=POTENCIA(Le,3)/(6*RC*Le)
;;;	(cadr (|E|CPT e_le '(0 0) (|E|K e_rc e_le) 0.0 +))
	(/ (expt e_le 3) (* e_rc e_le 6))
)

;calcular el valor de Xe
;"|E|XE"
(defun |E|XE (e_rc e_le) ;=Le*(1-(POTENCIA(tita,2)/2))
;;;	(car (|E|CPT e_le '(0 0) (|E|K e_rc e_le) 0.0 +))
	(* e_le (1- (/ (expt (|E|TITA e_rc e_le) 2) 2)))
)

;calcular el valor de la tangente larga de una espiral
;"|E|TL"
(defun |E|TL (e_rc e_le) ;TL = Xc - Yc  Cotg titae
	(- (|E|XE e_rc e_le) (* (|E|YE e_rc e_le) (|N|CTAN (|E|TITA e_rc e_le))))
)

;calcular el valor de la tangente corta de una espiral
;"|E|TC"
(defun |E|TC (e_rc e_le) ;TC = Yc / Sen titae
	(/ (|E|YE e_rc e_le) (sin (|E|TITA e_rc e_le)))
)

;calcular elcentro de giro de la curva circular x0=x-R·sint,y0=y+R·cost
;"|E|CGC"
(defun |E|CGC (e_pi e_pf e_rc e_le e_tita e_dirt e_sg)
	(setq e_pf0 (|E|PTT (abs e_tita) '(0 0) (|E|K e_rc e_le) 0.0 +)
	      e_cg0 (list (- (car e_pf0) (* e_rc (sin (abs e_tita)))) (+ (cadr e_pf0) (* e_rc (cos (abs e_tita)))))
	      e_an0 (|K|ANG '(0 0) e_cg0) e_di (|K|DISTH '(0 0) e_cg0)
	)
	(polar e_pi (e_sg e_dirt e_an0) e_di)
)

;Cálcular el valor de la cuerda larga
;"|E|CUERDAL"
(defun |E|CUERDAL (e_rc e_le)
	(sqrt (+ (expt (|E|XE e_rc e_le) 2) (expt (|E|YE e_rc e_le) 2)))
)


;calcular el valor de P
;"|E|PE"
(defun |E|PE (e_rc e_le) ;=ye-Rc*(1-COS(tita)) ;=Le^2/(24*Rc))
	(cond
		((eq e_le 0) 0)
		(T
			(- (|E|YE e_rc e_le) (* e_rc (- 1 (cos (|E|TITA e_rc e_le)))))
		)
	)
)

; calcular el valor de Ke
;"|E|KE"
(defun |E|KE (e_Rc e_Le) ;=xe-Rc*SENO(tita) ;=Le*(1-Titae^2/10)+(Titae^3/6-Titae)*Rc
	(cond
		((eq e_le 0) 0)
		(T
			(+ (* e_le (- 1 (/ (expt (|E|TITA e_rc e_le) 2) 10))) (* e_Rc (- (/ (expt (|E|TITA e_rc e_le) 3) 6) (|E|TITA e_rc e_le))))
		)
	)
)

;Calcular el desplazamiento de la curva circular
;"|E|DESP"
(defun |E|DESP (e_delta e_Pe e_Ps) ;=(Ps-Pe)/SENO(delta)
	(/ (- e_Ps e_Pe) (sin e_delta))
)

;Calcular las longitudes de las tangentes de una curva con espirales
;"|E|LTANG"
(defun |E|LTANG (e_rc e_lee e_les e_delta) ;=(Rc+P)*TAN(Delta/2)+k-D
  	(setq e_despl (|E|DESP e_delta (|E|PE e_rc e_lee) (|E|PE e_rc e_les)))
	(list
		(+ (+ (* (+ e_rc (|E|PE e_rc e_lee)) (|N|TAN (/ e_delta 2))) (|E|KE e_rc e_lee)) e_despl)
		(- (+ (* (+ e_rc (|E|PE e_rc e_les)) (|N|TAN (/ e_delta 2))) (|E|KE e_rc e_les)) e_despl)
	)
)

; Calcular la externa de una curva con espirales
;"|E|EXTER"
(defun |E|EXTER (e_rc e_le e_delta) ;=(Rc+(Pe+Ps)/2)*(1/COS(Delta/2)-1)+(Pe+Ps)/2
	(setq e_delta (abs e_delta))
	(cond
		((listp e_le)
			(+ (* (+ e_rc (/ (+ (|E|PE e_rc (car e_le)) (|E|PE e_rc (cadr e_le))) 2)) (- (/ 1 (cos (/ e_delta 2))) 1)) (/ (+ (|E|PE e_rc (car e_le)) (|E|PE e_rc (cadr e_le))) 2))
		)
		((eq e_le 0.0)
			(* e_Rc (- (/ 1 (cos (/ e_delta 2))) 1))
		)
		(T
			(+ (* (- (/ 1 (cos (/ e_delta 2))) 1) (+ e_rc (/ (expt e_le 2) (* e_rc 24)))) (/ (expt e_le 2) (* e_rc 24)))
		)
	)
)

; Calcular el desarrollo de una curva
;"|E|DES"
(defun |E|DES (e_rc e_le e_delta)
	(cond
		((listp e_le)
			(+ (* e_rc (- e_delta (|E|TITA e_rc (car e_le)) (|E|TITA e_rc (cadr e_le)))) (car e_le) (cadr e_le))
		)
		(T (+ (* e_rc e_delta) e_le))
	)
)

; Calcular los puntos de inicio y finalización de la curva con espirales
;"|E|PTIF"
(defun |E|PTIF (e_v e_rc e_lee e_les e_delta e_ai e_af)
	(setq e_ltangs (|E|LTANG e_rc e_lee e_les (abs e_delta)))
	(list (|K|XY (|K|POLAR e_v e_ai (car e_ltangs))) (|K|XY (|K|POLAR e_v e_af (cadr e_ltangs))))
)

; Calcular el Vértice de una curva con espirales
;"|E|VERT"
(defun |E|VERT (e_see e_scc e_ses)
	(cond
		((and e_see e_ses) 
			(inters (|K|POLAR (car e_see) (nth 5 e_see) 10000.0) (|K|POLAR (cadr e_ses) (nth 5 e_ses) 10000.0))
		)
		((and e_see e_scc)
			
		)
	)
	(inters (|K|POLAR a_et (+ e_ai Pi) 1000.0) (|K|POLAR a_te (+ e_af Pi) 1000.0))
)

;calcular un punto en una espiral para un ángulo titap determinado TE o ET
;"|E|PTT"
(defun |E|PTT (e_titp e_po e_k e_dir e_sg)
	(setq e_osp (list 0 0)
		e_cp0 (list (|E|CX e_titp e_k) (e_sg (|E|CY e_titp e_k)))
		e_d0 (|K|DISTH e_osp e_cp0) e_a0 (|K|ANG e_osp e_cp0)
	)
	(|K|XY (|K|POLAR e_po (+ e_dir e_a0) e_d0))
)

;replantear una espiral
;"|E|REPPE"
(defun |E|REPPE (e_po e_rc e_le e_dir e_sg e_prec)
	(defun |E|OLP (e_dp e_pp e_k e_lus)
		(cond
			((equal e_pp e_le e_lus) (list (|E|CPT e_pp e_po e_k e_dir e_sg) (|E|CPT e_le e_po e_k e_dir e_sg)))
			(T (cons (|E|CPT e_pp e_po e_k e_dir e_sg) (|E|OLP (* e_dp 0.9) (+ e_pp (max (* e_dp 0.9) e_lus)) e_k e_lus)))
		)
	)
  	(cons e_po (|E|OLP (/ e_le 10.0) (/ e_le 10.0) (|E|K e_rc e_le) (/ e_le e_prec)))
)

;replantear una espiral
;"|E|REPPE"
(defun |E|REPPEP (e_pri e_po e_rc e_le e_dir e_sg e_prec e_sign)
	(defun |E|OLP (e_dp e_pp e_k e_lus)
		(cond
			((equal e_pp e_le e_lus) (list (list (e_sign e_pri e_pp) (|E|CPT e_pp e_po e_k e_dir e_sg)) (list (e_sign e_pri e_le) (|E|CPT e_le e_po e_k e_dir e_sg))))
			(T (cons (list (e_sign e_pri e_pp) (|E|CPT e_pp e_po e_k e_dir e_sg)) (|E|OLP (* e_dp 0.9) (+ e_pp (max (* e_dp 0.9) e_lus)) e_k e_lus)))
		)
	)
  	(cons (list e_pri e_po) (|E|OLP (/ e_le 10.0) (/ e_le 10.0) (|E|K e_rc e_le) (/ e_le e_prec)))
)

;"|E|REPED"
(defun |E|REPED (e_po e_rc e_le e_dir e_sg e_dist)
	(defun |E|OLPD (e_dp e_pp e_k)
		(cond
			((>= e_pp e_le) (list (|E|CPT e_le e_po e_k e_dir e_sg)))
			(T (cons (|E|CPT e_pp e_po e_k e_dir e_sg) (|E|OLPD e_dp (+ e_pp e_dp) e_k)))
		)
	)
  	(cons e_po (|E|OLPD e_dist e_dist (|E|K e_rc e_le)))
)

;replantear una espiral
;"|E|REPPEK"
(defun |E|REPPEK (e_po e_k e_dir e_sg)
	(defun |E|OLPK (e_dp e_k e_le)
		(cond
			((equal e_dp e_le g_tol) (list (|E|CPT e_le e_po e_k e_dir e_sg)))
			(T (cons (|E|CPT e_dp e_po e_k e_dir e_sg) (|E|OLPK (+ e_dp 0.01) e_k e_le)))
		)
	)

	(setq e_le (* e_k 3.54))
  	(cons e_po (|E|OLPK 0.01 e_k e_le))
)

; Calcular el centro de giro de la curva circular con espirales
;"|E|CG"
(defun |E|CG (e_rc e_le e_delta e_te e_ec e_ce e_et)
	(cond
		((listp e_le)
			(|C|CENTRO (* (- (abs e_delta) (|E|TITA e_rc (car e_le)) (|E|TITA e_rc (cadr e_le))) (/ e_delta (abs e_delta))) e_rc e_ec e_ce)
		)
		(T
			(|C|CENTRO (* (- (abs e_delta) (* (|E|TITA e_rc e_le) 2)) (/ e_delta (abs e_delta))) e_rc e_ec e_ce)
		)
	)
)

;determinar si un punto cae en el ámbito de una curva espiral
;"|E|ENESP"		    e-,s+ e+,-
(defun |E|ENESP (e_pto e_seg)
	(setq e_pi (car e_seg) e_pf (cadr e_seg) e_le (cadddr e_seg) e_tita (nth 4 e_seg) e_dirt (nth 5 e_seg) e_tip (last e_seg))
	(if (eq e_tip "S") (setq e_ans ((|N|SIGNO e_tita) (/ pi 2))) (setq e_ans ((|N|-SIGNO e_tita) (/ pi 2))))
	(setq e_pa1 (|K|POLAR e_pi (+ e_dirt e_ans) 1000.0)
	      e_pa2 (|K|POLAR e_pf (+ e_dirt e_ans (- e_tita)) 1000.0)
	      e_pip (|GC|INT2R (list e_pi e_pa1) (list e_pf e_pa2)) e_dc (|K|DISTH e_pip e_pi)
	      e_titap1 (|N|A3P e_pip e_pi e_pto) e_titap2 (|N|A3P e_pip e_pf e_pto)
	)
	(and (<= e_titap1 (abs e_tita)) (<= e_titap2 (abs e_tita)))
)

;ID de un punto en una espiral
;"|E|IDP"
(defun |E|IDP (e_pt e_seg)
	(defun |E|BPE (e_pri e_dist e_sgi e_dpa e_pra e_pa)
		(setq e_pri (+ e_pri e_dist) e_pe (|E|CPT e_pri e_pi e_k e_dirt e_sgi) e_dpe (|K|DISTH e_pt e_pe))
		(cond
			((<= e_dist 0.0025) (list e_pe e_pri e_dpe))
			((> e_dpe e_dpa)
				(setq e_pra (- e_pra e_dist) e_pa (|E|CPT e_pra e_pi e_k e_dirt e_sgi) e_dpa (|K|DISTH e_pt e_pa))
				(|E|BPE e_pra (* e_dist 0.5) e_sgi e_dpa e_pra e_pa)
			)
			((>= e_pri e_le)
				(|E|BPE e_pra (* e_dist 0.5) e_sgi e_dpa e_pra e_pa)
			)
			(T
				(|E|BPE (+ e_pri e_dist) e_dist e_sgi e_dpe e_pri e_pe)
			)
		)
	)

	(cond
		((equal e_pt (car e_seg) 0.0025)
			(if (eq (last e_seg) "E")
		 		(list (- (caddr e_seg) (cadddr e_seg)) 0.0 "E" (car e_seg) (nth 5 e_seg))
		 		(list (- (caddr e_seg) (cadddr e_seg)) 0.0 "E" (car e_seg) ((|N|-SIGNO (nth 4 e_seg)) (nth 5 e_seg) (nth 4 e_seg)))
			)
		)
		((equal e_pt (cadr e_seg) 0.0025)
			(if (eq (last e_seg) "E")
		 		(list (caddr e_seg) 0.0 "E" (cadr e_seg) ((|N|SIGNO (nth 4 e_seg)) (nth 5 e_seg) (nth 4 e_seg)))
		 		(list (caddr e_seg) 0.0 "E" (cadr e_seg) (|N|DOSPI (+ (nth 5 e_seg) PI)))
			)
		)
		((eq (last e_seg) "E")
			(setq e_pi (car e_seg) e_prf (caddr e_seg) e_le (cadddr e_seg) e_tita (nth 4 e_seg) e_dirt (nth 5 e_seg) e_pri (- e_prf e_le)
			      e_rc (|E|RC e_le e_tita) e_sg (|N|-SIGNO e_tita) e_scg (|N|SIGNO e_tita) e_k (|E|K e_rc e_le)
			      e_lidp (|E|BPE 0.0 (* e_le 0.1) e_sg (|K|DISTH e_pt e_pi) 0.0 e_pi)
			      e_titap (|E|TITAP e_tita (cadr e_lidp) e_le) e_pa (|K|POLAR e_pi (|K|ANG e_pi (car e_lidp)) (* (|K|DIST e_pi (car e_lidp)) 100.0))
			)
			(list (|N|ROUND (+ e_pri (cadr e_lidp)) 2) (caddr e_lidp) (|C|LRU e_pi e_pa e_pt) (car e_lidp) (|N|DOSPI (e_sg e_dirt e_titap)))
		)
		(T
			(setq e_pi (cadr e_seg) e_prf (caddr e_seg) e_le (cadddr e_seg) e_tita (nth 4 e_seg) e_dirt (nth 5 e_seg)
			      e_rc (|E|RC e_le e_tita) e_sg (|N|-SIGNO e_tita) e_scg (|N|SIGNO e_tita) e_k (|E|K e_rc e_le)
			      e_lidp (|E|BPE 0.0 (* e_le 0.1) e_scg (|K|DISTH e_pt e_pi) 0.0 e_pi)
			      e_titap (|E|TITAP e_tita (cadr e_lidp) e_le) e_pa (|K|POLAR e_pi (|K|ANG e_pi (car e_lidp)) (* (|K|DIST e_pi (car e_lidp)) 100.0))
			)
			(list (|N|ROUND (- e_prf (cadr e_lidp)) 2) (caddr e_lidp) (|C|LRU e_pa e_pi e_pt) (car e_lidp) (|N|DOSPI (e_scg (+ e_dirt PI) e_titap)))
		)
	)
)

; Cálculo del punto de inserción y el ángulo en un segmento de espiral
;"|E|PYAE"
(defun |E|PYAE (e_pr e_seg)
	(setq e_pp (car e_seg) e_sp (cadr e_seg) e_prfe (caddr e_seg) e_seg (cdddr e_seg) e_le (car e_seg) e_tita (cadr e_seg) e_dirt (caddr e_seg) e_tip (last e_seg)
	      e_rc (|E|RC e_le e_tita) e_k (|E|K e_rc e_le)
	)
	(cond
		((equal e_pr e_prfe 0.005)
			(if (eq e_tip "E")
				(list e_sp (+ e_dirt ((|N|SIGNO e_tita) e_tita)))
				(list e_sp (|N|DOSPI (+ e_dirt PI)))
			)
		)
		((equal e_pr (- e_prfe e_le) 0.005)
			(if (eq e_tip "E")
				(list e_pp e_dirt)
				(list e_pp (|N|DOSPI (+ e_dirt PI ((|N|-SIGNO e_tita) e_tita))))
			)
		)
		((eq e_tip "E")
			(setq e_titap (|E|TITAP e_tita (- e_pr (- e_prfe e_le)) e_le) e_sg (|N|-SIGNO e_tita))
			(list (|E|CPT (- e_pr (- e_prfe e_le)) e_pp e_k e_dirt e_sg) (|N|DOSPI (e_sg e_dirt e_titap)))
		)
		(T
			(setq e_titap (|E|TITAP e_tita (- e_prfe e_pr) e_le) e_sg (|N|SIGNO e_tita))
			(list (|E|CPT (- e_prfe e_pr) e_sp e_k e_dirt e_sg) (|N|DOSPI (e_sg (+ e_dirt PI) e_titap)))
		)
	)
) 

;---------------------------------------------------------------------------------------------------------------------------------------------------------
;			P	E	R	A	L	T	E	S		Y		S	O	B	R	E	A	N	C	H	O	S
;---------------------------------------------------------------------------------------------------------------------------------------------------------

; calcular el peralte para una curva en función del radio y la velocidad de diseño Barnett=((VD*0.75)^2/(127*Rc)) ConoSur=(Pmax*((RMin*2)/Rc-Rmin^2/Rc^2))) vmm=1.035*Vd-(Vd/20)^2
;"|C|PER"
(defun |C|PER (c_rc c_vd c_pmax c_pn / c_pere c_dec)
	(if (not c_vd) (setq c_vd g_vd))
	(if (not c_pmax) (setq c_pmax g_perm))
	(if (not c_pn) (setq c_pn g_pern))
	(setq c_barnett (* (/ (expt (* c_vd 0.75) 2) (* c_rc 127)) 100) c_pere (fix c_barnett) c_dec (- c_barnett c_pere))
	(cond
		((< c_dec 0.3) (max (min (/ c_pere 100.0) c_pmax) (abs c_pn)))
		((< c_dec 0.8) (max (min (/ (+ c_pere 0.5) 100.0) c_pmax) (abs c_pn)))
		(T (max (min (/ (+ c_pere 1) 100.0) c_pmax) (abs c_pn)))
	)
)

; calcular las transiciones de peralte
;"|C|TRANSP"
(defun |C|TRANSP (c_per c_vd c_ac c_pn / c_pmi1 c_pmi2)
	(cond ((>= c_vd 100)
			(setq c_pmi1 400 c_pmi2 200)
		)
		((and (< c_vd 100) (>= c_vd 60))
			(setq c_pmi1 350 c_pmi2 175)
		)
		((< c_vd 60)
			(setq c_pmi1 300 c_pmi2 150)
		)
	)
	(setq c_ht (+ c_per (abs c_pn))
	      c_lh (* (abs c_pn) c_ac c_pmi1)
	      c_l2f (+ c_lh (* (abs c_pn) c_ac c_pmi2))
	      c_lpt (+ c_l2f (* (- c_per (abs c_pn)) c_ac c_pmi2))
	)
	(list c_lh c_l2f c_lpt)
)

; calcular el sobreancho de una curva en función del radio y la velocidad de diseño NC*(Rc-RAIZ(Rc^2-8^2))+0.1*Vd/RAIZ(Rc)) 
;"|C|SANCHO"
(defun |C|SANCHO (c_rc c_vd)
	(if (not c_vd) (setq c_vd g_vd))
	(setq c_s (+ (* 2 (- c_rc (sqrt (- (expt c_rc 2) (expt 8 2))))) (/ (* 0.1 c_vd) (sqrt c_rc))))
	(cond
		((< c_s 0.3)
			0
		)
		((< c_s 0.5)
			0.5
		)
		(T
			(|N|ROUND c_s 1)  
		)
	)
)

;"|C|TRPSA"
(defun |C|TRPSA (c_per c_rc c_delta c_pro c_le c_sn c_st c_vd c_ac c_pmax c_pn c_adp c_ads / c_sa c_ltr)
	(if (not c_vd) (setq c_vd g_vd))
	(if (not c_ac) (setq c_ac g_acar))
	(if (not c_pmax) (setq c_pmax g_perm))
	(if (not c_pn) (setq c_pn g_pern))
	(setq c_sac (if c_ads (|C|SANCHO c_rc c_vd) 0.0)
	      c_ltr (|C|TRANSP (/ c_per 100) c_vd c_ac (/ c_pn 100))
	)
	(if (> c_le 0)
		(setq c_prpn (c_sn c_pro c_le (car c_ltr)))
		(setq c_prpn (c_sn (c_st c_pro (/ (* c_rc c_delta) 3)) (caddr c_ltr)))
	)
	(setq c_prpt (c_st c_prpn (caddr c_ltr)) c_pr0 (c_st c_prpn (car c_ltr)) c_prlc (c_st c_prpn (cadr c_ltr))
	      c_sapn 0.0 c_sa0 (abs (car (|L|INTERP (list c_sapn) (list c_sac) c_pr0 c_prpn c_prpt)))
	      c_salc (car (|L|INTERP (list c_sapn) (list c_sac) c_prlc c_prpn c_prpt))
	)
	(cond
		((and c_adp c_ads)
			(list 
				(list c_prpn c_pn c_pn c_sapn c_sapn)
				(list c_pr0 c_pn 0.0 c_sa0 c_sapn)
				(list c_prlc c_pn (- c_pn) c_salc c_sapn)
				(list c_prpt (- c_per) c_per c_sac c_sapn)
			)
		)
		(c_adp
			(list 
				(list c_prpn c_pn c_pn 0.0 0.0)
				(list c_pr0 c_pn 0.0 0.0 0.0)
				(list c_prlc c_pn (- c_pn) 0.0 0.0)
				(list c_prpt (- c_per) c_per 0.0 0.0)
			)
		)
		(T
			nil
		)
	)
)

;---------------------------------------------------------------------------------------------------------------------------------------------------------
;			R	U	M	B	O	S		P	L	A	N	I	M	E	T	R	I	C	O	S
;---------------------------------------------------------------------------------------------------------------------------------------------------------

; Cálculo del rumbo de una tangente
;"|C|RUMBO"
(defun |C|RUMBO (c_pt1 c_pt2)
	(+ (/ PI 2) (- (* PI 2) (|K|ANG c_pt1 c_pt2)))
)

; Cálculo de la progresiva final de un rumbo
;"|C|PRFR"
(defun |C|PRFR (c_pti c_ptf c_prin)
	(+ c_prin (|K|DISTH c_pti c_ptf))
)

;calcular a que lado de un segmento de recta cae un punto
;"|C|LRU"
(defun |C|LRU (c_ppr c_spr c_pt)
	(setq c_pe (|K|XY (|GC|PROYPR c_pt (list c_ppr c_spr))) c_ang (|N|ANGULO c_pe c_ppr c_pt))
	(cond
		((equal c_pe (|K|XY c_pt) 0.005) "E")
		((< c_ang PI) "I") 
		(T "D")
	)
)

;calcular si un punto cae en el ámbito de un segmento de recta
;"|C|ENRECTA"
(defun |C|ENRECTA (c_pt c_r)
	(cond
		((< (|K|DISTH c_pt (car c_r)) 0.01) T)
		((< (|K|DISTH c_pt (cadr c_r)) 0.01) T)
		(T
			(setq c_pr (|GC|PROYPR c_pt c_r))
			(and (|GC|ENSEG c_pr c_r) (< (|K|DISTH c_pr c_pt) 250.0))
;;;			(setq a_app (n_a3p a_pp a_sp a_pt) a_asp (n_a3p a_sp a_pp a_pt))
;;;			(or (and (<= a_app (/ PI 2)) (<= a_asp (/ PI 2))) (equal a_asp (/ PI 2) g_tol))
		)
	)
)

;ID de un punto en una recta
;"|C|IDPR"
(defun |C|IDPR (c_pt c_seg / c_pej)
	(setq c_pej (|GC|PROYPR c_pt (|L|SUBL c_seg 2)))
	(list (- (caddr c_seg) (|K|DISTH c_pej (cadr c_seg))) (|K|DISTH c_pt c_pej) (|C|LRU (car c_seg) (cadr c_seg) c_pt) c_pej (|K|ANG (car c_seg) (cadr c_seg)))
)
