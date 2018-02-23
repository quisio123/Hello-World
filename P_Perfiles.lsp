;---------------------------------------------------------------------------------------------------------------------------------------------------------
;				 	A	L	T	I	M	E	T	R	I	A
;---------------------------------------------------------------------------------------------------------------------------------------------------------

;obtener el perfil longitudinal del terreno
;obtener la cota de un listado de puntos
;"|P|OBTCPS"
(defun |P|OBTCPS (p_lps)
	(mapcar '|K|OBTC p_lps)
)

;"|P|OBTCPRS"
(defun |P|OBTCPRS (p_lpr p_alin)
	(mapcar '(lambda (el) (list (car el) (caddr (|K|OBTC (car (|A|PINS (car el) p_alin)))) (cadr el))) p_lpr)
)

;"|P|OBTPTS"
(defun |P|OBTPTS (p_lar p_p1 p_p2)
	(cond
		((null p_lar) nil)
;;;		((setq p_ar (|S|ARCOIN g_lar (list p_p1 p_p2)))
;;;			p_ar
;;;		)
		((> (length p_lar) 10000)
			(setq p_divl (|L|DIVM p_lar))
			(append (|P|OBTPTS (car p_divl) p_p1 p_p2) (|P|OBTPTS (cadr p_divl) p_p1 p_p2))
		)
		((setq p_pin (|K|INTERPOLAR (caar p_lar) (cadar p_lar) (inters (|K|XY (caar p_lar)) (|K|XY (cadar p_lar)) p_p1 p_p2) 0.001))
			(cons p_pin (|P|OBTPTS (cdr p_lar) p_p1 p_p2))
		)
		(T
			(|P|OBTPTS (cdr p_lar) p_p1 p_p2)
		)
	)
)

;"|P|ELCERC"
(defun |P|ELCERC (p_lper p_dh p_dv p_pa)
	(cond
		((null p_lper) nil)
		((null p_pa) (cons (car p_lper) (|P|ELCERC (cdr p_lper) p_dh p_dv (car p_lper))))
		((and (equal (caar p_lper) (car p_pa) p_dh) (equal (cadar p_lper) (cadr p_pa) p_dv)) (|P|ELCERC (cdr p_lper) p_dh p_dv p_pa))
		(T (cons (car p_lper) (|P|ELCERC (cdr p_lper) p_dh p_dv (car p_lper))))
	)
)

;"|P|INTAR" encontrar los puntos de intersección con las aristas y calcular sus cotas
(defun |P|INTAR (p_lar p_seg)
	(cond
		((null p_lar) nil)
		((> (length p_lar) 10000)
			(setq p_divl (|L|DIVM p_lar))
			(append (|P|INTAR (car p_divl) p_seg) (|P|INTAR (cadr p_divl) p_seg))
		)
		((equal (caar p_lar) (cadar p_lar) g_tol) (|P|INTAR (cdr p_lar) p_seg))
		((member (caar p_lar) p_seg)
			(cons (caar p_lar) (|P|INTAR (cdr p_lar) p_seg)) 
		)
		((member (cadar p_lar) p_seg)
			(cons (cadar p_lar) (|P|INTAR (cdr p_lar) p_seg)) 
		)
		((car (setq p_lpint (|I|SS (mapcar '|K|XY (car p_lar)) (|L|SUBL p_seg 2))))
			(setq p_ar (car p_lar) p_pp (car p_ar) p_sp (cadr p_ar) p_lpic '())
			(foreach p_pt p_lpint
				(setq p_lpic (cons (|K|INTERPOLAR p_pp p_sp p_pt 0.001) p_lpic))
			)
			(append p_lpic (|P|INTAR (cdr p_lar) p_seg))
		)
		(T (|P|INTAR (cdr p_lar) p_seg))
	)
)

(defun |P|OBTDREL (p_lalin)
	(defun |P|PROCPL (p_lpiz p_lpde)
		(setq p_lpio (|L|ORDPOS p_lpiz 1) p_lpdo (|L|ORDPOS p_lpde 1))
		(cond
			((and p_lpio p_lpdo)
				(setq p_dpi (car p_lpio) p_dpd (car p_lpdo) p_pri (car p_dpi) p_disi (cadr p_dpi) p_pi (cddr p_dpi) p_eli (caddr p_pi)
				      p_prd (car p_dpd) p_disd (cadr p_dpd) p_pd (cddr p_dpd) p_eld (caddr p_pd) p_dpr (- p_prd p_pri) p_del (- p_eld p_eli)
				      p_pr (+ p_pri (/ (* p_disi p_dpr) (+ p_disi p_disd))) p_pe (car (|A|PINS p_pr p_lalin)) p_pe (|K|INTERPOLAR p_pi p_pd p_pe 0.1)
				)
				(if p_pe
					(list (list p_pr (caddr p_pe)))
					nil
				)
			)
			(p_lpio
				(setq p_dp (car p_lpio) p_pr (car p_dp) p_dist (cadr p_dp) p_p (cddr p_dp) p_el (caddr p_p))
				(if (< p_dist 3.0)
					(list (list p_pr p_el))
					nil
				)
			)
			(p_lpdo
				(setq p_dp (car p_lpdo) p_pr (car p_dp) p_dist (cadr p_dp) p_p (cddr p_dp) p_el (caddr p_p))
				(if (< p_dist 3.0)
					(list (list p_pr p_el))
					nil
				)
			)
			(T nil)
		)
	)
	(setq p_lpr (append (|N|SEQNUM (|A|PRI p_lalin) 10.0 (|A|PRF p_lalin)) (list (list (|A|PRF p_lalin) ""))) p_lpl '())
	(foreach p_dpr p_lpr
		(setq p_pr (car p_dpr) p_idp (|A|PINS p_pr p_lalin) p_pe (car p_idp) p_dir (cadr p_idp) p_ai (+ p_dir (/ PI 2)) p_ad (+ p_dir (* PI 1.5)) p_-dir (+ p_dir PI)
		      p_pli (|K|POLAR p_pe p_ai 10.0) p_p1 (|K|POLAR p_pli p_-dir 5.0) p_p2 (|K|POLAR p_pli p_dir 5.0) p_p3 (|K|POLAR p_p2 p_ad 20.0) p_p4 (|K|POLAR p_p3 p_-dir 10.0)
		      p_ss (|K|SELPTS g_lptsm (list p_p1 p_p2 p_p3 p_p4))
		      p_lpe '() p_lpi '() p_lpd '()
		)
		(foreach p_pt p_ss
			(if (setq p_idp (|A|IDP p_pt p_lalin T))
			  (if (|N|ENTRE (car p_idp) (list (- p_pr 5.0) (+ p_pr 5.0)))
				(cond
					((eq (caddr p_idp) "E") (setq p_lpe (cons (list (car p_idp) (caddr p_pt)) p_lpe)))
					((eq (caddr p_idp) "I") (setq p_lpi (cons (append (|L|SUBL p_idp 2) p_pt) p_lpi)))
					(T (setq p_lpd (cons (append (|L|SUBL p_idp 2) p_pt) p_lpd)))
				)
			  )
			)
		)
		(if p_lpe
			(setq p_lpl (append p_lpe p_lpl))
			(setq p_lpl (append (|P|PROCPL p_lpi p_lpd) p_lpl))
		)
	)
	(|L|ORDENAR (|L|DEPURAR p_lpl))
)

;"|P|LISTER" 
(defun |P|LISTER (p_lalin)
	(cond
		((null p_lalin) nil)
		(T
			(append (|P|INTAR g_larg (car p_lalin)) (|P|LISTER (cdr p_lalin)))
		)
	)
)

;"|P|SUAVIZAR"
(defun |P|SUAVIZAR (p_lpts)
	(defun |P|SEGH (p_lisseg)
		(cond
			((null p_lisseg) nil)
			((= (cadaar p_lisseg) (cadadr (car p_lisseg)))
				(cons (car p_lisseg) (|P|SEGH (cdr p_lisseg)))
			)
			(T
				(|P|SEGH (cdr p_lisseg))
			)
		)
	)

	(setq p_lseg (|A|LISSEG p_lpts) p_lsh (|P|SEGH p_lseg))
	(foreach p_seg p_lsh
		(setq p_pp (car p_seg) p_sp (cadr p_seg) p_dx (|K|DX p_pp p_sp)
		      p_sega (|A|ASEG p_pp p_lseg) p_segs (|A|PSEG p_sp p_lseg)
		      p_pi (|P|I (car p_sega) (cadr p_sega)) p_pf (|P|I (car p_segs) (cadr p_segs))
		      p_pm (|K|PMED p_pp p_sp) p_mp (/ (min (abs p_pi) (abs p_pf)) 2)
		      p_divl (|L|DIVL p_lseg (|L|POS p_seg p_lseg)) p_ppl (car p_divl) p_spl (cdr (cadr p_divl))
		)
		(cond
			((eq (|N|SIGNO p_pi) (|N|SIGNO p_pf))
				(setq p_p14 (|K|PTP p_pp (/ (|K|DIST p_pp p_pm) 2) ((|N|SIGNO p_pi) p_mp))
				      p_p24 (|K|PTP p_pm (/ (|K|DIST p_pp p_pm) 2) ((|N|-SIGNO p_pi) p_mp))
				      p_lseg (append p_ppl (list (list p_pp p_p14) (list p_p14 p_pm) (list p_pm p_p24) (list p_p24 p_sp)) p_spl)
				)
			)
			(T
				(setq p_pm (|K|PTP p_pp (/ (|K|DIST p_pp p_sp) 2) ((|N|SIGNO p_pi) p_mp))
				      p_lseg (append p_ppl (list (list p_pp p_pm) (list p_pm p_sp)) p_spl)
				)
			)
		)
	)
	(|A|LISTAV p_lseg)
)

;"|P|OBTPLONG"
(defun |P|OBTPLONG (p_alin)
	(if (null g_larg)
		(|P|OBTDREL p_alin)
		(|L|ORDENAR (|L|ELIMDUP (mapcar '(lambda (el) (list (|N|ROUND (car (|A|IDP (|K|XY el) p_alin nil)) 2) (caddr el))) (|L|DEPURAR (|P|LISTER p_alin)))))
  	)
)

;"|P|PRYCPS"
(defun |P|PRYCPS (p_wpoints)
	(mapcar '(lambda (el) (list (|N|ROUND (cadr el) 2) (caddr (|K|OBTC (|K|PTATOP (|K|XY (caddr el))))) (car el))) p_wpoints)
)

;"|P|OBTPLONGD"
(defun |P|OBTPLONGD (p_di p_pri)
	(cond
		((null p_di) (|P|OBTPLONGD 25.0 0.0))
		((>= p_pri (caddr (last g_alin))) (list (list (caddr (last g_alin)) (last (|K|OBTC (cadr (last g_alin)))))))
		(T
			(cons (list p_pri (last (|K|OBTC (car (|A|PINS p_pri g_alin))))) (|P|OBTPLONGD p_di (+ p_pri p_di)))
		)
	)
)

; Deformar un perfil
;"|P|DEFPER"
(defun |P|DEFPER (p_ldata p_dv p_posx)
	(cond ((null p_ldata) nil)
		((null p_dv) p_ldata)
		(T (cons (list (nth p_posx (car p_ldata))  (* p_dv (nth (1+ p_posx) (car p_ldata)))) (|P|DEFPER (cdr p_ldata) p_dv p_posx)))
	)
)

; Calcular la pendiente entre dos puntos de un perfil
;"|P|I"
(defun |P|I (p_pt1 p_pt2)
	(cond
		((or (null p_pt1) (null p_pt2)) nil)
		((equal p_pt1 p_pt2 g_tol) 0)
		(T (/ (- (cadr p_pt2) (cadr p_pt1)) (- (car p_pt2) (car p_pt1))))
	)
)

; Calcular el segmento de la rasante en que cae una progresiva
;"|P|SEG"
(defun |P|SEG (p_pr p_lr p_sa)
	(cond
		((null p_lr) nil)
		((>= p_pr (car (last p_lr))) (last p_lr))
		((> p_pr (caar p_lr)) (|P|SEG p_pr (cdr p_lr) (car p_lr)))
		(T p_sa)
	)
)

; Calcular el segmento siguiente a una progresiva en la rasante
;"|P|PSEG"
(defun |P|PSEG (p_pr p_lr p_sa)
	(cond
		((null p_lr) nil)
		((<= p_pr (car (last p_lr))) (last p_lr))
		((< p_pr (caar p_lr)) (|P|PSEG p_pr (cdr p_lr) (car p_lr)))
		(T p_sa)
	)
)

;Calcular la cota del terreno natural para una progresiva
;"|P|ELP"
(defun |P|ELP (p_st p_lt / p_entre p_segi p_segf p_sti p_stf )
	(setq p_entre (|L|OBTPROX p_lt p_st))
	(cond
		((null p_entre) nil)
		((null (car p_entre))
			(- (cadar p_lt) (* (- (caar p_lt) p_st) (|P|I (car p_lt) (cadr p_lt))))
		)
		((null (cadr p_entre))
			(setq p_lt (reverse p_lt))
			(+ (cadar p_lt) (* (- p_st (caar p_lt)) (|P|I (car p_lt) (cadr p_lt))))
		)
		(T
			(cadr (|L|INTERP (car p_entre) (cadr p_entre) p_st (caar p_entre) (caadr p_entre)))
		)
	)
)

; Calcular la cota de una progresiva en la rasante
;"|P|ELEV"
(defun |P|ELEV (p_st p_lr / p_pseg p_useg)
	(setq p_entre (|L|OBTPROX p_lr p_st))
	(cond
		((null p_entre) nil)
		((and (null (car p_entre)) (null (cadr p_entre)))
			nil
		)
		((null (car p_entre))
			(setq p_seg (cadr p_entre))
			(- (cadr p_seg) (* (- (car p_seg) p_st) (caddr p_seg)))
		)
		(T
			(setq p_seg (car p_entre))
			(if (eq (last p_seg) "P")
				(+ (cadr p_seg) (* (- p_st (car p_seg)) (caddr p_seg)))
				(+ (cadr p_seg) (* (caddr p_seg) (- p_st (car p_seg))) (* (/ (- (cadddr p_seg) (caddr p_seg)) (* (nth 4 p_seg) 2)) (expt (- p_st (car p_seg)) 2)))
			)
		)
	)
)

;"|P|GUITARRA"
(defun |P|GUITARRA (p_ldg p_pipl p_terr p_ras p_pri / p_pr p_ct p_cr)
	(cond
		((null p_ldg) nil)
		(T
			(setq p_pr (caar p_ldg) p_ct (|P|ELP p_pr p_terr) p_cr (|P|ELEV p_pr p_ras) p_pin (|K|X+ p_pipl (/ (- p_pr p_pri) g_eschi)) p_ptr (list p_pr p_cr))
			(if p_cr
				(setq p_dif (- p_cr p_ct) p_signo (|N|TSIGNO p_dif) p_dif (rtos (abs p_dif) 2 2) p_cr (rtos p_cr 2 2) p_ct (rtos p_ct 2 2))
				(setq p_dif "" p_signo "" p_ct (rtos p_ct 2 2) p_cr "")
			)
			(|B|INSERT "DG" p_pin "La_Guitarra" 0 1 (list (rtos p_pr 2 2) p_ct p_cr p_dif p_signo))
			(if (/= (cadar p_ldg) "") (|B|INSERT "DCA" p_pin "La_DCA" 0 1 (list (cadar p_ldg))))
			(if (member (cadar p_ldg) (list "PCV" "FCV" "PIV"))
				(cons (list p_ptr (cadar p_ldg)) (|P|GUITARRA (cdr p_ldg) p_pipl p_terr p_ras p_pri))
				(|P|GUITARRA (cdr p_ldg) p_pipl p_terr p_ras p_pri)
			)
		)
	)
)

;"|P|TABPIV"
(defun |P|TABPIV (p_pr p_pin)
	(setq p_dpiv (|L|BUSCAR p_pr u_lpivs 0) p_cpiv (cadr p_dpiv) p_lcv (caddr p_dpiv) p_difp (cadddr p_dpiv) p_param (|P|PARAM p_lcv p_difp) p_yv (|P|YV p_lcv p_difp))
	(if (= p_lcv 0) (setq p_lcv "-" p_yv "-") (setq p_lcv (rtos p_lcv 2 2) p_yv (rtos p_yv 2 2)))
	(|B|INSERT "PIV" p_pin "La_PIVs" 0 g_eschi (list (rtos p_pr 2 2) (rtos p_cpiv 2 2) (strcat (rtos p_difp  2 2) "%") (rtos p_param 2 2) p_lcv p_yv)) (|B|EXPLOT (entlast))
)

;"|P|CCUN"
(defun |P|CCUN (p_st p_lcun)
	(setq p_entre (|L|OBTSLV p_lcun p_st p_st))
	(cond
		((and (null (car p_entre)) (null (cadr p_entre)) (null (last p_entre)))
			(if (< p_st (caar p_lt))
				(- (cadar p_lcun) (* (- (caar p_lcun) p_st) (|P|I (list (car p_lcun) (cadr p_lcun)) (list (caddr p_lcun) (cadddr p_lcun)))))
				(progn
					(setq p_lcun (reverse p_lcun) p_useg (car p_lcun) p_pseg (cadr p_lcun))
					(+ (caar p_useg) (* (- p_st (car p_useg)) (|P|I (list (car p_pseg) (cadr p_pseg)) (list (car p_useg) (cadr p_useg)))))
				)
			)
		)
		((car p_entre) (cadar p_entre))
		(T (setq p_entre (cdr p_entre) p_pseg (car p_entre) p_useg (cadr p_entre))
			(+ (cadr p_pseg) (* (- p_st (car p_pseg)) (|P|I (list (car p_pseg) (cadr p_pseg)) (list (car p_useg) (cadr p_useg)))))
		)
	)
)

; Calcular el Parámetro de una curva vertical
;"|P|PARAM"
(defun |P|PARAM (p_lc p_difpen)
	(cond
		((= p_lc 0) 0.0)
;;;		(T (/ p_lc (/ (abs p_difpen) 100)))
		(T (/ p_lc (abs p_difpen)))
	)
)

; Calcular la longitud de curva de una curva vertical
;"|P|LCV"
(defun |P|LCV (p_param p_difpen)
	(* p_param (abs p_difpen))
)

; Calcular el Y de una curva vertical
;"|P|YV"
(defun |P|YV (p_lc p_difpen)
	(cond ((= p_lc 0) 0.0)
		(T (* (/ (expt (/ p_lc 2) 2) (* (|P|PARAM p_lc p_difpen) 2)) (/ p_difpen (abs p_difpen))))
	)
)

;crear la lista de una rasante
;"|P|LISTRAS"
(defun |P|LISTRAS (p_lpiv p_upt)
	(defun |P|IFCV (p_lcv p_piv p_pa p_ps)
		(list (append (|K|PTP p_piv (- (/ p_lcv 2)) (- (|P|I p_piv p_pa))) (list (|P|I p_pa p_piv) (|P|I p_piv p_ps) p_lcv "C"))
		      (append (|K|PTP p_piv (/ p_lcv 2) (|P|I p_piv p_ps)) (list (|P|I p_piv p_ps) "P"))
		)
	)

	(cond
		((null p_lpiv) nil)
		((< (length p_lpiv) 2)
	      		(list (append (|L|BULT (car p_lpiv)) (list (|P|I p_upt (|L|BULT (car p_lpiv))) "P")))
		)
		((> (caddar p_lpiv) 0)
			(append (|P|IFCV (caddar p_lpiv) (|L|BULT (car p_lpiv)) p_upt (|L|BULT (cadr p_lpiv))) (|P|LISTRAS (cdr p_lpiv) (|L|BULT (car p_lpiv))))
		)
		(T
			(cons (append (|L|BULT (car p_lpiv)) (list (|P|I (|L|BULT (car p_lpiv)) (|L|BULT (cadr p_lpiv))) "P")) (|P|LISTRAS (cdr p_lpiv) (|L|BULT (car p_lpiv))))
		)
	)
)

;"|P|LISTPENDS"
(defun |P|LISTPENDS (p_lpivs p_pa)
	(cond
		((null p_lpivs) nil)
		((null p_pa) (|P|LISTPENDS (cdr p_lpivs) (|L|BULT (car p_lpivs))))
		(T
			(setq p_pt (|L|BULT (car p_lpivs)) p_i (|P|I p_pa p_pt))
			(append (list (list (car p_pa) p_i) (list (car p_pt) p_i)) (|P|LISTPENDS (cdr p_lpivs) p_pt))
		)
	)
)

;listar puntos singulares de la rasante
;"|P|LPSR"
(defun |P|LPSR (p_lras p_ta); g_ras
	(cond
		((null p_lras) nil)
		(T
			(if (eq (last (car p_lras)) "C")
				(cons (list (caar p_lras) "PCV") (|P|LPSR (cdr p_lras) "C"))
				(if (eq p_ta "C")
					(cons (list (caar p_lras) "FCV") (|P|LPSR (cdr p_lras) "P"))
					(cons (list (caar p_lras) "PIV") (|P|LPSR (cdr p_lras) "P"))
				)
			)
		)
	)
)

;"|P|OBTDIF"
(defun |P|OBTDIF (p_lt p_lras)
	(cond
		((null p_lt) nil)
		(T
			(cons (list (caar p_lt) (- (|P|ELEV (caar p_lt) p_lras) (cadar p_lt))) (|P|OBTDIF (cdr p_lt) p_lras))
		)
	)
)

;replantear una curva vertical
;"|P|REPCV"
(defun |P|REPCV (p_seg / p_pi p_param p_lcv p_ii p_if) g_ras
	(defun |P|LISTPTCV (p_nrep p_dist p_paso)
		(cond
			((= p_nrep -1) nil)
			(T
;;;				(+ elevi (* pendini (- pr prini)) (* (/ difpend (* lcv 2)) (expt dist 2)))
				(setq p_xp (+ p_px p_dist) p_yp (+ p_py (* p_dist p_ii) (* (/ p_difp (* p_lcv 2)) (expt p_dist 2)))) 
				(cons (list p_xp p_yp) (|P|LISTPTCV (1- p_nrep) (+ p_dist p_paso) p_paso))
			)
		)
	)

	(setq p_pi (|L|SUBL p_seg 2) p_ii (caddr p_seg) p_if (cadddr p_seg) p_lcv (car (cddddr p_seg)) p_difp (- p_if p_ii) p_param (/ (|P|PARAM p_lcv p_difp) 100))
	(cond
		((<= p_param 200.0)
			(setq p_direp 1.0 p_nrep (1- (fix p_lcv)))
		)
		((<= p_param 1000.0)
			(setq p_direp 2.0 p_nrep (1- (fix (/ p_lcv 2))))
		)
		((<= p_param 5000.0)
			(setq p_direp 5.0 p_nrep (1- (fix (/ p_lcv 5))))
		)
		(T (setq p_direp 10.0 p_nrep (1- (fix (/ p_lcv 10)))))
	)
	(setq p_px (car p_pi) p_py (cadr p_pi))
	(|P|LISTPTCV p_nrep p_direp p_direp)
)

;"|P|PRAS3D"
(defun |P|PRAS3D (p_lras)
	(defun |P|CPTS (p_lprc)
		(cond
			((null p_lprc) nil)
			(T
				(setq p_est (caar p_lprc) p_elev (cadar p_lprc) p_pt (|K|Z+ (|K|XY (car (|A|PINS p_est g_alin))) p_elev))
				(cons (list p_est p_pt) (|P|CPTS (cdr p_lprc)))
			)

		)
	)
  
	(cond
		((null p_lras) nil)
		((eq (last (car p_lras)) "P")
			(setq p_est (caar p_lras) p_elev (cadar p_lras) p_pt (|K|Z+ (|K|XY (car (|A|PINS p_est g_alin))) p_elev))
			(cons (list p_est p_pt) (|P|PRAS3D (cdr p_lras)))
		)
		(T
			(append (|P|CPTS (|P|REPCV (car p_lras))) (|P|PRAS3D (cdr p_lras)))
		)
	)
)

;"|P|COTA"
(defun |P|COTA (p_listpr)
	(cond
		((null p_listpr) nil)
		(T
			(cons (list (caar p_listpr) (|P|ELEV (caar p_listpr) g_ras)) (|P|COTA (cdr p_listpr)))
		)
	)
)

;"|P|LM&M"
(defun |P|LM&M (p_lpts p_pa p_signo p_lmax p_lmin); lm&m
	(cond
		((null p_lpts)
			(if (= p_signo >)
				(setq p_lmax (cons p_pa p_lmax))
				(setq p_lmin (cons p_pa p_lmin))
			)
			(list (reverse p_lmax) (reverse p_lmin))
		)
		((null p_signo)
			(cond
				((> (cadr (car p_lpts)) (cadr p_pa))
					(|P|LM&M (cdr p_lpts) (car p_lpts) > p_lmax (cons p_pa p_lmin))
				)
				((< (cadr (car p_lpts)) (cadr p_pa))
					(|P|LM&M (cdr p_lpts) (car p_lpts) < (cons p_pa p_lmax) p_lmin)
				)
				(T
					(|P|LM&M (cdr p_lpts) (car p_lpts) nil p_lmax p_lmin)
				)
			)
		)
		((or (p_signo (cadar p_lpts) (cadr p_pa)) (= (cadar p_lpts) (cadr p_pa)))
			(|P|LM&M (cdr p_lpts) (car p_lpts) p_signo p_lmax p_lmin)
		)
		(T
			(if (= p_signo >)
				(|P|LM&M (cdr p_lpts) (car p_lpts) < (cons p_pa p_lmax) p_lmin)
				(|P|LM&M (cdr p_lpts) (car p_lpts) > p_lmax (cons p_pa p_lmin))
			)
		)
	)
)

;---------------------------------------------------------------------------------------------------------------------------------------------------------
;				 			R	A	S	A	N	T	E
;---------------------------------------------------------------------------------------------------------------------------------------------------------
;"|P|RASANTE"
(defun |P|RASANTE (p_lpiv p_upt)
	(cond
		((null p_lpiv) nil)
		((< (length p_lpiv) 2)
			(setq p_pivf (|L|SUBL (car p_lpiv) 2))
			(|FV|DIBFORM (list p_upt p_pivf) "Pl_Rasante" 128 g_pipl g_pcpl 1.0 g_defv)
			(list (list (car p_pivf) (cadr p_pivf) 0))
		)
		(T
			(setq p_seg (car p_lpiv) p_lpiv (cdr p_lpiv) p_pseg (car p_lpiv)
			      p_piv (|L|IAR (|L|SUBL p_seg 2)) p_ppiv (|L|IAR (|L|SUBL p_pseg 2))
			      p_i2 (|P|I p_piv p_ppiv) p_rcp 1.0 p_param (caddr p_seg) p_lcv (* (cadddr p_seg) 1.0)
			)
			(if p_upt
				(setq p_i1 (|P|I p_upt p_piv))
				(setq p_i1 nil)
			)
			(cond
				((> p_lcv 0)
					(setq p_xpcv (- (car p_piv) (/ p_lcv 2)) p_xfcv (+ p_xpcv p_lcv)
					      p_ypcv (+ (cadr p_upt) (* (- p_xpcv (car p_upt)) p_i1))
					      p_yfcv (+ (cadr p_piv) (* (/ p_lcv 2) p_i2))
					      p_pcv (list p_xpcv p_ypcv) p_fcv (list p_xfcv p_yfcv)
					      p_tpcv (|K|POLAR p_pcv (|K|ANG p_pcv p_piv) (* p_lcv 0.2))
					      p_tfcv (|K|POLAR p_fcv (|K|ANG p_fcv p_piv) (* p_lcv 0.2))
					      p_liscv (cons p_pcv (|P|REPCV (list p_xpcv p_ypcv p_i1 p_i2 p_lcv)))
					      p_liscv (append p_liscv (list p_fcv))
					)
					(|O|CIRCLE (|FV|DESP p_pcv g_pipl g_pcpl g_defv 1) p_rcp "Pl_polpiv")
					(|O|CIRCLE (|FV|DESP p_piv g_pipl g_pcpl g_defv 1) p_rcp "Pl_polpiv")
					(|O|CIRCLE (|FV|DESP p_fcv g_pipl g_pcpl g_defv 1) p_rcp "Pl_polpiv")
					(|FV|DIBFORM (list p_tpcv p_piv p_tfcv) "Pl_polpiv" 128 g_pipl g_pcpl 1.0 g_defv)
					(|FV|DIBFORM (list p_upt p_pcv) "Pl_Rasante" 128 g_pipl g_pcpl 1.0 g_defv)
					(|FV|DIBFORM p_liscv "Pl_Rasante" 128 g_pipl g_pcpl 1.0 g_defv)
					(cons (list (car p_piv) (cadr p_piv) p_lcv) (|P|RASANTE p_lpiv p_fcv))
				)
				(T
					(if p_upt
						(|FV|DIBFORM (list p_upt p_piv) "Pl_Rasante" 128 g_pipl g_pcpl 1.0 g_defv)
					)
					(|O|CIRCLE (|FV|DESP p_piv g_pipl g_pcpl g_defv 1) p_rcp "Pl_polpiv")
					(cons (list (car p_piv) (cadr p_piv) 0) (|P|RASANTE p_lpiv p_piv))
				)
			)
		)
	)
)

;"|P|DIBRAS"
(defun |P|DIBRAS (p_lpiv p_upt p_lpol p_lras p_dpl)
	(cond
		((< (length p_lpiv) 2)
			(setq p_pivf (|L|SUBL (car p_lpiv) 2))
			(|FV|DIBFORM (list p_upt p_pivf) p_lras 128 (car p_dpl) (cadr p_dpl) 1.0 (last p_dpl))
			nil
		)
		(T
			(setq p_seg (car p_lpiv) p_lpiv (cdr p_lpiv) p_pseg (car p_lpiv)
			      p_piv (|L|IAR (|L|SUBL p_seg 2)) p_ppiv (|L|IAR (|L|SUBL p_pseg 2))
			      p_i2 (|P|I p_piv p_ppiv) p_rcp 1.0 p_lcv (* (last p_seg) 1.0)
			)
			(cond
				((> p_lcv 0)
					(setq p_i1 (|P|I p_upt p_piv) p_difp (- p_i2 p_i1) p_param (|P|PARAM p_lcv p_difp)
					      p_xpcv (- (car p_piv) (/ p_lcv 2)) p_xfcv (+ p_xpcv p_lcv)
					      p_ypcv (+ (cadr p_upt) (* (- p_xpcv (car p_upt)) p_i1))
					      p_yfcv (+ (cadr p_piv) (* (/ p_lcv 2) p_i2))
					      p_pcv (list p_xpcv p_ypcv) p_fcv (list p_xfcv p_yfcv)
					      p_tpcv (|K|POLAR p_pcv (|K|ANG p_pcv p_piv) (* p_lcv 0.2))
					      p_tfcv (|K|POLAR p_fcv (|K|ANG p_fcv p_piv) (* p_lcv 0.2))
					      p_liscv (cons p_pcv (|P|REPCV (list p_xpcv p_ypcv p_i1 p_i2 p_lcv)))
					      p_liscv (append p_liscv (list p_fcv))
					)
					(|O|CIRCLE (|FV|DESP p_pcv (car p_dpl) (cadr p_dpl) (last p_dpl) 1) p_rcp p_lpol)
					(|O|CIRCLE (|FV|DESP p_piv (car p_dpl) (cadr p_dpl) (last p_dpl) 1) p_rcp p_lpol)
					(|O|CIRCLE (|FV|DESP p_fcv (car p_dpl) (cadr p_dpl) (last p_dpl) 1) p_rcp p_lpol)
					(|FV|DIBFORM (list p_tpcv p_piv p_tfcv) p_lpol 128 (car p_dpl) (cadr p_dpl) 1.0 (last p_dpl))
					(|FV|DIBFORM (list p_upt p_pcv) p_lras 128 (car p_dpl) (cadr p_dpl) 1.0 (last p_dpl))
					(|FV|DIBFORM p_liscv p_lras 128 (car p_dpl) (cadr p_dpl) 1.0 (last p_dpl))
					(|P|DIBRAS p_lpiv p_fcv p_lpol p_lras p_dpl)
				)
				(T
					(|FV|DIBFORM (list p_upt p_piv) p_lras 128 (car p_dpl) (cadr p_dpl) 1.0 (last p_dpl))
					(|O|CIRCLE (|FV|DESP p_piv (car p_dpl) (cadr p_dpl) (last p_dpl) 1) p_rcp p_lpol)
					(|P|DIBRAS p_lpiv p_piv p_lpol p_lras p_dpl)
				)
			)
		)
	)
)

(defun |P|LISCUN (p_ssl)
	(cond
		((null p_ssl) nil)
		(T
			(setq p_ent (entget (car p_ssl)) p_tip (|O|ITEM 0 p_ent))
			(if (eq p_tip "LINE")
				(cons (list (|O|ITEM 10 p_ent) (|O|ITEM 11 p_ent)) (|P|LISCUN (cdr p_ssl)))
				(append (|A|LISSEG (|A|LVPOLI (car p_ssl))) (|P|LISCUN (cdr p_ssl)))
			)
		)
	)
)

(defun |P|CUNETAS ()
	(setq p_lgpl (|N|SIGPAS (car (last g_terr)) 25) p_hgpl (* (+ (- (|N|SIGPAS (caadr (|L|M&M g_terr)) 1) g_pcpl) 1) 10)
	      p_esdg (list (+ (car g_pipl) p_lgpl) (+ (cadr g_pipl) p_hgpl))
	      p_lsci (|P|LISCUN (|L|SSL (ssget "W" g_pipl p_esdg '((8 . "A_Cuneta_Izq")))))
	      p_lscd (|P|LISCUN (|L|SSL (ssget "W" g_pipl p_esdg '((8 . "A_Cuneta_Der")))))
	      p_lscc (|P|LISCUN (|L|SSL (ssget "W" g_pipl p_esdg '((8 . "A_Cuneta_Cen")))))
	      p_lsci (append p_lsci (|P|LISCUN (|L|SSL (ssget "W" g_pipl p_esdg '((8 . "A_Ambas_Cunetas"))))))
	      p_lscd (append p_lscd (|P|LISCUN (|L|SSL (ssget "W" g_pipl p_esdg '((8 . "A_Ambas_Cunetas"))))))
	)
	(list p_lsci p_lscc p_lscd)
)
