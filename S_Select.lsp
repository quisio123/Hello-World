;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;	C	O	N	J	U	N	T	O	S		D	E		S	E	L	E	C	C	I	O	N
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(setq <or '(-4 . "<OR") or> '(-4 . "OR>")  <and '(-4 . "<AND") and> '(-4 . "AND>") 
      s_flist (list
		<and
			<or
				(cons 0 "ARC") (cons 0 "LINE") (cons 0 "LWPOLYLINE")
			or>
			(cons 410 "Model")
		and>
	      )
)

(defun |S|SELPMS (s_pt / s_resp)
	(setq s_dpt (|K|TRANS s_pt nil) s_pt (|K|XY (car s_dpt)) s_nvp (cadr s_dpt)
	      s_vp (entget s_nvp) s_fesc (/ (|O|ITEM 41 s_vp) (|O|ITEM 45 s_vp)) s_vtwist (|O|ITEM 51 s_vp)
	      s_apert (getvar "APERTURE") s_vsize (getvar "VIEWSIZE") s_scsize (getvar "SCREENSIZE") s_pickb (getvar "PICKBOX")
	      s_%1 (/ s_vsize (cadr s_scsize)) s_%2 (/ (* s_%1 s_apert) s_fesc 2)
	      s_ssms (ssget "X" s_flist) s_resp '() s_listlf (|O|LISTLF T) s_resp nil
	)
	(while (and s_ssms (setq s_ssn (ssname s_ssms 0)))
		(ssdel s_ssn s_ssms)
		(setq s_ent (entget s_ssn) s_tip (|O|ITEM 0 s_ent) s_lay (|O|ITEM 8 s_ent))
		(cond
			((member s_lay s_listlf))
			((eq s_tip "ARC")
				(setq s_cen (|O|ITEM 10 s_ent) s_ra (|O|ITEM 40 s_ent) s_ai (|O|ITEM 50 s_ent) s_af (|O|ITEM 51 s_ent)
				      s_ap (|K|ANG s_cen s_pt) s_dp (|K|DISTH s_cen s_pt)
				)
				(cond
					((> s_ai s_af)
						(if (and (equal s_dp s_ra s_%2) (or (|N|ENTRE s_ap (list s_ai (* PI 2))) (|N|ENTRE s_ap (list 0.0 s_af))))
							(setq s_resp s_ssn s_ssms nil)
						)
					)
					(T
						(if (and (equal s_dp s_ra s_%2) (|N|ENTRE s_ap (list s_ai s_af)))
							(setq s_resp s_ssn s_ssms nil)
						)
					)
				)
			)
			((eq s_tip "LINE")
				(setq s_pi (|K|XY (|O|ITEM 10 s_ent)) s_pf (|K|XY (|O|ITEM 11 s_ent)))
				(if (and (|C|ENRECTA s_pt (list s_pi s_pf)) (<= (|K|DISTH s_pt (|GC|PROYPR s_pt (list s_pi s_pf))) s_%2))
					(setq s_resp s_ssn s_ssms nil)
				)
			)
			((eq s_tip "LWPOLYLINE")
				(setq s_lseg (|A|PRALIN (|S|LISSEG (|L|ELIMDUP (|L|BULT (|S|LISVER s_ent)))) 0.0))
				(if (and (setq s_idp (|A|IDP s_pt s_lseg nil)) (<= (cadr s_idp) s_%2))
					(setq s_resp s_ssn s_ssms nil)
				)
			)
		)
	)
	s_resp
;;;	(setq s_ssms nil s_resp (|L|ORDENAR s_resp) s_dent (car s_resp) s_di (car s_dent))
;;;	(if (<= s_di s_coef) (cadr s_dent) nil)
)

(defun |S|SELENT (s_msg)
	(defun |S|SELECT (s_cod s_coord / s_obn)
		(cond
			((or (eq s_cod 2) (eq s_cod 25)) nil)
			((eq s_cod 3)
				(if (setq s_ss (ssget s_coord))
					(setq s_obn (ssname s_ss 0) s_ss nil)
;;;					(setq s_obn (|S|SELPMS s_coord))
				  (progn
					(setq s_dpms (|K|TRANS s_coord nil) s_pms (car s_dpms) s_vp (entget (cadr s_dpms)))
				 	(command "._mspace")
				  	(setvar "CVPORT" (|O|ITEM 69 s_vp))
				  	(setq s_ss (ssget s_pms))
				 	(command "._pspace")
					(if s_ss (setq s_obn (ssname s_ss 0) s_ss nil))
				  )
				)
				s_obn
			)
			(T
				(setq s_pos (grread T 4 2) s_cod (car s_pos) s_coord (cadr s_pos))
				(|S|SELECT s_cod s_coord)
			)
		)
	)
  
	(princ (strcat "\n" s_msg))
	(|S|SELECT nil nil)
)


(defun |S|SELAR (s_msg)
	(defun |S|SELAR1 (s_cod s_val / s_obn)
		(cond
			((eq s_cod 2) s_val)
			((eq s_cod 25) 13)
			((eq s_cod 3)
				(if (setq s_ss (ssget s_val))
					(setq s_obn (ssname s_ss 0))
					(setq s_obn nil)
				)
				(setq s_ss nil)
				s_obn
			)
			(T
				(setq s_pos (grread T 4 2) s_cod (car s_pos) s_val (cadr s_pos))
				(|S|SELAR1 s_cod s_val)
			)
		)
	)
  
	(princ (strcat "\n" s_msg))
	(|S|SELAR1 nil nil)
)

(defun |S|SELEOP (s_msg)
	(defun |S|SELEC (s_cod s_coord / s_obn)
		(cond
			((or (eq s_cod 2) (eq s_cod 25)) nil)
			((eq s_cod 3)
				(if (setq s_ss (ssget s_coord))
					(setq s_obn (ssname s_ss 0))
					(setq s_obn s_coord)
				)
				(setq s_ss nil)
				s_obn
			)
			(T
				(setq s_pos (grread T 4 2) s_cod (car s_pos) s_coord (cadr s_pos))
				(|S|SELEC s_cod s_coord)
			)
		)
	)
  
	(princ (strcat "\n" s_msg))
	(|S|SELEC nil nil)
)

;devolver una lista con los vértices y el factor de curvatura de una lwpolyline
;"|S|LISVER"
(defun |S|LISVER (s_lpol)
	(cond
		((null s_lpol) '())
		(T (cons (list (|O|ITEM 10 s_lpol) (|O|ITEM 42 s_lpol)) (|S|LISVER (cdr (member (assoc 42 s_lpol) s_lpol)))))
	)
)

;listar segmentos de una lwpolyline
;"|S|LISSEG"
(defun |S|LISSEG (s_lpol)
	(cond
		((null s_lpol) '())
		((= (length s_lpol) 1) '())
		((/= (cadar s_lpol) 0.0)
			(setq s_delta (|C|DELTA (cadar s_lpol) nil nil) s_rc (|C|RADIO s_delta (caar s_lpol) (caadr s_lpol) nil))
			(cons (list (caar s_lpol) (caadr s_lpol) nil s_delta s_rc "A") (|S|LISSEG (cdr s_lpol)))
		)
		(T (cons (list (caar s_lpol) (caadr s_lpol)) (|S|LISSEG (cdr s_lpol))))
	)
)

;seleccionar elementos para un alineamiento
;"|S|RESEL"
(defun |S|RESEL (s_lel / s_nel)
	(cond
		((setq s_nel (|S|SELENT "seleccione una entidad"))
			(cond
				((member s_nel s_lel)
					(|S|RESEL s_lel)
				)
				(T (redraw s_nel 3) (cons s_nel (|S|RESEL s_lel)))
			)
		)
		(T s_lel)
	)
)

;procesar elementos de un conjunto de selección
(defun |S|PROCEL (s_lisel)
	(cond
		((null s_lisel) nil)
		((setq s_del (entget (car s_lisel)))
			(setq s_tip (|O|ITEM 0 s_del))
			(cond
				((eq s_tip "LWPOLYLINE")
					(append (|S|LISSEG (|L|BULT (|S|LISVER s_del))) (|S|PROCEL (cdr s_lisel)))
				)
				((eq s_tip "LINE")
					(cons (list (|L|BULT (|O|ITEM 10 s_del)) (|L|BULT (|O|ITEM 11 s_del))) (|S|PROCEL (cdr s_lisel)))
				)
				((eq s_tip "ARC")
					(setq s_cen (|O|ITEM 10 s_del) s_rc (|O|ITEM 40 s_del) s_ai (|O|ITEM 50 s_del) s_af (|O|ITEM 51 s_del))
					(setq s_delta (|C|DELTA nil s_ai s_af))
					(cons (|A|ARC2SEG s_del) (|S|PROCEL (cdr s_lisel)))
				)
				((eq s_tip "CIRCLE")
					(setq s_cen (|O|ITEM 10 s_del) s_rc (|O|ITEM 40 s_del))
					(cons (list s_cen s_rc "C") (|S|PROCEL (cdr s_lisel)))
				)
				(T (|S|PROCEL (cdr s_lisel)))
			)
		)
	)
)

; buscar intersecciones de un segmento con una polilínea cerrada
(defun |S|CINT (s_p1 s_p2 s_lplim s_pa)
	(cond
		((null s_lplim) 0)
		((inters s_p1 s_p2 s_pa (car s_lplim))
			(1+ (|S|CINT s_p1 s_p2 (cdr s_lplim) (car s_lplim)))
		)
		(T (|S|CINT s_p1 s_p2 (cdr s_lplim) (car s_lplim)))
	)
)

;determinar si un punto 2D está dentro de un polígono cerrado
;"|S|DENTRO"
(defun |S|DENTRO (s_pt s_polc)
	(setq s_sp (|K|X+ s_pt 100000.0))
	(or (member s_pt s_polc) (not (|N|ESPAR (|S|CINT s_pt s_sp s_polc (last s_polc)))))
)

;---------------------------------------------------------------------------------------------------------------------------------------------------------------
;							C	O	N	T	O	R	N	O	S
;---------------------------------------------------------------------------------------------------------------------------------------------------------------
;"|S|LIMPAN"
(defun |S|LIMPAN (/ s_cp s_dimp s_tamp)
    (setq s_tamp (/ (getvar "VIEWSIZE") 2.0)
	  s_cp (getvar "VIEWCTR")
	  s_dimp (list (* s_tamp (apply '/ (getvar "SCREENSIZE"))) s_tamp)
	  s_eii (mapcar '- s_cp s_dimp) s_esd (mapcar '+ s_cp s_dimp)
	  s_esi (list (car s_eii) (cadr s_esd)) s_eid (list (car s_esd) (cadr s_eii)) 
    )
    (list s_eii s_esi s_esd s_eid)
)

(defun |S|BLAR (s_lsg s_rec)
	(cond
		((null s_lsg) nil)
		((> (length s_lsg) 10000)
			(setq s_divl (|L|DIVM s_lsg))
			(append (|S|BLAR (car s_divl) s_rec) (|S|BLAR (cadr s_divl) s_rec))
		)
		((setq s_int (car (|I|SS (car s_lsg) s_rec)))
			(cons (list (|K|DIST (car s_rec) s_int) s_int (car s_lsg)) (|S|BLAR (cdr s_lsg) s_rec))
		)
		(T (|S|BLAR (cdr s_lsg) s_rec))
	)
)

(defun |S|BAR (s_lsg s_rec)
	(car (|L|ORDENAR (|S|BLAR s_lsg s_rec)))
)

(defun |S|BINT (s_lsg)
	(cond
		((null s_lsg) nil)
		((= (length s_lsg) 1) nil)
		(T (append (|I|INTSA (car s_lsg) (cdr s_lsg)) (|S|BINT (cdr s_lsg))))
	)
)

(defun |S|ANGS (s_lpts s_po)
	(cond
		((null s_lpts) nil)
		(T (cons (list (|K|ANG s_po (car s_lpts)) (|K|DISTH s_po (car s_lpts)) (car s_lpts)) (|S|ANGS (cdr s_lpts) s_po)))
	)
)

(defun |S|ESAR (s_p1 s_p2 s_lsg)
	(cond
		((null s_lsg) nil)
		((and (|GC|ENSEG s_p1 (car s_lsg)) (|GC|ENSEG s_p2 (car s_lsg)))
			T
		)
		(T (|S|ESAR s_p1 s_p2 (cdr s_lsg)))
	)
)

(defun |S|ELPDUP (s_lps)
	(defun |S|ESTA (s_p s_lp)
		(cond
			((null s_lp) nil)
			((equal s_p (car s_lp) g_tol)
				T
			)
			(T (|S|ESTA s_p (cdr s_lp)))
		)
	)

	(cond
		((null s_lps) nil)
		((|S|ESTA (car s_lps) (cdr s_lps)) (|S|ELPDUP (cdr s_lps)))
		(T (cons (car s_lps) (|S|ELPDUP (cdr s_lps))))
	)
)

;;;(defun |S|ENAR (s_p s_lsg)
;;;	(cond
;;;		((null s_lsg) nil)
;;;		((|GC|ENSEG s_pt (car s_lsg)) T)
;;;		(T (|S|ENAR s_p (cdr s_lsg)))
;;;	)
;;;)

;;;(defun |S|ELINS (s_lint)
;;;	(setq s_res '() s_cont 0 s_len (length s_lpb) s_na (1- s_len) s_lint (append s_lint (list (car s_lint))))
;;;	(repeat s_len
;;;		(setq s_1p (nth s_na s_lint) s_pi (nth s_cont s_lint) s_2p (nth (1+ s_cont) s_lint) s_na s_cont s_cont (1+ s_cont))
;;;		(if (not (|GC|COLINEAL s_1p s_pi s_2p 0.01)) (setq s_res (cons s_pi s_res)))
;;;	)
;;;	s_res
;;;)

;encontrar el contorno que encierra a un punto
;"|S|CONTORNO"
(defun |S|CONTORNO (s_pt s_ctab s_dib)
	(defun |S|PROCLPI (s_lpin s_pr)
		(cond
			((null s_lpin) nil)
			(T
				(setq s_pi (car s_lpin) s_dar (|S|BAR s_lseg (list s_pr s_pi))
				      s_dia (car s_dar) s_pia (cadr s_dar) s_ar (caddr s_dar)
				      s_dip (|K|DIST s_pr s_pi)
				)
				(cond
					((equal s_pia s_pi g_tol) (cons s_pi (|S|PROCLPI (cdr s_lpin) s_pr)))
					((>= s_dia s_dip) (cons s_pi (|S|PROCLPI (cdr s_lpin) s_pr)))
					(T (|S|PROCLPI (cdr s_lpin) s_pr))
				)
			)
		)
	)
	(defun |S|AGAR (s_pp s_sp s_lpl)
		(setq s_pm (|K|PMED s_pp s_sp) s_lpl (mapcar 'last (|L|ORDENAR (|S|ANGS (|S|ELPDUP s_lpl) s_pm)))
		      s_lapb (|L|LIBRES (|S|PROCLPI s_lpl (|K|POLAR s_pm (|K|ANG s_pt s_pm) 0.01)) s_lpb nil)
		)
		(|S|PROCLB (|A|LISSEG (cons s_pp (append s_lapb (list s_sp)))))
	)
	(defun |S|PROCLB (s_lbo)
		(cond
			((null s_lbo) nil)
			((|S|ESAR (caar s_lbo) (cadar s_lbo) s_lseg)
				(cons (car s_lbo) (|S|PROCLB (cdr s_lbo)))
			)
			(T (append (|S|AGAR (caar s_lbo) (cadar s_lbo) s_lpi) (|S|PROCLB (cdr s_lbo))))
		)
	)
	(defun |S|ENZONA (s_lar s_rec)
		(cond
			((null s_lar) nil)
			((or (|S|DENTRO (caar s_lar) s_rec) (|S|DENTRO (cadar s_lar) s_rec))
				(cons (car s_lar) (|S|ENZONA (cdr s_lar) s_rec))
			)
			(T (|S|ENZONA (cdr s_lar) s_rec))
		)
	)
	(setq s_lim (|S|LIMPAN))
	(if (not (eq s_ctab "Model"))
	  (progn
		(setq s_nvp (|O|GETVP s_pt s_ctab) s_vp (entget s_nvp) s_pt (car (|K|TRANS s_pt nil))
		      s_vpid (|O|ITEM 69 s_vp) s_fesc (/ (|O|ITEM 41 s_vp) (|O|ITEM 45 s_vp))
		      s_lim (mapcar '(lambda (el) (car (|K|TRANS el nil))) s_lim)
		)
		(command "._mspace")
	  	(setq s_lnent (|L|SSL (ssget "CP" s_lim)))
	  	(command "._pspace")
	  )
	  	(setq s_lnent (|L|SSL (ssget "CP" s_lim)) s_vpid nil)
	)
	(setq s_lmax (|K|DIST (car s_lim) (caddr s_lim)) s_lseg (|S|ENZONA (|L|ELIMDUPS (|S|PROCEL s_lnent)) s_lim)
	      s_lpi (mapcar 'last (|L|ORDENAR (|S|ANGS (|S|ELPDUP (|S|BINT s_lseg)) s_pt))) s_lpb '()
	) ;(|O|LWPOLY s_lpi "1Paso" 1) (REDRAW)
	(setq s_lpb (|S|PROCLPI s_lpi s_pt)) ;(|O|LWPOLY s_lpb "2Paso" 1) (REDRAW)
	(setq s_lsb (|S|PROCLB (|A|LISSEG (append s_lpb (list (car s_lpb))))) s_lpb (|A|LISTAV s_lsb))
	(if (and (> (length s_lpb) 2) (|S|DENTRO s_pt s_lpb))
	  (progn
		(if s_dib (|O|LWPOLY (cdr s_lpb) "CONTORNO" 1))
	  	(if s_vpid
	  		(list (mapcar '(lambda (el) (|K|TRANS el s_vpid)) s_lpb) (|GC|GAUSS (cdr s_lpb)) s_fesc)
	  		(list s_lpb (|GC|GAUSS (cdr s_lpb)) 1.0)
		)
	  )
	)
)

(defun |S|MENSURA ()
	(defun |S|ANGDI (s_lp s_pa)
		(cond
			((= (length s_lp) 1) nil)
			(T
				(setq s_pt (car s_lp) s_ps (cadr s_lp))
				(setq s_di (* (|K|DISTH s_pt s_ps) s_fe)
				      s_ang (|N|ANGULO s_pt s_pa s_ps) s_ai (|K|ANG s_pt s_pa) s_af (|K|ANG s_pt s_ps)
				      s_pma (|K|PMED s_pa s_ps) s_pid (|K|POLAR (|K|PMED s_pt s_ps) (+ s_af (/ PI 2)) 1) s_cpc (|N|CUADRANTE s_af)
				      s_ain (- s_ai (/ s_ang 2)) s_pia (|K|POLAR s_pt s_ain 2) s_cai (|N|CUADRANTE s_ain)
				)
				(if (member s_cpc '(2 3)) (setq s_af (+ s_af PI) s_alv 3) (setq s_alv 0))
				(|O|TEXT s_pid s_pid 3 (rtos s_di 2 2) s_af 0.7 0.0 "ROMANS" 0 1 s_alv "L_TEXTOS")
				(if (member s_cai '(2 3))
					(setq s_ain (+ s_ain PI) s_alh 2)
					(setq s_alh 0)
				)
				(|O|TEXT s_pia s_pia 3 (|T|D_G (angtos s_ang 1 4)) s_ain 0.7 0.0 "ROMANS" 0 s_alh 2 "L_TEXTOS")
				(|S|ANGDI (cdr s_lp) s_pt)
	  		)
		)
	)
  
	(setq s_ctab (getvar "CTAB") s_pt (|K|XY (getpoint "\marque un punto interno del lote a mensurar"))
	      s_dl (|S|CONTORNO s_pt s_ctab nil) s_lpc (car s_dl) s_area (cadr s_dl) s_fe (caddr s_dl)
	      s_cl (|K|CENTRO s_lpc)
	)
	(|O|TEXT s_cl s_cl 4 (strcat (rtos s_area 2 2) "m2") 0 0.7 0.0 "ROMANS" 0 1 2 "L_TEXTOS")
	(|S|ANGDI s_lpc (last s_lpc))
)

