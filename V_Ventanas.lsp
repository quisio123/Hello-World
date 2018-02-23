;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;								L	A	Y	O	U	T	S
;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


;"|LO|LIMPAN"
(defun |LO|LIMPAN (/ s_cp s_dimp s_tamp)
    (setq lo_tamp (/ (getvar "VIEWSIZE") 2.0)
	  lo_cp (getvar "VIEWCTR")
	  lo_dimp (list (* lo_tamp (apply '/ (getvar "SCREENSIZE"))) lo_tamp)
	  lo_eii (mapcar '- lo_cp lo_dimp) lo_esd (mapcar '+ lo_cp lo_dimp)
	  lo_esi (list (car lo_eii) (cadr lo_esd)) lo_eid (list (car lo_esd) (cadr lo_eii)) 
    )
    (list lo_eii lo_esi lo_esd lo_eid)
)

;crear un nuevo Layout
;"|LO|CREA"
(defun |LO|CREA (lo_nombre)
	(if (not (member lo_nombre (layoutlist)))
		(command "._LAYOUT" "N" lo_nombre)
		(|LO|ACTIVAR lo_nombre)
	)
)

;"|LO|RENOM"
(defun |LO|RENOM (lo_nact lo_nnom)
	(if (member lo_nact (layoutlist))
		(command "._LAYOUT" "R" lo_nact lo_nnom)
	)
)

;"|LO|ACTIVAR"
(defun |LO|ACTIVAR (lo_nombre)
	(setvar "CTAB" lo_nombre)
	(|O|PSETUP lo_nombre)
)

;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;								V	P	O	R	T	S
;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;crear una ventana (mview)
;"|MV|CREA"
(defun |MV|CREA (mv_lp)
	(|O|ACTLAY "Defpoints")
	(if (= (length mv_lp) 2)
		(command "._MVIEW" (car mv_lp) (cadr mv_lp) "._mspace" "._plan" "W")
	  (progn
		(command "._MVIEW" "P")
	    	(foreach mv_pt mv_lp
			(command mv_pt)
		)
		(command "C" "._mspace" "._plan" "W")
	  )
	)
	(|O|ACTLAY "0")
	(entlast)
)

;recortar una ventana
;"|MV|CLIP"
(defun |MV|CLIP (mv_vp mv_lpc)
	(|O|LWPOLY mv_lpc "Defpoints" 129)
	(setq mv_polc (entlast))
	(command "._vpclip" mv_vp mv_polc)
)

;enfocar una ventana
;"|MV|ENFOQUE"
(defun |MV|ENFOQUE (mv_nv)
  	(command "._mspace")
	(setvar "CVPORT" mv_nv)
;;;	(if (eq mv_nva 1) (command "._mspace"))
;;;	(while (not (eq (car (vports)) mv_nv)) (command (strcat (chr 7) "R")))
)


;escalar vista
;"|MV|ESC"
(defun |MV|ESC (mv_esch)
	(setq mv_esc (strcat (rtos (/ 1000.0 mv_esch) 2 2) "XP"))
;;;	(command "._mspace" "._ZOOM" mv_esc "._pspace")
	(command "._ZOOM" mv_esc)
)

;rotar vista
;"|MV|ROTAR"
(defun |MV|ROTAR (mv_pti mv_ptf)
;;;	(command "._mspace" "ucs" "3p" mv_pti mv_ptf (|K|POLAR mv_pti (+ (|K|ANG mv_pti mv_ptf) (/ PI 2)) 100.0) "._plan" "" "ucs" "W" "._pspace") 
	(command "ucs" "3p" mv_pti mv_ptf (|K|POLAR mv_pti (+ (|K|ANG mv_pti mv_ptf) (/ PI 2)) 100.0) "._plan" "" "ucs" "W") 
)

;alinear vista
;"|MV|CENTRAR"
(defun |MV|CENTRAR (mv_ptmn)
;;;	(command "._mspace")
	(setq mv_ptma (getvar "VIEWCTR")) ;centro de la vista actual
	(command ".-pan" mv_ptmn mv_ptma "._regen" "._pspace")
)

;gestionar layers en la vista
;"|MV|GESTLAY"
;;;(defun |MV|GESTLAY (mv_llay mv_lop)
;;;	(cond
;;;		((or (null mv_llay) (null mv_lop)) nil)
;;;		(T
;;;			
;;;		)
;;;	)
;;;)

;freezar layers en la vista
;"|MV|FREEZAR"
(defun |MV|FREEZAR (mv_layl)
	(command "._mspace")
	(cond
		((eq (type mv_layl) 'STR)
			(command "._vplayer" "F" mv_layl "" "")
		)
		((eq (type mv_layl) 'LIST)
			(command "._vplayer")
			(foreach mv_lay mv_layl (command "F" mv_lay ""))
			(command "")
		)
	)
;;;	(command "._pspace")
)

;desfreezar layers en la vista
;"|MV|DFREEZAR"
(defun |MV|DFREEZAR (mv_layl)
	(command "._mspace")
	(cond
		((eq (type mv_layl) 'STR)
			(command "._vplayer" "T" mv_layl "" "")
		)
		((eq (type mv_layl) 'LIST)
			(command "._vplayer")
			(foreach mv_lay mv_layl (command "T" mv_lay ""))
			(command "")
		)
	)
;;;	(command "._pspace")
)


;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;									A	L	T	I	M	E	T	R	I	A	S
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;dibujar vport de altimetría
(defun |VA|DIB (v_p1 v_p2 v_ptm v_llayf)
	(|MV|CREA (list v_p1 v_p2))
	(|MV|ESC g_esch)
	(if v_llayf (|MV|FREEZAR v_llayf))
	(|MV|CENTRAR v_ptm)
)

;poner cota del plano de comparación
(defun |VA|PC (v_p1 v_eini v_jh v_lay)
	(|O|TEXT v_p1 v_p1 3.0 (strcat "P.C.: " (rtos v_eini 2 1)) 0 0.8 0 "ROMANS" 0 v_jh 0 "L_Textos")
)

;poner cotas
(defun |VA|COTAS (v_p1 v_eini v_cc v_jh v_inc v_lay)
	(setq v_cota (+ v_eini v_inc) v_p1 (|K|Y+ v_p1 10.0))
	(repeat v_cc
		(|O|TEXT v_p1 v_p1 3.0 (rtos v_cota 2 1) 0 0.75 0 "ROMANS" 0 v_jh 0 v_lay)
		(setq v_p1 (|K|Y+ v_p1 10.0) v_cota (+ v_cota v_inc))
	)
)

;calcular vport de altimetria
(defun |VA|CVP (v_xi v_xf v_alt v_ltn v_lpiv)
	(setq v_mmt (|L|M&M v_ltn) v_mmr (|L|M&M v_lpiv) v_mmct (cadr v_mmt) v_mmcr (cadr v_mmr)
	      v_min (min (cadr v_mmct) (cadr v_mmcr)) v_max (max (car v_mmct) (car v_mmcr))
	      v_am (/ (+ v_min v_max) 2.0)
	)
	(cond
		((<= (- v_max v_min) (- v_alt 2))
			(list v_xf v_am)
		)
		(T (|VA|CVP v_xi (/ (+ v_xi v_xf) 2) v_alt v_ltn v_lpiv))
	)
)

;crear los vports nencesarios para altimetría
(defun |VA|CREA (v_eii v_dy v_pri v_prf v_ltn v_lpiv v_dpl v_lay v_llayf)
	(setq v_prfv 0.0 v_pasov (/ g_defv 10.0) v_alm (* v_dy v_pasov) v_cc (fix (- v_dy 1.0)) v_ci T
	      v_pipl (car v_dpl) v_pc (cadr v_dpl) 
	)
	(while (< v_prfv v_prf)
	 	(setq v_lvp (|VA|CVP v_pri v_prf v_dy v_ltn v_lpiv) v_prfv (|N|SIGPAS (car v_lvp) (* g_eschi 25.0))
		      v_emin (fix (- (cadr v_lvp) (/ v_dy 2))) v_am (+ v_emin (/ v_dy 2))
		      v_esd (|K|Y+ (|K|X+ v_eii (/ (- v_prfv v_pri) g_eschi)) (* v_dy 10.0))
		      v_pmv (|FV|DESP (list (/ (+ v_prfv v_pri) 2) v_am) v_pipl v_pc g_defv 1)
		)
		(|VA|DIB v_eii v_esd v_pmv v_llayf)
		(if v_ci (progn (|VA|PC v_eii v_emin 2 v_lay) (|VA|COTAS v_eii v_emin v_cc 2 1 v_lay) (setq v_ci nil)) (|VA|PC v_eii v_emin 0 v_lay))
		(setq v_eii (list (car v_esd) (cadr v_eii)))
	)
	(|VA|COTAS (list (car v_esd) (cadr v_eii)) v_emin v_cc 0 1 v_lay)
)

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;									P	L	A	N	I	M	E	T	R	I	A	S
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;crear los vports que se necesiten para la planimetría
(defun |VP|CREA (vp_eii vp_dy vp_pri vp_prf vp_prfl vp_alin vp_llayf vp_+l)
	(defun |VP|PLC (vp_pr vp_la vp_ls vp_pilc vp_pit vp_anl vp_ant)
		(setq vp_atlc1 "LINEA DE CORTE" vp_atlc2 (strcat "Pr. " (rtos vp_pr 2 2))
		      vp_atlc3 (if vp_la (strcat "VIENE DE LAMINA N° " (itoa (1- pl_nl))) (if vp_ls (strcat "CONT. EN LAMINA N° " (itoa (1+ pl_nl))) ""))
		)
		(|B|INSERT "LC" vp_pilc "L_LinCor" vp_anl 0.5 nil)
		(|B|INSERT "TEXTLC" vp_pit "L_LinCor" vp_ant 1 (list vp_atlc1 vp_atlc2 vp_atlc3))
	)
	(defun |VP|VAL (vp_lpi)
		(cond
			((null vp_lpi) T)
			((> (|K|DISTH (reverse (cadar vp_lpi)) (|GC|PROYPR (reverse (cadar vp_lpi)) (list vp_cpi vp_cup))) vp_desvm)
				nil
			)
			(T
				(|VP|VAL (cdr vp_lpi))
			)
		)
	)
	(defun |VP|LISTPB ()
		(setq vp_entn (|MV|CREA (list vp_eii (|K|Y+ (|K|X+ vp_eii (+ (/ (|K|DISTH vp_cpi vp_cup) g_eschi) 20.0)) vp_dy))))
		(|MV|ROTAR vp_cpi vp_cup)
		(|MV|ESC g_esch)
		(if vp_llayf (|MV|FREEZAR vp_llayf))
		(|MV|CENTRAR vp_pm)
		(setq vp_diri (- (cadr vp_dpi) vp_ai) vp_dirf (- (cadr vp_dup) vp_ai)
;;;		      vp_pmi (|K|Y+ vp_eii (/ vp_dy 2)) vp_pmf (|K|X+ vp_pmi (/ (|K|DISTH vp_cpi vp_cup) g_eschi)) 
		      vp_pmi (car (|K|BPTPS vp_cpi (cdr (vports)))) vp_pmf (car (|K|BPTPS vp_cup (cdr (vports))))
		      vp_p1 (|K|POLAR vp_pmi (+ vp_diri (* PI 0.5)) 70.0) vp_p2 (|K|POLAR vp_p1 (* PI 0.25) 70.0) vp_p2 (list (car vp_p2) (+ (cadr vp_eii) vp_dy))
		      vp_p4 (|K|POLAR vp_pmf (+ vp_dirf (* PI 0.5)) 70.0) vp_p3 (|K|POLAR vp_p4 (* PI 0.75) 70.0) vp_p3 (list (car vp_p3) (+ (cadr vp_eii) vp_dy))
		      vp_p5 (|K|POLAR vp_pmf (+ vp_dirf (* PI 1.5)) 70.0) vp_p6 (|K|POLAR vp_p5 (* PI 1.25) 70.0) vp_p6 (list (car vp_p6) (cadr vp_eii))
		      vp_p8 (|K|POLAR vp_pmi (+ vp_diri (* PI 1.5)) 70.0) vp_p7 (|K|POLAR vp_p8 (* PI 1.75) 70.0) vp_p7 (list (car vp_p7) (cadr vp_eii))
		)
		(|MV|CLIP vp_entn (list vp_p1 vp_p2 vp_p3 vp_p4 vp_p5 vp_p6 vp_p7 vp_p8))
		(|B|INSERT "NORTE" (|K|Y+ vp_eii (- vp_dy 35.0)) "L_Simbolos" (- (* PI 2) vp_ai) 1 nil)
		(if (/= vp_pri g_pria)
			(if (/= vp_pri g_pri)
				(|VP|PLC vp_pri nil nil vp_pmi (|K|POLAR vp_pmi (+ vp_diri PI) 7.0) vp_diri (+ vp_diri (* PI 1.5)))
				(|VP|PLC vp_pri (not vp_+l) nil vp_pmi (|K|POLAR vp_pmi (+ vp_diri PI) 7.0) vp_diri (+ vp_diri (* PI 1.5)))
			)
		)
		(if (/= vp_prf g_prfa)
			(if (/= vp_prf g_prf)
				(|VP|PLC vp_prf nil nil vp_pmf (|K|POLAR vp_pmf vp_dirf 7.0) vp_dirf (+ vp_dirf (* PI 0.5)))
				(|VP|PLC vp_prf nil (not vp_+l) vp_pmf (|K|POLAR vp_pmf vp_dirf 7.0) vp_dirf (+ vp_dirf (* PI 0.5)))
			)
		)
	)
	(cond
		((>= vp_pri vp_prf) nil)
		(T
			(setq vp_dpi (|A|PINS vp_pri vp_alin) vp_dup (|A|PINS vp_prf vp_alin)
			      vp_cpi (car vp_dpi) vp_cup (car vp_dup) vp_ai (angle vp_cpi vp_cup)
			      vp_desvm (* (- (/ vp_dy 2) 10.0) g_eschi) vp_pint (|L|OBTSL (|L|PRIFIN (|A|WAYPOINTS vp_alin)) vp_pri vp_prf)
			      vp_pm (|K|PMED vp_cpi vp_cup)
			)
			(if (|VP|VAL vp_pint)
			  (progn
				(|VP|LISTPB)
				(if (> vp_prfl vp_prf)
;;;					(|VP|CREA (|K|X+ vp_eii (+ (/ (|K|DISTH vp_cpi vp_cup) g_eschi) 50.0)) vp_dy vp_prf vp_prfl vp_prfl vp_alin)
					(|VP|CREA (list (- 815.0 (/ (- vp_prfl vp_prf) g_eschi)) (cadr vp_eii)) vp_dy vp_prf vp_prfl vp_prfl vp_alin vp_llayf vp_+l)
				)
			  )
				(|VP|CREA vp_eii vp_dy vp_pri (- vp_prf 100.0) vp_prfl vp_alin vp_llayf vp_+l)
			)
		)
	)
)
