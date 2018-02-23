;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;	O	B	T	E	N	E	R		L	A	S		S	E	C	C	I	O	N	E	S	
;				T	R	A	N	S	V	E	R	S	A	L	E	S						
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;ordenar una sección transversal por su distancia al eje
;"|ST|OBTST"
(defun |ST|LISTT (st_lptt st_pe)
	(cond
		((null st_lptt) nil)
		(T
			(cond
				((equal (|K|XY (car st_lptt)) st_pe 0.005)
					(cons (list 0.0 (caddar st_lptt)) (|ST|LISTT (cdr st_lptt) st_pe)) 
				)
				((equal (|K|ANG st_pe (|K|XY (car st_lptt))) (|K|ANG st_pe st_pi) g_tol)
					(cons (list (- (|K|DISTH st_pe (car st_lptt))) (caddar st_lptt)) (|ST|LISTT (cdr st_lptt) st_pe)) 
				)
				(T
					(cons (list (|K|DISTH st_pe (car st_lptt)) (caddar st_lptt)) (|ST|LISTT (cdr st_lptt) st_pe)) 
				)
			)
		)
	)
)

;obtener puntos para la sección transversal
;"|ST|OBTPTS"
(defun |ST|OBTPTS (st_lar st_p1 st_p2)
	(cond
		((null st_lar) nil)
		((> (length st_lar) 10000)
			(setq st_divl (|L|DIVM st_lar) st_ppl (car st_divl) st_spl (cadr st_divl))
			(append (|ST|OBTPTS st_ppl st_p1 st_p2) (|ST|OBTPTS st_spl st_p1 st_p2))
		)
		((setq st_pin (inters (caar st_lar) (cadar st_lar) st_p1 st_p2))
			(setq st_ar (car st_lar) st_pp (car st_ar) st_sp (cadr st_ar))
			(cons (|K|INTERPOLAR st_pp st_sp st_pin 0.001) (|ST|OBTPTS (cdr st_lar) st_p1 st_p2))
		)
		(T
			(|ST|OBTPTS (cdr st_lar) st_p1 st_p2)
		)
	)
)

;obtener una sección transversal (length g_larg)
;"|ST|OBTST"
(defun |ST|OBTST (st_pe st_pi st_pd)
	(|L|ORDENAR (|ST|LISTT (|L|ELIMDUP (|ST|OBTPTS g_larg (|K|XY st_pi) (|K|XY st_pd))) st_pe))
)

;obtener la sección transversal para una progresiva determinada
;"|ST|OBTSTPR"
(defun |ST|OBTSTPR (st_pr st_sa st_alin / st_dpt st_pt st_ap)
	(setq st_dpt (|A|PINS st_pr st_alin) st_pt (car st_dpt) st_ap (cadr st_dpt))
	(|ST|OBTST st_pt (|K|POLAR st_pt (+ st_ap (* Pi 0.5)) st_sa) (|K|POLAR st_pt (+ st_ap (* Pi 1.5)) st_sa))
)


;obtener secciones tranversales para una lista de progresivas del alineamiento activo
;"|ST|OBTSTS"
(defun |ST|OBTSTRS (st_lpr st_alin st_san)
	(cond
		((null st_lpr) nil)
		(T
			(setq st_dpr (car st_lpr) st_dpt (|A|PINS (car st_dpr) st_alin) st_pt (car st_dpt) st_ap (cadr st_dpt) st_ce (caddr (|K|OBTC st_pt))
			      st_lst (|ST|OBTST st_pt (|K|POLAR st_pt (+ st_ap (* Pi 0.5)) 100.0) (|K|POLAR st_pt (+ st_ap (* Pi 1.5)) 100.0))
			      st_dst (list (car st_dpr) st_ce (cadr st_dpr)) 
			)
			(cons (cons st_dst (|L|OBTSLV (|L|DEPURAR st_lst) (- st_san) st_san)) (|ST|OBTSTS (cdr st_lpr) st_alin st_san))
;;;			(cons (cons (car st_lpr) (|L|OBTSLV (|L|DEPURAR (|ST|OBTSTPR (caar st_lpr) 100.0 st_alin)) (- st_san) st_san)) (|ST|OBTSTS (cdr st_lpr) st_alin st_san))
		)
	)
)
(defun |ST|BUSCST (st_alin st_san)
	(defun |ST|DEPURAR (st_list st_lsta)
		(cond
			((null st_list) nil)
			((null st_lsta) (|ST|DEPURAR (cdr st_list) (car st_list))) 
			((equal (caar st_list) (car st_lsta) 10.0)
				(if (> (length st_lsta) (length (car st_list)))
					(|ST|DEPURAR (cdr st_list) st_lsta)
					(|ST|DEPURAR (cdr st_list) (car st_list))
				)
			)
			(T
				(cons (cons (car st_lsta) (|L|OBTSLV (cdr st_lsta) (- st_san) st_san)) (|ST|DEPURAR (cdr st_list) (car st_list)))
			)
		)
	)
	(setq st_lpr (append (|N|SEQNUM (|A|PRI st_alin) 10.0 (|A|PRF st_alin)) (list (list (|A|PRF st_alin) ""))) st_lsts '())
;;;	(setq st_lpr (append (|N|SEQNUM (|A|PRI g_alin) 10.0 (|A|PRF g_alin)) (list (list (|A|PRF g_alin) ""))) st_lsts '())
	(foreach st_dpr st_lpr
		(setq st_pr (car st_dpr) st_idp (|A|PINS st_pr st_alin) st_pe (car st_idp) st_dir (cadr st_idp)
		      st_ai (+ st_dir (/ PI 2)) st_ad (+ st_dir (* PI 1.5)) st_-dir (+ st_dir PI) st_pli (|K|POLAR st_pe st_ai (* st_san 2.0))
		      st_p1 (|K|POLAR st_pli st_-dir 5.0) st_p2 (|K|POLAR st_pli st_dir 5.0) st_p3 (|K|POLAR st_p2 st_ad (* st_san 4.0)) st_p4 (|K|POLAR st_p3 st_-dir 10.0)
		      st_ss (|K|SELPTS g_lptsm (list st_p1 st_p2 st_p3 st_p4)) st_lsti '() st_lstd '()
		)
		(if (and st_ss (> (length st_ss) 4))
		  (progn
			(foreach st_pt st_ss
				(if (setq st_idp (|A|IDP st_pt st_alin T))
				  (if (|N|ENTRE (car st_idp) (list (- st_pr 5.0) (+ st_pr 5.0)))
					(if (eq (caddr st_idp) "I")
						(setq st_lsti (cons (list (car st_idp) (- (|N|ROUND (cadr st_idp) 2)) (caddr st_pt)) st_lsti))
						(setq st_lstd (cons (list (car st_idp) (|N|ROUND (cadr st_idp) 2) (caddr st_pt)) st_lstd))
					)
				  )
				)
			)
			(if (and st_lsti st_lstd (> (length st_lsti) 1) (> (length st_lstd) 1))
				(setq st_lst (append st_lsti st_lstd) st_pr (max (|L|PROM (mapcar 'car st_lst)) 0.0) st_lst (|L|ORDENAR (|L|DEPURAR (|L|CDRSL st_lst)))
				      st_ce (|P|ELP 0.0 st_lst) st_lsts (cons (append (list (list st_pr st_ce "")) st_lst) st_lsts)
				)
			)
		  )
		)
	)
	(|ST|DEPURAR (|L|ORDENAR (|L|DEPURAR st_lsts)) nil)
)

(defun |ST|OBTSTS (st_lpr st_alin st_san)
	(if st_lpr
		(|ST|OBTSTRS st_lpr st_alin st_san)
		(|ST|BUSCST st_alin st_san)
	)
)
;listar una sección transversal para guardarla en un archivo
;"|ST|LISTST"
(defun |ST|LISTST (st_lst)
	(defun |ST|PRPTS (st_lrpt)
		(cond
			((null st_lrpt) nil)
			((= (|N|ROUND (caar st_lrpt) 2) 0.0)
				(|ST|PRPTS (cdr st_lrpt))
			)
			(T
				(cons (list nil nil (|N|ROUND (cadar st_lrpt) 3) (|N|ROUND (caar st_lrpt) 2) (if (minusp (caar st_lrpt)) "I" "D")) (|ST|PRPTS (cdr st_lrpt)))
			)
		)
	)
	(cond
		((null st_lst) nil)
		(T
			(setq st_slst (car st_lst) st_pr (|N|ROUND (caar st_slst) 2) st_ps (last (car st_slst))
			      st_dst (cdr st_slst) st_de (assoc 0.0 st_dst) st_dst (|L|BORRA st_de st_dst)
			)
			(append (cons (list st_pr st_ps (|N|ROUND (cadr st_de) 3) (|N|ROUND (car st_de) 2) "E") (|ST|PRPTS st_dst)) (|ST|LISTST (cdr st_lst)))
		)
	)
)

;leer las secciones transversales de un archivo
;"|ST|CARGA"
(defun |ST|CARGA (st_nomal)
	(if (setq st_listr (cadr (|F|LEECSVA g_nomal "secciones transversales" 1)))
	  (progn
		(setq st_ltrc '() st_ltr '()
		      st_tr (car st_listr) st_listr (cdr st_listr)
		      st_dpr (list (car st_tr) (caddr st_tr) (|T|DTOST (cadr st_tr) "")) st_ltr (cons (list (cadddr st_tr) (caddr st_tr)) st_ltr)
		)
		(foreach st_tr st_listr
			(if (car st_tr)
				(setq st_ltrc (cons (cons st_dpr (reverse st_ltr)) st_ltrc) st_dpr (list (car st_tr) (caddr st_tr) (|T|DTOST (cadr st_tr) "")) st_ltr nil)
			)
			(setq st_ltr (cons (list (cadddr st_tr) (caddr st_tr)) st_ltr))
		)
		(reverse (cons (cons st_dpr (reverse st_ltr)) st_ltrc))
	  )
		nil
	)
)		

;ALAM: dibujar los alambrados en las secciones transversales
;"|ST|ALAM"
(defun |ST|ALAM (st_lini st_lst st_san st_ag st_alin)
	(setq st_lisalp (|S|PROCEL (|L|SSL (ssget "X" '((-4 . "<OR") (8 . "EP_Alambrados") (8 . "EP_Tejido") (-4 . "OR>")))))
	      st_lisale (|S|PROCEL (|L|SSL (ssget "X" '((-4 . "<OR") (8 . "R_Alambrados") (8 . "R_Tejido") (-4 . "OR>")))))
	      st_lap (strcat st_lini "_Alamb_Proy") st_lae (strcat st_lini "_Alamb_Exist") st_lte (strcat st_lini "_Terreno")
	)
	(foreach st_dst st_lst
		(setq st_pr (car st_dst) st_pist (cadr st_dst) st_pcst (caddr st_dst) st_enst (handent (last st_dst))
		      st_dpe (|A|PINS st_pr st_alin) st_pe (car st_dpe) st_dir (cadr st_dpe)
		      st_pi (|K|POLAR st_pe (+ st_dir (/ PI 2)) 70.0) st_pd (|K|POLAR st_pe (+ st_dir (* PI 1.5)) 70.0)
		      st_terr (|A|LVPOLI st_enst) st_ft nil st_pali nil st_pald nil
		      st_dali (car (|S|BAR st_lisalp (list st_pe st_pi)))
		      st_dald (car (|S|BAR st_lisalp (list st_pe st_pd)))
		)
		(if (and st_dali (< st_dali st_san))
		  (progn
		  	(setq st_pali (list (- (car st_pist) st_dali) (|P|ELP (- (car st_pist) st_dali) st_terr)))
		  	(|B|INSERT "STAL" st_pali st_lap 0 (list -1.0 0.5 1.0) nil)
		  )
		  (progn
			(setq st_dali (car (|S|BAR st_lisale (list st_pe st_pi))))
			(if (and st_dali (< st_dali st_san))
			  (progn
				(setq st_pali (list (- (car st_pist) st_dali) (|P|ELP (- (car st_pist) st_dali) st_terr)))
			  	(|B|INSERT "STAL" (list (- (car st_pist) st_dali) (|P|ELP (- (car st_pist) st_dali) st_terr)) st_lae 0 (list -1.0 0.5 1.0) nil)
			  )
			)
		  )
		)
;;;		(if (and st_pali (< (car st_pali) (caar st_terr)))
;;;			(setq st_terr (cons st_pali st_terr) st_ft T)	
;;;		)
		(if (and st_dald (< st_dald st_san))
		  (progn
		  	(setq st_pald (list (+ (car st_pist) st_dald) (|P|ELP (+ (car st_pist) st_dald) st_terr)))
		  	(|B|INSERT "STAL" (list (+ (car st_pist) st_dald) (|P|ELP (+ (car st_pist) st_dald) st_terr)) st_lap 0 (list 1.0 0.5 1.0) nil)
		  )
		  (progn
			(setq st_dald (car (|S|BAR st_lisale (list st_pe st_pd))))
			(if (and st_dald (< st_dald st_san))
			  (progn
				(setq st_pald (list (+ (car st_pist) st_dald) (|P|ELP (+ (car st_pist) st_dald) st_terr)))
				(|B|INSERT "STAL" (list (+ (car st_pist) st_dald) (|P|ELP (+ (car st_pist) st_dald) st_terr)) st_lae 0 (list 1.0 0.5 1.0) nil)
			  )
			)
		  )
		)
;;;		(if (and st_pald (> (car st_pald) (car (last st_terr))))
;;;			(setq st_terr (append st_terr (list  st_pald)) st_ft T)	
;;;		)
;;;		(if st_ft (progn (entdel st_nterr) (|O|LWPOLY st_terr st_lte 0)))
	)
)

(defun |ST|ENST (st_lst st_san st_ag st_pt)
	(cond
		((null st_lst) nil)
		(T
			(setq st_pist (cadar st_lst) st_pii (|K|Y- (|K|X- st_pist st_san) 2.0) st_pid (|K|Y- (|K|X+ st_pist st_san) 2.0))
			(if (|S|DENTRO st_pt (list st_pii st_pid (|K|Y+ st_pid (+ st_ag 4.0)) (|K|Y+ st_pii (+ st_ag 4.0))))
				(car st_lst)
				(|ST|ENST (cdr st_lst) st_san st_ag st_pt)
			)
		)
	)
)