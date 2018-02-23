;Encontrar las intersecciones de dos segmentos
;"|I|IS"
(defun |I|SS (i_s1 i_s2)
	(cond
		((member (car i_s1) i_s2) (list (car i_s1)))
		((member (cadr i_s1) i_s2) (list (cadr i_s1)))
		((eq (last i_s1) "A")
			(cond 
				((eq (last i_s2) "A")
					(|GC|INTAA i_s1 i_s2)
				)
				((eq (last i_s2) "E")
					(|I|SA i_s1 (|A|LISSEG (|E|REPPE (car i_s2) (|E|RC (cadddr i_s2) (abs (nth 4 i_s2))) (cadddr i_s2) (nth 5 i_s2) (|N|SIGNO (nth 4 i_s2)) 100)))
				)
				((eq (last i_s2) "S")
					(|I|SA i_s1 (|A|LISSEG (|E|REPPE (cadr i_s2) (|E|RC (cadddr i_s2) (abs (nth 4 i_s2))) (cadddr i_s2) (nth 5 i_s2) (|N|SIGNO (nth 4 i_s2)) 100)))
				)
				(T (|GC|INTSA i_s2 i_s1))
			)
		)
		((eq (last i_s1) "E")
			(|I|SA i_s2 (|A|LISSEG (|E|REPPE (car i_s1) (|E|RC (cadddr i_s1) (abs (nth 4 i_s1))) (cadddr i_s1) (nth 5 i_s1) (|N|SIGNO (nth 4 i_s1)) 100)))
		)
		((eq (last i_s1) "S")
			(|I|SA i_s2 (|A|LISSEG (|E|REPPE (cadr i_s1) (|E|RC (cadddr i_s1) (abs (nth 4 i_s1))) (cadddr i_s1) (nth 5 i_s1) (|N|SIGNO (nth 4 i_s1)) 100)))
		)
  		(T
			(cond	      
				((eq (last i_s2) "A")
					(|GC|INTSA i_s1 i_s2)
				)
				((eq (last i_s2) "E")
					(|I|SA i_s1 (|A|LISSEG (|E|REPPE (car i_s2) (|E|RC (cadddr i_s2) (abs (nth 4 i_s2))) (cadddr i_s2) (nth 5 i_s2) (|N|SIGNO (nth 4 i_s2)) 100)))
				)
				((eq (last i_s2) "S")
					(|I|SA i_s1 (|A|LISSEG (|E|REPPE (cadr i_s2) (|E|RC (cadddr i_s2) (abs (nth 4 i_s2))) (cadddr i_s2) (nth 5 i_s2) (|N|SIGNO (nth 4 i_s2)) 100)))
				)
				(T (list (inters (|K|XY (car i_s1)) (|K|XY (cadr i_s1)) (|K|XY (car i_s2)) (|K|XY (cadr i_s2)))))
			)
		)
	)
)

;Encontrar las intersecciones de un segmento con un alineamiento
;"|I|SA"
(defun |I|SA (i_seg i_lali)
	(cond
		((null i_lali) nil)
		((setq i_li (|I|SS i_seg (car i_lali)))
			(append i_li (|I|SA i_seg (cdr i_lali)))
	 	)
		(T
			(|I|SA i_seg (cdr i_lali))
		)
	)
)

;"|I|INTSA"
(defun |I|INTSA (i_seg i_lali)
	(|L|PURGAR (|I|SA i_seg i_lali))
)

;Encontrar las intersecciones de dos alineamientos
;"|I|I2A"
(defun |I|I2A (i_lali1 i_lali2)
	(cond
		((null i_lali1) nil)
		((setq i_li (|I|SA (car i_lali1) i_lali2))
			(append i_li (|I|I2A (cdr i_lali1) i_lali2))
		)
		(T
			(|I|I2A (cdr i_lali1) i_lali2)
		)
	)
)

;"|I|INT2A"
(defun |I|INT2A (i_lali1 i_lali2)
	(|L|DEPURAR (|I|I2A i_lali1 i_lali2))
)

; Encontrar la intersección de un segmento de recta con un alineamiento
;"|I|INTSRA"
(defun |I|INTSRA (i_sr i_al i_forzar / i_pint)
	(setq i_pint (|I|INTSA i_segr i_lali))
	(cond
		(i_pint (car i_pint))
		(i_forzar
			(if (< (|K|DISTH (car i_sr) (caar i_lali)) (|K|DISTH (car i_sr) (cadr (last i_lali)))) (caar i_lali) (cadr (last i_lali)))
		)
		(T
			nil
		)
	)
)
