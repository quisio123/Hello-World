
;Calcular la línea de destape
;"|Z|CLD"
(defun |Z|CLD (z_lppt z_lptn z_prof / z_ldestv z_ldest)
;;;	(defun |Z|BINT (z_lsd)
;;;	)
	(setq z_ldestv (|L|ELIMDUP (mapcar '(lambda (el) (|K|Y- el z_prof)) z_lptn)) z_lsd (|A|LISSEG z_ldestv)
	      z_ipt (car z_lppt) z_fpt (last z_lppt) z_ldi (cons z_ipt nil) z_ldd (cons z_fpt nil)
	      z_lppt (|L|BULT (cdr z_lppt)) z_pt (car z_lppt) z_ctn (|P|ELP (car z_pt) z_lptn) z_ctd (|P|ELP (car z_pt) z_ldestv)
	)
	(while z_pt
		(if (< (cadr z_pt) z_ctn)
			(if (> (cadr z_pt) z_ctd)
				(setq z_ldi (cons z_pt z_ldi) z_ipt z_pt z_lppt (cdr z_lppt) z_pt (car z_lppt))
				(setq z_ldi (cons (car (|I|INTSA (list z_ipt z_pt) z_lsd)) z_ldi) z_pt nil)
			)
			(setq z_ldi (cons (list (+ (car z_ipt) 0.001) (|P|ELP (car z_ipt) z_ldestv)) z_ldi) z_pt nil)
		)
	)
	(setq z_ldi (reverse z_ldi) z_lppt (reverse z_lppt) z_pt (car z_lppt) z_ctn (|P|ELP (car z_pt) z_lptn) z_ctd (|P|ELP (car z_pt) z_ldestv))
	(while z_pt
		(if  (< (cadr z_pt) z_ctn)
			(if (> (cadr z_pt) z_ctd)
				(setq z_ldd (cons z_pt z_ldd) z_fpt z_pt z_lppt (cdr z_lppt) z_pt (car z_lppt))
				(setq z_ldd (cons (car (|I|INTSA (list z_fpt z_pt) z_lsd)) z_ldd) z_pt nil)
			)
			(setq z_ldd (cons (list (- (car z_fpt) 0.001) (|P|ELP (car z_fpt) z_ldestv)) z_ldd) z_pt nil)
		)
	)
	(append z_ldi (|Z|LPINTX z_ldestv (last z_ldi) (car z_ldd)) z_ldd)
)

;procesar líneas verticales
;"|Z|PLV"
(defun |Z|PLV (z_lpol z_pa)
	(cond
		((null z_lpol) nil)
		((null z_pa) (cons (car z_lpol) (|Z|PLV (cdr z_lpol) (car z_lpol))))
		((= (caar z_lpol) (car z_pa))
			(setq z_pt (|K|X+ (car z_lpol) 0.001))
			(cons z_pt (|Z|PLV (cdr z_lpol) z_pt))
		)
		(T (cons (car z_lpol) (|Z|PLV (cdr z_lpol) (car z_lpol))))
	)
)

; Dadas dos polilineas encontrar las areas entre las mismas
; esta rutina devuelve dos valores de areas en donde el primer valor queda determinado por los puntos
; de la primera polilinea que quedan por debajo de la segunda polilinea y el segundo valor corresponde
; a los puntos de la primera polilinea que quedan por encima de la segunda.
;"|Z|AREAS"
(defun |Z|AREAS (z_poli1 z_poli2)
	(defun |Z|SAREA (z_lpi z_area+ z_area- z_ppi)
		(cond
			((null z_lpi) (list z_area+ z_area-))
			((null z_ppi) (|Z|SAREA (cdr z_lpi) z_area+ z_area- (car z_lpi)))
			(T
		      		(setq z_spi (car z_lpi)
				      z_lpi1 (|A|LPINT z_poli1 z_ppi z_spi) z_lpi2 (|A|LPINT z_poli2 z_ppi z_spi)
				      z_pm (|K|PMED z_ppi z_spi) z_itn (car (|I|INTSA (list (|K|Y- z_pm 100.0) (|K|Y+ z_pm 100.0)) z_poli2))
				      z_ipt (car (|I|INTSA (list (|K|Y- z_pm 100.0) (|K|Y+ z_pm 100.0)) z_poli1))
				)
				(if (or z_lpi1 z_lpi2)
					(if (> (cadr z_ipt) (cadr z_itn))
						(|Z|SAREA (cdr z_lpi) (append z_area+ (list (append (list z_ppi) z_lpi2 (list z_spi) (reverse z_lpi1)))) z_area- z_spi)
						(|Z|SAREA (cdr z_lpi) z_area+ (append z_area- (list (append (list z_ppi) z_lpi2 (list z_spi) (reverse z_lpi1)))) z_spi)
					)
					(|Z|SAREA (cdr z_lpi) z_area+ z_area- z_spi)
				)
			)
		)
	) ;(|o|line z_ppi z_spi "O")
	(|Z|SAREA (|L|ORDENAR (|L|ELIMDUP (|I|INT2A z_poli1 z_poli2))) nil nil nil)
)

;Obtener las areas de destape desmonte y terraplén de una sección transversal
(defun |Z|GETAREAS (z_lpt z_ltn z_pdest / z_ipt z_fpt z_itn z_ftn z_ldest z_aread z_adest z_adesm z_aterr)
	(setq z_ipt (caar z_lpt) z_fpt (cadr (last z_lpt))
	      z_itn (caar z_ltn) z_ftn (cadr (last z_ltn))
	)
 	(if (> (car z_ipt) (car z_fpt)) (setq z_lpt (|A|CDIRALI z_lpt)))
 	(if (> (car z_itn) (car z_ftn)) (setq z_ltn (|A|CDIRALI z_ltn)))
	(setq z_areas (|Z|AREAS z_lpt z_ltn) z_adest 0.0 z_adesm 0.0 z_aterr 0.0)
;;; 	(|O|LWPOLY z_ldest "z_Destape" 0)
;;;	(foreach z_poly z_aread
;;;		(setq z_adest (+ z_adest (|GC|GAUSS z_poly)))
;;;		(|O|LWPOLY z_poly "C_Destape" 1)
;;;	)
	(foreach z_poly (car z_areas)
		(setq z_aterr (+ z_aterr (|GC|GAUSS z_poly)))
		(|O|LWPOLY z_poly "C_Terraplen" 1)
	)
	(foreach z_poly (cadr z_areas)
		(setq z_adesm (+ z_adesm (|GC|GAUSS z_poly)))
		(|O|LWPOLY z_poly "C_Desmonte" 1)
	)
;;;	(list (/ z_adest 2) (/ z_adesm 2) (/ z_aterr 2))
	(list z_adesm z_aterr)
)

;Calcular las areas de destape desmonte y terraplén de una sección transversal
(defun |Z|CAREAS (z_lnent z_area / z_ar1)
	(cond
		((null z_lnent) (/ z_area 2))
		(T
			(setq z_ar1 (|GC|GAUSS (|A|LVPOLI (car z_lnent))))
			(|Z|CAREAS (cdr z_lnent) (+ z_area z_ar1))
		)
	)
)

