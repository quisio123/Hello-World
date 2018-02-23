;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;										M	A	T	R	I	C	E	S
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;						C	R	E	A	R		U	N	A		M	A	T	R	I	Z
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;Crear una matriz llenando todas sus filas y columnas con un valor (m_val) específico
;"|M|CREAFIL"
(defun |M|CREAFIL (m_val m_nel)
	(cond ((null m_val) nil)
		((= m_nel 0) nil)
		(T
			(cons m_val (|M|CREAFIL m_val (1- m_nel)))
		)
	)
)

;"|M|CREAMAT"
(defun |M|CREAMAT (m_val m_fil m_col)
	(cond ((null m_val) nil)
		((= m_fil 0) nil)
		(T
			(cons (|M|CREAFIL m_val m_col) (|M|CREAMAT m_val (1- m_fil) m_col))
		)
	)
)

;crear una matriz identidad
;"|M|MATID"
(defun |M|MATID (m_ord)
	;poner unos en la diagonal de una matriz
	(defun |M|PON1 (m_mat m_p1 m_ord)
		(cond ((= m_p1 m_ord) nil)
			(T
				(cons (|L|REEMINDEX m_p1 1.0 (car m_mat)) (|M|PON1 (cdr m_mat) (1+ m_p1) m_ord))
			)
		)
	)

	(|M|PON1 (|M|CREAMAT 0.0 m_ord m_ord) 0 m_ord)
)

;			T	R	A	N	S	P	U	E	S	T	A		D	E		U	N	A		M	A	T	R	I	Z
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;transpuesta de una matriz
;"|M|TRANSP"
(defun |M|TRANSP (m_m)
	(apply 'mapcar (cons 'list m_m))
)

;	E	L	I	M	I	N	A	R		F	I	L	A	S		Y		C	O	L	U	M	N	A	S		N	U	L	A	S	
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;verificar si todos los elementos de una fila son cero
;"|M|FNULA"
(defun |M|FNULA (m_fil)
	(apply 'and (mapcar 'zerop m_fil))
)

;eliminar las filas nulas de una matriz
;"|M|FNULAS"
(defun |M|FNULAS (m_mat)
	(cond ((null m_mat) nil)
		((|M|FNULA (car m_mat))
			(|M|FNULAS (cdr m_mat))
		)
		(T
			(cons (car m_mat) (|M|FNULAS (cdr m_mat)))
		)
	)
)

;"|M|FCNULAS"
(defun |M|FCNULAS (m_mat)
	(|M|TRANSP (|M|FNULAS (|M|TRANSP (|M|FNULAS m_mat))))
)

;			O	R	D	E	N		D	E		U	N	A		M	A	T	R	I	Z
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;orden de una matriz
;"|M|ORDEN"
(defun |M|ORDEN (m_mat)
	(if (listp m_mat)
		(list (length m_mat) (if (listp (car m_mat)) (length (car m_mat)) 1))
		nil
	)
)

;			S	U	M	A		D	E		M	A	T	R	I	C	E	S
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;suma de filas
;"|M|SUMAF"
(defun |M|SUMAF (m_f1 m_f2)
	(cond ((= (length m_f1) (length m_f2))
			(mapcar '+ m_f1 m_f2)
		)
		(T
			nil
		)
	)
)

;suma de matrices
;"|M|SUMA"
(defun |M|SUMA (m_m1 m_m2)
	(cond ((equal (|M|ORDEN m_m1) (|M|ORDEN m_m2))
			(mapcar '(lambda (m_m1f m_m2f) (mapcar '|M|SUMAF m_m1f m_m2f)) m_m1 m_m2)
		)
		(T
			nil
		)
	)
)

;			M	U	L	T	I	P	L	I	C	A	C	I	O	N		D	E		M	A	T	R	I	C	E	S
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Aplicar una matriz de transformación a un vector
;"|M|MTOV"
(defun |M|MTOV (m_m m_v)
	(mapcar '(lambda (m_e) (apply '+ (mapcar '* m_e m_v))) m_m)
)

;multiplicar dos matrices
;"|M|M*M"
(defun |M|M*M (m_m1 m_m2)
	(mapcar '(lambda (m_e) (|M|MTOV (|M|TRANSP m_m2) m_e)) m_m1)
)

;producto punto
;"|M|PPTO"
(defun |M|PPTO (m_m1 m_m2)
	(apply '+ (mapcar '* m_m1 m_m2))
)

;multiplicar una matriz por un vector
;"|M|M*V"
(defun |M|M*V (m_mat m_vec)
	(mapcar '(lambda (m_fil) (|M|PPTO m_fil m_vec)) m_mat)
)

;			D	E	T	E	R	M	I	N	A	N	T	E		D	E		U	N	A		M	A	T	R	I	Z
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;obtener los elementos de la diagonal de una matriz
;"|M|DIAG"
(defun |M|DIAG (m_mat)
	(cond ((null m_mat) nil)
		(T
			(cons (caar m_mat) (|M|DIAG (|L|CDRSL (cdr m_mat))))
		)
	)
)

;multiplicar todos los elementos de la diagonal de una matriz
;"|M|MDIAG"
(defun |M|MDIAG (m_mat)
	(apply '* (|M|DIAG m_mat))
)

;mover todos los primeros elementos de cada fila al final de la misma
;"|M|MOV1F"
(defun |M|MOV1F (m_mat / m_nmat)
	(mapcar '(lambda (m_el) (append (cdr m_el) (list (car m_el)))) m_mat)
)

;invertir todos los elementos de cada fila de una matriz
;"|M|INVF"
(defun |M|INVF (m_mat / m_nmat)
	(mapcar 'reverse m_mat)
)

;obtener todas las diagonales
;"|M|TDIAG"
(defun |M|TDIAG (m_mat m_ord)
	(cond ((null m_mat) nil)
		((= m_ord 0) nil)
		(T (cons (|M|DIAG m_mat) (|M|TDIAG (|M|MOV1F m_mat) (1- m_ord))))
	)
)

;calcular el determinante de una matriz
;"|M|DET"
(defun |M|DET (m_mat)
  	(setq m_ord (|M|ORDEN m_mat) m_nf (car m_ord) m_nc (cadr m_ord))
	(cond ((null m_mat) nil)
		(T
			(- (apply '+ (|M|TDIAG m_mat m_ord)) (apply '+ (|M|TDIAG (|M|INVF m_mat) m_ord)))
		)
	)
)

;			R	E	D	U	C	C	I	O	N		D	E		U	N	A		M	A	T	R	I	Z
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;intercambiar filas para que no haya ceros en la diagonal ppal
;"|M|IFIL"
(defun |M|IFIL (m_mat m_mres m_nit m_mor m_mreso)
	(cond ((null m_mat) (list m_mor m_mreso))
		((= (nth m_nit (car m_mat)) 0)
			(|M|IFIL (append (cdr m_mat) (list (car m_mat))) (append (cdr m_mres) (list (car m_mres))) m_nit m_mor m_mreso)
		)
		(T
			(|M|IFIL (cdr m_mat) (cdr m_mres) (1+ m_nit) (append m_mor (list (car m_mat))) (append m_mreso (list (car m_mres))))
		) 
	)
)

;llevar a 1 los elementos de la diagonal ppal
;"|M|R1DP"
(defun |M|R1DP (m_mat m_mid m_nit m_mr m_mir)
	(cond ((null m_mat) (list m_mr m_mir))
		(T
			(|M|R1DP (cdr m_mat) (cdr m_mid) (1+ m_nit)
				(append m_mr (list (|L|DIVC (car m_mat) (nth m_nit (car m_mat)))))
				(append m_mir (list (|L|DIVC (car m_mid) (nth m_nit (car m_mat)))))
			)
		) 
	)
)


;llevar a 0 los demás elementos
;"|M|RMAT"
(defun |M|RMAT (m_mat m_mid)
	(defun |M|RAB ()
		(setq m_i 0)
		(while (setq m_fo (nth m_i m_mat))
			(setq m_j (1+ m_i) m_foi (nth m_i m_mid))
			(while (setq m_f (nth m_j m_mat))
				(setq	m_el (nth m_i m_f) m_fi (nth m_j m_mid)
					m_fs (|L|MULTC m_fo (- m_el)) m_fis (|L|MULTC m_foi (- m_el))
					m_fr (|M|SFIL m_f m_fs) m_fir (|M|SFIL m_fi m_fis)
					m_el (nth m_j m_fr)
					m_fr (|L|DIVC m_fr m_el) m_fir (|L|DIVC m_fir m_el)
					m_mat (|L|REEMPOS m_j m_fr m_mat)
					m_mid (|L|REEMPOS m_j m_fir m_mid)
					m_j (1+ m_j)
				)
			)
			(setq m_i (1+ m_i))
		)
	)
	(defun |M|RAR ()
		(setq m_i (- (length m_mat) 1))
		(while (>= m_i 0)
			(setq m_fo (nth m_i m_mat) m_j (- m_i 1) m_foi (nth m_i m_mid))
			(while (>= m_j 0)
				(setq m_f (nth m_j m_mat) m_fi (nth m_j m_mid)
				      m_el (nth m_i m_f)
					m_fs (|L|MULTC m_fo (- m_el)) m_fis (|L|MULTC m_foi (- m_el))
					m_fr (|M|SFIL m_f m_fs) m_fir (|M|SFIL m_fi m_fis)
					m_el (nth m_j m_fr)
					m_fr (|L|DIVC m_fr m_el) m_fir (|L|DIVC m_fir m_el)
					m_mat (|L|REEMPOS m_j m_fr m_mat)
					m_mid (|L|REEMPOS m_j m_fir m_mid)
					m_j (1- m_j)
				)
			)
			(setq m_i (1- m_i))
		)
	)

	(|M|RAB)
	(|M|RAR)
	m_mid
)

;comprobar si una matriz es cuadrada
;"|M|CUAD"
(defun |M|CUAD (m_mat / m_ord)
	(setq m_ord (|M|ORDEN m_mat))
	(if (= (car m_ord) (cadr m_ord))
		T
		nil
	)
)

;Reducir una matriz (Método de Gauss-Jordan)
;"|M|G-J"
(defun |M|G-J (m_mat m_m1)
	(cond ((null m_mat) nil)
		(T
			(setq m_ord (|M|ORDEN m_mat) m_nf (car m_ord) m_nc (cadr m_ord))
			(cond ((eq m_nf m_nc)
					(setq m_lmat (|M|IFIL m_mat m_m1 0 nil nil) m_mat (car m_lmat) m_m1 (cadr m_lmat)	;;;intercambiar filas para que no haya ceros en la diagonal ppal
					      m_lmat (|M|R1DP m_mat m_m1 0 nil nil) m_mat (car m_lmat) m_m1 (cadr m_lmat)	;;;llevar a 1 los elementos de la diagonal ppal
					)
					(|M|RMAT m_mat m_m1)
			      )
			      (T nil)
			)
		)
	)
)

;Devolver la inversa de una matriz
;"|M|INV"
(defun |M|INV (m_mat)
	(setq m_mat (|M|FCNULAS m_mat))
	(cond ((null m_mat) nil)
		((|M|CUAD m_mat)
			(|M|G-J m_mat (|M|MATID (car (|M|ORDEN m_mat))))
		)
		(T nil)
	)
)

;Reducción de matrices
;"|M|REDUCIR"
(defun |M|REDUCIR (m_mat m_ti)
	(setq m_mat (|M|FCNULAS m_mat))
	(cond ((null m_mat) nil)
		((= (length m_mat) (length m_ti))
			(|M|G-J m_mat m_ti)
		)
		(T nil)
	)
)


;transformación del punto de inserción y el ángulo entre sistemas de coordenadas
;crear una matriz para rotación alrededor del eje Z
;"|M|MROT"
(defun |M|MROT (m_ang)
	(list (list (cos m_ang) (- (sin m_ang)) 0.0) (list (sin m_ang) (cos m_ang) 0.0) (list 0.0 0.0 1.0))
)



