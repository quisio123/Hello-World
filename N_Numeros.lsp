;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;									N	U	M	E	R	O	S							   
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;obtener un entero
;"|N|GETINT"
(defun |N|GETINT (n_pre n_msg)
	(if n_pre
		(setq n_msg (strcat n_msg " <" (itoa n_pre) "> :"))
	)
	(if (setq n_num (getint n_msg))
		n_num
		n_pre
	)
)

;obtener un número real
;"|N|GETREAL"
(defun |N|GETREAL (n_pre n_msg)
	(if n_pre
		(setq n_msg (strcat n_msg " <" (rtos n_pre 2 2) "> :"))
	)
	(if (setq n_num (getreal n_msg))
		n_num
		n_pre
	)
)

;convertir un número entero en real
;"|N|IAR"
(defun |N|IAR (n_num)
	(cond
		((null n_num) nil)
		(T (* n_num 1.0))
	)
)

;incrementar un número en una unidad
;"|N|INC1"
(defun |N|INC1 (n_num)
	(cond
		((null n_num) nil)
		((numberp n_num) (1+ n_num))
	)
)

; determinar el próximo número que corresponde a un número dado según el paso (ej.: n_num 1235.80 n_paso 10.0 devuelve 1240.0)
;"|N|SIGPAS"
(defun |N|SIGPAS (n_num n_paso)
	(cond
		((null n_num) nil)
		((= (rem n_num n_paso) 0) n_num)
		(T
			(+ (- n_num (rem n_num n_paso)) n_paso)
		)
	)
)

;determinar si un número entero es par o impar
;"|N|ESPAR"
(defun |N|ESPAR (n_int)
	(cond
		((eq (type n_int) 'INT)
			(= (rem n_int 2) 0)
		)
		(T nil)
	)
)

;verificar si un número esta dentro de un rango
;"|N|ENTRE"
(defun |N|ENTRE (n_num n_rango)
	(cond
		((> (car n_rango) (cadr n_rango))
			(|N|ENTRE n_num (reverse n_rango))
		)
		(T
			(and (>= n_num (car n_rango)) (<= n_num (cadr n_rango)))
		)
	)
)

;verificar si un número esta dentro de un rango excluidos los extremos
;"|N|ENTREE"
(defun |N|ENTREE (n_num n_rango)
	(cond
		((> (car n_rango) (cadr n_rango))
			(|N|ENTREE n_num (reverse n_rango))
		)
		(T
			(and (> n_num (car n_rango)) (< n_num (cadr n_rango)))
		)
	)
)

;redondear un número
;"|N|ROUND"
(defun |N|ROUND (n_num n_dec)
	(cond
		((not (numberp n_num)) nil)
		((= n_dec 0)
			(cond
				((= (fix n_num) 0)
					(if (> n_num 0.5) 1 0)
				)
				((> (rem n_num (fix n_num)) 0.5) (+ (fix n_num) 1))
				(T (fix n_num))
			)
		)
		(T (/ (|N|ROUND (* n_num 10.0) (- n_dec 1)) 10.0))
	)
)

;Factorial
;"|N|FACT"
(defun |N|FACT (n_num)
	(cond ((= 1 n_num) n_num)
		(T (* n_num 1.0 (|N|FACT (1- n_num))))
	)
)

;devolver el signo de un número
;"|N|SIGNO"
(defun |N|SIGNO (n_num)
	(if (minusp n_num) - +)
)

;devolver el signo contrario de un número
;"|N|-SIGNO"
(defun |N|-SIGNO (n_num)
	(if (minusp n_num) + -)
)
;"|N|TSIGNO"
(defun |N|TSIGNO (n_num)
	(cond
		((or (null n_num) (zerop n_num)) "")
		((minusp n_num) "-")
		(T "+")
	)
)

; Devolver el número de lámina al que corresponde una progresiva
;"|N|NUMLAM"
(defun |N|NUMLAM (n_pr n_ll)
	(cond
		((<= n_pr n_ll) 1)
		(T (1+ (|N|NUMLAM (- n_pr n_ll) n_ll))) 
	)
)

;"|N|DLADO"
(defun |N|DLADO (n_lado n_d1)
	(if (eq n_lado "D") n_d1 (- n_d1))
)

;"|N|SEQNUM"
(defun |N|SEQNUM (n_ini n_inc n_fin)
	(cond
		((> n_ini n_fin) nil)
		(T (cons (list n_ini "") (|N|SEQNUM (+ n_ini n_inc) n_inc n_fin)))
	)
)


(setq n_rom (list (cons 0 "") (cons 1 "I") (cons 5 "V") (cons 10 "X") (cons 50 "L") (cons 100 "C") (cons 500 "D") (cons 1000 "M")))

;"|N|ROM1"
(defun |N|ROM1 (n_num n_ref)
	(cond
		((= n_num 0) "")
		((= n_num 9) (strcat (|O|ITEM n_ref n_rom) (|O|ITEM (* 10 n_ref) n_rom)))
		((> n_num 4) (strcat (|O|ITEM (* 5 n_ref) n_rom) (|N|ROM1 (- n_num 5) n_ref)))
		((= n_num 4) (strcat (|O|ITEM n_ref n_rom) (|O|ITEM (* 5 n_ref) n_rom)))
		(T
			(strcat (|O|ITEM n_ref n_rom) (|N|ROM1 (1- n_num) n_ref))
		)
	)
)

;"|N|ROMANO"
(defun |N|ROMANO (n_num) 
	(cond
		((= n_num 0) "")
		((>= n_num 4000) nil)
		(T
			(setq n_miles (fix (/ n_num 1000)) n_cientos (fix (/ (- n_num (* n_miles 1000)) 100))
			      n_diezmos (fix (/ (- n_num (* n_miles 1000) (* n_cientos 100)) 10))
			      n_unid (- n_num (* n_miles 1000) (* n_cientos 100) (* n_diezmos 10))
			      n_mt (|N|ROM1 n_miles 1000) n_ct (|N|ROM1 n_cientos 100) n_dt (|N|ROM1 n_diezmos 10)
			      n_ut (|N|ROM1 n_unid 1)
			)
			(strcat n_mt n_ct n_dt n_ut) 
		)
	)
)

;"|N|ROMAINT"
(defun |N|ROMAINT (n_nr n_sum)
	(setq n_m 1000 n_5c 500 n_c 100 n_5d 50 n_d 10 n_5u 5)
	(cond
		((eq n_nr "") n_sum)
		((eq (substr n_nr 1 1) "M") (|N|ROMAINT (substr n_nr 2) (+ n_sum 1000)))
		((eq (substr n_nr 1 2) "CM") (|N|ROMAINT (substr n_nr 3) (+ n_sum 900)))
		((eq (substr n_nr 1 1) "D") (|N|ROMAINT (substr n_nr 2) (+ n_sum 500)))
		((eq (substr n_nr 1 2) "CD") (|N|ROMAINT (substr n_nr 3) (+ n_sum 400)))
		((eq (substr n_nr 1 1) "C") (|N|ROMAINT (substr n_nr 2) (+ n_sum 100)))
		((eq (substr n_nr 1 2) "XC") (|N|ROMAINT (substr n_nr 3) (+ n_sum 90)))
		((eq (substr n_nr 1 1) "L") (|N|ROMAINT (substr n_nr 2) (+ n_sum 50)))
		((eq (substr n_nr 1 2) "XL") (|N|ROMAINT (substr n_nr 3) (+ n_sum 40)))
		((eq (substr n_nr 1 1) "X") (|N|ROMAINT (substr n_nr 2) (+ n_sum 10)))
		((eq (substr n_nr 1 2) "IX") (|N|ROMAINT (substr n_nr 3) (+ n_sum 9)))
		((eq (substr n_nr 1 1) "V") (|N|ROMAINT (substr n_nr 2) (+ n_sum 5)))
		((eq (substr n_nr 1 2) "IV") (|N|ROMAINT (substr n_nr 3) (+ n_sum 4)))
		((eq (substr n_nr 1 1) "I") (|N|ROMAINT (substr n_nr 2) (+ n_sum 1)))
	)
)

;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;									A	N	G	U	L	O	S							   
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Convertir un ángulo en grados a radianes
;"|N|GAR"
(defun |N|GAR (n_ang)
	(* pi (/ n_ang 180.0))
)

;Convertir un ángulo en radianes a grados
;"|N|RAG"
(defun |N|RAG (n_ang)
	(/ (* n_ang 180.0) pi)
)

;SENO de un ángulo dado el coseno {sen2 + cos2 = 1}
;"|N|SENO"
(defun |N|SENO (n_cos)
	(sqrt (- 1 (expt n_cos 2)))
)

;ARCOSENO
;"|N|ASEN"
(defun |N|ASEN (n_ang)
	(atan (/ n_ang (sqrt (- 1 (expt n_ang 2)))))
)

;COSENO de un ángulo dado el seno {sen2 + cos2 = 1}
;"|N|COS"
(defun |N|COS (n_sen)
	(sqrt (- 1 (expt n_sen 2)))
)

;ARCOCOSENO
;"|N|ACOS"
(defun |N|ACOS (n_ang)
	(if (or (equal n_ang -1.0 g_tol) (equal n_ang 1.0 g_tol))
		0.0
		(if (and (> n_ang -1.0) (< n_ang 1.0))
			(- 1.5707963267949 (atan (/ n_ang (sqrt (- 1.0 (expt n_ang 2)))))) ;1.5707963267949 - arctag (x / raiz (1 - x ^ 2))
			nil
		)
	)
)

;TANGENTE
;"|N|TAN"
(defun |N|TAN (n_ang)
	(/ (sin n_ang) (cos n_ang))
)

;COTANGENTE
;"|N|CTAN"
(defun |N|CTAN (n_ang)
	(/ (cos n_ang) (sin n_ang))
)

;SECANTE
;"|N|SECANTE"
(defun |N|SECANTE (n_ang)
	(/ 1 (cos n_ang))
)

;COSECANTE
;"|N|COSECANTE"
(defun |N|COSECANTE (n_ang)
	(/ 1 (sin n_ang))
)

;reducir a 2PI
;"|N|DOSPI"
(defun |N|DOSPI (n_ang)
	(cond
		((null n_ang) nil)
		((minusp n_ang) (+ (- (|N|DOSPI (abs n_ang))) (* PI 2)))
		((< n_ang (* PI 2)) n_ang)
		(T (|N|DOSPI (- n_ang (* PI 2))))
	)
)

;positivo en 2PI de un ángulo negativo
;"|N|ANGCOMP"
(defun |N|ANGCOMP (n_ang)
	(cond
		((null n_ang) nil)
		((minusp n_ang) (+ (* PI 2) n_ang))
		(T n_ang)
	)
)

;negativo de un angulo mayor a PI
;"|N|ENPI"
(defun |N|ENPI (n_ang)
	(cond
		((null n_ang) nil)
		((> n_ang PI) (- n_ang (* PI 2)))
		(T n_ang)
	)
)

;complemento a PI de un ángulo
;"|N|COMPPI"
(defun |N|COMPPI (n_ang)
	(cond
		((null n_ang) nil)
		((minusp n_ang) (|N|COMPPI (|N|ANGCOMP n_ang)))
		(T
			(if (> n_ang PI)
			  (- n_ang PI)
			  (- PI n_ang)
			)
		)
	)
)

;devolver el ángulo en sistema Autocad que corresponde a un ángulo topográfico
;"|N|TOPACAD"
(defun |N|TOPACAD (n_ang n_anga) ;a_ang alfa topográfico a_anga rumbo anterior en cad
	(cond ((null n_ang) nil)
		((null n_anga) (|N|TOPACAD n_ang (* PI 1.5)))
		(T
			(|N|DOSPI (- (+ (- (* PI 2) n_ang) n_anga) PI))
		)
	)
)

;devolver el ángulo en sistema topográfico que corresponde a un ángulo en cad
;"|N|CADATOP"
(defun |N|CADATOP (n_ang n_anga) ; a_ang rumbo en cad a_anga rumbo anterior en cad
	(cond ((null n_ang) nil)
		((null n_anga) (|N|CADATOP n_ang (* PI -0.5)))
		(T
			(|N|DOSPI (- (* pi 2) (|N|DOSPI (- pi (- n_anga n_ang)))))
		)
	)
)

;devolver el cuadrante al que corresponde un ángulo
;"|N|CUADRANTE"
(defun |N|CUADRANTE (n_ang)
	(setq n_ang (|N|DOSPI (- n_ang 0.001)))
	(1+ (fix (/ n_ang (/ PI 2))))
)

;devolver el octante al que corresponde un ángulo
;"|N|OCTANTE"
(defun |N|OCTANTE (n_ang)
	(setq n_ang (|N|DOSPI n_ang))
	(1+ (fix (/ n_ang (/ PI 4))))
)

;devolver el punto cardinal más proximo a una dirección
(defun |N|PCARD (n_dir)
	(setq n_oct (|N|OCTANTE n_dir))
	(cond
		((member n_oct '(1 8)) "E")
		((member n_oct '(2 3)) "N")
		((member n_oct '(4 5)) "O")
		((member n_oct '(6 7)) "S")
	)
)

;devolver el punto cardinal opuesto a una dirección
(defun |N|PCARO (n_dir)
	(|N|PCARD (+ n_dir PI))
)

;devolver el cuadrante al que corresponde un ángulo
;"|N|CUADRANT2"
(defun |N|CUADRANT2 (n_ang n_angb)
	(setq n_ang (- n_ang n_angb))
	(|N|CUADRANTE (|N|DOSPI n_ang))
)

;devolver una lista de dos valores que indican en que cuadrante se encuentra un ángulo respecto de los eje X e Y
; si un valor es 0 (cero) corresponde al cero de su eje si es -1 corresponde a valores negativos del eje y si es 1 a valores positivos
;"|N|SENTS"
(defun |N|SENTS (n_ang)
	(setq n_ang (|N|DOSPI n_ang))
	(cond
		((= n_ang 0) (list 1 0))
		((and (> n_ang 0) (< n_ang (/ PI 2))) (list 1 1))
		((= n_ang (/ PI 2)) (list 0 1))
		((and (> n_ang (/ PI 2)) (< n_ang PI)) (list -1 1))
		((= n_ang PI) (list -1 0))
		((and (> n_ang PI) (< n_ang (* PI 1.5))) (list -1 -1))
		((= n_ang (* PI 1.5)) (list 0 -1))
		(T (list 1 -1))
	)
)

;devolver el menor ángulo entre 3 puntos
;"|N|A3P"
(defun |N|A3P (n_v n_p1 n_p2)
	(setq n_an1 (angle n_v n_p1) n_an2 (angle n_v n_p2))
	(min (|N|DOSPI (- n_an1 n_an2)) (|N|DOSPI (- n_an2 n_an1)))
)

;devolver el ángulo entre 3 puntos
;"|N|ANGULO"
(defun |N|ANGULO (n_v n_p1 n_p2)
	(setq n_an1 (angle n_v n_p1) n_an2 (angle n_v n_p2))
	(|N|DOSPI (- n_an1 n_an2))
)

;devolver el ángulo de barrido entre 3 puntos
;"|N|ANGULO"
(defun |N|ANGBAR (n_v n_p1 n_p2)
	(setq n_an1 (angle n_v n_p1) n_an2 (angle n_v n_p2))
	(if (< n_an2 n_an1) (- (+ n_an2 (* PI 2)) n_an1) (- n_an2 n_an1))
)

;devolver el orden de dos ángulos cuyo barrido sea menor
(defun |N|MINBAR (n_v n_p1 n_p2)
	(cond
		((< (|N|ANGBAR n_v n_p1 n_p2) (|N|ANGBAR n_v n_p2 n_p1))
			(list (angle n_v n_p1) (angle n_v n_p2))
		)
		(T (list (angle n_v n_p2) (angle n_v n_p1)))
	)
)

;"|N|ANGM"
(defun |N|ANGM (n_cg n_p1 n_p2)
	(setq n_an1 (angle n_cg n_p1) n_an2 (angle n_cg n_p2) n_anm (/ (+ n_an1 n_an2) 2))
	(cond
		((> (abs (- n_an2 n_an1)) PI) (- n_anm PI))
		(T n_anm)
	)
)

;"|N|ANGC"
(defun |N|ANGC (n_a1 n_a2)
	(setq n_ac (/ (+ n_a1 n_a2) 2))
	(cond
		((> (abs (- n_a2 n_a1)) PI) (- n_ac PI))
		(T n_ac)
	)
)

;Devolver el Acimut de un ángulo
;"|N|AZIMUT"
(defun |N|AZIMUT (n_ang)
	(|N|CADATOP n_ang nil)
)

;convertir un texto que representa una fracción en un número real
;"|N|FTOR"
(defun |N|FTOR (n_frac)
	(cond
		((null n_frac) 1e30)
		(T
			(setq n_lnum (|T|SUBCH n_frac "/"))
			(/ (atof (car n_lnum)) (atof (cadr n_lnum)))
		)
	)
)

;convertir un texto que representa una pendiente en un ángulo en radianes
;"|N|FTOA"
(defun |N|FTOA (n_frac)
	(cond
		((null n_frac) (/ PI 2))
		(T
			(setq n_lnum (|T|SUBCH n_frac "/"))
			(atan (atof (car n_lnum)) (atof (cadr n_lnum)))
		)
	)
)

;calcular el coeficiente de desplazamiento en X para un desplazamiento en una pendiente determinada
;"|N|CDX"
(defun |N|CDX (n_desp n_pend)
	(cond
		((null n_pend) n_desp)
		(T
			(- (* n_desp (sin (|N|FTOA n_pend))) (/ (- n_desp (* n_desp (cos (|N|FTOA n_pend)))) (|N|FTOR n_pend)))
		)
	)
)

;calcular el coeficiente de desplazamiento en Y para un desplazamiento en una pendiente determinada
;"|N|CDY"
(defun |N|CDY (n_desp n_pend)
	(cond
		((null n_pend) n_desp)
		(T
			(- (* n_desp (cos (|N|FTOA n_pend))) (/ (- n_desp (* n_desp (sin (|N|FTOA n_pend)))) (|N|FTOR n_pend)))
		)
	)
)

;convertir un número real que representa una pendiente en su correspondiente ángulo en radianes
;"|N|ANGPD"
(defun |N|ANGPD (n_pend)
	(cond
		((= n_pend 0.0) 0.0)
		((minusp n_pend) (+ (atan 10 (/ 1 n_pend)) PI))
		(T (atan 10 (/ 1 n_pend)))
	)
)

;devolver el ángulo perpendicular según el lado
;"|N|ANGLADO"
(defun |N|ANGLADO (n_lado)
	(cond
		((eq n_lado "D") (* PI 1.5))
		(T (/ PI 2))
	)
)

