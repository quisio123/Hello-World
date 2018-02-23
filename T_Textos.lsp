;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;								C	A	D	E	N	A	S		D	E		T	E	X	T	O
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;"|T|CHARS"
(defun |T|CHARS (t_num)
	(setq t_pref 0)
	(while (> t_num 26) (setq t_num (- t_num 26) t_pref (1+ t_pref)))
	(if (> t_pref 0) (setq t_pref (+ t_pref 64)))
	(strcat (chr t_pref) (chr (+ t_num 64)))
)

;"|T|LSTV"
(defun |T|LSTV (t_string)
	(|T|LISDATOS t_string " ")
)

;"|T|INICIALES"
(defun |T|INICIALES (t_string t_~sep t_sep)
	(cond
		((null t_string) "")
		((eq (type t_string) 'STR)
			(|T|INICIALES (|T|LISDATOS t_string " ") t_~sep t_sep)
		)
		((and t_~sep (wcmatch (car t_string) (strcat "*" t_~sep "*")))
			(strcat (strcase (car t_string)) (|T|INICIALES (cdr t_string) t_~sep t_sep))
		)
		(T (if (eq (type (car t_string)) 'STR)
			(strcat (strcase (substr (car t_string) 1 1)) t_sep (|T|INICIALES (cdr t_string) t_~sep t_sep))
			(|T|INICIALES (cdr t_string) t_~sep t_sep)
		   )
		)
	)
)

;"|T|ABREV"
(defun |T|ABREV (t_string)
	(cond
		((null t_string) "")
		((eq (type t_string) 'STR)
			(|T|ABREV (|T|LSTV t_string))
		)
		(T
			(setq t_st1 (car t_string))
			(cond
				((eq (type t_st1) 'INT) (strcat (itoa t_st1) (|T|ABREV (cdr t_string))))
				((eq (strcase t_st1) "COLECTORA") (strcat "Col. " (|T|ABREV (cdr t_string))))
				((eq (strcase t_st1) "AVENIDA") (strcat "Av " (|T|ABREV (cdr t_string))))
				((wcmatch t_st1 "*#*") (strcat t_st1 (|T|ABREV (cdr t_string))))
				(T (strcat (strcase (substr t_st1 1 1)) "." (|T|ABREV (cdr t_string))))
			)
		)
	)
)

;"|T|LINICIALES"
(defun |T|LINICIALES (t_string)
	(cond ((null t_string) nil)
		((eq (type t_string) 'STR)
			(|T|LINICIALES (|T|LSTV t_string))
		)
		(T (if (eq (type (car t_string)) 'STR)
			(cons (substr (car t_string) 1 1) (|T|LINICIALES (cdr t_string)))
			(|T|LINICIALES (cdr t_string))
		   )
		)
	)
)

; agregar un caracter al final
;"|T|CONCC"
(defun |T|CONCC (t_str t_c)
	(cond
		((eq t_str "") t_c)
		((null t_c) t_str)
		((eq t_c "") t_str)
		(T (strcat t_str t_c))
	)
)

;agregar un caracter en la posición t_pos
;"|T|AGREGC"
(defun |T|AGREGC (t_str t_c t_pos)
	(cond
		((eq t_str "") t_c)
		((< (strlen t_str) t_pos)
			(strcat t_str t_c)
		)
		((= t_pos 1) (strcat t_c t_str))
		(T
			(strcat (substr t_str 1 1) (|T|AGREGC (substr t_str 2) t_c (- t_pos 1)))
		)
	)
)

;borrar el último caracter
;"|T|BORRAU"
(defun |T|BORRAU (t_str)
	(cond
		((eq t_str "") t_str)
		(T (substr t_str 1 (- (strlen t_str) 1)))
	)
)


;borrar el caracter en la posición t_pos
;"|T|BORRAC"
(defun |T|BORRAC (t_str t_pos)
	(cond
		((eq t_str "") t_str)
		((< (strlen t_str) t_pos) t_str)
		((= t_pos 1) (substr t_str 2))
		(T
			(strcat (substr t_str 1 1) (|T|BORRAC (substr t_str 2) (- t_pos 1)))
		)
	)
)

; devuelve la posición de un caracter
;"|T|IND"
(defun |T|IND (t_str t_c t_cont)
	(cond
		((eq t_str "") 2147483647)
		((not (wcmatch t_str (strcat "*[" t_c "]*"))) 2147483647)
		((wcmatch (substr t_str 1 1) (strcat "*[" t_c "]*")) t_cont)
		(T (|T|IND (substr t_str 2) t_c (1+ t_cont)))
	)
)

; reemplaza un caracter por otro
;"|T|REEMPC"
(defun |T|REEMPC (t_str t_co t_cn)
	(cond
		((eq t_str "") t_str)
		((not (wcmatch t_str (strcat "*" t_co "*"))) t_str)
		((equal (substr t_str 1 1) t_co) (|T|REEMPC (strcat t_cn (substr t_str 2)) t_co t_cn))
		(T (strcat (substr t_str 1 1) (|T|REEMPC (substr t_str 2) t_co t_cn)))
	)
)

; reemplaza la letra d por el símbolo de grados "°"
;"|T|D_G"
(defun |T|D_G (t_at)
	(|T|REEMPC t_at "d" "°")
)

; reemplaza el símbolo de grados "°" por la letra d
;"|T|G_D"
(defun |T|G_D (t_at / t_cc t_g t_m)
	(|T|REEMPC t_at "°" "d")
)

; leer hasta un caracter definido
;"|T|LEEC"
(defun |T|LEEC (t_str t_c t_sc1)
	(cond
		((null t_str) (list t_sc1 ""))
		((eq t_str "") (list t_sc1 ""))
		((null t_c) (|T|LEEC t_str "," t_sc1))
		((wcmatch t_str (strcat "*" t_c "*"))
			(cond
				((eq (substr t_str 1 1) t_c) (list t_sc1 (substr t_str 2)))
				(T
					(|T|LEEC (substr t_str 2) t_c (strcat t_sc1 (substr t_str 1 1))) 
				)
			)
		)
		(T (list t_str ""))
	)
)

;"|T|SUBCH"
(defun |T|SUBCH (t_l t_ch)
	(|T|LEEC t_l t_ch "")
)

; leer hasta una coma
;"|T|SUBC"
(defun |T|SUBC (t_l)
	(|T|LEEC t_l "," "")
)

; confirmar si un string es número entero
;"|T|ESINT"
(defun |T|ESINT (t_str t_neg)
	(cond
		((eq t_str "") (if t_neg nil T))
		((and t_neg (wcmatch t_str "-*"))
			(|T|ESINT (substr t_str 2) T)
		)
		((wcmatch t_str "#*")
			(|T|ESINT (substr t_str 2) nil)
		)
		(T nil)
	)
)

; confirmar si un string es número real
;"|T|ESREAL"
(defun |T|ESREAL (t_str)
	(cond
		((wcmatch t_str "*E*")
			(|T|ESREAL (substr t_str 1 (- (|T|IND t_str "E" 1) 1)))
		)
		((wcmatch t_str "*[.]*")
			(|T|ESINT (|T|BORRAC t_str (|T|IND t_str "." 1)) T)
		)
		(T nil)
	)
)

; confirmar si un string es un angulo
;"|T|ESANG"
(defun |T|ESANG (t_str)
	(cond
		((= (strlen t_str) 1) nil)
		((wcmatch t_str "*[DdGg°]*[Mm']*[Ss\"]")
			(and (|T|ESINT (|T|BORRAU (substr t_str 1 (|T|IND t_str "DdGg°" 1))) T)
				(|T|ESINT (|T|BORRAU (substr t_str (+ (|T|IND t_str "DdGg°" 1) 1) (- (|T|IND t_str "Mm'" 1) (+ (|T|IND t_str "DdGg°'" 1) 1)))) T)
				(or (|T|ESINT (|T|BORRAU (substr t_str (+ (|T|IND t_str "Mm'" 1) 1))) T)
					(|T|ESREAL (|T|BORRAU (substr t_str (+ (|T|IND t_str "Mm'" 1) 1))))
				)
			)
		)
;;;		((wcmatch t_str "*[DdGg°]*[Mm']")
;;;			(and (|T|ESINT (|T|BORRAU (substr t_str 1 (|T|IND t_str "DdGg°" 1))) T)
;;;				(or (|T|ESINT (|T|BORRAU (substr t_str (+ (|T|IND t_str "DdGg°" 1) 1))) T)
;;;					(|T|ESREAL (|T|BORRAU (substr t_str (+ (|T|IND t_str "DdGg°'" 1) 1))))
;;;				)
;;;			)
;;;		)
;;;		((wcmatch t_str "*[DdGg°]*[Ss\"]")
;;;			(and (|T|ESINT (|T|BORRAU (substr t_str 1 (|T|IND t_str "DdGg°" 1))) T)
;;;				(or (|T|ESINT (|T|BORRAU (substr t_str (+ (|T|IND t_str "DdGg°" 1) 1))) T)
;;;					(|T|ESREAL (|T|BORRAU (substr t_str (+ (|T|IND t_str "DdGg°" 1) 1))))
;;;				)
;;;			)
;;;		)
;;;		((wcmatch t_str "*[DdGg°]")
;;;			(or (|T|ESINT (|T|BORRAU (substr t_str 1 (|T|IND t_str "DdGg°" 1))) T)
;;;				(|T|ESREAL (|T|BORRAU (substr t_str 1 (|T|IND t_str "DdGg°" 1))))
;;;			)
;;;		)
;;;		((wcmatch t_str "*[Mm']*[Ss\"]")
;;;			(and (|T|ESINT (|T|BORRAU (substr t_str 1 (|T|IND t_str "Mm'" 1))) T)
;;;				(or (|T|ESINT (|T|BORRAU (substr t_str (+ (|T|IND t_str "Mm'" 1) 1))) T)
;;;					(|T|ESREAL (|T|BORRAU (substr t_str (+ (|T|IND t_str "Mm'" 1) 1))))
;;;				)
;;;			)
;;;		)
;;;		((wcmatch t_str "*[Mm']")
;;;			(or (|T|ESINT (|T|BORRAU (substr t_str 1 (|T|IND t_str "Mm'" 1))) T)
;;;				(|T|ESREAL (|T|BORRAU (substr t_str 1 (|T|IND t_str "Mm'" 1))))
;;;			)
;;;		)
;;;		((wcmatch t_str "*[Ss\"]")
;;;			(or (|T|ESINT (|T|BORRAU (substr t_str 1 (|T|IND t_str "Ss\"" 1))) T)
;;;				(|T|ESREAL (|T|BORRAU (substr t_str 1 (|T|IND t_str "Ss\"" 1))))
;;;			)
;;;		)
		(T nil)
	)
)

;devolver el valor correspondiente a un string
;"|T|STVAL"
(defun |T|STVAL (t_string)
	(cond
		((eq t_string "") nil)
		((|T|ESINT t_string T) (atoi t_string))
		((|T|ESREAL t_string) (atof t_string))
		((|T|ESANG t_string)
			(cond
				((wcmatch t_priel "*G*") (setq t_string (|T|REEMPC t_string "G" "D")))
				((wcmatch t_string "*g*") (setq t_string (|T|REEMPC t_string "g" "D")))
				((wcmatch t_string "*°*") (setq t_string (|T|REEMPC t_string "°" "D")))
			)
			(cond
				((wcmatch t_string "*M*") (setq t_string (|T|REEMPC t_string "M" "'")))
				((wcmatch t_string "*m*") (setq t_string (|T|REEMPC t_string "m" "'")))
			)
			(cond
				((wcmatch t_string "*S*") (setq t_string (|T|REEMPC t_string "S" "\"")))
				((wcmatch t_string "*s*") (setq t_string (|T|REEMPC t_string "s" "\"")))
			)
			(angtof t_string 3)
		)
		(T t_string)
	)
)


;separar datos de una línea de texto
;"|T|LISDATOS"
(defun |T|LISDATOS (t_linea t_ch / t_ldat t_list t_priel)
	(cond
		((eq t_linea "") nil)
		(T (setq t_list (|T|SUBCH t_linea t_ch) t_priel (car t_list) t_linea (cadr t_list) t_val (|T|STVAL t_priel))
;;;			(if t_val 
				(cons t_val (|T|LISDATOS t_linea t_ch))
;;;				(|T|LISDATOS t_linea t_ch)
;;;			)
		)
	)
)

;convertir un dato a string
;"|T|DTOST"
(defun |T|DTOST (t_dat t_ch)
	(defun |T|EL0 (t_str)
		(cond
			((eq (substr t_str (strlen t_str) 1) "0")
				(|T|EL0 (substr t_str 1 (1- (strlen t_str))))
			)
			((eq (substr t_str (strlen t_str) 1) ".")
				(strcat t_str "0")
			)
			(T t_str)
		)
	)
	(cond	((null t_dat) "")
		((eq (type t_dat) 'INT)
			(itoa t_dat)
		)
		((eq (type t_dat) 'REAL)
			(|T|EL0 (rtos t_dat 2 14))
		)
		((eq (type t_dat) 'LIST)
			(cond
				((eq (car t_dat) "A")
					(angtos (cadr t_dat) 1 (caddr t_dat))
				)
				((eq (car t_dat) "R")
					(rtos (cadr t_dat) 2 (caddr t_dat))
				)
				(T (|T|CONCATCH t_dat t_ch))
			)
		)
		(T t_dat)
	)
)

;concatenar una lista convirtiendo sus datos a strings y separándolos con un caracer especificado
;"|T|CONCATST"
(defun |T|CONCATST (t_lisel t_ch)
	(cond
		((null t_lisel) "")
		((null t_ch) (|T|CONCATST t_lisel ","))
		(T (strcat (|T|DTOST (car t_lisel) t_ch) t_ch (|T|CONCATST (cdr t_lisel) t_ch)))
	)
)
  
;"|T|CONCATCH"
(defun |T|CONCATCH (t_lisel t_ch)
	(|T|BORRAU (|T|CONCATST t_lisel t_ch))
)

;concatenar una lista de strings separándolos con ","
;"|T|CONCAT"
(defun |T|CONCAT (t_lisel)
	(|T|CONCATCH t_lisel ",")
)

; convertir grados, minutos y segundos en un número real
;"|T|GMSAR"
(defun |T|GMSAR (t_gms)
	(cond
		((wcmatch t_gms "*d*")
			(setq t_lg (|T|SUBCH t_gms "d") t_g (car t_lg) t_gms (cadr t_lg))
		)
		((wcmatch t_gms "*D*")
			(setq t_lg (|T|SUBCH t_gms "D") t_g (car t_lg) t_gms (cadr t_lg))
		)
		((wcmatch t_gms "*g*")
			(setq t_lg (|T|SUBCH t_gms "g") t_g (car t_lg) t_gms (cadr t_lg))
		)
		((wcmatch t_gms "*G*")
			(setq t_lg (|T|SUBCH t_gms "G") t_g (car t_lg) t_gms (cadr t_lg))
		)
		((wcmatch t_gms "*°*")
			(setq t_lg (|T|SUBCH t_gms "°") t_g (car t_lg) t_gms (cadr t_lg))
		)
		((wcmatch t_gms "* *")
			(setq t_lg (|T|SUBCH t_gms " ") t_g (car t_lg) t_gms (cadr t_lg))
		)
		(T (setq t_g "0"))
	)
	(cond
		((wcmatch t_gms "*m*")
			(setq t_lm (|T|SUBCH t_gms "m") t_m (car t_lm) t_gms (cadr t_lm))
		)
		((wcmatch t_gms "*M*")
			(setq t_lm (|T|SUBCH t_gms "M") t_m (car t_lm) t_gms (cadr t_lm))
		)
		((wcmatch t_gms "*'*")
			(setq t_lm (|T|SUBCH t_gms "'") t_m (car t_lm) t_gms (cadr t_lm))
		)
		((wcmatch t_gms "* *")
			(setq t_lg (|T|SUBCH t_gms " ") t_g (car t_lg) t_gms (cadr t_lg))
		)
		(T (setq t_m "0"))
	)
	(+ (atof t_g) (/ (atof t_m) 60) (/ (atof t_gms) 3600)) 
)

;"|T|NTOST"
(defun |T|CTOST (t_dat)
	(cond
		((null t_dat) "N/A")
		((eq (type t_dat) 'INT)
			(itoa t_dat)
		)
		((eq (type t_dat) 'REAL)
			(rtos t_dat 2 2)
		)
		(T t_dat)
	)
)

(defun |T|PRINTDAT (t_lisdata t_lpref)
	(cond
		((null t_lisdata) "")
		(T (strcat " - " (car t_lpref) (|T|CTOST (car t_lisdata)) (|T|PRINTDAT (cdr t_lisdata) (cdr t_lpref))))
	)
)

;"|T|STOL"
(defun |T|STOL (t_char)
	(if (eq (strcase t_char) "S") T nil)
)

(defun |T|GETST (t_pre t_msg t_b)
 	(if t_pre
		(setq t_msg (strcat "\n" t_msg " <" t_pre ">: "))
		(setq t_msg (strcat "\n" t_msg ": "))
	)
	(if (setq t_text (getstring t_b t_msg))
		t_text
		t_pre
	)
)

(defun |T|SEP- (t_t1 t_t2)
	(cond
		((eq t_t1 "") t_t2)
		((eq t_t2 "") t_t1)
		(T (strcat t_t1 "-" t_t2))
	)
)

(defun |T|LISATT (t_lisatt t_lis0)
	(defun |T|ATT2ST (t_att)
		(cond
			((or (null t_att) (eq u_att "")) (list "--"))
			((eq (type t_att) 'INT)
				(if (and (= t_att 0) (not t_lis0)) (list "--") (list (itoa t_att)))
			)
			((eq (type t_att) 'REAL)
				(if (and (= (|N|ROUND t_att 2) 0.0) (not t_lis0)) (list "--") (list (rtos t_att 2 2)))
			)
			((eq (type t_att) 'LIST)
				(cond
					((eq (car t_att) "<")
						(list (|T|D_G (angtos (cadr t_att) 1 6)))
					)
					((eq (car t_att) "-")
						(list (|T|CONCATCH (|T|LISATT (cdr t_att) t_lis0) (car t_att)))
					)
					(T (|T|LISATT t_att t_lis0))
				)
			)
			(T (list t_att))
		)
	)


	(cond
		((null t_lisatt) nil)
		(T
			(append (|T|ATT2ST (car t_lisatt)) (|T|LISATT (cdr t_lisatt) t_lis0))
		)
	)
)

(defun |T|ITOC (t_num t_dig)
	(setq t_cod (itoa t_num) t_lt (strlen t_cod))
	(repeat (- t_dig t_lt)
		(setq t_cod (strcat "0" t_cod))
	)
	t_cod
)

(defun |T|SINGULAR (t_str)
	(cond
		((null t_str) "")
		((wcmatch t_str "* *")
			(apply 'strcat (mapcar '|T|SINGULAR (|T|LISDATOS t_str " ")))
		)
		(T
			(setq t_ls (strlen t_str))
			(cond
				((eq (strcase (substr t_str (- t_ls 2))) "CES")
					(strcat (substr t_str 1 (- t_ls 3)) "Z ")
				)
				((eq (strcase (substr t_str (- t_ls 1))) "ES")
					(strcat (substr t_str 1 (- t_ls 2)) " ")
				)
				((eq (strcase (substr t_str t_ls)) "S")
					(strcat (substr t_str 1 (- t_ls 1)) " ")
				)
				(T (strcat t_str " "))
			)
		)
	)
)