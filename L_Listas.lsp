;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;								L		I		S		T		A		S
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;					C	R	E	A	R		Y		C	O	M	P	L	E	T	A	R
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
; crear una lista con n elementos
;"|L|CREA"
(defun |L|CREA (l_n l_el)
	(cond
		((zerop l_n) nil)
		(T (cons l_el (|L|CREA (1- l_n) l_el)))
	)
)

;"|L|LNUM"
(defun |L|LNUM (l_n l_max l_nc)
	(cond
		((> l_n l_max) nil)
		((> l_max l_nc) (append (|L|LNUM l_n l_nc l_nc) (|L|LNUM (1+ l_nc) l_max (+ l_nc 10000.0))))
		(T (cons l_n (|L|LNUM (1+ l_n) l_max l_nc)))
	)
)

; completar una lista con n elementos
;"|L|COMPLETAR"
(defun |L|COMPLETAR (l_lista l_n l_el)
	(cond
		((> (length l_lista) l_n) l_lista)
		(T
			(append l_lista (|L|CREA (- (1+ l_n) (length l_lista)) l_el))
		)
	)
)

;agregar un elemento a una lista en una posición determinada
;"|L|AGREGAP"
(defun |L|AGREGAP (l_lista l_el l_pos)
	(cond
		((null l_lista) (list l_el))
		((> l_pos (length l_lista)) (|L|AGREGAP (|L|COMPLETAR l_lista l_pos nil) l_el l_pos))
		((zerop l_pos) (cons l_el l_lista))
		(T
			(setq l_divl (|L|DIVL l_lista l_pos))
			(append (car l_divl) (list l_el) (cadr l_divl))
		)
	)
)

;agregar un elemento a una lista ordenada de menor a mayor
;"|L|AGREGA"
(defun |L|AGREGA (l_lista l_el / l_divm l_sl1 l_sl2)
	(cond
		((null l_lista) (list l_el))
		((< l_el (car l_lista)) (cons l_el l_lista))
		((> l_el (last l_lista)) (append l_lista (list l_el)))
		(T
			(setq l_divm (|L|DIVM l_lista) l_sl1 (car l_divm) l_sl2 (cadr l_divm))
			(if (< l_el (last l_sl1))
				(append (|L|AGREGA l_sl1 l_el) l_sl2)
				(append l_sl1 (|L|AGREGA l_sl2 l_el))
			)
		)
	)
)

;determinar la igualdad entre dos listas
;"|L|IGUAL"
(defun |L|IGUAL (l_l1 l_l2)
	(cond
		((and (null l_l1) (null l_l2)) T)
		((/= (length l_l1) (length l_l2)) nil)
		((or (null l_l1) (not (listp l_l1))) nil)
		((or (null l_l2) (not (listp l_l2))) nil)
		((eq (car l_l1) (car l_l2))
			(|L|IGUAL (cdr l_l1) (cdr l_l2))
		)
		(T nil)
	)
)

;determinar la igualdad entre dos listas con tolerancias
;"|L|IGUALT"
(defun |L|IGUALT (l_l1 l_l2 l_ltol)
	(cond
		((and (null l_l1) (null l_l2)) T)
		((/= (length l_l1) (length l_l2)) nil)
		((or (null l_l1) (not (listp l_l1))) nil)
		((or (null l_l2) (not (listp l_l2))) nil)
		((null l_ltol)
			(if (eq (car l_l1) (car l_l2))
				(|L|IGUALT (cdr l_l1) (cdr l_l2) l_ltol)
			)
		)
		((atom l_ltol)
			(if (equal (car l_l1) (car l_l2) l_ltol)
				(|L|IGUALT (cdr l_l1) (cdr l_l2) l_ltol)
			)
		)
		((equal (car l_l1) (car l_l2) (car l_ltol))
			(|L|IGUALT (cdr l_l1) (cdr l_l2) (cdr l_ltol))
		)
		(T nil)
	)
)

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;							I	N	F	O	R	M	A	C	I	O	N
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;verificar si todos los puntos de una lista son 3d
;"|L|3D"
(defun |L|3D (l_lista)
	(apply 'and (mapcar '|K|3D l_lista))
)

;devolver la ubicación de un elemento en una lista
;"|L|POS"
(defun |L|POS (l_el l_lista / l_laux)
	(setq l_laux (member l_el l_lista))
	(- (length l_lista) (length l_laux))
)

;devolver la ubicación de la sublista en la cual se encuentra el elemento buscado en la posición indicada
;"|L|POSSL"
(defun |L|POSSL (l_el l_lista l_p)
	(|L|POS l_el (mapcar '(lambda (el) (nth l_p el)) l_lista))
)

;Obtener los máximos y mínimos de una lista
;"|L|M&M"
(defun |L|M&M (l_lista)
	(cond
		((or (null l_lista) (null (car l_lista))) nil)
		((listp (car l_lista))
			(cons (|L|M&M (mapcar 'car l_lista)) (|L|M&M (mapcar 'cdr l_lista)))
		)
		(T 
			(list (apply 'max l_lista) (apply 'min l_lista))
		)
	)
)

;obtener el máximo de una lista y la lista sin el
;"|L|MAX"
(defun |L|MAX (l_lista / l_max l_sl)
	(cond
		((null l_lista) nil)
		((listp (car l_lista))
			(setq l_max (apply 'max (mapcar 'car l_lista)) l_sl (|L|OBTSLE l_lista l_max 0 T) l_psl 1)
			(while (and (> (length l_sl) 1) (< l_psl (length (car l_sl))))
				(setq l_max (apply 'max (mapcar '(lambda (el) (nth l_psl el)) l_sl)) l_sl (|L|OBTSLE l_sl l_max l_psl T) l_psl (1+ l_psl))
			)
			(list (car l_sl) (|L|BPOS (|L|POS (car l_sl) l_lista) l_lista))
		)
		(T
			(setq l_max (apply 'max l_lista)) 
			(list l_max (|L|BPOS (|L|POS l_max l_lista) l_lista))
		)
	)
)

;obtener el mínimo de una lista y la lista sin el
;"|L|MIN"
(defun |L|MIN (l_lista / l_min l_sl)
	(cond
		((null l_lista) nil)
		((listp (car l_lista))
			(setq l_min (apply 'min (mapcar 'car l_lista)) l_sl (|L|OBTSLE l_lista l_min 0 T) l_psl 1)
			(while (and (> (length l_sl) 1) (< l_psl (length (car l_sl))))
				(setq l_min (apply 'min (mapcar '(lambda (el) (nth l_psl el)) l_sl)) l_sl (|L|OBTSLE l_sl l_min l_psl T) l_psl (1+ l_psl))
			)
			(list (car l_sl) (|L|BPOS (|L|POS (car l_sl) l_lista) l_lista))
		)
		(T
			(setq l_min (apply 'max l_lista)) 
			(list l_min (|L|BPOS (|L|POS l_min l_lista) l_lista))
		)
	)
)

;verificar si existe un elemento en una lista de sublistas en una posición determinada de la sublista
;"|L|EXISTE"
(defun |L|EXISTE (l_elem l_lista l_pos)
	(cond
		((null l_lista) nil)
		((listp (car l_lista))
			(if (equal l_elem (nth l_pos (car l_lista)) g_tol) T
				(|L|EXISTE l_elem (cdr l_lista) l_pos)
			)
		)
		(T (|L|EXISTE l_elem (cdr l_lista) l_pos))
	)
)

;buscar un elemento en una posición determinada de una sublista de una lista de sublistas y devolver la sublista correspondiente
;"|L|BUSCAR"
(defun |L|BUSCAR (l_elem l_lista l_pos / l_ind)
	(cond
		((null l_lista) nil)
		((setq l_ind (|L|POSSL l_elem l_lista l_pos))
			(nth l_ind l_lista)
		)
		(T nil)
	)
)

;verificar si una lista de valores es continua
;"|L|ESCONT"
(defun |L|ESCONT (l_list l_ord)
	(cond
		((= (length l_list) 1) T)
		((and (= l_ord "D") (= (1+ (car l_list)) (cadr l_list)))
			(|L|ESCONT (cdr l_list) l_ord)
		)
		((and (= l_ord "A") (= (1- (car l_list)) (cadr l_list)))
			(|L|ESCONT (cdr l_list) l_ord)
		)
		(T nil)
	)
)

;buscar si una lista es parte de otra lista
;"|L|ESPARTE"
(defun |L|ESPARTE (l_list1 l_list2 l_ord l_lind / l_ind)
	(cond
		((null l_list2) nil)
		((not (listp l_list1)) (member l_list1 l_list2))
		((null l_list1) (if l_ord (|L|ESCONT l_lind l_ord) T))
		((setq l_ind (|L|POS (car l_list1) l_list2))
			(|L|ESPARTE (cdr l_list1) l_list2 l_ord (cons l_ind l_lind))
		)
		(T nil)
	)
)

;determinar si un elemento es menor que otro (incluidas listas)
;"|L|ESMENOR"
(defun |L|ESMENOR (l_el1 l_el2)
	(cond
		((and (listp l_el1) (listp l_el2))
			(equal (|L|MENOR l_el1 l_el2) l_el1 g_tol)
		)
		((and (listp l_el1) (not (listp l_el2)))
			(|L|ESMENOR (car l_el1) l_el2)
		)
		((and (not (listp l_el1)) (listp l_el2))
			(|L|ESMENOR l_el1 (car l_el2))
		)
		((< l_el2 l_el1) nil)
		(T T)
	)
)

;determinar si un elemento es mayor que otro (incluidas listas)
;"|L|ESMAYOR"
(defun |L|ESMAYOR (l_el1 l_el2)
	(cond
		((and (listp l_el1) (listp l_el2))
			(equal (|L|MAYOR l_el1 l_el2) l_el1 g_tol)
		)
		((and (listp l_el1) (not (listp l_el2)))
			(|L|ESMAYOR (car l_el1) l_el2)
		)
		((and (not (listp l_el1)) (listp l_el2))
			(|L|ESMAYOR l_el1 (car l_el2))
		)
		((< l_el2 l_el1) nil)
		(T T)
	)
)

;determinar si un elemento pertenece a una lista
;"|L|MIEMBRO"
(defun |L|MIEMBRO (l_el l_list l_tol)
	(cond
		((null l_list) nil)
		((equal l_el (car l_list) l_tol) T)
		(T (|L|MIEMBRO l_el (cdr l_list) l_tol)) 
	)
)

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;					O	B	T	E	N	E	R		E	L	E	M	E	N	T	O	S
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;dividir una lista en dos
;"|L|DIVL"
(defun |L|DIVL (l_lista l_nel / l_laux)
	(cond
		((null l_lista) nil)
		((minusp l_nel) nil)
		((>= l_nel (length l_lista))
			(list l_lista nil)
		)
		(T
			(setq l_laux '())
			(repeat l_nel
				(setq l_laux (cons (car l_lista) l_laux) l_lista (cdr l_lista))
			)
			(list (reverse l_laux) l_lista)
		)
	)
)

;dividir una lista a la mitad
;"|L|DIVM"
(defun |L|DIVM (l_lista)
	(|L|DIVL l_lista (fix (/ (length l_lista) 2)))
)

; obtener los n primeros elementos de una lista
;"|L|SUBL"
(defun |L|SUBL (l_lista l_nel)
	(car (|L|DIVL l_lista l_nel))
) 

; obtener una sublista sin los n primeros elementos de una lista
;"|L|RESL"
(defun |L|RESL (l_lista l_nel)
	(cadr (|L|DIVL l_lista l_nel))
)

; obtener una sublista con los n últimos elementos de una lista
;"|L|ULTL"
(defun |L|ULTL (l_lista l_nel)
	(cadr (|L|DIVL l_lista (- (length l_lista) l_nel)))
)

;dada una lista de elementos y una lista de posiciones devolver una lista con los elementos de la primera lista en las posiciones de la segunda lista
;"|L|SUBLP"
(defun |L|SUBLP (l_lista l_lpos)
	(defun |L|OBTPOS (l_sl l_lp)
		(cond
			((null l_lp) nil)
			(T
				(cons (nth (car l_lp) l_sl) (|L|OBTPOS l_sl (cdr l_lp)))
			)
		)
	)
	(cond
		((null l_lista) nil)
		((> (length l_lista) 10000)
			(setq l_divm (|L|DIVM l_lista))
			(append (|L|SUBLP (car l_divm) l_lpos) (|L|SUBLP (cadr l_divm) l_lpos))
		)
		((listp (car l_lista))
			(cons (|L|OBTPOS (car l_lista) l_lpos) (|L|SUBLP (cdr l_lista) l_lpos))
		)
		(T
			(|L|OBTPOS l_lista l_lpos)
		)
	)
)

;devolver los elementos de una lista que no están en la segunda lista
; si l_sim es T se verifica tambien si el elemnto inverso está en la segunda lista
;"|L|LIBRES"
(defun |L|LIBRES (l_l1 l_l2 l_sim)
	(cond
		((null l_l1) nil)
		((null l_l2) l_l1)
		((> (length l_l1) 10000)
			(setq l_divm (|L|DIVM l_l1))
			(append (|L|LIBRES (car l_divm) l_l2 l_sim) (|L|LIBRES (cadr l_divm) l_l2 l_sim))
		)
		((member (car l_l1) l_l2)
			(|L|LIBRES (cdr l_l1) l_l2 l_sim)
		)
		((and l_sim (listp (car l_l1)))
			(if (member (reverse (car l_l1)) l_l2)
				(|L|LIBRES (cdr l_l1) l_l2 l_sim)
				(cons (car l_l1) (|L|LIBRES (cdr l_l1) l_l2 l_sim)) 
			)
		)
		(T
			(cons (car l_l1) (|L|LIBRES (cdr l_l1) l_l2 l_sim))
		)
	)
)

;devolver todas las sublistas de una lista que contengan el elemnto buscado en la posición indicada l_pos
;si l_f es nil devuelve una lista sin las sublistas que contengan dicho elemento
;"|L|OBTSLE"
(defun |L|OBTSLE (l_lista l_el l_pos l_f)
	(cond
		((null l_lista) nil)
		((> (length l_lista) 10000)
			(setq l_divm (|L|DIVM l_lista))
			(append (|L|OBTSLE (car l_divm) l_el l_pos l_f) (|L|OBTSLE (cadr l_divm) l_el l_pos) l_f)
		)
		((equal (nth l_pos (car l_lista)) l_el g_tol)
			(if l_f
				(cons (car l_lista) (|L|OBTSLE (cdr l_lista) l_el l_pos l_f))
				(|L|OBTSLE (cdr l_lista) l_el l_pos l_f)
			)
		)
		((and (eq (type l_el) 'STR) (eq (type (nth l_pos (car l_lista))) 'STR) (wcmatch (nth l_pos (car l_lista)) l_el))
			(if l_f
				(cons (car l_lista) (|L|OBTSLE (cdr l_lista) l_el l_pos l_f))
				(|L|OBTSLE (cdr l_lista) l_el l_pos l_f)
			)
		)
		(T
			(if (not l_f)
				(cons (car l_lista) (|L|OBTSLE (cdr l_lista) l_el l_pos l_f))
				(|L|OBTSLE (cdr l_lista) l_el l_pos l_f)
			)
		)
	)
)

;Devolver un sublista de una lista
;"|L|OBTSL"
(defun |L|OBTSL (l_lista l_desde l_hasta)
	(defun |L|PARCIAL (l_list l_val l_simb)
		(cond
			((null l_list) nil)
			((listp (car l_list))
				(cond
					((= (caar l_list) l_val) l_list)
					((l_simb (caar l_list) l_val)
						(|L|PARCIAL (cdr l_list) l_val l_simb)
					)
					(T l_list)
				)
			)
			(T
				(cond
					((= (car l_list) l_val) l_list)
					((l_simb (car l_list) l_val)
						(|L|PARCIAL (cdr l_list) l_val l_simb)
					)
					(T l_list)
				)
			)
		)
	)

	(cond
		((null l_lista) nil)
		((> (length l_lista) 10000)
			(setq l_divm (|L|DIVM l_lista))
			(append (|L|OBTSL (car l_divm) l_desde l_hasta) (|L|OBTSL (cadr l_divm) l_desde l_hasta))
		)
		(T
			(setq l_ldesde (|L|PARCIAL l_lista l_desde <) l_lhasta (|L|PARCIAL (reverse l_ldesde) l_hasta >))
			(reverse l_lhasta)
		)
	)
)

;devolver una lista como resultado de interpolar los valores de dos listas
;"|L|INTERP"
(defun |L|INTERP (l_list1 l_list2 l_st l_sti l_stf)
	(cond
		((null l_list1) l_list2)
		((or (null l_list2) (null l_st)) l_list1)
		(T
			(setq l_val1 (car l_list1) l_val2 (car l_list2))
		 	(cond
				((eq l_val1 l_val2)
					(cons l_val1 (|L|INTERP (cdr l_list1) (cdr l_list2) l_st l_sti l_stf))
				)
				((and (eq (type l_val1) 'STR) (eq (type l_val2) 'STR))
					(cons (strcat l_val1 "-" l_val2) (|L|INTERP (cdr l_list1) (cdr l_list2) l_st l_sti l_stf))
				)
				((eq (type l_val1) 'STR)
					(cons l_val2 (|L|INTERP (cdr l_list1) (cdr l_list2) l_st l_sti l_stf))
				)
				((eq (type l_val2) 'STR)
					(cons l_val1 (|L|INTERP (cdr l_list1) (cdr l_list2) l_st l_sti l_stf))
				)
				(T
					(cons (+ l_val1 (* (- l_st l_sti) (/ (- l_val2 l_val1) (- l_stf l_sti)))) (|L|INTERP (cdr l_list1) (cdr l_list2) l_st l_sti l_stf))
				)
			)
		)
	)
)

;Devolver un valor o una sublista por interpolación de los valores más próximos de una lista
;"|L|OBTSLV"
(defun |L|OBTSLV (l_lista l_desde l_hasta)
	(defun |L|PARCIAL (l_list l_val l_simb l_eant)
		(cond
			((null l_list) (list l_eant nil))
			((listp (car l_list))
				(cond
					((= (caar l_list) l_val) (list nil l_list))
					((l_simb (caar l_list) l_val)
						(|L|PARCIAL (cdr l_list) l_val l_simb (car l_list))
					)
					(T (list l_eant l_list))
				)
			)
			(T
				(cond
					((= (car l_list) l_val) (list nil l_list))
					((l_simb (car l_list) l_val)
						(|L|PARCIAL (cdr l_list) l_val l_simb (car l_list))
					)
					(T (list l_eant l_list))
				)
			)
		)
	)

	(cond
		((null l_lista) nil)
		(T
			(setq l_ldesde (|L|PARCIAL l_lista l_desde < nil) l_lhasta (|L|PARCIAL (reverse (cadr l_ldesde)) l_hasta > nil)
			      l_ant (car l_ldesde) l_post (car l_lhasta) l_entre (reverse (cadr l_lhasta))
			)
			(cond
				((null l_entre)
					(list (|L|INTERP l_ant l_post l_desde (car l_ant) (car l_post))
					      (|L|INTERP l_ant l_post l_hasta (car l_ant) (car l_post))
					)
				)
				((and l_ant l_post)
					(append (list (|L|INTERP l_ant (car l_entre) l_desde (car l_ant) (caar l_entre)))
						l_entre
						(list (|L|INTERP (last l_entre) l_post l_hasta (car (last l_entre)) (car l_post)))
					)
				)
				(l_ant
					(append (list (|L|INTERP l_ant (car l_entre) l_desde (car l_ant) (caar l_entre)))
						l_entre
					)
				)
				(l_post
					(append 
						l_entre
						(list (|L|INTERP (last l_entre) l_post l_hasta (car (last l_entre)) (car l_post)))
					)
				)
				(T l_entre)
			)
		)
	)
)

;Devolver la lista con el valor anterior y el valor posterior al valor buscado
;"|L|OBTPROX"
(defun |L|OBTPROX (l_lista l_val)
	(defun |L|PARC (l_list l_val l_simb l_eant)
		(cond
			((null l_list) l_eant)
			((listp (car l_list))
				(cond
					((l_simb (caar l_list) l_val)
						(|L|PARC (cdr l_list) l_val l_simb (car l_list))
					)
					(T l_eant)
				)
			)
			(T
				(cond
					((l_simb (car l_list) l_val)
						(|L|PARC (cdr l_list) l_val l_simb (car list))
					)
					(T l_eant)
				)
			)
		)
	)

	(cond
		((null l_lista) nil)
		(T
			(setq l_ant (|L|PARC l_lista l_val < nil) l_post (|L|PARC (reverse l_lista) l_val > nil))
			(list l_ant l_post)
		)
	)
)

;devolver los elementos pares de una lista
;"|L|PARES"
(defun |L|PARES (l_lista)
	(cond
		((null l_lista) nil)
		(T
			(cons (cadr l_lista) (|L|PARES (cddr l_lista)))
		)
	)
)

;devolver los elementos impares de una lista
;"|L|IMPARES"
(defun |L|IMPARES (l_lista)
	(cond
		((null l_lista) nil)
		(T
			(cons (car l_lista) (|L|IMPARES (cddr l_lista)))
		)
	)
)

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;				E	L	I	M	I	N	A	R		E	L	E	M	E	N	T	O	S
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
; borrar el último elemento de una lista
;"|L|BULT"
(defun |L|BULT (l_lista)
	(cond
		((null l_lista) nil)
		((eq (length l_lista) 1) nil)
		(T (reverse (cdr (reverse l_lista))))
	)
)

;borrar el elemento n° n de una lista
;"|L|BPOS"
(defun |L|BPOS (l_n l_lista)
	(cond
		((null l_lista) nil)
		((zerop l_n) (cdr l_lista))
		((> l_n (length l_lista)) l_lista)
		(T
			(setq l_divl (|L|DIVL l_lista l_n))
			(append (car l_divl) (cdadr l_divl))
		)
	)
)

;borrar una única ocurrencia de un elemento en una lista
;"|L|BORRA"
(defun |L|BORRA (l_el l_lista)
	(|L|BPOS (|L|POS l_el l_lista) l_lista)
)

;borrar todas las ocurrencias de un elemento en una lista
;"|L|BORRAT"
(defun |L|BORRAT (l_el l_lista / l_pos l_laux)
	(setq l_pos (|L|POS l_el l_lista) l_divl (|L|DIVL l_lista l_pos) l_laux (car l_divl) l_lista (cdadr l_divl))
	(while l_lista
		(setq l_pos (|L|POS l_el l_lista) l_divl (|L|DIVL l_lista l_pos) l_laux (append l_laux (car l_divl)) l_lista (cdadr l_divl))
	)
	l_laux
)

;borrar todas las ocurrencias de un elemento y su simétrico de una lista
;"|L|BORRATS"
(defun |L|BORRATS (l_el l_lista / l_pos l_laux)
	(setq l_pos (|L|POS l_el l_lista) l_divl (|L|DIVL l_lista l_pos) l_laux (car l_divl) l_lista (cdadr l_divl))
	(while l_lista
		(setq l_pos (|L|POS l_el l_lista) l_divl (|L|DIVL l_lista l_pos) l_laux (append l_laux (car l_divl)) l_lista (cdadr l_divl))
	)
	(setq l_lista l_laux l_el (reverse l_el) l_pos (|L|POS l_el l_lista) l_divl (|L|DIVL l_lista l_pos) l_laux (car l_divl) l_lista (cdadr l_divl))
 	(while l_lista
		(setq l_pos (|L|POS l_el l_lista) l_divl (|L|DIVL l_lista l_pos) l_laux (append l_laux (car l_divl)) l_lista (cdadr l_divl))
	)
	l_laux
)
;borrar todas las ocurrencias de un elemento en las sublistas de una lista
;"|L|BESL"
(defun |L|BESL (l_el l_lista)
	(cond
		((null l_lista) nil)
		(T (cons (|L|BORRAT l_el (car l_lista)) (|L|BESL l_el (cdr l_lista))))
	)
)

;borrar todas las sublistas que contengan un elemento determinado en una lista
;"|L|BSL"
(defun |L|BSL (l_el l_lista)
	(cond
		((null l_lista) nil)
		((member l_el (car l_lista)) (|L|BSL l_el (cdr l_lista)))
		(T (cons (car l_lista) (|L|BSL l_el (cdr l_lista))))
	)
)

;depurar una lista eliminando miembros repetidos y nulos
;"|L|DEPURAR"
(defun |L|DEPURAR (l_lista / l_res l_el)
	(setq l_res '())
	(while l_lista
		(setq l_el (car l_lista) l_lista (cdr l_lista))
		(if (and l_el (not (member l_el l_res)))
			(setq l_res (cons l_el l_res))
		)
	)
	(reverse l_res)
)

;borrar todas las repeticiones de un elemento en una lista
;"|L|ELIMDUP"
(defun |L|ELIMDUP (l_lista / l_res l_el)
;;;	(setq l_res '())
;;;	(while l_lista
;;;		(setq l_el (car l_lista) l_lista (cdr l_lista))
;;;		(if (and l_el (not (member l_el l_res)))
;;;			(setq l_res (cons l_el l_res))
;;;		)
;;;	)
;;;	(reverse l_res)
	(cond
		((null l_lista) nil)
		((null (car l_lista)) (|L|ELIMDUP (cdr l_lista)))
		(T (cons (car l_lista) (|L|ELIMDUP (|L|BORRAT (car l_lista) (cdr l_lista)))))
	)
)

(defun |L|ELIMDUPS (l_lista / l_res l_el)
	(setq l_res '())
	(while l_lista
		(setq l_el (car l_lista) l_lista (cdr l_lista))
		(if (not (or (member l_el l_res) (member (|L|CPOS l_el 0 1) l_res)))
			(setq l_res (cons l_el l_res))
		)
	)
	(reverse l_res)
)

;borrar todas las repeticiones de un elemento en una lista ordenada
;"|L|ELREPLO"
(defun |L|ELREPLO (l_lista)
	(cond
		((null l_lista) nil)
		((equal (car l_lista) (cadr l_lista) 0.1) (|L|ELREPLO (cdr l_lista)))
		(T (cons (car l_lista) (|L|ELREPLO (cdr l_lista))))
	)
)

;purgar una lista eliminando elementos nulos
;"|L|PURGAR"
(defun |L|PURGAR (l_lista)
	(cond
		((null l_lista) nil)
		((null (car l_lista)) (|L|PURGAR (cdr l_lista)))
		((listp (car l_lista)) (cons (|L|PURGAR (car l_lista)) (|L|PURGAR (cdr l_lista))))
		(T (cons (car l_lista) (|L|PURGAR (cdr l_lista))))
	)
)

;borrar elementos similares de una lista
;"|L|BSIM"
(defun |L|BSIM (l_el l_lista l_holg)
	(cond
		((null l_lista) nil)
		((equal (car l_lista) l_el l_holg) (|L|BSIM l_el (cdr l_lista) l_holg))
		(T (cons (car l_lista) (|L|BSIM l_el (cdr l_lista) l_holg)))
	)
)

;"|L|ELSIM"
(defun |L|ELSIM (l_lista l_holg)
	(cond
		((null l_lista) nil)
		((null (car l_lista)) (|L|ELSIM (cdr l_lista) l_holg))
		(T (cons (car l_lista) (|L|ELSIM (|L|BSIM (car l_lista) (cdr l_lista) l_holg) l_holg)))
	)
)

;devolver una lista de sublistas sin el primer elemento de cada sublista
;"|L|CDRSL"
(defun |L|CDRSL (l_lista)
	(cond
		((null l_lista) nil)
		(T (mapcar 'cdr l_lista))
	)
)

;devolver una lista de sublistas con los n primeros elementos de cada sublista
;"|L|#PRISL"
(defun |L|#PRISL (l_lista l_nel)
	(cond
		((null l_lista) nil)
		(T (mapcar '(lambda (l_el) (|L|SUBL l_el l_nel)) l_lista))
	)
)

;devolver una lista de sublistas sin el último elemento de cada sublista
;"|L|BULTSL"
(defun |L|BULTSL (l_lista)
	(cond ((null l_lista) nil)
		(T (mapcar '|L|BULT l_lista))
	)
)

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;		R	E	E	M	P	L	A	Z	A	R		E	L	E	M	E	N	T	O	S
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;reemplazar el elemento nº n de una lista
;"|L|REEMPOS"
(defun |L|REEMPOS (l_ind l_nuevo l_lista)
	 (cond
		  ((minusp l_ind) l_lista)
		  ((zerop l_ind) (cons l_nuevo (cdr l_lista)))
		  (T (cons (car l_lista) (|L|REEMPOS (1- l_ind) l_nuevo (cdr l_lista))))
	 )
)

;reemplazar un elemento de una lista
;"|L|REEMP"
(defun |L|REEMP (l_ov l_no l_lista)
	 (cond
		  ((null l_lista) nil)
		  ((equal (car l_lista) l_ov g_tol) (cons l_no (cdr l_lista)))
		  (T (cons (car l_lista) (|L|REEMP l_ov l_no (cdr l_lista))))
	 )
;;;	(|L|REEMPOS (|L|POS l_ov) l_no l_lista)
)

;reemplazar todas las ocurrencias de un elemento de una lista
;"|L|REEMPT"
(defun |L|REEMPT (l_ov l_no l_lista)
	(cond
		((null l_lista) nil)
		((equal (car l_lista) l_ov 0.001) (cons l_no (|L|REEMPT l_ov l_no (cdr l_lista)))) 
		(T (cons (car l_lista) (|L|REEMPT l_ov l_no (cdr l_lista))))
	)
)

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;				A	G	R	U	P	A	R		E	L	E	M	E	N	T	O	S
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;agrupar datos de una lista
;"|L|ENLAZAR"
(defun |L|ENLAZAR (l_lista l_grupos)
	(cond
		((null l_lista) nil)
		((null l_grupos) l_lista)
		(T
			(cond
				((= (car l_grupos) 1)
					(cons (car l_lista) (|L|ENLAZAR (cdr l_lista) (cdr l_grupos)))
				)
				(T
					(setq l_divl (|L|DIVL l_lista (car l_grupos)))
					(cons (car l_divl) (|L|ENLAZAR (cadr l_divl) (cdr l_grupos)))
				)
			)
		)
	)
)

;agrupar datos de sublistas de una lista
;"|L|ENLAZSL"
(defun |L|ENLAZSL (l_lista l_grupos)
	(cond
		((null l_lista) nil)
		((null l_grupos) l_lista)
		((listp (car l_lista))
			(cons (|L|ENLAZAR (car l_lista) l_grupos) (|L|ENLAZSL (cdr l_lista) l_grupos))
		)
		(T (cons (car l_lista) (|L|ENLAZSL (cdr l_lista) l_grupos)))
	)
)

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;						O	R	D	E	N	A	R		L	I	S	T	A	S
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;comparar dos listas y devolver la que tenga algún elemento menor
;"|L|MENOR"
(defun |L|MENOR (l_list1 l_list2)
	(cond
		((or (null l_list1) (null l_list2)) nil)
		((equal l_list1 l_list2 g_tol) l_list1)
		((|L|ESMENOR (car l_list1) (car l_list2)) l_list1)
		((|L|ESMENOR (car l_list2) (car l_list1)) l_list2)
		(T (cons (car l_list1) (|L|MENOR (cdr l_list1) (cdr l_list2))))
	)
)

;comparar dos listas y devolver la que tenga algún elemento mayor
;"|L|MAYOR"
(defun |L|MAYOR (l_list1 l_list2)
	(cond
		((or (null l_list1) (null l_list2)) nil)
		((equal l_list1 l_list2 g_tol) l_list1)
		((|L|ESMAYOR (car l_list1) (car l_list2)) l_list1)
		((|L|ESMAYOR (car l_list2) (car l_list1)) l_list2)
		(T (cons (car l_list1) (|L|MAYOR (cdr l_list1) (cdr l_list2))))
	)
)

;función auxiliar para ordenar una lista
;"|L|MAXI"
(defun |L|MAXI (l_l)
	(cond
		((null l_l) nil)
		((null (cdr l_l)) (car l_l))
		((|L|ESMENOR (car l_l) (cadr l_l))
			(|L|MAXI (|L|BORRAT (car l_l) l_l))
		)
		((|L|ESMENOR (cadr l_l) (car l_l))
			(|L|MAXI (|L|BORRAT (cadr l_l) l_l))
		)
		(T
			(|L|MAXI (cdr l_l))
		)
	)
)

;agregar un elemento a una lista ordenada de menor a mayor
;"|L|ADD"
(defun |L|ADD (l_list l_el / l_divm l_sl1 l_sl2)
	(defun |L|COMP (l_l1 l_l2 l_sig)
		(cond
			((null l_l1) nil)
			((l_sig (car l_l1) (car l_l2)) T)
			((= (car l_l1) (car l_l2)) (|L|COMP (cdr l_l1) (cdr l_l2) l_sig))
			(T nil)
		)
	)

	(cond
		((null l_list) (list l_el))
		((|L|COMP l_el (car l_list) <) (cons l_el l_list))
		((|L|COMP l_el (last l_list) >) (append l_list (list l_el)))
		(T
			(setq l_divm (|L|DIVM l_list) l_sl1 (car l_divm) l_sl2 (cadr l_divm))
			(if (|L|COMP l_el (last l_sl1) >)
				(append l_sl1 (|L|ADD l_sl2 l_el))
				(append (|L|ADD l_sl1 l_el) l_sl2)
			)
		)
	)
)

;ordenar una lista a partir de sus máximos y mínimos (REVERSE LISTA)
;"|L|ORDMM"
(defun |L|ORDMM (l_lista l_orden / l_max l_min l_dm l_res)
	(setq l_dm (|L|MAX l_lista) l_max1 (car l_dm) l_lista (cadr l_dm)
	      l_dm (|L|MIN l_lista) l_min1 (car l_dm) l_lista (cadr l_dm)
	      l_dm (|L|MIN l_lista) l_min2 (car l_dm) l_lista (cadr l_dm)
	      l_dm (|L|MAX l_lista) l_max2 (car l_dm) l_lista (cadr l_dm)
	      l_res (list l_min1 l_min2 l_max2 l_max1)
	)
	(foreach l_el l_lista
		(setq l_res (|L|ADD l_res l_el))
	)
	(if (eq l_orden "A")
		l_res
		(reverse l_res)
	)
)

;ordenar una lista de mayor a menor
;"|L|ORDEN"
(defun |L|ORDEN (l_list)
	(cond
		((null l_list) nil)
		((= (length l_list) 1) l_list)
		(T
			(cons (|L|MAXI l_list) (|L|ORDEN (|L|BORRA (|L|MAXI l_list) l_list)))
		)
	)
)

;intercalar dos listas
;"|L|INTERC"
;;;(defun |L|INTERC (l_list1 l_list2)
;;;	(cond
;;;		((null l_list1) l_list2)
;;;		((null l_list2) l_list1)
;;;		((|L|ESMENOR (car l_list1) (car l_list2))
;;;			(cons (car l_list1) (|L|INTERC (cdr l_list1) l_list2))
;;;		)
;;;		(t (cons (car l_list2) (|L|INTERC l_list1 (cdr l_list2))))
;;;	)
;;;)

(defun |L|INTERC (l_list1 l_list2)
	(setq l_res '())
	(while l_list1
		(if l_list2
			(if (|L|ESMENOR (car l_list1) (car l_list2))
				(setq l_res (cons (car l_list1) l_res) l_list1 (cdr l_list1))
				(setq l_res (cons (car l_list2) l_res) l_list2 (cdr l_list2))
			)
			(setq l_res (cons (car l_list1) l_res) l_list1 (cdr l_list1))
		)
	)
	(append (reverse l_res) l_list2)
)

;ordenar una lista de menor a mayor
;"|L|ORDENAR"
(defun |L|ORDENAR (l_list)
	(cond
		((null l_list) nil)
		((= (length l_list) 1) l_list)
		((> (length l_list) 100)
			(setq l_divl (|L|DIVL l_list 100) l_ppl (car l_divl) l_spl (cadr l_divl))
			(|L|INTERC (|L|ORDENAR l_ppl) (|L|ORDENAR l_spl))
		)
		(T
			(setq l_divl1 (|L|DIVM l_list))
			(|L|INTERC (reverse (|L|ORDEN (car l_divl1))) (reverse (|L|ORDEN (cadr l_divl1))))
		)
	)
)

; ordenar una lista de sublistas por un elemento numérico específico de la sublista
;"|L|ORDPOS"
(defun |L|ORDPOS (l_lista l_pos)
	(cond
		((null l_lista) nil)
		(T (|L|PRIN (|L|ORDENAR (|L|NPRI l_lista l_pos)) l_pos)) 
	)
)

;devolver una lista de sublistas pasando el primer elemento de cada sublista al final de la misma
;"|L|PRIFIN"
(defun |L|PRIFIN (l_lista)
	(mapcar '(lambda (el) (append (cdr el) (list (car el)))) l_lista)
)

;devolver una lista de sublistas pasando el últmo elemento de cada sublista al principio de la misma
;"|L|FINPRI"
(defun |L|FINPRI (l_lista)
	(mapcar '(lambda (el) (cons (last el) (|L|BULT el))) l_lista)
)

;devolver una lista de sublistas pasando el n elemento de cada sublista al principio de la misma
;"|L|NPRI"
(defun |L|NPRI (l_lista l_n)
	(mapcar '(lambda (el) (cons (nth l_n el) (|L|BPOS l_n el))) l_lista)
)

;devolver una lista de sublistas pasando el primer elemento de cada sublista la posición n de la misma
;"|L|PRIN"
(defun |L|PRIN (l_lista l_n)
	(mapcar '(lambda (el) (|L|AGREGAP (cdr el) (car el) l_n)) l_lista)
)

;cambiar el orden de dos elementos de una lista
;"|L|CPOS"
(defun |L|CPOS (l_lista l_pos1 l_pos2)
	(cond
		((null l_lista) nil)
		((= l_pos1 l_pos2) l_lista)
		((> l_pos1 (length l_lista)) (|L|CPOS l_lista (length l_lista) l_pos2))
		((> l_pos2 (length l_lista)) (|L|CPOS l_lista l_pos1 (length l_lista)))
		((> l_pos1 l_pos2) (|L|CPOS l_lista l_pos2 l_pos1))
		(T
			(setq l_divl (|L|DIVL l_lista l_pos1)
			      l_sli (car l_divl) l_sl2 (cdadr l_divl)
			      l_divl (|L|DIVL l_sl2 (- l_pos2 (1+ l_pos1)))
			      l_slm (car l_divl) l_slf (cdadr l_divl)
			)
			(if (>= l_pos2 (length l_lista))
				(append l_sli l_slm (list (nth l_pos1 l_lista)))
				(append l_sli (list (nth l_pos2 l_lista)) l_slm (list (nth l_pos1 l_lista)) l_slf)
			)
		)
	)
)

;"|L|CLPOS"
(defun |L|CLPOS (l_lista l_pos)
	(cond
		((null l_lista) nil)
		((null l_pos) l_lista)
		((listp (car l_pos))
			(|L|CLPOS (|L|CLPOS l_lista (car l_pos)) (cdr l_pos))
		)
		(T (|L|CPOS l_lista (car l_pos) (cadr l_pos)))
	)
)

;cambiar el orden de dos elementos de cada sublista en una lista
;"|L|SLCPOS"
(defun |L|SLCPOS (l_lista l_pos)
	(cond
		((null l_lista) nil)
		(T (cons (|L|CLPOS (car l_lista) l_pos) (|L|SLCPOS (cdr l_lista) l_pos)))
	)
)

;unir dos listas eliminando de la primera lista las sublistas cuyo primer lemento sea mayor que el primer elemento de la primera sublista de la segunda lista
;"|L|UNIRLISTAS"
(defun |L|UNIRLISTAS (l_list1 l_list2 l_toler)
	(cond
		((null l_list2) l_list1)
		((null l_list1) l_list2)
		((listp (car l_list1))
			(append (car (|L|OBTSLV l_list1 (caar l_list1) (caar l_list2))) l_list2)
		)
		(T
			(append (car (|L|OBTSLV l_list1 (car l_list1) (car l_list2))) l_list2)
		)
	)
)

; ordenar una lista de sublistas por un elemento numérico específico de la sublista
;"|L|ORDLP"
(defun |L|ORDLP (l_lista l_pos)
	(defun |L|CLP (l_list l_cant)
		(setq l_n (1- l_cant) l_lc '())
		(repeat l_cant
			(if (not (member l_n l_list))
				(setq l_lc (cons l_n l_lc))
			)
			(setq l_n (1- l_n))  
		)
		(append l_list l_lc)
	)
	(defun |L|REVPOS (l_lpo)
		(setq l_pord (reverse (|L|ORDEN l_lpo)))
		(mapcar '(lambda (l_p) (|L|POS l_p l_lpo)) l_pord)
	)
	(defun |L|ORDSL (l_subl l_lpo)
		(mapcar '(lambda (l_ph) (nth l_ph l_subl)) l_lpo)
	)
	(cond
		((null l_lista) nil)
		((< (length l_pos) (length (car l_lista)))
			(|L|ORDLP l_lista (|L|CLP l_pos (length (car l_lista))))
		)
		((= (length l_lista) 2)
			(if (< (nth l_pos (car l_lista)) (nth l_pos (cadr l_lista)))
				l_lista
				(reverse l_lista)
			)
		)
		((listp l_pos)
			(mapcar '(lambda (l_el) (|L|ORDSL l_el (|L|REVPOS l_pos)))
				(|L|ORDENAR (mapcar '(lambda (l_el) (|L|ORDSL l_el l_pos)) l_lista))
			)
		)
		(T (mapcar '(lambda (l_el)
			    	(setq l_divl (|L|DIVL l_el (- (length l_el) l_pos)))
			    	(append (cadr l_divl) (car l_divl))
			    )
				(|L|ORDENAR (mapcar '(lambda (l_el)
						     	(setq l_divl (|L|DIVL l_el l_pos))
						     	(append (cadr l_divl) (car l_divl))
						      )
						    l_lista
					    )
				)
		   )
		)
	)
)

;invertir el orden de todas las sublistas de una lista
;"|L|INVSL"
(defun |L|INVSL (l_lista)
	(mapcar 'reverse l_lista)
)

;convertir todos los números enteros de una lista a reales
;"|L|IAR"
(defun |L|IAR (l_lista)
	(mapcar '(lambda (el) (* el 1.0)) l_lista)
)

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;		O	P	E	R	A	C	I	O	N	E	S		C	O	N	 	L	I	S	T	A	S 
;																							  
;							N	U	M	E	R	I	C	A	S								  
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;sumar todos los números de una lista
;"|L|SUMA"
(defun |L|SUMA (l_lista)
	(apply '+ l_lista)
)

;restar todos los números de una lista
;"|L|RESTA"
(defun |L|RESTA (l_lista)
	(apply '- l_lista)
)

;obtener el promedio de una lista de números
;"|L|PROM"
(defun |L|PROM (l_lista)
	(cond
		((null l_lista) nil)
		(T (/ (|L|SUMA l_lista) (length l_lista)))
	)
)

;promediar dos listas de valores
;"|L|PROM2L"
(defun |L|PROM2L (l_list1 l_list2)
	(cond
		((null l_list1) l_list2)
		((null l_list2) l_list1)
		(T
			(cons (/ (+ (car l_list1) (car l_list2)) 2) (|L|PROM2L (cdr n_list1) (cdr n_list2)))
		)
	)
)

;promediar las sublistas de una lista
;"|L|PROMSL"
(defun |L|PROMSL (l_lista)
	(cond
		((null l_lista) nil)
		((listp (car l_lista))
			(cons (|L|PROM (car l_lista)) (|L|PROMSL (cdr l_lista))) 
		)
		(T
			(cons (car l_lista) (|L|PROMSL (cdr l_lista)))
		)
	)
)

;promediar los valores en la misma posición de las sublistas de una lista
;"|L|PROMVSL"
(defun |L|PROMVSL (l_lista)
	(cond
		((null l_lista) nil)
		((listp (car l_lista))
			(cons (|L|PROM (mapcar 'car l_lista)) (|L|PROMVSL (mapcar 'cdr l_lista))) 
		)
		(T
			(cons (|L|PROM l_lista) (|L|PROMVSL nil))
		)
	)
)

	       
;sumar una constante a todos los elementos de una lista
;"|L|SUMAC"
(defun |L|SUMAC (l_lista l_const)
	(if (zerop l_const) l_lista (mapcar '(lambda (l_el) (+ l_el l_const)) l_lista))
)

;restar una constante a todos los elementos de una lista
;"|L|RESTAC"
(defun |L|RESTAC (l_lista l_const)
	(|L|SUMAC l_lista (- l_const))
)

;multiplicar todos los elementos de una lista por una constante
;"|L|MULTC"
(defun |L|MULTC (l_lista l_const)
	(mapcar '(lambda (l_el) (* l_el l_const)) l_lista)
)

;dividir todos los elementos de una lista por una constante
;"|L|DIVC"
(defun |L|DIVC (l_lista l_const)
	(if (zerop l_const) nil
		(|L|MULTC l_lista (/ 1 l_const))
	)
)

;numerar una lista de elementos
;"|L|NUMERAR"
(defun |L|NUMERAR (l_lista l_pref l_un)
	(cond
		((null l_lista) nil)
		((null l_pref) (cons (cons l_un (car l_lista)) (|L|NUMERAR (cdr l_lista) l_pref (1+ l_un)))) 
		(T (cons (cons (strcat l_pref (itoa l_un)) (car l_lista)) (|L|NUMERAR (cdr l_lista) l_pref (1+ l_un)))) 
	)
)

;negativo del primer elemento de una sublista
;"|L|NEGATIVO"
(defun |L|NEGATIVO (l_lista)
	(mapcar '(lambda (el) (cons (- (car el)) (cdr el))) l_lista)
)

;incrementar el elemento n de una lista
;"|L|INCN"
(defun |L|INCN (l_lista l_n l_inc)
	(cond
		((null l_lista) nil)
		((<= (length l_lista) l_n) (|L|INCN (|L|COMPLETAR l_lista l_n 0) l_n l_inc))
		(T
			(|L|REEMPOS l_n (+ (nth l_n l_lista) l_inc) l_lista)
		)
	)
)

;incermentar todos los elementos n de las sublistas de una lista
;"|L|INCNS"
(defun |L|INCNS (l_lista l_n l_inc)
	(cond
		((null l_lista) nil)
		(T
			(cons (|L|INCN (car l_lista) l_n l_inc) (|L|INCNS (cdr l_lista) l_n l_inc))
		)
	)
)

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;		L	I	S	T	A	S		D	E		E	N	T	I	D	A	D	E	S		S	S
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Devolver un conjunto ssget como una lista de nombres de entidades
;"|L|SSL"
(defun |L|SSL (l_ss)
	(cond
		((null l_ss) nil)
		((null (ssname l_ss 0)) nil)
		(t
			(cons (ssname l_ss 0) (|L|SSL (ssdel (ssname l_ss 0) l_ss)))
		)
	)
)

