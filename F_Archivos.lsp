(defun |F|WAIT ()
	(repeat 1000
		(setq f_c 0)
		(repeat 1000
			(setq f_c (1+ f_c))
		)
	)
)

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;										C	A	R	P	E	T	A	S
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;"|F|NOMCARP"
(defun |F|NOMCARP (f_nombl)
  	(while (wcmatch f_nombl "*\\*")
		(setq f_nombl (cadr (|T|SUBCH f_nombl "\\")))
	)
	f_nombl
)

;crear una carpeta nueva
;"|F|NDIR"
(defun |F|NDIR (f_carp)
	(cond
		((not (findfile f_carp))
			(setq f_carpc (strcat "\"" f_carp "\""))
			(startapp "cmd /Q /D /C mkdir" f_carpc)
			(|F|WAIT)
			f_carp
		)
		(T f_carp)
	)
)

;"|F|LPATH"
(defun |F|LPATH (f_camino)
	(|T|LISDATOS f_camino "\\")
)

;"|F|PATHEX"
(defun |F|PATHEX (f_camtot)
	(cond
		((findfile (|T|BORRAU f_camtot)) f_camtot)
		(T (|F|PATHEX (strcat (|T|CONCATCH (|L|BULT (|F|LPATH f_camtot)) "\\") "\\")))
	)
)

;"|F|PPATH"
(defun |F|PPATH (f_camin)
	(car (|T|SUBCH (substr f_camin (+ (strlen (|F|PATHEX f_camin)) 1)) "\\"))
)

;"|F|RPATH"
(defun |F|RPATH (f_camin)
	(substr f_camin (+ (strlen (|F|PATHEX f_camin)) 1))
)

;"|F|CPATH"
(defun |F|CPATH (f_camino)
	(cond
		((equal (|F|PATHEX f_camino) f_camino) "")
		(T
			(|F|CPATH (strcat (|F|NDIR (strcat (|F|PATHEX f_camino) (|F|PPATH f_camino))) "\\" (|F|RPATH f_camino)))
		)
	)
)

;"|F|CDIR"
(defun |F|CDIR (f_camino f_carpeta)
	(cond
		((null f_camino)
			(|F|CDIR g_path f_carpeta)
		)
		((null f_carpeta)
			(|F|CPATH f_camino)
		)
		((findfile (|T|BORRAU f_camino))
			(|F|NDIR (strcat f_camino f_carpeta))
		)
		(T
			(|F|CPATH f_camino)
			(|F|NDIR (strcat f_camino f_carpeta))
		)
	)
)

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;										A	R	C	H	I	V	O	S
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;"|F|NOMBREA"
(defun |F|NOMBREA (f_nombl)
  	(while (wcmatch f_nombl "*\\*")
		(setq f_nombl (cadr (|T|SUBCH f_nombl "\\")))
	)
	(car (|T|SUBCH f_nombl "."))
)

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;								L	E	E	R		A	R	C	H	I	V	O	S
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;listar lineas del archivo
;"|F|LISTLIN"
(defun |F|LISTLIN (f_fn f_sep / f_lin)
	(cond	((setq f_lin (read-line f_fn))
			(cons (|T|LISDATOS f_lin f_sep) (|F|LISTLIN f_fn f_sep))
		)
		(T (close f_fn))
	)
)

;Leer datos de un archivo de texto
;"|F|LEER"
(defun |F|LEER (f_cbus f_nomar f_ext f_sep f_enc f_msg)
	(if (null f_nomar)
		(setq f_fname (getfiled f_msg f_cbus f_ext 2))
		(setq f_fname (strcat f_cbus f_nomar "." f_ext))
	)
	(cond
		((null (findfile f_fname)) nil)
		(T
			(setq f_fnam (open f_fname "r")
				f_list (|F|LISTLIN f_fnam f_sep)
			)
			(if f_enc
				(list (|L|SUBL f_list f_enc) (|L|RESL f_list f_enc))
				f_list
			)
		)
	)
)

;"|F|LEECSV"
(defun |F|LEECSV (f_cbus f_nomar f_enc f_msg)
	(|F|LEER f_cbus f_nomar "csv" "," f_enc f_msg)
)

;Leer datos de un archivo de texto correspondiente a un alineamiento
;"|F|LEECSVA"
(defun |F|LEECSVA (f_nomal f_nomar f_enc)
	(if f_nomal
		(|F|LEECSV (strcat g_alipath f_nomal "\\") f_nomar f_enc nil)
		nil
	)
)

;Leer datos del eje de un archivo de texto
;"|F|LEEALI"
(defun |F|LEEALI (f_nomal / f_lobj f_sl)
	(|L|ENLAZSL (|F|LEECSVA f_nomal "Eje" nil) (list 2 2))
)

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;							G	U	A	R	D	A	R		A	R	C	H	I	V	O	S
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;escrbir una linea de texto en un archivo
;"|F|WRITELIST"
(defun |F|WRITELIST (f_lt f_sep f_fw)
	(cond
		((null f_lt) nil)
		(T
			(write-line (|T|CONCATCH (car f_lt) f_sep) f_fw)
			(|F|WRITELIST (cdr f_lt) f_sep f_fw)
		)
	)
)

;guardar datos en un archivo de texto separados por un caracter específico
;"|F|GUARDAR"
(defun |F|GUARDAR (f_cam f_nomar f_ext f_tit f_datos f_sep f_sob)
	(cond
		((not (findfile (|T|BORRAU f_cam)))
			(|F|CDIR f_cam nil)
			(|F|GUARDAR f_cam f_nomar f_ext f_tit f_datos f_sep f_sob)
		)
		((not (findfile (strcat f_cam f_nomar "." f_ext)))
			(setq f_fw (open (strcat f_cam f_nomar "." f_ext) "w"))
			(if f_tit (|F|WRITELIST f_tit f_sep f_fw))
			(setq f_divl (|L|DIVL f_datos 5000) l_ppd (car f_divl) f_datos (cadr f_divl))
			(|F|WRITELIST l_ppd f_sep f_fw) 
			(close f_fw)
			(if f_datos
				(|F|GUARDAR f_cam f_nomar f_ext f_tit f_datos f_sep nil)
			)
		)
		(T 	(if f_sob
				(setq f_fw (open (strcat f_cam f_nomar "." f_ext) "w"))
				(setq f_fw (open (strcat f_cam f_nomar "." f_ext) "a") f_tit nil)
			)
			(if f_tit (|F|WRITELIST f_tit f_sep f_fw))
			(setq f_divl (|L|DIVL f_datos 5000) l_ppd (car f_divl) f_datos (cadr f_divl))
			(|F|WRITELIST l_ppd f_sep f_fw) 
			(close f_fw)
			(if f_datos
				(|F|GUARDAR f_cam f_nomar f_ext f_tit f_datos f_sep nil)
			)
		)
	)
)

;guardar datos en un archivo de texto separado por comas
;"|F|GUARDACSV"
(defun |F|GUARDACSV (f_cam f_nomar f_tit f_lista f_sob)
	(|F|GUARDAR f_cam f_nomar "csv" f_tit f_lista "," f_sob)
)

;guardar datos de un archivo de texto correspondiente a un alineamiento
;"|F|GUARDACSVA"
(defun |F|GUARDACSVA (f_nomal f_nomar f_tit f_lista f_sob)
	(|F|GUARDACSV (strcat g_alipath f_nomal "\\") f_nomar f_tit f_lista f_sob)
)

;guardar en archivo los datos de un alineamiento
;"|F|GUARDAALI"
(defun |F|GUARDAALI (f_nomali f_lalin f_sob)
	(|F|GUARDACSVA f_nomali "Eje" nil f_lalin f_sob)
	(setq g_alact (list f_nomali f_lalin))
)
