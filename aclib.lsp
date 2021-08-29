 ;========
;========================
;;String manipulation
;========================
;;Ref : https://www.cadtutor.net/forum/topic/66221-how-to-split-a-string-by-character/
(defun Jef!:splitstr (delim str / nxt ep ret)
  (setq nxt (1+ (strlen delim)))
  (while (setq ep (vl-string-search delim str))
    (setq ret (cons (substr str 1 ep)ret))
    (setq str (substr str (+ nxt (strlen (car ret))) (strlen str)))
  )
  (reverse(cons str ret))
);Jef!:splitstr
;========
;;Split string by specified character
;;Ref : https://www.cadtutor.net/forum/topic/66221-how-to-split-a-string-by-character/
(defun split ( s d )
  ( (lambda (f) (if (/= "" d) (f s d "" nil)))
    (lambda ( s d c b / k )
      (if (/= s "")
        (if (= d (setq k (substr s 1 1))) 
          (append 
            (cond 
              ( (/= c "") (list c d) )
              ( (list d) )
            ) 
            (f (substr s 2) d "" t)
          )
          (f (substr s 2) d (strcat c k) b)
        )
        (if b (list c) c)
      )
    )
  )
);split
;========
;;To parse string into list by specified character
;;Usage : (parsestr string delimiter)
;;Ex : (parsestr "A        249901.5131    223780.0329    50.44" " ")
;;Return : '("A" "249901.5131" "223780.0329" "50.44")
(defun parsestr (s d)
  (setq s (vl-string-trim d s))
  (setq s (split s d))
  ;(setq s (Jef!:splitstr d s))
  (vl-remove d s)
);parsestr
 ;| Block comment
 (setq str1 "  A    123.45    789.87   XYZ    345  ")
 (setq str2 " B    ,   322.555  ,  945.687 ,  FGH  478.43  ")
 (parsestr str1 " ")
 (parsestr str2 ",")
 |;
;========
;========================
;;Geometry computation
;========================	 
;;To calculate average value
(defun avg (a b / c)
   (setq c (/ (+ a b) 2.0))
);avg
;========
;;To calculate middle of 2 points
(defun midpt (p1 p2 / mp)
   (setq mp (mapcar 'avg p1 p2))
);midpt
;========
;;To calculate minimum of dX,dY
(defun mindXY(pta ptb / dxy)
   (setq dxy (mapcar '- ptb pta))
   (setq dxy (mapcar 'abs dxy))
   (eval (cons 'min (reverse (cdr (reverse dxy)))))
);mindXY
;========
;;Return list of vertex co-ordinate by specified entity name of Polyline
 (defun getvertex(en / co_list)
         (setq co_list (vl-remove-if 'not
				         (mapcar
					       (function (lambda(p)
                                (if (= 10 (car p))
								  (cdr p)
							    );if
							  );lambda
					       );func
					       (entget en)
				         );mapcar
                       );vl-remove-if
         );setq
 );getvertex
 ;========
 ;;Return list of vertex co-ordinate XYZ by specified entity name of LWPolyline
 (defun getvertex_xyz(en / co_list)
         (setq co_list (vl-remove-if 'not
              (mapcar
                 (function (lambda(p / ptx sspt)
                               (if (= 10 (car p))
									(progn 
										(setq ptx (cdr p))
										;(setq sspt (ssget "C" ptx ptx (list (cons 0 "INSERT"))))
										(setq sspt (ssget "C" ptx ptx (list (cons 0 "VERTEX"))))
                                        (if sspt
											(cdr (assoc 10 (entget (ssname sspt 0))))
											(progn (princ "\nELEV. not found @") (princ ptx) (exit)) 
										);if
									);progn
								);if
							);lambda
				);func
                (entget en)
			   );mapcar
            );vl-remove-if
         );setq
 );getvertex_xyz
 
;========
;; Polygon Centroid  -  Lee Mac
;; Returns the WCS Centroid of an LWPolyline Polygon Entity
;; Ref: http://www.lee-mac.com/polygoncentroid.html
(defun LM:PolyCentroid ( e / l )
    (foreach x (setq e (entget e))
        (if (= 10 (car x)) (setq l (cons (cdr x) l)))
    )
    (
        (lambda ( a )
            (if (not (equal 0.0 a 1e-8))
                (trans
                    (mapcar '/
                        (apply 'mapcar
                            (cons '+
                                (mapcar
                                    (function
                                        (lambda ( a b )
                                            (
                                                (lambda ( m )
                                                    (mapcar
                                                        (function
                                                            (lambda ( c d ) (* (+ c d) m))
                                                        )
                                                        a b
                                                    )
                                                )
                                                (- (* (car a) (cadr b)) (* (car b) (cadr a)))
                                            )
                                        )
                                    )
                                    l (cons (last l) l)
                                )
                            )
                        )
                        (list a a)
                    )
                    (cdr (assoc 210 e)) 0
                )
            )
        )
        (* 3.0
            (apply '+
                (mapcar
                    (function
                        (lambda ( a b )
                            (- (* (car a) (cadr b)) (* (car b) (cadr a)))
                        )
                    )
                    l (cons (last l) l)
                )
            )
        )
    )
);LM:PolyCentroid

;========================
;;User input
;========================
;;Get points from CAD windows
(defun getpts(/ pickp ptlist pt)
  (setq pickp T)
  (setq ptlist '())
  (setq pt '())
  (while pickp
     (if pt
       (setq pickp (getpoint pt "\nPick point to draw <ENTER to end>:"))
       (setq pickp (getpoint "\nPick point to draw <ENTER to end>:"))
	 );if
	 (if pickp
	    (progn 
	      (if pt (grdraw pt pickp 1 1))
	      (setq ptlist (append ptlist (list pickp))
        		pt pickp)
		);progn
	 );if
  );while
  ptlist                   ;Return list of points
);getpts

;========
;; Get angle from user in format DD.MMSSS
;; if MM or SS greater than 60, function will request input again
;; if Ok function return DD.MMSSS
(defun getdms(/ kok d dd mm ss r)
  (setq kok nil)
  (while (not kok)
    (initget 1)
    (setq d (getreal "\nInput angle <DD.MMSSS>:"))
    (setq dd (fix d)
          mm (fix (* (- d dd) 100.0))
	      ;ss (- (* d 10000.00000000001) (* dd 10000.0) (* mm 100.0))
		  ss (- (* d 10000.0) (* dd 10000.0) (* mm 100.0))
          ss (* ss 1.00000001)
    )
	(if (or (>= mm 60.0)(>= ss 60.0))
	  (progn
	    ;(setq kok nil)
		(if (>= mm 60.0)
		  (princ "\n>>Minute must less than 60")
		);if
		(if (>= ss 60.0)
		  (princ "\n>>>>Second must less than 60");
		)
	  );progn
	  (progn
        (setq r d)	  
	    ;(setq r (+ dd (/ mm 100.0) (/ ss 10000.0)))
		(setq kok T)
	  );progn
	);if
   );while
   r
);end getdms

;========================
;;Unit conversion
;========================
;;To calculate area in Rai-Ngan-Wa
(defun rnw (sqm / ret rr r ng wa)
   (setq wa (/ sqm 4.0))
   (setq rr (/ wa 400)
         r (fix rr)
         ng (fix (* (- rr r) 4))
		 wa (- wa (* r 400) (* ng 100))
   )
   (setq ret (strcat (itoa r) "-" (itoa ng) "-" (rtos wa 2 2)))
);rnw
;========
;; To converse DD.DDDDD to DD-MM-SS.SS
(defun dms (d / dd mm ss r)
  (setq dd (fix d)
        mm (fix (* (- d dd) 60.0000001))
	    ss (* (- d dd (/ mm 60.0)) 3600.0) 
  )
  (setq r (strcat (itoa dd) "-" 
				  (if (< mm 10 ) (strcat "0" (itoa mm)) (itoa mm)) "-" 
				  (if (< ss 10) (strcat "0" (rtos ss 2 2)) (rtos ss 2 2))))
);def

;; To converse DD.MMSSS to DD.DDDDD
(defun deg (d / dd mm ss r)
  (setq dd (fix d)
        mm (fix (* (- d dd) 100.0000001))
	    ss (* (- d dd (/ mm 100.0)) 10000.0)
  )
  (setq r (+ dd (/ mm 60.0) (/ ss 3600.0)))
);def

;; To converse DD to Radian
(defun d2r (d / r)
  (setq r (/ (* d pi) 180.0))
);def

;; To converse Radian to DD
(defun r2d (r / d)
  (setq d (/ (* r 180.0) pi))
);def
;========================
;;Property manipulation
;========================
;;To change property of entity by given ename code & value
;;code 62 for color
(defun chgprop(en code val / elist old_val new_val)
  (if (/= (type en) 'ENAME)
      (progn 
	    (princ "\nFirst argument must be ENAME")
        (exit)
	  );progn
  );if
  (setq elist (entget en))
  (setq old_val (assoc code elist)
        new_val (cons code val))
  (if (or old_val (equal code 62))
	(progn
	  (if (equal code 62)    ;if the color of entity is bylayer "code 62 not exist"
		(setq elist (append elist (list new_val)))    ;For color code=62
		;else
		(setq elist (subst new_val old_val elist))    ;change old to new
	  );if
	  (entmod elist)    ;return elist if success
	);progn
	nil                 ;return nil if fail
   );if
   en   
);chgprop
;========
;; To filter selection set by specified code & code value
(defun sscode (si code cval / ss i en elist)
  (if (equal (type si) 'PICKSET)
    (progn
       (setq i 0)
       (setq ss (ssadd))
       (repeat (sslength si)
          (setq en (ssname si i)
	          elist (entget en))
          (if (equal (cdr (assoc code elist)) cval)
	     (setq ss (ssadd en ss))
	  );filter si by cval 
	  (setq i (+ i 1))
	  ;(princ i)
       );rep
    );progn
    (setq ss (ssget "X" (list (cons code cval))))
  );if
  ss   ;;return selection set [ss] with code=cval 
);sscode
;========

;; To change selection set property by given code & code value
(defun chg_ss_bycode (si code cval / ss i en elist)
  (if (equal (type si) 'PICKSET)
    (progn
       (setq i 0 ss (ssadd))
       (repeat (sslength si)
          (setq en (ssname si i)
	          elist (entget en))
          (if (or (assoc code elist) (equal code 62))
			(progn
			  (setq elist (subst (cons code cval) (assoc code elist) elist))
			  (if (null (assoc code elist))
			     (setq elist (append elist (list (cons code cval)))) ;For color code=62
				 ;if the color of entity is bylayer "code 62 not exist"
			  )
	          (entmod elist)
	          (setq ss (ssadd en ss))
			);progn
		  );filter si by code 
		  (setq i (+ i 1))
		  ;(princ i)
       );rep
    );progn
    (princ "1st argument have to be PICKSET")
  );if
  ss   ;;return selection set [ss] which changing code=cval 
);def
;========

;========================
;;Entity creation
;========================
;;To make 3D Polyline by specifed point list and Layer name, then return entity name of 3D Polyline
(defun make_3dpolyline (ptlist lay / pt ent 3D_lay)
  ;(setq 3D_lay "3D_BR-LINE")
  (if lay (setq 3D_lay lay)
          (setq 3D_lay "3D_BR-LINE")
  )
  (entmake (list '(0 . "POLYLINE")
                 '(100 . "AcDbEntity")
                 '(100 . "AcDb3dPolyline")
                 '(70 . 8)
		          (cons 8 3D_lay)
            );list
  );entmake

  (foreach pt ptlist
    (entmake (list '(0 . "VERTEX")
                   '(100 . "AcDb3dPolylineVertex")
                    (cons 10 pt)
                   '(70 . 32)
		            (cons 8 3D_lay)
              );list
    );entmake
  ;);repeat
  );foreach
  (entmake (list '(0 . "SEQEND") (cons 8 3D_lay)))
  (entlast)
);make_3dpolyline
;========
;;To create line by spectify point1 & point2
(defun make-line(pt1 pt2 lay / ent L_lay)
  (if lay (setq L_lay lay)
          (setq L_lay "LINE_Layer")
  )
  (entmake (list '(0 . "LINE")
                 '(100 . "AcDbEntity")
                 '(100 . "AcDbLine")
				 (cons 10 pt1)
				 (cons 11 pt2)
				 (cons 8 L_lay)
            );list
  );entmake
  (setq ent (entlast))
);make-line
;========
;;To create point by spectify location
(defun make-point(pt lay / ent L_lay)
  (if lay (setq L_lay lay)
          (setq L_lay "POINT_Layer")
  )
  (entmake (list '(0 . "POINT")
                 '(100 . "AcDbEntity")
                 '(100 . "AcDbPoint")
				 (cons 10 pt)
				 (cons 8 L_lay)
            );list
  );entmake
  (setq ent (entlast))
);make-point
;========
;;To Define function to create Mtext entity by specified Name, Coordinate, Layer, Height & BlockWidth
(defun pmtxt (txt txt_co txt_lay txt_ht txt_bw)    ;Function pmtxt with 5 parameters
		(entmake (list '(0 . "MTEXT")
                       '(100 . "AcDbEntity")
                       '(100 . "AcDbMText")
						(cons 1 txt)
                        (cons 10 txt_co)
						(cons 8 txt_lay)
						(cons 40 txt_ht)        ;MText height
						(cons 41 txt_bw)        ;MText block width
						;(cons 50 0.0)
						(cons 71 4)             ;middle left justification
                 );list
         );entmake
		 (entlast)   ;Return entity name of last entity
);pmtxt	 
;========
;;To Define function to create text entity by specified Name, Coordinate, Layer, Height
(defun ptxt (pt_name pt_co pt_lay pt_ht)    ;Function ptxt with 4 parameters
		(entmake (list '(0 . "TEXT")
                       '(100 . "AcDbEntity")
                       '(100 . "AcDbText")
						(cons 1 pt_name)
                        (cons 10 pt_co)
						(cons 8 pt_lay)
						(cons 40 pt_ht)         ;Text height
						;(cons 50 0.0)
                 );list
         );entmake
		 (entlast)   ;Return entity name of last entity
);ptxt

;========================
;;Miscellaneous
;========================
;;To sort co-ordinate list by x value
;;Ex: (setq ptlist '((4 5) (2 4) (1 5) (7 8)))
;;    (sort_x ptlist) = '((1 5) (2 4) (4 5) (7 8))
(defun sort_x (l)
  (vl-sort l '(lambda ( a b ) (< (car a) (car b))))
);sort_x

;;Sorting by X value for list of '("code" (X Y Z))
(defun sort_x_rtk (l)
  (vl-sort l '(lambda ( a b ) (< (car (cadr a)) (car (cadr b)))))
);sort_x

;;Add space before string by specified width
(defun spacestr (str len)
  (while (< (strlen str) len)
    (setq str (strcat " " str))
  );while
  str
);def
