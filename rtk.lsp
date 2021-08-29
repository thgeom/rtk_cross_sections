;;To sort co-ordinate list by x value
;;Ex: (setq ptlist '((4 5) (2 4) (1 5) (7 8)))
;;    (sort_x ptlist) = '((1 5) (2 4) (4 5) (7 8))
(defun sort_x (l)
  (vl-sort l '(lambda ( a b ) (< (car a) (car b))))
);sort_x

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

;;Transform plist from WCS to X-section
(defun trans_wcs2xs (plist / pt ptu xs_list)
  (setq xs_list '())
  (foreach pt plist
     (setq ptu (trans pt 0 1))
	 (setq xs_list (append  xs_list (list ptu)))
  )
  xs_list
);def

;;Transform plist X-section to WCS
(defun trans_xs2wcs (plist / pt ptw wcs_list)
  (setq wcs_list '())
  (foreach pt plist
     (setq ptw (trans pt 1 0))
	 (setq wcs_list (append  wcs_list (list ptw)))
  )
  wcs_list
);def


;;Read RTK record in csv & store in rtk_pts list
(defun rtk2pts(fname fmode / f eof i rtk_rec pt)
  (setq f (open fname fmode))   ;open file for reading
  (setq eof nil
        rtk_pts '()
        i 0)
  (princ "\nProcessing ...")
  (read-line f)                                      ;Read file heading
  (while (not eof)
     (setq rtk_rec (read-line f))                    ;Read rtk record from file
	 (if rtk_rec
	    (progn
           (setq pt (parsestr rtk_rec ","))
		   (setq pt (list (nth 0 pt) (nth 1 pt) (nth 2 pt) (nth 3 pt) (nth 4 pt))
		         pt (list (nth 0 pt) (nth 1 pt) (list (atof (nth 3 pt)) (atof (nth 2 pt)) (atof (nth 4 pt)))))
           (setq rtk_pts (append rtk_pts (list pt)))
		   (princ ".")
		   (setq i (1+ i))
		);progn
		(setq eof T)
	 );if
  );while
  (close f)
  (princ (strcat "\nTotal RTK-point = " (itoa i) " imported."))
  rtk_pts  
);def

;;Read coordinate from csv file and draw texts
(defun rtk2dwg(/ i pname pcode ploc ptx ocmd frtk)
  (setq ocmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setvar "PDMODE" 2)                                             ;set point style
  (setq frtk "d:/TGA_Lisp/RTK_X-sec.csv")                         ;FileName of RTK file
  (setq rtk_pts (rtk2pts frtk "r"))                               ;call function read from file & store into List
  (setq i 0)
  (foreach ptx rtk_pts
      (setq pname (cadr ptx)
	        pcode (car ptx)
			ploc (caddr ptx)
	  )
	  (make-point ploc "XS")                                        ;put point @ploc
      (ptxt pname ploc "XS" 0.65)                                   ;put pname @ploc
	  (chgprop (ptxt pcode ploc "XS_Code" 0.9) 50 (* pi 0.45))      ;put pcode @ploc, rotation 90
	  (setq i (1+ i))
  );foraech
  (princ (strcat "\nTotal RTK-point = " (itoa i) " in drawing."))
  (setvar "CMDECHO" ocmd)   
  (princ)
);c:rtk2dwg

(defun c:rtk2d()
  (rtk2dwg)
);def
;;==== 

;;Get points by specified line & buffer distance
(defun getpts_by_xbuf (en buf ptc / xl_list p1 p2 xl_ang xl_perp_xl_buf p11 p12 p21 p22 s1 s2 i pt_list xs_list ent elist pti elev ptix pt_ol)
   (setq xl_list (entget en))
   (setq p1 (cdr (assoc 10 xl_list))
         p2 (cdr (assoc 11 xl_list))
		 p1 (list (car p1) (cadr p1))
		 p2 (list (car p2) (cadr p2))
		 ptc (list (car ptc) (cadr ptc))
	)
   (setq xl_ang (angle p1 p2))                       ;cal angle of line
   (setq XS_ang xl_ang)
   (setq xl_perp (- xl_ang (* pi 0.5)))              ;cal perpendicular angle of line
   (setq pti (polar ptc xl_perp buf))
   (setq ptc (inters ptc pti p1 p2 nil))             ;compute center point on the X-section line
   (setq xl_buf buf)                                 ;buffer distance from define X-section line
   (setq p11 (polar p1 (+ xl_ang (* pi 0.5)) xl_buf) ;compute boundary for selection
         p12 (polar p1 (- xl_ang (* pi 0.5)) xl_buf)
		 p21 (polar p2 (+ xl_ang (* pi 0.5)) xl_buf)
		 p22 (polar p2 (- xl_ang (* pi 0.5)) xl_buf)
	)
	(command "ZOOM" "O" en "")                  ;zoom to X-section line
	(setq xl_wp (list p11 p12 p22 p21))         ;define list of window polygon
	(setq s1 (ssget "WP" xl_wp))                ;select all entities with in polygon boundary
	(setq s2 (sscode s1 8 "XS_Code"))           ;filter entities Layer="XS", to be changed according to spec.
	(setq s2 (sscode s2 0 "TEXT"))
	(command "ZOOM" "W" p11 p22)                ;zoom to define area
	(setq i 0
	      pt_list '()
	      xs_list '())
	(repeat (sslength s2)
	   (setq ent (ssname s2 i))
	   (setq elist (entget ent))
	   (setq pti (cdr (assoc 10 elist))
	         elev (last pti)
	         pti (list (car pti) (cadr pti))
			 pcode (cdr (assoc 1 elist))
	   )
	   (setq ptix (polar pti xl_perp buf))                ;compute temporary point 
	   (setq pt_ol (inters pti ptix p1 p2 nil))           ;compute the perpendicular point on the X-section line
	   (setq pt_xy pt_ol)                                 ;pt_xy is point in 2D
	   (setq pt_ol (list (car pt_ol) (cadr pt_ol) elev))  ;set pt_ol = '(x y z)
	   (setq pt_list (append pt_list (list (list pcode pt_ol))))
	   
	   (setq ofs (distance ptc pt_xy))
	   (setq ang (abs (- (angle ptc pt_xy) XS_ang)))      ;checking angle pt to center, to define offset sign
       (if (and (> ang (* pi 0.5)) (< ang (* pi 1.5))) (setq ofs (- 0 ofs)))
	   (setq ofs_elev (list ofs elev))
	   (setq xs_list (append xs_list (list (list pcode ofs_elev))))
	   (setq i (1+ i))
	);rep
	(setq xs_list (sort_x_rtk xs_list))
	(setq pt_list (sort_x_rtk pt_list))
	(if (and (> XS_ang (* pi 0.5)) (< XS_ang (* pi 1.5))) (setq pt_list (reverse pt_list))) ;reverse sorting
	(list s2 pt_list xs_list)        ;return list of selection set, (x y z) & (offset elev)
);defun
;;====
;;Write X-section points to file by giving Chainage, PointList & FileName
(defun write-xsec (CHN plist fname fmode / ptstr j fo)
	(setq fo (open fname fmode))
	(write-line CHN fo)
	(setq j 0)
	(foreach pt plist
	  (setq pcode (car pt)
	        pt (cadr pt)
			;pcode (MT:Conv:Unichar->Char pcode)
	  )
	  (setq ptstr (strcat (rtos (car pt) 2 3) "  " (rtos (cadr pt) 2 3) "  " (rtos (caddr pt) 2 3) "  " pcode)) ;code of point
	  (write-line ptstr fo)
	  ;(make-point pt "XS_Point")                      ;plot point on X-section line for checking
	  (setq j (1+ j))
	);for
	(write-line "#" fo)
	(close fo)
	(princ (strcat "\nChainage " CHN " = " (itoa j) " xyz points ok."))
);def
;;====
;;Write x-section to text file by giving Chainage, PointList & FileName
(defun xs2file(CHN plist fname fmode / j np pti elev ofs elev ofs_str fo)
  (setq fo (open fname fmode))
  (setq j 0)
  (setq np (length plist))
  (repeat np
	 (setq pti (nth j plist))
	 (setq pcode (car pti)
	       pti (cadr pti)
	 )
     (setq ofs (car pti)
	       elev (cadr pti))
	 (setq ofs_str (strcat (spacestr (rtos ofs 2 0) 12) (spacestr (rtos elev 2 3) 12) "  " pcode))
	 (if (= j 0) (princ (spacestr CHN 10) fo)
	             (princ (spacestr " " 12) fo)
	 );
	 (write-line ofs_str fo)
	 (setq j (1+ j))
  );rep
  (close fo)
  (princ (strcat "\nChainage " CHN " = " (itoa np) " Offset-Elev. points ok."))
);def
;;====

;;Select X-section line, Define chainage & Pick center line
;;Buffer is distance from X-section line to define boundary of selection points
(defun coor2xs(/ ocmd buffer fxyz fofs xl_en cpt CHN ofs_pts xyz_pts)
  (setq buffer 10.0)                                     ;set buffer distance
  (setq fxyz "d:/TGA_Lisp/xsec_data-4.csv")              ;FileName for XYZ output
  (setq fofs "d:/TGA_Lisp/xsec-4.csv")                   ;FileName for Offset&Eleveation output
  (setq ocmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "UCS" "W")
  (setq xl_en (car (entsel "\nSelect X-section line :")))
  (initget (+ 1 2 4))
  (setq cpt (getpoint "\nPick point of Center line :"))
  (setq CHN (getstring "\nChainage or Name of X-section :"))
  (setq ss_pts (getpts_by_xbuf xl_en buffer cpt))                 ;call function to select RTK points
                                                                  ;ss_pts = list of selection set, xyz & offset_elevtion
  (setq ofs_pts (caddr ss_pts))   
  (xs2file CHN ofs_pts fofs "a")                                  ;call function for writing offset&elev.
  (setq xyz_pts (cadr ss_pts))
  (write-xsec CHN xyz_pts fxyz "a")                               ;call function for writing xyz
  (chg_ss_bycode (car ss_pts) 62 5)                               ;change color ss to 5 (already wrote to file)
  ;(chg_ss_bycode (sscode 0 8 "XS_Point") 62 5)                    ;change point in "XS_Point" layer to 5
  (chgprop xl_en 8 "XS_Line_Completed")                           ;change X-section line to layer "XS_line_Completed"
  (setvar "CMDECHO" ocmd)
  (princ)
);def
;;====
(defun c:c2x()
  (coor2xs)
);def
;;===========================


(defun xdata(appname)
   (if  (tblsearch "appid" appname)                          ; Checks if already registered.
       (princ (strcat "\n" appname " already registered. "))
       (if (=  (regapp appname) nil)                         ; Some other problem.
           (princ (strcat "\nCan't register XDATA for " appname ". "))
       )
   )
);def
;;====
;;To attach XDATA to an entity
(defun xl_attxdata(XL_en CHN_hent CPT_hent appname / CHN XL_xdata XL_list)
  (setq CHN (cdr (assoc 1 (entget (handent CHN_hent)))))
  (setq XL_xdata (list (list -3 (list appname (cons 1000 CHN)(cons 1005 CHN_hent) (cons 1005 CPT_hent)))))
  (setq XL_list (entget XL_en))
  (setq XL_list (append XL_list XL_xdata))
  (entmod XL_list)
);def
;;====
;;To create XDATA attach to X-section line
(defun c:cxline(/ xl_en CPT_en CPT_hent CHN xl_list p1 p2 Xl_ang CHN_en)
  (setq appname "RTK_XS")
  (if (not (tblsearch "appid" appname)) (regapp appname))     ;Xdata registeration

  (initget (+ 1 2 4))  
  (setq xl_en (car (entsel "\nSelect X-section line :")))
  (setq CPT_en (car (entsel "\nSelect point of Center line :")))
  (setq CPT_hent (cdr (assoc 5 (entget CPT_en))))
  (setq CHN (getstring "\nChainage or Name of X-section :"))
  (setq xl_list (entget xl_en))
  (setq p1 (cdr (assoc 10 xl_list))
        p2 (cdr (assoc 11 xl_list))
  )
  (setq xl_ang (angle p1 p2))
  (setq CHN_en (ptxt CHN p2 "CHN_Layer" 5))
  (setq CHN_hent (cdr (assoc 5 (entget CHN_en))))
  (chgprop  CHN_en 50 xl_ang)                                  ;create CHN at p2, direction = X-section line


  (xl_attxdata xl_en CHN_hent CPT_hent appname)                ;add XDATA to X-section line
  (chgprop xl_en 8 "XS_Line")                                  ;change X-section line to "XS_Line" layer
  (princ (strcat "\nChainage " CHN " created."))
  (princ)  
);def
;;====
;;Given X-section line with chainage & Point of center line
;;Buffer is distance from X-section line to define boundary of selection points
(defun xline2xs(xl_en appname / ocmd buffer fxyz fofs xl_en cpt CHN ofs_pts xyz_pts xdata XL_list CHN_hent CPT_hent)
  (setq buffer 10.0)                                     ;set buffer distance
  (setq fxyz "d:/TGA_Lisp/xsec_data-4.csv")              ;FileName for XYZ output
  (setq fofs "d:/TGA_Lisp/xsec-4.csv")                   ;FileName for Offset&Eleveation output
  (setq ocmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "UCS" "W")
  (setq XL_list (entget xl_en (list appname)))
  (setq xdata (assoc -3 XL_list))
  (setq CHN_hent (cdr (nth 2 (cadr xdata)))
        CPT_hent (cdr (nth 3 (cadr xdata)))
  )
  (setq cpt (cdr (assoc 10 (entget (handent CPT_hent))))
        CHN (cdr (assoc 1 (entget (handent CHN_hent))))
  )
  (setq ss_pts (getpts_by_xbuf xl_en buffer cpt))                 ;call function to select RTK points
                                                                  ;ss_pts = list of selection set, xyz & offset_elevtion
  (setq ofs_pts (caddr ss_pts))   
  (xs2file CHN ofs_pts fofs "a")                                  ;call function for writing offset&elev.
  (setq xyz_pts (cadr ss_pts))
  (write-xsec CHN xyz_pts fxyz "a")                               ;call function for writing xyz
  (chg_ss_bycode (car ss_pts) 62 5)                               ;change color ss to 5 (already wrote to file)
  ;(chg_ss_bycode (sscode 0 8 "XS_Point") 62 5)                    ;change point in "XS_Point" layer to 5
  (setvar "CMDECHO" ocmd)
  (princ)
);def
;;====
;;
(defun ssxl2xs(/ ssxl xl_en XL_list xdata CHN CHN_xl)
   (if (null appname) (setq appname "RTK_XS"))
   (setq ssxl (sscode 0 8 "XS_Line"))
   (if (null ssxl) (progn (princ "\nXS_Line not found") (exit)))
   (setq i 0)
   (setq ssxl_list '())
   (princ "\nStart processing XS_Line")
   (repeat (sslength ssxl)
      (setq xl_en (ssname ssxl i))
	  (setq XL_list (entget xl_en (list appname)))
      (setq xdata (assoc -3 XL_list))
	  (if xdata
	     (progn
           (setq CHN (cdr (nth 1 (cadr xdata))))
	       (setq CHN_xl (cons CHN xl_en))
           (setq ssxl_list (append ssxl_list (list CHN_xl)))
		 );progn
	  );if
	  (setq i (1+ i))
   );rep
   (setq ssxl_list (sort_x ssxl_list))                      ;sorting by Chainage name
   (foreach xl ssxl_list
      (setq xl_en (cdr xl))
   	  (xline2xs xl_en appname)
   );for
   (chg_ss_bycode ssxl 8 "XS_Line_Completed")               ;change X-section line to layer "XS_line_Completed"
   (princ "\nFinished XS_Line")
   (princ)
);def

(defun c:ss2x()
  (ssxl2xs)
);def
;;===========================