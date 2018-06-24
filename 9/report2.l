(setf (get 's 'neighbors) '(a d)
      (get 'a 'neighbors) '(s b d)
      (get 'b 'neighbors) '(a c e)
      (get 'c 'neighbors) '(b)
      (get 'd 'neighbors) '(s a e)
      (get 'e 'neighbors) '(b d f)
      (get 'f 'neighbors) '(e)
      (get 'nil 'neighbors) '(s)
      )
(defun get_neighbors (node);隣接接点情報を取得
  (get node 'neighbors) )


(defun my-search (start finish queue);最上層関数
  (cond ((my-endp queue) nil)
	((completep finish (car queue)) (reverse (car queue)))
	(t (my-search start finish (appends (my-extend (car queue)) (cdr queue))))
	) )

(defun my-endp (queue)
  (cond ((null queue) t)
	(t nil)))

(defun completep (finish list);完全経路
  (cond ((eq finish (car list)) t)
	(t nil)
	) )

(defun appends (list1 list2);リストの結合
  (cond ((null list1) list2)
	(t (cons (car list1) (appends (cdr list1) list2)))
	) )

(defun my-extend (path);spanを噛ませる事で閉路を持たない木を構成
  (extend2  (span (get_neighbors (last_node path)) path) path)
  )

(defun extend2 (neighbors path);経路を展開する
  (cond ((null neighbors) nil)
	(t (cond ((eq neighbors nil) nil)
		 ((atom neighbors) (cons neighbors path))
		 (t (cons (cons (car neighbors) path) (extend2 (cdr neighbors) path)))
		 )
	   )
	)
  )

(defun last_node (list)
  (cond ((atom list) list)
	(t (car list))
	)
  )
(defun span (neighbors path);隣接接点リストから一度通ったことのあるアトムを削除する
  (cond ((null path) neighbors)
	(t  (span (removecycle (car path) neighbors) (cdr path))
	    )
	)
  )

(defun removecycle (a list);リストからaを削除
  (cond ((null list) nil)
	((eq a (car list)) (cdr list) );隣接接点情報自体には重複がないので1つ削除できればそれより後ろは見なくていい
	(t (cons (car list) (removecycle a (cdr list)) ) )
	)
  )		    

(print (my-search 's 'f '(nil)))
