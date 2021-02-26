;; guile-wc.scm
;;来源：garfileo@思否
(define (get-file-name args)
  ;;若args这个列表的cdr部分为空列表，则获取car部分作为文件名。
  ;;总而言之，列表args的最后一个有效元素将作为文件名。
 ;;如果用户没有输文件名，guile-wc.scm就会对自己进行统计
  (cond ((null? (cdr args)) (car args))
;;cdr不为空列表，则调用自身继续分析其cdr部分，直到cdr部分为空列表。
        (else (get-file-name (cdr args)))))

;;文本分析器
(define (arg-parser args opt)
  (cond ((null? args) #f)
;;;如果args为空，返回false
        ((string=? (car args) opt) #t)
;;;如果args的car部分与opt相同，则返回true
;;;否则抽取args的cdr部分，再次调用自身
;;;这样做实现了多参数兼容
        (else (arg-parser (cdr args) opt))))


;;;主函数
(define (guile-wc args file)
  (define (lwm-count l w m)
;;;命名怪不讲究的啊:D,
    (let ((char (read-char file)))
      (cond ((eof-object? char) `(,l ,w ,m))
;;若char为文件终止符，就是说文件分析完了
;;就用一个前置表达式返回l,w,m三个变量的值。l,w,m会放在一个car部分为l,cdr部分为w.m的列表中。
            ((char=? char #\newline) (lwm-count (+ l 1) (+ w 1) (+ m 1)))
;;若char为换行符，l,m,w三个参数全体++并再次调用lwm-count函数
            ((char=? char #\space) (lwm-count l (+ w 1) (+ m 1)))
;;char为空格，则w,m++，再次调用lwm-count
            (else (lwm-count l w (+ m 1))))))
;;;以上情况都不符合，则m++，再次调用自身。
  (let ((lwm (lwm-count 0 0 0)))
;;;使用let调用lwm-count，完成对行数，单词数，字符数的统计
;;;并将产生的列表绑定到lwm变量上
    (cond ((arg-parser args "-l") (car lwm))
;;;参数中有-l则返回文件行数
          ((arg-parser args "-w") (cadr lwm))
;;;参数中含-w则返回词数
          ((arg-parser args "-m") (caddr lwm))
;;;参数中含-m，则返回字符数。
          (else lwm))))
;;;参数只有文件名，就全返回
;;;反正计数工作已经结束了
;;为args赋值，令其值为命令行参数列表
(define args (command-line))
;;打开文件读写端口
(define file (open-input-file (get-file-name args) #:encoding "utf-8"))
;;调用guile-wc，并将求值结果由stdout输出
(display (guile-wc args file)) (newline)
;;关闭文件读写端口
(close-input-port file)

;;;显然，如果给guile-wc写上shebang，扔到$PATH里，无文件名时就会出乱子。
