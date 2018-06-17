#lang racket
{require html-parsing}
{require xml}
{require racket/system}
{define paths (string-split (port->string (first (process "find -wholename './blog-cn/*/*/*/*' | sort | tac"))) "\n")}
{define (allow x)
  (cond
    [(equal? x '(string "类型的值found，你必须使用“函数式编程”的方式，来写这之后的代码："))
     "<String>类型的值found，你必须使用“函数式编程”的方式，来写这之后的代码："] ; blog-cn/2015/11/21/programming-philosophy的錯誤
    [(equal? x '(br)) x]
    [(pair? x)
     {define h (car x)}
     (case h
       [(script *COMMENT*) ""]
       [(div
         p
         h5 h4 h3 h2 h1
         ul ol li
         strong em blockquote sup
         table tbody tr td) (cons h (map allow (cdr x)))]
       [(img @ code pre a) x]
       [else (error x)])]
    [(string? x)
     (case x
       [("\r\n" "\n") "\r\n"]
       [else x])]
    [else (error x)])}
{define all-bodys
  {map
   {λ (path)
     {define port (open-input-file	path)}
     {define htmlv (html->xexp port)}
     {match-define (list '*TOP* _ ... (list 'html _ ... (list 'body body ...) _ ...) _ ...) htmlv}
     (map allow body)
     }
   paths}}
{define body (apply append all-bodys)}
{define html `(html (body . ,body))}
{define xml (xexpr->xml html)}
{define doc (document (prolog '() #f '()) xml '())}
(write-xml doc)