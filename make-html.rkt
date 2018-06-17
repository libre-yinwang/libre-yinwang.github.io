#lang racket
{require html-parsing}
{define (html->xexpr p)
  {define (F x)
    (match x
      [`(,A (@ ,@B) ,@C) `(,A ,B ,@(map F C))]
      [`(,A ,@C) `(,A () ,@(map F C))]
      [x x])}
  {match (html->xexp p)
    [`(*TOP* (html ,@x) ,@_) `(html () . ,(map F x))]}}
{require xml}
{require racket/system}
{define paths (string-split (port->string (first (process "find -wholename './blog-cn/*/*/*/*' | sort | tac"))) "\n")}
{define (allow x)
  {match x
    ['(string () "类型的值found，你必须使用“函数式编程”的方式，来写这之后的代码：")
     "<String>类型的值found，你必须使用“函数式编程”的方式，来写这之后的代码："] ; blog-cn/2015/11/21/programming-philosophy的錯誤
    ['(br ()) x]
    [`(,A ,B ,@C)
     {case A
       [(script *COMMENT*) ""]
       [(div
         p
         h5 h4 h3 h2 h1
         ul ol li
         strong em blockquote sup
         table tbody tr td) `(,A ,B ,@(map allow C))]
       [(code pre a) x]
       [(img)
        (match B
          [(list D ... `(src ,(? string? path)) E ...)
           {define path1
             {match (string->list path)
               [`(#\. #\. #\/ #\. #\. #\/ #\. #\. #\/ #\. #\. #\/ ,@R) (list->string `(#\. #\/ ,@R))]
               [_ path]}}
           `(,A (,@D (src ,path1) ,@E) ,@C)])]}]
    [(or "\r\n" "\n") "\r\n"]
    [(? string?) x]}}
{define all-bodys
  {map
   {λ (path)
     {define port (open-input-file path)}
     {define htmlv (html->xexpr port)}
     {match-define (list 'html _ _ ... `(body () ,@body) _ ...) htmlv}
     (map allow body)
     }
   paths}}
{define head
  '(head ()
    (meta ((http-equiv "content-type") (content "text/html; charset=utf-8")))
    (meta ((name "viewport") (content "width=device-width, initial-scale=0.5")))
    (link ((href "main.css") (rel "stylesheet") (type "text/css")))
    (link ((rel "shortcut icon") (href "images/Yc.jpg")))
    )}
{define body `(body () . ,(apply append all-bodys))}
{define html `(html () ,head ,body)}
{define xml (xexpr->xml html)}
{define doc (document (prolog '() #f '()) xml '())}
(write-xml doc (open-output-file "./all.html" #:exists 'replace))