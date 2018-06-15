#lang racket
{require html-parsing}
{require xml}
{require racket/system}
{define paths (string-split (port->string (first (process "find -wholename './blog-cn/*/*/*/*' | sort"))) "\n")}
{define all-bodys '()}
{for ([path paths])
  {define port (open-input-file	path)}
  {define htmlv (html->xexp port)}
  {match-define (list '*TOP* _ ... (list 'html _ ... (list 'body body ...) _ ...) _ ...) htmlv}
  {writeln body}
  (error)
  }
;{define all-html (html '() (list (head '() '()) (body '() all-bodys)))}
;(write-xml all-html)