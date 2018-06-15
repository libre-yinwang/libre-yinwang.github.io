#lang racket
{require html}
{require xml}
{require racket/system}
{define paths (string-split (port->string (first (process "find -wholename './blog-cn/*/*/*/*' | sort"))) "\n")}
{define all-bodys '()}
{for ([path paths])
  {define port (open-input-file	path)}
  {define htmlv (read-html port)}
  {match-define (html _ (list _ (body _ bodys))) htmlv}
  {set! all-bodys (append bodys all-bodys)}
  }
{define all-html (html '() (list (head '() '()) (body '() all-bodys)))}