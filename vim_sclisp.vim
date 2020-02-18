let &maxfuncdepth = 200

let s:k_lpar = '('
let s:k_rpar = ')'
let s:k_quote = "'"
let s:k_nil = {'tag': 'nil', 'data': 'nil'}

function! s:safe_car(obj) abort
  if a:obj['tag'] ==# 'cons' | return a:obj['car'] | endif
  return s:k_nil
endfunction

function! s:safe_cdr(obj) abort
  if a:obj['tag'] ==# 'cons' | return a:obj['cdr'] | endif
  return s:k_nil
endfunction

function! s:make_error(str) abort
  return {'tag': 'error', 'data': a:str}
endfunction

let s:sym_table = [["nil", s:k_nil]]

function! s:make_sym(str) abort
  for tpl in s:sym_table
    if a:str ==# tpl[0]
      return tpl[1]
    endif
  endfor
  let ret = {'tag': 'sym', 'data': a:str}
  call insert(s:sym_table, [a:str, ret])
  return ret
endfunction

let s:sym_t = s:make_sym("t")
let s:sym_quote = s:make_sym("quote")
let s:sym_if = s:make_sym("if")
let s:sym_lambda = s:make_sym("lambda")
let s:sym_defun = s:make_sym("defun")
let s:sym_setq = s:make_sym("setq")
let s:sym_loop = s:make_sym("loop")
let s:sym_return = s:make_sym("return")
let s:loop_val = s:k_nil

function! s:make_num(n) abort
  return {'tag': 'num', 'data': a:n}
endfunction

function! s:make_cons(a, d) abort
  return {'tag': 'cons', 'car': a:a, 'cdr': a:d}
endfunction

function! s:make_subr(f) abort
  return {'tag': 'subr', 'data': a:f}
endfunction

function! s:make_expr(args, env) abort
  return {
      \ 'tag': 'expr',
      \ 'args': s:safe_car(a:args),
      \ 'body': s:safe_cdr(a:args),
      \ 'env': a:env
      \ }
endfunction

function! s:nreconc(lst, tail) abort
  let lst = a:lst
  let tail = a:tail
  while lst['tag'] ==# 'cons'
    let tmp = lst['cdr']
    let lst['cdr'] = tail
    let tail = lst
    let lst = tmp
  endwhile
  return tail
endfunction

function! s:nreverse(lst) abort
  return s:nreconc(a:lst, s:k_nil)
endfunction

function! s:pairlis(lst1, lst2) abort
  let ret = s:k_nil
  let lst1 = a:lst1
  let lst2 = a:lst2
  while lst1['tag'] ==# 'cons' && lst2['tag'] ==# 'cons'
    let ret = s:make_cons(s:make_cons(lst1['car'], lst2['car']), ret)
    let lst1 = lst1['cdr']
    let lst2 = lst2['cdr']
  endwhile
  return s:nreverse(ret)
endfunction

function! s:is_delimiter(chr) abort
  return a:chr ==# s:k_lpar || a:chr ==# s:k_rpar || a:chr ==# s:k_quote
      \ || a:chr =~# '\s\+'
endfunction

function! s:skip_spaces(str) abort
  let str = a:str
  return substitute(str, '^\s\+', '', '')
endfunction

function! s:make_num_or_sym(str) abort
  if a:str =~# '^[+-]\?\d\+$' | return s:make_num(str2nr(a:str)) | endif
  return s:make_sym(a:str)
endfunction

function! s:look_ahead(str) abort
  let t = s:skip_spaces(a:str)
  let c = empty(t) ? '_' : a:str[0]
  let rest = empty(t) ? '' : t[1:]
  return [t, c, rest]
endfunction

function! s:read(str) abort
  let [str, c, rest] = s:look_ahead(a:str)
  if empty(str) | return [s:make_error('empty input'), ''] | endif
  if c ==# s:k_rpar | return [s:make_error('invalid syntax: ' . str), ''] | endif
  if c ==# s:k_lpar | return s:read_list(rest) | endif
  if c ==# s:k_quote | return s:read_quote(rest) | endif
  return s:read_atom(str)
endfunction

function! s:read_list(str) abort
  let str = a:str
  let ret = s:k_nil
  let [s, c, rest] = [0, 0, 0]
  while 1
    let [s, c, rest] = s:look_ahead(str)
    if empty(s) | return [s:make_error('unfinished parenthesis'), ''] | endif
    if c ==# s:k_rpar | break | endif
    let [elm, next] = s:read(s)
    if elm['tag'] ==# 'error' | return [elm, next] | endif
    let ret = s:make_cons(elm, ret)
    let str = next
  endwhile
  return [s:nreverse(ret), rest]
endfunction

function! s:read_quote(str) abort
  let [elm, next] = s:read(a:str)
  return [s:make_cons(s:make_sym('quote'), s:make_cons(elm, s:k_nil)), next]
endfunction

function! s:read_atom(str) abort
  let n = len(a:str)
  let i = 0
  while i < n
    if s:is_delimiter(a:str[i])
      return [s:make_num_or_sym(a:str[:i - 1]), a:str[i:]]
    endif
    let i += 1
  endwhile
  return [s:make_num_or_sym(a:str), '']
endfunction

function! s:print_obj(obj) abort
  if a:obj['tag'] ==# 'num' || a:obj['tag'] ==# 'sym' || a:obj['tag'] ==# 'nil'
    return a:obj['data']
  elseif a:obj['tag'] ==# 'error'
    return '<error: ' . a:obj['data'] . '>'
  elseif a:obj['tag'] ==# 'cons'
    return s:print_list(a:obj)
  elseif a:obj['tag'] ==# 'subr'
    return '<subr>'
  elseif a:obj['tag'] ==# 'expr'
    return '<expr>'
  endif
  return '<unknown object>'
endfunction

function! s:print_list(obj) abort
  let obj = a:obj
  let ret = ''
  let is_first = 1
  while obj['tag'] ==# 'cons'
    if is_first
      let ret = s:print_obj(obj['car'])
      let is_first = 0
    else
      let ret .= ' ' . s:print_obj(obj['car'])
    endif
    let obj = obj['cdr']
  endwhile
  if obj['tag'] ==# 'nil' | return '(' . ret . ')' | endif
  return '(' . ret . ' . ' . s:print_obj(obj) . ')'
endfunction

function! s:find_var_in_frame(str, alist) abort
  let alist = a:alist
  while 1
    let x = s:safe_car(s:safe_car(alist))
    if x['tag'] !=# 'sym' | return s:k_nil | endif
    if x['data'] ==# a:str | return s:safe_car(alist) | endif
    let alist = s:safe_cdr(alist)
  endwhile
endfunction

function! s:find_var(sym, env) abort
  let env = a:env
  while env['tag'] ==# 'cons'
    let pair = s:find_var_in_frame(a:sym['data'], env['car'])
    if pair['tag'] !=# 'nil' | return pair | endif
    let env = env['cdr']
  endwhile
  return s:k_nil
endfunction

let s:g_env = s:make_cons(s:k_nil, s:k_nil)

function! s:add_to_env(sym, val, env) abort
  if a:env['tag'] !=# 'cons' | return | endif
  let a:env['car'] = s:make_cons(s:make_cons(a:sym, a:val), a:env['car'])
endfunction

function! s:update_var(sym, val, env) abort
  let bind = s:find_var(a:sym, a:env)
  if bind['tag'] ==# 'cons'
    let bind['cdr'] = a:val
  else
    call s:add_to_env(a:sym, a:val, s:g_env)
  endif
endfunction

function! s:eval(obj, env) abort
  if a:obj['tag'] ==# 'sym'
    let pair = s:find_var(a:obj, a:env)
    if pair ==# s:k_nil | return s:make_error(a:obj['data'] . ' has no value') | endif
    return s:safe_cdr(pair)
  endif
  if a:obj['tag'] ==# 'cons' | return s:eval_cons(a:obj, a:env) | endif
  return a:obj
endfunction

function! s:eval_cons(obj, env) abort
  let opr = s:safe_car(a:obj)
  let args = s:safe_cdr(a:obj)
  if opr ==# s:sym_quote
    return s:safe_car(args)
  elseif opr ==# s:sym_if
    if s:eval(s:safe_car(args), a:env) ==# s:k_nil
      return s:eval(s:safe_car(s:safe_cdr(s:safe_cdr(args))), a:env)
    endif
    return s:eval(s:safe_car(s:safe_cdr(args)), a:env)
  elseif opr ==# s:sym_lambda
    return s:make_expr(args, a:env)
  elseif opr ==# s:sym_defun
    let sym = s:safe_car(args)
    let expr = s:make_expr(s:safe_cdr(args), a:env)
    call s:add_to_env(sym, expr, s:g_env)
    return sym
  elseif opr ==# s:sym_setq
    let sym = s:safe_car(args)
    let l:val = s:eval(s:safe_car(s:safe_cdr(args)), a:env)
    call s:update_var(sym, l:val, a:env)
    return l:val
  elseif opr ==# s:sym_loop
    return s:loop(args, a:env)
  elseif opr ==# s:sym_return
    let s:loop_val = s:eval(s:safe_car(args), a:env)
    return s:make_error('')
  endif
  return s:apply(s:eval(opr, a:env), s:evlis(args, a:env))
endfunction

function! s:evlis(lst, env) abort
  let lst = a:lst
  let ret = s:k_nil
  while lst['tag'] ==# 'cons'
    let elm = s:eval(lst['car'], a:env)
    if elm['tag'] ==# 'error' | return elm | endif
    let ret = s:make_cons(elm, ret)
    let lst = lst['cdr']
  endwhile
  return s:nreverse(ret)
endfunction

function! s:progn(body, env) abort
  let body = a:body
  let ret = s:k_nil
  while body['tag'] ==# 'cons'
    let ret = s:eval(body['car'], a:env)
    if ret['tag'] ==# 'error' | return ret | endif
    let body = body['cdr']
  endwhile
  return ret
endfunction

function! s:loop(body, env) abort
  while 1
    let ret = s:progn(a:body, a:env)
    if ret['tag'] ==# 'error'
      if empty(ret['data']) | return s:loop_val | endif
      return ret
    endif
  endwhile
endfunction

function! s:apply(f, args) abort
  if a:f['tag'] ==# 'error' | return a:f | endif
  if a:args['tag'] ==# 'error' | return a:args | endif
  if a:f['tag'] ==# 'subr' | return a:f['data'](a:args) | endif
  if a:f['tag'] ==# 'expr'
    return s:progn(a:f['body'], s:make_cons(s:pairlis(a:f['args'], a:args), a:f['env']))
  endif
  return s:make_error(s:print_obj(a:f) . ' is not function')
endfunction

function! s:subr_car(args) abort
  return s:safe_car(s:safe_car(a:args))
endfunction

function! s:subr_cdr(args) abort
  return s:safe_cdr(s:safe_car(a:args))
endfunction

function! s:subr_cons(args) abort
  return s:make_cons(s:safe_car(a:args), s:safe_car(s:safe_cdr(a:args)))
endfunction

function! s:subr_eq(args) abort
  let x = s:safe_car(a:args)
  let y = s:safe_car(s:safe_cdr(a:args))
  if x['tag'] ==# 'num' && y['tag'] ==# 'num'
    return x['data'] ==# y['data'] ? s:sym_t : s:k_nil
  endif
  return x ==# y ? s:sym_t : s:k_nil
endfunction

function! s:subr_atom(args) abort
  return s:safe_car(a:args)['tag'] ==# 'cons' ? s:k_nil : s:sym_t
endfunction

function! s:subr_numberp(args) abort
  return s:safe_car(a:args)['tag'] ==# 'num' ? s:sym_t : s:k_nil
endfunction

function! s:subr_symbolp(args) abort
  return s:safe_car(a:args)['tag'] ==# 'sym' ? s:sym_t : s:k_nil
endfunction

function! s:subr_add_or_mul(f, init_val) abort
  function! s:aux_a(f, init_val, args) abort
    let ret = a:init_val
    let args = a:args
    while args['tag'] ==# 'cons'
      if args['car']['tag'] !=# 'num' | return s:make_error('wrong type') | endif
      let ret = a:f(ret, args['car']['data'])
      let args = args['cdr']
    endwhile
    return s:make_num(ret)
  endfunction
  return {args -> s:aux_a(a:f, a:init_val, args)}
endfunction

let s:subr_add = s:subr_add_or_mul({x, y -> x + y}, 0)
let s:subr_mul = s:subr_add_or_mul({x, y -> x * y}, 1)

function! s:subr_sub_or_div_or_mod(f) abort
  function! s:aux_b(f, args) abort
    let x = s:safe_car(a:args)
    let y = s:safe_car(s:safe_cdr(a:args))
    if x['tag'] !=# 'num' || y['tag'] !=# 'num' | return s:make_error('wrong type') | endif
    return s:make_num(a:f(x['data'], y['data']))
  endfunction
  return {args -> s:aux_b(a:f, args)}
endfunction

let s:subr_sub = s:subr_sub_or_div_or_mod({x, y -> x - y})
let s:subr_div = s:subr_sub_or_div_or_mod({x, y -> x / y})
let s:subr_mod = s:subr_sub_or_div_or_mod({x, y -> x % y})

function! s:repl() abort
  while 1
    let l = input('> ')
    echo "\n"
    let obj = s:eval(s:read(l)[0], s:g_env)
    echo s:print_obj(obj)
  endwhile
endfunction

function! REPL() abort
  call s:add_to_env(s:make_sym('car'), s:make_subr(function('s:subr_car')), s:g_env)
  call s:add_to_env(s:make_sym('cdr'), s:make_subr(function('s:subr_cdr')), s:g_env)
  call s:add_to_env(s:make_sym('cons'), s:make_subr(function('s:subr_cons')), s:g_env)
  call s:add_to_env(s:make_sym('eq'), s:make_subr(function('s:subr_eq')), s:g_env)
  call s:add_to_env(s:make_sym('atom'), s:make_subr(function('s:subr_atom')), s:g_env)
  call s:add_to_env(s:make_sym('numberp'), s:make_subr(function('s:subr_numberp')), s:g_env)
  call s:add_to_env(s:make_sym('symbolp'), s:make_subr(function('s:subr_symbolp')), s:g_env)
  call s:add_to_env(s:make_sym('+'), s:make_subr(s:subr_add), s:g_env)
  call s:add_to_env(s:make_sym('*'), s:make_subr(s:subr_mul), s:g_env)
  call s:add_to_env(s:make_sym('-'), s:make_subr(s:subr_sub), s:g_env)
  call s:add_to_env(s:make_sym('/'), s:make_subr(s:subr_div), s:g_env)
  call s:add_to_env(s:make_sym('mod'), s:make_subr(s:subr_mod), s:g_env)
  call s:add_to_env(s:sym_t, s:sym_t, s:g_env)
  call s:repl()
endfunction
