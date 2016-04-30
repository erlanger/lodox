(defmodule lodox-search
  (doc "Doc-searching functions.")
  (export (funcs 2) (funcs 3))
  (import (rename erlang
            ((atom_to_list 1)    atom->string)
            ((integer_to_list 1) int->string))))

(defun funcs (modules partial-func)
  "Find the best-matching `def{un,macro}`.

  Given a list of modules and a partial `def{un,macro}` string,
  return the first matching definition.
  If none is found, return `` 'undefined ``.

  Equivalent to [[funcs/3]] with `` 'undefined `` as `starting-mod`."
  (funcs modules partial-func 'undefined))

(defun funcs (modules partial-func starting-mod)
  "Like [[funcs/2]], but give precedence to matches in `starting-mod`."
  (let* ((suffix   (if (lists:member #\: partial-func)
                     partial-func
                     (cons #\: partial-func)))
         (matches  (lists:filter
                     (lambda (func-name) (lists:suffix suffix func-name))
                     (exported-funcs modules)))
         (external (lists:dropwhile
                     (lambda (func-name)
                       (=/= (atom->string starting-mod) (module func-name)))
                     matches)))
    (if (null? external)
      (if (null? matches) 'undefined (car matches))
      (car external))))


;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun exported-funcs (modules)
  (lc ((<- mod modules) (<- func (get mod 'exports)))
    (func-name mod func)))

(defun func-name (mod func)
  (++ (atom->string (get mod 'name))
      ":" (atom->string (get func 'name))
      "/" (int->string (get func 'arity))))

(defun module (func-name)
  (lists:takewhile (lambda (c) (=/= c #\:)) func-name))

(defun get (plist key) (proplists:get_value key plist))

(defun null? ([()] 'true) ([_] 'false))
