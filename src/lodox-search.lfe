;;; ======================================================= [ lodox-search.lfe ]

(defmodule lodox-search
  "Doc-searching functions."
  (export (exports 2) (exports 3))
  (import (rename erlang
            ((atom_to_list    1) atom->string)
            ((integer_to_list 1) int->string))))

(include-lib "lfe/include/clj.lfe")

;;; ==================================================================== [ API ]

(defun exports (modules partial-name)
  "Find the best-matching function or macro.

  Given a list of modules and a partial export name,
  return the first matching definition.
  If none is found, return `` 'undefined ``.

  Equivalent to `(`[[exports/3]] `modules partial-name 'undefined)`."
  (exports modules partial-name 'undefined))

(defun exports (modules partial-name starting-mod)
  "Like [[exports/2]], but give precedence to matches in `starting-mod`."
  (let* ((suffix   (cond->> partial-name
                     (not (lists:member #\: partial-name)) (cons #\:)))
         (matches  (lists:filter
                     (lambda (export-name)
                       (lists:suffix suffix export-name))
                     (exports modules)))
         (external (lists:dropwhile
                     (lambda (export-name)
                       (=/= (atom->string starting-mod) (module export-name)))
                     matches)))
    (cond
     ((not (nil? external)) (car external))
     ((nil? matches)        'undefined)
     ('true                     (car matches)))))

;;; ===================================================== [ Internal functions ]

(defun exports (modules)
  (lc ((<- mod modules) (<- export (get mod 'exports)))
    (export-name mod export)))

(defun export-name (mod export)
  (let ((arity (get export 'arity)))
    (cond-> (++ (atom->string (get mod 'name)) ":"
                (atom->string (get export 'name)))
      (not (undefined? arity)) (++ "/" (int->string arity)))))

(defun module (export-name)
  (lists:takewhile (lambda (c) (=/= c #\:)) export-name))

(defun get (plist key) (proplists:get_value key plist))

;;; ==================================================================== [ EOF ]
