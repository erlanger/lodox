(defmodule unit-lodox-tests
  "Lodox unit tests.")

(include-lib "ltest/include/ltest-macros.lfe")

(deftestgen projects-shapes
  (lists:zipwith #'validate_project/2 (src-dirs) (all-docs)))

;; EUnit gets very upset if the following _ is a -.
(defun validate_project (dir project)
  `[#(#"project is a proplist"
      ,(is* (clj-p:proplist? project)))
    #(#"description is a binary"
      ,(is* (is_binary (get* project 'description))))
    #(#"libs is a list"
      ,(is* (is_list (get* project 'libs))))
    #(#"modules is a list"
      ,(is* (is_list (get* project 'modules))))
    #(#"name matches directory"
      ,(is-equal* (project-name dir) (get* project 'name)))
    #(#"version is a binary"
      ,(is* (is_binary (get* project 'version))))])

(deftestgen modules-shapes
  (lists:map #'validate_module/1 (project-wide 'modules)))

(defun validate_module (module)
  `[#(#"module is a proplist"
      ,(is* (clj-p:proplist? module)))
    #(#"module has correct keys"
      ,(is-equal* '(behaviour doc exports filepath name)
                  (lists:sort (proplists:get_keys module))))
    #(#"behaviour is a list of atoms"
      ,(is* (lists:all #'is_atom/1 (get* module 'behaviour))))
    #(#"doc is a binary"
      ,(is* (is_binary (get* module 'doc))))
    #(#"exports is a list"
      ,(is* (is_list (get* module 'exports))))
    #(#"filepath refers to a regular file"
      ,(is* (filelib:is_regular (get* module 'filepath))))
    #(#"name is an atom"
      ,(is* (is_atom (get* module 'name))))])

(deftestgen exports-shapes
  (lists:map #'validate_exports/1 (project-wide 'exports 'modules)))

(defun validate_exports (exports)
  `[#(#"exports is a proplist"
      ,(is* (clj-p:proplist? exports)))
    #(#"exports has correct keys"
      ,(is-equal* '(arity doc line name patterns)
                  (lists:sort (proplists:get_keys exports))))
    #(#"patterns is a list of patterns (which may end with a guard)"
      ,(let ((patterns (lists:map
                         (lambda (pattern)
                           (if (is_list pattern)
                             (lists:filter
                               (match-lambda
                                 ([`(when . ,_t)] 'false)
                                 ([_]             'true))
                               pattern)))
                         (get* exports 'patterns))))
         (is* (lists:all #'patterns?/1 patterns))))
    #(#"artity is an integer"
      ,(is* (is_integer (get* exports 'arity))))
    #(#"doc is a binary"
      ,(is* (is_binary (get* exports 'doc))))
    #(#"line is an integer"
      ,(is* (is_integer (get* exports 'line))))
    #(#"name is an atom"
      ,(is* (is_atom (get* exports 'name))))])


;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun all-docs () (lists:map #'lodox-parse:docs/1 '(#"lodox")))

(defun get* (plist k) (proplists:get_value k plist 'error))

(defun project-name
  (["src"] #"lodox")
  ([dir]   (filename:basename (filename:dirname dir))))

(defun project-wide
  ([f]   (when (is_function f)) (lists:flatmap f (all-docs)))
  ([key]                        (project-wide (lambda (proj) (get* proj key)))))

(defun project-wide (key2 key1)
  (project-wide
   (lambda (proj) (lists:flatmap (lambda (m) (get* m key2)) (get* proj key1)))))

(defun src-dirs () '("src"))

(defun patterns?
  "Given a term, return `true` iff it is either the empty list, a list of
  elements satisfying [[pattern?/1]] or a term that satisfies [[pattern?/1]]."
  (['()]        'true)
  ([`(,h . ,t)]
   (andalso (pattern? h) (if (is_list t) (patterns? t) (pattern? t))))
  ([_] 'false))

(defun pattern?
  "Return `true` iff `x` seems like a valid pattern or satisfies [[arg?/1]]."
  ([(= x `(,h . ,_t))]
   (orelse (string? x)
           (lists:member h
             '[= ++* () backquote quote binary cons list map tuple])
           (andalso (is_atom h) (lists:prefix "match-" (atom_to_list h)))))
  ([x] (arg? x)))

(defun string? (data)
  "Return `true` iff `data` is a flat list of printable characters."
  (io_lib:printable_list data))

(defun arg? (x)
  "Return `true` iff `x` seems like a valid element of an arglist."
  (lists:any (lambda (p) (funcall p x))
    (list #'is_atom/1
          #'is_binary/1
          #'is_bitstring/1
          #'is_number/1
          ;; FIXME: for older OTP releases
          ;; #'is_map/1
          #'is_tuple/1
          #'string?/1)))
