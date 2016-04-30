(defmodule lodox-parse
  (doc "Parsing LFE source files for metadata.")
  (export (docs 1) (documented 1))
  (import (rename erlang ((list_to_float 1) list->float))))

(include-lib "clj/include/compose.lfe")
(include-lib "lodox/include/lodox-macros.lfe")


;;;===================================================================
;;; API
;;;===================================================================

;; TODO: write a better docstring
(defun docs (app-name)
  "Given an app-name (binary), return a proplist like:

```commonlisp
[#(name        #\"lodox\")
 #(version     \"0.13.1\")
 #(description \"The LFE rebar3 Lodox plugin\")
 #(documents   [])
 #(modules     {{list of proplists of module metadata}})
 #(documented  {{ see documented/1 }})]
```"
  (let* ((app         (doto (binary_to_atom app-name 'latin1)
                        (application:load)))
         (app-info    (let ((`#(ok ,info) (application:get_all_key app)))
                        info))
         (modules     (mod-docs (proplists:get_value 'modules app-info)))
         (version     (proplists:get_value 'vsn app-info ""))
         (documented  (documented modules))
         (description (proplists:get_value 'description app-info "")))
    `[#(name         ,app-name)
      #(version      ,version)
      #(description  ,description)
      ;; TODO: parse includes as before
      #(libs         [])
      #(modules      ,modules)
      #(documented ,documented)]))

(defun documented (modules)
  "Given a list of parsed modules, return a proplist representing
  undocumented functions therein.

```commonlisp
[#(percentage   {{float 0.0-100.0}}
 #(undocumented [#({{ module name (atom) }}
                   [\"{{function/arity}}\" ...]),...]))]
```"
  (flet ((percentage
          ([`#(#(,n ,d) ,modules)]
           (->> `[,(* (/ n d) 100)]
                (io_lib:format "~.2f")
                (compose #'list->float/1 #'car/1)
                (tuple 'percentage)
                (list `#(undocumented ,modules))))))
    (->> modules
         (lists:foldl #'documented/2 #(#(0 0) []))
         (percentage))))

(defun documented
  ([mod-doc `#(,tally ,modules)]
   (let ((`#(,tally* ,undocumented)
          (lists:foldl #'-documented/2 `#(,tally [])
            (proplists:get_value 'exports mod-doc))))
     (-> (tuple (proplists:get_value 'name mod-doc) undocumented)
         (cons modules)
         (->> (tuple tally*))))))

(defun -documented
  ([export `#(#(,n ,d) ,undocumented)]
   (if (undocumented? export)
     `#(#(,n       ,(+ d 1)) ,(cons (func-name export) undocumented))
     `#(#(,(+ n 1) ,(+ d 1)) ,undocumented))))

(defun undocumented? (export) (=:= #"" (proplists:get_value 'doc export #"")))

;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun mod-behaviour (module)
  (let ((attributes (call module 'module_info 'attributes)))
    (proplists:get_value 'behaviour attributes '())))

(defun mod-docs
  ([mod] (when (is_atom mod))
   (let ((file (proplists:get_value 'source (call mod 'module_info 'compile))))
     (iff (=:= ".lfe" (filename:extension file))
       (case (mod-doc mod)
         (`#(,exports ,mod-doc)
          `#(true [#(name      ,(mod-name mod))
                   #(behaviour ,(mod-behaviour mod))
                   #(doc       ,mod-doc)
                   #(exports   ,exports)
                   #(filepath  ,file)]))
         ('false 'false)))))
  ([mods] (when (is_list mods))
   (lists:filtermap #'mod-docs/1 mods)))

(defun mod-doc (mod)
  (case (ldoc-chunk mod)
    (#"Missing \"LDoc\" chunk." 'false)
    (`#(lfe_docs_v1 ,docs ,mod-doc)
     (-> (match-lambda
           ([`#(doc function true ,name ,arity ,patterns ,doc ,line)]
            `#(true [#(name ,name)
                     #(arity    ,arity)
                     #(patterns ,patterns)
                     #(doc      ,doc)
                     #(line     ,line)]))
           ([_] 'false))
         (lists:filtermap docs)
         (tuple mod-doc)))))

(defun mod-name (mod) (call mod 'module_info 'module))

(defun patterns (forms) (lists:map #'pattern/1 forms))

(defun pattern
  ([`(,patt ,(= `(when . ,_) guard) . ,_)] `(,@patt ,guard))
  ([`(,arglist . ,_)] arglist))

(defun func-name (def)
  "Given a parsed def{un,macro} form (map), return a string, `\"name/arity\"`."
  (->> (lc ((<- k '[name arity])) (proplists:get_value k def))
       (io_lib:format "~s/~w")
       (lists:flatten)))

(defun ldoc-chunk (mod)
  "Return a given `beam` module's `\"LDoc\"` chunk as a term.

  If the chunk is missing, return `#\"Missing \"LDoc\" chunk.\"`."
  (case (beam_lib:chunks (code:which mod) '["LDoc"] '[allow_missing_chunks])
    (`#(ok #(,_ [#("LDoc" missing_chunk)])) #"Missing \"LDoc\" chunk.")
    (`#(ok #(,_ [#("LDoc" ,chunk)]))        (binary_to_term chunk))))
