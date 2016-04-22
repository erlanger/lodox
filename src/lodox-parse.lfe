(defmodule lodox-parse
  (doc "Parsing LFE source files for metadata.")
  (export (docs 1) (documented 1)))

(include-lib "clj/include/compose.lfe")
(include-lib "lodox/include/lodox-macros.lfe")


;;;===================================================================
;;; API
;;;===================================================================

;; TODO: write a better docstring
(defun docs (app-name)
  "Given an app-name (binary), return a map like:

```commonlisp
'#m(name        #\"lodox\"
    version     \"0.13.1\"
    description \"The LFE rebar3 Lodox plugin\"
    documents   ()
    modules     {{list of maps of module metadata}}
    documented  #m(modules    {{map from module name to list of f/a strings}}
                   percentage {{percent documented (float)}}))
```"
  (let* ((app         (doto (binary_to_atom app-name 'latin1)
                        (application:load)))
         (app-info    (let ((`#(ok ,info) (application:get_all_key app)))
                        (maps:from_list info)))
         (modules     (mod-docs (mref app-info 'modules)))
         (version     (maps:get 'vsn         app-info ""))
         (documented  (documented modules))
         (description (maps:get 'description app-info "")))
    `#m(name        ,app-name
        version     ,version
        description ,description
        libs        []
        modules     ,modules
        documented  ,documented)))

(defun documented (modules)
  "Given a list of parsed modules, return a map representing
  undocumented functions therein.

```commonlisp
(map 'percentage   {{float 0.0-100.0}}
     'undocumented (map {{module name (atom) }} [\"{{function/arity}}\" ...]
                        ...))
```"
  (flet ((percentage
           ([`#(#(,n ,d) ,modules)]
            (->> `[,(* (/ n d) 100)]
                 (io_lib:format "~.2f")
                 (clj-comp:compose #'list_to_float/1 #'hd/1)
                 (mset `#m(undocumented ,modules) 'percentage)))))
    (->> modules
         (lists:foldl #'documented/2 #(#(0 0) #m()))
         (percentage))))

(defun documented
  ([`#m(exports ,exports name ,name) acc]
   (fletrec ((tally
               ([(= (map 'doc "") export) `#(#(,n ,d) ,m)]
                `#(#(,n ,(+ d 1))
                   ,(-> (func-name export)
                        (cons (maps:get name m []))
                        (->> (mset m name)))))
               ([`#m(doc ,_) `#(#(,n ,d) ,m)]
                `#(#(,(+ n 1) ,(+ d 1)) ,m))))
     (lists:foldl #'tally/2 acc exports))))


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
          `#(true #m(name      ,(mod-name mod)
                     behaviour ,(mod-behaviour mod)
                     doc       ,mod-doc
                     exports   ,exports
                     filepath  ,file)))
         ('false 'false)))))
  ([mods] (when (is_list mods))
   (lists:filtermap #'mod-docs/1 mods)))

(defun mod-doc (mod)
  (case (ldoc-chunk mod)
    (#"Missing \"LDoc\" chunk." 'false)
    (`#(lfe_docs_v1 ,docs ,mod-doc)
     (-> (match-lambda
           ([`#(doc function true ,name ,arity ,patterns ,doc ,line)]
            `#(true #m(name ,name
                       arity    ,arity
                       patterns ,patterns
                       doc      ,doc
                       line     ,line)))
           ([_] 'false))
         (lists:filtermap docs)
         (tuple mod-doc)))))

(defun mod-name (mod) (call mod 'module_info 'module))

(defun patterns (forms) (lists:map #'pattern/1 forms))

(defun pattern
  ([`(,patt ,(= `(when . ,_) guard) . ,_)] `(,@patt ,guard))
  ([`(,arglist . ,_)] arglist))

(defun func-name
  "Given a parsed def{un,macro} form (map), return a string, `\"name/arity\"`."
  ([`#m(name ,name arity ,arity)]
   (->> `[,name ,arity] (io_lib:format "~s/~w") (lists:flatten))))

(defun ldoc-chunk (mod)
  "Return a given `beam` module's `\"LDoc\"` chunk as a term.

  If the chunk is missing, return `#\"Missing \"LDoc\" chunk.\"`."
  (case (beam_lib:chunks (code:which mod) '["LDoc"] '[allow_missing_chunks])
    (`#(ok #(,_ [#("LDoc" missing_chunk)])) #"Missing \"LDoc\" chunk.")
    (`#(ok #(,_ [#("LDoc" ,chunk)]))        (binary_to_term chunk))))
