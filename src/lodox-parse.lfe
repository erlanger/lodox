;;; ======================================================== [ lodox-parse.lfe ]

(defmodule lodox-parse
  "Parsing LFE source files for metadata."
  (export (docs 1) (docs 2) (documented 1) (mod-doc 1))
  (import (from clj (comp 3)))
  (import (rename erlang ((list_to_float 1) list->float))))

(include-lib "lfe/include/clj.lfe")

;;; ==================================================================== [ API ]

(defun docs (app-name)
  "Equivalent to [[docs/2]] with the empty list as `excluded-modules`."
  (docs app-name []))

;; TODO: write a better docstring
;; TODO: document excluded-modules
(defun docs (app-name excluded-modules)
  "Given an app-name (binary), return a proplist like:

  ```commonlisp
  [#(name        #\"lodox\")
   #(version     \"0.16.2\")
   #(description \"The LFE rebar3 Lodox plugin\")
   #(documents   [])
   #(modules     {{list of proplists of module metadata}})
   #(documented  {{ see documented/1 }})]
  ```"
  (let* ((app         (doto (binary_to_atom app-name 'latin1)
                        (application:load)))
         (app-info    (let ((`#(ok ,info) (application:get_all_key app)))
                        info))
         (version     (proplists:get_value 'vsn app-info ""))
         (description (proplists:get_value 'description app-info ""))
         (modules     (-> (proplists:get_value 'modules app-info)
                          (->> (filter-excluded excluded-modules))
                          (mod-docs)))
         (documented  (documented modules)))
    `[#(name        ,app-name)
      #(version     ,(list_to_binary version))
      #(description ,(list_to_binary description))
      ;; TODO: parse includes as before
      ;; TODO: documents
      #(libs        [])
      #(modules     ,modules)
      #(documented  ,documented)]))

(defun documented (modules)
  "Given a list of parsed modules, return a proplist representing
  undocumented functions therein.

  ```commonlisp
  [#(percentage   {{float 0.0-100.0}}
   #(undocumented [#({{ module name (atom) }}
                     [\"{{function/arity}}\" ...]),...]))]
  ```"
  (flet ((percentage
          ([`#(#(,_ 0) ,modules)]
           `[#(percentage 0) #(undocumented ,modules)])
          ([`#(#(,n ,d) ,modules)]
           (->> `[,(* (/ n d) 100)]
                (io_lib:format "~.2f")
                (comp #'list->float/1 #'car/1)
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
     `#(#(,n       ,(+ d 1)) ,(cons (export-name export) undocumented))
     `#(#(,(+ n 1) ,(+ d 1)) ,undocumented))))

(defun undocumented? (export) (=:= #"" (proplists:get_value 'doc export #"")))

;;; ===================================================== [ Internal functions ]

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
                   #(filepath  ,(list_to_binary file))]))
         ('false 'false)))))
  ([mods] (when (is_list mods))
   (lists:filtermap #'mod-docs/1 mods)))

(defun mod-doc (mod)
  (case (lfe_doc:get_module_docs mod)
    (`#(error ,_) 'false)
    (`#(ok ,chunk)
     (-> (lambda (doc)
           (case (lfe_doc:mf_doc_type doc)
             ('function
              `#(true [#(name     ,(lfe_doc:function_name doc))
                       #(arity    ,(lfe_doc:function_arity doc))
                       #(patterns ,(patterns (lfe_doc:function_patterns doc)))
                       #(doc      ,(flatten (lfe_doc:function_doc doc)))
                       #(line     ,(lfe_doc:function_line doc))]))
             ('macro
               `#(true [#(name     ,(lfe_doc:macro_name doc))
                        #(patterns ,(patterns (lfe_doc:macro_patterns doc)))
                        #(doc      ,(flatten (lfe_doc:macro_doc doc)))
                        #(line     ,(lfe_doc:macro_line doc))]))))
         (lists:filtermap (lfe_doc:mf_docs chunk))
         (tuple (flatten (lfe_doc:module_doc chunk)))))))

(defun mod-name (mod) (call mod 'module_info 'module))

(defun patterns (forms) (lists:map #'pattern/1 forms))

(defun pattern
  ([`#(,patt  [])]        patt)
  ([`#(,patt ,guard)] `(,@patt (when ,@guard))))

(defun export-name (def)
  "Given a parsed def{un,macro} form (map), return a string, `\"name/arity\"`."
  (->> (lc ((<- k '[name arity])) (proplists:get_value k def))
       (io_lib:format "~s/~w")
       (iolist_to_binary)))

(defun filter-excluded (excluded-modules modules)
  (-> (lambda (module) (not (lists:member module excluded-modules)))
      (lists:filter modules)))

(defun flatten
  ([lines] (when (is_list lines))
   (iolist_to_binary (lists:map (lambda (line) (list line #"\n")) lines)))
  ([bin] bin))

;;; ==================================================================== [ EOF ]
