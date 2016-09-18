;;; ========================================================= [ lodox-main.lfe ]

(defmodule lodox-main
  "Main module for generating documentation"
  (export-macro DEFAULTS)
  (import (rename erlang
            ((function_exported 3) exported?))))

(include-lib "lfe/include/clj.lfe")

;;; ================================================================= [ Macros ]

(defmacro DEFAULTS ()
  `'[#(output-path  #"docs")
     #(source-paths [#"src"])
     #(doc-paths    [#"priv/docs"])
     #(doc-files    all)
     #(modules      all)
     #(metadata     [])
     #(themes       [default])])

;;; ==================================================================== [ API ]

(defn generate-docs [app-name]
  "Equivalent to `(`[[generate-docs/1]]` app-name [])`.")

(defn generate-docs [app-name options]
  "Generate documentation from source files."
  (as-> options opts
    (lists:keymerge 1 (lists:keysort 1 opts) (lists:keysort 1 (DEFAULTS)))
    (->> (read-modules app-name opts)
         (tuple 'modules)
         (lists:keystore 'modules 1 opts))
    (->> (read-documents opts)
         (tuple 'documents)
         (lists:keystore 'documents 1 opts))
    (funcall (writer opts) opts)))

;;; ===================================================== [ Internal functions ]

(defn- read-modules [app-name options]
  (as-> app-name app
    (doto (binary_to_atom app 'latin1) (application:load))
    (let ((`#(ok ,info) (application:get_all_key app))) info)
    (-> (proplists:get_value 'modules app)
        (filter-modules (proplists:get_value 'modules options))
        (mod-docs)
        (add-source-paths)
        )))

(defn- mod-docs
  ([mod] (when (is_atom mod))
   (let ((file (proplists:get_value 'source (call mod 'module_info 'compile))))
     (clj:iff (=:= ".lfe" (filename:extension file))
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

(defn- writer [options]
  (let ((writer-mod (proplists:get_value 'writer options 'lodox-html-writer)))
    (if (exported? writer-mod 'write-docs 1)
      (erlang:make_fun writer-mod 'write-docs 1)
      (exit 'bad-writer-mod (list opts)))))

(defn- macro? [doc]
  (=:= 'macro (lfe_doc:mf_doc_type doc)))

;;; ==================================================================== [ EOF ]
