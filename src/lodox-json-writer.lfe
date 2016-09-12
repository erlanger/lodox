(defmodule lodox-json-writer
  "Documentation writer that outputs JSON."
  (export (write-docs 1)))

(defun write-docs (app)
  "Take raw documentation info and turn it into JSON.
  Write to and return `output-path` in `app`. Default: `\"./docs\"`"
  (let* ((json        (jsx:encode (do-modules app) '[space #(indent 2)]))
         (output-path (proplists:get_value 'output-path app "docs"))
         ('ok         (filelib:ensure_dir output-path))
         (app-name    (proplists:get_value 'name app))
         (filename    (binary (app-name binary) ".json")))
    (file:write_file (filename:join output-path filename) json)
    output-path))

(defun do-modules (app)
  (clj:-> (lists:map #'do-module/1 (proplists:get_value 'modules app))
          (clj:->> (tuple 'modules))
          (cons (proplists:delete 'modules app))))

(defun do-module (module)
  (clj:-> (lists:map #'do-patterns/1 (proplists:get_value 'exports module))
          (clj:->> (tuple 'exports))
          (list* (do-behaviour (proplists:get_value 'behaviour module))
                 (clj:->> (proplists:delete 'exports module)
                          (proplists:delete 'behaviour)))))

(defun do-patterns (export)
  (clj:-> (lists:map #'do-pattern/1 (proplists:get_value 'patterns export))
          (clj:->> (tuple 'patterns))
          (cons (proplists:delete 'patterns export))))

(defun do-pattern (pattern)
  (clj:->> '[global #(return binary)]
           (re:replace (lfe_io_pretty:term pattern) "comma " ". ,")))

(defun do-behaviour (behaviours)
  (clj:-> (lambda (atm) (atom_to_binary atm 'latin1))
          (lists:map behaviours)
          (clj:->> (tuple 'behaviour))))
