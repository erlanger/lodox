(defmodule lodox-json-writer
  (doc "Documentation writer that outputs JSON.")
  (export (write-docs 1)))

(include-lib "clj/include/compose.lfe")

(defun write-docs (app)
  "Take raw documentation info and turn it into JSON.
  Write to and return `output-path` in `app`. Default: `\"./doc\"`"
  (let* ((json        (jsx:encode (do-modules app) '[space #(indent 2)]))
         (output-path (proplists:get_value 'output-path app "doc"))
         ('ok         (filelib:ensure_dir output-path))
         (app-name    (proplists:get_value 'name app))
         (filename    (binary (app-name binary) ".json")))
    (file:write_file (filename:join output-path filename) json)
    output-path))

(defun do-modules (app)
  (-> (lists:map #'do-module/1 (proplists:get_value 'modules app))
      (->> (tuple 'modules))
      (cons (proplists:delete 'modules app))))

(defun do-module (module)
  (-> (lists:map #'do-patterns/1 (proplists:get_value 'exports module))
      (->> (tuple 'exports))
      (list* (do-behaviour (proplists:get_value 'behaviour module))
             (proplists:delete 'behaviour (proplists:delete 'exports module)))))

(defun do-patterns (export)
  (-> (lists:map #'do-pattern/1 (proplists:get_value 'patterns export))
      (->> (tuple 'patterns))
      (cons (proplists:delete 'patterns export))))

(defun do-pattern (pattern)
  (re:replace (lfe_io_pretty:term pattern) "comma " ". ,"
              '[global #(return binary)]))

(defun do-behaviour (behaviours)
  (-> (lambda (atm) (atom_to_binary atm 'latin1))
      (lists:map behaviours)
      (->> (tuple 'behaviour))))
