;;; ============================================================== [ lodox.lfe ]

(defmodule lodox
  "The Lodox [Rebar3][1] [provider][2].

  [1]: http://www.rebar3.org/docs/plugins
  [2]: https://github.com/tsloughter/providers"
  (behaviour provider)
  ;; N.B. Export all since LFE doesn't like us exporting do/1.
  (export all))

(defun namespace ()
  "The namespace in which `lodox` is registered, `lfe`."
  'lfe)

(defun provider-name ()
  "The 'user friendly' name of the task, `lodox`."
  'lodox)

(defun short-desc ()
  "A one line, short description of the task, used in lists of providers."
  "Generate documentation from LFE source files.")

(defun deps ()
  "The list of dependencies, providers, that need to run before this one."
  '[#(default compile)])

(defun desc ()
  "The description for the task, used by `rebar3 help`."
  (short-desc))

;;; ==================================================================== [ API ]

(defun init (state)
  "Initiate the Lodox provider."
  (rebar_api:debug "Initializing {~p, ~p}" `[,(namespace) ,(provider-name)])
  (let* ((opts `[#(name       ,(provider-name))   ; The 'user friendly' name
                 #(module     ,(MODULE))          ; The module implementation
                 #(namespace  ,(namespace))       ; Plugin namespace
                 #(opts       [])                 ; List of plugin options
                 #(deps       ,(deps))            ; The list of dependencies
                 #(example    "rebar3 lfe lodox") ; How to use the plugin
                 #(short_desc ,(short-desc))      ; A one-line description
                 #(desc       ,(desc))            ; A longer description
                 #(bare       true)               ; Task can be run by user
                 #(profiles   [docs])])
         (provider (providers:create opts)))
    (let ((state* (rebar_state:add_provider state provider)))
      (rebar_api:debug "Initialized lodox" [])
      `#(ok ,state*))))

(defun do (state)
  "Generate documentation for each application in the project.

  See: [[lodox-html-writer:write-docs/1]]"
  (rebar_api:debug "Starting do/1 for lodox" [])
  (let ((apps (rebar_state:project_apps state)))
    (lists:foreach #'write-docs/1 apps))
  `#(ok ,state))

(defun format_error (reason)
  "When an exception is raised or a value returned as
  `#(error #((MODULE) reason)`, `(format_error reason)` will be called
  so a string can be formatted explaining the issue."
  (io_lib:format "~p" `[,reason]))

;;; ===================================================== [ Internal functions ]

(defun write-docs (app-info)
  "Given an [app_info_t], call [[lodox-html-writer:write-docs/1]] appropriately.

  [app_info_t]: https://github.com/rebar/rebar3/blob/master/src/rebar_app_info.erl"
  (let* ((`[,opts ,app-dir ,name ,vsn ,out-dir]
          (lists:map (lambda (f) (call 'rebar_app_info f app-info))
            '[opts dir name original_vsn out_dir]))
         (lodox-opts       (get-lodox-opts name opts))
         (excluded-modules (proplists:get_value 'excluded-modules lodox-opts []))
         (ebin-dir         (filename:join out-dir "ebin"))
         (doc-dir          (filename:join app-dir "docs")))
    (rebar_api:debug "Adding ~p to the code path" `[,ebin-dir])
    (code:add_patha ebin-dir)
    (let ((app (++ (lodox-parse:docs name excluded-modules)
                   (cons `#(app-dir ,app-dir)
                         (maybe-default 'output-path doc-dir lodox-opts)))))
      (rebar_api:debug "Generating docs for ~p with excluded modules: ~p"
        `[,(proplists:get_value 'name app) ,excluded-modules])
      (lodox-html-writer:write-docs app)
      (rebar_api:console "Generated ~s v~s docs in ~s"
        `[,name ,vsn ,(proplists:get_value 'output-path app)]))))

(defun get-lodox-opts
  "Parse rebar.config for Lodox options.
  If `name` is a binary, convert it to an atom first."
  ([name rebar-opts] (when (is_binary name))
   (get-lodox-opts (binary_to_atom name 'latin1) rebar-opts))
  ([app rebar-opts] (when (is_atom app))
   (let* ((lodox-config (if (dict:is_key 'lodox rebar-opts)
                          (dict:fetch 'lodox rebar-opts)
                          []))
          (lodox-apps   (proplists:get_value 'apps lodox-config [])))
     (proplists:get_value app lodox-apps []))))

(defun maybe-default (key value opts)
  "Prepend `` `#(,key ,value) `` to `opts` iff `key` is not already defined."
  (if (proplists:is_defined key opts) opts (cons `#(,key ,value) opts)))

;;; ==================================================================== [ EOF ]
