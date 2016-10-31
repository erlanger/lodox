;;; ================================================== [ lodox-html-writer.lfe ]

(defmodule lodox-html-writer
  "Documentation writer that outputs HTML."
  (export (write-docs 1))
  (export (escape-html 1))
  (import (rename levaindoc ((markdown_github->html 1) gfm->html)))
  (import (rename erlang
            ((atom_to_list    1) atom->string)
            ((integer_to_list 1) int->string))))

(include-lib "lfe/include/clj.lfe")
(include-lib "exemplar/include/html-macros.lfe")

;;; ==================================================================== [ API ]

(defun write-docs (app)
  "Take raw documentation info and turn it into formatted HTML.
  Write to and return `output-path` in `app`. Default: `\"./docs\"`"
  (doto (proplists:get_value 'output-path app "docs")
    (ensure-dirs '["css" "js"])
    (copy-resource "css/default.css")
    (copy-resource "css/hk-pyg.css")
    (copy-resource "js/jquery.min.js")
    (copy-resource "js/page_effects.js")
    (write-index        app)
    (write-modules      app)
    (write-documents    app)
    (write-libs         app)
    (write-undocumented app)))

(defun escape-html
  "Change special characters into HTML character entities."
  ([x] (when (is_atom x))
   (escape-html (atom->string x)))
  ([text]
   (fold-replace text
     '[#("\\&" "\\&amp;")
       #("<"   "\\&lt;")
       #(">"   "\\&gt;")
       #("\""  "\\&quot;")
       #("'"   "\\&apos;")])))

;;; ===================================================== [ Internal functions ]

(defun include-css (style)
  (link `[type "text/css" href ,style rel "stylesheet"]))

(defun include-js (script)
  (script `[type "text/javascript" src ,script]))

(defun link-to (uri content)
  "```html
  <a href=\"{{uri}}\">{{content}}</a>
  ```"
  (a `[href ,uri] content))

(defun export-id (export)
  (if (clj:string? export)
    (-> (http_uri:encode export)
        (re:replace "%" "." '[global #(return list)])
        (->> (++ "export-")))
    (export-id (export-name export))))

(defun format-docstring (app def) (format-docstring app 'undefined def))

(defun format-docstring (app module export)
  ;; TODO: make format configurable
  (format-docstring app module export 'markdown))

(defun format-docstring (app mod def format)
  ;; TODO: ensure binary
  (case (proplists:get_value 'doc def #"")
    (#"" "")
    (doc
     (case format
       ('plaintext
        (pre '[class "plaintext"] (h doc)))
       ('markdown (when (is_list mod))
        (format-md-docstring app (proplists:get_value 'name mod) doc))
       ('markdown
        (format-md-docstring app (proplists:get_value 'name def) doc))))))

(defun format-md-docstring (app name doc)
  (markdown->html (format-wikilinks app (unicode:characters_to_list doc) name)))

(defun markdown->html (markdown)
  "Given a Markdown string, convert it to HTML, using [pandoc].
    [pandoc]: http://pandoc.org"
  (case (os:find_executable "pandoc")
    ('false (exit "Pandoc is required."))
    (pandoc (let ((`#(ok ,html) (gfm->html markdown))) html))))

(defun format-wikilinks (app html init)
  (case (re:run html "\\[\\[([^\\[]+(/\\d+)?)\\]\\]"
                '[global #(capture all_but_first list)])
    ('nomatch html)
    (`#(match ,matches)
     (let ((to-search (++ (proplists:get_value 'modules app)
                          (proplists:get_value 'libs    app))))
       (->> matches
            (lists:filtermap (do-format-wikilinks html init to-search))
            (fold-replace html))))))

(defun do-format-wikilinks (html init to-search)
  (match-lambda
    ([`(,match)]
     (let ((export (lodox-search:exports to-search match init)))
       (if-not (=:= 'undefined export)
         (let ((`#(,mod [,_ . ,fname])
                (lists:splitwith (lambda (c) (=/= c #\:)) export)))
           `#(true #(,(re-escape (++ "[[" match "]]"))
                     ,(link-to (export-uri mod fname)
                        (cond->> fname
                          (=/= (atom->string init) mod) (++ mod ":")))))))))))

(defun index-by (k plists)
  (-> (lambda (p pp) (-> (proplists:get_value k p) (tuple p) (cons pp)))
      (lists:foldl [] plists)))

(defun mod-filename (mod)
  (if (clj:string? mod)
    (++ mod ".html")
    (mod-filename (mod-name mod))))

(defun mod-filepath (output-dir module)
  (filename:join output-dir (mod-filename module)))

(defun mod-name (mod) (atom->string (proplists:get_value 'name mod)))

(defun doc-filename (doc)
  (doc-filename "" doc))

(defun doc-filename (prefix doc)
  (++ prefix (proplists:get_value 'name doc) ".html"))

(defun doc-filepath (output-dir doc)
  (doc-filepath output-dir "" doc))

(defun doc-filepath (output-dir prefix doc)
  (filename:join output-dir (doc-filename prefix doc)))

(defun export-uri (module export)
  (++ (mod-filename module) "#" (export-id export)))

(defun export-source-uri (source-uri app module export)
  (let* ((offset    (+ 1 (length (proplists:get_value 'app-dir app))))
         (filepath* (proplists:get_value 'filepath module))
         (filepath  (binary:part filepath* offset (- (size filepath*) offset)))
         (line      (integer_to_binary (proplists:get_value 'line export)))
         (version   (proplists:get_value 'version app)))
    (fold-replace source-uri
      `[#("{filepath}"  ,filepath)
        #("{line}"      ,line)
        #("{version}"   ,version)])))

(defun index-link (_app on-index?)
  `[,(h3 '[class "no-link"] (span '[class "inner"] "Application"))
    ,(ul '[class "index-link"]
       (li `[class ,(++ "depth-1" (if on-index? " current" ""))]
           (link-to "index.html" (div '[class "inner"] "Index"))))])

(defun topics-menu (app current-doc)
  (case (proplists:get_value 'documents app [])
    ([] [])
    (docs
     (list
       (h3 '[class "no-link"] (span '[class "inner"] "Topics"))
       (ul
         (lc ((<- doc docs))
           (li `[class ,(cond-> "depth-1"
                          (=:= doc current-doc) (++ " current"))]
               (link-to (doc-filename "topic-" doc)
                 (div '[class "inner"]
                   (span (h (proplists:get_value 'title doc))))))))))))

(defun includes-menu (app current-lib)
  (make-menu "Includes" (proplists:get_value 'libs app) current-lib))

(defun modules-menu (app current-mod)
  (make-menu "Modules" (proplists:get_value 'modules app) current-mod))

(defun make-menu
  ([_heading [] _current] [])
  ([heading plists current]
   (flet ((menu-item
           ([`#(,name ,mod)]
            (let ((class (++ "depth-1" (if (=:= mod current) " current" "")))
                  (inner (div '[class "inner"] (h (atom->string name)))))
              (li `[class ,class] (link-to (mod-filename mod) inner))))))
     (list (h3 '[class "no-link"] (span '[class "inner"] heading))
           (ul (lists:map #'menu-item/1 (index-by 'name plists)))))))

(defun primary-sidebar (app) (primary-sidebar app []))

(defun primary-sidebar (app current)
  (div '[class "sidebar primary"]
    `[,(index-link app (=:= () current))
      ,(topics-menu app current)
      ,(includes-menu app current)
      ,(modules-menu app current)]))

(defun sorted-exports (module)
  (-> (lambda (a b)
        (=< (string:to_lower (export-name a))
            (string:to_lower (export-name b))))
      (lists:sort (proplists:get_value 'exports module))))

(defun exports-sidebar (module)
  (div '[class "sidebar secondary"]
    `[,(h3 (link-to "#top" (span '[class "inner"] "Exports")))
      ,(ul
         (lists:map
           (lambda (export)
             (li '[class "depth-1"]
                 (link-to (export-uri module export)
                   (div '[class "inner"]
                     (span (h (export-name export))))))) ; TODO: members?
           (sorted-exports module)))]))

(defun default-includes ()
  `[,(meta '[charset "UTF-8"])
    ,(include-css "css/default.css")
    ,(include-css "css/hk-pyg.css")
    ,(include-js "js/jquery.min.js")
    ,(include-js "js/page_effects.js")])

(defun project-title (app)
  (span '[class "project-title"]
    (list (span '[class "project-name"]
            (h (proplists:get_value 'name app)))
          " "
          (span '[class "project-version"]
            (h (proplists:get_value 'version app))))))

(defun header* (app)
  (div '[id "header"]
    `[,(h2 `["Generated by "
             ,(link-to "https://github.com/lfe-rebar3/lodox" "Lodox")])
      ,(h1 (link-to "index.html"
             `[,(project-title app) " "
               ,(span '[class "project-documented"]
                  (->> (proplists:get_value 'documented app)
                       (proplists:get_value 'percentage)
                       (round)
                       (list)
                       (io_lib:format "(~w% documented)")))]))]))

(defun index-page (app)
  (html
    (list
      (head
        (list
          (default-includes)
          (title (++ (h (proplists:get_value 'name app)) " "
                     (h (proplists:get_value 'version app))))))
      (body
        (list
          (header* app)
          (primary-sidebar app)
          (div '[id "content" class "module-index"]
            (list
              (h1 (project-title app))
              (case (proplists:get_value 'description app)
                ("" "")
                (description (div '[class "doc"] (p (h description)))))
              (case (lists:sort
                      (lambda (a b) (=< (mod-name a) (mod-name b)))
                      (proplists:get_value 'libs app))
                ([] "")
                (libs
                 (list
                   (h2 "Includes")
                   (lists:map
                     (lambda (lib)
                       (div '[class "module"]
                         (list
                           (h3 (link-to (mod-filename lib)
                                 (h (mod-name lib))))
                           (div '[class "index"]
                             (list
                               (p "Definitions")
                               (unordered-list
                                (lists:map
                                  (lambda (export)
                                    (list
                                      " "
                                      (link-to (export-uri lib export)
                                        (export-name export))
                                      " "))
                                  (sorted-exports lib))))))))
                     libs))))
              (case (proplists:get_value 'documents app)
                ([] "")
                (docs
                 (list
                   (h2 "Topics")
                   (ul '[class "topics"]
                     (lc ((<- doc docs))
                       (li (link-to (doc-filename "topic-" doc)
                             (h (proplists:get_value 'title doc)))))))))
              (h2 "Modules")
              (lists:map
                (lambda (module)
                  (div '[class "module"]
                    (list
                      (h3 (link-to (mod-filename module)
                            (h (mod-name module))))
                      (case (format-docstring app [] module)
                        (""  "")
                        ;; TODO: summarize
                        (doc (div '[class "doc"] doc)))
                      (div '[class "index"]
                        (list
                          (p "Exports")
                          (unordered-list
                           (lists:map
                             (lambda (export)
                               (list
                                 " "
                                 (link-to (export-uri module export)
                                   (export-name export))
                                 " "))
                             (sorted-exports module))))))))
                (lists:sort
                  (lambda (a b) (=< (mod-name a) (mod-name b)))
                  (proplists:get_value 'modules app))))))))))

;; TODO: exemplar-ify this
(defun unordered-list (lst) (ul (lists:map #'li/1 lst)))

(defun format-document (app doc)
  (case (proplists:get_value 'format doc)
    ('markdown
     (div '[class "markdown"]
       (let ((html (-> (proplists:get_value 'content doc)
                       (unicode:characters_to_list)
                       (markdown->html))))
         (format-wikilinks app html 'undefined))))))

(defun document-page (app doc)
  (html
    (list (head
            (list (default-includes)
                  (title (h (proplists:get_value 'title doc)))))
          (body
            (list (header* app)
                  (primary-sidebar app doc)
                  (div '[id "content" class "document"]
                    (div '[class "doc"] (format-document app doc))))))))

(defun export-usage (export)
  (lists:map
    (lambda (pattern)
      (re:replace (lfe_io_pretty:term pattern) "comma " ". ,"
                  '[global #(return list)]))
    (proplists:get_value 'patterns export)))

(defun mod-behaviour (mod)
  (lists:map
    (lambda (behaviour)
      (h4 '[class "behaviour"] (atom->string behaviour)))
    (proplists:get_value 'behaviour mod)))

(defun export-docs (app module export)
  (div `[class "public anchor" id ,(h (export-id export))]
    `[,(h3 (h (export-name export)))
      ,(case (export-usage export)
         ('["()"] [])
         (usages
          (div '[class "usage"]
            (-> `["```commonlisp"
                  ,@(lists:map #'unicode:characters_to_list/1 usages)
                  "```"]
                (string:join "\n")
                (markdown->html)))))
      ,(div '[class "doc"]
         (format-docstring app module export))
      ;; TODO: members?
      ,(case (proplists:get_value 'source-uri app)
         ('undefined [])                ; Log failure to generate link?
         (source-uri
          (div '[class "src-link"]
            (link-to (export-source-uri source-uri app module export)
              "view source"))))]))

(defun module-page (app module)
  (html
    `[,(head
         `[,(default-includes)
           ,(title (++ (h (mod-name module)) " documentation"))])
      ,(body
         `[,(header* app)
           ,(primary-sidebar app module)
           ,(exports-sidebar module)
           ,(div '[id "content" class "module-docs"]
              `[,(h1 '[id "top" class "anchor"] (h (mod-name module)))
                ,(mod-behaviour module)
                ,(div '[class "doc"] (format-docstring app [] module))
                ,(lists:map (lambda (export) (export-docs app module export))
                   (sorted-exports module))])])]))

(defun lib-page (app lib)
  (html
    `[,(head
         `[,(default-includes)
           ,(title (++ (h (proplists:get_value 'name lib))
                       " documentation"))])
      ,(body
         `[,(header* app)
           ,(primary-sidebar app lib)
           ,(exports-sidebar lib)
           ,(div '[id "content" class "module-docs"] ; TODO: confirm this
              (list (h1 '[id "top" class "anchor"]
                        (h (proplists:get_value 'name lib)))
                    (lists:map (lambda (export) (export-docs app lib export))
                      (sorted-exports lib))))])]))

(defun copy-resource (output-dir resource)
  (let* ((this  (proplists:get_value 'source (module_info 'compile)))
         (lodox (filename:dirname (filename:dirname this))))
    (file:copy (filename:join `[,lodox "resources" ,resource])
               (filename:join output-dir resource))))

(defun ensure-dirs
  "Given a `path` and list of `dirs`, call [[ensure-dir/2]] `path` `dir`
for each `dir` in `dirs`."
  ([path `(,dir . ,dirs)]
   (ensure-dir path dir)
   (ensure-dirs path dirs))
  ([path ()] 'ok))

(defun ensure-dir (dir)
  "Given a `dir`ectory path, perform the equivalent of `mkdir -p`.
  If something goes wrong, throw a descriptive error."
  (case (filelib:ensure_dir (filename:join dir "dummy"))
    ('ok               'ok)
    (`#(error ,reason) (error reason))))

(defun ensure-dir (path dir)
  "Given a `path` and `dir`ectory name, call [[ensure-dir/1]] on `path`/`dir`."
  (ensure-dir (filename:join path dir)))

(defun write-index (output-dir app)
  (file:write_file (filename:join output-dir "index.html")
                   (index-page app)))

(defun write-modules (output-dir app)
  (lists:foreach
    (lambda (module)
      (file:write_file (mod-filepath output-dir module)
                       (module-page app module)))
    (proplists:get_value 'modules app)))

(defun write-documents (output-dir app)
  (lists:foreach
    (lambda (document)
      ;; TODO
      ;; (file:write_file (transform-html app (document-page app document)))
      (file:write_file (doc-filepath output-dir "topic-" document)
                       (document-page app document)))
    (proplists:get_value 'documents app [])))

(defun write-libs (output-dir app)
  (lists:foreach
    (lambda (lib)
      (file:write_file (mod-filepath output-dir lib)
                       (lib-page app lib)))
    (proplists:get_value 'libs app)))

(defun write-undocumented (output-dir app)
  (let ((undocumented (clj-seq:get-in app '[documented undocumented]))
        (output-file  (filename:join output-dir "undocumented.txt"))
        (collect      (match-lambda
                        ([`#(,_mod []) acc] acc)
                        ([`#(,mod ,exports) acc]
                         (-> (cons (car exports)
                                   (lc ((<- export (cdr exports)))
                                     (list #"\n" export)))
                             (->> (list mod) (io_lib:format "== ~s ==~n~s~n"))
                             (cons acc))))))
    (case (lists:foldl collect "" undocumented)
      (""    (if (filelib:is_file output-file) (file:delete output-file) 'ok))
      (lines (->> (string:join lines "\n") (file:write_file output-file))))))

(defun export-name (export)
  (let ((arity (proplists:get_value 'arity export)))
    (cond-> (atom->string (proplists:get_value 'name export))
      (not (undefined? arity)) (++ "/" (int->string arity)))))

(defun h (text)
  "Convenient alias for escape-html/1."
  (escape-html text))

(defun fold-replace (string pairs)
  (-> (match-lambda
        ([`#(,patt ,replacement) acc]
         (re:replace acc patt replacement '[global #(return list)])))
      (lists:foldl string pairs)))

;; Stolen from Elixir
;; https://github.com/elixir-lang/elixir/blob/944990381f6cadbaf751f2443d485684ba35b6d8/lib/elixir/lib/regex.ex#L601-L619
(defun re-escape (string)
  (re:replace string "[.^$*+?()[{\\\|\s#]" "\\\\&" '[global  #(return list)]))

;;; ==================================================================== [ EOF ]
