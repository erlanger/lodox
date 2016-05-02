(defmodule lodox-html-writer
  (doc "Documentation writer that outputs HTML.")
  (export (write-docs 1))
  (import (from levaindoc (markdown_github->html 1 ))))

(include-lib "clj/include/compose.lfe")
(include-lib "exemplar/include/html-macros.lfe")
(include-lib "lodox/include/lodox-macros.lfe")

(defun write-docs (app)
  "Take raw documentation info and turn it into formatted HTML.
  Write to and return `output-path` in `app`. Default: `\"./doc\"`"
  (doto (proplists:get_value 'output-path app "doc")
    (ensure-dirs '["css" "js"])
    (copy-resource "css/default.css")
    (copy-resource "css/hk-pyg.css")
    (copy-resource "js/jquery.min.js")
    (copy-resource "js/page_effects.js")
    (write-index        app)
    (write-modules      app)
    (write-libs         app)
    (write-undocumented app)))

(defun include-css (style)
  (link `[type "text/css" href ,style rel "stylesheet"]))

(defun include-js (script)
  (script `[type "text/javascript" src ,script]))

(defun link-to (uri content)
  "```html
<a href=\"{{uri}}\">{{content}}</a>
```"
  (a `[href ,uri] content))

(defun func-id (func)
  (if (clj-p:string? func)
    (-> (http_uri:encode (h func))
        (re:replace "%" "." '[global #(return list)])
        (->> (++ "func-")))
    (func-id (func-name func))))

(defun format-docstring (app def) (format-docstring app 'undefined def))

(defun format-docstring (app module func)
  ;; TODO: make format configurable
  (format-docstring app module func 'markdown))

(defun format-docstring (app mod def format)
  ;; TODO: ensure binary
  (case (proplists:get_value 'doc def #"")
    (#"" "")
    (doc
     (case format
       ('plaintext
        (pre '[class "plaintext"] (h doc)))
       ('markdown (when (is_list mod))
        (let ((name (proplists:get_value 'name mod))
              (html (markdown->html (unicode:characters_to_list doc))))
          (format-wikilinks app html name)))
       ('markdown
        (let ((name (proplists:get_value 'name def))
              (html (markdown->html (unicode:characters_to_list doc))))
          (format-wikilinks app html name)))))))

(defun markdown->html (markdown)
  "Given a Markdown string, convert it to HTML.
  Use [pandoc] if available, otherwise [erlmarkdown].

  [pandoc]: http://pandoc.org
  [erlmarkdown]: https://github.com/erlware/erlmarkdown"
  (case (os:find_executable "pandoc")
    ('false (exit "Pandoc is required."))
    (pandoc (let ((`#(ok ,html) (markdown_github->html markdown))) html))))

(defun format-wikilinks (app html init)
  (case (re:run html "\\[\\[([^\\[]+/\\d+)\\]\\]"
                '[global #(capture all_but_first)])
    ('nomatch html)
    (`#(match ,matches)
     (let ((to-search (++ (proplists:get_value 'modules app)
                          (proplists:get_value 'libs    app))))
       (-> (match-lambda
             ([`#(,start ,length)]
              (let* ((match (lists:sublist html (+ 1 start) length))
                     (mfa   (lodox-search:funcs to-search match init)))
                (iff (=/= mfa 'undefined)
                  (let ((`#(,mod [,_ . ,fname])
                         (lists:splitwith (lambda (c) (=/= c #\:)) mfa)))
                    `#(true #(,(re-escape (++ "[[" match "]]"))
                              ,(link-to (func-uri mod fname)
                                 (if (=:= (atom_to_list init) mod)
                                   (h fname)
                                   (h (++ mod ":" fname)))))))))))
           (lists:filtermap (lists:flatten matches))
           (->> (fold-replace html)))))))

(defun index-by (k plists)
  (lists:foldl
    (lambda (p pp) (-> (proplists:get_value k p) (tuple p) (cons pp)))
    [] plists))

(defun mod-filename (mod)
  (if (clj-p:string? mod)
    (++ mod ".html")
    (mod-filename (mod-name mod))))

(defun mod-filepath (output-dir module)
  (filename:join output-dir (mod-filename module)))

(defun mod-name (mod) (atom_to_list (proplists:get_value 'name mod)))

(defun doc-filename (doc)
  (++ (proplists:get_value 'name doc) ".html"))

(defun doc-filepath (output-dir doc)
  (filename:join output-dir (doc-filename doc)))

(defun func-uri (module func)
  (++ (mod-filename module) "#" (func-id func)))

(defun func-source-uri (source-uri app module func)
  (let* ((offset    (+ 1 (length (proplists:get_value 'app-dir app))))
         (filepath* (proplists:get_value 'filepath module))
         (filepath  (binary:part filepath* offset (- (size filepath*) offset)))
         (line      (integer_to_binary (proplists:get_value 'line func)))
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
                  (inner (div '[class "inner"] (h (atom_to_list name)))))
              (li `[class ,class] (link-to (mod-filename mod) inner))))))
     (list (h3 '[class "no-link"] (span '[class "inner"] heading))
           (ul (lists:map #'menu-item/1 (index-by 'name plists)))))))

(defun primary-sidebar (app) (primary-sidebar app []))

(defun primary-sidebar (app current)
  (div '[class "sidebar primary"]
    `[,(index-link app (=:= () current))
      ,(includes-menu app current)
      ,(modules-menu app current)]))

(defun sorted-exported-funcs (module)
  (-> (lambda (a b)
        (=< (string:to_lower (func-name a))
            (string:to_lower (func-name b))))
      (lists:sort (proplists:get_value 'exports module))))

(defun funcs-sidebar (module)
  (div '[class "sidebar secondary"]
    `[,(h3 (link-to "#top" (span '[class "inner"] "Exports")))
      ,(ul
         (lists:map
           (lambda (func)
             (li '[class "depth-1"]
                 (link-to (func-uri module func)
                   (div '[class "inner"]
                     (span (h (func-name func))))))) ; TODO: members?
           (sorted-exported-funcs module)))]))

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
    `[,(head
         `[,(default-includes)
           ,(title (++ (h (proplists:get_value 'name app)) " "
                       (h (proplists:get_value 'version app))))])
      ,(body
         `[,(header* app)
           ,(primary-sidebar app)
           ,(div '[id "content" class "module-index"]
              `[,(h1 (project-title app))
                ,(case (proplists:get_value 'description app)
                   ("" "")
                   (doc (div '[class "doc"] (p (h doc)))))
                ,(case (lists:sort
                         (lambda (a b) (=< (mod-name a) (mod-name b)))
                         (proplists:get_value 'libs app))
                   ([] "")
                   (libs
                    `[,(h2 "Includes")
                      ,(lists:map
                         (lambda (lib)
                           (div '[class "module"]
                             `[,(h3 (link-to (mod-filename lib)
                                      (h (mod-name lib))))
                               ,(div '[class "index"]
                                  `[,(p "Definitions")
                                    ,(unordered-list
                                      (lists:map
                                        (lambda (func)
                                          `[" "
                                            ,(link-to (func-uri lib func)
                                               (func-name func))
                                            " "])
                                        (sorted-exported-funcs lib)))])]))
                         libs)]))
                ,(h2 "Modules")
                ,(lists:map
                   (lambda (module)
                     (div '[class "module"]
                       `[,(h3 (link-to (mod-filename module)
                                (h (mod-name module))))
                         ,(case (format-docstring app [] module)
                            (""  "")
                            ;; TODO: summarize
                            (doc (div '[class "doc"] doc)))
                         ,(div '[class "index"]
                            `[,(p "Exports")
                              ,(unordered-list
                                (lists:map
                                  (lambda (func)
                                    `[" "
                                      ,(link-to (func-uri module func)
                                         (func-name func))
                                      " "])
                                  (sorted-exported-funcs module)))])]))
                   (lists:sort
                     (lambda (a b) (=< (mod-name a) (mod-name b)))
                     (proplists:get_value 'modules app)))])])]))

;; TODO: exemplar-ify this
(defun unordered-list (lst) (ul (lists:map #'li/1 lst)))

#|
(defun format-document
  ([app (= doc `#m(format ,format))] (when (=:= format 'markdown))
   ;; TODO: render markdown
   `[div (class "markdown") ,(proplists:get_value 'content doc)]))

(defun document-page (app doc)
  (html
    (head
      `[,(default-includes)
        ,(title (h (proplists:get_value 'title doc)))])
    (body
      `[,(header* app)
        ,(primary-sidebar app doc)
        ,(div '[id "content" class "document"]
           (div '[id "doc"] (format-document app doc)))])))
|#

(defun func-usage (func)
  (lists:map
    (lambda (pattern)
      (re:replace (lfe_io_pretty:term pattern) "comma " ". ,"
                  '[global #(return list)]))
    (proplists:get_value 'patterns func)))

(defun mod-behaviour (mod)
  (lists:map
    (lambda (behaviour)
      (h4 '[class "behaviour"] (atom_to_list behaviour)))
    (proplists:get_value 'behaviour mod)))

(defun func-docs (app module func)
  (div `[class "public anchor" id ,(h (func-id func))]
    `[,(h3 (h (func-name func)))
      ,(case (func-usage func)
         ('["()"] [])
         (usages
          (div '[class "usage"]
            (-> `["```commonlisp"
                  ,@(lists:map #'unicode:characters_to_list/1 usages)
                  "```"]
                (string:join "\n")
                (markdown->html)))))
      ,(div '[class "doc"]
         (format-docstring app module func))
      ;; TODO: members?
      ,(case (proplists:get_value 'source-uri app)
         ('undefined [])                ; Log failure to generate link?
         (source-uri
          (div '[class "src-link"]
            (link-to (func-source-uri source-uri app module func)
              "view source"))))]))

(defun module-page (app module)
  (html
    `[,(head
         `[,(default-includes)
           ,(title (++ (h (mod-name module)) " documentation"))])
      ,(body
         `[,(header* app)
           ,(primary-sidebar app module)
           ,(funcs-sidebar module)
           ,(div '[id "content" class "module-docs"]
              `[,(h1 '[id "top" class "anchor"] (h (mod-name module)))
                ,(mod-behaviour module)
                ,(div '[class "doc"] (format-docstring app [] module))
                ,(lists:map (lambda (func) (func-docs app module func))
                   (sorted-exported-funcs module))])])]))

(defun lib-page (app lib)
  (html
    `[,(head
         `[,(default-includes)
           ,(title (++ (h (proplists:get_value 'name lib))
                       " documentation"))])
      ,(body
         `[,(header* app)
           ,(primary-sidebar app lib)
           ,(funcs-sidebar lib)
           ,(div '[id "content" class "module-docs"] ; TODO: confirm this
              (list (h1 '[id "top" class "anchor"]
                        (h (proplists:get_value 'name lib)))
                    (lists:map (lambda (func) (func-docs app lib func))
                      (sorted-exported-funcs lib))))])]))

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
  (flet ((write-module (module)
           (-> (mod-filepath output-dir module)
               (file:write_file (module-page app module)))))
    (lists:foreach #'write-module/1 (proplists:get_value 'modules app))))

(defun write-libs (output-dir app)
  (flet ((write-lib (lib)
           (file:write_file (mod-filepath output-dir lib)
                            (lib-page app lib))))
    (lists:foreach #'write-lib/1 (proplists:get_value 'libs app))))

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

#|
(defun write-documents (output-dir app)
  (flet ((write-document (document)
           (-> (doc-filepath output-dir document)
               (file:write_file (document-page app document)))))
    (lists:foreach #'write-document/1 (proplists:get_value 'documents app))))
|#

(defun func-name (func)
  (->> (integer_to_list (proplists:get_value 'arity func))
       (++ (h (proplists:get_value 'name func)) "/")))

(defun h (text)
  "Convenient alias for escape-html/1."
  (escape-html text))

(defun escape-html
  "Change special characters into HTML character entities."
  ([x] (when (is_atom x))
   (escape-html (atom_to_list x)))
  ([text]
   (fold-replace text
     '[#("\\&"  "\\&amp;")
       #("<"  "\\&lt;")
       ;; #(">"  "\\&gt;")
       #("\"" "\\&quot;")
       #("'"  "\\&apos;")])))

;; TODO: remove this unless we actually need it.
#|
(defun escape (string)
  "Given a string, return a copy with backticks and double quotes escaped."
  (re:replace string "[`\"]" "\\\\&" '[global #(return list)]))
|#

(defun fold-replace (string pairs)
  (-> (match-lambda
        ([`#(,patt ,replacement) acc]
         (re:replace acc patt replacement '[global #(return list)])))
      (lists:foldl string pairs)))

;; Stolen from Elixir
;; https://github.com/elixir-lang/elixir/blob/944990381f6cadbaf751f2443d485684ba35b6d8/lib/elixir/lib/regex.ex#L601-L619
(defun re-escape (string)
  (re:replace string "[.^$*+?()[{\\\|\s#]" "\\\\&" '[global  #(return list)]))
