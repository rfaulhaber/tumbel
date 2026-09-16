;;; tumbel-api-test.el --- Tests for tumbel-api.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for URL construction, authentication levels, envelope
;; handling and the endpoint wrappers, against the fake backend.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumbel-api)
(require 'tumbel-test-support)

(defconst tumbel-api-test-ok-envelope
  "{\"meta\":{\"status\":200,\"msg\":\"OK\"},\"response\":{\"posts\":[]}}"
  "A minimal successful envelope.")





(ert-deftest tumbel-api-test-blog-identifier ()
  "Names, hostnames, UUIDs and URLs all become path identifiers."
  (should (equal (tumbel-api-blog-identifier "staff") "staff"))
  (should (equal (tumbel-api-blog-identifier " staff.tumblr.com ")
                 "staff.tumblr.com"))
  (should (equal (tumbel-api-blog-identifier "t:AbC-123") "t:AbC-123"))
  (should (equal (tumbel-api-blog-identifier "https://staff.tumblr.com/")
                 "staff.tumblr.com"))
  (should (equal (tumbel-api-blog-identifier "http://www.davidslog.com/x")
                 "www.davidslog.com")))

(ert-deftest tumbel-api-test-query-string ()
  "Query strings keep order, drop nils and encode booleans and spaces."
  (should (equal (tumbel-api-query-string '((npf . t) (limit . 20)
                                            (tag . "a b") (offset . nil)
                                            ("filter" . "raw")
                                            (sort . :false)))
                 "npf=true&limit=20&tag=a%20b&filter=raw&sort=false"))
  (should (equal (tumbel-api-query-string nil) "")))

(ert-deftest tumbel-api-test-blog-info-with-api-key ()
  "Logged out, optional auth sends the API key and no bearer token."
  (tumbel-test-logged-out
    (tumbel-test-with-backend `(("/blog/example.tumblr.com/info" 200
                                 ,(tumbel-test-fixture "blog-info.json")))
      (let ((blog (tumbel-api-blog-info "https://example.tumblr.com/"
                                        :then 'sync))
            (call (tumbel-test-call 0)))
        (should (equal (alist-get 'name blog) "example"))
        (should (equal (alist-get 'uuid blog) "t:AbCdEfGhIjKlMnOpQrStUv"))
        (should (equal (nth 1 call)
                       (concat "https://api.tumblr.com/v2/blog/"
                               "example.tumblr.com/info?api_key=KEY")))
        (should-not (tumbel-test-call-header call "Authorization"))))))

(ert-deftest tumbel-api-test-optional-auth-uses-token ()
  "Logged in, optional auth sends the bearer token and no API key."
  (tumbel-test-logged-in
    (tumbel-test-with-backend `(("." 200
                                 ,(tumbel-test-fixture "blog-info.json")))
      (tumbel-api-blog-info "example" :then 'sync)
      (let ((call (tumbel-test-call 0)))
        (should (equal (nth 1 call)
                       "https://api.tumblr.com/v2/blog/example/info"))
        (should (equal (tumbel-test-call-header call "Authorization")
                       "Bearer TOKEN"))))))

(ert-deftest tumbel-api-test-required-auth-when-logged-out ()
  "Required auth without a token signals before any request is sent."
  (tumbel-test-logged-out
    (tumbel-test-with-backend '(("." 200 "{}"))
      (should-error (tumbel-api-request 'get "/user/dashboard"
                                        :auth 'required :then 'sync)
                    :type 'tumbel-auth-error)
      (should (null tumbel-test-calls)))))

(ert-deftest tumbel-api-test-no-auth ()
  "Auth level nil sends neither key nor token."
  (tumbel-test-logged-in
    (tumbel-test-with-backend `(("." 200 ,tumbel-api-test-ok-envelope))
      (tumbel-api-request 'get "/x" :auth nil :then 'sync)
      (let ((call (tumbel-test-call 0)))
        (should (equal (nth 1 call) "https://api.tumblr.com/v2/x"))
        (should-not (tumbel-test-call-header call "Authorization"))))))

(ert-deftest tumbel-api-test-json-body ()
  "Lisp bodies are encoded as JSON with the matching content type."
  (tumbel-test-logged-in
    (tumbel-test-with-backend
        '(("." 201 "{\"meta\":{\"status\":201,\"msg\":\"Created\"},\
\"response\":{\"id\":\"123\"}}"))
      (let ((response (tumbel-api-request 'post "/blog/example/posts"
                                          :body '((content . [])
                                                  (state . "draft"))
                                          :auth 'required :then 'sync))
            (call (tumbel-test-call 0)))
        (should (equal (alist-get 'id response) "123"))
        (should (eq (car call) 'post))
        (should (equal (tumbel-test-call-key call :body)
                       "{\"content\":[],\"state\":\"draft\"}"))
        (should (equal (tumbel-test-call-header call "Content-Type")
                       "application/json"))
        (should (eq (tumbel-test-call-key call :body-type) 'binary))))))

(ert-deftest tumbel-api-test-prebuilt-body ()
  "A string body with a content type is sent verbatim as binary."
  (tumbel-test-logged-in
    (tumbel-test-with-backend `(("." 201 ,tumbel-api-test-ok-envelope))
      (tumbel-api-request 'post "/x" :body "--b\r\n..."
                          :content-type "multipart/form-data; boundary=b"
                          :then 'sync)
      (let ((call (tumbel-test-call 0)))
        (should (equal (tumbel-test-call-key call :body) "--b\r\n..."))
        (should (eq (tumbel-test-call-key call :body-type) 'binary))
        (should (equal (tumbel-test-call-header call "Content-Type")
                       "multipart/form-data; boundary=b"))))))

(ert-deftest tumbel-api-test-envelope-failure ()
  "A failure reported inside a 2xx envelope still signals."
  (tumbel-test-logged-out
    (tumbel-test-with-backend `(("." 200 ,(tumbel-test-fixture "error-400.json")))
      (let ((err (should-error (tumbel-api-blog-info "example" :then 'sync)
                               :type 'tumbel-api-error)))
        (should (equal (tumbel-api-error-status err) 400))
        (should (equal (tumbel-api-error-message err) "Bad Request"))))))

(ert-deftest tumbel-api-test-async ()
  "Asynchronous wrappers deliver the transformed response to THEN."
  (tumbel-test-logged-out
    (tumbel-test-with-backend `(("." 200 ,(tumbel-test-fixture "blog-info.json")))
      (let (got)
        (tumbel-api-blog-info "example" :then (lambda (blog) (setq got blog)))
        (should (equal (alist-get 'title got) "Example Blog"))))))

(ert-deftest tumbel-api-test-async-envelope-failure-goes-to-else ()
  "Envelope failures on asynchronous requests reach ELSE."
  (tumbel-test-logged-out
    (tumbel-test-with-backend `(("." 200 ,(tumbel-test-fixture "error-401.json")))
      (let (got called)
        (tumbel-api-blog-info "example"
                              :then (lambda (_) (setq called t))
                              :else (lambda (err) (setq got err)))
        (should-not called)
        (should (eq (car got) 'tumbel-api-error))
        (should (equal (tumbel-api-error-status got) 401))))))

(ert-deftest tumbel-api-test-blog-posts-params ()
  "Blog posts are requested in NPF with the caller's parameters."
  (tumbel-test-logged-out
    (tumbel-test-with-backend `(("." 200 ,tumbel-api-test-ok-envelope))
      (let ((response (tumbel-api-blog-posts "example"
                                             :params '((limit . 5)
                                                       (offset . 20))
                                             :then 'sync)))
        (should (null (alist-get 'posts response)))
        (should (equal (nth 1 (tumbel-test-call 0))
                       (concat "https://api.tumblr.com/v2/blog/example/posts"
                               "?npf=true&limit=5&offset=20&api_key=KEY")))))))

(ert-deftest tumbel-api-test-blog-post-url ()
  "A single post is fetched in NPF."
  (tumbel-test-logged-out
    (tumbel-test-with-backend `(("." 200 ,tumbel-api-test-ok-envelope))
      (tumbel-api-blog-post "example" "123" :then 'sync)
      (should (equal (nth 1 (tumbel-test-call 0))
                     (concat "https://api.tumblr.com/v2/blog/example/posts/123"
                             "?post_format=npf&api_key=KEY"))))))

(ert-deftest tumbel-api-test-tagged-wraps-bare-array ()
  "The tagged endpoint's bare array is wrapped under `posts'."
  (tumbel-test-logged-out
    (tumbel-test-with-backend
        '(("/tagged" 200 "{\"meta\":{\"status\":200,\"msg\":\"OK\"},\
\"response\":[{\"type\":\"blocks\",\"id_string\":\"1\"}]}"))
      (let ((response (tumbel-api-tagged "emacs lisp" :then 'sync)))
        (should (equal (alist-get 'id_string
                                  (car (alist-get 'posts response)))
                       "1"))
        (should (string-prefix-p
                 "https://api.tumblr.com/v2/tagged?tag=emacs%20lisp&npf=true"
                 (nth 1 (tumbel-test-call 0))))))))

(ert-deftest tumbel-api-test-wrap-posts ()
  "Wrapping tolerates empty, bare and already wrapped responses."
  (should (equal (tumbel-api--wrap-posts nil) '((posts))))
  (should (equal (tumbel-api--wrap-posts '((posts . (1)))) '((posts . (1)))))
  (should (equal (tumbel-api--wrap-posts '(((type . "blocks"))))
                 '((posts . (((type . "blocks"))))))))

(ert-deftest tumbel-api-test-next-params ()
  "Pagination parameters come from the `_links' entry."
  (should (equal (tumbel-api-next-params
                  '((_links . ((next . ((href . "/v2/x")
                                        (method . "GET")
                                        (query_params
                                         . ((before_timestamp . "1")
                                            (id . "2")))))))))
                 '((before_timestamp . "1") (id . "2"))))
  (should (null (tumbel-api-next-params '((posts))))))

(ert-deftest tumbel-api-test-json-body-is-utf-8 ()
  "Non-ASCII text is sent as UTF-8 bytes."
  (tumbel-test-logged-in
    (tumbel-test-with-backend `(("." 201 ,tumbel-api-test-ok-envelope))
      (tumbel-api-request 'post "/x" :body '((text . "café")) :then 'sync)
      (let ((body (tumbel-test-call-key (tumbel-test-call 0) :body)))
        (should-not (multibyte-string-p body))
        (should (equal (decode-coding-string body 'utf-8)
                       "{\"text\":\"café\"}"))))))
(defmacro tumbel-api-test-with-refreshable-token (&rest body)
  "Run BODY logged in with a token that a refresh replaces."
  (declare (indent 0))
  `(let ((tumbel-consumer-key "KEY")
         (tumbel-consumer-secret "SECRET")
         (tumbel-api-test-token "TOKEN")
         (tumbel-api-test-refreshes 0))
     (cl-letf (((symbol-function 'tumbel-auth-logged-in-p) (lambda () t))
               ((symbol-function 'tumbel-auth-access-token)
                (lambda () tumbel-api-test-token))
               ((symbol-function 'tumbel-auth-refresh)
                (lambda ()
                  (cl-incf tumbel-api-test-refreshes)
                  (setq tumbel-api-test-token "TOKEN2")))
               ((symbol-function 'tumbel-auth-refresh-async)
                (lambda (then _else)
                  (cl-incf tumbel-api-test-refreshes)
                  (setq tumbel-api-test-token "TOKEN2")
                  (funcall then))))
       ,@body)))

(defvar tumbel-api-test-token nil
  "Token handed out by the stubbed `tumbel-auth-access-token'.")

(defvar tumbel-api-test-refreshes 0
  "Number of refreshes the stubs performed.")

(ert-deftest tumbel-api-test-sync-401-refreshes-and-retries ()
  "A 401 with a bearer token triggers one refresh and a retry."
  (tumbel-api-test-with-refreshable-token
    (tumbel-test-with-backend
        `(("/user/info" 401 ,(tumbel-test-fixture "error-401.json") once)
          ("/user/info" 200 ,(tumbel-test-fixture "user-info.json")))
      (should (equal (alist-get 'name (tumbel-api-user-info :then 'sync))
                     "example"))
      (should (equal tumbel-api-test-refreshes 1))
      (should (equal (length tumbel-test-calls) 2))
      (should (equal (tumbel-test-call-header (tumbel-test-call 0)
                                              "Authorization")
                     "Bearer TOKEN2"))
      (should (equal (tumbel-test-call-header (tumbel-test-call 1)
                                              "Authorization")
                     "Bearer TOKEN")))))

(ert-deftest tumbel-api-test-async-401-refreshes-and-retries ()
  "The asynchronous path refreshes through the shared guard."
  (tumbel-api-test-with-refreshable-token
    (tumbel-test-with-backend
        `(("/user/info" 401 ,(tumbel-test-fixture "error-401.json") once)
          ("/user/info" 200 ,(tumbel-test-fixture "user-info.json")))
      (let (got)
        (tumbel-api-user-info :then (lambda (user) (setq got user)))
        (should (equal (alist-get 'name got) "example"))
        (should (equal tumbel-api-test-refreshes 1))
        (should (equal (length tumbel-test-calls) 2))))))

(ert-deftest tumbel-api-test-401-retried-once ()
  "A second 401 is reported instead of looping."
  (tumbel-api-test-with-refreshable-token
    (tumbel-test-with-backend
        `(("/user/info" 401 ,(tumbel-test-fixture "error-401.json")))
      (let ((err (should-error (tumbel-api-user-info :then 'sync)
                               :type 'tumbel-api-error)))
        (should (equal (tumbel-api-error-status err) 401)))
      (should (equal tumbel-api-test-refreshes 1))
      (should (equal (length tumbel-test-calls) 2)))))

(ert-deftest tumbel-api-test-401-with-api-key-is-not-retried ()
  "Without a bearer token a 401 is just an error."
  (tumbel-test-logged-out
    (tumbel-test-with-backend
        `(("." 401 ,(tumbel-test-fixture "error-401.json")))
      (should-error (tumbel-api-blog-info "example" :then 'sync)
                    :type 'tumbel-api-error)
      (should (equal (length tumbel-test-calls) 1)))))

(ert-deftest tumbel-api-test-error-body ()
  "The parsed body travels with API errors."
  (tumbel-test-logged-out
    (tumbel-test-with-backend
        '(("." 400 "{\"error\":\"invalid_grant\",\"error_description\":\"x\"}"))
      (let ((err (should-error (tumbel-api-request 'get "/x" :then 'sync)
                               :type 'tumbel-api-error)))
        (should (equal (alist-get 'error_description (tumbel-api-error-body err))
                       "x"))))))

(ert-deftest tumbel-api-test-dashboard-and-user-wrappers ()
  "Dashboard and user info require the token and request NPF."
  (tumbel-test-logged-in
    (tumbel-test-with-backend `(("." 200 ,tumbel-api-test-ok-envelope))
      (tumbel-api-dashboard :params '((offset . 20)) :then 'sync)
      (should (equal (nth 1 (tumbel-test-call 0))
                     "https://api.tumblr.com/v2/user/dashboard?npf=true&offset=20"))
      (should (equal (tumbel-test-call-header (tumbel-test-call 0)
                                              "Authorization")
                     "Bearer TOKEN"))))
  (tumbel-test-logged-out
    (tumbel-test-with-backend '(("." 200 "{}"))
      (should-error (tumbel-api-dashboard :then 'sync)
                    :type 'tumbel-auth-error))))
(ert-deftest tumbel-api-test-form-body ()
  "Form bodies are encoded strictly with the matching content type."
  (tumbel-test-logged-in
    (tumbel-test-with-backend `(("." 200 ,tumbel-api-test-ok-envelope))
      (tumbel-api-request 'post "/x" :form '((url . "https://a/b?c=d e"))
                          :then 'sync)
      (let ((call (tumbel-test-call 0)))
        (should (equal (tumbel-test-call-key call :body)
                       "url=https%3A%2F%2Fa%2Fb%3Fc%3Dd%20e"))
        (should (equal (tumbel-test-call-header call "Content-Type")
                       "application/x-www-form-urlencoded"))))))

(ert-deftest tumbel-api-test-action-wrappers ()
  "The action wrappers hit the documented paths with the right bodies."
  (tumbel-test-logged-in
    (tumbel-test-with-backend `(("." 200 ,tumbel-api-test-ok-envelope))
      (let ((post '((id_string . "7") (reblog_key . "k")
                    (blog . ((name . "b") (uuid . "t:B"))))))
        (tumbel-api-like post :then 'sync)
        (should (equal (nth 1 (tumbel-test-call 0))
                       "https://api.tumblr.com/v2/user/like"))
        (should (equal (tumbel-test-call-key (tumbel-test-call 0) :body)
                       "id=7&reblog_key=k"))
        (tumbel-api-unlike post :then 'sync)
        (should (string-suffix-p "/user/unlike" (nth 1 (tumbel-test-call 0))))
        (tumbel-api-follow "https://b.tumblr.com/" :then 'sync)
        (should (string-suffix-p "/user/follow" (nth 1 (tumbel-test-call 0))))
        (tumbel-api-unfollow "https://b.tumblr.com/" :then 'sync)
        (should (string-suffix-p "/user/unfollow" (nth 1 (tumbel-test-call 0))))
        (tumbel-api-delete-post "b" "7" :then 'sync)
        (should (string-suffix-p "/blog/b/post/delete"
                                 (nth 1 (tumbel-test-call 0))))
        (should (equal (tumbel-test-call-key (tumbel-test-call 0) :body)
                       "id=7"))
        (tumbel-api-edit-post "b" "7" '((content . [])) :then 'sync)
        (should (eq (car (tumbel-test-call 0)) 'put))
        (should (string-suffix-p "/blog/b/posts/7" (nth 1 (tumbel-test-call 0))))
        (tumbel-api-reblog "mine" post :content [((type . "text") (text . "hi"))]
                           :then 'sync)
        (should (equal (tumbel-test-call-key (tumbel-test-call 0) :body)
                       (concat "{\"content\":[{\"type\":\"text\",\"text\":\"hi\"}],"
                               "\"parent_tumblelog_uuid\":\"t:B\","
                               "\"parent_post_id\":\"7\",\"reblog_key\":\"k\"}")))))))

(ert-deftest tumbel-api-test-post-blog-uuid ()
  "The blog UUID comes from the post, or from cached blog info."
  (let ((tumbel-api--blog-uuids (make-hash-table :test #'equal)))
    (should (equal (tumbel-api-post-blog-uuid '((blog . ((uuid . "t:X")))))
                   "t:X"))
    (should (equal (tumbel-api-post-blog-uuid '((tumblelog_uuid . "t:Y")))
                   "t:Y"))
    (tumbel-test-logged-out
      (tumbel-test-with-backend `(("/blog/example/info" 200
                                   ,(tumbel-test-fixture "blog-info.json")))
        (should (equal (tumbel-api-post-blog-uuid '((blog_name . "example")))
                       "t:AbCdEfGhIjKlMnOpQrStUv"))
        (should (equal (tumbel-api-post-blog-uuid '((blog_name . "example")))
                       "t:AbCdEfGhIjKlMnOpQrStUv"))
        (should (equal (length tumbel-test-calls) 1))))))
(ert-deftest tumbel-api-test-only-nil-params ()
  "Parameters that are all nil leave the URL without a query string."
  (tumbel-test-logged-in
    (tumbel-test-with-backend `(("." 200 ,tumbel-api-test-ok-envelope))
      (tumbel-api-request 'get "/x" :params '((before . nil)) :auth nil
                          :then 'sync)
      (should (equal (nth 1 (tumbel-test-call 0))
                     "https://api.tumblr.com/v2/x")))))
(provide 'tumbel-api-test)
;;; tumbel-api-test.el ends here
