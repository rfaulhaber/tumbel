;;; tumblr-api-test.el --- Tests for tumblr-api.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for URL construction, authentication levels, envelope
;; handling and the endpoint wrappers, against the fake backend.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumblr-api)
(require 'tumblr-test-support)

(defconst tumblr-api-test-ok-envelope
  "{\"meta\":{\"status\":200,\"msg\":\"OK\"},\"response\":{\"posts\":[]}}"
  "A minimal successful envelope.")





(ert-deftest tumblr-api-test-blog-identifier ()
  "Names, hostnames, UUIDs and URLs all become path identifiers."
  (should (equal (tumblr-api-blog-identifier "staff") "staff"))
  (should (equal (tumblr-api-blog-identifier " staff.tumblr.com ")
                 "staff.tumblr.com"))
  (should (equal (tumblr-api-blog-identifier "t:AbC-123") "t:AbC-123"))
  (should (equal (tumblr-api-blog-identifier "https://staff.tumblr.com/")
                 "staff.tumblr.com"))
  (should (equal (tumblr-api-blog-identifier "http://www.davidslog.com/x")
                 "www.davidslog.com")))

(ert-deftest tumblr-api-test-query-string ()
  "Query strings keep order, drop nils and encode booleans and spaces."
  (should (equal (tumblr-api-query-string '((npf . t) (limit . 20)
                                            (tag . "a b") (offset . nil)
                                            ("filter" . "raw")
                                            (sort . :false)))
                 "npf=true&limit=20&tag=a%20b&filter=raw&sort=false"))
  (should (equal (tumblr-api-query-string nil) "")))

(ert-deftest tumblr-api-test-blog-info-with-api-key ()
  "Logged out, optional auth sends the API key and no bearer token."
  (tumblr-test-logged-out
    (tumblr-test-with-backend `(("/blog/example.tumblr.com/info" 200
                                 ,(tumblr-test-fixture "blog-info.json")))
      (let ((blog (tumblr-api-blog-info "https://example.tumblr.com/"
                                        :then 'sync))
            (call (tumblr-test-call 0)))
        (should (equal (alist-get 'name blog) "example"))
        (should (equal (alist-get 'uuid blog) "t:AbCdEfGhIjKlMnOpQrStUv"))
        (should (equal (nth 1 call)
                       (concat "https://api.tumblr.com/v2/blog/"
                               "example.tumblr.com/info?api_key=KEY")))
        (should-not (tumblr-test-call-header call "Authorization"))))))

(ert-deftest tumblr-api-test-optional-auth-uses-token ()
  "Logged in, optional auth sends the bearer token and no API key."
  (tumblr-test-logged-in
    (tumblr-test-with-backend `(("." 200
                                 ,(tumblr-test-fixture "blog-info.json")))
      (tumblr-api-blog-info "example" :then 'sync)
      (let ((call (tumblr-test-call 0)))
        (should (equal (nth 1 call)
                       "https://api.tumblr.com/v2/blog/example/info"))
        (should (equal (tumblr-test-call-header call "Authorization")
                       "Bearer TOKEN"))))))

(ert-deftest tumblr-api-test-required-auth-when-logged-out ()
  "Required auth without a token signals before any request is sent."
  (tumblr-test-logged-out
    (tumblr-test-with-backend '(("." 200 "{}"))
      (should-error (tumblr-api-request 'get "/user/dashboard"
                                        :auth 'required :then 'sync)
                    :type 'tumblr-auth-error)
      (should (null tumblr-test-calls)))))

(ert-deftest tumblr-api-test-no-auth ()
  "Auth level nil sends neither key nor token."
  (tumblr-test-logged-in
    (tumblr-test-with-backend `(("." 200 ,tumblr-api-test-ok-envelope))
      (tumblr-api-request 'get "/x" :auth nil :then 'sync)
      (let ((call (tumblr-test-call 0)))
        (should (equal (nth 1 call) "https://api.tumblr.com/v2/x"))
        (should-not (tumblr-test-call-header call "Authorization"))))))

(ert-deftest tumblr-api-test-json-body ()
  "Lisp bodies are encoded as JSON with the matching content type."
  (tumblr-test-logged-in
    (tumblr-test-with-backend
        '(("." 201 "{\"meta\":{\"status\":201,\"msg\":\"Created\"},\
\"response\":{\"id\":\"123\"}}"))
      (let ((response (tumblr-api-request 'post "/blog/example/posts"
                                          :body '((content . [])
                                                  (state . "draft"))
                                          :auth 'required :then 'sync))
            (call (tumblr-test-call 0)))
        (should (equal (alist-get 'id response) "123"))
        (should (eq (car call) 'post))
        (should (equal (tumblr-test-call-key call :body)
                       "{\"content\":[],\"state\":\"draft\"}"))
        (should (equal (tumblr-test-call-header call "Content-Type")
                       "application/json"))
        (should (eq (tumblr-test-call-key call :body-type) 'binary))))))

(ert-deftest tumblr-api-test-prebuilt-body ()
  "A string body with a content type is sent verbatim as binary."
  (tumblr-test-logged-in
    (tumblr-test-with-backend `(("." 201 ,tumblr-api-test-ok-envelope))
      (tumblr-api-request 'post "/x" :body "--b\r\n..."
                          :content-type "multipart/form-data; boundary=b"
                          :then 'sync)
      (let ((call (tumblr-test-call 0)))
        (should (equal (tumblr-test-call-key call :body) "--b\r\n..."))
        (should (eq (tumblr-test-call-key call :body-type) 'binary))
        (should (equal (tumblr-test-call-header call "Content-Type")
                       "multipart/form-data; boundary=b"))))))

(ert-deftest tumblr-api-test-envelope-failure ()
  "A failure reported inside a 2xx envelope still signals."
  (tumblr-test-logged-out
    (tumblr-test-with-backend `(("." 200 ,(tumblr-test-fixture "error-400.json")))
      (let ((err (should-error (tumblr-api-blog-info "example" :then 'sync)
                               :type 'tumblr-api-error)))
        (should (equal (tumblr-api-error-status err) 400))
        (should (equal (tumblr-api-error-message err) "Bad Request"))))))

(ert-deftest tumblr-api-test-async ()
  "Asynchronous wrappers deliver the transformed response to THEN."
  (tumblr-test-logged-out
    (tumblr-test-with-backend `(("." 200 ,(tumblr-test-fixture "blog-info.json")))
      (let (got)
        (tumblr-api-blog-info "example" :then (lambda (blog) (setq got blog)))
        (should (equal (alist-get 'title got) "Example Blog"))))))

(ert-deftest tumblr-api-test-async-envelope-failure-goes-to-else ()
  "Envelope failures on asynchronous requests reach ELSE."
  (tumblr-test-logged-out
    (tumblr-test-with-backend `(("." 200 ,(tumblr-test-fixture "error-401.json")))
      (let (got called)
        (tumblr-api-blog-info "example"
                              :then (lambda (_) (setq called t))
                              :else (lambda (err) (setq got err)))
        (should-not called)
        (should (eq (car got) 'tumblr-api-error))
        (should (equal (tumblr-api-error-status got) 401))))))

(ert-deftest tumblr-api-test-blog-posts-params ()
  "Blog posts are requested in NPF with the caller's parameters."
  (tumblr-test-logged-out
    (tumblr-test-with-backend `(("." 200 ,tumblr-api-test-ok-envelope))
      (let ((response (tumblr-api-blog-posts "example"
                                             :params '((limit . 5)
                                                       (offset . 20))
                                             :then 'sync)))
        (should (null (alist-get 'posts response)))
        (should (equal (nth 1 (tumblr-test-call 0))
                       (concat "https://api.tumblr.com/v2/blog/example/posts"
                               "?npf=true&limit=5&offset=20&api_key=KEY")))))))

(ert-deftest tumblr-api-test-blog-post-url ()
  "A single post is fetched in NPF."
  (tumblr-test-logged-out
    (tumblr-test-with-backend `(("." 200 ,tumblr-api-test-ok-envelope))
      (tumblr-api-blog-post "example" "123" :then 'sync)
      (should (equal (nth 1 (tumblr-test-call 0))
                     (concat "https://api.tumblr.com/v2/blog/example/posts/123"
                             "?post_format=npf&api_key=KEY"))))))

(ert-deftest tumblr-api-test-tagged-wraps-bare-array ()
  "The tagged endpoint's bare array is wrapped under `posts'."
  (tumblr-test-logged-out
    (tumblr-test-with-backend
        '(("/tagged" 200 "{\"meta\":{\"status\":200,\"msg\":\"OK\"},\
\"response\":[{\"type\":\"blocks\",\"id_string\":\"1\"}]}"))
      (let ((response (tumblr-api-tagged "emacs lisp" :then 'sync)))
        (should (equal (alist-get 'id_string
                                  (car (alist-get 'posts response)))
                       "1"))
        (should (string-prefix-p
                 "https://api.tumblr.com/v2/tagged?tag=emacs%20lisp&npf=true"
                 (nth 1 (tumblr-test-call 0))))))))

(ert-deftest tumblr-api-test-wrap-posts ()
  "Wrapping tolerates empty, bare and already wrapped responses."
  (should (equal (tumblr-api--wrap-posts nil) '((posts))))
  (should (equal (tumblr-api--wrap-posts '((posts . (1)))) '((posts . (1)))))
  (should (equal (tumblr-api--wrap-posts '(((type . "blocks"))))
                 '((posts . (((type . "blocks"))))))))

(ert-deftest tumblr-api-test-next-params ()
  "Pagination parameters come from the `_links' entry."
  (should (equal (tumblr-api-next-params
                  '((_links . ((next . ((href . "/v2/x")
                                        (method . "GET")
                                        (query_params
                                         . ((before_timestamp . "1")
                                            (id . "2")))))))))
                 '((before_timestamp . "1") (id . "2"))))
  (should (null (tumblr-api-next-params '((posts))))))

(ert-deftest tumblr-api-test-json-body-is-utf-8 ()
  "Non-ASCII text is sent as UTF-8 bytes."
  (tumblr-test-logged-in
    (tumblr-test-with-backend `(("." 201 ,tumblr-api-test-ok-envelope))
      (tumblr-api-request 'post "/x" :body '((text . "café")) :then 'sync)
      (let ((body (tumblr-test-call-key (tumblr-test-call 0) :body)))
        (should-not (multibyte-string-p body))
        (should (equal (decode-coding-string body 'utf-8)
                       "{\"text\":\"café\"}"))))))
(defmacro tumblr-api-test-with-refreshable-token (&rest body)
  "Run BODY logged in with a token that a refresh replaces."
  (declare (indent 0))
  `(let ((tumblr-consumer-key "KEY")
         (tumblr-consumer-secret "SECRET")
         (tumblr-api-test-token "TOKEN")
         (tumblr-api-test-refreshes 0))
     (cl-letf (((symbol-function 'tumblr-auth-logged-in-p) (lambda () t))
               ((symbol-function 'tumblr-auth-access-token)
                (lambda () tumblr-api-test-token))
               ((symbol-function 'tumblr-auth-refresh)
                (lambda ()
                  (cl-incf tumblr-api-test-refreshes)
                  (setq tumblr-api-test-token "TOKEN2")))
               ((symbol-function 'tumblr-auth-refresh-async)
                (lambda (then _else)
                  (cl-incf tumblr-api-test-refreshes)
                  (setq tumblr-api-test-token "TOKEN2")
                  (funcall then))))
       ,@body)))

(defvar tumblr-api-test-token nil
  "Token handed out by the stubbed `tumblr-auth-access-token'.")

(defvar tumblr-api-test-refreshes 0
  "Number of refreshes the stubs performed.")

(ert-deftest tumblr-api-test-sync-401-refreshes-and-retries ()
  "A 401 with a bearer token triggers one refresh and a retry."
  (tumblr-api-test-with-refreshable-token
    (tumblr-test-with-backend
        `(("/user/info" 401 ,(tumblr-test-fixture "error-401.json") once)
          ("/user/info" 200 ,(tumblr-test-fixture "user-info.json")))
      (should (equal (alist-get 'name (tumblr-api-user-info :then 'sync))
                     "example"))
      (should (equal tumblr-api-test-refreshes 1))
      (should (equal (length tumblr-test-calls) 2))
      (should (equal (tumblr-test-call-header (tumblr-test-call 0)
                                              "Authorization")
                     "Bearer TOKEN2"))
      (should (equal (tumblr-test-call-header (tumblr-test-call 1)
                                              "Authorization")
                     "Bearer TOKEN")))))

(ert-deftest tumblr-api-test-async-401-refreshes-and-retries ()
  "The asynchronous path refreshes through the shared guard."
  (tumblr-api-test-with-refreshable-token
    (tumblr-test-with-backend
        `(("/user/info" 401 ,(tumblr-test-fixture "error-401.json") once)
          ("/user/info" 200 ,(tumblr-test-fixture "user-info.json")))
      (let (got)
        (tumblr-api-user-info :then (lambda (user) (setq got user)))
        (should (equal (alist-get 'name got) "example"))
        (should (equal tumblr-api-test-refreshes 1))
        (should (equal (length tumblr-test-calls) 2))))))

(ert-deftest tumblr-api-test-401-retried-once ()
  "A second 401 is reported instead of looping."
  (tumblr-api-test-with-refreshable-token
    (tumblr-test-with-backend
        `(("/user/info" 401 ,(tumblr-test-fixture "error-401.json")))
      (let ((err (should-error (tumblr-api-user-info :then 'sync)
                               :type 'tumblr-api-error)))
        (should (equal (tumblr-api-error-status err) 401)))
      (should (equal tumblr-api-test-refreshes 1))
      (should (equal (length tumblr-test-calls) 2)))))

(ert-deftest tumblr-api-test-401-with-api-key-is-not-retried ()
  "Without a bearer token a 401 is just an error."
  (tumblr-test-logged-out
    (tumblr-test-with-backend
        `(("." 401 ,(tumblr-test-fixture "error-401.json")))
      (should-error (tumblr-api-blog-info "example" :then 'sync)
                    :type 'tumblr-api-error)
      (should (equal (length tumblr-test-calls) 1)))))

(ert-deftest tumblr-api-test-error-body ()
  "The parsed body travels with API errors."
  (tumblr-test-logged-out
    (tumblr-test-with-backend
        '(("." 400 "{\"error\":\"invalid_grant\",\"error_description\":\"x\"}"))
      (let ((err (should-error (tumblr-api-request 'get "/x" :then 'sync)
                               :type 'tumblr-api-error)))
        (should (equal (alist-get 'error_description (tumblr-api-error-body err))
                       "x"))))))

(ert-deftest tumblr-api-test-dashboard-and-user-wrappers ()
  "Dashboard and user info require the token and request NPF."
  (tumblr-test-logged-in
    (tumblr-test-with-backend `(("." 200 ,tumblr-api-test-ok-envelope))
      (tumblr-api-dashboard :params '((offset . 20)) :then 'sync)
      (should (equal (nth 1 (tumblr-test-call 0))
                     "https://api.tumblr.com/v2/user/dashboard?npf=true&offset=20"))
      (should (equal (tumblr-test-call-header (tumblr-test-call 0)
                                              "Authorization")
                     "Bearer TOKEN"))))
  (tumblr-test-logged-out
    (tumblr-test-with-backend '(("." 200 "{}"))
      (should-error (tumblr-api-dashboard :then 'sync)
                    :type 'tumblr-auth-error))))
(ert-deftest tumblr-api-test-form-body ()
  "Form bodies are encoded strictly with the matching content type."
  (tumblr-test-logged-in
    (tumblr-test-with-backend `(("." 200 ,tumblr-api-test-ok-envelope))
      (tumblr-api-request 'post "/x" :form '((url . "https://a/b?c=d e"))
                          :then 'sync)
      (let ((call (tumblr-test-call 0)))
        (should (equal (tumblr-test-call-key call :body)
                       "url=https%3A%2F%2Fa%2Fb%3Fc%3Dd%20e"))
        (should (equal (tumblr-test-call-header call "Content-Type")
                       "application/x-www-form-urlencoded"))))))

(ert-deftest tumblr-api-test-action-wrappers ()
  "The action wrappers hit the documented paths with the right bodies."
  (tumblr-test-logged-in
    (tumblr-test-with-backend `(("." 200 ,tumblr-api-test-ok-envelope))
      (let ((post '((id_string . "7") (reblog_key . "k")
                    (blog . ((name . "b") (uuid . "t:B"))))))
        (tumblr-api-like post :then 'sync)
        (should (equal (nth 1 (tumblr-test-call 0))
                       "https://api.tumblr.com/v2/user/like"))
        (should (equal (tumblr-test-call-key (tumblr-test-call 0) :body)
                       "id=7&reblog_key=k"))
        (tumblr-api-unlike post :then 'sync)
        (should (string-suffix-p "/user/unlike" (nth 1 (tumblr-test-call 0))))
        (tumblr-api-follow "https://b.tumblr.com/" :then 'sync)
        (should (string-suffix-p "/user/follow" (nth 1 (tumblr-test-call 0))))
        (tumblr-api-unfollow "https://b.tumblr.com/" :then 'sync)
        (should (string-suffix-p "/user/unfollow" (nth 1 (tumblr-test-call 0))))
        (tumblr-api-delete-post "b" "7" :then 'sync)
        (should (string-suffix-p "/blog/b/post/delete"
                                 (nth 1 (tumblr-test-call 0))))
        (should (equal (tumblr-test-call-key (tumblr-test-call 0) :body)
                       "id=7"))
        (tumblr-api-edit-post "b" "7" '((content . [])) :then 'sync)
        (should (eq (car (tumblr-test-call 0)) 'put))
        (should (string-suffix-p "/blog/b/posts/7" (nth 1 (tumblr-test-call 0))))
        (tumblr-api-reblog "mine" post :content [((type . "text") (text . "hi"))]
                           :then 'sync)
        (should (equal (tumblr-test-call-key (tumblr-test-call 0) :body)
                       (concat "{\"content\":[{\"type\":\"text\",\"text\":\"hi\"}],"
                               "\"parent_tumblelog_uuid\":\"t:B\","
                               "\"parent_post_id\":\"7\",\"reblog_key\":\"k\"}")))))))

(ert-deftest tumblr-api-test-post-blog-uuid ()
  "The blog UUID comes from the post, or from cached blog info."
  (let ((tumblr-api--blog-uuids (make-hash-table :test #'equal)))
    (should (equal (tumblr-api-post-blog-uuid '((blog . ((uuid . "t:X")))))
                   "t:X"))
    (should (equal (tumblr-api-post-blog-uuid '((tumblelog_uuid . "t:Y")))
                   "t:Y"))
    (tumblr-test-logged-out
      (tumblr-test-with-backend `(("/blog/example/info" 200
                                   ,(tumblr-test-fixture "blog-info.json")))
        (should (equal (tumblr-api-post-blog-uuid '((blog_name . "example")))
                       "t:AbCdEfGhIjKlMnOpQrStUv"))
        (should (equal (tumblr-api-post-blog-uuid '((blog_name . "example")))
                       "t:AbCdEfGhIjKlMnOpQrStUv"))
        (should (equal (length tumblr-test-calls) 1))))))
(ert-deftest tumblr-api-test-only-nil-params ()
  "Parameters that are all nil leave the URL without a query string."
  (tumblr-test-logged-in
    (tumblr-test-with-backend `(("." 200 ,tumblr-api-test-ok-envelope))
      (tumblr-api-request 'get "/x" :params '((before . nil)) :auth nil
                          :then 'sync)
      (should (equal (nth 1 (tumblr-test-call 0))
                     "https://api.tumblr.com/v2/x")))))
(provide 'tumblr-api-test)
;;; tumblr-api-test.el ends here
