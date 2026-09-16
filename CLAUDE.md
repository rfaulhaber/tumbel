# tumblr.el

Interactive Tumblr client for Emacs, in the spirit of twittering-mode and md4rd.
Status: transport, OAuth login, API layer, user cache, NPF renderer, feed
buffers, blog/tag views, the dashboard, inline images and the like/reblog/
follow/delete actions, the Org compose buffer with image uploads, notes and the
activity view, the likes feed, the following/followers tables and the
queue/drafts/inbox views exist (plan
Phases 0 to 9), feeds collapse posts matching the account filters, and Org
files or entries can be posted in place. Left
for later: a `since_id` fast refresh, an auto-refresh timer, avatars in feed
headers, an Info manual.

## Commands

Run inside `nix develop` (direnv picks up `.envrc`).

- `just compile` — byte-compile with warnings as errors
- `just test` — ERT suites in `test/`
- `just lint` — elisp-lint (checkdoc, package-lint, indentation, whitespace)
- `nix flake check` — build + test + lint against a clean store copy
- `nix fmt` — format Nix files

## Layout

- `tumblr.el` — entry point: `defgroup`, version, autoloaded commands; requires
  the other files. Only this file carries `Package-Requires`.
- `tumblr-http.el` — plz wrapper (`tumblr-http-request`), the swappable
  `tumblr-http-backend`, JSON parse/encode, the `tumblr-error` hierarchy.
- `tumblr-auth.el` — consumer credentials (customs or auth-source), the OAuth
  2.0 flow (redirect listener with paste fallback), token file, single-flight
  refresh, `tumblr-login`/`tumblr-logout`.
- `tumblr-org.el` — `tumblr-org-to-npf` (Org subset via `org-element` to NPF
  blocks plus files to upload) and `tumblr-org-from-npf` (text blocks back to
  Org, other blocks as `#+tumblr-block: N` placeholders passed through).
- `tumblr-compose.el` — `tumblr-compose-mode` (derived from `org-mode`):
  `#+blog:`/`#+tags:`/`#+state:`
  header lines then the body in Org; new posts,
  reblogs with a comment, edits of own posts (fetched in fidelity form so
  media survives); `C-c C-c` sends, `C-c C-a` attaches an image.
- `tumblr-post.el` — `tumblr-post-buffer` and `tumblr-post-subtree`: an
  Org file or the entry at point sent as a post through the compose
  request builder; metadata from file keywords or inherited `TUMBLR_*`
  properties, the id written back (`#+tumblr_id:` or `TUMBLR_ID`) so a
  rerun is a PUT.
- `tumblr-notes.el` — notes of a post as a feed of `kind` `note` (`v` in feeds).
- `tumblr-notifications.el` — activity of an own blog (`tumblr-notifications`).
- `tumblr.el` — also holds the `tumblr-dispatch` transient bound to `?`.
- `tumblr-user.el` — cached `/user/info`, `tumblr-user-default-blog`,
  `tumblr-user-own-blog-p`.
- `tumblr-api.el` — URL and query building, auth levels, envelope unwrapping,
  thin endpoint wrappers.
- `tumblr-media.el` — picks image renditions, fetches images a few at a time
  into memory and disk caches, and swaps them into every placeholder (text
  carrying `tumblr-media-url`) via a `display` property.
- `tumblr-npf.el` — renders NPF posts into propertized text; faces; data-only
  buttons that dispatch through the `tumblr-npf-*-function` variables.
- `tumblr-feed.el` — `tumblr-feed-mode`: an ewoc of posts fed by a
  `tumblr-feed-source` (fetch function + cursor), paging, navigation, guards,
  and the like/reblog/follow/delete actions (`tumblr-feed--act` keeps one
  request per post in flight and re-renders the node). A source may set
  `kind`, `render`, `key` and `open` to show items other than posts; the
  post actions refuse those.
- `tumblr-manage.el` — queue, drafts and inbox feeds of own posts; `P`
  publishes now (fidelity fetch + PUT), `A` answers an ask through compose
  (all blocks kept as placeholders, ask layout sent), `M-<up>`/`M-<down>`
  reorder and `S` shuffles the queue.
- `tumblr-lists.el` — `tumblr-lists-mode` (tabulated) for `tumblr-following` and
  `tumblr-followers`; `RET` opens, `o`/`y` browse or copy the blog URL,
  `u` unfollows, `L` pages.
- `tumblr-blog.el` — blog (optionally narrowed to a tag), tag and likes
  sources, the blog header, `tumblr-blog`, `tumblr-tag`, `tumblr-likes`; and
  nothing else.
- `test/NAME-test.el` — one ERT suite per source file.
  `test/tumblr-test-support.el` holds the fake backend and fixture loader;
  `test/fixtures/*.json` are scrubbed API responses.

Dependency order is http → auth → api → user → media → npf → feed → blog and
the other views. Sub-files never
`require` `tumblr.el`.

## Conventions

- Symbol prefix `tumblr-` in every file (the lint recipe sets
  `package-lint-main-file` to `tumblr.el`), `lexical-binding: t`, checkdoc
  docstrings on every definition, two spaces after sentence-ending periods.
- All network traffic goes through `tumblr-http-request`. Tests swap
  `tumblr-http-backend` with `tumblr-test-with-backend` and never touch the
  network.
- Parsed JSON is alists with lists, nil for null and false; never re-encode
  it. Build request bodies explicitly (vectors for arrays, `:false`, `:null`)
  and let `tumblr-http-encode` serialize them. The only server data sent back
  is the `:fidelity` parse used by edit passthrough.
- New runtime dependencies go in two places: `runtimeDeps` in `flake.nix` and
  the `Package-Requires` header in `tumblr.el`. plz needs `curl` on `PATH`.
- Fixtures live under `test/fixtures/` and must be staged in git, otherwise
  `nix flake check` does not see them.
- Text is never edited after insertion: build a block as a string, apply
  faces and buttons to the string, then `insert` it. Writes outside ewoc run
  under `inhibit-read-only`.
- Every network callback checks `buffer-live-p` before touching a buffer;
  feeds keep a `loading` flag so only one page fetch is in flight.
- Lint must pass clean; keep lines within 80 columns.
