;;; w3-init    [sj--96/08/10]

(autoload 'w3-preview-this-buffer "w3" "WWW Previewer" t)
(autoload 'w3-follow-url-at-point "w3" "Find document at pt" t)
(autoload 'w3 "w3" "WWW Browser" t)
(autoload 'w3-open-local "w3" "Open local file for WWW browsing" t)
(autoload 'w3-fetch "w3" "Open remote file for WWW browsing" t)
(autoload 'w3-use-hotlist "w3" "Use shortcuts to view WWW docs" t)
(autoload 'w3-show-hotlist "w3" "Use shortcuts to view WWW docs" t)
(autoload 'w3-follow-link "w3" "Follow a hypertext link." t)
(autoload 'w3-batch-fetch "w3" "Batch retrieval of URLs" t)
(autoload 'url-get-url-at-point "url" "Find the url under the cursor" nil)
(autoload 'url-file-attributes  "url" "File attributes of a URL" nil)
(autoload 'url-popup-info "url" "Get info on a URL" t)
(autoload 'url-retrieve   "url" "Retrieve a URL" nil)
(autoload 'url-buffer-visiting "url" "Find buffer visiting a URL." nil)
(autoload 'gopher-dispatch-object "gopher" "Fetch gopher dir" t)


;;; Local Variables:
;;; sj/recompile-file:t
;;; End:
