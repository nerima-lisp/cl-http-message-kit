# Changelog

## 2.0.0

### Breaking changes

- HPACK encoding and decoding, along with QPACK's Huffman, integer, and
  header-name validation primitives, now use `cl-hpack-codec-kit`. Applications
  must make that system available to ASDF before loading `cl-http-message-kit`.
- The ASDF and Nix package version is now `2.0.0`.

### Migration

- QPACK encoding adds `qpack-prepare-set-capacity`,
  `qpack-prepare-insert-with-name-reference`,
  `qpack-prepare-insert-with-literal-name`, `qpack-prepare-duplicate`, and
  `qpack-prepare-header-block`, with `qpack-commit-encoding` and
  `qpack-abort-encoding`. Prepare before writing and commit after a complete
  successful write; abort only when the transport confirms that zero bytes
  were sent. Hold the connection's QPACK serialization lock across preparation,
  the complete write, and commit or abort. Close the connection and discard the
  encoder and decoder QPACK contexts if any bytes may have been sent or
  delivery is uncertain. Existing state-mutating `qpack-encode-*` operations
  still update context immediately. Header-block preparation requires
  `:stream-id` when `:use-dynamic-p t`. See [QPACK encoder transactions](README.md#qpack-encoder-transactions)
  for transport locking and write guidance.
- `qpack-context-set-max-table-capacity` and its SETF accessor update the
  context maximum and backing dynamic-table limit together without changing
  current capacity. To lower the maximum below current capacity, first reduce
  current capacity with `qpack-context-set-capacity`; an invalid maximum is
  rejected without changing either limit. Changing the maximum makes prepared
  encodings stale; committing one signals `qpack-encoding-transaction-error`
  with reason `:stale`. Prepare a new encoding from the current context.
- Explicit request targets are accepted when they contain visible ASCII. Their
  form-specific syntax is checked by the HTTP parser on input and by
  `http1-serialize-request` or `http-validate-header-section` before output.
- Request-target character errors now return the original target through
  `http-protocol-error-detail`, instead of `(:target target :reason :characters)`.
  Form-specific errors keep their structured details.
- Response constructors accept bodies and trailers for 1xx, 204, and 304
  statuses. HTTP/1 serialization rejects them; HTTP/2 and HTTP/3 transport
  adapters must apply the same status rule when writing response headers, DATA
  frames, and trailer sections. See [Design notes](README.md#design-notes).
- Request and response constructors no longer reject prohibited trailer field
  names. HTTP/1 trailer encoding and decoding and HTTP/2 or HTTP/3
  `http-validate-header-section` calls with `:trailers-p t` reject them.
- `http-header-name` and `http-header-content` expose their stored values, and
  `setf` stores and returns the exact assigned value. Message constructors
  continue to copy and revalidate headers. Request and response header and
  trailer accessors return a new list of copied header objects; changing a
  returned header does not change the message.
- `make-http-response-stream` retains the supplied body function. The adapter
  supplied by the transport is responsible for checking returned chunks and
  enforcing the declared body length. Function and body-length arguments
  remain checked by the constructor. See [Design notes](README.md#design-notes).
- Protocol-version constructors retain the supplied string object, and the
  request, response, request-stream, and response-stream protocol-version
  accessors return the stored string directly. Mutations to a supplied string
  are visible to every value sharing it; pass `(copy-seq version)` when you
  need an independent value.
- `make-http-uri` accepts a symbol scheme and treats its name as a string.
- `make-http-uri` and `parse-http-uri` reject RFC 6874 IPv6 zone identifiers,
  such as `[fe80::1%25en0]`.
- Add the `cl-hpack-codec-kit` source directory to `CL_SOURCE_REGISTRY` beside
  this checkout, or use `nix develop`, which configures both systems.
- HPACK decoding accepts per-call `:max-fields` and `:max-header-bytes`
  limits, which cannot exceed the context limits. `:validate-header-names t`
  enables lowercase name syntax checks, and context-level validation cannot be
  disabled per call. HPACK encoding accepts `:static-only-p` and
  `:sensitive-predicate`; the predicate receives normalized name and value
  octet vectors, and a non-NIL result marks the field never-indexed.
- The generic `qpack-decode-header-block` checks pseudo-header name syntax but
  does not apply HTTP/3 section-role rules. When using it directly for HTTP/3,
  call `http3-validate-header-section` on the decoded fields with
  `:request-p t` for requests, `:trailers-p t` for trailers, or neither flag
  for responses. `qpack-decode-http3-field-section` instead requires a `:role`
  of `:request`, `:response`, or `:trailers` and validates decoded fields itself,
  including after `qpack-resume-blocked-stream`. See
  [HTTP/3 QPACK transport adapter](README.md#http3-qpack-transport-adapter).
