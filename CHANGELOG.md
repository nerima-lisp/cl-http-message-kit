# Changelog

## 2.0.0 (unreleased)

### Breaking changes

- HPACK encoding and decoding now use `cl-hpack-codec-kit`. Applications must
  make that system available to ASDF before loading `cl-http-message-kit`.
- The ASDF and Nix package version is now `2.0.0`.

### Migration

- QPACK encoding now exposes `qpack-prepare-*` operations with companion
  `qpack-commit-encoding` and `qpack-abort-encoding` operations. Prepare before
  writing and commit only after a successful write; abort when zero bytes are
  known to have been sent. Close the connection and discard the encoder and
  decoder QPACK contexts if any bytes may have been sent or delivery is
  uncertain. Existing state-mutating `qpack-encode-*` operations still update
  context immediately. Header-block preparation requires `:stream-id` when
  dynamic references are enabled.
- `qpack-context-set-max-table-capacity` and its SETF accessor update the
  context maximum and backing dynamic-table limit together without changing
  current capacity. To lower the maximum below current capacity, first reduce
  current capacity with `qpack-context-set-capacity`; an invalid maximum is
  rejected without changing either limit. Changing the maximum invalidates
  outstanding prepared QPACK encodings.
- Explicit request targets are accepted when they contain visible ASCII. Their
  form-specific syntax is checked by the HTTP parser on input and by a
  serializer or header-section validator before output.
- Request-target character errors now return the original target through
  `http-protocol-error-detail`, instead of `(:target target :reason :characters)`.
  Form-specific errors keep their structured details.
- Response constructors accept bodies and trailers for 1xx, 204, and 304
  statuses. HTTP/1 serialization rejects them; HTTP/2 and HTTP/3 transport
  adapters must apply the same status rule when writing response headers, DATA
  frames, and trailer sections.
- Request and response constructors no longer reject prohibited trailer field
  names. HTTP/1 trailer encoding and decoding and HTTP/2 or HTTP/3 trailer
  section validation reject them at the protocol boundary.
- `http-header-name` and `http-header-content` expose their stored values, and
  `setf` stores and returns the exact assigned value. Message constructors
  continue to copy and revalidate headers, and request and response accessors
  return copies of each header.
- `make-http-response-stream` retains the supplied body function. The adapter
  that consumes it is responsible for checking returned chunks and enforcing
  the declared body length. Function and body-length arguments remain checked
  by the constructor.
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
- QPACK now obtains its shared Huffman, integer, and header-name validation
  operations from `cl-hpack-codec-kit`.
- HPACK decoding accepts per-call `:max-fields` and `:max-header-bytes`
  limits, which cannot exceed the context limits. `:validate-header-names t`
  enables lowercase name syntax checks, and context-level validation cannot be
  disabled per call. HPACK encoding accepts `:static-only-p` and
  `:sensitive-predicate`; the predicate receives normalized name and value
  octet vectors.
- QPACK checks pseudo-header name syntax only. After decoding or resuming an
  HTTP/3 field section, call `http3-validate-header-section` with
  `:request-p t` for requests, `:trailers-p t` for trailers, or neither flag
  for responses, before converting it to a message model. This upper layer
  validates pseudo-header definitions, requirements, and ordering.
