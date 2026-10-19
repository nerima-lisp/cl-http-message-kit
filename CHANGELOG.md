# Changelog

## 2.0.0 (unreleased)

### Breaking changes

- HPACK encoding and decoding now use `cl-hpack-codec-kit`. Applications must
  make that system available to ASDF before loading `cl-http-message-kit`.
- The ASDF and Nix package version is now `2.0.0`.

### Migration

- Explicit request targets are accepted when they contain visible ASCII. Their
  form-specific syntax is checked by the HTTP parser on input and by a
  serializer or header-section validator before output.
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
