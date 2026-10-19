# Changelog

## 2.0.0 (unreleased)

### Breaking changes

- HPACK encoding and decoding now use `cl-hpack-codec-kit`. Applications must
  make that system available to ASDF before loading `cl-http-message-kit`.
- The ASDF and Nix package version is now `2.0.0`.

### Migration

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
