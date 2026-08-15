# cl-http-message-kit

[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

cl-http-message-kit is the vocabulary an HTTP implementation needs before it
picks a wire format: validated URIs, header fields with case-insensitive
lookup, request and response values, deadlines, and a structured condition
hierarchy. It has no dependencies, performs no I/O, and knows nothing about
HTTP/1.1 framing, HTTP/2, or connections.

That boundary is the point. A message model is shared by every protocol
version — an HTTP/2 request and an HTTP/1.1 request differ in how they are
encoded, not in what they are — so it belongs below the wire formats rather
than inside any one of them.

## Install

```lisp
(asdf:load-system "cl-http-message-kit")
```

## Usage

```lisp
(let ((request
        (http-message-kit:make-http-request
         :method "GET"
         :uri (http-message-kit:parse-http-uri
               "https://example.test/health?x=1"))))
  (list (http-message-kit:http-request-authority request)
        (http-message-kit:http-request-target request)))
;; => ("example.test" "/health?x=1")
```

Field names are normalised once at construction, so lookup is a plain string
comparison and callers never have to think about case.

```lisp
(let ((headers (list (http-message-kit:make-http-header "Content-Type" "text/plain"))))
  (http-message-kit:http-header-value headers "CONTENT-TYPE"))
;; => "text/plain"
```

## API

| Group | Operations |
| --- | --- |
| URI | `parse-http-uri`, `make-http-uri`, `http-uri-scheme`, `-authority`, `-host`, `-port`, `-path`, `-query`, `-string` |
| Header fields | `make-http-header`, `http-header-name`, `-content`, `http-header-value`, `-values`, `-present-p` |
| Requests | `make-http-request`, `http-request-method`, `-uri`, `-target`, `-authority`, `-path`, `-query`, `-headers`, `-trailers`, `-body`, `-protocol-version` |
| Responses | `make-http-response`, `http-response-status`, `-reason`, `-headers`, `-trailers`, `-body`, `-protocol-version` |
| Response streams | `make-http-response-stream` and its accessors, for a body produced incrementally |
| Deadlines | `http-deadline`, `with-http-deadline` |
| Conditions | `http-error`, `http-protocol-error`, `http-invalid-uri`, `http-invalid-header`, `http-invalid-status`, `http-connection-error`, `http-timeout`, `http-size-limit-exceeded`, `http-unsupported-feature` |

## Design notes

An absent port stays absent rather than being filled in from the scheme. The
default belongs to whoever opens the connection, and storing it in the value
would make two URIs that differ only in an implied port compare unequal.

`http-request-target` returns the origin form — path and query only — because
that is what an HTTP/1.1 request line carries. Absolute-form targets are a
proxy concern and belong to the code that talks to a proxy.

Validation happens at construction. A request or response value that exists is
one that passed its checks, so downstream code does not re-validate.

## License

MIT. See [LICENSE](LICENSE).
