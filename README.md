# cl-http-message-kit

[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

cl-http-message-kit is the HTTP message model and wire codecs an HTTP
implementation builds on: validated HTTP URIs, normalized header fields,
request and response values, request-target forms, streaming bodies, status
metadata, deadlines, size-limit checks, and a structured condition hierarchy,
together with an HTTP/1.1 wire codec, HTTP/2 and HTTP/3 frame codecs, and
HPACK and QPACK header compression. It uses `cl-hpack-codec-kit` for HPACK
encoding, decoding, and shared compression primitives. It performs no I/O —
it knows nothing about TLS, sockets, connection pooling, or QUIC transport.

The message model is shared by every protocol version — an HTTP/2 request and
an HTTP/1.1 request differ in how they are encoded, not in what they are — so
it belongs below the wire formats rather than inside any one of them. The wire
codecs turn that shared model into and out of octets for each protocol
version, so a caller that supplies the transport (sockets, TLS, QUIC) gets a
complete encode/decode path without re-implementing framing or header
compression.

## Install

The runtime system depends on `cl-hpack-codec-kit`. Register this checkout and
the dependency source directory with ASDF before loading it:

```sh
export CL_SOURCE_REGISTRY="$PWD//:/path/to/cl-hpack-codec-kit//"
```

Replace the example dependency path with its source directory. `nix develop`
configures both systems automatically.

```lisp
(asdf:load-system "cl-http-message-kit")
```

The Nix flake provides the same environment together with the test and
coverage commands:

```sh
nix develop
nix run .#test
nix run .#coverage
```

Coverage writes the machine-readable report to
`cl-http-message-kit.coverage` and the HTML report to
`cl-http-message-kit-coverage-report/`. The coverage command fails unless
expression and branch coverage both reach 100%.

`nix run .#test` is the release gate. It runs the complete suite, prints the
test count and result totals, and fails if no tests run or any test fails.
`nix run .#coverage` is currently
unreliable on this codebase — `sb-cover` (the SBCL coverage instrumenter
`cl-weave --coverage` drives) stalls once the cumulative amount of
instrumented code in one compilation session crosses some threshold. This
reproduces independent of which source file sits at that position — moving
`hpack.lisp` earlier in `cl-http-message-kit.asd` lets it compile cleanly and
shifts the stall onto whichever file then lands in its place — and excluding
declaration-only files (already applied in `flake.nix`) is not enough to
clear the threshold without also excluding the logic files coverage exists
to measure. This is an upstream `sb-cover`/`cl-weave` limitation, not a defect
in this library; it is not currently a release gate.

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

Field names are normalized once at construction, values are copied at the API
boundary, and lookup is case-insensitive. Header input may be a list of header
objects or `(name . value)` / `(name value)` pairs.

```lisp
(let ((headers (list (http-message-kit:make-http-header "Content-Type" "text/plain"))))
  (http-message-kit:http-header-value headers "CONTENT-TYPE"))
;; => "text/plain"
```

Streaming bodies can be consumed without an adapter protocol. The chunk
continuation receives the next continuation, and the end continuation runs
when the body function returns `NIL`:

```lisp
(let ((chunks (list #(1 2) #(3) nil)))
  (http-message-kit:http-body-cps
   (lambda () (pop chunks))
   (lambda (chunk next)
     (declare (ignore chunk))
     (funcall next))
   (lambda () :complete)))
;; => :complete
```

The HTTP/1 codec serializes a request or response to wire octets and parses
octets back into a message, either all at once or incrementally as bytes
arrive:

```lisp
(let ((wire (http-message-kit:http1-serialize-request
             (http-message-kit:make-http-request
              :method "GET" :uri "http://example.test/health"))))
  (http-message-kit:http-request-target
   (http-message-kit:http1-parse-request wire)))
;; => "/health"
```

HPACK and QPACK compress a header list to and from wire octets against a
stateful compression context; the context tracks the dynamic table across
calls:

```lisp
(let* ((context (http-message-kit:make-hpack-context))
       (headers (list (http-message-kit:make-http-header "content-type" "text/plain")))
       (wire (http-message-kit:hpack-encode-header-block headers :context context)))
  (http-message-kit:hpack-decode-header-block
   wire :context (http-message-kit:make-hpack-context)))
```

## API

| Group | Operations |
| --- | --- |
| URI | `parse-http-uri`, `make-http-uri`, `http-uri-scheme`, `-authority`, `-host`, `-port`, `-path`, `-query`, `-string` |
| Header fields | `make-http-header`, `http-header-copy`, `http-header-name`, `-content`, `http-header-value`, `-values`, `-present-p` |
| Requests | `make-http-request`, `http-request-method`, `-uri`, `-target`, `-target-form`, `-authority`, `-path`, `-query`, `-headers`, `-trailers`, `-body`, `-protocol-version` |
| Request streams | `make-http-request-stream` and its accessors, including target-form and declared body length |
| Responses | `make-http-response`, `http-response-status`, `-reason`, `-headers`, `-trailers`, `-body`, `-protocol-version` |
| Response streams | `make-http-response-stream` and its accessors, for a body produced incrementally |
| Body CPS | `http-body-cps` for chunk, completion, and error continuations |
| Methods | `http-method-p`, `-name`, `-known-p`, `-properties`, `-safe-p`, `-idempotent-p`, `-cacheable-p`, `-connect-p` |
| Message framing | `http-request-framing`, `http-response-framing`, `http-message-framing`, `http-message-body-length`, `http-message-body-allowed-p`, `http-trailer-field-p`, `http-validate-trailers` |
| HTTP/1 wire codec | `http1-serialize-request`, `-response`, `http1-parse-request`, `-response`, `http1-decode-chunked`, `-encode-chunked`, `make-http1-parser` and its `feed`/`finish`/`reset` operations |
| HTTP/2 and HTTP/3 framing | `make-http-frame`, `http2-encode-frame`, `-decode-frame`, `-encode-settings`, `-decode-settings`, `http3-encode-frame`, `-decode-frame`, `-encode-varint`, `-decode-varint`, `-encode-settings`, `-decode-settings`, `http-validate-header-section` |
| HPACK (HTTP/2) | `make-hpack-context`, `hpack-encode-header-block` (`:static-only-p`, `:sensitive-predicate`), `-decode-header-block` (`:max-fields`, `:max-header-bytes`, `:validate-header-names`), `hpack-encode-integer`, `-decode-integer`, `-encode-string`, `-decode-string` |
| QPACK (HTTP/3) | `make-qpack-context`, `qpack-encode-header-block`, `-decode-header-block`, `-encode-insert-with-name-reference`, `-with-literal-name`, `-duplicate`, `qpack-decode-encoder-stream`, `-decoder-stream`, `qpack-encode-section-ack`, `-stream-cancellation`, `-insert-count-increment` |
| Status | `http-status-reason` and the five informational/success/redirection/client-error/server-error predicates |
| Limits and deadlines | `http-check-size-limit`, `*default-max-header-bytes*`, `*default-max-body-bytes*`, `http-deadline`, `with-http-deadline` |
| Conditions | `http-error`, `http-protocol-error`, `http-invalid-uri`, `http-invalid-header`, `http-invalid-status`, `http-connection-error`, `http-timeout`, `http-size-limit-exceeded`, `http-unsupported-feature`, `http-incomplete-message`, `http-invalid-framing`, `http-invalid-frame`, `http-qpack-blocked` |

## Design notes

URI construction validates HTTP(S) schemes, authority syntax, IPv4, IPv6,
IPvFuture, percent escapes, request-safe characters, and the RFC 6874
percent-encoded IPv6 zone-id form. Scheme and host are normalized to lowercase,
while callers receive copied strings. An absent port stays absent rather than
being filled in from the scheme. The default belongs to whoever opens the
connection, and storing it in the value would make two URIs that differ only in
an implied port compare unequal.

When no explicit target is supplied, `http-request-target` derives the
origin-form target — path and query only — from the URI. An explicit target is
validated and preserved as one of the four HTTP forms: origin, absolute,
authority, or asterisk. `http-request-target-form` exposes that classification.

The request and response stream values wrap chunk-producing functions. Chunks
are copied and checked as octets; a declared body length rejects overflow and
an early end. `http-check-size-limit` provides the same bounded accounting for
transport adapters without imposing a transport-specific default.

`http-body-cps` turns a chunk-producing body function into an explicit
continuation flow. It does not perform I/O or retain chunks, so a transport can
choose its own scheduling, backpressure, and resource lifetime around the
message model.

Validation happens at construction. A request or response value that exists is
one that passed its checks, so downstream code does not re-validate.

`http-message-framing` classifies a request or response by RFC 9112 message
framing rules (content-length, chunked, close-delimited, tunnel, or no body)
and rejects the framing ambiguities the RFC forbids, such as a
Content-Length paired with a Transfer-Encoding, or a status that cannot carry
a body still declaring one.

The HTTP/1 codec serializes the message model to wire octets and back.
Serialization derives the framing header (Host, Content-Length, or
Transfer-Encoding) from the message rather than trusting caller-supplied
values, and rejects a message whose headers already conflict with its body.
`make-http1-parser` provides an incremental parser for a transport that
receives octets in arbitrary-sized reads: `http1-parser-feed` accepts each
read and reports completion, an unconsumed remainder, or that more input is
needed, without re-parsing what was already consumed.

The HTTP/2 and HTTP/3 codecs cover frame and settings encoding/decoding
(`http2-encode-frame`, `http3-decode-frame`, and their settings
counterparts) and pseudo-header section validation
(`http-validate-header-section`), including the extended-CONNECT and
trailer-section forms both protocols define. HPACK block encoding and
decoding, static and dynamic table handling, and Huffman coding use
`cl-hpack-codec-kit`. QPACK also uses that kit's Huffman, integer, and
header-name validation primitives while retaining the RFC 9204 encoder/decoder stream
instructions and blocked-stream handling that support out-of-order delivery.
Both keep compression state in an explicit `hpack-context` or `qpack-context`
value rather than global state, so a caller manages one context per connection.

QPACK checks pseudo-header name syntax while decoding. It does not check which
pseudo-headers are defined for a section, which are required, or their order.
Immediately after decoding an HTTP/3 field section, call
`http3-validate-header-section` with `:request-p t` for a request,
`:trailers-p t` for trailers, or neither flag for a response. If decoding was
blocked, validate immediately after resuming the section. Do this before
building the request, response, or trailer model.

HPACK decoding accepts per-call `:max-fields` and `:max-header-bytes` limits;
each can only tighten the configured context limit. `:validate-header-names t`
enables lowercase name syntax validation, and a context configured to validate
names cannot be relaxed for an individual call. Encoding can select static
representations with `:static-only-p t`. A `:sensitive-predicate` receives the
normalized name and value as octet vectors and marks matching fields as
never-indexed.

This library stops at message and wire-format semantics. It does not open
sockets, negotiate TLS, drive QUIC transport, or manage connection pooling.
Those layers consume the values and codecs here and are responsible for I/O.

## Development

Tests are defined with `cl-weave`; the flake also exposes `paredit-cli` for
structure-aware Common Lisp inspection and refactoring. Run `nix run .#test`
for the serial test suite, or `nix run .#coverage` to produce coverage
artifacts. With `cl-weave` available directly, the equivalent test command is:

```sh
cl-weave run \
  --load cl-http-message-kit.asd \
  cl-http-message-kit/test \
  --reporter spec \
  --fail-with-no-tests \
  --test-timeout-ms 30000 \
  --max-workers 1
```

The test ASDF system is loaded by `cl-weave`; `cl-weave` is intentionally not
a runtime dependency of `cl-http-message-kit`.

## License

MIT. See [LICENSE](LICENSE).
