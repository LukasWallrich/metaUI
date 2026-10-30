"""Plain static file server for the shinylive prototype (no server-side R).

Sends `Cache-Control: max-age=600` like GitHub Pages and logs each request with a
millisecond timestamp and file size, so page loads can be timed and tallied.
Usage: python3 serve.py SITE_DIR PORT
"""
import functools, http.server, mimetypes, os, sys, time

mimetypes.add_type("application/wasm", ".wasm")
mimetypes.add_type("text/javascript", ".mjs")


class Handler(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header("Cache-Control", "max-age=600")
        super().end_headers()

    def log_request(self, code="-", size="-"):
        path = self.translate_path(self.path)
        if os.path.isdir(path):
            path = os.path.join(path, "index.html")
        try:
            success = int(code) == 200
        except (TypeError, ValueError):
            success = False
        nbytes = os.path.getsize(path) if success and os.path.isfile(path) else 0
        sys.stderr.write(f"{time.time():.3f}\t{code}\t{nbytes}\t{self.path}\n")
        sys.stderr.flush()


root, port = sys.argv[1], int(sys.argv[2])
handler = functools.partial(Handler, directory=root)
http.server.ThreadingHTTPServer(("127.0.0.1", port), handler).serve_forever()
