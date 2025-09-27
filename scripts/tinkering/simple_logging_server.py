from http.server import BaseHTTPRequestHandler, HTTPServer
import json
import logging

# Set up logging configuration
logging.basicConfig(level=logging.INFO)

class RequestHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        # Get the content length of the request body
        content_length = int(self.headers['Content-Length'])
        
        # Read the request body
        body = self.rfile.read(content_length)
        
        try:
            # Try to parse the request body as JSON
            payload = json.loads(body.decode('utf-8'))
            
            # Log the JSON payload
            logging.info(f"Received JSON payload: {payload}")
            
            # Send a 200 OK response back to the client
            self.send_response(200)
            self.end_headers()
        except json.JSONDecodeError:
            # If the request body is not valid JSON, send a 400 Bad Request response
            self.send_response(400)
            self.end_headers()
            self.wfile.write(b"Invalid JSON payload")

def run_server():
    server_address = ('', 8000)
    httpd = HTTPServer(server_address, RequestHandler)
    print("Starting web server on port 8000...")
    httpd.serve_forever()

if __name__ == "__main__":
    run_server()