import http.server
import os

os.chdir(r'c:\Users\27862\Documents\trae_projects\TSA')
port = 8080

print(f"启动本地服务器 http://localhost:{port}")
print(f"请在浏览器中打开 http://localhost:{port}/visualization.html")

server = http.server.HTTPServer(('localhost', port), http.server.SimpleHTTPRequestHandler)
server.serve_forever()