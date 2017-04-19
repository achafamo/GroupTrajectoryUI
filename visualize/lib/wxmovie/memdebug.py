# memdebug.py

import cherrypy
import dowser

def start(port):
    cherrypy.tree.mount(dowser.Root())
    cherrypy.config.update({
        'environment': 'embedded',
        'server.socket_port': port
    })
#    cherrypy.server.quickstart()
    cherrypy.engine.start()
