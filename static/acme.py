import webapp2
from google.appengine.ext import webapp

class LetsEncryptHandler(webapp.RequestHandler):

    def get(self, challenge):
        self.response.headers['Content-Type'] = 'text/plain'
        responses = {
                    'pFfq0636To7GyjaXpZOt4QP91FYwTOAqWysySyY_3Ic': 'pFfq0636To7GyjaXpZOt4QP91FYwTOAqWysySyY_3Ic.ouBXZ41JNflg82kbrpZyY764kAfDKX-EArdtm8KKoF4',
                }
        self.response.write(responses.get(challenge, ''))

app = webapp2.WSGIApplication([
    ('/.well-known/acme-challenge/([\w-]+)', LetsEncryptHandler),
])
