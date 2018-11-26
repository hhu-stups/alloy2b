abstract sig EndPoint { }
sig Server extends EndPoint {
causes: set HTTPEvent }
sig Client extends EndPoint { } abstract sig HTTPEvent {
from, to, origin: EndPoint }
sig Request extends HTTPEvent { response: lone Response
}
sig Response extends HTTPEvent { embeds: set Request
}
sig Redirect extends Response { }


fact Directions {
Request.from + Response.to in Client Request.to + Response.from in Server }
fact RequestResponse {
all r: Response | one response.r all r: Response |
r.to = response.r.from
and r.from = response.r.to all r: Request |
r not in r.^(response.embeds) }
fact Causality {
all e: HTTPEvent, s: Server |
e in s.causes iff e.from = s or some r: Response |
e in r.embeds and r in s.causes }
fact Origin {
all r: Response, e: r.embeds |
e.origin = r.origin
all r: Response | r.origin =
(r in Redirect implies response.r.origin else r.from)
all r: Request |
no embeds.r implies
r.origin in r.from }
pred ObeysOrigins (s: Server) { all r: Request |
r.to = s implies
r.origin = r.to or r.origin = r.from
}

check {
no good, bad: Server {
good.ObeysOrigins no r: Request |
r.to = bad and r.origin in Client some r: Request |
r.to = good and r in bad.causes }
} for 2
