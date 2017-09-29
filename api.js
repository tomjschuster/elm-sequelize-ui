const jsonServer = require('json-server')

const server = jsonServer.create()

server.use(jsonServer.defaults())

const router = jsonServer.router('db.json')
server.use(router)

console.log('Listening at 4001')
server.listen(4001)
