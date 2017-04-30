const ws = require('ws')
const Elm = require('./elm.js')

// start up the Elm server
const elmApp = Elm.Main.worker()

let nextUid = 0
let clients = {}
const server = new ws.Server({port: 8000})

server.on('connection', (client) => {
    console.log('new client!')

    const clientId = nextUid++
    clients[clientId] = client

    client.on('message', (message) => {
        console.log(message.toString())

        // if the clientId doesn't exist in the Elm app, 
        // it'll be added on the first message
        elmApp.ports.clientMessages.send({
            clientId: clientId,
            message: message.toString()
        })
    })

    client.on('close', () => {
        console.log('client disconnected!')
        elmApp.ports.clientDisconnects.send(clientId)
        delete clients[clientId]
    })
})

function broadcastToClients(clientIds, message) {
    for (let i = 0, length = clientIds.length; i < length; ++i) {
        const client = clients[clientIds[i]]
        if (client.readyState === ws.OPEN) {
            client.send(message)
        }
    }
}

// I'll get a message back from Elm that looks like:
// [
//     {
//         clients: [1, 2, 4, 6],
//         message: "..."
//     },
//     {
//         clients: [3, 5],
//         message: "..."
//     },
//     ...
// ]
elmApp.ports.stateUpdates.subscribe((update) => {
    for (let i = 0, length = update.length; i < length; ++i) {
        broadcastToClients(update[i].clients, update[i].message)
    }
})