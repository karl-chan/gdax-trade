const socketUrl = $('input[name="socket_url"]').val();
const initialMessage = $('input[name="initial_message"]').val();

// Establish websocket connection with gdax
const socket = new WebSocket(socketUrl);
const outputDiv = $('#playground .output-card .output-text');
const stopStreamingButton = $('#playground .output-card .stop-streaming');
socket.onopen = () => {
    socket.send(initialMessage);
}

// Append websocket response to text area
socket.onmessage = (msg) => {
    outputDiv.append(formatJSON(msg.data));
}

// Stop websocket streaming when button is pressed
stopStreamingButton.click(() => {
    socket.close();
})

