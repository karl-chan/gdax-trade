const serverUrl = location.protocol + '//' + location.hostname + (location.port ? ':' + location.port: '');
const socket = new WebSocket(`${baseUrl}/stream`);
socket.onopen = () => {
    const initialMessage = $('input[name="initial_message"]').value()
    socket.send(initialMessage);
}

socket.onmessage = (msg) => {
    $('')

}

$('#playground .output-area .output-text').
