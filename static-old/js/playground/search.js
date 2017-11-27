const responseText = $('input[name="response_text"]').val();

// Output area
$('#playground .output-card .output-text').text(formatJSON(responseText));
