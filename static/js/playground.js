/* OUTPUT AREA */
const outputArea = $('.output-area .output-text').text();
try {
    const indentedOutput = JSON.stringify(JSON.parse(outputArea), null, 4);
    outputArea.text(indentedOutput);
} catch (err) {
    // OK ... result is not JSON
}

/* FLOATING ACTION BUTTONS */
$('.fab-up').click(() => {
    $("html, body").animate({ scrollTop: 0 }, 250);
});

$('.fab-down').click(() => {
    $("html, body").animate({scrollTop: $(document).height()}, 250);
});
