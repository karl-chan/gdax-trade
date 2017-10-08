$(document).ready(() => {
    /* OUTPUT AREA */
    const output = $('.output-area .output-text').text();
    try {
        const indentedOutput = JSON.stringify(JSON.parse(output), null, 4);
        $('.output-area .output-text').text(indentedOutput);
    } catch (err) {
        // OK ... result is not JSON
    }

    /* FLOATING ACTION BUTTONS */
    $('.fab-up').click(() => {
        $("html, body").animate({ scrollTop: 0 }, 250);
    });

    $('.fab-down').click(() => {
        $("html, body").animate({ scrollTop: $(document).height() }, 250);
    });
})
