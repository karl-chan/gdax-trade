/* FLOATING ACTION BUTTONS */
$('.fab-up').click(() => {
    $("html, body").animate({ scrollTop: 0 }, 250);
});

$('.fab-down').click(() => {
    $("html, body").animate({scrollTop: $(document).height()}, 250);
});
