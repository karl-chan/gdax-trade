$('#playground .tabs .tab-results').click(() => {
    "use strict";
    $('#playground .results').show();
    $('#playground .docs').hide();
})

$('#playground .tabs .tab-docs').click(() => {
    "use strict";
    $('#playground .results').hide();
    $('#playground .docs').show();
})