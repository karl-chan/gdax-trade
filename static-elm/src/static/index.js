// pull in desired CSS/SASS files
require('materialize-css/dist/css/materialize.min.css');
require( './styles/main.scss' );

// inject bundled Elm app into div#main
const Elm = require( '../elm/Main' );
const $ = require('jquery');
require('materialize-css/dist/js/materialize.min.js');

const app = Elm.Main.embed( document.getElementById( 'main' ) );

app.ports.initPort.subscribe(args => {
    $(`#${args.drawer.toggleId}`).sideNav();
})
