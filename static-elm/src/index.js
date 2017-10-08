import './main.css';
const logoPath = require('./logo.svg');
const Elm = require('./App.elm');

const host = `${location.protocol}//${location.hostname}${location.port ? ':'+location.port: ''}`;

const flags = {
    host: host
};

const root = document.getElementById('root', flags);

Elm.App.embed(root, logoPath);
