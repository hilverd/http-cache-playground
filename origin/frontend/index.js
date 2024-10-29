import './index.css';
import { Elm } from './src/Main.elm';

var app = Elm.Main.init({
    flags: []
});

app.ports.scrollToBottomOfSequenceDiagram.subscribe(() => {
    setTimeout(() => {
        window.scrollTo({
            left: 0,
            top: document.body.scrollHeight, behavior: 'smooth'
        });
    }, 0);
});

app.ports.scrollToClientSettings.subscribe(() => {
    setTimeout(() => {
        window.scrollTo({
            top: document.getElementById('client-settings').offsetTop - 16,
            behavior: 'smooth'
        });
    }, 0);
});
