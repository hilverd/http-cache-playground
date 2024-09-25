import './index.css';
import { Elm } from './src/Main.elm';

var app = Elm.Main.init({
    flags: []
});

app.ports.scrollToBottomOfSequenceDiagram.subscribe(() => {
    setTimeout(() => {
        window.scrollTo({ left: 0, top: document.body.scrollHeight, behavior: 'smooth' });
    }, 0);
});