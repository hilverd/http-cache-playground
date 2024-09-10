import './index.css';
import { Elm } from './src/Main.elm';

var app = Elm.Main.init({
    flags: []
});

app.ports.scrollToBottomOfSequenceDiagram.subscribe(() => {
    const element = document.getElementById('sequence-diagram');

    if (element)
        element.scrollIntoView({ behavior: 'smooth', block: 'end' });
});
