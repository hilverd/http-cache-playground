const express = require('express');
const morgan = require('morgan');
const cors = require('cors');
const bodyParser = require('body-parser');
const app = express();
const port = process.env.PORT || 3000;
const jsonParser = bodyParser.json();
const { Buffer } = require('node:buffer');

const interactions = {};

app.disable('x-powered-by');
app.set('etag', false); // Disable automatic ETag generation
app.use(cors());
app.use(morgan('combined'));

// Serve static files with 21 days cache, except for the root URL (with any query parameters)
app.use((req, res, next) => {
    if (req.path === '/') {
        res.setHeader('Cache-Control', 'no-store, max-age=0');
        express.static('dist', { etag: false })(req, res, next);
    } else {
        express.static('dist', { etag: true, maxAge: '21 days' })(req, res, next);
    }
});

// Add a specific route for the root URL
app.get('/', (req, res) => {
    res.sendFile('dist/index.html', { root: __dirname });
});

app.get('/internal/status', (req, res) => res.json({ "status": "ok" }));

app.get("/ids/:id", (req, res) => {
    const unixTime = Math.floor(new Date().getTime() / 1000);

    const id = req.params.id;

    addInteraction(id, interactionEntryForRequest(req.path, req.headers))

    addCacheHeadersBasedOnQueryParameters(req, res);

    const body = `${unixTime}`;

    if (req.query['respond-slowly'] === '') {
        const howManySecondsToSleep = 2;
        const howManyMilliSecondsToSleep = howManySecondsToSleep * 1000;

        addInteraction(id, interactionEntryForSleeping(howManySecondsToSleep));
        setTimeout((() => {
            addInteraction(id, interactionEntryForResponse(res.statusCode, res.getHeaders(), body));
            res.send(body);
        }), howManyMilliSecondsToSleep);
    } else {
        addInteraction(id, interactionEntryForResponse(res.statusCode, res.getHeaders(), body));
        res.send(body);
    }

    return res;
});

app.get("/interactions/:id", (req, res) => {
    const id = req.params.id;
    res.setHeader("cache-control", "no-cache");
    res.json(interactions[id] ? interactions[id].data : []);
});

app.post("/interactions/:id/new", jsonParser, (req, res) => {
    const id = req.params.id;
    const interaction = req.body;

    addInteraction(id, interaction);

    res.sendStatus(200);
});

function addInteraction(id, interaction) {
    if (!interactions[id]) {
        interactions[id] = {
            timestamp: Date.now(),
            data: []
        };
    }
    interactions[id].data.push(interaction);
    // The timestamp is not updated for existing IDs
}

function cleanupInteractions() {
    const twoMinutesAgo = Date.now() - 2 * 60 * 1000;
    for (const id in interactions) {
        if (interactions[id].timestamp < twoMinutesAgo) {
            delete interactions[id];
        }
    }
}

function interactionEntryForRequest(path, headersObject) {
    return {
        "tag": "VarnishToOrigin",
        "args": [
            {
                "path": path,
                "headers": Object.keys(headersObject).map((key) => [key, headersObject[key]])
            }
        ]
    };
}

function interactionEntryForResponse(statusCode, headersObject, body) {
    return {
        "tag": "OriginToVarnish",
        "args": [
            {
                "statusCode": statusCode,
                "headers": Object.keys(headersObject).map((key) => [key, headersObject[key]]),
                "body": body
            }
        ]
    };
}

function interactionEntryForSleeping(seconds) {
    return {
        "tag": "OriginSleepingForSeconds",
        "args": [seconds]
    };
}

function addCacheHeadersBasedOnQueryParameters(req, res) {
    let base64EncodedHeadersToReturn = req.query['headers-to-return'] || [];

    if (base64EncodedHeadersToReturn && !Array.isArray(base64EncodedHeadersToReturn)) {
        base64EncodedHeadersToReturn = [base64EncodedHeadersToReturn];
    }

    const decodedHeadersToReturn = base64EncodedHeadersToReturn.map((base64EncodedHeaderToReturn) => Buffer.from(base64EncodedHeaderToReturn, 'base64').toString('ascii'));

    decodedHeadersToReturn.forEach(decodedHeaderToReturn => {
        const colonIndex = decodedHeaderToReturn.indexOf(":");
        const key = decodedHeaderToReturn.slice(0, colonIndex);
        const value = decodedHeaderToReturn.slice(colonIndex + 1);

        res.append(key, value);
    });
}

setInterval(cleanupInteractions, 60 * 1000); // Run every minute

app.listen(port, () => console.log('Listening on port ' + port));
