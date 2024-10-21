const express = require('express');
const morgan = require('morgan');
const bodyParser = require('body-parser');
const axios = require('axios');
const url = require('url');

const app = express();
const port = process.env.PORT || 3000;
const jsonParser = bodyParser.json();
const { Buffer } = require('node:buffer');

const originHost = 'varnish';

const interactions = {};

app.disable('x-powered-by');
app.set('etag', false); // Disable automatic ETag generation
app.use(morgan('combined'));

// Uncomment these for development
// const cors = require('cors');
// app.use(cors());

const sanitisingProxy = async (req, res) => {
    const id = req.params.id;
    const protocol = req.protocol;
    const query = { ...req.query };
    delete query['headers-to-send'];

    const targetUrl = url.format({
        protocol: protocol,
        host: originHost,
        pathname: `/sanitised/ids/${id}`,
        query: query
    });

    try {
        const headers = requestHeadersBasedOnQueryParameters(req);

        const response = await axios({
            method: 'get',
            url: targetUrl,
            headers: headers,
            validateStatus: () => true, // don't throw on 4xx and 5xx
        });

        // Ignore the actual response status
        res.status(200);

        Object.entries(response.headers).forEach(([key, value]) => {
            if (key.toLowerCase() === 'set-cookie') {
                res.set('X-VCP-Set-Cookie', value);
                res.set(key, `${value}; max-age=0`);
            } else {
                res.set(key, value);
            }
        });

        res.send(`${response.data}`);
    } catch (error) {
        console.error('Proxy error:', error);
        res.status(500).send('An error occurred while proxying the request.');
    }

    return res;
};

const sanitisingPurgeProxy = async (req, res) => {
    const id = req.params.id;
    const protocol = req.protocol;
    const query = { ...req.query };
    delete query['headers-to-send'];

    const targetUrl = url.format({
        protocol: protocol,
        host: originHost,
        pathname: `/sanitised/ids/${id}`,
        query: query
    });

    try {
        const headers = requestHeadersBasedOnQueryParameters(req);

        const response = await axios({
            method: 'purge',
            url: targetUrl,
            headers: headers,
            validateStatus: () => true, // don't throw on 4xx and 5xx
        });

        // Ignore the actual response status
        res.status(200);

        Object.entries(response.headers).forEach(([key, value]) => {
            if (key.toLowerCase() === 'set-cookie') {
                res.set('X-VCP-Set-Cookie', value);
                res.set(key, `${value}; max-age=0`);
            } else {
                res.set(key, value);
            }
        });

        res.send(`${response.data}`);
    } catch (error) {
        console.error('Proxy error:', error);
        res.status(500).send('An error occurred while proxying the request.');
    }

    return res;
};

app.use((req, res, next) => {
    if (req.path === '/') {
        res.setHeader('Cache-Control', 'no-store, max-age=0');
        express.static('dist', { etag: false })(req, res, next);
    } else {
        express.static('dist', { etag: true, maxAge: '21 days' })(req, res, next);
    }
});

app.get('/internal/status', (req, res) => {
    res.setHeader("cache-control", "no-store");
    res.json({ "status": "ok" })
});

app.get('/ids/:id', sanitisingProxy);

app.post('/purge/ids/:id', sanitisingPurgeProxy);

app.get("/sanitised/ids/:id", (req, res) => {
    const unixTime = Math.floor(new Date().getTime() / 1000);

    const id = req.params.id;

    addInteraction(id, interactionEntryForRequest(req.path.replace("/sanitised/", "/"), req.headers))

    let body = `${unixTime}`;

    if (req.query['auto-304'] === '' && (req.headers['if-none-match'] || req.headers['if-modified-since'])) {
        res.status(304);
        body = '';
    } else {
        res.status(200);
    }

    addResponseHeadersBasedOnQueryParameters(req, res, body);

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
    res.setHeader("cache-control", "no-store");
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

function requestHeadersBasedOnQueryParameters(req) {
    let base64EncodedHeadersToSend = req.query['headers-to-send'] || [];

    if (base64EncodedHeadersToSend && !Array.isArray(base64EncodedHeadersToSend)) {
        base64EncodedHeadersToSend = [base64EncodedHeadersToSend];
    }

    const decodedHeadersToSend = base64EncodedHeadersToSend.map((base64EncodedHeaderToSend) => Buffer.from(base64EncodedHeaderToSend, 'base64').toString('ascii'));

    let headersToSend = {};

    decodedHeadersToSend.map(decodedHeaderToSend => {
        const colonIndex = decodedHeaderToSend.indexOf(":");
        const key = decodedHeaderToSend.slice(0, colonIndex);
        const value = decodedHeaderToSend.slice(colonIndex + 1);

        headersToSend[key] = value;
    });

    return headersToSend;
}

function addResponseHeadersBasedOnQueryParameters(req, res, body) {
    let base64EncodedHeadersToReturn = req.query['headers-to-return'] || [];

    if (base64EncodedHeadersToReturn && !Array.isArray(base64EncodedHeadersToReturn)) {
        base64EncodedHeadersToReturn = [base64EncodedHeadersToReturn];
    }

    const decodedHeadersToReturn = base64EncodedHeadersToReturn.map((base64EncodedHeaderToReturn) => Buffer.from(base64EncodedHeaderToReturn, 'base64').toString('ascii'));

    decodedHeadersToReturn.forEach(decodedHeaderToReturn => {
        const colonIndex = decodedHeaderToReturn.indexOf(":");
        const key = decodedHeaderToReturn.slice(0, colonIndex);
        let value = decodedHeaderToReturn.slice(colonIndex + 1);

        if (key.toLowerCase() === "etag" && value.toLowerCase() === "auto") {
            if (body !== "") {
                value = `"${body}"`;
                res.append(key, value);
            }
        } else {
            res.append(key, value);
        }
    });
}

// Clean up interactions every minute
setInterval(cleanupInteractions, 60 * 1000);

app.listen(port, () => console.log('Listening on port ' + port));
