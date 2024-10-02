# Architecture

(These notes are incomplete.)

The diagram below shows the front end making two subsequent requests, the second of which results in a cached response being served.

The origin server has two main endpoints: and `/from-browser/ids/:id` and `/ids/:id`. The former proxies requests to the latter after removing the request headers sent by the browser and instead using those that are encoded in `headers-to-send`. The `/ids/:id` endpoint returns headers that are encoded in `headers-to-return`.

```mermaid
sequenceDiagram
    participant FE as Front end
    participant V as Varnish
    participant O as Origin
    FE ->> V: record upcoming GET request in log
    V ->> O: record GET request in log
    FE ->>+ V: GET /from-browser/ids/:id?headers-to-send=...&headers-to-return=...<br><br>Cache-Control: no-cache,no-store
    V ->>+ O: GET /from-browser/ids/:id?headers-to-return=...<br><br>Cache-Control: no-cache,no-store
    O ->> V: GET /ids/:id?headers-to-return=...
    V ->> O: GET /ids/:id?headers-to-return=...
    O ->> O: record incoming request and response in log
    O ->>- V: 200 OK<br><br>Cache-Control: s-maxage=5
    V ->>- FE: 200 OK<br><br>Cache-Control: s-maxage=5
    FE ->> V: record response in log
    V ->> O: record response in log
    FE ->> V: record upcoming GET request in log
    V ->> O: record GET request in log
    FE ->>+ V: GET /from-browser/ids/:id?headers-to-return=...<br><br>Cache-Control: no-cache,no-store
    V ->>- FE: 200 OK<br><br>Cache-Control: s-maxage=5
    FE ->> V: record response in log
    V ->> O: record response in log
```

The front end is able to retrieve the interaction log from the origin (which keeps it in memory) and render it as a sequence diagram.