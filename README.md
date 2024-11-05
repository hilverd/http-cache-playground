# Web Cache Playground

This project lets you explore how [Varnish Cache](https://varnish-cache.org/) reacts to [HTTP headers](https://developer.mozilla.org/en-US/docs/Web/HTTP/Caching).

## Getting Started

```
git clone https://github.com/hilverd/web-cache-playground.git
cd web-cache-playground
docker compose up
```

Then open http://localhost:8080/.

### Changing the Varnish configuration

You can change the Varnish configuration by editing [`default.vcl`](default.vcl) and restarting the `varnish` container.

### Adding support for purging

To add support for [purging](https://www.varnish-software.com/developers/tutorials/purge/) you will need to

1. edit the Varnish configuration as described above,
2. set `showButtonForAddingPurgeRequestStep` to `True` in [`Config.elm`](origin/frontend/src/Data/Config.elm), and
3. use `docker compose -f docker-compose-dev.yml up --build` to recompile and run the project.

### Using a different HTTP cache

I haven't tried this yet, but in principle you should be able to use a different cache (nginx maybe?) as follows.

1. Modify `docker-compose-dev.yml` to use something other than the `varnish` image,
2. change `originHost` in [`server.js`](origin/server.js), and
3. use `docker compose -f docker-compose-dev.yml up --build` to recompile and run the project.

See also the `backend default` section in [`default.vcl`](default.vcl).

## Disclaimer

This project is not affiliated with Varnish Software or its related projects.

## Acknowledgments

This project is mainly written in [Elm](https://elm-lang.org/). The UI is based on components from [daisyUI](https://daisyui.com/).
