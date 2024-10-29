module ElementIds exposing (clientSettings, getRequestHeaderKey, getRequestHeaderValue, originCacheControlCustomDirectives, originCacheControlMaxAge, originCacheControlNoStore, originCacheControlPrivate, originCacheControlSMaxAge, originCacheControlStaleWhileRevalidate, originCustomHeaderKey, originCustomHeaderValue)


getRequestHeaderKey : Int -> Int -> String
getRequestHeaderKey stepIndex index =
    "request-header-" ++ String.fromInt stepIndex ++ "-" ++ String.fromInt index ++ "-key"


getRequestHeaderValue : Int -> Int -> String
getRequestHeaderValue stepIndex index =
    "request-header-" ++ String.fromInt stepIndex ++ "-" ++ String.fromInt index ++ "-value"


originCustomHeaderKey : Int -> String
originCustomHeaderKey stepIndex =
    "origin-custom-header-" ++ String.fromInt stepIndex ++ "-key"


originCustomHeaderValue : Int -> String
originCustomHeaderValue stepIndex =
    "origin-custom-header-" ++ String.fromInt stepIndex ++ "-value"


originCacheControlMaxAge : Int -> String
originCacheControlMaxAge index =
    "origin-cache-control-max-age-" ++ String.fromInt index


originCacheControlSMaxAge : Int -> String
originCacheControlSMaxAge index =
    "origin-cache-control-s-max-age-" ++ String.fromInt index


originCacheControlNoStore : Int -> String
originCacheControlNoStore index =
    "origin-cache-control-no-store-" ++ String.fromInt index


originCacheControlPrivate : Int -> String
originCacheControlPrivate index =
    "origin-cache-control-private-" ++ String.fromInt index


originCacheControlStaleWhileRevalidate : Int -> String
originCacheControlStaleWhileRevalidate index =
    "origin-cache-control-stale-while-revalidate-" ++ String.fromInt index


originCacheControlCustomDirectives : Int -> String
originCacheControlCustomDirectives index =
    "origin-cache-control-custom-directives-" ++ String.fromInt index


clientSettings : String
clientSettings =
    "client-settings"
