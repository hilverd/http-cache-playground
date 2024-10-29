module Data.Config exposing (demoMode, originBaseUrl, showButtonForAddingPurgeRequestStep)


originBaseUrl : String
originBaseUrl =
    "http://localhost:8080"


showButtonForAddingPurgeRequestStep : Bool
showButtonForAddingPurgeRequestStep =
    False


demoMode : Bool
demoMode =
    False
