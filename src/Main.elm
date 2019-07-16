module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)



---- USER ----


type alias User =
    { name : String
    , age : Int
    }


type alias UserCollection =
    { data : List User }


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "name" string
        |> optional "age" int 1


userCollectionDecoder : Decoder UserCollection
userCollectionDecoder =
    Decode.map UserCollection
        (field "data" (Decode.list userDecoder))


getUsers : Cmd Msg
getUsers =
    Http.get
        { url = "http://127.0.0.1:4000/api/users"
        , expect = Http.expectJson GotUsers userCollectionDecoder
        }



---- MODEL ----


type alias Model =
    { users : List User }


init : ( Model, Cmd Msg )
init =
    ( { users = [] }, Cmd.none )



---- UPDATE ----


type Msg
    = GetUsers
    | GotUsers (Result Http.Error UserCollection)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetUsers ->
            ( model, getUsers )

        GotUsers (Ok userCollection) ->
            let
                users =
                    userCollection.data
            in
            ( { model | users = users }, Cmd.none )

        GotUsers (Err error) ->
            let
                _ =
                    Debug.log "GotUsers Error" error
            in
            ( model, Cmd.none )



---- VIEW ----


pluralize : String -> String -> Int -> String
pluralize singular plural count =
    if count == 1 then
        "1 " ++ singular

    else
        String.fromInt count ++ " " ++ plural


viewUser : User -> Html Msg
viewUser user =
    p []
        [ b [] [ text user.name ]
        , span [] [ text (" - " ++ pluralize "ano" "anos" user.age) ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Users:" ]
        , div [] (List.map viewUser model.users)
        , button [ onClick GetUsers ] [ text "Get Users" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
